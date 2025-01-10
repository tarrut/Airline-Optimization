source("mp_solver.R")
source("shorten_aircraft_name.R")

schedule <- fread("schedule.csv")
flight_info <- fread("flight_info.csv")
fleet_info <- fread("fleet_info.csv")

fleet_info[, Fleet_short := shorten_aircraft_name(Fleet), by = Fleet]

fleet_assignment_lp_code <- "
    set K;
    set F;
    set M;
    set A;
    set C within M;
    
    param ln;
    
    param c{F, K};
    param N{K};
    param S{A, F, M};
    
    var x{F, K}, binary;
    var G{A, M, K}, integer >= 0;
    
    minimize TotalCost: sum{i in F, j in K}(c[i,j]*x[i,j]);
    
    s.t. first{i in F}: sum{j in K}(x[i,j]) = 1;
    s.t. second_1{a in A, j in K}: G[a,ln,j] + sum{i in F}(S[a,i,1]*x[i,j]) = G[a,1,j];
    s.t. second_k{a in A, k in C, j in K}: G[a,k-1,j] + sum{i in F}(S[a,i,k]*x[i,j]) = G[a,k,j];
    s.t. third{j in K}: sum{a in A}(G[a,ln,j]) <= N[j];
"

F_map <- data.table()
F_map$ref <- flight_info$`Flight no.`
F_map$ind <- 1:length(F_map$ref)

K_map <- data.table()
K_map$ref <- fleet_info$Fleet
K_map$ind <- 1:length(K_map$ref)

A_map <- data.table()
A_map$ref <- unique(schedule$Origin)
A_map$ind <- 1:length(A_map$ref)

M_map <- data.table()
M_map$ref <- unique(c(schedule$`Departure time`,schedule$`Arrival time`))
M_map$ref <- M_map$ref[order(
  as.integer(substr(M_map$ref, 1, 2)),
  as.integer(substr(M_map$ref, 4, 5))
)]
M_map$ind <- 1:length(M_map$ref)

par_ln <- data.table(max(M_map$ind))

spill_func <- function(ce, mu, sigma) {
  integr_func <- function(x) {
    (x - ce) * dnorm(x,mu,sigma)}
  rs <- integrate(integr_func, ce, Inf)$value
  return(rs)
}

n_fleets <- nrow(fleet_info)
for (i in 1:n_fleets) {
  flight_info[, paste0("spill_", i) := spill_func(fleet_info$Seats[i], `Demand (mean)`, `Demand (sd)`), by = `Flight no.`]
  flight_info[, paste0("op_cost_", i) := fleet_info$CASK[i] * `Distance (km)` * fleet_info$Seats[i]]
  flight_info[, paste0("spill_cost_", i) := fleet_info$RASK[i] * `Distance (km)` * get(paste0("spill_", i))]
  flight_info[, paste0("cost_", i) := get(paste0("op_cost_", i)) + get(paste0("spill_cost_", i))]
  
  flight_info[, paste0("spill_", i) := NULL]
  flight_info[, paste0("op_cost_", i) := NULL]
  flight_info[, paste0("spill_cost_", i) := NULL]
}

cost_cols <- paste0("cost_", 1:length(K_map$ref))

co <- melt(
  flight_info,
  id.vars = "Flight no.",
  measure.vars = cost_cols,
  variable.name = "Cost_Type",
  value.name = "Cost"
)

K_map[, cost_type := paste0("cost_", .I)]
co[K_map, on = .(Cost_Type = cost_type), K := ind]
co <- co[, .(F = `Flight no.`, K, c = Cost)]
co <- merge(co, F_map[, .(ref, ind)], by.x = "F", by.y = "ref", all.x = TRUE)
co <- co[, .(F = ind,K,c)]


mat_S <- data.table(expand.grid(A = A_map$ind, F = F_map$ind, M = M_map$ind))
mat_S[, S := 0]
schedule[, F_ind := F_map$ind]
schedule[, `:=`(
  arr_t_ind = M_map[.SD, on = .(ref = `Arrival time`), x.ind],
  arr_a_ind = A_map[.SD, on = .(ref = Destination), x.ind]
)]
mat_S[schedule, on = .(A = arr_a_ind, F = F_ind, M = arr_t_ind), S := S + 1]
schedule[, `:=`(
  dep_t_ind = M_map[.SD, on = .(ref = `Departure time`), x.ind],
  dep_a_ind = A_map[.SD, on = .(ref = Origin), x.ind]
)]
mat_S[schedule, on = .(A = dep_a_ind, F = F_ind, M = dep_t_ind), S := S - 1]

set_K <- data.table(ind = K_map$ind)
set_F <- data.table(ind = F_map$ind)
set_M <- data.table(ind = M_map$ind)
set_A <- data.table(ind = A_map$ind)
set_C <- data.table(ind = 2:max(M_map$ind))

par_ln <- data.table(ind = max(M_map$ind))
par_c <- co
par_N <- data.table(ind = K_map$ind, size = fleet_info$Size)
par_S <- mat_S

comb <- data.table()
comb$n1 <- 0:sum(par_N$size)
comb$n2 <- sum(par_N$size):0

opt <- function(i) {
  par_N_copy <- copy(par_N)
  par_N_copy$size <- c(comb$n1[i], comb$n2[i])
  vars <- list(
    par.c = par_c,
    par.N = par_N_copy,
    par.S = par_S,
    par.ln = par_ln,
    set.K = set_K,
    set.F = set_F,
    set.A = set_A,
    set.M = set_M,
    set.C = set_C,
    lp.code = fleet_assignment_lp_code
  )
  solution_c2 <- lp_solver(vars, T, T, T)
  return(solution_c2$optim)
}

comb[, n3 := round(opt(as.integer(n1)+1)),by = n1]

n <- which(min(comb$n3) == comb$n3)

new_col <- paste(K_map$ref, "aircrafts")
setnames(comb,paste0("n",as.character(K_map$ind)),new_col)
setnames(comb, "n3", "Total daily cost (€)")

rm(list = setdiff(ls(), c("comb", "n")))
