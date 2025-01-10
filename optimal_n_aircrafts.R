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
par_N <- data.table(ind = K_map$ind, size = fleet_info$Size)
par_S <- mat_S


min_aircrafts_lp_code <- "
    set K;
    set F;
    set M;
    set A;
    set C within M;
    
    param ln;
    
    param N{K};
    param S{A, F, M};
    
    var x{F, K}, binary;
    var G{A, M, K}, integer >= 0;
    
    minimize TotalCost: sum{a in A,j in K}(G[a,ln,j]);
    
    s.t. first{i in F}: sum{j in K}(x[i,j]) = 1;
    s.t. second_1{a in A, j in K}: G[a,ln,j] + sum{i in F}(S[a,i,1]*x[i,j]) = G[a,1,j];
    s.t. second_k{a in A, k in C, j in K}: G[a,k-1,j] + sum{i in F}(S[a,i,k]*x[i,j]) = G[a,k,j];
    s.t. third{j in K}: sum{a in A}(G[a,ln,j]) <= N[j];
"

vars <- list(par.N = par_N,
             par.S = par_S,
             par.ln = par_ln,
             set.K = set_K,
             set.F = set_F,
             set.A = set_A,
             set.M = set_M,
             set.C = set_C,
             lp.code = min_aircrafts_lp_code)

solution_c1 <- lp_solver(vars, T, T, T)

opt_n_aircrafts <- solution_c1$optim

rm(list = setdiff(ls(), c("opt_n_aircrafts")))
