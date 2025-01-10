source("mp_solver.R")
source("shorten_aircraft_name.R")
schedule <- fread("schedule.csv")
new_schedule <- fread("new_schedule.csv")
fleet_dist <- fread("fleet_dist.csv")
fleet_info <- fread("fleet_info.csv")

OP_AIRP <- schedule[, .N, by = Origin][which.max(N), Origin]

schedule <- merge(schedule,fleet_dist,by = "Flight no.")
schedule$`Arrival time`   <- as.ITime(schedule$`Arrival time`)
schedule$`Departure time` <- as.ITime(schedule$`Departure time`)

new_schedule <- merge(new_schedule,fleet_dist,by = "Flight no.")
new_schedule$`Arrival time`   <- as.ITime(new_schedule$`Arrival time`)
new_schedule$`Departure time` <- as.ITime(new_schedule$`Departure time`)


# FLEET 1 -------------------------------------------------------------------- #
schedule_fleet1 <- schedule[Fleet == 1]
schedule_fleet1n <- new_schedule[Fleet == 1]

turnaround_time1 <- fleet_info[1,Turnaround]

schedule_fleet1[, adjusted_arrival := `Arrival time` + turnaround_time1 * 60]
schedule_fleet1n[, adjusted_arrival := `Arrival time` + turnaround_time1 * 60]


f_sch1 <- schedule_fleet1[, .(`Flight no.`, Origin, `Departure time`)]
setnames(f_sch1, "Flight no.", "2")

f_sch1n <- schedule_fleet1n[, .(`Flight no.`, Origin, `Departure time`)]
setnames(f_sch1n, "Flight no.", "2")


pairs1 <- schedule_fleet1[f_sch1, on = .(Destination = Origin,
                                         adjusted_arrival <= `Departure time`),
                          .("1" = `Flight no.`, `2`), nomatch = 0L]
pairs1n <- schedule_fleet1n[f_sch1n, on = .(Destination = Origin,
                                            adjusted_arrival <= `Departure time`),
                            .("1" = `Flight no.`, `2`), nomatch = 0L]


schedule_fleet1[, adjusted_arrival := NULL]
schedule_fleet1n[, adjusted_arrival := NULL]

pairs1[, route := paste0(`1`,"-",`2`)]
pairs1n[, route := paste0(`1`,"-",`2`)]

one_day_routes1 <- schedule_fleet1[,. (route = `Flight no.`)]
one_day_routes1 <- rbind(one_day_routes1,pairs1[,.(route)])

one_day_routes1n <- schedule_fleet1n[,. (route = `Flight no.`)]
one_day_routes1n <- rbind(one_day_routes1n,pairs1n[,.(route)])

pairs1$route <- NULL
pairs1n$route <- NULL

keep_going <- TRUE
prev1 <- copy(pairs1)
prev1n <- copy(pairs1n)
iteration <- 1

while (keep_going) {
  
  t1 <- pairs1[, .(`2`, `1`)]
  t1n <- pairs1[, .(`2`, `1`)]
  
  setnames(t1,"2",as.character(iteration + 2))
  prev1 <- merge(prev1, t1, by.x = as.character(iteration + 1),
                 by.y = "1", all.x = TRUE)
  prev1 <- prev1[!is.na(prev1[[as.character(iteration + 2)]]), ]
  
  setnames(t1n,"2",as.character(iteration + 2))
  prev1n <- merge(prev1n, t1n, by.x = as.character(iteration + 1),
                  by.y = "1", all.x = TRUE)
  prev1n <- prev1n[!is.na(prev1n[[as.character(iteration + 2)]]), ]
  
  if (nrow(prev1) > 0) {
    prev1[, route := paste(.SD, collapse = "-"),
          .SDcols = as.character(1:(iteration + 2)), by = .I]
    
    one_day_routes1 <- rbind(one_day_routes1,prev1[,.(route)])
  }
  
  if (nrow(prev1n) > 0) {
    prev1n[, route := paste(.SD, collapse = "-"),
           .SDcols = as.character(1:(iteration + 2)), by = .I]
    
    one_day_routes1n <- rbind(one_day_routes1n,prev1n[,.(route)])
  }
  
  iteration <- iteration + 1
  
  if (nrow(prev1)+nrow(prev1n) == 0) {
    keep_going <- FALSE
  }
  
}

one_day_routes1[, n_flights := length(tstrsplit(route, "-")), by = .I]
one_day_routes1n[, n_flights := length(tstrsplit(route, "-")), by = .I]

one_day_routes1[, last_flight := {
  result <- tstrsplit(route, "-")
  result[[length(result)]]
}, by = .I]
one_day_routes1n[, last_flight := {
  result <- tstrsplit(route, "-")
  result[[length(result)]]
}, by = .I]

one_day_routes1[, first_flight := {
  result <- tstrsplit(route, "-")
  result[1]
}, by = .I]
one_day_routes1n[, first_flight := {
  result <- tstrsplit(route, "-")
  result[1]
}, by = .I]

one_day_routes1 <- merge(one_day_routes1,
                         schedule_fleet1[ , .(`Flight no.`, Origin)],
                         by.x = "first_flight",
                         by.y = "Flight no.", all.x = TRUE)
one_day_routes1n <- merge(one_day_routes1n,
                          schedule_fleet1n[ , .(`Flight no.`, Origin)],
                          by.x = "first_flight",
                          by.y = "Flight no.", all.x = TRUE)

one_day_routes1 <- merge(one_day_routes1,
                         schedule_fleet1[ , .(`Flight no.`, Destination)],
                         by.x = "last_flight",
                         by.y = "Flight no.", all.x = TRUE)
one_day_routes1n <- merge(one_day_routes1n,
                          schedule_fleet1n[ , .(`Flight no.`, Destination)],
                          by.x = "last_flight",
                          by.y = "Flight no.", all.x = TRUE)

three_day_routes1 <- data.table(expand.grid(day1 = 1:nrow(one_day_routes1),
                                            day2 = 1:nrow(one_day_routes1),
                                            day3 = 1:nrow(one_day_routes1)))
three_day_routes1n <- data.table(expand.grid(day1 = 1:nrow(one_day_routes1n),
                                             day2 = 1:nrow(one_day_routes1n),
                                             day3 = 1:nrow(one_day_routes1n)))

three_day_routes1[, o1 := one_day_routes1[day1,Origin]]
three_day_routes1[, o2 := one_day_routes1[day2,Origin]]
three_day_routes1[, o3 := one_day_routes1[day3,Origin]]
three_day_routes1[, d1 := one_day_routes1[day1,Destination]]
three_day_routes1[, d2 := one_day_routes1[day2,Destination]]
three_day_routes1[, d3 := one_day_routes1[day3,Destination]]

three_day_routes1n[, o1 := one_day_routes1n[day1,Origin]]
three_day_routes1n[, o2 := one_day_routes1n[day2,Origin]]
three_day_routes1n[, o3 := one_day_routes1n[day3,Origin]]
three_day_routes1n[, d1 := one_day_routes1n[day1,Destination]]
three_day_routes1n[, d2 := one_day_routes1n[day2,Destination]]
three_day_routes1n[, d3 := one_day_routes1n[day3,Destination]]

three_day_routes1 <- three_day_routes1[o1 == d3]
three_day_routes1 <- three_day_routes1[o2 == d1]
three_day_routes1 <- three_day_routes1[o3 == d2]
three_day_routes1 <- three_day_routes1[d1 == OP_AIRP | d2 == OP_AIRP | d3 == OP_AIRP]

three_day_routes1n <- three_day_routes1n[o1 == d3]
three_day_routes1n <- three_day_routes1n[o2 == d1]
three_day_routes1n <- three_day_routes1n[o3 == d2]
three_day_routes1n <- three_day_routes1n[d1 == OP_AIRP | d2 == OP_AIRP | d3 == OP_AIRP]

three_day_routes1[, maint := {
  res <- 0
  if (d1 == OP_AIRP) {res <- res + 1}
  if (d2 == OP_AIRP) {res <- res + 1}
  if (d3 == OP_AIRP) {res <- res + 1}
  res
}, by = .I]

three_day_routes1n[, maint := {
  res <- 0
  if (d1 == OP_AIRP) {res <- res + 1}
  if (d2 == OP_AIRP) {res <- res + 1}
  if (d3 == OP_AIRP) {res <- res + 1}
  res
}, by = .I]

one_day_routes1$ind <- 1:nrow(one_day_routes1)
one_day_routes1n$ind <- 1:nrow(one_day_routes1n)

F_map1 <- data.table()
F_map1$ref <- schedule_fleet1$`Flight no.`
F_map1$ind <- 1:length(F_map1$ref)

F_map1n <- data.table()
F_map1n$ref <- schedule_fleet1n$`Flight no.`
F_map1n$ind <- 1:length(F_map1n$ref)

set_R1 <- data.table(ind = 1:nrow(three_day_routes1))
set_F1 <- data.table(ind = F_map1$ind)

set_R1n <- data.table(ind = 1:nrow(three_day_routes1n))
set_F1n <- data.table(ind = F_map1n$ind)

param_a1 <- merge(set_F1[, d:=1], set_R1[, d:=1],
                  by = "d", allow.cartesian = TRUE)
param_a1n <- merge(set_F1n[, d:=1], set_R1n[, d:=1],
                   by = "d", allow.cartesian = TRUE)

set_R1$d <- NULL
set_F1$d <- NULL
param_a1$d <- NULL

set_R1n$d <- NULL
set_F1n$d <- NULL
param_a1n$d <- NULL

setnames(set_R1,"ind","R")
setnames(set_F1,"ind","F")
setnames(param_a1,c("ind.x","ind.y"),c("F","R"))

setnames(set_R1n,"ind","R")
setnames(set_F1n,"ind","F")
setnames(param_a1n,c("ind.x","ind.y"),c("F","R"))

three_day_routes1$ind <- 1:nrow(three_day_routes1)
param_a1 <- merge(param_a1, F_map1, by.x = "F", by.y = "ind")
param_a1 <- merge(param_a1, three_day_routes1, by.x = "R", by.y = "ind")

three_day_routes1n$ind <- 1:nrow(three_day_routes1n)
param_a1n <- merge(param_a1n, F_map1n, by.x = "F", by.y = "ind")
param_a1n <- merge(param_a1n, three_day_routes1n, by.x = "R", by.y = "ind")

one_day_routes1[, d := 1]
F_map1[, d := 1]
o1 <- merge(one_day_routes1[, .(route,d,ind)],F_map1,
            by = "d", allow.cartesian = TRUE)

one_day_routes1n[, d := 1]
F_map1n[, d := 1]
o1n <- merge(one_day_routes1n[, .(route,d,ind)],F_map1n,
             by = "d", allow.cartesian = TRUE)

one_day_routes1$d <- NULL
F_map1$d <- NULL
o1$d <- NULL

one_day_routes1n$d <- NULL
F_map1n$d <- NULL
o1n$d <- NULL

o1[, a := +(ref %in% tstrsplit(route, "-")), by = .I]
setnames(o1,c("ind.x","ind.y"),c("R","F"))

o1n[, a := +(ref %in% tstrsplit(route, "-")), by = .I]
setnames(o1n,c("ind.x","ind.y"),c("R","F"))

param_a1 <- merge(param_a1,o1[,.(R,F,a1 = a)],
                  by.x = c("day1","F"), by.y = c("R","F"))
param_a1 <- merge(param_a1,o1[,.(R,F,a2 = a)],
                  by.x = c("day2","F"), by.y = c("R","F"))
param_a1 <- merge(param_a1,o1[,.(R,F,a3 = a)],
                  by.x = c("day3","F"), by.y = c("R","F"))

param_a1n <- merge(param_a1n,o1n[,.(R,F,a1 = a)],
                   by.x = c("day1","F"), by.y = c("R","F"))
param_a1n <- merge(param_a1n,o1n[,.(R,F,a2 = a)],
                   by.x = c("day2","F"), by.y = c("R","F"))
param_a1n <- merge(param_a1n,o1n[,.(R,F,a3 = a)],
                   by.x = c("day3","F"), by.y = c("R","F"))

par_a11 <- param_a1[, .(F, R, a1)]
par_a21 <- param_a1[, .(F, R, a2)]
par_a31 <- param_a1[, .(F, R, a3)]

par_a11n <- param_a1n[, .(F, R, a1)]
par_a21n <- param_a1n[, .(F, R, a2)]
par_a31n <- param_a1n[, .(F, R, a3)]

lp_code_fl1<- "
  set F;
  set R;
  
  param a1{F,R};
  param a2{F,R};
  param a3{F,R};
  param m{R};

  var x{R}, binary;
  
  minimize Maint: sum{j in R}(x[j]);
  
  s.t. first{i in F}: sum{j in R}(a1[i,j]*x[j]) = 1;
  s.t. second{i in F}: sum{j in R}(a2[i,j]*x[j]) = 1;
  s.t. third{i in F}: sum{j in R}(a3[i,j]*x[j]) = 1;
"

par_m1 <- data.table()
par_m1$R <- set_R1$R
par_m1$m <- three_day_routes1[,.(m = maint)]

par_m1n <- data.table()
par_m1n$R <- set_R1n$R
par_m1n$m <- three_day_routes1n[,.(m = maint)]

vars_fl1 <- list(par.a1 = par_a11,
                 par.a2 = par_a21,
                 par.a3 = par_a31,
                 set.F = set_F1,
                 set.R = set_R1,
                 par.m = par_m1,
                 lp.code = lp_code_fl1)

vars_fl1n <- list(par.a1 = par_a11n,
                  par.a2 = par_a21n,
                  par.a3 = par_a31n,
                  set.F = set_F1n,
                  set.R = set_R1n,
                  par.m = par_m1n,
                  lp.code = lp_code_fl1)

solution_fl1 <- lp_solver(vars_fl1, T, T, T)
n_aircraft_fleet1 <- sum(solution_fl1$vars$value)

solution_fl1n <- lp_solver(vars_fl1n, T, T, T)
n_aircraft_fleet1n <- sum(solution_fl1n$vars$value)

lp_code_maint1<- "
  set F;
  set R;
  
  param a1{F,R};
  param a2{F,R};
  param a3{F,R};
  param m{R};
  param N;
  
  var x{R}, binary;
  
  maximize Maint: sum{j in R}(x[j]*m[j]);
  
  s.t. first{i in F}: sum{j in R}(a1[i,j]*x[j]) = 1;
  s.t. second{i in F}: sum{j in R}(a2[i,j]*x[j]) = 1;
  s.t. third{i in F}: sum{j in R}(a3[i,j]*x[j]) = 1;
  s.t. fourth: sum{j in R}(x[j]) <= N;
"

par_N1 <- data.table(n_aircraft_fleet1)
vars_maint1 <- list(par.a1 = par_a11,
                    par.a2 = par_a21,
                    par.a3 = par_a31,
                    set.F = set_F1,
                    set.R = set_R1,
                    par.m = par_m1,
                    par.N = par_N1,
                    lp.code = lp_code_maint1)

par_N1n <- data.table(n_aircraft_fleet1n)
vars_maint1n <- list(par.a1 = par_a11n,
                     par.a2 = par_a21n,
                     par.a3 = par_a31n,
                     set.F = set_F1n,
                     set.R = set_R1n,
                     par.m = par_m1n,
                     par.N = par_N1n,
                     lp.code = lp_code_maint1)

solution_m1 <- lp_solver(vars_maint1, T, T, T)
n_maint1 <- solution_m1$optim
ind_sol_r1 <- solution_m1$vars[value == 1, i1]
sol_r1 <- three_day_routes1[ind %in% ind_sol_r1]

solution_m1n <- lp_solver(vars_maint1n, T, T, T)
n_maint1n <- solution_m1n$optim
ind_sol_r1n <- solution_m1n$vars[value == 1, i1]
sol_r1n <- three_day_routes1n[ind %in% ind_sol_r1n]

# FLEET 2 -------------------------------------------------------------------- #
schedule_fleet2 <- schedule[Fleet == 2]
schedule_fleet2n <- new_schedule[Fleet == 2]

turnaround_time2 <- fleet_info[2,Turnaround]

schedule_fleet2[, adjusted_arrival := `Arrival time` + turnaround_time2 * 60]
schedule_fleet2n[, adjusted_arrival := `Arrival time` + turnaround_time2 * 60]

f_sch2 <- schedule_fleet2[, .(`Flight no.`, Origin, `Departure time`)]
setnames(f_sch2, "Flight no.", "2")

f_sch2n <- schedule_fleet2n[, .(`Flight no.`, Origin, `Departure time`)]
setnames(f_sch2n, "Flight no.", "2")


pairs2 <- schedule_fleet2[f_sch2, on = .(Destination = Origin,
                                         adjusted_arrival <= `Departure time`),
                          .("1" = `Flight no.`, `2`), nomatch = 0L]
pairs2n <- schedule_fleet2n[f_sch2n, on = .(Destination = Origin,
                                            adjusted_arrival <= `Departure time`),
                            .("1" = `Flight no.`, `2`), nomatch = 0L]


schedule_fleet2[, adjusted_arrival := NULL]
schedule_fleet2n[, adjusted_arrival := NULL]

pairs2[, route := paste0(`1`,"-",`2`)]
pairs2n[, route := paste0(`1`,"-",`2`)]

one_day_routes2 <- schedule_fleet2[,. (route = `Flight no.`)]
one_day_routes2 <- rbind(one_day_routes2,pairs2[,.(route)])

one_day_routes2n <- schedule_fleet2n[,. (route = `Flight no.`)]
one_day_routes2n <- rbind(one_day_routes2n,pairs2n[,.(route)])

pairs2$route <- NULL
pairs2n$route <- NULL

keep_going <- TRUE
keep_going_n <- TRUE
prev2 <- copy(pairs2)
prev2n <- copy(pairs2n)
iteration <- 1

while (keep_going) {
  
  t2 <- pairs2[, .(`2`, `1`)]
  t2n <- pairs2[, .(`2`, `1`)]
  
  setnames(t2,"2",as.character(iteration + 2))
  prev2 <- merge(prev2, t2, by.x = as.character(iteration + 1),
                 by.y = "1", all.x = TRUE)
  prev2 <- prev2[!is.na(prev2[[as.character(iteration + 2)]]), ]
  
  setnames(t2n,"2",as.character(iteration + 2))
  prev2n <- merge(prev2n, t2n, by.x = as.character(iteration + 1),
                  by.y = "1", all.x = TRUE)
  prev2n <- prev2n[!is.na(prev2n[[as.character(iteration + 2)]]), ]
  
  if (nrow(prev2) > 0) {
    prev2[, route := paste(.SD, collapse = "-"),
          .SDcols = as.character(1:(iteration + 2)), by = .I]
    
    one_day_routes2 <- rbind(one_day_routes2,prev2[,.(route)])
  }
  
  if (nrow(prev2n) > 0) {
    prev2n[, route := paste(.SD, collapse = "-"),
           .SDcols = as.character(1:(iteration + 2)), by = .I]
    
    one_day_routes2n <- rbind(one_day_routes2n,prev2n[,.(route)])
  }
  
  iteration <- iteration + 1
  
  if (nrow(prev2)+nrow(prev2n) == 0) {
    keep_going <- FALSE
  }
  
}

one_day_routes2[, n_flights := length(tstrsplit(route, "-")), by = .I]
one_day_routes2n[, n_flights := length(tstrsplit(route, "-")), by = .I]

one_day_routes2[, last_flight := {
  result <- tstrsplit(route, "-")
  result[[length(result)]]
}, by = .I]
one_day_routes2n[, last_flight := {
  result <- tstrsplit(route, "-")
  result[[length(result)]]
}, by = .I]

one_day_routes2[, first_flight := {
  result <- tstrsplit(route, "-")
  result[1]
}, by = .I]
one_day_routes2n[, first_flight := {
  result <- tstrsplit(route, "-")
  result[1]
}, by = .I]

one_day_routes2 <- merge(one_day_routes2,
                         schedule_fleet2[ , .(`Flight no.`, Origin)],
                         by.x = "first_flight",
                         by.y = "Flight no.", all.x = TRUE)
one_day_routes2n <- merge(one_day_routes2n,
                          schedule_fleet2n[ , .(`Flight no.`, Origin)],
                          by.x = "first_flight",
                          by.y = "Flight no.", all.x = TRUE)

one_day_routes2 <- merge(one_day_routes2,
                         schedule_fleet2[ , .(`Flight no.`, Destination)],
                         by.x = "last_flight",
                         by.y = "Flight no.", all.x = TRUE)
one_day_routes2n <- merge(one_day_routes2n,
                          schedule_fleet2n[ , .(`Flight no.`, Destination)],
                          by.x = "last_flight",
                          by.y = "Flight no.", all.x = TRUE)

three_day_routes2 <- data.table(expand.grid(day1 = 1:nrow(one_day_routes2),
                                            day2 = 1:nrow(one_day_routes2),
                                            day3 = 1:nrow(one_day_routes2)))
three_day_routes2n <- data.table(expand.grid(day1 = 1:nrow(one_day_routes2n),
                                             day2 = 1:nrow(one_day_routes2n),
                                             day3 = 1:nrow(one_day_routes2n)))

three_day_routes2[, o1 := one_day_routes2[day1,Origin]]
three_day_routes2[, o2 := one_day_routes2[day2,Origin]]
three_day_routes2[, o3 := one_day_routes2[day3,Origin]]
three_day_routes2[, d1 := one_day_routes2[day1,Destination]]
three_day_routes2[, d2 := one_day_routes2[day2,Destination]]
three_day_routes2[, d3 := one_day_routes2[day3,Destination]]

three_day_routes2n[, o1 := one_day_routes2n[day1,Origin]]
three_day_routes2n[, o2 := one_day_routes2n[day2,Origin]]
three_day_routes2n[, o3 := one_day_routes2n[day3,Origin]]
three_day_routes2n[, d1 := one_day_routes2n[day1,Destination]]
three_day_routes2n[, d2 := one_day_routes2n[day2,Destination]]
three_day_routes2n[, d3 := one_day_routes2n[day3,Destination]]

three_day_routes2 <- three_day_routes2[o1 == d3]
three_day_routes2 <- three_day_routes2[o2 == d1]
three_day_routes2 <- three_day_routes2[o3 == d2]
three_day_routes2 <- three_day_routes2[d1 == OP_AIRP | d2 == OP_AIRP | d3 == OP_AIRP]

three_day_routes2n <- three_day_routes2n[o1 == d3]
three_day_routes2n <- three_day_routes2n[o2 == d1]
three_day_routes2n <- three_day_routes2n[o3 == d2]
three_day_routes2n <- three_day_routes2n[d1 == OP_AIRP | d2 == OP_AIRP | d3 == OP_AIRP]

three_day_routes2[, maint := {
  res <- 0
  if (d1 == OP_AIRP) {res <- res + 1}
  if (d2 == OP_AIRP) {res <- res + 1}
  if (d3 == OP_AIRP) {res <- res + 1}
  res
}, by = .I]

three_day_routes2n[, maint := {
  res <- 0
  if (d1 == OP_AIRP) {res <- res + 1}
  if (d2 == OP_AIRP) {res <- res + 1}
  if (d3 == OP_AIRP) {res <- res + 1}
  res
}, by = .I]

one_day_routes2$ind <- 1:nrow(one_day_routes2)
one_day_routes2n$ind <- 1:nrow(one_day_routes2n)

F_map2 <- data.table()
F_map2$ref <- schedule_fleet2$`Flight no.`
F_map2$ind <- 1:length(F_map2$ref)

F_map2n <- data.table()
F_map2n$ref <- schedule_fleet2n$`Flight no.`
F_map2n$ind <- 1:length(F_map2n$ref)

set_R2 <- data.table(ind = 1:nrow(three_day_routes2))
set_F2 <- data.table(ind = F_map2$ind)

set_R2n <- data.table(ind = 1:nrow(three_day_routes2n))
set_F2n <- data.table(ind = F_map2n$ind)

param_a2 <- merge(set_F2[, d:=1], set_R2[, d:=1],
                  by = "d", allow.cartesian = TRUE)
param_a2n <- merge(set_F2n[, d:=1], set_R2n[, d:=1],
                   by = "d", allow.cartesian = TRUE)

set_R2$d <- NULL
set_F2$d <- NULL
param_a2$d <- NULL

set_R2n$d <- NULL
set_F2n$d <- NULL
param_a2n$d <- NULL

setnames(set_R2,"ind","R")
setnames(set_F2,"ind","F")
setnames(param_a2,c("ind.x","ind.y"),c("F","R"))

setnames(set_R2n,"ind","R")
setnames(set_F2n,"ind","F")
setnames(param_a2n,c("ind.x","ind.y"),c("F","R"))

three_day_routes2$ind <- 1:nrow(three_day_routes2)
param_a2 <- merge(param_a2, F_map2, by.x = "F", by.y = "ind")
param_a2 <- merge(param_a2, three_day_routes2, by.x = "R", by.y = "ind")

three_day_routes2n$ind <- 1:nrow(three_day_routes2n)
param_a2n <- merge(param_a2n, F_map2n, by.x = "F", by.y = "ind")
param_a2n <- merge(param_a2n, three_day_routes2n, by.x = "R", by.y = "ind")

one_day_routes2[, d := 1]
F_map2[, d := 1]
o2 <- merge(one_day_routes2[, .(route,d,ind)],F_map2,
            by = "d", allow.cartesian = TRUE)

one_day_routes2n[, d := 1]
F_map2n[, d := 1]
o2n <- merge(one_day_routes2n[, .(route,d,ind)],F_map2n,
             by = "d", allow.cartesian = TRUE)

one_day_routes2$d <- NULL
F_map2$d <- NULL
o2$d <- NULL

one_day_routes2n$d <- NULL
F_map2n$d <- NULL
o2n$d <- NULL

o2[, a := +(ref %in% tstrsplit(route, "-")), by = .I]
setnames(o2,c("ind.x","ind.y"),c("R","F"))

o2n[, a := +(ref %in% tstrsplit(route, "-")), by = .I]
setnames(o2n,c("ind.x","ind.y"),c("R","F"))

param_a2 <- merge(param_a2,o2[,.(R,F,a1 = a)],
                  by.x = c("day1","F"), by.y = c("R","F"))
param_a2 <- merge(param_a2,o2[,.(R,F,a2 = a)],
                  by.x = c("day2","F"), by.y = c("R","F"))
param_a2 <- merge(param_a2,o2[,.(R,F,a3 = a)],
                  by.x = c("day3","F"), by.y = c("R","F"))

param_a2n <- merge(param_a2n,o2n[,.(R,F,a1 = a)],
                   by.x = c("day1","F"), by.y = c("R","F"))
param_a2n <- merge(param_a2n,o2n[,.(R,F,a2 = a)],
                   by.x = c("day2","F"), by.y = c("R","F"))
param_a2n <- merge(param_a2n,o2n[,.(R,F,a3 = a)],
                   by.x = c("day3","F"), by.y = c("R","F"))

par_a12 <- param_a2[, .(F, R, a1)]
par_a22 <- param_a2[, .(F, R, a2)]
par_a32 <- param_a2[, .(F, R, a3)]

par_a12n <- param_a2n[, .(F, R, a1)]
par_a22n <- param_a2n[, .(F, R, a2)]
par_a32n <- param_a2n[, .(F, R, a3)]

lp_code_fl2<- "
  set F;
  set R;
  
  param a1{F,R};
  param a2{F,R};
  param a3{F,R};
  param m{R};

  var x{R}, binary;
  
  minimize Maint: sum{j in R}(x[j]);
  
  s.t. first{i in F}: sum{j in R}(a1[i,j]*x[j]) = 1;
  s.t. second{i in F}: sum{j in R}(a2[i,j]*x[j]) = 1;
  s.t. third{i in F}: sum{j in R}(a3[i,j]*x[j]) = 1;
"

par_m2 <- data.table()
par_m2$R <- set_R2$R
par_m2$m <- three_day_routes2[,.(m = maint)]

par_m2n <- data.table()
par_m2n$R <- set_R2n$R
par_m2n$m <- three_day_routes2n[,.(m = maint)]

vars_fl2 <- list(par.a1 = par_a12,
                 par.a2 = par_a22,
                 par.a3 = par_a32,
                 set.F = set_F2,
                 set.R = set_R2,
                 par.m = par_m2,
                 lp.code = lp_code_fl2)

vars_fl2n <- list(par.a1 = par_a12n,
                  par.a2 = par_a22n,
                  par.a3 = par_a32n,
                  set.F = set_F2n,
                  set.R = set_R2n,
                  par.m = par_m2n,
                  lp.code = lp_code_fl2)

solution_fl2 <- lp_solver(vars_fl2, T, T, T)
n_aircraft_fleet2 <- sum(solution_fl2$vars$value)

solution_fl2n <- lp_solver(vars_fl2n, T, T, T)
n_aircraft_fleet2n <- sum(solution_fl2n$vars$value)

lp_code_maint2<- "
  set F;
  set R;
  
  param a1{F,R};
  param a2{F,R};
  param a3{F,R};
  param m{R};
  param N;
  
  var x{R}, binary;
  
  maximize Maint: sum{j in R}(x[j]*m[j]);
  
  s.t. first{i in F}: sum{j in R}(a1[i,j]*x[j]) = 1;
  s.t. second{i in F}: sum{j in R}(a2[i,j]*x[j]) = 1;
  s.t. third{i in F}: sum{j in R}(a3[i,j]*x[j]) = 1;
  s.t. fourth: sum{j in R}(x[j]) <= N;
"

par_N2 <- data.table(n_aircraft_fleet2)
vars_maint2 <- list(par.a1 = par_a12,
                    par.a2 = par_a22,
                    par.a3 = par_a32,
                    set.F = set_F2,
                    set.R = set_R2,
                    par.m = par_m2,
                    par.N = par_N2,
                    lp.code = lp_code_maint2)

par_N2n <- data.table(n_aircraft_fleet2n)
vars_maint2n <- list(par.a1 = par_a12n,
                     par.a2 = par_a22n,
                     par.a3 = par_a32n,
                     set.F = set_F2n,
                     set.R = set_R2n,
                     par.m = par_m2n,
                     par.N = par_N2n,
                     lp.code = lp_code_maint2)

solution_m2 <- lp_solver(vars_maint2, T, T, T)
n_maint2 <- solution_m2$optim
ind_sol_r2 <- solution_m2$vars[value == 1, i1]
sol_r2 <- three_day_routes2[ind %in% ind_sol_r2]

solution_m2n <- lp_solver(vars_maint2n, T, T, T)
n_maint2n <- solution_m2n$optim
ind_sol_r2n <- solution_m2n$vars[value == 1, i1]
sol_r2n <- three_day_routes2n[ind %in% ind_sol_r2n]




ind_r1 <- sol_r1[,.(day1, day2, day3)]

aicraft_routing_table_1 <- merge(ind_r1,one_day_routes1[,.(ind,`Day 1` = route)],by.x = "day1", by.y = "ind")
aicraft_routing_table_1 <- merge(aicraft_routing_table_1,one_day_routes1[,.(ind,`Day 2` = route)],by.x = "day2", by.y = "ind")
aicraft_routing_table_1 <- merge(aicraft_routing_table_1,one_day_routes1[,.(ind,`Day 3` = route)],by.x = "day3", by.y = "ind")

aicraft_routing_table_1[, Routing := .I]
aicraft_routing_table_1 <- aicraft_routing_table_1[,.(Routing, `Day 1`, `Day 2`, `Day 3`)]



ind_r2 <- sol_r2[,.(day1, day2, day3)]

aicraft_routing_table_2 <- merge(ind_r2,one_day_routes2[,.(ind,`Day 1` = route)],by.x = "day1", by.y = "ind")
aicraft_routing_table_2 <- merge(aicraft_routing_table_2,one_day_routes2[,.(ind,`Day 2` = route)],by.x = "day2", by.y = "ind")
aicraft_routing_table_2 <- merge(aicraft_routing_table_2,one_day_routes2[,.(ind,`Day 3` = route)],by.x = "day3", by.y = "ind")

aicraft_routing_table_2[, Routing := .I]
aicraft_routing_table_2 <- aicraft_routing_table_2[,.(Routing, `Day 1`, `Day 2`, `Day 3`)]




ind_r1n <- sol_r1n[,.(day1, day2, day3)]

tb1n <- merge(ind_r1n,one_day_routes1n[,.(ind,`Day 1` = route)],by.x = "day1", by.y = "ind")
tb1n <- merge(tb1n,one_day_routes1n[,.(ind,`Day 2` = route)],by.x = "day2", by.y = "ind")
tb1n <- merge(tb1n,one_day_routes1n[,.(ind,`Day 3` = route)],by.x = "day3", by.y = "ind")

tb1n[, Routing := .I]
aicraft_routing_table_3 <- tb1n[,.(Routing, `Day 1`, `Day 2`, `Day 3`)]



tb1n_m <- merge(ind_r1n,one_day_routes1n[,.(ind,`Day 1` = Destination)],by.x = "day1", by.y = "ind")
tb1n_m <- merge(tb1n_m,one_day_routes1n[,.(ind,`Day 2` = Destination)],by.x = "day2", by.y = "ind")
tb1n_m <- merge(tb1n_m,one_day_routes1n[,.(ind,`Day 3` = Destination)],by.x = "day3", by.y = "ind")
tb1n_m[, Routing := .I]
aicraft_routing_table_4 <- tb1n_m[,.(Routing, `Day 1`, `Day 2`, `Day 3`)]

aicraft_routing_table_4[, `Day 1` := {
  if (`Day 1` == OP_AIRP) {
    "\U2713"
  } else ""
}, by =.I]
aicraft_routing_table_4[, `Day 2` := {
  if (`Day 2` == OP_AIRP) {
    "\U2713"
  } else ""
}, by =.I]
aicraft_routing_table_4[, `Day 3` := {
  if (`Day 3` == OP_AIRP) {
    "\U2713"
  } else ""
}, by =.I]

tot1 <- sum(aicraft_routing_table_4$`Day 1`!="")
tot2 <- sum(aicraft_routing_table_4$`Day 2`!="")
tot3 <- sum(aicraft_routing_table_4$`Day 3`!="")
dt <- data.table()
dt$Routing <- "Total"
dt$`Day 1` <- tot1
dt$`Day 2` <- tot2
dt$`Day 3` <- tot3
aicraft_routing_table_4 <- rbind(aicraft_routing_table_4,dt)


ind_r2n <- sol_r2n[,.(day1, day2, day3)]

tb2n <- merge(ind_r2n,one_day_routes2n[,.(ind,`Day 1` = route)],by.x = "day1", by.y = "ind")
tb2n <- merge(tb2n,one_day_routes2n[,.(ind,`Day 2` = route)],by.x = "day2", by.y = "ind")
tb2n <- merge(tb2n,one_day_routes2n[,.(ind,`Day 3` = route)],by.x = "day3", by.y = "ind")

tb2n[, Routing := .I]
aicraft_routing_table_5 <- tb2n[,.(Routing, `Day 1`, `Day 2`, `Day 3`)]



tb2n_m <- merge(ind_r2n,one_day_routes2n[,.(ind,`Day 1` = Destination)],by.x = "day1", by.y = "ind")
tb2n_m <- merge(tb2n_m,one_day_routes2n[,.(ind,`Day 2` = Destination)],by.x = "day2", by.y = "ind")
tb2n_m <- merge(tb2n_m,one_day_routes2n[,.(ind,`Day 3` = Destination)],by.x = "day3", by.y = "ind")
tb2n_m[, Routing := .I]
aicraft_routing_table_6 <- tb2n_m[,.(Routing, `Day 1`, `Day 2`, `Day 3`)]

aicraft_routing_table_6[, `Day 1` := {
  if (`Day 1` == OP_AIRP) {
    "\U2713"
  } else ""
}, by =.I]
aicraft_routing_table_6[, `Day 2` := {
  if (`Day 2` == OP_AIRP) {
    "\U2713"
  } else ""
}, by =.I]
aicraft_routing_table_6[, `Day 3` := {
  if (`Day 3` == OP_AIRP) {
    "\U2713"
  } else ""
}, by =.I]

tot1 <- sum(aicraft_routing_table_6$`Day 1`!="")
tot2 <- sum(aicraft_routing_table_6$`Day 2`!="")
tot3 <- sum(aicraft_routing_table_6$`Day 3`!="")
dt <- data.table()
dt$Routing <- "Total"
dt$`Day 1` <- tot1
dt$`Day 2` <- tot2
dt$`Day 3` <- tot3
aicraft_routing_table_6 <- rbind(aicraft_routing_table_6,dt)



rm(list = setdiff(ls(), c("aicraft_routing_table_1", 
                          "aicraft_routing_table_2",
                          "aicraft_routing_table_3",
                          "aicraft_routing_table_4",
                          "aicraft_routing_table_5",
                          "aicraft_routing_table_6")))




