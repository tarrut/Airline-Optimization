source("mp_solver.R")
source("shorten_aircraft_name.R")
schedule <- fread("new_schedule.csv")
fleet_dist <- fread("fleet_dist.csv")
fleet_info <- fread("fleet_info.csv")


OP_AIRP <- schedule[, .N, by = Origin][which.max(N), Origin]

schedule <- merge(schedule,fleet_dist,by = "Flight no.")
schedule$`Arrival time`   <- as.ITime(schedule$`Arrival time`)
schedule$`Departure time` <- as.ITime(schedule$`Departure time`)
schedule$`Flight time` <- as.ITime(schedule$`Flight time`)
schedule[, `Flight no.` := as.character(as.integer(tstrsplit(`Flight no.`, "F", fixed = TRUE)[[2]]))]


schedule_fleet1 <- schedule[Fleet == 1]
schedule_fleet2 <- schedule[Fleet == 2]


crew_min1 <- fleet_info[1,`Crew min`]
crew_max1 <- fleet_info[1,`Crew max`]
crew_min2 <- fleet_info[2,`Crew min`]
crew_max2 <- fleet_info[2,`Crew max`]


schedule_fleet1[, arrival_min := `Arrival time` + crew_min1 * 60]
schedule_fleet1[, arrival_max := `Arrival time` + crew_max1 * 60]
schedule_fleet2[, arrival_min := `Arrival time` + crew_min2 * 60]
schedule_fleet2[, arrival_max := `Arrival time` + crew_max2 * 60]

pairs1 <- merge(schedule_fleet1,schedule_fleet1,by.x = "Destination", by.y = "Origin", allow.cartesian = TRUE)
pairs1 <- pairs1[`Departure time.y` >= arrival_min.x]
pairs1 <- pairs1[`Departure time.y` <= arrival_max.x]
pairs1 <- pairs1[,.("1" = `Flight no..x`, "2" = `Flight no..y`, time1 = `Flight time.x`, time2 = `Flight time.y`)][order(`1`)]
pairs1 <- pairs1[, `Flight time` := time1 + time2]
pairs1 <- pairs1[`Flight time` <= as.ITime("08:00:00")]
pairs2 <- merge(schedule_fleet2,schedule_fleet2,by.x = "Destination", by.y = "Origin", allow.cartesian = TRUE)
pairs2 <- pairs2[`Departure time.y` >= arrival_min.x]
pairs2 <- pairs2[`Departure time.y` <= arrival_max.x]
pairs2 <- pairs2[,.("1" = `Flight no..x`, "2" = `Flight no..y`, time1 = `Flight time.x`, time2 = `Flight time.y`)][order(`1`)]
pairs2 <- pairs2[, `Flight time` := time1 + time2]
pairs2 <- pairs2[`Flight time` <= as.ITime("08:00:00")]

schedule_fleet1[, arrival_min := NULL]
schedule_fleet1[, arrival_max := NULL]
schedule_fleet2[, arrival_min := NULL]
schedule_fleet2[, arrival_max := NULL]

pairs1[, route := paste0(`1`,"-",`2`)]
pairs2[, route := paste0(`1`,"-",`2`)]

one_day_routes1 <- schedule_fleet1[,. (route = `Flight no.`, `Flight time`)]
one_day_routes2 <- schedule_fleet2[,. (route = `Flight no.`, `Flight time`)]

one_day_routes1 <- rbind(one_day_routes1,pairs1[,.(route, `Flight time`)])
one_day_routes2 <- rbind(one_day_routes2,pairs2[,.(route, `Flight time`)])

pairs1$route <- NULL
pairs2$route <- NULL

keep_going <- TRUE
prev1 <- copy(pairs1)
prev1$time1 <- NULL
prev1$time2 <- NULL
iteration <- 1

while (keep_going) {
  
  t1 <- pairs1[, .(`2`, `1`, time2)]
  
  setnames(t1,"2",as.character(iteration + 2))
  prev1 <- merge(prev1, t1, by.x = as.character(iteration + 1),
                 by.y = "1", all.x = TRUE)
  prev1 <- prev1[!is.na(prev1[[as.character(iteration + 2)]]), ]
  prev1[, `Flight time` := `Flight time` + time2]
  prev1 <- prev1[`Flight time` <= as.ITime("08:00:00")]
  prev1$time2 <- NULL
  
  if (nrow(prev1) > 0) {
    prev1[, route := paste(.SD, collapse = "-"),
          .SDcols = as.character(1:(iteration + 2)), by = .I]
    
    one_day_routes1 <- rbind(one_day_routes1,prev1[,.(route, `Flight time`)])
    iteration <- iteration +1
  } else {
    keep_going <- FALSE
  }
  
}







keep_going <- TRUE
prev2 <- copy(pairs2)
prev2$time1 <- NULL
prev2$time2 <- NULL
iteration <- 1

while (keep_going) {
  
  t2 <- pairs2[, .(`2`, `1`, time2)]
  
  setnames(t2,"2",as.character(iteration + 2))
  prev2 <- merge(prev2, t2, by.x = as.character(iteration + 1),
                 by.y = "1", all.x = TRUE)
  prev2 <- prev2[!is.na(prev2[[as.character(iteration + 2)]]), ]
  prev2[, `Flight time` := `Flight time` + time2]
  prev2 <- prev2[`Flight time` <= as.ITime("08:00:00")]
  prev2$time2 <- NULL
  
  if (nrow(prev2) > 0) {
    prev2[, route := paste(.SD, collapse = "-"),
          .SDcols = as.character(1:(iteration + 2)), by = .I]
    
    one_day_routes2 <- rbind(one_day_routes2,prev2[,.(route, `Flight time`)])
    iteration <- iteration +1
  } else {
    keep_going <- FALSE
  }
  
}








one_day_routes1[, n_flights := length(tstrsplit(route, "-")), by = .I]
one_day_routes1[, last_flight := {
  result <- tstrsplit(route, "-")
  result[[length(result)]]
}, by = .I]
one_day_routes1[, first_flight := {
  result <- tstrsplit(route, "-")
  result[1]
}, by = .I]
one_day_routes2[, n_flights := length(tstrsplit(route, "-")), by = .I]
one_day_routes2[, last_flight := {
  result <- tstrsplit(route, "-")
  result[[length(result)]]
}, by = .I]
one_day_routes2[, first_flight := {
  result <- tstrsplit(route, "-")
  result[1]
}, by = .I]


one_day_routes1 <- merge(one_day_routes1,
                         schedule_fleet1[ , .(`Flight no.`, Origin)],
                         by.x = "first_flight",
                         by.y = "Flight no.", all.x = TRUE)
one_day_routes1 <- merge(one_day_routes1,
                         schedule_fleet1[ , .(`Flight no.`, Destination)],
                         by.x = "last_flight",
                         by.y = "Flight no.", all.x = TRUE)
one_day_routes2 <- merge(one_day_routes2,
                         schedule_fleet2[ , .(`Flight no.`, Origin)],
                         by.x = "first_flight",
                         by.y = "Flight no.", all.x = TRUE)
one_day_routes2 <- merge(one_day_routes2,
                         schedule_fleet2[ , .(`Flight no.`, Destination)],
                         by.x = "last_flight",
                         by.y = "Flight no.", all.x = TRUE)

one_day_routes1 <- one_day_routes1[order(route)]
one_day_routes1$ind <- 1:nrow(one_day_routes1)
one_day_routes2 <- one_day_routes2[order(route)]
one_day_routes2$ind <- 1:nrow(one_day_routes2)

two_day_routes1 <- data.table(expand.grid(day1 = 1:nrow(one_day_routes1),
                                          day2 = 1:nrow(one_day_routes1)))
two_day_routes2 <- data.table(expand.grid(day1 = 1:nrow(one_day_routes2),
                                          day2 = 1:nrow(one_day_routes2)))

two_day_routes1[, o1 := one_day_routes1[day1,Origin]]
two_day_routes1[, o2 := one_day_routes1[day2,Origin]]
two_day_routes1[, d1 := one_day_routes1[day1,Destination]]
two_day_routes1[, d2 := one_day_routes1[day2,Destination]]
two_day_routes1[, flight_time_1 := one_day_routes1[day1,`Flight time`]]
two_day_routes1[, flight_time_2 := one_day_routes1[day2,`Flight time`]]
two_day_routes2[, o1 := one_day_routes2[day1,Origin]]
two_day_routes2[, o2 := one_day_routes2[day2,Origin]]
two_day_routes2[, d1 := one_day_routes2[day1,Destination]]
two_day_routes2[, d2 := one_day_routes2[day2,Destination]]
two_day_routes2[, flight_time_1 := one_day_routes2[day1,`Flight time`]]
two_day_routes2[, flight_time_2 := one_day_routes2[day2,`Flight time`]]

two_day_routes1 <- two_day_routes1[o1 == d2]
two_day_routes1 <- two_day_routes1[o2 == d1]
two_day_routes1 <- two_day_routes1[d2 == OP_AIRP]
two_day_routes2 <- two_day_routes2[o1 == d2]
two_day_routes2 <- two_day_routes2[o2 == d1]
two_day_routes2 <- two_day_routes2[d2 == OP_AIRP]

two_day_routes1[, total_flight_time := flight_time_1 + flight_time_2]
two_day_routes2[, total_flight_time := flight_time_1 + flight_time_2]

valid_one_day_routes1 <- two_day_routes1[day1 == day2]
valid_one_day_routes1[, day2 := NaN][, o2 := ""][, d2 := ""]
valid_one_day_routes1[, flight_time_2 := NaN][, total_flight_time := flight_time_1]
valid_one_day_routes2 <- two_day_routes2[day1 == day2]
valid_one_day_routes2[, day2 := NaN][, o2 := ""][, d2 := ""]
valid_one_day_routes2[, flight_time_2 := NaN][, total_flight_time := flight_time_1]

all_pairings1 <- rbind(valid_one_day_routes1,two_day_routes1)
all_pairings1 <- all_pairings1[order(day1,day2)]
all_pairings1$ind <- 1:nrow(all_pairings1)
all_pairings2 <- rbind(valid_one_day_routes2,two_day_routes2)
all_pairings2 <- all_pairings2[order(day1,day2)]
all_pairings2$ind <- 1:nrow(all_pairings2)

schedule_fleet1$ind <- 1:nrow(schedule_fleet1)
schedule_fleet2$ind <- 1:nrow(schedule_fleet2)


set_F1 <- data.table(ind = 1:nrow(schedule_fleet1))
set_P1 <- data.table(ind = all_pairings1$ind)
set_K1 <- data.table(ind = 1)

par_c1 <- data.table()
par_c1$P <- 1:nrow(all_pairings1)
par_c1$c <- 1

set_F1$d <- 1
set_P1$d <- 1
par_a1 <- merge(set_F1[,.(F = ind,d)], set_P1[,.(P = ind,d)], by = "d", allow.cartesian = TRUE)
set_F1$d <- NULL
set_P1$d <- NULL
par_a1$d <- NULL

par_a1 <- merge(par_a1,all_pairings1[,.(day1,day2,ind)], by.x = "P", by.y = "ind")
par_a1 <- merge(par_a1,schedule_fleet1[,.(`Flight no.`,ind)], by.x = "F", by.y = "ind")

par_a1 <- merge(par_a1, one_day_routes1[,.(route1 = route, ind)], by.x = "day1", by.y = "ind", all.x = TRUE)
par_a1 <- merge(par_a1, one_day_routes1[,.(route2 = route, ind)], by.x = "day2", by.y = "ind", all.x = TRUE)

par_a1[, a1 := +(`Flight no.` %in% tstrsplit(route1, "-")), by = .I]
par_a1[, a2 := +(`Flight no.` %in% tstrsplit(route2, "-")), by = .I]
par_a1[, a := +(a1+a2>0)]
par_a1 <- par_a1[, .(F, P, a)]

par_h1 <- data.table()
par_h1$P <- set_P1[,.(P = ind)]
par_h1$K <- 1
par_h1$h <- 1

lp_code<- "
  set F; set P;
  param c{P}; param a{F,P};
  var x{P}, binary;
  
  minimize Cost: sum{j in P}(c[j]*x[j]);
  
  s.t. cond_1{i in F}: sum{j in P}(a[i,j]*x[j]) = 1;
"

vars1 <- list(par.c = par_c1,
              par.a = par_a1,
              set.F = set_F1,
              set.P = set_P1,
              lp.code = lp_code)

solution1 <- lp_solver(vars1, T, T, T)



set_F2 <- data.table(ind = 1:nrow(schedule_fleet2))
set_P2 <- data.table(ind = all_pairings2$ind)
set_K2 <- data.table(ind = 1)

par_c2 <- data.table()
par_c2$P <- 1:nrow(all_pairings2)
par_c2$c <- 1

set_F2$d <- 1
set_P2$d <- 1
par_a2 <- merge(set_F2[,.(F = ind,d)], set_P2[,.(P = ind,d)], by = "d", allow.cartesian = TRUE)
set_F2$d <- NULL
set_P2$d <- NULL
par_a2$d <- NULL

par_a2 <- merge(par_a2,all_pairings2[,.(day1,day2,ind)], by.x = "P", by.y = "ind")
par_a2 <- merge(par_a2,schedule_fleet2[,.(`Flight no.`,ind)], by.x = "F", by.y = "ind")

par_a2 <- merge(par_a2, one_day_routes2[,.(route1 = route, ind)], by.x = "day1", by.y = "ind", all.x = TRUE)
par_a2 <- merge(par_a2, one_day_routes2[,.(route2 = route, ind)], by.x = "day2", by.y = "ind", all.x = TRUE)

par_a2[, a1 := +(`Flight no.` %in% tstrsplit(route1, "-")), by = .I]
par_a2[, a2 := +(`Flight no.` %in% tstrsplit(route2, "-")), by = .I]
par_a2[, a := +(a1+a2>0)]
par_a2 <- par_a2[, .(F, P, a)]

par_h2 <- data.table()
par_h2$P <- set_P2[,.(P = ind)]
par_h2$K <- 1
par_h2$h <- 1

vars2 <- list(par.c = par_c2,
              par.a = par_a2,
              set.F = set_F2,
              set.P = set_P2,
              lp.code = lp_code)

solution2 <- lp_solver(vars2, T, T, T)








# ---------------------------------------------------------------------------- #
# CREW ROSTERING

sol_pairings1 <- all_pairings1[ind %in% solution1$vars[value==1,i1]]
sol_pairings2 <- all_pairings2[ind %in% solution2$vars[value==1,i1]]

possible_routes_comb <- data.table()
possible_routes_comb$monday <- c(1,1,0,0,0,0,0)
possible_routes_comb$tuesday <- c(0,0,1,1,0,0,0)
possible_routes_comb$wednesday <- c(0,0,0,0,1,1,0)
possible_routes_comb$thursday <- c(2,0,0,0,0,0,1)
possible_routes_comb$friday <- c(0,2,2,0,0,0,0)
possible_routes_comb$saturday <- c(0,0,0,2,2,0,0)
possible_routes_comb$sunday <- c(0,0,0,0,0,2,2)
possible_routes_comb$ind <- 1:nrow(possible_routes_comb)

sol_pairings1$ind <- 1:nrow(sol_pairings1)
sol_pairings2$ind <- 1:nrow(sol_pairings2)

sol_pairings1$d <- 1
rosters1 <- merge(sol_pairings1[,.(r1 = ind, d)], sol_pairings1[,.(r2 = ind), d], by = "d", allow.cartesian = TRUE)
possible_routes_comb$d <- 1
rosters1 <- merge(rosters1, possible_routes_comb[,.(W = ind, d)], by = "d", allow.cartesian = TRUE)
rosters1$d <- NULL
possible_routes_comb$d <- NULL
rosters1$ind <- 1:nrow(rosters1)

sol_pairings2$d <- 1
rosters2 <- merge(sol_pairings2[,.(r1 = ind, d)], sol_pairings2[,.(r2 = ind), d], by = "d", allow.cartesian = TRUE)
possible_routes_comb$d <- 1
rosters2 <- merge(rosters2, possible_routes_comb[,.(W = ind, d)], by = "d", allow.cartesian = TRUE)
rosters2$d <- NULL
possible_routes_comb$d <- NULL
rosters2$ind <- 1:nrow(rosters2)

rosters1 <- merge(rosters1, sol_pairings1[,.(fl_time1 = total_flight_time, ind)], by.x = "r1", by.y = "ind")
rosters1 <- merge(rosters1, sol_pairings1[,.(fl_time2 = total_flight_time, ind)], by.x = "r2", by.y = "ind")
rosters1[,flight_time := as.numeric(fl_time1+fl_time2)/3600]

rosters2 <- merge(rosters2, sol_pairings2[,.(fl_time1 = total_flight_time, ind)], by.x = "r1", by.y = "ind")
rosters2 <- merge(rosters2, sol_pairings2[,.(fl_time2 = total_flight_time, ind)], by.x = "r2", by.y = "ind")
rosters2[,flight_time := as.numeric(fl_time1+fl_time2)/3600]

set1_R <- data.table(R = 1:nrow(rosters1))
set2_R <- data.table(R = 1:nrow(rosters2))


days <- data.table(D = 1:7)
pairs1 <- data.table(C = 1:nrow(sol_pairings1))
pairs2 <- data.table(C = 1:nrow(sol_pairings2))

days$d <- 1
pairs1$d <- 1
pairs2$d <- 1

set1_P <- merge(days, pairs1, by="d", allow.cartesian = TRUE)
set2_P <- merge(days, pairs2, by="d", allow.cartesian = TRUE)

days$d <- NULL
set1_P$d <- NULL
set2_P$d <- NULL
pairs1$d <- NULL
pairs2$d <- NULL

set1_P$P <- 1:nrow(set1_P)
set2_P$P <- 1:nrow(set2_P)


par1_h <- data.table()
par1_h$R <- rosters1$ind
par1_h$h <- rosters1$flight_time

par2_h <- data.table()
par2_h$R <- rosters2$ind
par2_h$h <- rosters2$flight_time


set1_R$d <- 1
set1_P$d <- 1
par1_a <- merge(set1_R[,.(R ,d)], set1_P[,.(P ,d)], by = "d", allow.cartesian = TRUE)
set1_R$d <- NULL
set1_P$d <- NULL
par1_a$d <- NULL

set2_R$d <- 1
set2_P$d <- 1
par2_a <- merge(set2_R[,.(R ,d)], set2_P[,.(P ,d)], by = "d", allow.cartesian = TRUE)
set2_R$d <- NULL
set2_P$d <- NULL
par2_a$d <- NULL

par1_a <- merge(par1_a, rosters1[, .(r1, r2, W, ind)], by.x = "R", by.y = "ind")
par1_a <- merge(par1_a, set1_P, by = "P")

par2_a <- merge(par2_a, rosters2[, .(r1, r2, W, ind)], by.x = "R", by.y = "ind")
par2_a <- merge(par2_a, set2_P, by = "P")

par1_a_1 <- merge(par1_a[D == 1], possible_routes_comb[, .(day = monday,ind)], by.x = "W", by.y = "ind")
par1_a_2 <- merge(par1_a[D == 2], possible_routes_comb[, .(day = tuesday,ind)], by.x = "W", by.y = "ind")
par1_a_3 <- merge(par1_a[D == 3], possible_routes_comb[, .(day = wednesday,ind)], by.x = "W", by.y = "ind")
par1_a_4 <- merge(par1_a[D == 4], possible_routes_comb[, .(day = thursday,ind)], by.x = "W", by.y = "ind")
par1_a_5 <- merge(par1_a[D == 5], possible_routes_comb[, .(day = friday,ind)], by.x = "W", by.y = "ind")
par1_a_6 <- merge(par1_a[D == 6], possible_routes_comb[, .(day = saturday,ind)], by.x = "W", by.y = "ind")
par1_a_7 <- merge(par1_a[D == 7], possible_routes_comb[, .(day = sunday,ind)], by.x = "W", by.y = "ind")

par2_a_1 <- merge(par2_a[D == 1], possible_routes_comb[, .(day = monday,ind)], by.x = "W", by.y = "ind")
par2_a_2 <- merge(par2_a[D == 2], possible_routes_comb[, .(day = tuesday,ind)], by.x = "W", by.y = "ind")
par2_a_3 <- merge(par2_a[D == 3], possible_routes_comb[, .(day = wednesday,ind)], by.x = "W", by.y = "ind")
par2_a_4 <- merge(par2_a[D == 4], possible_routes_comb[, .(day = thursday,ind)], by.x = "W", by.y = "ind")
par2_a_5 <- merge(par2_a[D == 5], possible_routes_comb[, .(day = friday,ind)], by.x = "W", by.y = "ind")
par2_a_6 <- merge(par2_a[D == 6], possible_routes_comb[, .(day = saturday,ind)], by.x = "W", by.y = "ind")
par2_a_7 <- merge(par2_a[D == 7], possible_routes_comb[, .(day = sunday,ind)], by.x = "W", by.y = "ind")


par1_a <- rbind(par1_a_1,par1_a_2,par1_a_3,par1_a_4,par1_a_5,par1_a_6,par1_a_7)
par1_a <- par1_a[order(P,R)]

par2_a <- rbind(par2_a_1,par2_a_2,par2_a_3,par2_a_4,par2_a_5,par2_a_6,par2_a_7)
par2_a <- par2_a[order(P,R)]

par1_a[day == 1, r := r1]
par1_a[day == 2, r := r2]
par1_a[day == 0, r := 0]
par1_a$r1 <- NULL
par1_a$r2 <- NULL

par2_a[day == 1, r := r1]
par2_a[day == 2, r := r2]
par2_a[day == 0, r := 0]
par2_a$r1 <- NULL
par2_a$r2 <- NULL

par1_a[, a := +(r == C)]
par2_a[, a := +(r == C)]

par1_a <- par1_a[,.(P, R, a)]
par2_a <- par2_a[,.(P, R, a)]


lp_code2 <- "
  set R; set P;
  param h{R};
  param a{P,R};
  var x{R}, binary;
  
  minimize Cost: sum{j in R}(abs(h[j]-17)*x[j]);
  
  s.t. cond_1{i in P}: sum{j in R}(a[i,j]*x[j]) = 1;
"

vars3 <- list(par.h = par1_h,
              par.a = par1_a,
              set.R = set1_R,
              set.P = set1_P[,.(P)],
              lp.code = lp_code2)

# solution3 <- lp_solver(vars3, T, T, T)
# 
# 
# sol_3 <- rosters1[ind %in% solution3$vars[value == 1, i1], .(r1,r2,W,flight_time)]
# sol_3 <- merge(sol_3, possible_routes_comb, by.x = "W", by.y = "ind")
# 
# sol_3[monday != 0, monday := r1 * (monday == 1) + r2 * (monday != 1)]
# sol_3[tuesday != 0, tuesday := r1 * (tuesday == 1) + r2 * (tuesday != 1)]
# sol_3[wednesday != 0, wednesday := r1 * (wednesday == 1) + r2 * (wednesday != 1)]
# sol_3[thursday != 0, thursday := r1 * (thursday == 1) + r2 * (thursday != 1)]
# sol_3[friday != 0, friday := r1 * (friday == 1) + r2 * (friday != 1)]
# sol_3[saturday != 0, saturday := r1 * (saturday == 1) + r2 * (saturday != 1)]
# sol_3[sunday != 0, sunday := r1 * (sunday == 1) + r2 * (sunday != 1)]
