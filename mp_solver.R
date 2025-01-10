list.of.packages <- c("data.table", "Rglpk", "kableExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
rm(list.of.packages, new.packages)
suppressMessages(library(data.table))
suppressMessages(library(Rglpk))
suppressMessages(library(kableExtra))

lp_solver <- function(variables = list(), verbose = T, want_0 = F, math_txt = F){
  old_files <- list.files(tempdir())
  
  # Algorithms ----
  lp.mathprog_write <- function(){
    
    lp.code <- list.files(tempdir(), pattern = ".txt", full.names = T)
    lp.code <- lp.code[basename(lp.code) != "lp_mathprog.txt"]
    lp.constl <- list.files(tempdir(), pattern = ".csv", full.names = T)
    lp.constl <- lp.constl[substr(basename(lp.constl), 1, 4) %in% c("par.", "set.")]
    
    lp.code <- suppressWarnings(readLines(lp.code))
    lp.constr <- lapply(lp.constl, function(x) fread(x, header=TRUE))
    parset <- names(lp.constr) <- substr(basename(lp.constl), 1, nchar(basename(lp.constl))-4)
    names(lp.constr) <- substr(names(lp.constr),5,nchar(names(lp.constr)))
    parset <- substr(parset,1,3)
    parset[which(parset=="par")] <- "param"
    
    data.lines <- function(constraint){
      if(ncol(constraint) == 1) setnames(constraint, "value")
      if(ncol(constraint) > 1)  setnames(constraint, c(c("i","j","k")[seq(ncol(constraint)-1)], "value"))
      if(ncol(constraint) == 1) return(paste(":=", paste(unlist(constraint), collapse = " ")))
      if(ncol(constraint) == 2) return(paste(":=", paste(paste(constraint$i, constraint$value), collapse = " ")))
      if(ncol(constraint) == 3) return(paste(":= ", paste(constraint[, paste0("[", i, ",", j, "] ", value)], collapse = " ")))
      if(ncol(constraint) == 4) return(paste(":= ", paste(constraint[, paste0("[", i, ",", j, "," ,k, "] ", value)], collapse = " ")))
    }
    def <- tryCatch(sapply(lp.constr, data.lines), error=function(e) NULL)
    if(is.null(def)) return(FALSE)
    def <-  c(lp.code,"","data;", paste(parset, names(def), def,";"), "end;")
    writeLines(def, paste0(tempdir(), "/lp_mathprog.txt"))
    if(math_txt) writeLines(def, "lp_mathprog.txt")
    return(TRUE)
  }
  
  lp_mp <- function(){
    if(verbose) cat("READING THE FILE:\n\n")
    model <- tryCatch(Rglpk_read_file(paste0(tempdir(), "/lp_mathprog.txt"), type="MathProg", verbose = verbose),
                      error = function(e) NULL)
    if(is.null(model)) return(NULL)
    if(verbose) cat("\n\nSOLVING THE LINEAR PROBLEM:\n\n")
    sol <- Rglpk_solve_LP(obj=model$objective, mat=model$constraints[[1]], 
                          dir=model$constraints[[2]], rhs=model$constraints[[3]], 
                          bounds=model$bounds, types=model$types, max=model$maximum,
                          verbose = verbose)
    sol <- sol[1:3]
    names(sol$solution) <- attr(model, "objective_vars_names")
    if(want_0) sol$solution <- as.data.frame(sol$solution)
    if(!want_0) sol$solution <- as.data.frame(sol$solution[which(sol$solution>0)])
    names(sol$solution) <- ""
    
    sol_vars <- 0
    if(sol$status == 0){
      sol_vars <- data.table(var = row.names(sol$solution), value = sol$solution)
      setnames(sol_vars, c("var", "value"))
      sol_v2 <- sol_vars[, tstrsplit(gsub("]","",var), ",")]
      sol_v3 <- sol_v2[, tstrsplit(V1, "\\[")]
      sol_v3 <- cbind(sol_v3, sol_v2[, -1])
      setnames(sol_v3, c("var", "i1", "i2", "i3")[seq(ncol(sol_v3))])
      sol_vars <- cbind(sol_v3, sol_vars[, -1])
    }
    return(list(sol = sol, vars = sol_vars))
  }
  
  if(sum(names(variables) %in% "lp.code") != 1) {
    cat("There is no lp.code\n")
    return(NULL)
  }
  
  for(i in seq(length(variables))){
    if(names(variables[i]) == "lp.code") {
      writeLines(variables[[i]], paste0(tempdir(), "/", names(variables)[i], ".txt"))
    } else {
      fwrite(variables[[i]], paste0(tempdir(), "/", names(variables)[i], ".csv"))
    }
  }
  
  math_file <- lp.mathprog_write()
  if(!math_file) {
    cat("Sets and/or parameters should be revised\n")
    sol <- NULL
  }
  if(math_file) {
    sol <- lp_mp()
    if(is.null(sol)) {
      cat("\nThe model should be revised.\n")
      sol <- NULL
    }
    if(!is.null(sol)) sol <- list(optim = sol$sol$optimum, vars = sol$vars)
  }
  file.remove(paste0(tempdir(), "/", setdiff(list.files(tempdir()), old_files)))
  return(sol)
}