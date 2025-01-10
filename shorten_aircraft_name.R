shorten_aircraft_name <- function(aircraft) {
  if (substr(aircraft, 1, 6) == "Boeing") {
    return(paste0("B", substr(aircraft, 8, nchar(aircraft))))
  }
  if (substr(aircraft, 1, 6) == "Airbus") {
    return(substr(aircraft, 8, nchar(aircraft)))
  }
  return(aircraft)
}