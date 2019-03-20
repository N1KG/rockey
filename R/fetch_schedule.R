#' fetch_schedule
#'
#' fetch schedule info between given dates from the nhl api
#'
#' @param start_date a string written as mm/dd/yyyy that represents the start date of the desired schedule period, default to empty
#' @param end_date a string written as mm/dd/yyyy that represents the end date of the desired schedule period, default to empty
#' @param team_id a number or string that represent the id of an nhl team, if not feed return schedule for all teams
#'
#' @return data.table that contains all schedule info for the period between the given date of return the schedule for the current day if no dates are passed as arguments
#'
#' @import data.table
#' @import httr
#'
#' @export
fetch_schedule <- function(start_date = "", end_date = "", team_id = "") {
  url  <- "https://statsapi.web.nhl.com"
  path <- paste0("/api/v1/schedule?startDate=",start_date,"&endDate=",end_date,"&teamId=", team_id)

  r <- GET(url = url, path = path)
  r_content <- content(r, "parsed")$dates
  return(r_content)
 #to do format data to return a data.table
}

sched <- fetch_schedule("03/16/2019", "03/18/2019")
fetch_schedule("03/16/2019", "03/18/2019")
fetch_schedule(team_id = 8)
