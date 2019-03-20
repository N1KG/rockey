#' fetch_teams
#'
#' fetch all teams data from the nhl api
#'
#' @return data.table that contains data for all nhl teams
#'
#' @import data.table
#' @import httr
#'
#' @export
#' @examples
#' fetch_teams()
fetch_teams <- function() {
  url  <- "https://statsapi.web.nhl.com"
  path <- "api/v1/teams"

  r <- GET(url = url, path = path)

  r_content <- content(r, "parsed")[[2]]

  rbindlist(lapply(r_content , function(team) {
    misc_dt <- data.table(team_id = team$id, team_name = team$name, team_abbreviation = team$abbreviation, team_teamName = team$teamName,
                          team_locationName = team$locationName, team_firstYearOfPlay = team$firstYearOfPlay,
                          team_officialSiteUrl = team$officialSiteUrl, team_franchiseId = team$franchiseId, team_active = team$active)
    venue_dt <- as.data.table(team$venue[names(team$venue) != "timeZone"])
    names(venue_dt) <- paste0("venue_", names(venue_dt))
    timezone_dt <- as.data.table(team$venue$timeZone)
    names(timezone_dt) <- paste0("timezone_", names(timezone_dt))
    firstYearOfPlay_dt <- as.data.table(team$firstYearOfPlay)
    division_dt <- as.data.table(team$division)
    names(division_dt) <- paste0("division_", names(division_dt))
    conference_dt <- as.data.table(team$conference)
    names(conference_dt) <- paste0("conference_", names(conference_dt))
    franchise_dt <- as.data.table(team$franchise)
    names(franchise_dt) <- paste0("franchise_", names(franchise_dt))
    do.call(cbind, list(misc_dt, venue_dt, timezone_dt, division_dt, conference_dt, franchise_dt))
  }), fill = T)

}



