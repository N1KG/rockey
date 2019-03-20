#' fetch_team_roster
#'
#' fetch all team roster data from the nhl api
#'
#' @param team_id a number or string that represent the id of an nhl team
#'
#' @return data.table that contains data for an nhl team roster
#'
#' @import data.table
#' @import httr
#'
#' @export
#' @examples
#' fetch_team_roster(8)
#' fetch_team_roster("8")
#'
fetch_team_roster <- function(team_id) {
  url  <- "https://statsapi.web.nhl.com"
  path <- paste0("api/v1/teams/", team_id, "?expand=team.roster")

  r <- GET(url = url, path = path)
  r_content <- content(r, "parsed")$teams[[1]]$roster[[1]]

  rbindlist(lapply(r_content , function(player) {
   misc_dt <- data.table(player_jerseyNumber = player$jerseyNumber)
   person_dt <- as.data.table(player$person)
   names(person_dt) <- paste0("player_", names(person_dt))
   position_dt <- as.data.table(player$position)
   names(position_dt) <- paste0("position_", names(position_dt))
    do.call(cbind, list(person_dt, position_dt, misc_dt))
  }), fill = T)

}
