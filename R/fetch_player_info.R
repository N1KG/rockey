#' fetch_player_info
#'
#' fetch miscellaneous info about specific player from the nhl api
#'
#' @param player_id a number or string that represent the id of an nhl player
#'
#' @return data.table that contains all info for the requested player
#'
#' @import data.table
#' @import httr
#'
#' @export
fetch_player_info <- function(player_id) {
  url  <- "https://statsapi.web.nhl.com"
  path <- paste0("api/v1/people/", player_id)

  r <- GET(url = url, path = path)
  r_content <- content(r, "parsed")[[2]]

  rbindlist(lapply(r_content , function(player) {
    misc_dt <- data.table(player_id = player$id, player_fullName = player$fullName , player_link = player$link, player_firstName = player$firstName,
                          player_lastName = player$lastName, player_primaryNumber = player$primaryNumber, player_birthDate = player$birthDate,
                          player_currentAge = player$currentAge, player_birthCity = player$birthCity, player_birthStateProvince = player$birthStateProvince,
                          player_birthCountry = player$birthCountry, player_nationality = player$nationality, player_height = player$height,
                          player_weight = player$weight, player_active = player$active, player_alternateCaptain = player$alternateCaptain, player_captain = player$captain,
                          player_rookie = player$rookie, player_shootsCatches = player$shootsCatches, player_rosterStatus = player$rosterStatus)
    currentTeam_dt <- as.data.table(player$currentTeam)
    names(currentTeam_dt) <- paste0("currentTeam_", currentTeam_dt)
    primaryPosition_dt <- as.data.table(player$primaryPosition)
    names(primaryPosition_dt) <- paste0("primaryPosition_", primaryPosition_dt)
    do.call(cbind, list(misc_dt, currentTeam_dt, primaryPosition_dt))
  }), fill = T)

}
