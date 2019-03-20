#' fetch_player_games_stats
#'
#' fetch all games stats for a specific player from the nhl api
#'
#' @param player_id a number or string that represent the id of an nhl player
#' @param season a number of string, if you want to specify a specific season
#'
#' @return data.table that contains all stats for the requested player
#'
#' @import data.table
#' @import httr
#'
#' @export
fetch_player_games_stats <- function(player_id, season = "") {
  url  <- "https://statsapi.web.nhl.com"
  path <- paste0("api/v1/people/", player_id, "/stats?stats=gameLog&season=", season)

  r <- GET(url = url, path = path)

  r_content <- content(r, "parsed")$stats[[1]]

  rbindlist(lapply(r_content$splits , function(game) {
    stat_dt <- as.data.table(game$stat)
    names(stat_dt) <- paste0("stat_" , names(stat_dt))
    team_dt <- as.data.table(game$team)
    names(team_dt) <- paste0("team_" , names(team_dt))
    opponent_dt <- as.data.table(game$opponent)
    names(opponent_dt) <- paste0("opponent_" , names(opponent_dt))
    misc_dt <- data.table(season = game$season, date = game$date, isHome = game$isHome, isWin = game$isWin, isOT = game$isOT, game_link = game$game$link)
    cbind(stat_dt, team_dt, opponent_dt, misc_dt)
  }), fill = T)
}

