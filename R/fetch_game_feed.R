#' fetch_game_feed
#'
#' fetch the game feed for a specific game from the nhl api
#'
#' @param game_id a number or string that represent the id of an nhl game
#'
#' @return data.table that contains all event for the requested game
#'
#' @import data.table
#' @import httr
#'
#' @export
fetch_game_feed <- function(game_id) {
  url  <- "https://statsapi.web.nhl.com"
  path <- paste0("/api/v1/game/", game_id, "/feed/live")

  r <- GET(url = url, path = path)

  r_content <- content(r, "parsed")$liveData$plays$allPlays

  rbindlist(lapply(r_content , function(play) {
    result_dt <- as.data.table(play$result)[1]
    names(result_dt) <- paste0("result_", names(result_dt))
    about_dt <- as.data.table(play$about[names(play$about) != "goals"])
    names(about_dt) <- paste0("about_", names(about_dt))
    goals_dt <- as.data.table(play$about$goals)
    names(goals_dt) <- paste0("goals_", names(goals_dt))
    dt_to_cbind <- list(result_dt, about_dt, goals_dt)
    if (length(play$coordinates) > 0) {
      coordinates_dt <- as.data.table(play$coordinates)
      names(coordinates_dt) <- paste0("coordinates_", names(coordinates_dt))
      dt_to_cbind <- c(dt_to_cbind, coordinates_dt)
    }
    if (length(play$team) > 0) {
      team_dt <- as.data.table(play$team)
      names(team_dt) <- paste0("team_", names(team_dt))
      dt_to_cbind <- c(dt_to_cbind, team_dt)
    }
    if (length(play$players) > 0) {
      players_dt <-lapply(play$players, function(player){
        data.table(player_id = player$player$id, player_fullName = player$player$fullName, player_link = player$player$link, player_playerType = player$playerType)
      })
      dcast_players_dt <- dcast(rbindlist(players_dt)[ , player_number := 1:.N], 1~player_number , value.var = c("player_id", "player_fullName", "player_link", "player_playerType"))[,-1]
      dt_to_cbind <- c(dt_to_cbind, dcast_players_dt)
    }
    do.call("cbind", dt_to_cbind)
  }), fill = T)
}

