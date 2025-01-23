# Analyze player statistics
player <- function(data, name){
  subset <- c("name","team","cost","chance_of_playing_this_round", "form", "points_per_game",
              "first_name", "selected_by_percent", "total_points",
              "value_form", "minutes", "goals_scored", "assists",
              "expected_goals_per_90", "expected_assists_per_90",
              "starts_per_90", "position")
  player <- data[data$name == name,subset]
  return(player)
}

# Create additional stats
expected_stats <- function(players, game_settings = NULL){
  if(is.null(game_settings)){
    current_stats <- get_premier_stats()
    events <- current_stats$events
    games_played <- nrow(events[events$finished,]) # minimum games played
    game_settings <- get_premier_stats()$game_config$scoring
  }
  # Ensure players is data.table
  players <- as.data.table(players)
  # Initialize a data.table with the positions
  positions <- c("GKP", "DEF", "MID", "FWD")
  point_breakdown <- data.table(position = positions)

  # Iterate over the list and add columns
  for (name in names(game_settings)) {
    item <- game_settings[[name]]
    if (is.list(item)) {
      # If the item is a list (e.g., goals_conceded), extract values by position
      point_breakdown[, (name) := sapply(positions, function(pos) {
        if (!is.null(item[[pos]])) item[[pos]] else NA })]
    } else {
      # If the item is a scalar, repeat it for all positions
      point_breakdown[, (name) := item]
    }
  }

  # Adjust goals conceded to points lost per goal
  point_breakdown$goals_conceded <-  point_breakdown$goals_conceded*0.5

  setnames(point_breakdown,
           old = setdiff(names(point_breakdown), "position"),
           new = paste0(setdiff(names(point_breakdown), "position"), "_points"))
  # Calculate minutes per game
  players$minutes_per_game <- players$minutes / games_played
  players[minutes_per_game > 90, minutes_per_game := 90] # make sure players are below 90

  # Merge the two tables
  players <- merge(players, point_breakdown, "position")

  # Create expected offensive points per game
  players[, expected_goal_points := (expected_goals_per_90 * goals_scored_points +
            expected_assists_per_90 * assists_points) * minutes_per_game /90]
  # Column for minutes played
  players[, expected_play_time_points := fifelse(
            minutes_per_game >= 60, 2, # 2 points for 60+ minutes
            fifelse(minutes_per_game >= 5, 1, 0)# 1 point for 5+ minutes
  )]
  # Column for defense - how to calculate clean sheets?
  players[, expected_defensive_points := expected_goals_conceded_per_90 * goals_conceded_points + # conceded points
            clean_sheets_per_90* clean_sheets_points + # points from clean sheets
            yellow_cards/ games_played * yellow_cards_points +
            red_cards/ games_played * red_cards_points +
            saves_per_90 * saves_points/2
            ]
  # Create points per goal by position

  #Determine expected points per game unweighted by opponent
  players[ , expected_points_per_game := round(expected_goal_points + expected_play_time_points + expected_defensive_points, 2)]

  # Future weighted points per game
  return(players)

}
