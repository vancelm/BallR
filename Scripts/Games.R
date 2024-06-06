add_column <- function(df, new_df, column, home_away) {
  new_df[column] <- df[paste(column, home_away, sep = "_")]
  return(new_df)
}

pivot_game_data <- function(df, home = TRUE) {
  if (home) {
    home_away = "home"
  } else {
    home_away = "away"
  }
  
  new_df <- df[c("season_id",
                 "season_type",
                 "game_id",
                 "game_date",
                 "min")]
  
  new_df$home_away <- home_away
  new_df <- add_column(df, new_df, "team_id", home_away)
  new_df <- add_column(df, new_df, "team_abbreviation", home_away)
  new_df <- add_column(df, new_df, "team_name", home_away)
  new_df <- add_column(df, new_df, "matchup", home_away)
  new_df <- add_column(df, new_df, "wl", home_away)
  new_df <- add_column(df, new_df, "fgm", home_away)
  new_df <- add_column(df, new_df, "fga", home_away)
  new_df <- add_column(df, new_df, "fg_pct", home_away)
  new_df <- add_column(df, new_df, "fg3m", home_away)
  new_df <- add_column(df, new_df, "fg3a", home_away)
  new_df <- add_column(df, new_df, "fg3_pct", home_away)
  new_df <- add_column(df, new_df, "ftm", home_away)
  new_df <- add_column(df, new_df, "fta", home_away)
  new_df <- add_column(df, new_df, "ft_pct", home_away)
  new_df <- add_column(df, new_df, "oreb", home_away)
  new_df <- add_column(df, new_df, "dreb", home_away)
  new_df <- add_column(df, new_df, "reb", home_away)
  new_df <- add_column(df, new_df, "ast", home_away)
  new_df <- add_column(df, new_df, "stl", home_away)
  new_df <- add_column(df, new_df, "blk", home_away)
  new_df <- add_column(df, new_df, "tov", home_away)
  new_df <- add_column(df, new_df, "pf", home_away)
  new_df <- add_column(df, new_df, "pts", home_away)
  new_df <- add_column(df, new_df, "plus_minus", home_away)
  
  return(new_df)
}

read_game_data <- function(omit = TRUE) {
  game_df <- read.csv("Data/game.csv")
  if (omit) {
    game_df <- na.omit(game_df)
  }
  
  home_df <- pivot_game_data(game_df, TRUE)
  away_df <- pivot_game_data(game_df, FALSE)
  combined_df <- rbind(home_df, away_df)
  return(combined_df)
}

df <- read_game_data()
