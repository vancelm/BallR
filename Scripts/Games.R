# This function essentially takes all the _home and _away columns
# and pivots them to separate rows with a new column called home_away
# home_away will have a value of either "home" or "away"
#
# By doing this, we have all of our home and away stats in the same columns
# but we can still group them separately for aggregate functions by the
# home_away column and/or other categorical columns such as season_id, date,
# season_type, etc
pivot_game_data <- function(df, home = TRUE) {
  # if home is set to TRUE we'll use "home"
  # if false we'll use "away"
  # Rather than hardcoding, this lets us use the same code for everything
  if (home) {
    home_away <- "home"
  } else {
    home_away <- "away"
  }

  # first we create a new dataframe using the columns that are the same
  # for both the home and away team.
  new_df <- df[c("season_id",
                 "season_type",
                 "game_id",
                 "game_date",
                 "min")]

  # next we add a home_away column, and set the value to our variable home_away
  new_df$home_away <- home_away

  # Vector of base column names
  columns <- c("team_id",
               "team_abbreviation",
               "team_name",
               "matchup",
               "wl",
               "fgm",
               "fga",
               "fg_pct",
               "fg3m",
               "fg3a",
               "fg3_pct",
               "ftm",
               "fta",
               "ft_pct",
               "oreb",
               "dreb",
               "reb",
               "ast",
               "stl",
               "blk",
               "tov",
               "pf",
               "pts",
               "plus_minus")

  # Now we simply loop through all the columns from the original dataframe to
  # our new dataframe and handle while dropping the "_home" or "_away" part of
  # the name.
  for (i in seq_along(columns)) {
    new_df[columns[i]] <- df[paste(columns[i], home_away, sep = "_")]
  }

  # finally, we return our new dataframe
  return(new_df)
}

# This function simply reads in the CSV, omitting null/NA values by default
# It also creates a dataframe of home games and a dataframe of away games
# and then combines the two together and returns the combined dataframe.
read_game_data <- function(omit = TRUE) {
  # Read the csv
  game_df <- read.csv("Data/game.csv")
  if (omit) {
    # Get rid of rows will null/NA if omit is TRUE
    game_df <- na.omit(game_df)
  }

  # Call our pivot function to get all of our home game columns
  home_df <- pivot_game_data(game_df, TRUE)

  # Call our pivot function to get all of our away game columns
  away_df <- pivot_game_data(game_df, FALSE)

  # Combine the home and away dataframes
  combined_df <- rbind(home_df, away_df)

  # Return the combined dataframe
  return(combined_df)
}

# Simply calls the read_game_data() function which handles all the dirty
# work of getting our data in more useable format ready for analysis
df <- read_game_data()
