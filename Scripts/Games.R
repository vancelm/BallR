# Adds a column to the dataframe using naming conventions specific
#  to the game.csv file. This just makes the code easier to use since
#  there's a pattern with the naming of columns "_away" and "_home"
#
# The function returns the dataframe since R unfortunately doesn't
# support passing by reference
add_column <- function(df, new_df, column, home_away) {
  # adds a new column to new_df using the base column name only.
  # paste() concatenates two strings with a separator.
  # column is the base column name, such as "fga" (field goals attempted)
  # home_away should be either "home" or "away"
  #
  # The concatenated string is used to create the correct column name
  # used in the original CSV/dataframe
  new_df[column] <- df[paste(column, home_away, sep = "_")]
  return(new_df)
}

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
    home_away = "home"
  } else {
    home_away = "away"
  }

  # first we create a new dataframe and store the columns that are the same
  # for both the home and away team.
  new_df <- df[c("season_id",
                 "season_type",
                 "game_id",
                 "game_date",
                 "min")]

  # next we add a home_away column, and set the value to our variable home_away
  new_df$home_away <- home_away

  # now we simply add all the columns from the original dataframe to our new
  # dataframe using our add_column function that also takes care of the name
  # changes
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
