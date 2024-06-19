# This function essentially takes all the _home and _away columns
# and pivots them to separate rows with a new column called home_away
# home_away will have a value of either "home" or "away"
#
# By doing this, we have all of our home and away stats in the same columns
# but we can still group them separately for aggregate functions by the
# home_away column and/or other categorical columns such as season_id, date,
# season_type, etc
#
# Here's a simplified example of what is being done:
#
# Original: col1_home, col2_home, col3_home, col1_away, col2_away, col3_away
#           23,        45,        13,        20,        43,        12
# Pivot: home_away, col1, col2, col3
#        "home",    23,   45,   13
#        "away",    20,   43,   12
#
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
  # Also, split up the date into separate year, month, day columns
  new_df <- data.frame(season_id = df$season_id,
                       season_type = df$season_type,
                       game_id = df$game_id,
                       year = as.numeric(substring(df$game_date, 1, 4)),
                       month = as.numeric(substring(df$game_date, 6, 7)),
                       day = as.numeric(substring(df$game_date, 9, 10)),
                       min = df$min)

  # next we add a home_away column, and set the value to our variable home_away
  new_df$home_away <- home_away

  # Vector of base column names (without _home or _away)
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
  # our new dataframe and handle dropping the "_home" or "_away" part of
  # the name.
  # seq_along is just a nice way to handle situations like no data
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

# Filter out all but regular season games from the year 2000 on
regular_season <- subset(df, (season_type == "Regular Season") & (year >= 2000))

# calculate aggregates for each team for each year
#  This means team A in 2000 is different from team A in 2002
aggregate <- aggregate(list(fg3m = regular_season$fg3m,
                            fg3a = regular_season$fg3a),
                       list(year = regular_season$year,
                            team = regular_season$team_abbreviation,
                            home_away = regular_season$home_away),
                       sum)

# recalulate the 3-point shooting percentage for each team/year
aggregate$fg3pct <- aggregate$fg3m / aggregate$fg3a

# so we can look at home and away separately.
#  This may seem silly since we put them all together, but that step made
#  further analysis much easier.
home <- subset(aggregate, home_away == "home")
away <- subset(aggregate, home_away == "away")

# create a vector containing the difference for each team/year (they are
#  already sorted by team and year, so we don't need to sort first).
diff <- home$fg3pct - away$fg3pct

# create a histogram showing the home/away pct diff
hist(diff, probability = TRUE,
     breaks = "FD",
     col = "#00d9ff",
     main = "Difference Between Home and Away 3-Point Shooting (2000-2023)",
     xlab = "Shot Percentge Difference",
     ylab = "frequency")

# Add a line showing the estimated probability density function
lines(density(diff), col = "red", lwd = 3)

# Output several summary statistcs
summary(aggregate$fg3pct)
sd(aggregate$fg3pct)
summary(home$fg3pct)
sd(home$fg3pct)
summary(away$fg3pct)
sd(away$fg3pct)
summary(diff)
sd(diff)

# Perform a two-sample (not-paired) t-test
#  based on testing, the same conclusion is drawn with a paired t-test
t.test(x = home$fg3pct,
       y = away$fg3pct,
       paired = FALSE)
