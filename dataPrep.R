library(httr)
library(curl)
library(jsonlite)
library(dplyr)
library(highcharter)
library(purrr)


### General Information ###########################################################################
url <- "https://fantasy.premierleague.com/api/bootstrap-static"
response <- GET(url)
if (status_code(response) == 200) {
  generalInformation <- content(response, "parsed")
} else {
  print(paste("Error:", status_code(response)))
}


getPlayerDetails <- function() {
  map_dfr(generalInformation$elements, ~{
    tibble::as_tibble(map(.x, ~ replace(.x, is.null(.x), NA)))
  })
}

playerDetails <- getPlayerDetails() %>%
  rename(player_id = id)


getGameweekDetails <- function() {
  generalInformation$events %>%
    map_dfr(~ {
      # Replace NULL values with NA
      x <- map(.x, ~ replace(.x, is.null(.x), NA))
      
      # Select only the fields "id", "name", and "finished"
      x <- x[c("id", "name", "is_current", "can_manage", "finished", "ranked_count")]
      
      # Return as a named list to allow binding
      x
    })
}

# List of gameweeks with complete flag
gameweekDetails <- getGameweekDetails() %>%
  rename(gameweek = id,
         total_users = ranked_count) %>%
  mutate(available = ((is_current & !can_manage) | finished))

completeGameweek <- gameweekDetails %>% filter(finished == T) %>% nrow()
maxGameweek <- gameweekDetails %>% filter(available == T) %>% nrow()

### League Details ################################################################################


getLeagueDetails <- function(leagueID) {
  url <- paste0(
    "https://fantasy.premierleague.com/api/leagues-classic/",
    leagueID,
    "/standings"
  )
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    data <- content(response, "parsed")
    
    # Use map_dfr to process and bind data
    leagueDetails <- map_dfr(data$standings$results, as.data.frame)
    
    # Rename columns
    leagueDetails <- leagueDetails %>%
      rename(user_id = entry, user_name = player_name)
    
    return(leagueDetails)
  } else {
    stop(paste("Error:", status_code(response)))
  }
}


leagueID <- 564553
# Latest league table
leagueDetails <- getLeagueDetails(leagueID) %>%
  select(-id)




### League Teams ##################################################################################

# Function to return the team summary for a selected user in a selected gameweek
getGameweekSummary <- function(userID, gameweek){
  url <- paste0("https://fantasy.premierleague.com/api/entry/",userID,"/event/",gameweek,"/picks/")
  response <- GET(url)
  # Check the status
  if (status_code(response) == 200) {
    data <- content(response, "parsed")
  } else {
    print(paste("Error:", status_code(response)))
  }
  
  return(as.data.frame(data$entry_history))
}

getGameweekSummary(3469942, 17)

# Returns the team summary for each user in the league for each gameweek in the league
gameweekSummary <- data.frame()
for (userID in leagueDetails$user_id) {
  for (gameweek in 1:completeGameweek) {
    df <- getGameweekSummary(userID, gameweek)
    df$user_id <- userID
    df$gameweek <- gameweek
    gameweekSummary <- rbind(gameweekSummary, df)
  }
}

gameweekSummary <- gameweekSummary %>%
  left_join(leagueDetails %>% select(user_id, user_name, entry_name), by = c("user_id" = "user_id")) %>%
  group_by(gameweek) %>%
  mutate(league_rank = rank(-total_points, ties.method = "max")) %>%
  ungroup()
  

# Function to return the player IDs for a selected team in a selected gameweek
getGameweekTeam <- function(teamID, gameweek) {
  url <- paste0(
    "https://fantasy.premierleague.com/api/entry/",
    teamID,
    "/event/",
    gameweek,
    "/picks/"
  )
  response <- GET(url)
  # Check the status
  if (status_code(response) == 200) {
    data <- content(response, "parsed")
  } else {
    print(paste("Error:", status_code(response)))
  }
  
  return(do.call(rbind, lapply(data$picks, as.data.frame)))
}

# Check my team for a specified gameweek
getGameweekTeam(3469942, 17)

# Get's the player IDs for each user for each gameweek in the league
gameweekTeams <- data.frame()
for (userID in leagueDetails$user_id) {
  for (gameweek in gameweekDetails %>% filter(available == T) %>% pull(gameweek)) {
    df <- getGameweekTeam(userID, gameweek)
    df$user_id <- userID
    df$gameweek <- gameweek
    gameweekTeams <- rbind(gameweekTeams, df)
  }
}

gameweekTeams <- gameweekTeams %>%
  rename(player_id = element) %>%
  left_join(leagueDetails %>% select(user_id, user_name, entry_name), by = c("user_id" = "user_id")) %>%
  left_join(playerDetails %>% select(player_id, web_name), by = c("player_id" = "player_id"))

### Player History ################################################################################
getPlayerHistory <- function(playerID){
  # Call Player Stats API
  url <- paste0("https://fantasy.premierleague.com/api/element-summary/",playerID)
  response <- GET(url)
  if (status_code(response) == 200) {
    playerStats <- content(response, "parsed")
  } else {
    print(paste("Error:", status_code(response)))
  }
  
  # Select history from player stats and create a data frame with a row for each gameweek
  playerHistory <- do.call(rbind, lapply(seq_along(playerStats$history), function(index) {
    x <- playerStats$history[[index]]
    
    # Replace all NULL elements with NA
    x <- lapply(x, function(field) {
      if (is.null(field)) NA else field
    })
    
    
    x <- x[c(
      "round",
      "element",
             "minutes",
             "total_points",
             "goals_scored",
             "assists",
             "yellow_cards",
             "red_cards",
             "bps",
             "selected")]
    
    # Convert the modified list to a data frame
    df <- as.data.frame(x, stringsAsFactors = FALSE) %>%
      rename(gameweek = round)
    
    return(df)
  }))
  
  return(playerHistory)
}

getPlayerHistory(714)

playerHistory <- data.frame()
for (playerID in unique(gameweekTeams$player_id)) {
  df <- getPlayerHistory(playerID)
  playerHistory <- rbind(playerHistory, df)
}

playerHistory <- playerHistory %>%
  rename(player_id = element) %>%
  left_join(gameweekDetails %>% select(gameweek, total_users)) %>%
  mutate(selected = selected/total_users) %>%
  select(-total_users)

gameweekTeams <- gameweekTeams %>%
  left_join(playerHistory)


### League Transfers ##############################################################################
# Function to return the transfers for a selected user in a selected gameweek
getTransferSummary <- function(userID){
  url <- paste0("https://fantasy.premierleague.com/api/entry/",userID,"/transfers/")
  response <- GET(url)
  # Check the status
  if (status_code(response) == 200) {
    data <- content(response, "parsed")
  } else {
    print(paste("Error:", status_code(response)))
  }
  
  return(do.call(rbind, lapply(data, as.data.frame)))
}

# Get's the transfers for each user in the league
transfers_raw <- data.frame()
for (userID in leagueDetails$user_id) {
  df <- getTransferSummary(userID)
  # df$user_id <- userID
  # df$gameweek <- gameweek
  transfers_raw <- rbind(transfers_raw, df)
}
transfers_detail <- transfers_raw %>%
  rename(gameweek = event,
         user_id = entry) %>%
  left_join(leagueDetails %>% select(user_id, user_name, entry_name)) %>%
  left_join(playerDetails %>% select(player_id, web_name), by = c("element_in" = "player_id")) %>%
  rename(player_in = web_name) %>%
  left_join(playerDetails %>% select(player_id, web_name), by = c("element_out" = "player_id")) %>%
  rename(player_out = web_name)  %>%
  left_join(playerHistory %>% select(player_id, gameweek, total_points), by = c("element_in" = "player_id",
                                                                "gameweek" = "gameweek")) %>%
  rename(player_in_points = total_points) %>%
  left_join(playerHistory %>% select(player_id, gameweek, total_points), by = c("element_out" = "player_id",
                                                                  "gameweek" = "gameweek")) %>%
  rename(player_out_points = total_points) %>%
  mutate(net_transfer_points = player_in_points - player_out_points)

transfers <- transfers_detail %>% group_by(user_name) %>%
  summarise(net_transfer_points = sum(net_transfer_points, na.rm = T))

