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
      
      # # Select only the fields "id", "name", and "finished"
      # x <- x[c("id", "name", "is_current", "can_manage", "finished", "ranked_count")]
      
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