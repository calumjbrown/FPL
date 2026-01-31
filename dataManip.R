

J_gameweekTeams <- gameweekTeams %>%
  left_join(leagueDetails)

gameweekPlayerCount <- gameweekTeams %>%
  group_by(gameweek, player_id) %>%
  count() %>%
  left_join(playerDetails %>% select(player_id, web_name))

dfyt <- gameweekTeams %>%
  left_join(gameweekPlayerCount) %>%
  group_by(user_name, gameweek) %>%
  summarise(dfyt_index = (sum(n, na.rm = T)-11)/(11*nrow(leagueDetails)),
            selected_index = mean(selected)) %>%
  group_by(user_name) %>%
  summarise(dfyt_index = mean(dfyt_index),
            selected_index = mean(selected_index)) %>%
  arrange(desc(dfyt_index))

dfytw <- gameweekTeams %>%
  left_join(gameweekPlayerCount) %>%
  group_by(user_name, gameweek) %>%
  summarise(dfyt_index = (sum(n, na.rm = T)-11)/111)

superSubw <- gameweekTeams %>%
  left_join(playerHistory) %>%
  filter(multiplier == 0) %>%
  group_by(user_name, gameweek) %>%
  summarise(sub_points = sum(total_points, na.rm = T))

superSub <- gameweekTeams %>%
  left_join(playerHistory) %>%
  filter(multiplier == 0) %>%
  group_by(user_name) %>%
  summarise(sub_points = sum(total_points, na.rm = T)) %>%
  arrange(desc(sub_points))


topPoints <- gameweekTeams %>%
  left_join(playerHistory) %>%
  filter(multiplier > 0) %>%
  group_by(user_name, gameweek) %>%
  summarise(top_points = max(total_points,na.rm = T))

captainPoints <- gameweekTeams %>%
  left_join(playerHistory) %>%
  filter(multiplier > 1) %>%
  select(user_name, gameweek, total_points) %>%
  rename(captain_points = total_points)

captainFantastic <- topPoints %>%
  left_join(captainPoints) %>%
  mutate(difference = captain_points - top_points) %>%
  group_by(user_name) %>%
  summarise(captain_difference = sum(difference, na.rm = T)) %>%
  arrange(captain_difference)


transferTotal <- gameweekSummary %>% 
  group_by(user_name) %>% 
  summarise(transfers = sum(event_transfers, na.rm = T)) %>%
  arrange(desc(transfers))


  

hchart(gameweekSummary, "line", hcaes(x = gameweek, y = -overall_rank, group = user_name))

hchart(gameweekSummary, "line", hcaes(x = gameweek, y = -league_rank, group = user_name))

hchart(dfyt, "scatter", hcaes(x = dfyt_index, y = selected_index, group = user_name)) %>%
  hc_plotOptions(
    scatter = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.user_name}",
        allowOverlap = FALSE, 
        connectorAllowed = TRUE,
        defer = FALSE,
        style = list(fontSize = "8px")
      )
    )
  )%>%
  hc_xAxis(title = list(text = "DFYTber Index"),
           labels = list(
             formatter = JS("function(){ return Highcharts.numberFormat(this.value*100, 0) + '%'; }")
           )) %>%
  hc_yAxis(title = list(text = "Average % Selected By"),
           labels = list(
             formatter = JS("function(){ return Highcharts.numberFormat(this.value*100, 0) + '%'; }")
           )) %>%
  hc_legend(enabled = FALSE)

