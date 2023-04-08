rm(list=ls())
options(scipen=999)

## Cargo las librerías a utilizar
require(worldfootballR)
require(dplyr)
require(tidyr)

##################################################################################################

## Busco las estadísticas de Fbref.com de la temporada 2021-22 para las principales ligas europeas

#################################################################################################

stats <- c("standard", "shooting", "passing", "passing_types", "gca", "defense", "possession", "playing_time", "misc", "keepers", "keepers_adv")

results_list <- list()

for (i in stats) {
  
  big5_player <- fb_big5_advanced_season_stats(season_end_year= 2022, stat_type= i, team_or_player= "player")
  results_list[[i]] <- big5_player
  
}

# Joineo los distintos dataframes
joined_df <- Reduce(function(x, y) left_join(x, y %>% select(-Season_End_Year, -Comp, -Nation, -Pos, -Age, -Url, -Born), by = c("Player", "Squad")), results_list)

## Elimino algunas columnas repetidas
joined_df <- select(joined_df, -matches("\\.y$"))
names(joined_df) <- gsub("\\.x(\\.\\w+)*$", "", names(joined_df))
joined_df <- joined_df %>% select(-Mins_Per_90, -Season_End_Year)


##################################################################################################

## Busco el valor de mercado de cada uno de los jugadores tanto para el comienzo de la temperada 2021 como para el comienzo de la temporada 2022.

# player valuations: link kaggle
# player bio: link kaggle

#################################################################################################

players <- read.csv("~/Documents/GitHub/tt1-player-clustering/data/players.csv")
player_valuations <- read.csv("~/Documents/GitHub/tt1-player-clustering/data/player_valuations.csv")


market_value_2021 = player_valuations %>%
  group_by(player_id) %>%
  filter(date <= "2021-07-01") %>%
  slice_tail(n = 1) %>%
  mutate(market_value_2021 = market_value_in_eur) %>%
  select(player_id, market_value_2021)

market_value_2022 = player_valuations %>%
  group_by(player_id) %>%
  filter(date <= "2022-07-01") %>%
  slice_tail(n = 1) %>%
  mutate(market_value_2022 = market_value_in_eur) %>%
  select(player_id, market_value_2022)


valuations = players %>%
  select(player_id, url) %>%
  left_join(market_value_2021, by= "player_id") %>%
  left_join(market_value_2022, by= "player_id")

##################################################################################################

# Combino la información de estadísticas con la valuación de mercado

##################################################################################################

## mapping entre fbref y transfermarkt
mapped_players <- player_dictionary_mapping()
valuations$url <- gsub("co.uk", "com", valuations$url)

dataset = joined_df %>%
  left_join(mapped_players %>% select(-PlayerFBref, -TmPos), by = c('Url' = 'UrlFBref')) %>%
  left_join(valuations, by=c('UrlTmarkt'='url')) %>%
  select(-UrlTmarkt, -player_id, -Url)

write.csv(dataset, "~/Documents/GitHub/tt1-player-clustering/data/dataset.csv", row.names = FALSE)