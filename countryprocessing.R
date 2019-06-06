## This file will produce summary statistics for danceability, valence, tempo, energy, speechiness,
## acousticness, length, and liveness for songs.

library(stringr)

countries <- c("Argentina", "Australia", "Austria", "Belgium", "Bolivia", "Brazil", "Bulgaria", 
               "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", 
               "Dominican Republic", "Ecuador", "El Salvador", "Estonia", "Finland", "France", 
               "Germany", "Greece", "Guatemala", "Honduras", "Hong Kong", "Hungary", "Iceland", 
               "India", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
               "Luxembourg", "Malaysia", "Malta", "Mexico", "Netherlands", "New Zealand", "Nicaragua", 
               "Norway", "Panama", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Romania", 
               "Singapore", "Slovakia", "South Africa", "Spain", "Sweden", "Switzerland", "Taiwan", 
               "Thailand", "Turkey", "United Kingdom", "United States", "Uruguay", "Vietnam")

country_data <- data.frame('country' = countries, 'danceability' = NA, 'valence' = NA, 'tempo' = NA, 
                           'energy' = NA, 'speechiness' = NA, 'acousticness' = NA, 'length' = NA, 
                           'liveness' = NA, stringsAsFactors = FALSE)

for(var in countries) {
  intermediatedf <- read.csv(str_c('./data/', var, '_top_50_song_audio_features.csv'))
  
  danceability <- intermediatedf$danceability
  danceability <- mean(danceability)
  valence <- intermediatedf$valence
  valence <- mean(valence)
  tempo <- intermediatedf$tempo
  tempo <- mean(tempo)
  energy <- intermediatedf$energy
  energy <- mean(energy)
  speechiness <- intermediatedf$speechiness
  speechiness <- mean(speechiness)
  acousticness <- intermediatedf$acousticness
  acousticness <- mean(acousticness)
  length <- intermediatedf$duration_ms
  length <- mean(length)
  liveness <- intermediatedf$liveness
  liveness <- mean(liveness)
  
  country_data[country_data$country == var,]$danceability <- danceability
  country_data[country_data$country == var,]$valence <- valence
  country_data[country_data$country == var,]$tempo <- tempo
  country_data[country_data$country == var,]$energy <- energy
  country_data[country_data$country == var,]$speechiness <- speechiness
  country_data[country_data$country == var,]$acousticness <- acousticness
  country_data[country_data$country == var,]$length <- length
  country_data[country_data$country == var,]$liveness <- liveness
}

rm(intermediatedf)
