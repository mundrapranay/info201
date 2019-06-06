library(dplyr)
library(jsonlite)
library(httr)
library(tidyr)
library(spotifyr)
library(plyr)


library(mapdata)
world <- map_data('world')

## Associate the URL IDs with each of the names
## Use the names to plot the associated data
## Should be an easy merge

z <- playlistGet('Canada')

countries <- c("Argentina", "Australia", "Austria", "Belgium", "Bolivia", "Brazil", "Bulgaria", 
               "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", 
               "Dominican Republic", "Ecuador", "El Salvador", "Estonia", "Finland", "France", 
               "Germany", "Greece", "Guatemala", "Honduras", "Hong Kong", "Hungary", "Iceland", 
               "India", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Latvia", "Lithuania", 
               "Luxembourg", "Malaysia", "Malta", "Mexico", "Netherlands", "New Zealand", "Nicaragua", 
               "Norway", "Panama", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Romania", 
               "Singapore", "Slovakia", "South Africa", "Spain", "Sweden", "Switzerland", "Taiwan", 
               "Thailand", "Turkey", "United Kingdom", "United States", "Uruguay", "Vietnam")


url_code <- c("37i9dQZEVXbMMy2roB9myp", "37i9dQZEVXbJPcfkRz0wJ0", "37i9dQZEVXbKNHh6NIXu36", 
              "37i9dQZEVXbJNSeeHswcKB", "37i9dQZEVXbJqfMFK4d691", "37i9dQZEVXbMXbN3EUUhlg", 
              "37i9dQZEVXbNfM2w2mq1B8", "37i9dQZEVXbKj23U1GF4IR", "37i9dQZEVXbL0GavIqMTeb", 
              "37i9dQZEVXbOa2lmxNORXQ", "37i9dQZEVXbMZAjGMynsQX", "37i9dQZEVXbIP3c3fqVrJY", 
              "37i9dQZEVXbL3J0k32lWnN", "37i9dQZEVXbKAbrMR8uuf7", "37i9dQZEVXbJlM6nvL1nD1", 
              "37i9dQZEVXbLxoIml4MYkT", "37i9dQZEVXbLesry2Qw2xS", "37i9dQZEVXbMxcczTSoGwZ", 
              "37i9dQZEVXbIPWwFssbupI", "37i9dQZEVXbJiZcmkrIHGU", "37i9dQZEVXbJqdarpmTJDL", 
              "37i9dQZEVXbLy5tBFyQvd4", "37i9dQZEVXbJp9wcIM9Eo5", "37i9dQZEVXbLwpL8TjsxOG", 
              "37i9dQZEVXbNHwMxAkvmF8", "37i9dQZEVXbKMzVsSGQ49S", "37i9dQZEVXbLZ52XmnySJg", 
              "37i9dQZEVXbObFQZ3JLcXt", "37i9dQZEVXbKM896FDX8L1", "37i9dQZEVXbJ6IpvItkve3", 
              "37i9dQZEVXbIQnj7RRhdSX", "37i9dQZEVXbKXQ4mDTEBXq", "37i9dQZEVXbJWuzDrTxbKS", 
              "37i9dQZEVXbMx56Rdq5lwc", "37i9dQZEVXbKGcyg6TFGx6", "37i9dQZEVXbJlfUljuZExa", 
              "37i9dQZEVXbMD2H5HJqmx9", "37i9dQZEVXbO3qyFxbkOE1", "37i9dQZEVXbKCF6dqVpDkS", 
              "37i9dQZEVXbM8SIrkERIYl", "37i9dQZEVXbISk8kxnzfCq", "37i9dQZEVXbJvfa0Yxg7E7", 
              "37i9dQZEVXbKypXHVwk1f0", "37i9dQZEVXbNOUPGj7tW6T", "37i9dQZEVXbJfdy5b0KP7W", 
              "37i9dQZEVXbNBz9cRCSFkY", "37i9dQZEVXbN6itCcaL3Tt", "37i9dQZEVXbKyJS56d1pgi", 
              "37i9dQZEVXbNZbJ6TZelCq", "37i9dQZEVXbK4gjvS1FjPY", "37i9dQZEVXbKIVTPX9a2Sb", 
              "37i9dQZEVXbMH2jvi6jvjk", "37i9dQZEVXbNFJfN1Vw8d9", "37i9dQZEVXbLoATJ81JYXz", 
              "37i9dQZEVXbJiyhoAPEfMK", "37i9dQZEVXbMnZEatlMSiu", "37i9dQZEVXbMnz8KIWsvf9", 
              "37i9dQZEVXbIVYVBNw9D5K", "37i9dQZEVXbLnolsZ8PSNw", "37i9dQZEVXbLRQDuF5jeBp", 
              "37i9dQZEVXbMJJi3wgRbAy", "37i9dQZEVXbLdGSmz6xilI")

countries <- data.frame(countries, url_code)

clientID <- "d39d8c736e174cb18a58abec9ba00ec9"
secret <- "40f864e7d87c49dcb0e55ed42ada4e7d"

response <- POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

token <- content(response)$access_token

auth_header <- httr::add_headers('Authorization'=
                                   paste('Bearer',token))

## Gets the playlist for one country.

playlistGet <- function(country) {
  
  id <- countries[countries$countries == country,]$url_code
  
  playlist <- httr::content(httr::GET(paste('https://api.spotify.com/v1/playlists/',
                                            id, '/tracks',sep=''),auth_header))
  return(playlist)
}

## Reusing code from retrieve_data.R

songlistCompute <- function(country) {
  
  playlist <- playlistGet(country)
  song_list<- c()
  
  for (n in 1:50) {
    song_list <- c(song_list, playlist$items[[n]]$track$id)
  }
  
  collapsed_list <- paste0(song_list,collapse = ',')
  
  tracks <- httr::content(httr::GET(paste('https://api.spotify.com/v1/tracks/?ids=', collapsed_list, sep=''), auth_header))
  
  tracks_df = data.frame(matrix(ncol = 17, nrow = 50))
  
  colnames(tracks_df) <- names(tracks[[1]][[1]])
  
  list <- unlist(tracks, recursive = FALSE)
  
  tracks_df <- do.call("rbind", list) %>% as.data.frame
  
  
  ## get audio features
  
  audio_features = lapply(1:50, function(n) {
    GET(paste('https://api.spotify.com/v1/audio-features/',
              song_list[n],sep=''),auth_header)
  })
  
  audio_features_content = sapply(1:50, function(n) {
    content(audio_features[[n]])
  })
  
  audio_features_matrix = t(audio_features_content)
  
  song_names <- tracks_df[,12]
  artist_names <- c()
  artist_column <- tracks_df[,2]
  for (n in 1:50) {
    artist_names <- c(artist_names, artist_column[[n]][[1]]$name)
  }
  
  audio_features_df <- cbind(rank = 1:50, id = song_list[1:50], name = song_names[1:50], 
                             artist = artist_names[1:50], danceability = audio_features_matrix[,1], 
                             energy = audio_features_matrix[,2], key = audio_features_matrix[,3], 
                             loudness = audio_features_matrix[,4], mode = audio_features_matrix[,5],
                             speechiness = audio_features_matrix[,6], acousticness = audio_features_matrix[,7],
                             instrumentalness = audio_features_matrix[,8], liveness = audio_features_matrix[,9],
                             valence = audio_features_matrix[,10], tempo = audio_features_matrix[,11], 
                             duration_ms = audio_features_matrix[,17], time_signature = audio_features_matrix[,18])
  audio_features_df = audio_features_df %>% as.data.frame
  tracks_df <- apply(tracks_df,2,as.character)
  audio_features_df <- apply(audio_features_df,2,as.character)
  write.csv(tracks_df, str_c("./data/",country,"_top_50_song_data.csv"))
  write.csv(audio_features_df, str_c("./data/",country,"_top_50_song_audio_features.csv"))
  
}

## Spaced out like this because any other method results in corrupted data. Recommended to run these one at a time
## Possibly due to API keys being spammed over & over again and Spotify not being able to communicate fast
## Enough with the computer.

songlistCompute('Argentina')
songlistCompute('Australia')
songlistCompute('Austria')
songlistCompute('Belgium')
songlistCompute('Bolivia')
songlistCompute('Brazil')
songlistCompute('Bulgaria')
songlistCompute('Canada')
songlistCompute('Chile')
songlistCompute('Colombia')
songlistCompute('Costa Rica')
songlistCompute('Czech Republic')
songlistCompute('Denmark')
songlistCompute('Dominican Republic')
songlistCompute('Ecuador')
songlistCompute('El Salvador')
songlistCompute('Estonia')
songlistCompute('Finland')
songlistCompute('France')
songlistCompute('Germany')
songlistCompute('Greece')
songlistCompute('Guatemala')
songlistCompute('Honduras')
songlistCompute('Hong Kong')
songlistCompute('Hungary')
songlistCompute('Iceland')
songlistCompute('India')
songlistCompute('Indonesia')
songlistCompute('Ireland')
songlistCompute('Israel')
songlistCompute('Italy')
songlistCompute('Japan')
songlistCompute('Latvia')
songlistCompute('Lithuania')
songlistCompute('Luxembourg')
songlistCompute('Malaysia')
songlistCompute('Malta')
songlistCompute('Mexico')
songlistCompute('Netherlands')
songlistCompute('New Zealand')
songlistCompute('Nicaragua')
songlistCompute('Singapore')
songlistCompute('Slovakia')
songlistCompute('South Africa')
songlistCompute('Spain')
songlistCompute('Sweden')
songlistCompute('Switzerland')
songlistCompute('Taiwan')
songlistCompute('Thailand')
songlistCompute('Turkey')
songlistCompute('United Kingdom')
songlistCompute('United States')
songlistCompute('Uruguay')
songlistCompute('Vietnam')
