library(dplyr)
library(jsonlite)
library(httr)
library(tidyr)
library(spotifyr)


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

playlist <- httr::content(httr::GET(paste('https://api.spotify.com/v1/playlists/',
                                       '37i9dQZEVXbMDoHDwVN2tF', '/tracks',sep=''),auth_header))
song_list<- c()

for (n in 1:50) {
  song_list <- c(song_list, playlist$items[[n]]$track$id)
}

tracks = lapply(1:50, function(n) {
  GET(paste('https://api.spotify.com/v1/tracks/',
                    song_list[n],sep=''),auth_header)
})

tracks.content = sapply(1:50, function(n) {
  content(tracks[[n]])
})

tracks.matrix = t(tracks.content)

tracks_df = data.frame(matrix(ncol = 13, nrow = 50))

names_list <- tracks.matrix
  
colnames(tracks_df) <- names(tracks.matrix[[1]])


for (i in 1:50) {
  song_info <- unlist(tracks.matrix[i], recursive=FALSE)
  
  for (j in 1:length(song_info)) {
    tracks_df[i,j] <- song_info[[j]]}
  
}

View(tracks_df)

tracks_df <- apply(tracks_df,2,as.character)

write.csv(tracks_df, file = './data/global_top_50_data.csv')
