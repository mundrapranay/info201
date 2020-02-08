library(dplyr)
library(jsonlite)
library(httr)
library(tidyr)
library(spotifyr)
library(plyr)


clientID <- ""
secret <- ""

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

collapsed_list <- paste0(song_list,collapse = ',')

tracks <- httr::content(httr::GET(paste('https://api.spotify.com/v1/tracks/?ids=', collapsed_list, sep=''), auth_header))

tracks_df = data.frame(matrix(ncol = 17, nrow = 50))

colnames(tracks_df) <- names(tracks[[1]][[1]])

list <- unlist(tracks, recursive = FALSE)

tracks_df <- do.call("rbind", list) %>% as.data.frame


#get audio features

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

write.csv(tracks_df, "./data/global_top_50_song_data.csv")
write.csv(audio_features_df, "./data/global_top_50_song_audio_features.csv")

df<-subset(audio_features_df, select = c("danceability",	"energy","speechiness","acousticness", "valence"))
new_df<- data.frame(stringsAsFactors = F)
new_df<- rbind(df[2,],rep(0:5),rep(5:0))
