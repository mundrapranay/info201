library("spotifyr")
library("knitr")
library("tidyverse")
library("dplyr")
library("lubridate")
library("ggjoy")

Sys.setenv(SPOTIFY_CLIENT_ID = "d39d8c736e174cb18a58abec9ba00ec9")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "40f864e7d87c49dcb0e55ed42ada4e7d")

access_token <- get_spotify_access_token()

beatles <- get_artist_audio_features("the beatles")

get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()

get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 5) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()

joy <- get_artist_audio_features('joy division')

joy %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(5) %>% 
  kable()
