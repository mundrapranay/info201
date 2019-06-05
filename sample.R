library(httr)
library(magrittr)
library(rvest)
library(ggplot2)

clientID = "d39d8c736e174cb18a58abec9ba00ec9"
secret = "40f864e7d87c49dcb0e55ed42ada4e7d"

response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

token = content(response)$access_token

authorization.header = paste0("Bearer ", token)

weekly.top.songs = read_html("https://spotifycharts.com/regional/global/weekly/latest") %>%
  html_nodes("#content > div > div > div > span > table > tbody > tr > td.chart-table-image > a")

id.start = regexpr("/track/", weekly.top.songs) 
id.end = regexpr('" target="', weekly.top.songs) 

top.song.ids = substr(weekly.top.songs, id.start+7, id.end-1)

tracks = lapply(1:200, function(n) {
  GET(url = paste("https://api.spotify.com/v1/tracks/", top.song.ids[n], sep = ""),
      config = add_headers(authorization = authorization.header))
})

features = lapply(1:200, function(n) {
  GET(url = paste0("https://api.spotify.com/v1/audio-features/", top.song.ids[n]),
      config = add_headers(authorization = authorization.header))
})

features.content = sapply(1:200, function(n) {
  content(features[[n]])
})

features.content = t(features.content)

features.df = cbind(rank = 1:200, rank.desc = 200:1, danceability = features.content[,1], 
                    energy = features.content[,2], key = features.content[,3], 
                    loudness = features.content[,4], mode = features.content[,5],
                    speechiness = features.content[,6], acousticness = features.content[,7],
                    instrumentalness = features.content[,8], liveness = features.content[,9],
                    valence = features.content[,10], tempo = features.content[,11], 
                    duration_ms = features.content[,17], time_signature = features.content[,18])

features.df = features.df %>% as.data.frame

for (i in 1:ncol(features.df)) {
  features.df[i,] = unlist(features.df[i])
}

head(features.df)


