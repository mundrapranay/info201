library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# Spotify Credentials
clientID <- "d39d8c736e174cb18a58abec9ba00ec9"
secret <- "40f864e7d87c49dcb0e55ed42ada4e7d"
#Makes the request to Spotify for the API token
response <- POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

#Extracts and stores the API token and then creates the authorization header
token <- content(response)$access_token
authorization.header <- paste0("Bearer ", token)

# get the top artists and filter over their major keys and tempos and other audio features of the tracks