library(shiny)
library(shinythemes)


data <- read.csv("./data/global_top_50_song_audio_features.csv")

ranks <- c("-", as.vector(data$rank))
name <- c("-", as.vector(paste(data$name, data$artist, sep=" by ")))

          
ui <- fluidPage(
  
  ## Dark background with white text
  theme = shinytheme("darkly"),
  
  ## Title
  navbarPage("Audio Analysis of the Global Top 50 Songs on Spotify",
             
             ## First page; looks at main audio factors (danceability, energy, valence, and tempo).
             ## Makes four scatterplots with lines of regression to show trends.
             tabPanel("Audio Factors Summary", 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("range", label = h3("Select Sample Size:"), 
                                      min = 1, max = nrow(data),value = c(20, 30), round = TRUE, step = 1),
                          p("Danceability = Describes how suitable a track is for dancing based on a combination
                            of musical elements including tempo, rhythm stability, beat strength, and 
                            overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.", 
                            style = "font-family: 'arial'; font-si16pt"),
                          p("Valence = A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.
                            Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), 
                            while tracks with low valence sound more negative (e.g. sad, depressed, angry).", 
                            style = "font-family: 'arial'; font-si16pt"),
                          p("Energy = A measure from 0.0 to 1.0 and represents a perceptual measure of 
                            intensity and activity. Typically, energetic tracks feel fast, loud, and noisy.", 
                            style = "font-family: 'arial'; font-si16pt"),
                          p("Tempo = The overall estimated tempo of a track in beats per minute (BPM).", 
                            style = "font-family: 'arial'; font-si16pt")
                        ), mainPanel("Summary of Selected songs from Spotify's Global Top 50",
                                     fluidRow(
                                       splitLayout(column(12,plotOutput("barGraph"), plotOutput("barGraph2")), 
                                                   column(12,plotOutput("barGraph3"), plotOutput("barGraph4")))
                                     ))
                      )), 
             
             ## Other pertinent (or otherwise) factors affecting why a song might be on the 
             ## Top fifty charts on Spotify. Looks at loudness, liveness, duration, and speechiness.
             ## Makes scatterplots like above.
             tabPanel("Other Factors", sidebarLayout(
               sidebarPanel(
                 sliderInput("range2", label = h3("Select Sample Size:"), 
                             min = 1, max = nrow(data),value = c(20, 30), round = TRUE, step = 1),
                 p("Loudness = Relative loudness of a given song", style = "font-family: 'arial'; font-si-16pt"),
                 p("Speechiness = How much the song resembles normal speech/no instruments/music.", style = "font-family: 'arial'; font-si-16pt"),
                 p("Length = How long the song is in milliseconds", style = "font-family: 'arial'; font-si-16pt"),
                 p("Liveness = How lively the song feels", style = "font-family: 'arial'; font-si-16pt")
                 
               ),
               
               mainPanel("Summary of Other Factors from the Top 50",
                         fluidRow(
                           splitLayout(column(12,plotOutput("scatterplot1"), plotOutput("scatterplot2")), 
                                       column(12,plotOutput("scatterplot3"), plotOutput("scatterplot4")))
                         ))
             )),
             tabPanel("World Analyses",
                      plotOutput('worldmap'),
                      hr(),
                      fluidRow(
                        column(4,
                               wellPanel(
                                 selectInput('factor', label = h3("Select the factor you'd like to see global information for."),
                                                         choices = c("-", "Danceability", "Valence", "Tempo", "Energy", "Acousticness", 
                                                                     "Speechiness", "Length", "Liveness"), selected = 1)
                               ))
                      ),
                      textOutput('worldanalysis')
                    ),
             ## Allows the user to select a single song from a drop-down list of songs and look at
             ## five of its individual song statistics. Danceability, valence, energy, acousticness, and
             ## speechiness.
             tabPanel("Single Song Analyses", 
                      sidebarPanel(
                        selectInput("song", label = h3("Select song in the Top 50:"), 
                                    choices = name,
                                    selected = 1),
                        p("Danceability = Describes how suitable a track is for dancing based on a combination
                            of musical elements including tempo, rhythm stability, beat strength, and 
                            overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.", 
                          style = "font-family: 'arial'; font-si16pt"),
                        p("Valence = A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.
                            Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), 
                            while tracks with low valence sound more negative (e.g. sad, depressed, angry).", 
                          style = "font-family: 'arial'; font-si16pt"),
                        p("Energy = A measure from 0.0 to 1.0 and represents a perceptual measure of 
                            intensity and activity. Typically, energetic tracks feel fast, loud, and noisy.", 
                          style = "font-family: 'arial'; font-si16pt"),
                        p("Acousticness = A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 
                          1.0 represents high confidence the track is acoustic. ", 
                          style = "font-family: 'arial'; font-si16pt"),
                        p("Speechiness = detects the presence of spoken words in a track. The more exclusively 
                          speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. ", 
                          style = "font-family: 'arial'; font-si16pt")
                      ),
                      mainPanel("Radar Charts of Song Audio Factors",
                                plotOutput("radarChart"))
             )

    )
  
)


