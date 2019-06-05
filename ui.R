library(shiny)
library(shinythemes)


data <- read.csv("./data/global_top_50_song_audio_features.csv")

ranks <- c("-", as.vector(data$rank))
name <- c("-", as.vector(paste(data$name, data$artist, sep=" by ")))

          
ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  
  navbarPage("Audio Analysis of the Global Top 50 Songs on Spotify",
             tabPanel("Summary", 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("range", label = h3("Select Sample Size:"), 
                                      min = 1, max = nrow(data),value = c(20, 30), round = TRUE, step = 1),
                          p("Danceability = describes how suitable a track is for dancing based on a combination
                            of musical elements including tempo, rhythm stability, beat strength, and 
                            overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.", 
                            style = "font-family: 'arial'; font-si16pt"),
                          p("Valence = A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.
                            Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), 
                            while tracks with low valence sound more negative (e.g. sad, depressed, angry).", 
                            style = "font-family: 'arial'; font-si16pt"),
                          p("Energy = a measure from 0.0 to 1.0 and represents a perceptual measure of 
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
             tabPanel("Single Song", 
                      sidebarPanel(
                        selectInput("song", label = h3("Select song in the Top 50:"), 
                                    choices = name,
                                    selected = 1),
                        p("Danceability = describes how suitable a track is for dancing based on a combination
                            of musical elements including tempo, rhythm stability, beat strength, and 
                            overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.", 
                          style = "font-family: 'arial'; font-si16pt"),
                        p("Valence = A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.
                            Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), 
                            while tracks with low valence sound more negative (e.g. sad, depressed, angry).", 
                          style = "font-family: 'arial'; font-si16pt"),
                        p("Energy = a measure from 0.0 to 1.0 and represents a perceptual measure of 
                            intensity and activity. Typically, energetic tracks feel fast, loud, and noisy.", 
                          style = "font-family: 'arial'; font-si16pt"),
                        p("Acousticness = A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 
                          1.0 represents high confidence the track is acoustic. ", 
                          style = "font-family: 'arial'; font-si16pt"),
                        p("Speechiness = detects the presence of spoken words in a track. The more exclusively 
                          speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. ", 
                          style = "font-family: 'arial'; font-si16pt")
                      ),
                      mainPanel("mainpanel2",
                                plotOutput("radarChart"))
             )

    )
  
)


