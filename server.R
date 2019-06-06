library(shiny)
library(stringr)
library(rsconnect)
library(ggplot2)
library(fmsb)
library(maps)

world <- map_data('world')

data <-read.csv("./data/global_top_50_song_audio_features.csv")
source('./countryprocessing.R')

server <- function(input, output) {
  data_subset <- reactive({
      partial_data <- data[input$range[1]:input$range[2],]
  })
  
  song_subset <- reactive({
    df<-subset(data, select = c("danceability",	"energy","speechiness","acousticness", "valence"))
    new_df<- data.frame(stringsAsFactors = F)
    
    if (input$song != "-") {
      value <- str_split(input$song, " by ")[[1]][1]
      num <- data$rank[which(data$name == value)]
      new_df<- rbind(c(0,0,0,0,0), c(1,1,1,1,1), df[num,])
    } else {
      new_df<- rbind(c(0,0,0,0,0), c(1,1,1,1,1),  df["-",])
    }
   
  })

  
  output$barGraph <- renderPlot({
    ggplot(data_subset(), aes(x = data_subset()$rank, y=data_subset()$danceability)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Danceability", x ="Song Ranks", y = "Score") +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold"),
        plot.caption = element_text(color = "black", size = 12, face = "italic"),
        axis.text.x = element_text(color = "black", size = 12, angle = 60,
                                   hjust = 1.1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 12, angle = 0,
                                   hjust = 1, vjust = 0),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, 
                                    hjust = .5, vjust = 0),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, 
                                    hjust = .5, vjust = .5)
      )

  })
  
  output$barGraph2 <- renderPlot({
    ggplot(data_subset(), aes(x = data_subset()$rank, y=data_subset()$valence)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Valence", x ="Song Ranks", y = "Score") +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold"),
        plot.caption = element_text(color = "black", size = 12, face = "italic"),
        axis.text.x = element_text(color = "black", size = 12, angle = 60,
                                   hjust = 1.1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 12, angle = 0,
                                   hjust = 1, vjust = 0),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, 
                                    hjust = .5, vjust = 0),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, 
                                    hjust = .5, vjust = .5)
      )
  })
  
  output$barGraph3 <- renderPlot({
    ggplot(data_subset(), aes(x = data_subset()$rank, y=data_subset()$energy)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Energy", x ="Song Ranks", y = "Score") +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold"),
        plot.caption = element_text(color = "black", size = 12, face = "italic"),
        axis.text.x = element_text(color = "black", size = 12, angle = 60,
                                   hjust = 1.1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 12, angle = 0,
                                   hjust = 1, vjust = 0),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, 
                                    hjust = .5, vjust = 0),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, 
                                    hjust = .5, vjust = .5)
      )
    
  })
  
  output$barGraph4 <- renderPlot({
    ggplot(data_subset(), aes(x = data_subset()$rank, y=data_subset()$tempo)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Tempo", x ="Song Ranks", y = "Score", 
           caption = paste("Selected subset contains", 
                           input$range[2]-input$range[1], "observations", sep=" ")) +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold"),
        plot.caption = element_text(color = "black", size = 12, face = "italic"),
        axis.text.x = element_text(color = "black", size = 12, angle = 60,
                                   hjust = 1.1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 12, angle = 0,
                                   hjust = 1, vjust = 0),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, 
                                    hjust = .5, vjust = 0),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, 
                                    hjust = .5, vjust = .5)
      )
  })
  
  output$radarChart <- renderPlot({
    radarchart(song_subset(), centerzero=TRUE, axistype=1,
               pcol=rgb(0.3,0.6,0.8,0.8),pfcol=rgb(0.3,0.6,0.8,0.8), 
               cglcol=rgb(0.6,0.6,0.6), cglty=1,axislabcol=rgb(0.6,0.6,0.6))
  })
  
  other_sub <- reactive({
    pd <- data[input$range2[1]:input$range2[2],]
  })
  
  output$scatterplot1 <- renderPlot({
    ggplot(other_sub(), aes(x = other_sub()$rank, y=other_sub()$liveness)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Liveness", x ="Song Ranks", y = "Liveness") +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold"),
        plot.caption = element_text(color = "black", size = 12, face = "italic"),
        axis.text.x = element_text(color = "black", size = 12, angle = 60,
                                   hjust = 1.1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 12, angle = 0,
                                   hjust = 1, vjust = 0),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, 
                                    hjust = .5, vjust = 0),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, 
                                    hjust = .5, vjust = .5)
      )
    
  })
  
  output$scatterplot2 <- renderPlot({
    ggplot(other_sub(), aes(x = other_sub()$rank, y=other_sub()$duration_ms)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Length", x ="Song Ranks", y = "Length") +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold"),
        plot.caption = element_text(color = "black", size = 12, face = "italic"),
        axis.text.x = element_text(color = "black", size = 12, angle = 60,
                                   hjust = 1.1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 12, angle = 0,
                                   hjust = 1, vjust = 0),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, 
                                    hjust = .5, vjust = 0),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, 
                                    hjust = .5, vjust = .5)
      )
  })
  
  output$scatterplot3 <- renderPlot({
    ggplot(other_sub(), aes(x = other_sub()$rank, y=other_sub()$loudness)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Loudness", x ="Song Ranks", y = "Loudness") +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold"),
        plot.caption = element_text(color = "black", size = 12, face = "italic"),
        axis.text.x = element_text(color = "black", size = 12, angle = 60,
                                   hjust = 1.1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 12, angle = 0,
                                   hjust = 1, vjust = 0),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, 
                                    hjust = .5, vjust = 0),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, 
                                    hjust = .5, vjust = .5)
      )
    
  })
  
  output$scatterplot4 <- renderPlot({
    ggplot(other_sub(), aes(x = other_sub()$rank, y=other_sub()$speechiness)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Speechiness", x ="Song Ranks", y = "Speechiness") +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold"),
        plot.caption = element_text(color = "black", size = 12, face = "italic"),
        axis.text.x = element_text(color = "black", size = 12, angle = 60,
                                   hjust = 1.1, vjust = 1),
        axis.text.y = element_text(color = "black", size = 12, angle = 0,
                                   hjust = 1, vjust = 0),  
        axis.title.x = element_text(color = "black", size = 18, angle = 0, 
                                    hjust = .5, vjust = 0),
        axis.title.y = element_text(color = "black", size = 18, angle = 90, 
                                    hjust = .5, vjust = .5)
      )
  })
  
  ## Currently just renders a standalone map.
  
  output$worldmap <- renderPlot ({
    ggplot(world, aes(x = long, y = lat, group = group)) +
      geom_polygon(fill = 'lightgray', color = 'white')
  })
  
}

