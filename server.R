library(shiny)
library(stringr)
library(rsconnect)
library(ggplot2)
library(fmsb)
library(maps)
library(dplyr)
library(plotly)

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
    ggplot(other_sub(), aes(x = other_sub()$rank, y=(other_sub()$duration_ms)/1000)
           , fill=x) + geom_point(stat="identity") + geom_smooth(method='lm') +
      labs(title="Length", x ="Song Ranks", y = "Length (s)") +
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
  
  ## This part of the code renders a map that plots the average factors of the songs that are in the
  ## Top 50 in countries across the world.
  
  
  output$worldmap <- renderPlot ({
    
    averages <- data.frame('region' = country_data$country, stringsAsFactors = FALSE)
    averages$stat <- country_data$tempo
    check <- input$factor
    
    print(input$factor)
    
    if(!(check == '-' || is.null(check))) {
      if(check == 'Danceability') {
        averages$stat <- country_data$danceability
      } else if(check == 'Liveness') {
        averages$stat <- country_data$liveness
      } else if(check == 'Valence') {
        averages$stat <- country_data$valence
      } else if(check == 'Tempo') {
        averages$stat <- country_data$tempo
      } else if(check == 'Energy') {
        averages$stat <- country_data$energy
      } else if(check == 'Acousticness') {
        averages$stat <- country_data$acousticness
      } else if(check == 'Speechiness') {
        averages$stat <- country_data$speechiness
      } else {
        averages$stat <- country_data$length/1000
      }
      
      world_graph <- left_join(world, averages, by = 'region')
      
      if(check == 'Length') {
        lt <- 'Duration in Seconds'
      } else {
        lt <- check
      }
      
      ggplot(world_graph, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = world_graph$stat)) + labs(fill = lt, title = str_c(check, 
            " Plotted Across the World (average of top 50 from a given country)")) +
        theme(plot.title = element_text(color = "black", size = 20, face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) + scale_fill_gradient(low = "#d70004", high = "#0db3f4", na.value = 'white')
    } else {
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = 'lightgray', color = 'white') +
        labs(title = 'World Map') +
        theme(
          plot.title = element_text(color = "black", size = 20, face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
          )
    }
  })
  
  output$worldanalysis <- renderText ({
    if(input$factor == 'Danceability') {
      paste0("This graph shows an interesting effect. Many of the top fifty songs in countries closer to China have 
             relatively much lower danceability scores than countries like America or Canada. This is 
             likely due to cultural differences between these countries. It almost seems as though it's 
             correlated to the level of English proficiency in the country, with countries that have little 
             exposure to English seeming to be more likely to have a lower danceability score.")
    } else if(input$factor == 'Valence') {
      paste0("As we've all suspected, America is in love with sad songs. However, aside from that, 
             it looks like there's not much correlation between geographic position/culture and valence 
             level. Having said that, one thing that's interesting is that, despite many of the countries 
             preferring songs that have valence levels of .55 or below, the graph that looks at the 
             valence levels of the top fifty worldwide songs shows very clearly that music consumers today 
             listen to more positive music!")
    } else if(input$factor == 'Tempo') {
      paste0("This graph correlates somewhat to the valence graph. Slower songs are well-known to sound 
             less positive, and what shows up is that countries that prefer slower songs also have a 
             lot of lower-valence songs in their top fifty lists.")
    } else if(input$factor == 'Energy') {
      paste0("And another graph that is easily correlated with the valence graph. This one is moreso 
             easily correlated than the tempo graph because this graph has low-energy songs in many of 
             the same places as low-valence songs. This makes sense, as low-valence songs tend to have 
             less energy. Though it's not a perfect comparison, as some low-valence songs are angry songs, 
             and those can have a lot of energy.")
    } else if(input$factor == 'Acousticness') {
      paste0("It's somewhat difficult to correlate this graph to anything, but if an analysis were to be 
             made, it might look at how many of the countries that have been 'modern' for longer have a 
             feeling for less acoustic music. This makes sense in America because we've been hearing a 
             lot more synthetic/electronic stuff lately, and also there has been an uprising of heavily 
             autotuned rap in the popular genres of music.")
    } else if(input$factor == 'Speechiness') {
      paste0("It's interesting that a lot of the countries near China are similar in their tastes. Very often, 
             they are grouped together when it comes to these factors. This is no expection. Notice how they're 
             all some of the lowest scoring for the speech factor?")
    } else if(input$factor == 'Length') {
      paste0("Most countries agree on this, with relatively small variation between them. Japan, on the other 
             hand, enjoys songs that are much longer than the worldwide average. This is most likely an outlier, 
             though it would be interesting to plot Japan's preference for music in terms of duration over a long 
             period of time to see if it's always an outlier. It's just so far out there that it's very surprising.")
    } else if(input$factor == 'Liveness') {
      paste0("This graph and the length graph both have significant outliers. All other countries agree heartily on 
             what constitutes a good average liveliness for their songs, but Brazil's top 50 songs have much higher 
             than average liveness scores. It would be interesting to look at this over time.")
    } else {
      paste0("Choose one of the options above to view a map about that factor! This map looks at the average level of 
             aforementioned audio factor in countries across the world, though data for certain countries is missing, 
             likely because Spotify isn't streamed there.")
    }
  })
}

