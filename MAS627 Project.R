
####### LIBRARIES ###########

library(spotifyr)
library(stringr)
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(tidyr)
library(purrr)


ls("package:spotifyr")

#######

#####TO-DO

# Increase font size of x and y axis labels
# change titles of visuals
# explain what the audio features mean. + why observe them throu8gh densityß



####### AUTHENTICATION ###########

client_id = Sys.setenv(SPOTIFY_CLIENT_ID = 'fbc194065b41426bade65bb1e1d8c819')
client_secret = Sys.setenv(SPOTIFY_CLIENT_SECRET = '326eb15d679b4da29bbfe0c1e1feae50')

access_token = get_spotify_access_token()


##########



####### GET PLAYLIST DATA ###########

playlist_username <- 'spotify'

all_out_10s <- get_playlist_audio_features(playlist_username, '37i9dQZF1DX5Ejj0EkURtP')

all_out_00s <- get_playlist_audio_features(playlist_username, '37i9dQZF1DX4o1oenSJRJd')

all_out_90s <- get_playlist_audio_features(playlist_username, '37i9dQZF1DXbTxeAdrVG2l')

all_out_80s <- get_playlist_audio_features(playlist_username, '37i9dQZF1DX4UtSsGT1Sbe')

combined_data <- bind_rows(
  data1 = all_out_80s,
  data2 = all_out_90s,
  data3 = all_out_00s,
  data4 = all_out_10s,
  .id = "dataset_name"
)


############


####### DATA CLEANING ###########


extract_artist_data <- function(data) {
  data %>%
    mutate(
      artist_name = sapply(track.album.artists, function(x) {
        if(length(x) >= 3) x[[3]] else NA
      }),
      combined_name = ifelse(lengths(artist_name) > 1, 
                             sapply(artist_name, paste, collapse = " & "), 
                             as.character(artist_name))
    )
}
all_out_10s = extract_artist_data(all_out_10s)
all_out_00s = extract_artist_data(all_out_00s)
all_out_90s = extract_artist_data(all_out_90s)
all_out_80s = extract_artist_data(all_out_80s)
combined_data = extract_artist_data(combined_data)

all_out_10s = all_out_10s %>%
  select(
    playlist_id,
    playlist_name,
    Danceability = danceability,
    Energy = energy,
    Loudness = loudness,
    Speechiness = speechiness,
    Acousticness = acousticness,
    Instrumentalness = instrumentalness,
    Liveness = liveness,
    Valence = valence,
    Tempo = tempo,
    Track = track.name,
    track.popularity,
    track.album.name,
    Artist = combined_name
  )

all_out_00s = all_out_00s %>%
  select(
    playlist_id,
    playlist_name,
    Danceability = danceability,
    Energy = energy,
    Loudness = loudness,
    Speechiness = speechiness,
    Acousticness = acousticness,
    Instrumentalness = instrumentalness,
    Liveness = liveness,
    Valence = valence,
    Tempo = tempo,
    Track = track.name,
    track.popularity,
    track.album.name,
    Artist = combined_name
  )

all_out_90s = all_out_90s %>%
  select(
    playlist_id,
    playlist_name,
    Danceability = danceability,
    Energy = energy,
    Loudness = loudness,
    Speechiness = speechiness,
    Acousticness = acousticness,
    Instrumentalness = instrumentalness,
    Liveness = liveness,
    Valence = valence,
    Tempo = tempo,
    Track = track.name,
    track.popularity,
    track.album.name,
    Artist = combined_name
  )

all_out_80s = all_out_80s %>%
  select(
    playlist_id,
    playlist_name,
    Danceability = danceability,
    Energy = energy,
    Loudness = loudness,
    Speechiness = speechiness,
    Acousticness = acousticness,
    Instrumentalness = instrumentalness,
    Liveness = liveness,
    Valence = valence,
    Tempo = tempo,
    Track = track.name,
    track.popularity,
    track.album.name,
    Artist = combined_name
  )

combined_data = combined_data %>%
  select(
    playlist_id,
    playlist_name,
    Danceability = danceability,
    Energy = energy,
    Loudness = loudness,
    Speechiness = speechiness,
    Acousticness = acousticness,
    Instrumentalness = instrumentalness,
    Liveness = liveness,
    Valence = valence,
    Tempo = tempo,
    Track = track.name,
    track.popularity,
    track.album.name,
    Artist = combined_name
  )



#################################



#######
# UI (all the stuff we see on the app)
#########
ui = fluidPage(
  
  titlePanel("Spotify Music Data Visualization with Shiny"),
  
  tabsetPanel(
    
    tabPanel('Top 10 Songs by Decade',
             fluidRow(
               column(4, selectInput("dataset2", 
                                     "Choose a Spotify Playlist:", 
                                     choices = c("All Out 80s" = "all_out_80s", 
                                                 "All Out 90s" = "all_out_90s", 
                                                 "All Out 2000s" = "all_out_00s", 
                                                 "All Out 2010s" = "all_out_10s"),
                                     selected = "all_out_80s")
               )
             ),
             plotOutput("topArtistsPlot")
    ),
    
    tabPanel('Density Plots by Track Audio Features',
             fluidRow(
               column(4, selectInput("dataset1", 
                                     "Choose a Spotify Playlist:", 
                                     choices = c("All Out 80s to 2010s" = "combined_data",
                                                 "All Out 80s" = "all_out_80s", 
                                                 "All Out 90s" = "all_out_90s", 
                                                 "All Out 2000s" = "all_out_00s", 
                                                 "All Out 2010s" = "all_out_10s"
                                                 ),
                                     selected = "combined_data")
               ),
               column(4, selectInput('xvar', 
                                     'X-Axis Variable', 
                                     choices=c('Danceability', 
                                               'Energy', 
                                               'Loudness',
                                               'Speechiness',
                                               "Acousticness",
                                               "Liveness",
                                               "Valence",
                                               "Tempo"), 
                                     selected='Danceability')),
             ),
             plotOutput("densityPlot")
    ),
    
    tabPanel('Playlist Data',
             fluidRow(
               column(4, selectInput("dataset3", 
                                     "Choose a Spotify Playlist:", 
                                     choices = c("All Out 80s" = "all_out_80s", 
                                                 "All Out 90s" = "all_out_90s", 
                                                 "All Out 2000s" = "all_out_00s", 
                                                 "All Out 2010s" = "all_out_10s"),
                                     selected = "all_out_80s")
             ),
             dataTableOutput("source_data")
    )
    )))

  


#################
# SERVER (build all the things we want to see on the app)
##############

server <- function(input, output) {
  
  
  # Artist with Most Popular Songs
  output$topArtistsPlot <- renderPlot({

    data_selected <- switch(input$dataset2,
                            "all_out_80s" = all_out_80s,
                            "all_out_90s" = all_out_90s,
                            "all_out_00s" = all_out_00s,
                            "all_out_10s" = all_out_10s)
    
    
    data_selected %>%
      group_by(Track) %>%
      summarise(avg_popularity = mean(track.popularity, na.rm = TRUE)) %>%
      arrange(desc(avg_popularity)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x =avg_popularity, y = reorder(Track, avg_popularity))) +  # Reorder artists by popularity
      geom_segment(aes(x = 80, xend = avg_popularity, yend = reorder(Track, avg_popularity)), color = "gray") +
      geom_point() +
      labs(title = "Top 10 Songs by Decade",
           x = "Popularity Index (Scale: 0-100, Displayed: 80-100",
           y = "Song") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12)
      ) +
      scale_x_continuous(breaks = seq(80,100,1))
    
  })
  
  # user defined density plot
  output$densityPlot <- renderPlot({
    
    data_selected <- switch(input$dataset1,
                            "all_out_80s" = all_out_80s,
                            "all_out_90s" = all_out_90s,
                            "all_out_00s" = all_out_00s,
                            "all_out_10s" = all_out_10s,
                            "combined_data" = combined_data)
    

    ggplot(data_selected, 
           aes(x=!!sym(input$xvar), fill=playlist_name,text = paste(playlist_name)))+
      geom_density(alpha=0.7, color=NA)+
      labs(x=input$xvar, y="Density") +
      guides(fill=guide_legend(title="Playlist"))+
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12)
      ) +
      ggtitle("Distribution of Audio Feature Data")
    
    
  })
  
  # output source data
  output$source_data <- renderDataTable({
    switch(input$dataset3,
           "all_out_80s" = all_out_80s,
           "all_out_90s" = all_out_90s,
           "all_out_00s" = all_out_00s,
           "all_out_10s" = all_out_10s,
           "combined_data" = combined_data)
  })
    
  
  

  
}





shinyApp(ui, server)


