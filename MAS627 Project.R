#str_subset(ls('package:shiny'), 'render')
#str_subset(ls('package:shiny'), 'Output')
#str_subset(ls('package:shiny'), 'Input')


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

###############################



####### AUTHENTICATION ###########

client_id = Sys.setenv(SPOTIFY_CLIENT_ID = 'fbc194065b41426bade65bb1e1d8c819')
client_secret = Sys.setenv(SPOTIFY_CLIENT_SECRET = '326eb15d679b4da29bbfe0c1e1feae50')

access_token = get_spotify_access_token()


###############################



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



all_out_00s$track.album.artists[[1]][[3]]

all_out_00s$extracted_data <- sapply(all_out_00s$track.album.artists, function(x) {
  if(length(x) >= 3) x[[3]] else NA
})

all_out_00s$extracted_data


####################################


####### DATA CLEANING ###########

####################################


############ DATA VIZ - DENSITY x DANCEABILITY ########################


all_out_00s %>%
  group_by(extracted_data) %>%
  summarise(song_count = n()) %>%
  arrange(desc(song_count)) %>%
  ggplot(aes(x = reorder(extracted_data, -song_count), y = song_count)) +  # Reorder artists by song count
  geom_col() +
  labs(title = "Number of Songs by Artist in All Out 00s Playlist",
       x = "Artist",
       y = "Number of Songs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



############ DATA VIZ - DENSITY x DANCEABILITY ########################

  playlist_audio_features %>% ggplot(aes(x=danceability, fill=playlist_name,
                              text = paste(playlist_name)))+
    geom_density(alpha=0.7, color=NA)+
    scale_fill_manual(values=c('skyblue'))+
    labs(x="Danceability", y="Density") +
    guides(fill=guide_legend(title="Playlist"))+
    theme_minimal()+
    ggtitle("Distribution of Danceability Data") + 
    geom_rug(alpha=0.5, sides="b") +
    geom_vline(data= . %>% group_by(playlist_name) %>% 
                   summarize(mean_danceability=mean(danceability)),
                 aes(xintercept=mean_danceability), linetype="dashed", color="blue", alpha=0.7)
  

####################################





#######
# UI (all the stuff we see on the app)
#########
ui = fluidPage(
  
  titlePanel("Density Visualization with Shiny"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("dataset", 
                  "Choose a dataset:", 
                  choices = c("all_out_80s", "all_out_90s", "all_out_00s", "all_out_10s"))
    ),
    
    mainPanel(
      plotOutput("densityPlot")
    )
  ) #closes sidebar layout
  
)


#################
# SERVER (build all the things we want to see on the app)
##############

server <- function(input, output) {
  
  output$densityPlot <- renderPlot({
    # Depending on input, select dataset
    
    data_selected <- switch(input$dataset,
                            "all_out_80s" = all_out_80s,
                            "all_out_90s" = all_out_90s,
                            "all_out_00s" = all_out_00s,
                            "all_out_10s" = all_out_10s)
    
    # Create plot
    ggplot(data_selected ,aes(x=danceability, fill=playlist_name,
                                           text = paste(playlist_name)))+
      geom_density(alpha=0.7, color=NA)+
      labs(x="Danceability", y="Density") +
      guides(fill=guide_legend(title="Playlist"))+
      theme_minimal()+
      ggtitle("Distribution of Danceability Data") + 
      geom_rug(alpha=0.5, sides="b") +
      geom_vline(data= . %>% group_by(playlist_name) %>% 
                   summarize(mean_danceability=mean(danceability)),
                 aes(xintercept=mean_danceability), linetype="dashed", color="blue", alpha=0.7)
  })
  
}




shinyApp(ui, server)


