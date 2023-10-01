

# TODAY:
#   3 levels of interactivity
#     - 1) Changing aesthetics
#     - 2) Changing columns selected
#     - 3) Filtering rows (maybe, this one is tough, we might save for Day 11)
#
#   Basic formatting
#     - sidebarlayout() today, I'll provide templates for some other stuff



#library(stringr)
#str_subset(ls('package:shiny'), 'render')
#str_subset(ls('package:shiny'), 'Output')
#str_subset(ls('package:shiny'), 'Input')


library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(DT)

library(purrr)
library(knitr)

library(spotifyr)

# Authentication
client_id = Sys.setenv(SPOTIFY_CLIENT_ID = 'fbc194065b41426bade65bb1e1d8c819')
client_secret = Sys.setenv(SPOTIFY_CLIENT_SECRET = '326eb15d679b4da29bbfe0c1e1feae50')

access_token = get_spotify_access_token()

get_spotify_authorization_code()

#Get Artist Data

beatles <- get_artist_audio_features('the beatles')

radiohead <- get_artist_audio_features('radiohead')

beatle_tracks = get_album_tracks('7C221PnWhYGv8Tc0xSbfdc')

install.packages("tidyverse")
library(tidyverse)
library(knitr)

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

library(ggjoy)

ggplot(joy, aes(x = valence, y = album_name)) + 
  geom_joy() + 
  theme_joy() +
  ggtitle("Joyplot of Joy Division's joy distributions", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")


# plot 2 - scatterplot of profit by sales, color by region (level 2: let user change x, y, color)
# maybe let them update point size as well?



#######
# UI (all the stuff we see on the app)
#########
ui = fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput('bar_fill', 'Bar Color', choices = colors(), selected = ''),
      selectInput('bar_color', 'Border Color', choices = colors(), selected = 'black'),
      selectInput('xvar', 'X-Axis?', choices = c('Sales', 'Profit', 'Discount', 'Quantity')),
      selectInput('yvar', 'Y-Axis?', choices = c('Sales', 'Profit', 'Discount', 'Quantity'), selected = 'Profit'),
      selectInput('colorvar', 'Color By?', choices = c('Segment', 'Region', 'Category')),
      
      radioButtons('sales_year', 'Sales Year', choices = c('All',2016, 2017, 2018, 2019)),
    ),
    
    mainPanel(
      plotOutput('sales_bar'),
      plotOutput('scatter'),
      dataTableOutput('source_data'),
    )
  ) #closes sidebar layout
  
)

# 3) add inputs for the user to interact with (using *Input)
# 2) display whatever we build (using *Output)

#################
# SERVER (build all the things we want to see on the app)
##############

server = function(input, output){
  # 1) build something to display (using render*)
  # Sales Bar Plot
  output$sales_bar = renderPlot({
    
    rows_i_want = TRUE
    
    if(input$sales_year != 'All'){rows_i_want = rows_i_want & d$Year == input$sales_year}
    
    d[rows_i_want ,] %>%
      group_by(State) %>%
      summarise(Sales = sum(Sales)) %>%
      top_n(10) %>%
      ggplot(aes(y=reorder(State, Sales), x=Sales)) +
      geom_col(fill = input$bar_fill, color = input$bar_color) +
      labs(y='') + 
      scale_x_continuous('Total Sales', labels=dollar) + 
      theme(
        axis.text = element_text(face='bold', size=18),
        text = element_text(face='bold', size=18)
      )
  })
  
  ?geom_col
  
  # User defined scatterplot
  output$scatter = renderPlot(
    ggplot(d, aes_string(x=input$xvar, y=input$yvar, color=input$colorvar)) +
      geom_point()
  )
  
  # output source data
  output$source_data = DT::renderDataTable(d)
  
}



shinyApp(ui, server)


