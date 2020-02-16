library(shiny)
library(ggplot2)
library(dplyr)
library(mlr)
library(DT)

chill3.0 = readRDS('chill3.0.rds', refhook = NULL)

ui = pageWithSidebar(
  
  # App title ----
  headerPanel('Spotify Playlist Analysis'),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    selectInput('var_x', 'X Variable:', 
                c('danceability' = 'danceability','energy' = 'energy','loudness' = 'loudness', 'speechiness' = 'speechiness', 'acousticness' = 'acousticness','instrumentalness' = 'instrumentalness','liveness' = 'liveness','valence' = 'valence', 'tempo' = 'tempo')),
    selectInput('var_y', 'Y Variable:', 
                c('danceability' = 'danceability','energy' = 'energy','loudness' = 'loudness', 'speechiness' = 'speechiness', 'acousticness' = 'acousticness','instrumentalness' = 'instrumentalness','liveness' = 'liveness','valence' = 'valence', 'tempo' = 'tempo')),
    
    selectInput('var_k', '# Clusters (k):', 
                c('2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7)),
    selectInput('view_k', 'Cluster to List Songs:', 
                c('1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5, '6' = 6, '7' = 7))
  ),
  
  mainPanel(
    h2('Clustering of my "Chill" Playlist'),
    
    h3(textOutput('caption')),
    
    plotOutput('clusterPlot'), #,
    #tableOutput('clusterTable')
    dataTableOutput('clusterTable')
  )
)

server = function(input, output) {
  
  formulaText = reactive({
    paste(input$var_y,'~', input$var_x)
  })
  
  output$caption = renderText({
    formulaText()
  })
    
  chill4.0 = reactive({
    chill.kmeans = chill3.0 %>% select('danceability', 'energy', 'loudness',  'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo') %>% 
      normalizeFeatures(method = 'standardize') %>%
      kmeans(centers = as.numeric(input$var_k), nstart = 25)
    cbind(chill3.0,cluster = chill.kmeans$cluster)
  })

  output$clusterPlot = renderPlot({
    chill4.0() %>% ggplot(aes_string(x = input$var_x, y = input$var_y, color = as.factor(chill4.0()$cluster))) +
      geom_point() #color = paste('~',as.factor(cluster))
  })
  
  output$clusterTable = renderDataTable({
    chill4.0() %>% select('artist', 'tracks', 'cluster') %>% filter(cluster == input$view_k) %>% arrange(artist)
    
  })
}

shinyApp(ui, server)