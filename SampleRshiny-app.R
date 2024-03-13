## app.R ##
#Import all the required libraries.
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(ggpubr)

#Create ui for the shiny app.
ui <- dashboardPage(
  dashboardHeader(title = "Please change the tabs to browse the app.",titleWidth = 350),
  dashboardSidebar(width = 360,sidebarMenu( 
    #Create 6 different menu items/tabs for each visualisation and one main page.
    menuItem("Main page.", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Vis 1- Boxplot of games Genre v/s Global Sales", tabName = "dash1", icon = icon("dashboard")),
    menuItem("Vis 2- Regional sales v/s genre with Critic Score", tabName = "dash2", icon = icon("dashboard")),
    menuItem("Vis 3- Pie chart top 5 games for year and genre", tabName = "dash3", icon = icon("dashboard")),
    menuItem("Vis 4- Sankey diagram of top 4 games and their sequels", tabName = "dash4", icon = icon("dashboard")),
    menuItem("Vis 5- Multivariate plot of critic score v/s Sales v/s Year", tabName = "dash5", icon = icon("dashboard"))
  )
  ),
  dashboardBody(
    tabItems(
      # Main page tab content
      tabItem(tabName = "dashboard",
              h2("Welcome to my Data Visualisation application for VideoGames Sales data."),
              h3("Please change the tabs on the left to proceed further.")
                )
              
      ,
      # First tab content.
      tabItem(tabName = "dash1",
              h2("Visualisation of Genre v/s Global Sales over all the years(1980-2016) combined."),
              fluidRow(
                mainPanel(plotlyOutput("plot0", height = 500,width = 1000))
                
              )
      ),
      
      # Second tab content.
      tabItem(tabName = "dash2",
              h2("Visualisation of sales of different regions with their average critic-score."),
              fluidRow(
                mainPanel(plotlyOutput("plot2",height = 550,width = 1050))
                
              )
      ),
      # Third tab content.
      tabItem(tabName = "dash3",
              h2("Visualisation of top 5 games based on the Selected Year and Genre."),
              
              fluidRow(
                #plot the piechart.
                box(plotlyOutput("plot3",height = 'auto')),
                #Slider input menu
                box(sliderInput("year", "Please Select the Year.",
                            min = 1980, max = 2016,
                            value = 2005, step = 1, width = '700px',
                            animate = animationOptions(interval = 800, loop = FALSE)),
                    Genre_Names <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), c('Action','Adventure','Fighting','Misc','Platform','Puzzle','Racing','Role-Playing',
                                                                                       'Shooter','Simulation','Sports','Strategy')),
                selectInput("genre_selected", "Please select the genre. (NOTE: Some Genres don't have enough games for a year, in this case please change the Year or the Genre.)",
                            choices = names(Genre_Names)),
                
                )
              )
      ),
      # Fourth tab content.
      tabItem(tabName = "dash4",
              h2("Sankey diagram - Top 4 games(global sales perspective) of all times and their sequels"),
              fluidRow(
                mainPanel(plotlyOutput("plot4"))
                
              )
      ),
      # Fifth tab content.
      tabItem(tabName = "dash5",
              h2("Multivariate Analysis 3D plot - Year of Release v/s Critic score & Year of Release v/s Global Sales."),
              fluidRow(
                mainPanel(plotlyOutput("plot5",height = 550,width = 950))
                
              )
      )
    )
  )
)

server <- function(input, output) {
  
  #Output Plot 0.
  output$plot0 <- renderPlotly({
    gamedf <- read_csv("Video_Games_Sales.csv")
    genre_sales <- aggregate(x = gamedf$Global_Sales,                # Specify data column
                             by = list(gamedf$Genre,gamedf$Year_of_Release),              # Specify group indicator
                             FUN = sum,na.rm=TRUE) 
    #print("ok")
    ggplotly(
    ggplot(genre_sales,aes(x=reorder(Group.1,-x), y=x, fill=Group.1)) +
      geom_boxplot(outlier.size=10,outlier.colour="red") +
      stat_boxplot(geom = 'errorbar', width=0.2) + theme(legend.position="none")+
      theme(text = element_text(size = 12)) +
      ggtitle("Boxplot of different game Genres v/s Global Sales") +  xlab("Genre") + ylab("Global Sales(in Millions of Copies)")
    )
    })
  #Output Plot 2.
  output$plot2 <- renderPlotly({
    gamedf <- read_csv("Video_Games_Sales.csv")
    gamesdff <- na.omit(gamedf) #Omit na values for critic score data.
    #Create files for Critic score - average,sum of NA,EU,JP and Other regions Sales.
    games_critic_score <- aggregate(Critic_Score ~ Genre, gamesdff, mean)
    games_NA_Sales <- aggregate(NA_Sales ~ Genre, gamesdff, sum)
    games_EU_Sales <- aggregate(EU_Sales ~ Genre, gamesdff, sum)
    games_JP_Sales <- aggregate(JP_Sales ~ Genre, gamesdff, sum)
    games_Other_Sales <- aggregate(Other_Sales ~ Genre, gamesdff, sum)
    
    #Merge the data from previous steps.
    games_critic_data <- list(games_critic_score, games_NA_Sales, games_EU_Sales,games_JP_Sales,games_Other_Sales)
    games_critic_data %>% reduce(full_join, by='Genre')
    games_critic_data <- data.frame(games_critic_data)
    
    #Plot NA region graph - Genre v/s Sales v/s Critic Score.
    na <- ggplot(data=games_critic_data, aes(x=NA_Sales,y=reorder(Genre,NA_Sales),fill=Critic_Score)) +
      geom_bar(stat="identity")+
      ggtitle("NA Region - Global Sales and Critic Rating v/s Genre") +  ylab("Genre") + xlab("NA Sales(in Mn copies)")+ # scale_fill_grey() +
      theme_classic() #+ theme(legend.position = "none")
    #coord_flip()
    
    #Plot EU region graph - Genre v/s Sales v/s Critic Score.
    eu <- ggplot(data=games_critic_data, aes(x=EU_Sales,y=reorder(Genre,EU_Sales),fill=Critic_Score)) +
      geom_bar(stat="identity")+
      ggtitle("EU Region- Global Sales and Critic Rating v/s Genre") +  ylab("Genre") + xlab("EU Sales(in Mn copies)")+ # scale_fill_grey() +
      theme_classic() #+ theme(legend.position = "none")
    #coord_flip()
    
    #Plot JP region graph - Genre v/s Sales v/s Critic Score.
    jp <- ggplot(data=games_critic_data, aes(x=JP_Sales,y=reorder(Genre,JP_Sales),fill=Critic_Score)) +
      geom_bar(stat="identity")+
      ggtitle("Japan region - Global Sales and Critic Rating v/s Genre") +  ylab("Genre") + xlab("JP Sales(in Mn copies)")+ # scale_fill_grey() +
      theme_classic() #+ theme(legend.position = "none")
    #coord_flip()
    
    #Plot Other region graph - Genre v/s Sales v/s Critic Score.
    other <- ggplot(data=games_critic_data, aes(x=Other_Sales,y=reorder(Genre,Other_Sales),fill=Critic_Score)) +
      geom_bar(stat="identity")+
      ggtitle("Rest of World region - Global Sales and Critic Rating v/s Genre") +  ylab("Genre") + xlab("Rest of the world Sales(in Mn copies)")+ # scale_fill_grey() +
      theme_classic() #+ theme(legend.position = "none")
    
   
    #Use the ggplots in Ploty.
    fig1 <- ggplotly(na, tooltip = c("NA_Sales","Critic_Score"))
    fig2 <- ggplotly(eu, tooltip = c("EU_Sales","Critic_Score"))
    fig3 <- ggplotly(jp, tooltip = c("JP_Sales","Critic_Score"))
    fig4 <- ggplotly(other, tooltip = c("Other_Sales","Critic_Score"))
    
    #Create sections to paste the 4 region subplots in one page.
    fig <- subplot(fig1, fig2, fig3, fig4, nrows = 2, titleY = TRUE, titleX = TRUE, margin = 0.1 )
    fig <- fig %>%layout(title =  'Genre v/s Regional Sales v/s Critic Score of the 4 different Regions.',
                         plot_bgcolor='#e5ecf6', 
                         xaxis = list( 
                           zerolinecolor = '#ffff', 
                           zerolinewidth = 2, 
                           gridcolor = 'ffff'), 
                         yaxis = list( 
                           zerolinecolor = '#ffff', 
                           zerolinewidth = 2, 
                           gridcolor = 'ffff'))
    
    # Update title
    annotations = list( 
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "NA region",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.8,  
        y = 1,  
        text = "EU region ",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.2,  
        y = 0.4,  
        text = "Japan region ",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),
      list( 
        x = 0.8,  
        y = 0.4,  
        text = "Rest of the world region",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ))
    
    fig <- fig %>%layout(annotations = annotations) 
    fig
    })
    
    
    #Output Plot 3(Pie Chart).
    output$plot3 <- renderPlotly({
      Genre_Names <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), c('Action','Adventure','Fighting','Misc','Platform','Puzzle','Racing','Role-Playing',
                                                                         'Shooter','Simulation','Sports','Strategy'))
      gamedf <- read_csv("Video_Games_Sales.csv")
      Genre_Names <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), c('Action','Adventure','Fighting','Misc','Platform','Puzzle','Racing','Role-Playing',
                                                                         'Shooter','Simulation','Sports','Strategy'))
      top5games <- filter(gamedf,Year_of_Release == as.character(input$year)& Genre == input$genre_selected)
      top5 <- top5games[order(top5games$Global_Sales,decreasing = TRUE),]
      top5pie <- top5[1:5,]
      draw_plot3 <- function(top5pie) {
      fig <- plot_ly(top5pie, labels = ~Name, values = ~Global_Sales, type = 'pie',
                     textposition = 'inside',textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',text =~paste('</br> Name: ',Name,'</br> Global Sales(in Mn copies): ',Global_Sales,
                                                     '</br> Platform: ',Platform,'</br> Publisher: ',Publisher,
                                                     '</br> Critic Score: ',Critic_Score,'(Out of 100)','</br> User Score: ',User_Score,'(Out of 10)',
                                                     '</br> Rating: ',Rating),
                     insidetextorientation='horizontal',
                     showlegend = FALSE)
      fig <- fig %>% layout((title = "Top 5 games for selected year and Genre"),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      fig
      }
      draw_plot3(top5pie)
    })
    #Output Plot 4.
    output$plot4 <- renderPlotly({
      fig <- plot_ly(
        type = "sankey",
        orientation = "h",
        
        node = list(
          label = c("Pokemon Red","Mario Kart Wii","Super Mario Bros","Wii Sports", "Pokemon Gold", "Mario Kart 7","New Super Mario Bros", "Wii Sports Resort", "Pokemon Diamond","Mario Kart 8","New Super Mario-Wii","Wii Sports Club","None"),
          color = c("blue", "blue", "blue", "blue", "red", "red","red", "red","green", "green","green", "green","white"),
          pad = 5,
          thickness = 20,
          line = list(
            color = "black",
            width = 0.5
          )
        ),
        
        link = list(
          source = c(0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7),
          target = c(4,5,6,7,8,9,10,11,12,12,12,12,12,12,12,12),
          value =  c(2,4,6,8,2,2,5,4,3,4,5,6,7,8,2,2,2,2)
        )
      )
      fig <- fig %>% layout(
        #title = "Sankey Diagram - Top 4 games of all times and their sequels.",
        font = list(
          size = 10
        )
      )
      
      fig
    })
    
    #Output Plot 5.
    output$plot5 <- renderPlotly({
      #Read input file.
      gamedf <- read_csv("Video_Games_Sales.csv")
      #Omit na values to calculate average critic score.
      gamedf2 <- na.omit(gamedf)
      games_critic_score_year <- aggregate(Critic_Score ~ Year_of_Release, gamedf2, mean)
      games_global_Sales <- aggregate(Global_Sales ~ Year_of_Release, gamedf2, sum)
      
      #Create file for multivariate analysis x,y,z axis.
      games_multivariate_data <- list(games_critic_score_year, games_global_Sales) 
      games_multivariate_data %>% reduce(full_join)#join critic score and global sales data.
      games_multivariate_data <- data.frame(games_multivariate_data)
      
      
      fig <- plot_ly(games_multivariate_data, x = ~Year_of_Release, y = ~Global_Sales, z = ~Critic_Score)
      fig <- fig %>% add_lines()
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'Year of Release'),
                                         yaxis = list(title = 'Global Sales'),
                                         zaxis = list(title = 'Critic Score')))
      
      fig
    })
    
  }

shinyApp(ui, server)
