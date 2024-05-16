#Reports Graph - 2 - Pie Chart
# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
#library(lubridate)
#library(httr)
library(tidyverse)
#library(stringr)
#library(reshape2)
#library(viridis)
#library(hrbrthemes)
#library(readxl)
#library(htmlwidgets)



#Read the 'out of schoool' statistics Global dataset.
Out_of_school <- read.csv("3- number-of-out-of-school-children.csv")

names(Out_of_school) <- c("Country", "Country_code","Year", "Count_of_Males","Count_of_Females")


#Filter out Years Greater than or equal to 2015 to consider most recent data.
Out_of_school_5Eyes <- Out_of_school %>% 
  filter(Year >= 2015)


#Filter out the developed "5 Eyes" countries (excluding USA as it's an outlier) for comparison.
Out_of_school_5Eyes <- Out_of_school_5Eyes %>% 
  filter(Country == "Australia" |Country == "Canada"|Country == "New Zealand"|Country == "United Kingdom")


#Create new column to get combined count of both the genders.
Out_of_school_5Eyes$Count_Both_Genders <- Out_of_school_5Eyes$Count_of_Males + Out_of_school_5Eyes$Count_of_Females

#Sum the values in all the years for different countries using groupby.
Out_of_school_5Eyes_summ <- Out_of_school_5Eyes %>%
  group_by(Country) %>%
  summarize(total_count = sum(Count_Both_Genders))

#Calculate percentages for pie chart.
Out_of_school_5Eyes_summ$percent <- paste0(round(100 * Out_of_school_5Eyes_summ$total_count / sum(Out_of_school_5Eyes_summ$total_count), 1), "%")
#print(Out_of_school_5Eyes_summ)

# Define UI
ui <- fluidPage(
  titlePanel("Figure 2: Number of 'Out of School' children in 5 Eyes Countries(2015-2018)."),
  
  # Create a full-page Plotly graph
  #print("Out_of_school_5Eyes"),
  plotlyOutput("plot_pie"),
  uiOutput("bottom_text"))


# Define server logic
server <- function(input, output, session) {
  
  # Generate ggplotly
 
  output$plot_pie <- renderPlotly({
    
   # Out_of_school_5Eyes_summ <- Out_of_school_5Eyes_summ %>% 
   #   filter(Year >= 2015)
 fig <- plot_ly(Out_of_school_5Eyes_summ, labels = ~Country, values = ~total_count, type = 'pie')
 fig <- fig %>% layout( legend = list(
   title = list(text = "Select Countries:")  # Add a legend header
 ),xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
 #fig
  })
  output$bottom_text <- renderUI({
    HTML("<div style='text-align: center;'>Source: Worldbank data - https://github.com/worldbank/GLAD</div>")})
  # Render ggplot in Plotly
 # output$plot <- renderPlotly({
  #  ggplotly(gg) %>%
  #    layout(
  #      autosize = TRUE,
  #      margin = list(l = 50, r = 50, t = 50, b = 50)
  #    )
#  })
}

# Run the application
shinyApp(ui = ui, server = server)