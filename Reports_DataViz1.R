#Reports Graph - 1 - With Checkbox
# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(httr)
library(tidyverse)
library(stringr)
library(reshape2)
library(viridis)
#library(hrbrthemes)
#library(readxl)
#library(htmlwidgets)

#setwd("C:/Users/HP/Documents/Iteration 1 Datasets/Datasets/app1")





#Read the 'out of schoool' statistics Global dataset.
Out_of_school <- read.csv("3- number-of-out-of-school-children.csv")

names(Out_of_school) <- c("Country", "Country_code","Year", "Count_of_Males","Count_of_Females")


#Filter out the developed "5 Eyes" countries (excluding USA as it's an outlier) for comparison.
Out_of_school_5Eyes <- Out_of_school %>% 
  filter(Country == "Australia" |Country == "Canada"|Country == "New Zealand"|Country == "United Kingdom")

#Filter out Years Greater than or equal to 2015 to consider most recent data.
Out_of_school_5Eyes <- Out_of_school_5Eyes %>% 
  filter(Year >= 2015)

#As factor Year.
Out_of_school_5Eyes$Year <- factor(Out_of_school_5Eyes$Year)

#Create new column to get combined count of both the genders.
Out_of_school_5Eyes$Count <- Out_of_school_5Eyes$Count_of_Males + Out_of_school_5Eyes$Count_of_Females
#Out_of_school_5Eyes$Count <- prettyNum(Out_of_school_5Eyes$Count, big.mark = ",", scientific = FALSE)
#Out_of_school_5Eyes$Count <- as.integer(Out_of_school_5Eyes$Count)
#str(Out_of_school_5Eyes)

# Define UI
ui <- fluidPage(
  titlePanel("Figure 1: Comparison of 'Out of School' children in the 5 Eyes Countries(Excluding USA)."),
  
  # Add checkboxes
  sidebarLayout(
    sidebarPanel (
      checkboxGroupInput("checkboxes", "Select year to display data:", 
                         choices = c("Australia", "Canada", "New Zealand", "United Kingdom"),
                         selected = c("Australia", "Canada", "New Zealand", "United Kingdom"),)),
    
  # Create a full-page Plotly graph
  mainPanel(plotlyOutput("gg", height = "auto"),
            uiOutput("bottom_text"))
))

# Define server logic
server <- function(input, output, session) {
  
  #Plot Stacked bar graph to compare 'out of school' children of different countries.
  output$gg <- renderPlotly({ 
    Out_of_school_5Eyes <- filter(Out_of_school_5Eyes,Country %in% as.character(input$checkboxes))
    
    
    ggplot(Out_of_school_5Eyes, aes(y=Count, x=Country, fill=Year)) + 
    geom_bar(position="stack", stat="identity",
             #aes(
     # text = paste0(
      #  "<b>", Country, "</b>", "<br>",
      #  "Year: ", Year, "<br>",
     #   "Count: ", scales::comma(Count, 1), "<br>"))
    ) +
    scale_fill_viridis(discrete = T) +
    #geom_label(label= Out_of_school_5Eyes$Count_Both_Genders,nudge_x = 0.25, nudge_y = 0.25, 
    #           check_overlap = T, size = 2.8)+
    #ggtitle("Comparison of 'Out of school' children of developed countries from year 2015-2018.") +
    theme(plot.title = element_text(size = 18)) +
    labs(fill = "Year:") +
    xlab("Country") + ylab("Count") +
    guides(color = guide_legend(title = "Select Year")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
    theme_bw() 
    
    
  })
  
#output$gg$x$layout$hovermode <- FALSE
  
  
  
   output$plot <- renderPlotly({
    ggplotly(output$gg,
      #tooltip = c("text")
      ) %>%
      layout(
        legend = list(title = list(text = "Select Year:")),
        autosize = TRUE,
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })
  
   output$bottom_text <- renderUI({
     HTML("<br><div style='text-align: center;'>Source: Worldbank data - https://github.com/worldbank/GLAD</div>")})
#  output$plot$x$layout$hovermode <- FALSE
}

# Run the application
shinyApp(ui = ui, server = server)
