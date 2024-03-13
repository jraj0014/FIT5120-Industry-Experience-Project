library(maps)
library(leaflet)
library(dplyr)
library(shiny)
library(lubridate)
library(httr)
library(jsonlite)
library(tidyverse)
library(stringr)
#library(plotly)

getwd()

df1 = read.csv("postcode.csv")
df1 <- arrange(df1, suburb)


#library(lubridate)
df2 = read.csv("dvp_vis2_final.csv")
df2$Year = year(df2$listingDate)

df2 = df2[df2$Year < 2022,]

df3 = read.csv("ans3_dvp 2.csv")
df_cancer = read.csv("Cancerstats.csv")

df3$workType = str_to_title(df3$workType, locale = "en")

data3 = df3 %>%
  group_by(workType,year) %>%
  summarise(Count = n())

data3_cancer = df_cancer %>%
  group_by(Cancer.group.site,Year,State.or.Territory) #%>%
  #summarise(Count = n())

data3$year = as.factor(data3$year)




ui <- fluidPage(
  
  column(width = 8,
         
         wellPanel(
           h2("Select location using Suburb or Postcode:"),
           fluidRow(
             selectInput("SelectTypeP1",
                         "Use the dropdown OR type in the suburb name or post code:",
                         # unique(df1$suburb),
                          choices = paste(df1$suburb, " - ", df1$postcode), 
                          width = '100%',
             )
           ),
           br(),
           mainPanel(
             column(width = 8,
             # Display the text from the dataframe
           #  UV_Index <- textOutput("Uniquelat"),
           #  print(typeof(UV_Index)),
             h2("The UV-index at this location is: " ),
             #tags$style(HTML(".shiny-text-output pre {font-size: 128px;}")),
             h1(textOutput("Uniquelat")))
             #print(uniquelat)
           ),
           #h2("The UVindex is ", weather_data$lat),
           fluidRow(
             column(width = 8,
             plotOutput("Plot1", height = "4px"),
             #h2("The UVindex is: ", uniquelat)
           ))
         )
         
  ) , 
  # column(width = 6,
  #        
  #        wellPanel(
  #          h2("Graph 2: Title here"),
  #          fluidRow(
  #            sliderInput("SelectYearP2",
  #                        "Select Year Range",
  #                        min = min(df2$Year),
  #                        max = max(df2$Year),
  #                        value = c(min(df2$Year),max(df2$Year)),
  #                        width = '100%',
  #                        step = 1,
  #                        sep = ""
  #            )
  #          ),
  #          
  #          fluidRow(
  #            leafletOutput("Plot2")
  #            
  #          )
  #        )
  #        
  # ), 
  column(width = 12,
         
         wellPanel(
           fluidRow(
             h2("Skin cancer Statistics in Australia (2007-2019).")
             
            
             )
           ),
           
           
           
           fluidRow(
             plotOutput("Plot3")
             
           )
         )
         
  )
  




server <- function(input, output, session) {
  
  
  output$Plot1 = renderPlot({
    req(input$SelectTypeP1)
    
    input_select <- str_split(input$SelectTypeP1, "-", simplify = TRUE)
    input_suburb <- trimws(input_select[1], which = "right")
    
    print(input_suburb)
    #print(input$SelectTypeP1)
    
    data1 = df1 %>%
      filter(suburb == input_suburb)
    
    view(data1)
    print("data1")
    
    #data1$Cat = as.factor(data1$Cat)
    
    lati = data1$latitude
    longi = data1$longitude
    
    print(lati)
    
    ## Make the API request
    url <- "https://api.openweathermap.org/data/3.0/onecall?appid=2b617bc58375d868af255a1eac2053d1&units=metric"
    response <- GET(url, 
                    query = list(lat = lati, lon = longi))
    
                    # Check if the request was successful
                    if (http_status(response)$category == "Success") {
                      # Parse JSON response
                      weather_data <- content(response, "text") %>%
                        fromJSON()
                      
                      # Extract relevant information
                      temperature <- weather_data$current$temp 
                      uvindex <- weather_data$current$uvi 
                      wind_speed <- weather_data$current$wind_speed 
                      # Print the weather details
                      
                      #h2("The UVindex is: ", weather_data$current$wind_speed)
                      output1 <- weather_data$lat
  
                      cat("Current Temperature of the location:", temperature, "°C\n")
                      cat("Current UV index", uvindex, "\n")
                      cat("Windspeed", wind_speed, "\n")
                      print(uvindex)
                      # Output the text from the dataframe
                      output$Uniquelat <- renderText({
                        # Convert list to string using paste()
                        uvindex_str <- paste(uvindex, collapse = ", ")
                        return(uvindex_str)})
                        #capture.output(weather_data$current$uvi)})
                        #capture.output(uvindex)})
                    } else {
                      cat("Error:", http_status(response)$reason, "\n")
                    }
    
    
    
  })
  
  
  output$Plot2 = renderLeaflet({
    
    req(input$SelectYearP2)
    
    leaflet(data = df2[df2$Year >= input$SelectYearP2[1]
                       & df2$Year <= input$SelectYearP2[2]
                       ,]) %>% addTiles() %>%
      addMarkers(
        ~Longitude, 
        ~Latitude, 
        popup = ~as.character(state),
        clusterOptions = markerClusterOptions()
      )
    
  })
  
  
  output$Plot3 = renderPlot({
   # req(input$SelectTypeP3)
  #  req(input$SelectYearP3)
    
    
  
    #  data3 = data3 %>%
    #  filter(workType %in% input$SelectTypeP3) %>%
    #  filter(year %in% input$SelectYearP3) 
    
    df_cancer = read.csv("Cancerstats.csv")
    
    data3_cancer = df_cancer %>%
      group_by(Cancer.group.site,Year,State.or.Territory)
    
    df_cancer$Count <- as.numeric(df_cancer$Count)
    
    
    stats <- df_cancer %>%
      group_by(Year,Cancer.group.site) %>%
      summarise(perYearcount = sum(Count, na.rm = TRUE))
    
    stats$'Cancer_Type' <- stats$Cancer.group.site
   # stats$Year <- as.numeric(stats$Year)
    
    ggplot(data=stats, (aes(x=Year, y=perYearcount, color=Cancer_Type))) +
      geom_line() +
      labs( x = "Year", y = "Number of Incidents Reported") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_continuous(breaks = stats$Year)
      #theme_minimal()
    
    
    
  })
  
}




shinyApp(ui, server)