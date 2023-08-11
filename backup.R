# Load libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)

# Define UI for the application that draws a histogram
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Voters Turnout for elections worldwide", 
                                    titleWidth = 450),
                    dashboardSidebar(
                      
                      
                      fileInput("file", "Choose CSV File", accept = ".csv"),
                      # Add filters for countries, election types, and years with default selected items
                      selectInput("countryInput", "Select Country:", choices = NULL),
                      selectInput("electionTypeInput", "Select Election Type:", choices = NULL),
                      selectInput("yearInput", "Select Year:", choices = NULL),
                      
                      
                      # Adding a shor note
                      p("The dataset used is a real world dataset obtained from the link:
      https://www.idea.int/data-tools/vt-advanced-search?region=&question=")
                    ),
                    dashboardBody(
                      
                      # Application title
                      titlePanel(""),
                      
                      fluidRow(
                        valueBoxOutput("box_A"),
                        valueBoxOutput("box_B"),
                        valueBoxOutput("box_C")
                      ),
                      
                      fluidRow(
                        box(title = "Voters to population", solidHeader = T,
                            width = 5, collapsible = T,
                            plotOutput("plot1")),
                        box(title = "Trend", solidHeader = T,
                            width = 7, collapsible = T,
                            plotOutput("plot2"))
                      ),
                      
                      
                      fluidRow(
                        box(title = "Maps", solidHeader = T,
                            width = 7,  collapsible = T,
                            plotlyOutput("plot3")),
                        box(title = "Population over time", solidHeader = T,
                            width = 5,  collapsible = T,
                            plotOutput("plot4"))
                      )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Function to read the CSV file and return the cleaned data frame
  df <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    data <- read.csv(input$file$datapath, header = TRUE)
    
    # Clean and pre-process the data
    data <- data %>%
      mutate(
        Voter.Turnout = gsub("%", "", Voter.Turnout),
        VAP.Turnout = gsub("%", "", VAP.Turnout),
        Invalid.votes = gsub("%", "", Invalid.votes),
        Year_date = make_date(Year, 1, 1),
        Country = as.factor(Country),
        Election.type = as.factor(Election.type),
        Year = as.factor(Year),
        Compulsory.voting = as.factor(Compulsory.voting),
        across(where(is.character), ~ str_remove_all(., ",")),
        across(where(is.character), ~ as.numeric(.))
      )
    
    # Update the country, election type, and year choices for the selectInput filters
    updateSelectInput(session, "countryInput", choices = unique(data$Country), selected = "Nigeria")
    updateSelectInput(session, "electionTypeInput", choices = unique(data$Election.type), selected = "Presidential")
    updateSelectInput(session, "yearInput", choices = unique(data$Year), selected = "2019")
    
    df(data)
  })
  
  
  # Value boxes
  
  
  
  
  # box_A
  output$box_A <- renderValueBox({
    data<- df()
    
    av_voter_turnout_pre <- data |> 
      select(Country,Election.type,Voter.Turnout) |> 
      group_by(Country, Election.type) |> 
      na.omit() |> 
      summarize(av_voter_turnout = mean(Voter.Turnout)) |> 
      mutate(av_voter_turnout = round(av_voter_turnout,2)) |>
      filter((input$countryInput == 'All' | Country %in% input$countryInput)&
               Election.type == 'Presidential')
    
    valueBox(paste0(av_voter_turnout_pre[,3], "%"), 
             "Presidential", icon = icon("fire"), color = "yellow")
  })
  
  # box_B
  output$box_B <- renderValueBox({
    
    data<- df()
    
    av_voter_turnout_par <- data |> 
      select(Country,Election.type,Voter.Turnout) |> 
      group_by(Country, Election.type) |> 
      na.omit() |> 
      summarize(av_voter_turnout = mean(Voter.Turnout)) |> 
      mutate(av_voter_turnout = round(av_voter_turnout,2)) |>
      filter((input$countryInput == 'All' | Country %in% input$countryInput)&
               Election.type == 'Parliamentary')
    
    valueBox(paste0(av_voter_turnout_par[,3], "%"), 
             "Parliamentary", icon = icon("exclamation-triangle"), color = "green")
  })
  
  # box_C
  output$box_C <- renderValueBox({
    
    data<- df()
    
    pop<-data |> 
      select(Country,Year,Population) |> 
      mutate(Population = paste0(round(Population/10^6,2),'M')) |> 
      filter((input$yearInput == "All" | Year == input$yearInput) &
               (input$countryInput == 'All' | Country %in% input$countryInput))
    
    valueBox(pop[1,3],
             "Nations Population", icon = icon("rocket"), color = "blue")
  })
  
  
  # Render the first plot
  output$plot1 <- renderPlot({
    data <- df()
    
    # Filter data based on selected country and election type
    filtered_data <- data %>%
      filter(
        (input$countryInput == "All" | Country %in% input$countryInput) &
          (input$electionTypeInput == "All" | Election.type %in% input$electionTypeInput)
      )
    
    # voters per nation vs registered voters
    filtered_data |> 
      select(Country, Election.type, Voting.age.population, Total.vote, Year) |> 
      # filter(Country=='Nigeria',Election.type == 'Presidential') |> 
      pivot_longer(cols = c(Total.vote,Voting.age.population)) |> 
      ggplot(aes(x = Year, y = value, fill = name))+
      geom_bar(position = 'dodge', stat = 'identity')+
      scale_fill_manual(values = c('skyblue','darkblue'))+
      theme_minimal()+
      theme(legend.position = "top")+
      scale_y_continuous(labels = function(x) paste0(x / 1e6, "M"))+
      labs(y = 'Voters in millions', fill = "",
           caption = "This graph shows the total number of votes every election period
       compared with total number of registered voters for the presidential election.")
  })
  
  # Render the second plot (add your code for the second plot here)
  output$plot2 <- renderPlot({
    data<- df()
    
    # Filter data based on selected country and election type
    filtered_data <- data %>%
      filter(
        (input$countryInput == "All" | Country %in% input$countryInput)
      )
    
    
    filtered_data %>%
      select(Election.type, Year_date, Total.vote, Country) %>%
      # filter(Country == 'Nigeria') %>%
      group_by(Year_date, Election.type) %>%
      na.omit() %>%
      summarize(avg_x = mean(Total.vote)) %>%
      ggplot(aes(x = Year_date, y = avg_x)) +
      geom_line(color = "skyblue", linewidth = 0.6) +
      facet_wrap(~Election.type, ncol = 1) +
      labs(
        title = "Total votes over time",
        x = "Date",
        y = "Voter Turnout"
      ) +
      scale_x_date(date_labels = "%b %Y", breaks = "10 years") +
      theme_minimal() +
      scale_y_continuous(labels = function(x) paste0(x / 1e6, "M")) +
      theme(
        strip.background = element_rect(fill = "skyblue"),
        strip.text = element_text(color = "black")
      )
  })
  
  # Render the third plot
  output$plot3 <- renderPlotly({
    data<- df()
    # Load world map data
    world_map <- map_data("world")
    
    ggplotly(
      data |>
        filter(
          (input$yearInput == "All" | Year == input$yearInput) &
            (input$electionTypeInput == "All" | Election.type == input$electionTypeInput)
        ) |> # Change year in filter
        left_join(world_map, by = c("Country" = "region")) |>
        ggplot(aes(
          x = long,
          y = lat,
          group = group,
          fill = Total.vote,
          text = Country
        )
        ) +
        geom_polygon(color = "#ffffff", linewidth = 0.1) +
        scale_fill_gradient(
          na.value = "ivory",
          low = "lightblue",
          high = "darkblue"
        ) +
        labs(title = "Total voters for each country") +
        theme_void() +
        theme(
          panel.background = element_rect(fill = "white"),
          legend.position = "none"
        )
    )
  })
  
  
  # Render the fourth plot
  output$plot4 <- renderPlot({
    data<- df()
    
    # Filter data based on selected country and election type
    filtered_data <- data %>%
      filter(
        (input$countryInput == "All" | Country %in% input$countryInput)
      )
    
    filtered_data |> 
      select(Year_date,Population,Country) |> 
      # filter(Country== 'Nigeria') |> 
      group_by(Year_date) |>
      na.omit() |> 
      ggplot(aes(x = Year_date, y = Population)) +
      geom_line(color="darkblue", linewidth = 2) +
      geom_line(color = "skyblue", linewidth = 1)+
      labs(title = "Nations population over time",
           x = "Date",
           y = "population") +
      scale_x_date(date_labels = "%b %Y", breaks = "10 years") +
      theme_minimal()+
      scale_y_continuous(labels = function(x) paste0(x / 1e6, "M"))
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
