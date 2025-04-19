# Importing libraries
library(shiny)
library(shinythemes)
library(fontawesome)
library(shinyWidgets)
library(igraph)
library(highcharter)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(leaflet)
library(stringr)
library(geojsonio)
library(sf)

######################
# Set Working Directory #
######################

# R script directory setting
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######################
# Data Preprocessing #
######################

###### EV_registration
# Read dataset
ev_data <- read.csv("Data/EV_registration.csv")
ev_station <- read.csv("Data/EV_station.csv", check.names = FALSE)

#Changing state name with 2 characters
state_abbr_map <- setNames(c(state.abb, "DC"), c(state.name, "District of Columbia"))

# Column(State) name change
ev_data <- ev_data %>%
  mutate(State = state_abbr_map[State])

# Load geo.json file for the map
usa <- jsonlite::fromJSON("us-all.geo.json", simplifyVector = FALSE)

usa_geojson <- st_read("us-all.geo.json")
usa_geo <- as.data.frame(usa_geojson)

# Choice vectors for the filters
state_choiceVec <- c("All States", sort(unique(ev_data$State)))


###### EV_infrastructure
# Read infrastructure data
ev_infra_2019 <- read.csv("Data/fuel_station(2019).csv")
ev_infra_2020 <- read.csv("Data/fuel_station(2020).csv")
ev_infra_2021 <- read.csv("Data/fuel_station(2021).csv")
ev_infra_2022 <- read.csv("Data/fuel_station(2022).csv")
ev_infra_2023 <- read.csv("Data/fuel_station(2023).csv")

# Read infrastructure data
ev_infra <- read.csv("ev_infrastructure.csv")

###### EV_incentive. regulation
ev_incentive <- read.csv("Data/ev_incentives.csv")
ev_regulation <- read.csv("Data/ev_regulations.csv")
ev_emission <- read.csv("Data/ev_emissions.csv")

##################
# USER INTERFACE #
##################
ui <- page_navbar(
  title = "EV Registration and infrastructure in the US",
  theme = bs_theme(
    bootswatch = "cerulean",
    navbar_bg = "#d3d3d3"
  ),
  nav_spacer(),
  #1st tab
  nav_panel("EV Registration",
            fluidPage(
              # Title for home tab
              titlePanel("Overview of US Electic Vehicle Registration"),
              hr(),
              h5(strong("Click States, Map Type, Year for the number of EV registrations & Charging Stations by state."),
                 style = "font-size:16px;"),
              
              # Value box
              layout_columns(
                actionLink("registration_link",
                  value_box(
                    title = "Total Registration (2023)",
                    value = textOutput("total_registration"),
                    theme = "bg-gradient-cyan-green",
                    showcase = bsicons::bs_icon("ev-front"),
                    showcase_layout = "top right"
                  )
                ),
                actionLink("infrastructure_link",
                  value_box(
                    title = "Total Charging Stations (2023)",
                    value = textOutput("total_infrastructure"),
                    theme = "bg-gradient-teal-blue",
                    showcase = bsicons::bs_icon("ev-station"),
                    showcase_layout = "top right"
                  )
                )
              ),
              hr(),
              
              # Adapt sidebar layout for the map visualization
              sidebarLayout(
                sidebarPanel(
                  # Define filter and options for the user
                  pickerInput("map_state", 
                              tags$p(fa("filter", fill = "#244f76"), 
                                     "State filter for visualisation"),
                              state_choiceVec, selected = state_choiceVec[1], 
                              multiple = TRUE, options = list(`actions-box` = TRUE)),
                  
                  # Add radio buttons to toggle between EV Registration and Infrastructure
                  radioButtons("map_type", "Select Map Type:",
                               choices = c("EV Registration", "EV Infrastructure"),
                               selected = "EV Registration"),
                  
                  sliderInput("year", "Select Year:",
                              min = 2019, max = 2023, value = 2023, step = 1)
                ),
                mainPanel(
                  highchartOutput("ev_map", height = "600px")
                )
              ),
              hr(),
              h5('Data Source: US Department of Energy https://afdc.energy.gov', 
                 style = "font-size:12px;")
            )
  ),
  # 2nd tab
  # Infrastructure tab for EV Charging Stations
  nav_panel("Infrastructure",
            fluidPage(
              # Title for infrastructure tab
              titlePanel("EV Charging Infrastructure in the US"),
              hr(),
              h5(strong("Click the marker of EV charging stations for further information."),
                 style = "font-size:16px;"),
              
              # Value box for Total Infrastructure
              layout_columns(
                value_box(
                  title = "Total Infrastructure (2023)",
                  value = nrow(ev_infra_2023), 
                  theme = "bg-gradient-orange-pink",
                  showcase = bsicons::bs_icon("ev-station"),
                  showcase_layout = "top right"
                )
              ),
              hr(),
              
              # Sidebar and main map visualization layout
              sidebarLayout(
                sidebarPanel(
                  pickerInput("map_state_infra", 
                              tags$p(fa("filter", fill = "#e37400"), 
                                     "State filter for visualisation"),
                              state_choiceVec, selected = state_choiceVec[1], 
                              multiple = TRUE, options = list(`actions-box` = TRUE)),
                  radioButtons("infrastructure_year", "Select Year:",
                               choices = 2019:2023, 
                               selected = 2023)
                ),
                mainPanel(
                  leafletOutput("infra_map", height = "600px")
                )
              ),
              h5('Data Source: US Department of Energy https://afdc.energy.gov',
                 style = "font-size:12px;")
            )
  ),
  #3rd tab
  nav_panel("Region",
            fluidPage(
              titlePanel("EV Registration and Charging Stations by Region"),
              hr(),
              h5(strong("Click 'Top 5 EV Registration' or 'Top 5 Charging Stations."),
                 style = "font-size:16px;"),
              
              layout_columns(
                actionLink("region_registration_link",
                           value_box(
                             title = "Top 5 EV Registration",
                             value = textOutput("top5_registration"),
                             theme = "bg-gradient-yellow-green",
                             showcase = bsicons::bs_icon("ev-front"),
                             showcase_layout = "top right"
                           )
                ),
                actionLink("region_infrastructure_link",
                           value_box(
                             title = "Top 5 Charging Stations",
                             value = textOutput("top5_infrastructure"),
                             theme = "bg-gradient-cyan-purple",
                             showcase = bsicons::bs_icon("ev-station"),
                             showcase_layout = "top right"
                           )
                )
              ),
              hr(),
              
              # Sidebar
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput("region", "Select Region:",
                                     choices = c("Midwest", "Northeast", "South", "West"),
                                     selected = c("Midwest", "Northeast", "South", "West")),
                  sliderInput("region_year", "Select Year:",
                              min = 2019, max = 2023, value = 2023, step = 1)
                ),
                mainPanel(
                  highchartOutput("region_packedbubble", height = "800px")
                )
              ),
              hr(),
              h5('Data Source: US Department of Energy https://afdc.energy.gov',
                 style = "font-size:12px;")
            )
  ),
  #4th tab
  nav_panel("EV Incentives & Regulation & GHG",
            fluidPage(
              # Title for incentives tab
              titlePanel("EV Incentives, Regulations and Greenhouse Gas Emission by Year"),
              hr(),
              
              # Value box for Total Regulations & Incentives
              layout_columns(
                value_box(
                  title = "Total Regulation & Incentives (2022)",
                  # Sum of the number of regulations and incentives in 2022
                  value = sum(format(as.POSIXct(ev_incentive$Significant.Update.Date, format="%Y-%m-%d %H:%M:%S"), "%Y") == "2022", na.rm = TRUE) + 
                    sum(format(as.POSIXct(ev_regulation$Significant.Update.Date, format="%Y-%m-%d %H:%M:%S"), "%Y") == "2022", na.rm = TRUE),
                  theme = "bg-gradient-indigo-pink",
                  showcase = bsicons::bs_icon("clipboard-check"),
                  showcase_layout = "top right"
                ),
                
                value_box(
                  title = "Total Greenhouse Gas Emission(2022)",
                  #Sum of total greenhouse gas emission in 2022
                  value = sum(ev_emission$Electricity.Generation[ev_emission$Year == 2022], na.rm = TRUE),
                  theme = "bg-gradient-green-yellow",
                  showcase = bsicons::bs_icon("lightning-charge"),
                  showcase_layout = "top right"
                )
              ),
              hr(),
              
              # Bar chart output
              highchartOutput("incentive_bar_chart", height = "500px"),
              h5('Data Source: US Department of Energy https://afdc.energy.gov',
                 style = "font-size:12px;")
            )
  )
  
)

################
# SHINY SERVER #
################

##################################### Homepage #####################################
server <- shinyServer(function(input, output, session) {
  
  # User click Registration value_box, update map_type to EV Registration
  observeEvent(input$registration_link, {
    updateRadioButtons(session, "map_type", selected = "EV Registration")
  })
  
  # User click Registration value_box, update map_type to EV Infrastructure
  observeEvent(input$infrastructure_link, {
    updateRadioButtons(session, "map_type", selected = "EV Infrastructure")
  })
  
  # Total Registration value update
  output$total_registration <- renderText({
    selected_year <- paste0("X", as.character(input$year))
    
    # Registration sum of selected year 
    total_registrations <- sum(ev_data[[selected_year]], na.rm = TRUE)
    
    format(total_registrations, big.mark = ",")
  })
  
  # Total charging station value update
  output$total_infrastructure <- renderText({
    selected_year <- as.character(input$year)
    
    # Sum of charging stations of selected year
    total_infrastructure <- sum(ev_station[[selected_year]], na.rm = TRUE)
    
    format(total_infrastructure, big.mark = ",")
  })
  
  ##################################### Registration & Infrastructure #####################################
  output$ev_map <- renderHighchart({
    
    # Data filtering based on the selected year and state
    selected_year <- paste0("X", as.character(input$year))
    selected_state <- input$map_state
    
    # Select data from the map_type (EV Registration or EV Infrastructure)
    if (input$map_type == "EV Registration") {
      
      # EV Registration
      map_data <- ev_data %>%
        select(State, all_of(selected_year)) %>%
        rename(value = all_of(selected_year))
      
      # Filter the data based on the selected state
      if (!"All States" %in% selected_state) {
        map_data <- map_data %>%
          filter(State %in% selected_state)
      }
      
      # Remove 'X' from the selected year
      clean_year <- gsub("X", "", selected_year)
      
      # Mapping the data to the map for EV Registration visualization
      highchart(type = "map") %>%
        hc_add_series_map(
          map = usa,  # 'usa' map data
          df = map_data,  # ev registration data
          joinBy = c("postal-code", "State"),  # maaping postal-code(map) 'State'(registration)
          value = "value",  # mapping the number of EV registrations to color
          name = paste("EV Registrations in", clean_year)
        ) %>%
        #Set color
        hc_colorAxis(
          min = min(map_data$value, na.rm = TRUE),
          max = max(map_data$value, na.rm = TRUE),
          minColor = "#D6EAF8",  # color for low value
          maxColor = "#1A5276",  # color for high value
          nullColor = "#E0E0E0",  # color for no data
          stops = color_stops(5, c("#D6EAF8", "#85C1E9", "#3498DB", "#2874A6", "#1A5276"))
        ) %>%
        #Set legend
        hc_legend(
          title = list(text = "EV Registrations"), 
          align = "right",
          layout = "vertical",
          floating = TRUE,
          valueDecimals = 0
        ) %>%
        #Set title
        hc_title(text = paste("Electric Vehicle Registrations in", clean_year)) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        #Tooltip
        hc_tooltip(
          headerFormat = "",
          pointFormat = "<b>{point.name}</b><br/>EV Registrations: {point.value}"
        ) %>%
        hc_chart(
          backgroundColor = "#E0E0E0"  # set background color
        )
      
    } else if (input$map_type == "EV Infrastructure") {
      
      # EV Infrastructure
      station_year <- as.character(input$year)
      map_data <- ev_station %>%
        select(State, station_year) %>%
        rename(Stations = all_of(station_year))
      
      # Filter the data based on the selected state
      if (!"All States" %in% selected_state) {
        map_data <- map_data %>%
          filter(State %in% selected_state)
      }
      
      # Remove 'X' from the selected year
      clean_year <- gsub("X", "", station_year)
      
      # Mapping the data to the map for EV Infrastructure visualization
      highchart(type = "map") %>%
        hc_add_series_map(
          map = usa,  # 'usa' map data
          df = map_data,  # charging station data
          joinBy = c("postal-code", "State"),  # mapping postal-code from map and 'State' from ev_station
          value = "Stations",  # mapping the number of charging stations to color
          name = paste("EV Charging Stations in", clean_year),
          color = "#355c30"  # green color scheme
        ) %>%
        #Set color
        hc_colorAxis(
          min = min(map_data$Stations, na.rm = TRUE),
          max = max(map_data$Stations, na.rm = TRUE),
          minColor = "#cbf5c6",  # color for low value
          maxColor = "#355c30",  # color for high value
          nullColor = "#E0E0E0"  # color for no data
        ) %>%
        #Set legend
        hc_legend(
          title = list(text = "EV Charging Stations"),
          align = "right",
          layout = "vertical",
          floating = TRUE,
          valueDecimals = 0
        ) %>%
        #Set title
        hc_title(text = paste("Electric Vehicle Charging Stations in", clean_year)) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        #Tooltip
        hc_tooltip(
          headerFormat = "",
          pointFormat = "<b>{point.name}</b><br/>Charging Stations: {point.value}"
        ) %>%
        hc_chart(
          backgroundColor = "#E0E0E0"  # background color
        )
      
    }
  })
  
  
  ##################################### Infrastructure #####################################
  
  output$infra_map <- renderLeaflet({
    
    # Filter the data based on the selected year and state
    infra_year <- input$infrastructure_year
    selected_data <- ev_infra %>%
      filter(Year == infra_year)  # user input year
    
    # Filter the data based on the selected state
    selected_state <- input$map_state_infra
    if (!"All States" %in% selected_state) {
      selected_data <- selected_data %>%
        filter(State %in% selected_state)
    }
    
    # Set view for US continent, Hawaii, Alaska
    selected_data <- selected_data %>%
      filter(
        (Longitude >= -125 & Longitude <= -66.9 & Latitude >= 24.5 & Latitude <= 49.5) |  # Continental US
          (Longitude >= -161 & Longitude <= -154 & Latitude >= 18 & Latitude <= 22) |  # Hawaii
          (Longitude >= -179 & Longitude <= -130 & Latitude >= 51 & Latitude <= 72)  # Alaska
      )
    
    # Create leaflet map, and add charging station markers
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addAwesomeMarkers(
        data = selected_data,
        lng = ~Longitude, lat = ~Latitude,
        popup = ~paste(
          "<b>Name:</b>", Station.Name, "<br>",
          "<b>City:</b>", City, "<br>",
          "<b>State:</b>", State, "<br>",
          "<b>ZIP:</b>", ZIP, "<br>",
          "<b>Telephone:</b>", Station.Phone, "<br>",
          "<b>Type:</b>", Access.Code
        ),
        icon = awesomeIcons(
          icon = 'bolt',  # set electricity icon 
          library = 'fa', 
          markerColor = ~ifelse(Access.Code == "public", 'blue', 'green')  # Marker color based on Access.Code(public/private)
        ),
        clusterOptions = markerClusterOptions()  # clustering
      )
  })
  
  ##################################### Region #####################################
  
  # Map type state: EV Registration or EV Infrastructure
  map_type <- reactiveVal("EV Registration")  # default: EV Registration
  
  # Registration is clicked, set map_type to "EV Registration"
  observeEvent(input$region_registration_link, {
    map_type("EV Registration")
    updateRadioButtons(session, "map_type", selected = "EV Registration")
  })
  
  # When Infrastructure is clicked, set map_type to "EV Infrastructure"
  observeEvent(input$region_infrastructure_link, {
    map_type("EV Infrastructure")
    updateRadioButtons(session, "map_type", selected = "EV Infrastructure")
  })
  
  # Top 5 Registration States
  output$top5_registration <- renderText({
    selected_year <- paste0("X", as.character(input$region_year))  # year
    
    # EV Registration filtering and calculate Top 5 states
    ev_filtered <- ev_data %>%
      select(State, all_of(selected_year)) %>%
      rename(value = all_of(selected_year)) %>%  # rename the column
      arrange(desc(value)) %>%  # sort in descending order
      head(5)  # top 5
    
    paste(ev_filtered$State, collapse = ", ")  # State name with comma
  })
  
  # Top 5 Infrastructure States 계산
  output$top5_infrastructure <- renderText({
    station_year <- as.character(input$region_year)  #year
    
    # EV Infrastructure filtering and calculate Top 5 states
    station_filtered <- ev_station %>%
      select(State, all_of(station_year)) %>%
      rename(Stations = all_of(station_year)) %>%
      arrange(desc(Stations)) %>%
      head(5)
    
    paste(station_filtered$State, collapse = ", ") #State name with comma
  })
  
  # Packedbubble chart
  output$region_packedbubble <- renderHighchart({
    
    # Filter based on the selected year and region
    selected_year <- paste0("X", as.character(input$region_year))  # user input year ('X2023' format)
    selected_regions <- input$region  # user input region
    
    # EV Registration data filtering
    ev_filtered <- ev_data %>% #Registration
      select(State, all_of(selected_year)) %>%
      rename(value = all_of(selected_year))
    
    # EV Infrastructure data filtering
    station_year <- as.character(input$region_year)  # year 
    station_filtered <- ev_station %>% #Station
      select(State, station_year) %>%
      rename(Stations = all_of(station_year))
    
    usa_geo <- usa_geo %>% # USA map data
      rename(State = postal.code)
    
    # Combine usa_map_data and ev data(registration, station)
    usa_map_data <- usa_geo %>%
      left_join(ev_filtered, by = "State") %>%
      left_join(station_filtered, by = "State")
    
    # Filter based on the selected region
    if (!is.null(selected_regions)) {
      usa_map_data <- usa_map_data %>%
        filter(region %in% selected_regions)
    }
    
    # Filter by map_type
    if (map_type() == "EV Registration") {
      # EV Registration(ev_data)
      region_data <- usa_map_data %>%
        select(name = State, value = value, group = region)  # Set bubble size based on the number of registration 
    } else if (map_type() == "EV Infrastructure") {
      # EV Infrastructure(ev_station)
      region_data <- usa_map_data %>%
        select(name = State, value = Stations, group = region)  # Set bubble size based on the number of charging station
    }
    
    # Filtering for the top 10% of the data
    q90 <- as.numeric(quantile(region_data$value, .90))
    
    # Packedbubble chart visualisation
    hchart(region_data, "packedbubble", hcaes(name = name, value = value, group = group)) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "<b>{point.name}</b>: {point.value}"
      ) %>%
      hc_plotOptions(
        packedbubble = list(
          maxSize = "150%",  # Max bubble size
          zMin = 0,
          layoutAlgorithm = list(
            gravitationalConstant = 0.05,
            splitSeries = TRUE,  # Split by group
            seriesInteraction = TRUE,
            dragBetweenSeries = TRUE,
            parentNodeLimit = TRUE
          ),
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}",
            filter = list(
              property = "y",
              operator = ">",
              value = q90  # 90% quantile
            ),
            style = list(
              color = "black",
              textOutline = "none",
              fontWeight = "normal"
            )
          )
        )
      )
  })
  
  
  ##################################### Regulation #####################################
  
  output$incentive_bar_chart <- renderHighchart({
    
    # Remove NA values and filter the data based on the year
    ev_incentive_clean <- ev_incentive %>%
      filter(!is.na(Significant.Update.Date)) %>%  # remove none value 
      mutate(Year = as.numeric(format(as.POSIXct(Significant.Update.Date, format="%Y-%m-%d %H:%M:%S", tz="UTC"), "%Y"))) %>%
      filter(Year >= 2014 & Year <= 2022)  # 2014~2022 filtering year
    
    # Sum of the number of incentives by year
    incentive_count <- ev_incentive_clean %>%
      group_by(Year) %>%
      summarise(Incentive_Count = n())
    
    # Remove NA values and filter the data based on the year
    ev_regulation_clean <- ev_regulation %>%
      filter(!is.na(Significant.Update.Date)) %>% # remove none value
      mutate(Year = as.numeric(format(as.POSIXct(Significant.Update.Date, format="%Y-%m-%d %H:%M:%S"), "%Y"))) %>%
      filter(Year >= 2014 & Year <= 2022)  # 2014~2022 filtering year
    
    # Sum of the number of regulations by year
    regulation_count <- ev_regulation_clean %>%
      group_by(Year) %>%
      summarise(Regulation_Count = n())
    
    # Remove NA values and filter the data based on the year
    ev_emission_clean <- ev_emission %>%
      filter(Year >= 2014 & Year <= 2022) %>%  # 2014~2024 범위로 필터링
      select(Year, Electricity.Generation) %>%  # year and generation of electricity
      filter(!is.na(Electricity.Generation))    # remove none value
    
    # Combine the data
    combined_data <- full_join(incentive_count, regulation_count, by = "Year") %>%
      full_join(ev_emission_clean, by = "Year") %>% 
      replace_na(list(Incentive_Count = 0, Regulation_Count = 0, Electricity.Generation = 0))  # Replace none value to 0
    
    # Create bar chart for incentives, regulations, and line graph for greenhouse gas emission
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "EV Incentive, Regulation, and Greenhose Gas Emission for generating electricity by Year") %>%
      hc_xAxis(categories = combined_data$Year, title = list(text = "Year")) %>%
      hc_yAxis_multiples(
        list(title = list(text = "Count"), opposite = FALSE),  # first Y axis (incentives, regulations)
        list(title = list(text = "Greenhouse Gas Emission for Electricity Generation(MMT CO2 eq)"), opposite = TRUE)  # second Y axis (generation of electricity)
      ) %>%
      hc_add_series(name = "Incentive Count", data = combined_data$Incentive_Count, color = "#3498DB") %>%
      hc_add_series(name = "Regulation Count", data = combined_data$Regulation_Count, color = "#E74C3C") %>%
      hc_add_series(
        name = "Electricity Generation", 
        data = combined_data$Electricity.Generation, 
        type = "line",  # line graph
        color = "#2ECC71",
        yAxis = 1  # connect 2nd Y axis
      ) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = TRUE),
        colorByPoint = FALSE  # Color by series(Not by point)
      )) %>%
      hc_legend(enabled = TRUE)
  })
  
})

#############
# RUN SHINY #
#############

shinyApp(ui, server)
