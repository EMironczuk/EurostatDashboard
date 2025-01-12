##################### Environment & libraries ##################################
#setwd("C:/Users/...")

library(dplyr)
library(ggplot2)
library(shinydashboard)
library(bs4Dash)
library(eurostat)
library(plotly)
library(DT)
library(shiny)
library(sf)
library(tmap)

##################### DATA LOAD ################################################
# Load population data from Eurostat and filter for total population (age = "TOTAL")
population <- get_eurostat("demo_pjan")
population <- population %>%
  filter(age == "TOTAL") %>%
  dplyr::select(!contains("freq") & !contains("unit"))

# Retrieve Eurostat geospatial data and merge it with population data
finaldata <- get_eurostat_geospatial(nuts_level = 0) %>%
  left_join(population, by = "geo")

# Extract the year from the TIME_PERIOD column and convert to numeric
finaldata$YEAR <- as.numeric(substr(finaldata$TIME_PERIOD, 1, 4))

# Replace sex codes with descriptive labels
finaldata$sex <- ifelse(finaldata$sex == "F", "Female", 
                        ifelse(finaldata$sex == "M", "Male", "Total"))

# Map country names using predefined country codes (EU, EFTA, candidates)
finaldata$name <- eu_countries$name[match(finaldata$id, eu_countries$code)]
finaldata$name <- ifelse(is.na(finaldata$name), 
                         efta_countries$name[match(finaldata$id, efta_countries$code)], 
                         finaldata$name)
finaldata$name <- ifelse(is.na(finaldata$name), 
                         eu_candidate_countries$name[match(finaldata$id, eu_candidate_countries$code)], 
                         finaldata$name)

# Handle a specific case for the United Kingdom
finaldata$name <- ifelse(is.na(finaldata$name) & finaldata$id == "UK", "United Kingdom", finaldata$name)

# Load fertility rate data and extract the year
fertility_rate <- get_eurostat("tps00199")
fertility_rate$YEAR <- as.numeric(substr(fertility_rate$TIME_PERIOD, 1, 4))

# Load life expectancy data and extract the year
life_expectancy <- get_eurostat("tps00205")
life_expectancy$YEAR <- as.numeric(substr(life_expectancy$TIME_PERIOD, 1, 4))

# Load infant mortality data and extract the year
infant_mortality <- get_eurostat("tps00027")
infant_mortality$YEAR <- as.numeric(substr(infant_mortality$TIME_PERIOD, 1, 4))

# Load population data by age groups and filter out specific categories
population_pjangroup <- get_eurostat("demo_pjangroup")
population_pjangroup <- population_pjangroup %>%
  filter(age != "TOTAL" & age != "UNK" & age != "Y_GE75" & age != "Y_GE80" & sex != "T") %>%
  dplyr::select(!contains("freq") & !contains("unit"))

# Extract the year and clean age group labels
population_pjangroup$YEAR <- as.numeric(substr(population_pjangroup$TIME_PERIOD, 1, 4))
population_pjangroup$age <- gsub("Y", "", population_pjangroup$age)
population_pjangroup$age <- gsub("_GE85", "85+", population_pjangroup$age)
population_pjangroup$age <- gsub("_LT5", "0-5", population_pjangroup$age)

# Load marriage data and filter for first marriage age indicator
marriage <- get_eurostat("demo_nind")
marriage <- marriage %>%
  filter(indic_de == "FAGEMAR1") %>%
  dplyr::select(!contains("freq"))
marriage$YEAR <- as.numeric(substr(marriage$TIME_PERIOD, 1, 4))

# Load divorce data and filter for total divorces indicator
divorce <- get_eurostat("demo_ndivind")
divorce <- divorce %>%
  filter(indic_de == "GDIVRT") %>%
  dplyr::select(!contains("freq"))
divorce$YEAR <- as.numeric(substr(divorce$TIME_PERIOD, 1, 4))

# Load fertility indicator data and filter for age of mother at first childbirth
fertility_ind <- get_eurostat("demo_find")
fertility_ind <- fertility_ind %>%
  filter(indic_de == "AGEMOTH1") %>%
  dplyr::select(!contains("freq"))
fertility_ind$YEAR <- as.numeric(substr(fertility_ind$TIME_PERIOD, 1, 4))

# Load social protection expenditure data and filter for total values in euros per capita
social_protection <- get_eurostat("spr_exp_ffa")
social_protection <- social_protection %>%
  filter(spdep == "SPR" & spdepm == "TOTAL" & unit == "EUR_HAB_KP15") %>%
  dplyr::select(!contains("freq"))
social_protection$YEAR <- as.numeric(substr(social_protection$TIME_PERIOD, 1, 4))

# Load migration data and filter for total migration values
migration <- get_eurostat("migr_netmigr")
migration <- migration %>%
  filter(age == "TOTAL" & agedef == "REACH") %>%
  dplyr::select(!contains("freq"))
migration$YEAR <- as.numeric(substr(migration$TIME_PERIOD, 1, 4))



##################### UI #######################################################
ui <- dashboardPage(
  title = "Eurostat population",
  
  dashboardHeader(
    title = dashboardBrand(
      title = "Eurostat population",
      color = "lightblue"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Population", tabName = "map", icon = icon("map")),
                menuItem("Population by country", tabName = "viz", icon = icon("chart-line")),
                menuItem("Country comparison", tabName = "comp", icon = icon("bars")),
                menuItem("Info", tabName = "info", icon = icon("info")),
                
                conditionalPanel("input.sidebar == 'map'", 
                                 selectInput(inputId = "selected_year", label = "Year", 
                                             choices = sort(unique(finaldata$YEAR), decreasing = TRUE))
                ),
                conditionalPanel("input.sidebar == 'viz'", 
                                 selectInput(inputId = "selected_country", label = "Country", selected = "PL", 
                                             choices = unique(finaldata$geo))
                ),
                conditionalPanel("input.sidebar == 'viz'", 
                                 selectInput(inputId = "selected_year2", label = "Year", selected = "2022", 
                                             choices = sort(unique(population_pjangroup$YEAR), decreasing = TRUE))
                ),
                conditionalPanel("input.sidebar == 'comp'", 
                                 selectInput(inputId = "country1", label = "Country 1", selected = "PL", 
                                             choices = unique(finaldata$geo))
                ),
                conditionalPanel("input.sidebar == 'comp'", 
                                 selectInput(inputId = "country2", label = "Country 2", selected = "DE", 
                                             choices = unique(finaldata$geo))
                ),
                conditionalPanel("input.sidebar == 'comp'", 
                                 selectInput(inputId = "selected_year3", label = "Year", selected = "2022", 
                                             choices = sort(unique(population_pjangroup$YEAR), decreasing = TRUE))
                )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        
        fluidRow(
          column(6, tmapOutput("map", height = "600px")),
          column(6, dataTableOutput("table"))
        ),
        
        fluidRow(
          column(12, style = "margin-top: 30px;")
        ),
        
        fluidRow(
          valueBoxOutput("lifeexpectancyBox", width = 4),
          valueBoxOutput("fertilityBox", width = 4),
          valueBoxOutput("infantmortalityBox", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Life Expectancy",
            width = 4, background = "navy", collapsed = TRUE,
            "Life expectancy at birth is defined as the mean number of years that a new-born child can expect to live if subjected throughout his life to the current mortality conditions (age specific probabilities of dying)."
          ),
          box(
            title = "Fertility Rate",
            width = 4, background = "navy", collapsed = TRUE,
            "The mean number of children that would be born alive to a woman during her lifetime if she were to survive and pass through her childbearing years conforming to the fertility rates by age of a given year."
          ),
          box(
            title = "Infant Mortality",
            width = 4, background = "navy", collapsed = TRUE,
            "The ratio of the number of deaths of children under one year of age during the year to the number of live births in that year. The value is expressed per 1,000 live births."
          )
        )
      ),
      
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            title = "Population size over time",
            width = 8,
            plotlyOutput("trend_plot", height = "350px")
          ),
          box(
            title = "Age pyramid by sex",
            width = 4,
            plotlyOutput("barchart_agegroup", height = "350px")
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            valueBoxOutput("fertilityBox2", width = 12),
            valueBoxOutput("marriageBox", width = 12),
            valueBoxOutput("migrationFBox", width = 12)
          ),
          column(
            width = 4,
            valueBoxOutput("fertilityBox3", width = 12),
            valueBoxOutput("divorceBox", width = 12),
            valueBoxOutput("migrationMBox", width = 12)
          ),
          column(
            width = 4,
            box(
              title = "Social protection expenditure",
              width = 12,
              plotlyOutput("social", height = "300px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "comp",
        fluidRow(
          box(
            title = "Population size - Country 1",
            width = 6,
            plotlyOutput("country1_plot", height = "200px")
          ),
          box(
            title = "Population size - Country 2",
            width = 6,
            plotlyOutput("country2_plot", height = "200px")
          )
        ),
        fluidRow(
          column(
            width = 3,
            box(
              title = "Age pyramid - Country 1",
              width = 12,
              plotlyOutput("country1_barchart", height = "425px")
            )
          ),
          column(
            width = 3,
            valueBoxOutput("fertilityBox4", width = 12),
            valueBoxOutput("fertilityBox6", width = 12),
            valueBoxOutput("marriageBox1", width = 12),
            valueBoxOutput("divorceBox1", width = 12)
          ),
          column(
            width = 3,
            box(
              title = "Age pyramid - Country 2",
              width = 12,
              plotlyOutput("country2_barchart", height = "425px")
            )
          ),
          column(
            width = 3,
            valueBoxOutput("fertilityBox5", width = 12),
            valueBoxOutput("fertilityBox7", width = 12),
            valueBoxOutput("marriageBox2", width = 12),
            valueBoxOutput("divorceBox2", width = 12)
          )
        ),
        fluidRow(
          valueBoxOutput("migrationFBox1", width = 3),
          valueBoxOutput("migrationMBox1", width = 3),
          valueBoxOutput("migrationFBox2", width = 3),
          valueBoxOutput("migrationMBox2", width = 3)
        )
      ),
      
      tabItem(
        tabName = "info",
        fluidRow(
          box(
            title = "Life Expectancy",
            width = 12, background = "navy", collapsed = TRUE,
            "Life expectancy at birth is defined as the mean number of years that a new-born child can expect to live if subjected throughout his life to the current mortality conditions (age specific probabilities of dying)."
          ),
          box(
            title = "Fertility Rate",
            width = 12, background = "navy", collapsed = TRUE,
            "The mean number of children that would be born alive to a woman during her lifetime if she were to survive and pass through her childbearing years conforming to the fertility rates by age of a given year."
          ),
          box(
            title = "Infant Mortality",
            width = 12, background = "navy", collapsed = TRUE,
            "The ratio of the number of deaths of children under one year of age during the year to the number of live births in that year. The value is expressed per 1,000 live births."
          ),
          box(
            title = "Crude divorce rate",
            width = 12, background = "navy", collapsed = TRUE,
            "The crude divorce rate is the ratio of the number of divorces during the year to the average population in that year. The value is expressed per 1000 persons."
          ),
          box(
            title = "Social protection expenditure",
            width = 12, background = "navy", collapsed = TRUE,
            "Expenditure on family/children function in Euro per inhabitant (at constant 2015 prices)."
          ),
          box(
            title = "Net migration",
            width = 12, background = "navy", collapsed = TRUE,
            "The net migration plus adjustment is calculated as the difference between the total change and the natural change of the population."
          )
        )
      )
    )
  )
)


##################### SERVER ################################################### 
server <- function(input, output) {
  
  # Set the default bounding box to Europe.
  tmap_options(bbox = st_bbox(c(
    xmin = -25, xmax = 40, ymin = 34, ymax = 72
  ), crs = 4326)) # Ustawienia współrzędnych Europy
  
  # Reaktywny filtr danych
  population_map_filtered <- reactive({
    finaldata %>% 
      filter(YEAR == input$selected_year & sex == "Total") %>%
      rename(Population = values, CountryID = geo, Country = name)
  })
  
  # Polulation table
  output$table <- renderDataTable({
    datatable(
      data = population_map_filtered() %>%
        st_set_geometry(NULL) %>%
        dplyr::select(CountryID, Country, Population) %>%
        mutate(Population = paste0(scales::number(Population / 1e6, accuracy = 0.01), " M")), 
      selection = "multiple",
      options = list(pageLength = 12)
    )
  })
  
  # Population map
  output$map <- renderTmap({
    selected_rows <- input$table_rows_selected
    selected_countries <- population_map_filtered()$CountryID[selected_rows]
    
    # Obsługa pustego zaznaczenia
    if (length(selected_countries) == 0) {
      tm_shape(population_map_filtered()) +
        tm_polygons(
          col = "Population", palette = "Blues", border.col = "dimgray",
          title = "Population"
        )
    } else {
      tm_shape(population_map_filtered()) +
        tm_polygons(
          col = "Population", palette = "Blues", border.col = "dimgray",
          title = "Population"
        ) +
        tm_shape(population_map_filtered() %>% filter(CountryID %in% selected_countries)) +
        tm_borders(col = "orangered", lwd = 2) # Podświetlenie zaznaczonych krajów
    }
  })
  
  # Life expectancy box
  output$lifeexpectancyBox <- renderValueBox({
    life_expectancy_box1 <- life_expectancy %>%
      filter(YEAR == input$selected_year & geo == "EU27_2020" & sex == "T")
    valueBox(
      "EU Life Expectancy", 
      ifelse(input$selected_year %in% life_expectancy_box1$YEAR, life_expectancy_box1$values, "No data"), 
      icon = icon("person-cane"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Fertility rate box
  output$fertilityBox <- renderValueBox({
    fertility_rate_box1 <- fertility_rate %>%
      filter(YEAR == input$selected_year & geo == "EU27_2020")
    valueBox(
      "EU Fertility Rate", 
      ifelse(input$selected_year %in% fertility_rate_box1$YEAR, fertility_rate_box1$values, "No data"),
      icon = icon("person-breastfeeding"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Infant mortality box
  output$infantmortalityBox <- renderValueBox({
    infant_mortality_box1 <- infant_mortality %>%
      filter(YEAR == input$selected_year & geo == "EU27_2020")
    valueBox(
      "EU Infant Mortality", 
      ifelse(input$selected_year %in% infant_mortality_box1$YEAR, infant_mortality_box1$values, "No data"),
      icon = icon("baby"),
      color = "lightblue",
      width = 4
    ) 
  })
  
  # Population trend plot
  output$trend_plot <- renderPlotly({
    population_trend <- finaldata %>%
      filter(geo %in% input$selected_country & sex == "Total") %>%
      rename(Year = YEAR, Value = values) %>%
      group_by(Year, geo) %>%
      summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop")
    
    trend_plot <- ggplot(population_trend, aes(x = Year, y = Value)) +
      geom_line(color = "midnightblue", linewidth = 1) +
      labs(x = "Year", y = "Total Population") +
      scale_x_continuous(
        breaks = seq(min(population_trend$Year), max(population_trend$Year), by = 10),
        limits = c(min(population_trend$Year), max(population_trend$Year))
      ) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) +
      theme_minimal()
    
    ggplotly(trend_plot) %>%
      layout(legend = list(title = list(text = "Country")))
  })
  
  # Barchart for population by sex and age
  output$barchart_agegroup <- renderPlotly({
    age_distribution_detailed <- population_pjangroup %>%
      filter(geo %in% input$selected_country & YEAR == input$selected_year2)
    
    age_order <- c("0-5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    
    age_distribution_detailed <- age_distribution_detailed %>%
      mutate(values = ifelse(sex == "M", -values, values))
    
    pyramid_plot <- ggplot(age_distribution_detailed, aes(x = factor(age, levels = age_order), y = values, fill = sex, text = paste("Sex: ", sex, "<br>Age group: ", age, "<br>Value: ", abs(values)))) +
      geom_bar(stat = "identity", position = "identity") +
      coord_flip() +
      scale_y_continuous(labels = scales::label_number(scale = 1e3, suffix = " K")) +
      labs(x = "Age Group", y = "Population") +
      theme_minimal() +
      scale_fill_manual(values = c("M" = "midnightblue", "F" = "darkred")) +
      theme(
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    ggplotly(pyramid_plot, tooltip = "text")
  })
  
  # Fertility rate (Country specific)
  output$fertilityBox2 <- renderValueBox({
    fertility_rate_box2 <- fertility_rate %>%
      filter(YEAR == input$selected_year2 & geo %in% input$selected_country)
    valueBox(
      "Fertility Rate",
      ifelse(input$selected_year2 %in% fertility_rate_box2$YEAR, fertility_rate_box2$values, "No data"),
      icon = icon("baby"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Mean age at first birth (Country specific)
  output$fertilityBox3 <- renderValueBox({
    fertility_rate_box3 <- fertility_ind %>%
      filter(YEAR == input$selected_year2 & geo %in% input$selected_country)
    valueBox(
      "Mean age of women at birth of first child",
      ifelse(input$selected_year2 %in% fertility_rate_box3$YEAR, fertility_rate_box3$values, "No data"),
      icon = icon("person-breastfeeding"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Mean age at first marriage (Females)
  output$marriageBox <- renderValueBox({
    marriage_box <- marriage %>%
      filter(YEAR == input$selected_year2 & geo %in% input$selected_country)
    valueBox(
      "Mean age at first marriage - Females",
      ifelse(input$selected_year2 %in% marriage_box$YEAR, marriage_box$values, "No data"),
      icon = icon("ring"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Divorce rate (Country specific)
  output$divorceBox <- renderValueBox({
    divorce_box <- divorce %>%
      filter(YEAR == input$selected_year2 & geo %in% input$selected_country)
    valueBox(
      "Crude divorce rate",
      ifelse(input$selected_year2 %in% divorce_box$YEAR, divorce_box$values, "No data"),
      icon = icon("heart-crack"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Migration data (Female)
  output$migrationFBox <- renderValueBox({
    migrationF_box <- migration %>%
      filter(YEAR == input$selected_year2 & geo %in% input$selected_country & sex == "F")
    valueBox(
      "Net migration - Females",
      ifelse(input$selected_year2 %in% migrationF_box$YEAR, migrationF_box$values, "No data"),
      icon = icon("person-dress"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Migration data (Male)
  output$migrationMBox <- renderValueBox({
    migrationM_box <- migration %>%
      filter(YEAR == input$selected_year2 & geo %in% input$selected_country & sex == "M")
    valueBox(
      "Net migration - Males",
      ifelse(input$selected_year2 %in% migrationM_box$YEAR, migrationM_box$values, "No data"),
      icon = icon("person"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Social protection trend
  output$social <- renderPlotly({
    social_trend <- social_protection %>% 
      filter(geo %in% input$selected_country) %>%
      rename(Year = YEAR, Value = values)
    
    trend_plot <- ggplot(social_trend, aes(x = Year, y = Value)) +
      geom_line(color = "midnightblue", linewidth = 1) +
      labs(x = "Year", y = "Euro per inhabitant") +
      scale_x_continuous(
        breaks = seq(min(social_trend$Year), max(social_trend$Year), by = 5),
        limits = c(min(social_trend$Year), max(social_trend$Year))
      ) +
      theme_minimal()
    
    ggplotly(trend_plot) %>%
      layout(legend = list(title = list(text = "Country")))
  })
  
  # Population trend plot - country 1
  output$country1_plot <- renderPlotly({
    country1_trend <- finaldata %>%
      filter(geo %in% input$country1 & sex == "Total") %>%
      rename(Year = YEAR, Value = values) %>%
      group_by(Year, geo) %>%
      summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop")
    
    country1_plot <- ggplot(country1_trend, aes(x = Year, y = Value)) +
      geom_line(color = "midnightblue", linewidth = 1) +
      labs(x = "Year", y = "Total Population") +
      scale_x_continuous(
        breaks = seq(min(country1_trend$Year), max(country1_trend$Year), by = 10),
        limits = c(min(country1_trend$Year), max(country1_trend$Year))
      ) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) +
      theme_minimal()
    
    ggplotly(country1_plot) %>%
      layout(legend = list(title = list(text = "Country")))
  })
  
  # Population trend plot - country 2
  output$country2_plot <- renderPlotly({
    country2_trend <- finaldata %>%
      filter(geo %in% input$country2 & sex == "Total") %>%
      rename(Year = YEAR, Value = values) %>%
      group_by(Year, geo) %>%
      summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop")
    
    country2_plot <- ggplot(country2_trend, aes(x = Year, y = Value)) +
      geom_line(color = "midnightblue", linewidth = 1) +
      labs(x = "Year", y = "Total Population") +
      scale_x_continuous(
        breaks = seq(min(country2_trend$Year), max(country2_trend$Year), by = 10),
        limits = c(min(country2_trend$Year), max(country2_trend$Year))
      ) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M")) +
      theme_minimal()
    
    ggplotly(country2_plot) %>%
      layout(legend = list(title = list(text = "Country")))
  })
  
  # Barchart for population by sex and age - country 1
  output$country1_barchart <- renderPlotly({
    age_distribution_country1 <- population_pjangroup %>%
      filter(geo %in% input$country1 & YEAR == input$selected_year3)
    
    age_order <- c("0-5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    
    age_distribution_country1 <- age_distribution_country1 %>%
      mutate(values = ifelse(sex == "M", -values, values))
    
    country1_barchart <- ggplot(age_distribution_country1, aes(x = factor(age, levels = age_order), y = values, fill = sex, text = paste("Sex: ", sex, "<br>Age group: ", age, "<br>Value: ", abs(values)))) +
      geom_bar(stat = "identity", position = "identity") +
      coord_flip() +
      scale_y_continuous(labels = scales::label_number(scale = 1e3, suffix = " K")) +
      labs(x = "Age Group", y = "Population") +
      theme_minimal() +
      scale_fill_manual(values = c("M" = "midnightblue", "F" = "darkred")) +
      theme(
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    ggplotly(country1_barchart, tooltip = "text")
  })
  
  # Barchart for population by sex and age - country 2
  output$country2_barchart <- renderPlotly({
    age_distribution_country2 <- population_pjangroup %>%
      filter(geo %in% input$country2 & YEAR == input$selected_year3)
    
    age_order <- c("0-5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    
    age_distribution_country2 <- age_distribution_country2 %>%
      mutate(values = ifelse(sex == "M", -values, values))
    
    country2_barchart <- ggplot(age_distribution_country2, aes(x = factor(age, levels = age_order), y = values, fill = sex, text = paste("Sex: ", sex, "<br>Age group: ", age, "<br>Value: ", abs(values)))) +
      geom_bar(stat = "identity", position = "identity") +
      coord_flip() +
      scale_y_continuous(labels = scales::label_number(scale = 1e3, suffix = " K")) +
      labs(x = "Age Group", y = "Population") +
      theme_minimal() +
      scale_fill_manual(values = c("M" = "midnightblue", "F" = "darkred")) +
      theme(
        legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    ggplotly(country2_barchart, tooltip = "text")
  })

  # Fertility rate (Country specific) - country 1
  output$fertilityBox4 <- renderValueBox({
    fertility_rate_box4 <- fertility_rate %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country1)
    valueBox(
      "Fertility Rate  - Country 1",
      ifelse(input$selected_year3 %in% fertility_rate_box4$YEAR, fertility_rate_box4$values, "No data"),
      icon = icon("baby"),
      color = "lightblue",
      width = 3
    )
  })
  
  # Fertility rate (Country specific) - country 2
  output$fertilityBox5 <- renderValueBox({
    fertility_rate_box5 <- fertility_rate %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country2)
    valueBox(
      "Fertility Rate - Country 2",
      ifelse(input$selected_year3 %in% fertility_rate_box5$YEAR, fertility_rate_box5$values, "No data"),
      icon = icon("baby"),
      color = "lightblue",
      width = 3
    )
  })
  
  # Mean age at first birth (Country specific) - country 1
  output$fertilityBox6 <- renderValueBox({
    fertility_rate_box6 <- fertility_ind %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country1)
    valueBox(
      "Mean age of women at birth of first child - Country 1",
      ifelse(input$selected_year3 %in% fertility_rate_box6$YEAR, fertility_rate_box6$values, "No data"),
      icon = icon("person-breastfeeding"),
      color = "lightblue",
      width = 3
    )
  })
  
  # Mean age at first birth (Country specific) - country 2
  output$fertilityBox7 <- renderValueBox({
    fertility_rate_box7 <- fertility_ind %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country2)
    valueBox(
      "Mean age of women at birth of first child - Country 2",
      ifelse(input$selected_year3 %in% fertility_rate$YEAR, fertility_rate_box7$values, "No data"),
      icon = icon("person-breastfeeding"),
      color = "lightblue",
      width = 3
    )
  })
  
  # Mean age at first marriage (Females) - country 1
  output$marriageBox1 <- renderValueBox({
    marriage_box1 <- marriage %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country1)
    valueBox(
      "Mean age at first marriage - Females  - Country 1",
      ifelse(input$selected_year3 %in% marriage_box1$YEAR, marriage_box1$values, "No data"),
      icon = icon("ring"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Mean age at first marriage (Females) - country 2
  output$marriageBox2 <- renderValueBox({
    marriage_box2 <- marriage %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country2)
    valueBox(
      "Mean age at first marriage - Females - Country 2",
      ifelse(input$selected_year3 %in% marriage_box2$YEAR, marriage_box2$values, "No data"),
      icon = icon("ring"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Wyjście: Divorce rate (Country specific) - country 1
  output$divorceBox1 <- renderValueBox({
    divorce_box1 <- divorce %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country1)
    valueBox(
      "Crude divorce rate - Country 1",
      ifelse(input$selected_year3 %in% divorce_box1$YEAR, divorce_box1$values, "No data"),
      icon = icon("heart-crack"),
      color = "lightblue",
      width = 4
    )
  })
  
  # Wyjście: Divorce rate (Country specific) - country 2
  output$divorceBox2 <- renderValueBox({
    divorce_box2 <- divorce %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country2)
    valueBox(
      "Crude divorce rate - Country 2",
      ifelse(input$selected_year3 %in% divorce_box2$YEAR, divorce_box2$values, "No data"),
      icon = icon("heart-crack"),
      color = "lightblue",
      width = 4
    )
  })

  # Migration data (Female) - country 1
  output$migrationFBox1 <- renderValueBox({
    migrationF_box1 <- migration %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country1 & sex == "F")
    valueBox(
      "Net migration - Females - Country 1",
      ifelse(input$selected_year3 %in% migrationF_box1$YEAR, migrationF_box1$values, "No data"),
      icon = icon("person-dress"),
      color = "lightblue",
      width = 4
    )
  })

  # Migration data (Female) - country 2
  output$migrationFBox2 <- renderValueBox({
    migrationF_box2 <- migration %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country2 & sex == "F")
    valueBox(
      "Net migration - Females - Country 2",
      ifelse(input$selected_year3 %in% migrationF_box2$YEAR, migrationF_box2$values, "No data"),
      icon = icon("person-dress"),
      color = "lightblue",
      width = 4
    )
  })

  # Migration data (Male) - country 1
  output$migrationMBox1 <- renderValueBox({
    migrationM_box1 <- migration %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country1 & sex == "M")
    valueBox(
      "Net migration - Males - Country 1",
      ifelse(input$selected_year3 %in% migrationM_box1$YEAR, migrationM_box1$values, "No data"),
      icon = icon("person"),
      color = "lightblue",
      width = 4
    )
  })

  # Migration data (Male) - country 2
  output$migrationMBox2 <- renderValueBox({
    migrationM_box2 <- migration %>%
      filter(YEAR == input$selected_year3 & geo %in% input$country2 & sex == "M")
    valueBox(
      "Net migration - Males - Country 2",
      ifelse(input$selected_year3 %in% migrationM_box2$YEAR, migrationM_box2$values, "No data"),
      icon = icon("person"),
      color = "lightblue",
      width = 4
    )
  })
  
}

##################### RUN APP ##################################################
shinyApp(ui, server)
