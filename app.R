library(shiny)
library(leaflet)
library(DT)
library(ggplot2)
library(rsconnect)
library(plotly)
library(ggridges)
library(bslib)

combined_data <- readRDS("data/SoilChamber_1.rds")
data.2 <- readRDS("data/SoilChamber_2.rds")
data.3 <- readRDS("data/Beringian_coastal_wetlands_CH4.rds")
data.4 <- readRDS("data/Annual_CH4_estimates.rds")

# addition of dates based on the references - needed for graphs 

data.3$Date <- as.Date(
  sapply(data.3$Reference, function(ref) {
    year <- regmatches(ref, regexpr("\\b\\d{4}\\b", ref))
    if (length(year) > 0) {
      return(paste0(year, "-06-01"))
    } else {
      return(NA)
    }
  }),
  format = "%Y-%m-%d"
)


# start of the app (ui)

ui <- fluidPage(
  # colors 
  theme = bs_theme(
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto"),
    primary = "#EEAD0E",
    secondary = "#2196F3",
    success = "#4CAF50",
    info = "#2196F3",
    warning = "#FFC107",
    danger = "#F44336",
    light = "#FAFAFA",
    dark = "#333333"
  ),

  # custom CSS
  tags$head(
    tags$style(HTML("
      .titlePanel { padding: 60px 50px; background-color: #f5f5f5; border-bottom: 2px solid}
       .navbar .navbar-nav { margin-top: 10px;}
      body { font-family: 'Open Sans', sans-serif; background-color: #FAFAFA; color: #333; }
      h1, h2, h3 { font-family: 'Roboto', sans-serif; color: #6B8E23; }
      .btn { background-color: #2196F3; color: #ffffff; }
      .btn:hover { background-color: #1976D2; }
      .shiny-input-container { margin-bottom: 15px; }
      .well { background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    "))
  ),
  titlePanel("Methane (CH4) Flux Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a Dataset:",
                  choices = c("CH4 consumption by Upland soils; High Resolution" = "combined_data",
                              "CH4 consumption by Upland soils; Manual Measurements" = "data.2",
                              "CH4 fluxes of Beringian Coastal Wetlands" = "data.3",
                              "CH4 fluxes from Temperate, Boreal, and Arctic Wetlands" = "data.4")),
      selectInput("ch4Type", "Select CH4 Variable:",
                  choices = NULL),
      checkboxInput("useDateFilter", "Apply Date Filter", value = FALSE),
      ## my favorite thing about this app
      sliderInput("fluxRange", "CH4 Flux Concentration Range:",
                  min = -50, max = 50, value = c(-10, 10)),
      conditionalPanel(
        condition = "input.useDateFilter == true",
        dateRangeInput("dateRange", "Select Date Range:",
                       start = 1900-12-31, end = "2024-12-31")
      ),
      conditionalPanel(
        condition = "input.dataset == 'data.4'",
        checkboxGroupInput("ecosystemType", "Select Ecosystem:",
                           choices = NULL)
      ),
      conditionalPanel(
        condition = "input.dataset == 'data.3'",
        checkboxGroupInput("standardizedType", "Select Ecosystem:",
                           choices = NULL)
      ),
      conditionalPanel(
        condition = "input.dataset == 'combined_data' || input.dataset == 'data.2'",
        checkboxGroupInput("landCover", "Select Vegetation:",
                           choices = NULL)
      ),

      uiOutput("plotTypeSelector")
    ),
    mainPanel(
      tabsetPanel(
        id = "mainTabs",
        tabPanel("About",
                 tags$div(class = "about-section",
                          h3("Welcome to the Methane (CH4) Flux Data Explorer!"),
                          h4("Hello!"),
                          p("The Methane Flux Data Explorer is an interactive tool designed for the exploration, visualization,
          and analysis of methane fluxes across various ecosystems. This application aims to support anyone
          in understanding the complex dynamics of methane dynamics and their implications on climate change
          and ecosystem health."),

                          h4("How to Use This Tool"),
                          tags$ul(
                            tags$li("Select a Dataset: Choose from any of the datasets to explore different aspects of methane fluxes."),
                            tags$li("Visualize Data: Use the map to visualize geographic locations, differences across types of
              vegetation or soil, and more."),
                            tags$li("Detailed Analysis: Access detailed tables for in-depth analysis of the data, including methane
              concentrations, vegetation types, and other ecosystem parameters."),
                            tags$li("Customize Views: Apply filters such as date ranges, CH4 concentration, or vegetation types to guide
              the information to your specific research need.")
                          ),

                          h4("Features"),
                          tags$ul(
                            tags$li("Interactive Maps: Geographically explore the distribution of methane fluxes."),
                            tags$li("Dynamic Graphs: Generate customizable plots to visualize data trends."),
                            tags$li("Data Tables: Examine detailed, filterable tables of the dataset entries."),
                            tags$li("Research Insights: Delve into synthesized research findings and data summaries.")
                          ),

                          tags$div(style = "text-align: center;",
                                   p("Enjoy!"),
                                   strong("Lyreshka C.M.")
                          )
                 )
         ),
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Trends", plotlyOutput("trendPlotInteractive")),
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Overview",
                 tableOutput("overview"),
                 textOutput("citation")),
        tabPanel("Research",

                 h4("CH4 consumption by Upland soils; High Resolution"),
                 p("This dataset includes two data tables of methane (CH4) fluxes measured in Arctic uplands. Dataset 1 contains CH4 fluxes measured at high temporal resolution (hourly fluxes) collected over two snow-free seasons (June–August; 2019, 2021) at Trail Valley Creek, an Arctic tundra site in the Western Canadian Arctic. Fluxes were measured with automated chambers installed in replication of six at three individual land cover vegetation units (Lichen, Shrub, Tussock) within dwarf-shrub dominated tundra. Site meteorological data are provided with the flux data at hourly resolution. Dataset 2 includes campaign-based, manual chamber measurements at sites displaying net CH4 uptake. These manual measurements were conducted during the growing season at typical, well-drained upland sites, which included, besides Trail Valley Creek, three additional sites in the Canadian and European Arctic (Havikpak Creek, Scotty Creek, Kilpisjärvi). Besides CH4 flux observations, dataset 2 contains measured greenhouse gas concentration profiles of CH4, carbon dioxide (CO2) and nitrous oxide (N2O) at 2 cm, 5 cm, 10 cm, and 20 cm soil depths, as well as site meteorological data. While wetlands are known CH4 emitters, drier arctic and boreal uplands may act as sinks of atmospheric CH4. The scope of the study and this dataset is to improve the spatial and temporal coverage of low CH4 emitting and sites displaying net CH4 uptake across the Arctic. Both datasets are meant as a supplement to the published study, where further, detailed information on site conditions and methodology can be found."),
                 p("Voigt, Carolina; Chevrier-Dion, Charles; Marquis, Charlotte; Nesic, Zoran; Hould Gosselin, Gabriel; Saarela, Taija; Virkkala, Anna-Maria; Bennett, Kathryn A; Marushchak, Maija E; Wilcox, Evan James; Sonnentag, Oliver (2023): Atmospheric methane consumption by upland soils in the Western Canadian Arctic and Finnish Lapland (2018–2021) [dataset bundled publication]. PANGAEA, https://doi.org/10.1594/PANGAEA.953120"),

                 h4("CH4 consumption by Upland soils; Manual Measurements"),
                 p("This dataset includes campaign-based, manual chamber measurements at sites displaying net methane (CH4) uptake. These manual measurements were conducted during the growing season at typical, well-drained upland sites, which included, besides Trail Valley Creek, three additional sites in the Canadian and European Arctic (Havikpak Creek, Scotty Creek, Kilpisjärvi). Besides CH4 flux observations, the dataset contains measured greenhouse gas concentration profiles of CH4, carbon dioxide (CO2) and nitrous oxide (N2O) at 2 cm, 5 cm, 10 cm, and 20 cm soil depths, as well as site meteorological data."),
                 p("Voigt, Carolina; Chevrier-Dion, Charles; Marquis, Charlotte; Nesic, Zoran; Hould Gosselin, Gabriel; Saarela, Taija; Virkkala, Anna-Maria; Bennett, Kathryn A; Marushchak, Maija E; Wilcox, Evan James; Sonnentag, Oliver (2023): Atmospheric methane consumption by upland soils in the Western Canadian Arctic and Finnish Lapland (2018–2021) [dataset bundled publication]. PANGAEA, https://doi.org/10.1594/PANGAEA.953120"),

                 h4("CH4 fluxes of Beringian Coastal Wetlands"),
                 p("For upscaling CH4 flux estimates in Beringia during the past 20,000 years, we collected 231 present-day CH4 fluxes from coastal wetlands in the Northern Hemisphere. We combined our own flux data (27 plot measurements) from the Kenai Peninsula, Alaska with previously published data. Data were compiled from different sources (e.g. Treat et al. 2018; 2021; Poffenbarger et al. 2011; Liikanen et al. 2009; Holmquist et al. 2018; Kuhn et al. 2021). CH4 fluxes from the literature were calculated in g CH4 m-2 yr-1 for the growing season, which we set to 153 days (May to September). Each CH4 data entry was harmonized by classifying it into one of the six wetland types Saltwater, tidal regularly flooded, Temporarily irregularly flooded, Permanently to semi-permanently flooded, Seasonally flooded, Non-tidal saturated, Water-body. This resulted in a stratified pool of CH4 fluxes and allowed a bootstrapping approach to estimate uncertainty in the CH4 fluxes for Beringian coastal wetlands based on the variability of CH4 fluxes associated to the different wetland types. For each of 258 sites, the dataset includes a site description, calculated CH4 flux from this research, wetland type, wetland class, method of CH4 measurement, major vegetation type, site location, the originally published CH4 value (orig val) in the referenced paper, original units of measurement, citation and persistent identifier for the original data source, and comments. For some of the data points no coordinates information was given in the original publication, therefore the latitude and longitude fields were left blank."),
                 p("Fuchs, Matthias; Jones, Miriam C; Gowan, Evan J; Frolking, Steve; Walter Anthony, Katey M; Grosse, Guido; Jones, Benjamin M; O'Donnel, Jonathan; Brosius, Laura Susan; Treat, Claire C (2024): Data set for modeling methane fluxes of Beringian coastal wetlands [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.960160"),

                 h4("CH4 fluxes from Temperate, Boreal, and Arctic Wetlands"),
                 p("Wetlands are the single largest natural source of atmospheric methane (CH4), a greenhouse gas, and occur extensively in the northern hemisphere. Large discrepancies remain between bottom-up and top-down estimates of northern CH4 emissions. To explore whether these discrepancies are due to poor representation of non-growing season CH4 emissions, we synthesized non-growing season and annual CH4 flux measurements from temperate, boreal, and tundra wetlands and uplands. Median non-growing season wetland emissions ranged from 0.9 g m-2 in bogs to 5.2 g m-2 in marshes and were dependent on moisture, vegetation, and permafrost. Annual wetland emissions ranged from 0.9 g m-2 y-1 in tundra bogs to 78 g m-2 y-1 in temperate marshes. Uplands varied from CH4 sinks to CH4 sources with a median annual flux of 0.0 ± 0.2 g m-2 y-1. The measured fraction of annual CH4 emissions during the non-growing season (observed: 13 to 47%) was significantly larger than was predicted by two process-based model ensembles, especially between 40-60º N (modeled: 4 to 17%). Constraining the model ensembles with the measured non-growing fraction increased total non-growing season and annual CH4 emissions. Using this constraint, the modeled non-growing season wetland CH4 flux from >40Â° north was 6.1 ± 1.5 Tg y-1, three times greater than the non-growing season emissions of the unconstrained model ensemble. The annual wetland CH4 flux was 37 ± 7 Tg y-1 from the data-constrained model ensemble, 25% larger than the unconstrained ensemble. Considering non-growing season processes is critical for accurately estimating CH4 emissions from high latitude ecosystems, and necessary for constraining the role of wetland emissions in a warming climate. This dataset contains the synthesis of measured flux data from the study."),
                 p("Treat, Claire C; Bloom, A Anthony; Marushchak, Maija E (2018): Growing season, non-growing season and annual CH4 fluxes from temperate, boreal, and Arctic wetlands and uplands [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.886976")
        )
      )
    )
  )
)


# Server (backhand)
server <- function(input, output, session) {
  # Observe
  observe({
    dataset <- switch(input$dataset,
                      "combined_data" = combined_data,
                      "data.2" = data.2,
                      "data.3" = data.3,
                      "data.4" = data.4)

    ch4_columns <- colnames(dataset)[grepl("CH4", colnames(dataset))]
    updateSelectInput(session, "ch4Type", choices = ch4_columns)

    if (input$dataset == "data.4" && "Standardized_Ecosystem_Type" %in% colnames(dataset)) {
      updateCheckboxGroupInput(session, "ecosystemType",
                               choices = unique(dataset$Standardized_Ecosystem_Type),
                               selected = unique(dataset$Standardized_Ecosystem_Type))
    }

    if (input$dataset == "data.3" && "Standardized_Type" %in% colnames(dataset)) {
      updateCheckboxGroupInput(session, "standardizedType",
                               choices = unique(dataset$Standardized_Type),
                               selected = unique(dataset$Standardized_Type))
    }

    if ((input$dataset == "combined_data" || input$dataset == "data.2") && "Vegetation_Cover" %in% colnames(dataset)) {
      updateCheckboxGroupInput(session, "landCover",
                               choices = unique(dataset$Vegetation_Cover),
                               selected = unique(dataset$Vegetation_Cover))
    }

    if (input$mainTabs == "interactive") {
      updateSelectInput(session, "plotType",
                        choices = c("Line Plot" = "line", "Scatter Plot" = "scatter"),
                        selected = "line")
    } else if (input$mainTabs == "static") {
      updateSelectInput(session, "plotType",
                        choices = c("Ridgeline Plot" = "ridge"),
                        selected = "ridge")
    }
  })

  # Reactive function for data filtering
  # Feature 1: Filter dataset by ecosystem type/vegetation
  # Purpose: Helps users focus on specific ecosystem data relevant to their research or interest.
  # works on map as well and data table

  filteredData <- reactive({
    dataset <- switch(input$dataset,
                      "combined_data" = combined_data,
                      "data.2" = data.2,
                      "data.3" = data.3,
                      "data.4" = data.4)

    # Check if CH4 variable exists before filtering
    if (!input$ch4Type %in% colnames(dataset)) {
      return(data.frame())  # Return an empty data frame if invalid selection
    }

    # Filter based on date range
    if (input$useDateFilter && "Date" %in% colnames(dataset)) {
      dataset <- dataset[dataset$Date >= input$dateRange[1] & dataset$Date <= input$dateRange[2], ]
    }

    # Filter based on ecosystem type or vegetation cover
    if (input$dataset == "data.4" && "Standardized_Ecosystem_Type" %in% colnames(dataset)) {
      if (!is.null(input$ecosystemType)) {
        dataset <- dataset[dataset$Standardized_Ecosystem_Type %in% input$ecosystemType, ]
      }
    } else if (input$dataset == "data.3" && "Standardized_Type" %in% colnames(dataset)) {
      if (!is.null(input$standardizedType)) {
        dataset <- dataset[dataset$Standardized_Type %in% input$standardizedType, ]
      }
    } else if ((input$dataset == "combined_data" || input$dataset == "data.2") && "Vegetation_Cover" %in% colnames(dataset)) {
      if (!is.null(input$landCover)) {
        dataset <- dataset[dataset$Vegetation_Cover %in% input$landCover, ]
      }
    }

    # Filter based on CH4 flux concentration range
    if (input$ch4Type %in% colnames(dataset)) {
      dataset <- dataset[!is.na(dataset[[input$ch4Type]]) &
                           dataset[[input$ch4Type]] >= input$fluxRange[1] &
                           dataset[[input$ch4Type]] <= input$fluxRange[2], ]
    }

    return(dataset)
  })


  # Render map output
  # Feature 2: Interactive map that displays CH4 flux data with color-coded markers: Source = Red, Sink = Green
  # Purpose: Allows users to visualize the geographic distribution of CH4 flux values,
  # providing insight into spatial patterns.

  output$map <- renderLeaflet({
    data_map <- filteredData()

    if (nrow(data_map) > 0 && "Latitude" %in% colnames(data_map) && "Longitude" %in% colnames(data_map)) {
      leaflet(data_map) %>%
        addTiles() %>%
        addCircleMarkers(~Longitude, ~Latitude,
                         radius = ~abs(data_map[[input$ch4Type]]) / 10,
                         color = ~ifelse(data_map[[input$ch4Type]] > 0, "darkred", "darkgreen"),
                         popup = ~paste("Date:", ifelse("Date" %in% colnames(data_map), ifelse(is.na(Date),
                                                                                               "N/A", Date),
                                                        "N/A"), "<br>",
                                        input$ch4Type, ":", data_map[[input$ch4Type]]))
    } else {
      leaflet() %>% addTiles() %>% addLabelOnlyMarkers(
        lng = 0, lat = 0,
        label = "No data available for the selected filters",
        labelOptions = labelOptions(noHide = TRUE)
      )
    }
  })

  # Render data table
  output$dataTable <- renderDT({
    dataset <- filteredData()
    if (nrow(dataset) > 0) {
      datatable(dataset, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      return(NULL)
    }
  })

  # Render trends/analysis plot
  # Feature 3: Time-series plot for CH4 flux trends
  # Purpose: Enables users to track changes in CH4 flux over time, facilitating temporal analysis.
  # also can be filtered by ecosystem or Vegetation type depending on data set and flux concentration
  
  # V2 additions
  # makes it fun to play as it includes a scatter plot, box plot, line plot, and a trend analysis with temperature
  # Can be adjusted by filtering, by flux range, and date 
  # all plots are now interactive so they give you more info upon placing the cursor on top of them and 
  # can be exported as a png, zoomed in and more  
  
  
  
  output$plotTypeSelector <- renderUI({
    if (input$dataset %in% c("combined_data", "data.2", "data.4")) {
      selectInput("plotType", "Select Graph Type:",
                  choices = c("Box Plot" = "box",
                              "Line Plot" = "line",
                              "Scatter Plot" = "scatter",
                              "Temperature vs CH4 Flux Plot" = "tempCH4"),
                  selected = "box")
    } else {
      selectInput("plotType", "Select Graph Type:",
                  choices = c("Box Plot" = "box",
                              "Line Plot" = "line",
                              "Scatter Plot" = "scatter"),
                  selected = "box")
    }
  })




  output$trendPlotInteractive <- renderPlotly({
    req(input$plotType %in% c("box", "line", "scatter", "tempCH4"))
    data_plot <- filteredData()

    if (!"Date" %in% colnames(data_plot)) {
      return(NULL)
    }

    data_plot$Date <- as.Date(data_plot$Date, origin = "1970-01-01")

    vegetation_column <- switch(input$dataset,
                                "combined_data" = "Vegetation_Cover",
                                "data.2" = "Vegetation_Cover",
                                "data.3" = "Standardized_Type",
                                "data.4" = "Standardized_Ecosystem_Type")

    temp_column <- switch(input$dataset,
                          "combined_data" = "Air_Temp_C",
                          "data.2" = "Soil_Temp_Flux_C",
                          "data.4" = "Mean_Annual_Air_Temp_C",
                          NULL)

    if (input$plotType == "line" || input$plotType == "scatter") {
      plot <- ggplot(data_plot, aes(x = Date, y = .data[[input$ch4Type]], color = .data[[vegetation_column]])) +
        {if (input$plotType == "line") geom_line() else geom_point()} +
        labs(title = paste(input$plotType, "Plot of CH4 Flux Over Time"))
    } else if (input$plotType == "box") {
      plot <- ggplot(data_plot, aes(x = .data[[vegetation_column]], y = .data[[input$ch4Type]], fill = .data[[vegetation_column]])) +
        geom_boxplot() +
        labs(title = "Box Plot of CH4 Flux by Vegetation Type")
    } else if (input$plotType == "tempCH4" && !is.null(temp_column)) {
      data_plot <- data_plot[!is.na(data_plot[[temp_column]]),]  # Filter out NA temp values
      plot <- ggplot(data_plot, aes(x = .data[[temp_column]], y = .data[[input$ch4Type]], color = .data[[vegetation_column]])) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(title = "Temperature vs. CH4 Flux")
    }

    ggplotly(plot)
  })

  getDataset <- function(dataset_input) {
    switch(input$dataset,
           "combined_data" = combined_data,
           "data.2" = data.2,
           "data.3" = data.3,
           "data.4" = NULL

    )
  }

  output$overview <- renderTable({
    if (input$dataset == "data.4") {
      return(data.frame(Apologies = "Summary not available for this dataset."))
    }
    data <- getDataset()

    # Calculate summary statistics
    summary_df <- data.frame(
      Variable = colnames(data),
      Mean = sapply(data, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
      Median = sapply(data, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
      StdDev = sapply(data, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA),
      MissingValues = sapply(data, function(x) sum(!is.na(x)))
    )

    rownames(summary_df) <- NULL

    # Return
    summary_df
  }, align = 'l')  # align left



  # citation
  output$citation <- renderText({
    dataset <- input$dataset
    citations <- list(
      "combined_data" = "Voigt, Carolina; Chevrier-Dion, Charles; Marquis, Charlotte; Nesic, Zoran; Hould Gosselin, Gabriel; Saarela, Taija; Virkkala, Anna-Maria; Bennett, Kathryn A; Marushchak, Maija E; Wilcox, Evan James; Sonnentag, Oliver (2023): Atmospheric methane consumption by upland soils in the Western Canadian Arctic and Finnish Lapland (2018–2021) [dataset bundled publication]. PANGAEA, https://doi.org/10.1594/PANGAEA.953120",
      "data.2" = "Voigt, Carolina; Chevrier-Dion, Charles; Marquis, Charlotte; Nesic, Zoran; Hould Gosselin, Gabriel; Saarela, Taija; Virkkala, Anna-Maria; Bennett, Kathryn A; Marushchak, Maija E; Wilcox, Evan James; Sonnentag, Oliver (2023): Atmospheric methane consumption by upland soils in the Western Canadian Arctic and Finnish Lapland (2018–2021) [dataset bundled publication]. PANGAEA, https://doi.org/10.1594/PANGAEA.953120",
      "data.3" = "Fuchs, Matthias; Jones, Miriam C; Gowan, Evan J; Frolking, Steve; Walter Anthony, Katey M; Grosse, Guido; Jones, Benjamin M; O'Donnel, Jonathan; Brosius, Laura Susan; Treat, Claire C (2024): Data set for modeling methane fluxes of Beringian coastal wetlands [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.960160",
      "data.4" = "Treat, Claire C; Bloom, A Anthony; Marushchak, Maija E (2018): Growing season, non-growing season and annual CH4 fluxes from temperate, boreal, and Arctic wetlands and uplands [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.886976"
    )
    paste("Citation:", citations[[dataset]])
  })
}



shinyApp(ui = ui, server = server)
