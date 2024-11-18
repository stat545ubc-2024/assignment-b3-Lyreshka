library(shiny)
library(leaflet)
library(DT)
library(ggplot2)
library('rsconnect')
# remotes::install_github("Lyreshka/MethaneData")
library(MethaneData)


combined_data <- load_soilchamber_1()
data.2 <- load_soilchamber_2()
data.3 <- load_beringian_wetlands()
data.4 <- load_annual_estimates()

ui <- fluidPage(
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
      # my favorite thing about this app
      sliderInput("fluxRange", "CH4 Flux Concentration Range:",
                  min = -50, max = 50, value = c(-10, 10))
    ),
    mainPanel(
      tabsetPanel(
        id = "mainTabs",
        tabPanel("About",
                 h3("Welcome to the Methane (CH4) Flux Data Explorer!"),
                 p("Dive into the world of methane flux analysis with my interactive application. Here, you can explore a variety of methane datasets from upland soils, coastal wetlands, and temperate to Arctic ecosystems. Whether you're interested in high-resolution data or annual flux estimates, this app provides the tools to visualize, analyze, and interpret methane emissions and consumption trends."),
                 p("Use the map to pinpoint data locations, examine detailed tables for in-depth analysis, and track trends over time with our intuitive plots. This explorer is designed to support researchers, students, and environmental enthusiasts in understanding greenhouse gas dynamics in diverse ecosystems. Enjoy your exploration!"),
                 p("Created by Lyreshka C.M.")
        ),
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Trends", plotOutput("trendPlot")),
        tabPanel("Overview",
                 verbatimTextOutput("overview"),
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
  })

  # Reactive function for data filtering
  # Feature 2: Filter dataset by ecosystem type/vegetation
  # Purpose: Helps users focus on specific ecosystem data relevant to their research or interest.
  # works on map as well and data table


  filteredData <- reactive({
    dataset <- switch(input$dataset,
                      "combined_data" = combined_data,
                      "data.2" = data.2,
                      "data.3" = data.3,
                      "data.4" = data.4)

    if (input$useDateFilter && "Date" %in% colnames(dataset)) {
      dataset <- dataset[dataset$Date >= input$dateRange[1] & dataset$Date <= input$dateRange[2], ]
    }

    if (input$dataset == "data.4" && !is.null(input$ecosystemType) && "Standardized_Ecosystem_Type" %in% colnames(dataset)) {
      dataset <- dataset[dataset$Standardized_Ecosystem_Type %in% input$ecosystemType, ]
    } else if (input$dataset == "data.3" && !is.null(input$standardizedType) && "Standardized_Type" %in% colnames(dataset)) {
      dataset <- dataset[dataset$Standardized_Type %in% input$standardizedType, ]
    } else if ((input$dataset == "combined_data" || input$dataset == "data.2") && !is.null(input$landCover) && "Vegetation_Cover" %in% colnames(dataset)) {
      dataset <- dataset[dataset$Vegetation_Cover %in% input$landCover, ]
    }

    if (input$ch4Type %in% colnames(dataset)) {
      dataset <- dataset[!is.na(dataset[[input$ch4Type]]) &
                           dataset[[input$ch4Type]] >= input$fluxRange[1] &
                           dataset[[input$ch4Type]] <= input$fluxRange[2], ]
    }

    dataset
  })

  # Render map output
  # Feature 1: Interactive map that displays CH4 flux data with color-coded markers
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
  # makes it fun to play with, maybe in the future I could have different filters depending on dataset
  # but requires more clean up and data standardization (especially with vegetation)


  output$trendPlot <- renderPlot({
    data_plot <- filteredData()

    if (input$ch4Type %in% colnames(data_plot)) {
      if ("Date" %in% colnames(data_plot) && !all(is.na(data_plot$Date))) {
        data_plot$Date <- as.Date(data_plot$Date, origin = "1970-01-01")

        ggplot(data_plot, aes(x = Date, y = .data[[input$ch4Type]], color = .data[[input$ch4Type]])) +
          geom_line() +
          scale_color_gradient2(low = "darkgreen", mid = "gray", high = "darkred", midpoint = 0) +
          theme_minimal() +
          labs(title = "CH4 Trend Analysis", x = "Date",
               y = paste("CH4 Value (", input$ch4Type, ")", sep = ""))
      } else {
        ggplot(data_plot, aes(x = seq_along(data_plot[[input$ch4Type]]), y = .data[[input$ch4Type]],
                              color = .data[[input$ch4Type]])) +
          geom_line() +
          scale_color_gradient2(low = "darkgreen", mid = "gray", high = "darkred", midpoint = 0) +
          theme_minimal() +
          labs(title = "CH4 Trend Analysis (Index)", x = "Index",
               y = paste("CH4 Value (", input$ch4Type, ")", sep = ""))
      }
    }
  })

  # Render overview
  # Gives a quick overview of the data (min, mean, max, q1, q3)
  # (I dont really count it as feature but its insightful to me)
  # the reason is because only a select portion of users would find it insightful otherwise its just ehhh

  output$overview <- renderPrint({
    dataset <- filteredData()
    if (nrow(dataset) > 0) {
      summary(dataset)
    } else {
      print("No data available for the selected filters.")
    }
  })

  # Display citation
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

