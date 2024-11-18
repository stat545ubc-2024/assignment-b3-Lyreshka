# Methane Flux Data Explorer

Welcome to the **Methane Flux Data Explorer** repository! This project features an interactive Shiny app for visualizing and analyzing methane (CH4) flux data from various ecosystems.

## Running Instance of the Shiny App

You can access the deployed Shiny app at the following link:
[Explore the Methane Flux Data](https://lyreshka.shinyapps.io/MethaneFluxDataExplorer/)

## Description of the App

The Methane Flux Data Explorer allows users to:

- View methane flux data on an interactive map with color-coded markers.
- Filter data by date range, ecosystem type, vegetation cover, and CH4 concentration range.
- Explore trends over time using customizable plots.
- Inspect data tables and summaries for detailed analysis.
- Access descriptions and metadata for various methane flux datasets.

This app is designed to support environmental researchers, students, and enthusiasts interested in studying greenhouse gas dynamics across diverse ecosystems.

## Datasets Used and Acknowledgements

This app uses multiple datasets related to methane fluxes:

1. **CH4 Consumption by Upland Soils (High-Resolution & Manual Measurements)**
   - Voigt, Carolina; Chevrier-Dion, Charles; Marquis, Charlotte; Nesic, Zoran; Hould Gosselin, Gabriel; Saarela, Taija; Virkkala, Anna-Maria; Bennett, Kathryn A; Marushchak, Maija E; Wilcox, Evan James; Sonnentag, Oliver (2023): *Atmospheric methane consumption by upland soils in the Western Canadian Arctic and Finnish Lapland (2018–2021)*. PANGAEA. [DOI: 10.1594/PANGAEA.953120](https://doi.org/10.1594/PANGAEA.953120)

2. **CH4 Fluxes of Beringian Coastal Wetlands**
   - Fuchs, Matthias; Jones, Miriam C; Gowan, Evan J; et al. (2024): *Data set for modeling methane fluxes of Beringian coastal wetlands*. PANGAEA. [DOI: 10.1594/PANGAEA.960160](https://doi.org/10.1594/PANGAEA.960160)

3. **CH4 Fluxes from Temperate, Boreal, and Arctic Wetlands**
   - Treat, Claire C; Bloom, A Anthony; Marushchak, Maija E (2018): *Growing season, non-growing season, and annual CH4 fluxes from temperate, boreal, and Arctic wetlands and uplands*. PANGAEA. [DOI: 10.1594/PANGAEA.886976](https://doi.org/10.1594/PANGAEA.886976)

## Open and Reproducible Science

All datasets are publicly accessible via PANGAEA and acknowledged with proper citations. The app processes these data through the `MethaneData` package, ensuring an open and reproducible workflow.

Felden, J; Möller, L; Schindler, U; Huber, R; Schumacher, S; Koppe, R; Diepenbroek, M; Glöckner, FO (2023): PANGAEA – Data Publisher for Earth & Environmental Science. Scientific Data, 10(1), 347, https://doi.org/10.1038/s41597-023-02269-x

## How to Run the App Locally

To run this app locally:

1. Clone this repository to your local machine.
2. Ensure that you have installed the necessary R packages (`shiny`, `leaflet`, `ggplot2`, `DT`, etc.).
3. Run the app using the `shiny::runApp()` function or by opening the `app.R` file in RStudio and clicking **Run App**.

## The `MethaneData` Package

The **MethaneData** package is a custom R package designed to facilitate the loading and processing of methane flux datasets used in this app. This package provides a streamlined way to load the datasets into your R environment, ensuring a consistent and reproducible workflow for methane data analysis.

### Features of the `MethaneData` Package

- Functions to load pre-processed datasets such as:
  - `load_soilchamber_1()` for high-resolution CH4 flux data.
  - `load_soilchamber_2()` for manual CH4 measurements.
  - `load_beringian_wetlands()` for CH4 flux data from Beringian coastal wetlands.
  - `load_annual_estimates()` for annual CH4 flux estimates from various wetland and upland ecosystems.
- Ensures data integrity by storing datasets in a standardized format within the package and thus no data downloading on your part!
  

### Installation

To install the **MethaneData** package from GitHub, use:

```R
remotes::install_github("Lyreshka/MethaneData")
```
For more information visit https://github.com/Lyreshka/MethaneData

