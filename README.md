# U.S. Electric Vehicle (EV) Dashboard

This project is a Shiny-based interactive dashboard that provides an overview and analysis of electric vehicle (EV) registrations, charging infrastructure, and related government regulations in the United States.

## 📦 Project Structure

- **Data/**:  
  Contains all source data files used in this project, including:
  - EV registration data
  - EV charging station data
  - EV incentives and regulations data
  - Greenhouse gas (GHG) emissions data
- **app.R**:  
  Main Shiny application file.

## 🚗 Dashboard Overview

The dashboard consists of four main tabs:

### 1. EV Registration
- Overview of EV registrations and charging stations from 2019 to 2023.
- Filtering options by state, map type, and year.
- Interactive maps using Highchart.
- Value boxes displaying totals.

### 2. Infrastructure
- Map of EV charging station locations.
- Leaflet-based interactive map with clustering and detailed pop-ups.

### 3. Region
- Regional comparison (Midwest, Northeast, South, West).
- Filtering by region and year.
- Top 5 states display.
- Packed Bubble Chart visualization.

### 4. EV Incentives & Regulation & GHG
- Correlation between EV incentives/regulations and greenhouse gas emissions.
- Combined bar and line charts.
- Value boxes highlighting key numbers.

## 📊 Data Sources

- U.S. Department of Energy's Alternative Fuels Data Center (AFDC):  
  [https://afdc.energy.gov/](https://afdc.energy.gov/)
- United States Environmental Protection Agency (EPA):  
  [https://cfpub.epa.gov/ghgdata/inventoryexplorer/index.html](https://cfpub.epa.gov/ghgdata/inventoryexplorer/index.html)
- Geographical data from Highcharts map collection:  
  [https://github.com/highcharts/map-collection-dist/blob/master/countries/us/us-all-all.geo.json](https://github.com/highcharts/map-collection-dist/blob/master/countries/us/us-all-all.geo.json)

## 🛠️ How to Run the Shiny App

1. Clone or download this repository.
2. Ensure all files (especially the **Data/**) are present in the project root.
3. Open `app.R` in RStudio.
4. Run the app by clicking the "Run App" button or executing:

```r
shiny::runApp()
