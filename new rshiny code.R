# ============================================================
# ASC ACTIVITY EXPLORER - R SHINY PROTOTYPE
# Purpose:
#   Interactive tool for exploring Adult Social Care Activity
#   Report CSV files.
#
# Reflects GOV.UK ASC Activity CSV structure:
#   Financial Year
#     -> Source CSV file
#       -> Measure
#         -> View
#           -> Geography
#             -> Breakdown variables
#               -> ITEMVALUE
#
# Main features:
#   - Reads all CSV files from the data folder
#   - Cleans suppressed or non-numeric values in ITEMVALUE
#   - Allows users to select one or more datasets at once
#   - Prioritises Measure and View because these are central
#     to the ASC Activity CSV structure
#   - Applies filters across selected datasets
#   - Allows flexible aggregation across multiple dimensions
#   - Produces charts, UK map and interactive tables
#   - Includes a documented data pipeline within the app
# ============================================================


# ============================================================
# 1. LOAD REQUIRED PACKAGES
# ============================================================

library(shiny)
library(data.table)
library(DT)
library(plotly)
library(ggplot2)
library(scales)
library(sf)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinyWidgets)
library(magrittr)
library(htmlwidgets)


# ============================================================
# 2. DATA PIPELINE FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# Clean ITEMVALUE
# ------------------------------------------------------------
# ASC files may contain suppressed values such as [c].
# These are converted to NA so they do not break calculations.
# Commas and non-numeric characters are also removed.
# ------------------------------------------------------------

clean_itemvalue <- function(x) {
  
  x <- as.character(x)
  
  x[x == "[c]"] <- NA
  
  x <- gsub(",", "", x)
  x <- gsub("[^0-9.-]", "", x)
  
  suppressWarnings(as.numeric(x))
}


# ------------------------------------------------------------
# Replace code 99 with Total
# ------------------------------------------------------------
# In many official datasets, coded category values such as 99
# can represent totals. This function replaces character value
# "99" with "Total" across character columns.
# ------------------------------------------------------------

replace_99_with_total <- function(dt) {
  
  dt <- dt[
    ,
    lapply(.SD, function(x) {
      
      if (is.character(x)) {
        x[x == "99"] <- "Total"
      }
      
      x
    })
  ]
  
  dt[]
}

# ------------------------------------------------------------
# Replace coded View values with clearer View labels
# ------------------------------------------------------------
# Some ASC Activity CSV files use coded values in the View column.
# This function changes those codes into clearer labels so that
# the Aggregated results table, filters, charts and key insights
# show meaningful text instead of raw codes.
# ------------------------------------------------------------

# ------------------------------------------------------------
# Replace coded View values with clearer View labels
# ------------------------------------------------------------
# This version is more robust because it handles differences in:
# - spaces
# - hyphens
# - brackets
# - View values such as "1", "1 - Clients", "1-Clients"
# - Measure values such as "LTS001A" or "LTS001A - some text"
# ------------------------------------------------------------

replace_view_codes <- function(dt) {
  
  if (!"SourceFile" %in% names(dt) || !"View" %in% names(dt)) {
    return(dt[])
  }
  
  dt[, SourceFile := trimws(as.character(SourceFile))]
  dt[, View := trimws(as.character(View))]
  
  if ("Measure" %in% names(dt)) {
    dt[, Measure := trimws(as.character(Measure))]
  }
  
  # Helper to standardise text for matching only
  make_key <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- tolower(x)
    x <- gsub("\\s+", "", x)
    x <- gsub("-", "", x)
    x <- gsub("_", "", x)
    x <- gsub("\\(", "", x)
    x <- gsub("\\)", "", x)
    x
  }
  
  dt[, SourceKey := make_key(SourceFile)]
  dt[, ViewKey := make_key(View)]
  
  if ("Measure" %in% names(dt)) {
    dt[, MeasureKey := toupper(trimws(as.character(Measure)))]
  }
  
  update_view_label <- function(source_file, measure_name = NULL, old_values, new_value) {
    
    source_key <- make_key(source_file)
    old_keys <- make_key(old_values)
    
    rows <- dt$SourceKey == source_key &
      dt$ViewKey %in% old_keys
    
    if (!is.null(measure_name) && "MeasureKey" %in% names(dt)) {
      rows <- rows & startsWith(dt$MeasureKey, toupper(measure_name))
    }
    
    dt[rows, View := new_value]
  }
  
  
  # ==========================================================
  # accommodation-and-employment-lts004-2025
  # ==========================================================
  
  update_view_label(
    source_file = "accomodation-and-employment-lts004-2025",
    old_values = c("1", "1.0 "),
    new_value = "1 - Clients with employment"
  )
  
  update_view_label(
    source_file = "accomodation-and-employment-lts004-2025",
    old_values = c("2", "2.0"),
    new_value = "2 - Clients with accommodation"
  )
  
  
  # ==========================================================
  # long-term-support-lts001-2025
  # ==========================================================
  
  update_view_label(
    source_file = "long-term-support-lts001-2025",
    measure_name = "LTS001A",
    old_values = c("1", "1 - Clients", "1-Clients"),
    new_value = "1 - Clients"
  )
  
  update_view_label(
    source_file = "long-term-support-lts001-2025",
    measure_name = "LTS001A",
    old_values = c("4", "4(Funding status)", "4 (Funding status)", "4 - Funding status"),
    new_value = "4 - Clients with funding status"
  )
  
  update_view_label(
    source_file = "long-term-support-lts001-2025",
    measure_name = "LTS001B",
    old_values = c("1", "1 - Clients", "1 - Clients"),
    new_value = "1 - Clients"
  )
  
  update_view_label(
    source_file = "long-term-support-lts001-2025",
    measure_name = "LTS001B",
    old_values = c("2", "2 - Clients", "2 - Clients"),
    new_value = "2 - Clients that has paid carer"
  )
  
  update_view_label(
    source_file = "long-term-support-lts001-2025",
    measure_name = "LTS001B",
    old_values = c("3", "3 - Clients", "3 - Clients"),
    new_value = "3 - Clients"
  )
  
  update_view_label(
    source_file = "long-term-support-lts001-2025",
    measure_name = "LTS001B",
    old_values = c("4", "4(Funding status)", "4 (Funding status)", "4 - Funding status"),
    new_value = "4 - Clients with funding status"
  )
  
  update_view_label(
    source_file = "long-term-support-lts001-2025",
    measure_name = "LTS001C",
    old_values = c("1", "1 - Clients", "1 - Clients"),
    new_value = "1 - Clients"
  )
  
  update_view_label(
    source_file = "long-term-support-lts001-2025",
    measure_name = "LTS001C",
    old_values = c("4", "4(Funding status)", "4 (Funding status)", "4 - Funding status"),
    new_value = "4 - Clients with funding status"
  )
  
  
  # ==========================================================
  # requests-sts001-2025
  # ==========================================================
  
  update_view_label(
    source_file = "requests-sts001-2025",
    old_values = c("1", "1- 18 to 64", "1 - 18 to 64"),
    new_value = "1 - Requests for 18 to 64 age band"
  )
  
  update_view_label(
    source_file = "requests-sts001-2025",
    old_values = c("2", "2-65 and over", "2 - 65 and over"),
    new_value = "2 - Requests for 65 and over age band"
  )
  
  update_view_label(
    source_file = "requests-sts001-2025",
    old_values = c("3", "3-unknown age", "3 - unknown age", "3 - Unknown age"),
    new_value = "3 - Requests with unknown age band"
  )
  
  
  # ==========================================================
  # reviews-lts002-2025
  # ==========================================================
  
  # LTS002A
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002A",
    old_values = c("1-Clients", "1 - Clients"),
    new_value = "1 - Clients receiving unplanned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002A",
    old_values = c("1-Reviews", "1 - Reviews"),
    new_value = "1 - Unplanned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002A",
    old_values = c("2-Clients", "2 - Clients"),
    new_value = "2 - Clients receiving planned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002A",
    old_values = c("2-Reviews", "2 - Reviews"),
    new_value = "2 - Planned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002A",
    old_values = c("3-Clients", "3 - Clients"),
    new_value = "3 - Clients receiving both planned and unplanned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002A",
    old_values = c("4-Clients", "4 - Clients"),
    new_value = "4 - Clients receiving reviews of unknown type"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002A",
    old_values = c("4-Reviews", "4 - Reviews"),
    new_value = "4 - Reviews of unknown type"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002A",
    old_values = c("5-Clients", "5 - Clients"),
    new_value = "5 - Clients receiving reviews of any type"
  )
  
  
  # LTS002B
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002B",
    old_values = c("1-Clients", "1 - Clients"),
    new_value = "1 - Clients receiving unplanned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002B",
    old_values = c("1-Reviews", "1 - Reviews"),
    new_value = "1 - Unplanned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002B",
    old_values = c("2-Clients", "2 - Clients"),
    new_value = "2 - Clients receiving planned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002B",
    old_values = c("2-Reviews", "2 - Reviews"),
    new_value = "2 - Planned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002B",
    old_values = c("3-Clients", "3 - Clients"),
    new_value = "3 - Clients receiving both planned and unplanned reviews"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002B",
    old_values = c("4-Clients", "4 - Clients"),
    new_value = "4 - Clients receiving reviews of unknown type"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002B",
    old_values = c("4-Reviews", "4 - Reviews"),
    new_value = "4 - Reviews of unknown type"
  )
  
  update_view_label(
    source_file = "reviews-lts002-2025",
    measure_name = "LTS002B",
    old_values = c("5-Clients", "5 - Clients"),
    new_value = "5 - Clients receiving reviews of any type"
  )
  
  
  # ==========================================================
  # st-max-sts002-2025
  # ==========================================================
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002A",
    old_values = c("1"),
    new_value = "1 - New clients ST-Max episodes"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002A",
    old_values = c("2"),
    new_value = "2 - New clients ST-Max episodes"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002A",
    old_values = c("3"),
    new_value = "3 - New clients ST-Max episodes"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002A",
    old_values = c("4"),
    new_value = "4 - New clients ST-Max episodes"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002A",
    old_values = c("5", "5(client numbers)", "5 (client numbers)", "5 - client numbers"),
    new_value = "5 - Clients"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002B",
    old_values = c("1"),
    new_value = "1 - Existing clients ST-Max episodes"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002B",
    old_values = c("2"),
    new_value = "2 - Existing clients ST-Max episodes"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002B",
    old_values = c("3"),
    new_value = "3 - Existing clients ST-Max episodes"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002B",
    old_values = c("4"),
    new_value = "4 - Existing clients ST-Max episodes"
  )
  
  update_view_label(
    source_file = "st-max-sts002-2025",
    measure_name = "STS002B",
    old_values = c("5", "5(client numbers)", "5 (client numbers)", "5 - client numbers"),
    new_value = "5 - Clients"
  )
  
  
  # Remove helper columns before returning data
  helper_cols <- intersect(
    c("SourceKey", "ViewKey", "MeasureKey"),
    names(dt)
  )
  
  if (length(helper_cols) > 0) {
    dt[, (helper_cols) := NULL]
  }
  
  dt[]
}
# ------------------------------------------------------------
# Load all ASC CSV files
# ------------------------------------------------------------

# ------------------------------------------------------------
# Create short dataset name
# Example:
# st-max-sts002-asc-activity-2024-to-2025
# becomes
# st-max-sts002-2025
# ------------------------------------------------------------

create_dataset_name <- function(filename) {
  
  file_name <- tools::file_path_sans_ext(
    basename(filename)
  )
  
  year_match <- regmatches(
    file_name,
    regexpr(
      "\\d{4}-to-\\d{4}",
      file_name
    )
  )
  
  if (length(year_match) > 0 && year_match != "") {
    
    end_year <- sub(
      ".*-to-(\\d{4})",
      "\\1",
      year_match
    )
    
  } else {
    
    end_year <- ""
  }
  
  measure_name <- sub(
    "-asc-activity-.*$",
    "",
    file_name
  )
  
  paste0(
    measure_name,
    "-",
    end_year
  )
}


load_asc_data <- function(data_folder = "data") {
  
  if (!dir.exists(data_folder)) {
    stop(
      "The data folder does not exist. Create a folder called 'data' beside app.R and put your CSV files in it."
    )
  }
  
  csv_files <- list.files(
    path = data_folder,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  if (length(csv_files) == 0) {
    stop(
      "No CSV files were found in the data folder. Please add your ASC Activity CSV files."
    )
  }
  
  data_list <- lapply(csv_files, function(file_path) {
    
    dt <- fread(
      file_path,
      fill = TRUE,
      na.strings = c("", "NA", "[c]"),
      showProgress = FALSE
    )
    
    dt[, SourceFile := create_dataset_name(file_path)]  
    
    dt[]
  })
  
  combined_data <- rbindlist(
    data_list,
    fill = TRUE,
    use.names = TRUE
  )
  
  if (!"ITEMVALUE" %in% names(combined_data)) {
    stop("The CSV files must contain a column called ITEMVALUE.")
  }
  
  combined_data[, ITEMVALUE_NUM := clean_itemvalue(ITEMVALUE)]
  
  combined_data <- replace_99_with_total(combined_data)
  
  combined_data <- replace_view_codes(combined_data)
  
  combined_data[]
}


# Load data once when the app starts
asc_data <- load_asc_data(
  app_sys("app", "data")
)
# ============================================================
# LOAD LOCAL AUTHORITY GEOJSON BOUNDARIES
# ============================================================
# The GeoJSON supplies polygon boundaries only.
# It is not included in any ASC activity-value calculation.
# ASC values are joined to these boundaries later using:
#
# GeographyCode in the ASC data
# CTYUA25CD in the GeoJSON
# ============================================================

map_boundary_file <- app_sys(
  "app",
  "data",
  "boundaries",
  "Local_Authority_Boundaries.geojson"
)

if (!file.exists(map_boundary_file)) {
  stop(
    paste(
      "The local-authority GeoJSON file was not found.",
      "Expected location:",
      map_boundary_file
    )
  )
}

local_authority_boundaries <- sf::st_read(
  map_boundary_file,
  quiet = TRUE,
  stringsAsFactors = FALSE
)

required_boundary_columns <- c(
  "CTYUA25CD",
  "CTYUA25NM",
  "geometry"
)

missing_boundary_columns <- setdiff(
  required_boundary_columns,
  names(local_authority_boundaries)
)

if (length(missing_boundary_columns) > 0) {
  stop(
    paste(
      "The local-authority GeoJSON is missing these required fields:",
      paste(
        missing_boundary_columns,
        collapse = ", "
      )
    )
  )
}

# Standardise the geography fields used for joining.
local_authority_boundaries$CTYUA25CD <- trimws(
  as.character(
    local_authority_boundaries$CTYUA25CD
  )
)

local_authority_boundaries$CTYUA25NM <- trimws(
  as.character(
    local_authority_boundaries$CTYUA25NM
  )
)

# Repair any invalid polygon geometries.
local_authority_boundaries <- sf::st_make_valid(
  local_authority_boundaries
)

# Leaflet requires longitude and latitude coordinates.
local_authority_boundaries <- sf::st_transform(
  local_authority_boundaries,
  crs = 4326
)

# Retain only records with a valid geography code.
local_authority_boundaries <- local_authority_boundaries[
  !is.na(local_authority_boundaries$CTYUA25CD) &
    local_authority_boundaries$CTYUA25CD != "",
]

# ============================================================
# 3. FILTER AND AGGREGATION SETUP
# ============================================================

# ------------------------------------------------------------
# Key ASC CSV fields
# ------------------------------------------------------------
# Measure and View are placed near the top because the ASC CSV
# guidance explains that the new files are organised around
# measures and views.
# ------------------------------------------------------------

possible_filter_columns <- c(
  "Measure",
  "View",
  "GeographyLevel",
  "GeographyName",
  "DHGeographyName",
  "AggregationLevel",
  "AgeBand",
  "Gender",
  "PrimarySupportReason",
  "ServiceType",
  "HasUnpaidCarer",
  "Ethnicity",
  "EthnicityGrouped",
  "DeliveryMechanism",
  "ClientFundingStatus",
  "RouteOfAccess",
  "SequelToRequestGrouped",
  "LongTermSupportSetting",
  "ShortTermCarePurpose",
  "NoFurtherActionOrEventType",
  "ClientTotals",
  "AccommodationStatusGroupAtHome",
  "AccommodationStatusGroupSupported"
)


# Default filter values used on startup and reset
default_filter_values <- list(
  AggregationLevel = "Detail",
  GeographyLevel = "England"
)


# Columns that should not normally be used for aggregation
columns_to_exclude_from_grouping <- c(
  "ITEMVALUE",
  "ITEMVALUE_NUM",
  "UID",
  "CASSRCode",
  "GeographyCode",
  "RegionGOCode"
)

max_extra_group_vars <- 3


# ============================================================
# 4. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML("
        #chart_type {
          display: flex;
          flex-wrap: wrap;
        }

        #chart_type .radio {
          width: 25%;
          margin-bottom: 8px;
        }

        .small-note {
          font-size: 13px;
          color: #666666;
          margin-top: -4px;
          margin-bottom: 10px;
        }

        .info-box {
          background: #F8F9FA;
          border-left: 4px solid #2C7FB8;
          padding: 12px;
          border-radius: 6px;
          margin-bottom: 15px;
        }

        .insight-box {
          background: #F8F9FA;
          border-left: 4px solid #2C7FB8;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 15px;
        }
      ")
    )
  ),
  
  titlePanel("Adult Social Care Metrics Explorer"),
  
  div(
    class = "info-box",
    p(
      "The Adult Social Care Metrics Explorer helps users analyse and understand Adult Social Care Activity Report data. Select one or more datasets, apply filters, explore activity across different measures and views, compare results by geography and demographic characteristics, and view findings through interactive charts, maps and summary tables. The tool supports flexible analysis while maintaining a clear connection to the structure of the published ASC Activity Report datasets")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      h4("1. Financial Year"),
      uiOutput("fy_filter"),
      
      hr(),
      
      h4("2. Select dataset(s)"),
      p(
        "Choose one or more ASC CSV files. Each selected file is treated as a SourceFile.",
        class = "small-note"
      ),
      
      selectizeInput(
        inputId = "selected_datasets",
        label = "Choose dataset(s)",
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        options = list(
          placeholder = "Select Financial Year first, then choose dataset(s)"
        )
      ),
      
      br(),
      
      fileInput(
        inputId = "upload_csv",
        label = "Upload additional CSV file(s)",
        multiple = TRUE,
        accept = ".csv",
        buttonLabel = "Browse..."
      ),
      
      hr(),
      
      
      h4("3. Choose aggregation"),
      p(
        "Choose the dimensions you want to summarise by. Every field selected under Apply filters is automatically added to this aggregation.",
        class = "small-note"
      ),      
      selectizeInput(
        inputId = "group_vars",
        label = "Aggregate by one or more variables",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Choose grouping variables"
        )
      ),
      actionButton(
        inputId = "reset_aggregation",
        label = "Reset Aggregation",
        icon = icon("rotate-left"),
        class = "btn-primary"
      ),
      
      h4("4. Apply filters"),
      p(
        "Measure and View are important because the CSV files are organised around these fields. Leaving a filter blank means no restriction is applied.",
        class = "small-note"
      ),
      
      uiOutput("dynamic_filters"),
      
      br(),
      
      actionButton(
        inputId = "reset_filters",
        label = "Reset Filters",
        icon = icon("rotate-left"),
        class = "btn-primary"
      ),
      
      hr(),
      
      
      radioButtons(
        inputId = "chart_type",
        label = "Chart type",
        choices = c(
          "Bar chart",
          "UK Map",
          "Stacked bar chart",
          "Line chart",
          "Pie chart",
          "Histogram",
          "Treemap"
        ),
        selected = "Bar chart",
        inline = TRUE
      ),
      
      numericInput(
        inputId = "top_n",
        label = "Number of categories to show in chart",
        value = 20,
        min = 5,
        max = 50,
        step = 5
      )
    ),
    
    mainPanel(
      
      width = 9,
      
      tabsetPanel(
        
        tabPanel(
          "Dashboard",
          br(),
          
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h4("Total activity"),
                textOutput("total_activity")
              )
            ),
            column(
              width = 4,
              wellPanel(
                h4("Rows after filters"),
                textOutput("row_count")
              )
            ),
            column(
              width = 4,
              wellPanel(
                h4("Selected dataset(s)"),
                textOutput("dataset_name")
              )
            )
          ),
          
          
          hr(),
          
          h3("Visualisation"),
          
          conditionalPanel(
            condition = "input.chart_type != 'UK Map'",
            plotlyOutput(
              "main_chart",
              height = "500px"
            ),
            
            tags$script(HTML("
Shiny.addCustomMessageHandler('capturePlot', function(message){

  Plotly.toImage(
      document.getElementById('main_chart'),
      {
        format:'png',
        width:1200,
        height:700
      }
  ).then(function(url){

      Shiny.setInputValue(
        'plot_image',
        url,
        {priority:'event'}
      );

  });

});
"))
          ),
          
          conditionalPanel(
            condition = "input.chart_type == 'UK Map'",
            leafletOutput(
              "uk_map",
              height = "500px"
            )
          ),
          
          hr(),
          
          fluidRow(
            
            column(
              width = 9,
              uiOutput("key_insights")
            ),
            
            column(
              width = 3,
              br(),
              br(),
              div(
                style = "text-align:right;",
                downloadButton(
                  "download_insights",
                  "Download Visual & Insights"
                )
              )
            )
            
          ),
          
          hr(),
          
          h3("Aggregated results"),
          
          p(
            "Use the table filters or select one or more rows below. The visualisation and key insights will update to reflect the visible or selected aggregated rows.",
            class = "small-note"
          ),
          
          DTOutput("summary_table")
        ),
        
        tabPanel(
          "Filtered data",
          br(),
          
          fluidRow(
            column(
              width = 8,
              h3("Filtered underlying data")
            ),
            column(
              width = 4,
              align = "right",
              br(),
              downloadButton(
                outputId = "download_filtered_data",
                label = "Download Filtered Data"
              )
            )
          ),
          
          DTOutput("filtered_table")
        ),
        
        tabPanel(
          "Data pipeline",
          br(),
          
          h3("Documented data pipeline"),
          
          div(
            class = "info-box",
            h4("How the GOV.UK CSV files are structured"),
            p(
              "The ASC Activity CSV files are long-format files. They are designed to be machine-readable and usable in spreadsheet or analytical tools."
            ),
            p(
              "The important structure is: Measure, View, Geography fields, breakdown fields and ITEMVALUE."
            ),
            p(
              "In the previous SALT publication, the metric list was shown using the column Sheet. In the new CSV files, this role is represented by Measure. The previous Table column is now represented by View."
            )
          ),
          
          h4("Step 1: Ingest CSV files"),
          p(
            "The app reads all CSV files stored in the local data folder using data.table::fread(). ",
            "New ASC Activity CSV files can be added to the folder without rewriting the code."
          ),
          
          h4("Step 2: Combine datasets"),
          p(
            "Each CSV file is imported and given a SourceFile column based on the file name. ",
            "All files are combined using rbindlist(fill = TRUE), which allows files with different column structures to work together."
          ),
          
          h4("Step 3: Clean activity values"),
          p(
            "The ITEMVALUE column is converted into a numeric column called ITEMVALUE_NUM. ",
            "Suppressed values such as [c], blanks and non-numeric text are converted to NA. ",
            "These missing values are ignored in summary calculations."
          ),
          
          h4("Step 4: Select Financial Year and SourceFile"),
          p(
            "The user first selects one or more financial years. ",
            "The dataset list then updates to show only SourceFile values available for the selected financial year."
          ),
          
          h4("Step 5: Apply Measure, View and breakdown filters"),
          p(
            "The app dynamically creates filters for columns that exist in the selected datasets. ",
            "Measure and View are shown as important filters because they describe the main reporting area and the specific cross-section of the data."
          ),
          
          h4("Step 6: Aggregate the long-format data"),
          p(
            "The filtered data is aggregated by the user-selected dimensions. ",
            "This allows users to explore totals by geography, age band, service type, support reason or other available breakdowns."
          ),
          
          h4("Step 7: Visualise and download"),
          p(
            "The app displays KPI totals, charts, a UK map outline, key insights, aggregated tables and the filtered underlying data. ",
            "The filtered data can also be downloaded as a CSV file."
          ),
          
          h4("Scalability design"),
          tags$ul(
            tags$li("Uses data.table for faster reading and processing of large CSV files."),
            tags$li("Allows users to select one or more datasets at once."),
            tags$li("Displays only filters that exist in the selected datasets."),
            tags$li("Prioritises Measure and View because they are central to the ASC CSV structure."),
            tags$li("Allows new CSV files to be uploaded while the app is running."),
            tags$li("Uses one flexible aggregation workflow rather than separate code for each table.")
          )
        )
      )
    )
  )
)


# ============================================================
# 5. SERVER LOGIC
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Store all available data
  # ----------------------------------------------------------
  
  all_data <- reactiveVal(asc_data)
  
  
  # ----------------------------------------------------------
  # Financial Year filter
  # ----------------------------------------------------------
  
  output$fy_filter <- renderUI({
    
    df <- all_data()
    
    if (!"FYEnding" %in% names(df)) {
      return(
        p("FYEnding column was not found in the data.")
      )
    }
    
    fy_values <- sort(
      unique(as.character(df$FYEnding))
    )
    
    fy_values <- fy_values[
      !is.na(fy_values) &
        fy_values != ""
    ]
    
    selectizeInput(
      inputId = "fy_ending",
      label = NULL,
      choices = fy_values,
      selected = character(0),
      multiple = TRUE,
      options = list(
        placeholder = "Select one or more Financial Years"
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # Dataset choices depend on selected Financial Year(s)
  # ----------------------------------------------------------
  
  datasets_available_for_selected_years <- reactive({
    
    df <- all_data()
    
    if (
      !"FYEnding" %in% names(df) ||
      !"SourceFile" %in% names(df)
    ) {
      return(character(0))
    }
    
    selected_years <- input$fy_ending
    
    if (
      is.null(selected_years) ||
      length(selected_years) == 0
    ) {
      return(character(0))
    }
    
    available_datasets <- df[
      as.character(FYEnding) %in% selected_years,
      unique(SourceFile)
    ]
    
    sort(available_datasets)
  })
  
  
  observeEvent(datasets_available_for_selected_years(), {
    
    available_datasets <- datasets_available_for_selected_years()
    
    current_selection <- input$selected_datasets
    
    valid_selection <- intersect(
      current_selection,
      available_datasets
    )
    
    updateSelectizeInput(
      session,
      inputId = "selected_datasets",
      choices = available_datasets,
      selected = valid_selection,
      server = TRUE
    )
    
  }, ignoreInit = FALSE)
  
  
  # ----------------------------------------------------------
  # Upload additional CSV files
  # ----------------------------------------------------------
  
  observeEvent(input$upload_csv, {
    
    req(input$upload_csv)
    
    
    uploaded_files <- input$upload_csv$datapath
    uploaded_names <- input$upload_csv$name
    
   
    new_data <- lapply(seq_along(uploaded_files), function(i) {
      
      dt <- fread(
        uploaded_files[i],
        fill = TRUE,
        na.strings = c("", "NA", "[c]"),
        showProgress = FALSE
      )
      
      dt[, SourceFile := create_dataset_name(uploaded_names[i])]
      
      if ("ITEMVALUE" %in% names(dt)) {
        dt[, ITEMVALUE_NUM := clean_itemvalue(ITEMVALUE)]
      }
      
      dt[]
    })
    
    combined_new <- rbindlist(
      new_data,
      fill = TRUE,
      use.names = TRUE
    )
    
    combined_new <- replace_99_with_total(combined_new)
    
    combined_new <- replace_view_codes(combined_new)
    
    updated_data <- rbindlist(
      list(all_data(), combined_new),
      fill = TRUE,
      use.names = TRUE
    )
    
    all_data(updated_data)
    
    available_datasets <- character(0)
    
    if (
      "FYEnding" %in% names(updated_data) &&
      !is.null(input$fy_ending) &&
      length(input$fy_ending) > 0
    ) {
      
      available_datasets <- updated_data[
        as.character(FYEnding) %in% input$fy_ending,
        unique(SourceFile)
      ]
      
      available_datasets <- sort(available_datasets)
    }
    
    current_selected <- input$selected_datasets
    
    valid_selection <- intersect(
      current_selected,
      available_datasets
    )
    
    updateSelectizeInput(
      session,
      inputId = "selected_datasets",
      choices = available_datasets,
      selected = valid_selection,
      server = TRUE
    )
  })
  
  
  # ----------------------------------------------------------
  # Selected data
  # ----------------------------------------------------------
  
  selected_data <- reactive({
    
    req(input$fy_ending)
    req(input$selected_datasets)
    
    validate(
      need(
        length(input$fy_ending) > 0,
        "Please select one or more Financial Years."
      )
    )
    
    validate(
      need(
        length(input$selected_datasets) > 0,
        "Please select one or more datasets."
      )
    )
    
    df <- all_data()
    
    df <- df[
      as.character(FYEnding) %in% input$fy_ending &
        SourceFile %in% input$selected_datasets
    ]
    
    validate(
      need(
        nrow(df) > 0,
        "No rows found for the selected Financial Year(s) and dataset(s)."
      )
    )
    
    df[]
  })
  
  
  # ----------------------------------------------------------
  # Create dynamic filter controls
  # ----------------------------------------------------------
  
  output$dynamic_filters <- renderUI({
    
    df <- selected_data()
    
    available_filter_columns <- intersect(
      possible_filter_columns,
      names(df)
    )
    
    filter_controls <- lapply(
      available_filter_columns,
      function(column_name) {
        
        values <- sort(
          unique(
            as.character(df[[column_name]])
          )
        )
        
        values <- values[
          !is.na(values) &
            values != ""
        ]
        
        if (length(values) == 0) {
          return(NULL)
        }
        
        default_value <- ""
        
        if (
          column_name %in% names(default_filter_values) &&
          default_filter_values[[column_name]] %in% values
        ) {
          default_value <- default_filter_values[[column_name]]
        }
        
        # Allow multiple selection only for geography name fields.
        # Other filters stay single-select to avoid users accidentally
        # over-filtering the data.
        allow_multiple <- column_name %in% c(
          "GeographyName",
          "DHGeographyName"
        )
        
        selectizeInput(
          inputId = paste0("filter__", column_name),
          label = column_name,
          choices = values,
          selected = if (allow_multiple) character(0) else default_value,
          multiple = allow_multiple,
          options = list(
            placeholder = paste(
              "Choose",
              column_name,
              "(optional)"
            ),
            allowEmptyOption = TRUE
          )
        )
      }
    )
    
    do.call(tagList, filter_controls)
  })
  
  # ----------------------------------------------------------
  # Restrict View choices based on selected Measure
  # ----------------------------------------------------------
  
  observe({
    
    req(selected_data())
    
    # Only proceed if both columns exist
    if (
      !"Measure" %in% names(selected_data()) ||
      !"View" %in% names(selected_data())
    ) {
      return()
    }
    
    df <- selected_data()
    
    selected_measure <- input$filter__Measure
    
    if (
      !is.null(selected_measure) &&
      length(selected_measure) > 0 &&
      selected_measure != ""
    ) {
      
      available_views <- sort(
        unique(
          as.character(
            df[
              Measure %in% selected_measure,
              View
            ]
          )
        )
      )
      
    } else {
      
      available_views <- sort(
        unique(
          as.character(df$View)
        )
      )
      
    }
    
    available_views <- available_views[
      !is.na(available_views) &
        available_views != ""
    ]
    
    current_view <- input$filter__View
    
    if (
      !is.null(current_view) &&
      length(current_view) > 0 &&
      !current_view %in% available_views
    ) {
      current_view <- ""
    }
    
    updateSelectizeInput(
      session,
      inputId = "filter__View",
      choices = available_views,
      selected = current_view
    )
    
  })
  
  # ----------------------------------------------------------
  # Apply default filters
  # ----------------------------------------------------------
  
  apply_default_filters <- function() {
    
    df <- selected_data()
    
    available_filter_columns <- intersect(
      possible_filter_columns,
      names(df)
    )
    
    for (column_name in available_filter_columns) {
      
      if (column_name %in% c("GeographyName", "DHGeographyName")) {
        selected_value <- character(0)
      } else {
        selected_value <- ""
      }
      
      if (
        column_name %in% names(default_filter_values) &&
        default_filter_values[[column_name]] %in%
        unique(as.character(df[[column_name]]))
      ) {
        selected_value <- default_filter_values[[column_name]]
      }
      
      updateSelectizeInput(
        session,
        inputId = paste0("filter__", column_name),
        selected = selected_value
      )
    }
  }
  
  
  observeEvent(input$selected_datasets, {
    
    req(input$selected_datasets)
    
    if (length(input$selected_datasets) > 0) {
      apply_default_filters()
    }
    
  }, ignoreInit = TRUE)
  
  
  # ----------------------------------------------------------
  # Reset filters
  # ----------------------------------------------------------
  
  observeEvent(input$reset_filters, {
    
    apply_default_filters()
    
  })
  
  # ----------------------------------------------------------
  # Reset aggregation
  # ----------------------------------------------------------
  
  observeEvent(input$reset_aggregation, {
    
    df <- filtered_data()
    
    available_group_columns <- setdiff(
      names(df),
      columns_to_exclude_from_grouping
    )
    
    default_group_vars <- intersect(
      c(
        "Measure",
        "View",
        "GeographyLevel",
        "AggregationLevel"
      ),
      available_group_columns
    )
    
    updateSelectizeInput(
      session,
      inputId = "group_vars",
      selected = default_group_vars
    )
    
  })
  
  observeEvent(input$group_vars, {
    
    default_vars <- intersect(
      c(
        "Measure",
        "View",
        "GeographyLevel",
        "AggregationLevel"
      ),
      input$group_vars
    )
    
    extra_vars <- setdiff(
      input$group_vars,
      default_vars
    )
    
    if (length(extra_vars) > 3) {
      
      showNotification(
        "You can select a maximum of 3 additional aggregation variables.",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "group_vars",
        selected = c(
          default_vars,
          extra_vars[1:3]
        )
      )
    }
    
  }, ignoreInit = TRUE) 
  
  # ----------------------------------------------------------
  # Identify actively selected filters
  # ----------------------------------------------------------
  # This helps keep the aggregation choices sensible.
  # Example:
  #   If GeographyLevel is already fixed to England,
  #   there is no need to aggregate by GeographyLevel.
  # ----------------------------------------------------------
  
  selected_filter_columns <- reactive({
    
    df <- selected_data()
    
    available_filter_columns <- intersect(
      possible_filter_columns,
      names(df)
    )
    
    active_filters <- character(0)
    
    for (column_name in available_filter_columns) {
      
      input_id <- paste0("filter__", column_name)
      selected_values <- input[[input_id]]
      
      if (
        !is.null(selected_values) &&
        length(selected_values) > 0 &&
        any(selected_values != "")
      ) {
        active_filters <- c(active_filters, column_name)
      }
    }
    
    active_filters
  })
  
  
  # ----------------------------------------------------------
  # Apply filters across selected datasets
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    df <- copy(selected_data())
    
    available_filter_columns <- intersect(
      possible_filter_columns,
      names(df)
    )
    
    for (column_name in available_filter_columns) {
      
      input_id <- paste0("filter__", column_name)
      selected_values <- input[[input_id]]
      
      # Only filter when the user has selected something.
      # If the filter is blank, leave the data unchanged.
      if (
        !is.null(selected_values) &&
        length(selected_values) > 0 &&
        any(selected_values != "")
      ) {
        
        df <- df[
          as.character(get(column_name)) %in% selected_values
        ]
      }
    }
    
    df[]
  })
  
  
  # ----------------------------------------------------------
  # Update aggregation choices
  # ----------------------------------------------------------
  # This version updates the aggregation choices using the data
  # after the current Apply filters have been applied.
  #
  # This means:
  # - if a View is selected,
  # - and some columns have no data for that View,
  # - those columns will not appear in Choose aggregation.
  #
  # It keeps the existing behaviour:
  # - existing aggregation selections are preserved where valid
  # - default aggregation choices are still used
  # - active Apply filter columns are still added to aggregation
  # - technical columns are still excluded
  # ----------------------------------------------------------
  
  observe({
    
    # Use data after Apply filters, not just selected_data().
    # This is the key change.
    df <- filtered_data()
    
    validate(
      need(
        nrow(df) > 0,
        "No data available for aggregation choices after applying the selected filters."
      )
    )
    
    # Get columns currently selected in Apply filters
    active_filters <- selected_filter_columns()
    
    # Start with all possible grouping columns,
    # but remove technical columns that should not be grouped.
    available_group_columns <- setdiff(
      names(df),
      columns_to_exclude_from_grouping
    )
    
    # Optional:
    # Remove SourceFile when more than one dataset is selected.
    # This keeps your existing behaviour.
    if (
      !is.null(input$selected_datasets) &&
      length(input$selected_datasets) > 1
    ) {
      
      available_group_columns <- setdiff(
        available_group_columns,
        "SourceFile"
      )
    }
    
    # --------------------------------------------------------
    # Keep only columns that actually contain usable data
    # in the currently filtered data.
    #
    # This is what removes columns such as:
    # AccommodationStatusGroupAtHome
    # AccommodationStatusGroupSupported
    # when the selected View has no data for them.
    # --------------------------------------------------------
    
    column_has_usable_data <- function(x) {
      
      x <- as.character(x)
      
      x <- trimws(x)
      
      x <- x[
        !is.na(x) &
          x != "" &
          x != "NA" &
          x != "[c]"
      ]
      
      length(x) > 0
    }
    
    if (length(available_group_columns) > 0) {
      
      columns_with_data <- sapply(
        df[, ..available_group_columns],
        column_has_usable_data
      )
      
      available_group_columns <- available_group_columns[
        columns_with_data
      ]
    }
    
    # --------------------------------------------------------
    # Keep only columns that are useful for grouping.
    #
    # A useful column is one with more than one non-empty value.
    # However, important ASC fields are kept if they have data,
    # even if they currently have only one value.
    # --------------------------------------------------------
    
    if (length(available_group_columns) > 0) {
      
      useful_columns <- sapply(
        df[, ..available_group_columns],
        function(x) {
          
          x <- as.character(x)
          x <- trimws(x)
          
          x <- x[
            !is.na(x) &
              x != "" &
              x != "NA" &
              x != "[c]"
          ]
          
          length(unique(x)) > 1
        }
      )
      
      # Important ASC fields and actively selected filters must remain
      # available for aggregation, even when filtering leaves only one value.
      #
      # For example, if AgeBand = "18 to 64" is selected under Apply filters,
      # AgeBand must still appear in Choose aggregation even though the filtered
      # data now contains only that one AgeBand value.
      always_keep <- unique(
        c(
          "Measure",
          "View",
          "GeographyLevel",
          "AggregationLevel",
          active_filters
        )
      )
      
      always_keep_available <- intersect(
        always_keep,
        available_group_columns
      )
      
      available_group_columns <- unique(
        c(
          available_group_columns[useful_columns],
          always_keep_available
        )
      )
      
    }
    
    # Normal default aggregation variables.
    # This keeps your existing default setup.
    default_group_vars <- intersect(
      c(
        "Measure",
        "View",
        "GeographyLevel",
        "AggregationLevel"
      ),
      available_group_columns
    )
    
    # Keep the aggregation variables the user already selected,
    # but only if they are still valid for the selected View/filter.
    current_group_vars <- isolate(input$group_vars)
    
    valid_current_group_vars <- intersect(
      current_group_vars,
      available_group_columns
    )
    
    # Add active Apply filter columns only if they are valid
    # and have usable data for the current selected View/filter.
    active_filters_to_add <- intersect(
      active_filters,
      available_group_columns
    )
    
    # If the user already selected aggregation variables,
    # keep them.
    # Otherwise, use the default aggregation variables.
    base_group_vars <- if (
      !is.null(valid_current_group_vars) &&
      length(valid_current_group_vars) > 0
    ) {
      valid_current_group_vars
    } else {
      default_group_vars
    }
    
    # Add selected Apply filter columns to the existing aggregation.
    selected_group_vars <- unique(
      c(
        base_group_vars,
        active_filters_to_add
      )
    )
    
    default_group_vars <- intersect(
      c(
        "Measure",
        "View",
        "GeographyLevel",
        "AggregationLevel"
      ),
      available_group_columns
    )
    
    extra_selected <- setdiff(
      selected_group_vars,
      default_group_vars
    )
    
    if (length(extra_selected) > max_extra_group_vars) {
      
      extra_selected <- extra_selected[
        seq_len(max_extra_group_vars)
      ]
      
      selected_group_vars <- c(
        default_group_vars,
        extra_selected
      )
    }
    
    updateSelectizeInput(
      session,
      inputId = "group_vars",
      choices = sort(available_group_columns),
      selected = selected_group_vars,
      server = TRUE
    )
  })
  
  # ----------------------------------------------------------
  # Aggregate data
  # ----------------------------------------------------------
  
  summary_data <- reactive({
    
    df <- filtered_data()
    
    validate(
      need(
        nrow(df) > 0,
        "No data available after applying the selected filters."
      )
    )
    
    validate(
      need(
        "ITEMVALUE_NUM" %in% names(df),
        "ITEMVALUE_NUM column is missing. Check that ITEMVALUE exists in the selected CSV files."
      )
    )
    
    group_vars <- input$group_vars
    
    if (is.null(group_vars) || length(group_vars) == 0) {
      
      result <- df[
        ,
        .(
          Total = sum(ITEMVALUE_NUM, na.rm = TRUE),
          Records = .N
        )
      ]
      
      result[, Group := "All selected data"]
      
      setcolorder(
        result,
        c("Group", "Total", "Records")
      )
      
      return(result[])
    }
    
    result <- df[
      ,
      .(
        Total = sum(ITEMVALUE_NUM, na.rm = TRUE),
        Records = .N
      ),
      by = group_vars
    ]
    
    setorder(result, -Total)
    
    result[]
  })
  
  
  
  # ----------------------------------------------------------
  # Aggregated data used by visualisations
  # ----------------------------------------------------------
  # This makes the charts respond to the Aggregated results table.
  #
  # Logic:
  # 1. If the user selects rows in the Aggregated results table,
  #    the chart uses only those selected rows.
  # 2. If the user filters/searches the Aggregated results table,
  #    the chart uses the visible filtered rows.
  # 3. If the user does nothing in the table,
  #    the chart uses the full aggregated result.
  # ----------------------------------------------------------
  
  summary_data_for_visuals <- reactive({
    
    result <- summary_data()
    
    selected_rows <- input$summary_table_rows_selected
    visible_rows <- input$summary_table_rows_all
    
    if (
      !is.null(selected_rows) &&
      length(selected_rows) > 0
    ) {
      
      result <- result[selected_rows]
      
    } else if (
      !is.null(visible_rows)
    ) {
      
      result <- result[visible_rows]
    }
    
    validate(
      need(
        nrow(result) > 0,
        "No aggregated rows are currently selected or visible."
      )
    )
    
    result[]
  })
  
  
  # ----------------------------------------------------------
  # Raw filtered data used by visuals that need underlying rows
  # ----------------------------------------------------------
  # Some charts, such as Histogram, use the raw filtered data
  # rather than the aggregated table directly.
  #
  # This section keeps those visuals consistent with any selected
  # rows in the Aggregated results table.
  # ----------------------------------------------------------
  
  filtered_data_for_visuals <- reactive({
    
    raw_data <- copy(filtered_data())
    selected_summary <- summary_data_for_visuals()
    
    group_vars <- input$group_vars
    
    if (
      is.null(group_vars) ||
      length(group_vars) == 0
    ) {
      return(raw_data[])
    }
    
    join_cols <- intersect(
      group_vars,
      names(raw_data)
    )
    
    join_cols <- intersect(
      join_cols,
      names(selected_summary)
    )
    
    if (length(join_cols) == 0) {
      return(raw_data[])
    }
    
    keys <- unique(
      selected_summary[, ..join_cols]
    )
    
    # Convert join columns to character on both sides.
    # This avoids matching problems caused by mixed column types.
    for (column_name in join_cols) {
      
      raw_data[
        ,
        (column_name) := as.character(get(column_name))
      ]
      
      keys[
        ,
        (column_name) := as.character(get(column_name))
      ]
    }
    
    raw_data <- raw_data[
      keys,
      on = join_cols,
      nomatch = 0
    ]
    
    raw_data[]
  })
  
  
  
  # ----------------------------------------------------------
  # Key insights
  # ----------------------------------------------------------
  # This version creates detailed plain-English insights for
  # every row generated in the Aggregated results table.
  #
  # It uses:
  # - Measure to identify the ASC activity area
  # - View to identify whether the value is clients, reviews,
  #   requests or ST-Max episodes
  # - GeographyName and DHGeographyName where present
  # - all other aggregated columns to describe the row
  #
  # The output responds to:
  # - Apply filters
  # - Choose aggregation
  # - Aggregated table search/filter
  # - Aggregated table row selection
  # ----------------------------------------------------------
  
  get_measure_code <- function(measure_value) {
    
    measure_value <- toupper(trimws(as.character(measure_value)))
    
    # Keep the first code-like part only.
    # Examples:
    # "LTS001A - Some label" becomes "LTS001A"
    # "LTS001A" stays "LTS001A"
    measure_code <- sub("\\s.*$", "", measure_value)
    measure_code <- sub("-.*$", "", measure_code)
    
    measure_code
  }
  
  
  get_measure_description <- function(measure_value) {
    
    measure_code <- get_measure_code(measure_value)
    
    descriptions <- c(
      LTS004 = "accommodation and employment status of people receiving long-term support",
      
      LTS001A = "people aged 18 or over who accessed long-term support at any time during the year",
      
      LTS001B = "people aged 18 or over who accessed long-term support at the end of the year",
      
      LTS001C = "people aged 18 or over who accessed long-term support continuously during the 12-month of the year",
      
      LTS002A = "reviews for people who received long-term support at some point during the year",
      
      LTS002B = "reviews for people who received long-term support consistently throughout the year",
      
      STS001 = "requests for support received from new clients, broken down by the different sequels to that request",
      
      STS002A = "episodes of short-term support to maximise independence provided to people aged 18 or over who had not received long-term support in the previous 3 months",
      
      STS002B = "episodes of short-term support to maximise independence provided to people aged 18 or over who were receiving long-term support at the start of the ST-Max episode or had received long-term support in the previous 3 months"
    )
    
    if (measure_code %in% names(descriptions)) {
      return(descriptions[[measure_code]])
    }
    
    if (!is.na(measure_value) && measure_value != "") {
      return(as.character(measure_value))
    }
    
    "ASC activity"
  }
  
  
  get_activity_unit <- function(measure_value, view_value) {
    
    measure_code <- get_measure_code(measure_value)
    view_value <- trimws(as.character(view_value))
    view_lower <- tolower(view_value)
    
    # -------------------------------
    # LTS004
    # Accommodation and employment
    # -------------------------------
    if (measure_code == "LTS004") {
      return("clients")
    }
    
    # -------------------------------
    # LTS001A, LTS001B, LTS001C
    # Long-term support client counts
    # -------------------------------
    if (
      measure_code %in% c(
        "LTS001A",
        "LTS001B",
        "LTS001C"
      )
    ) {
      return("clients")
    }
    
    # -------------------------------
    # LTS002A and LTS002B
    # Reviews and/or clients
    # -------------------------------
    if (
      measure_code %in% c(
        "LTS002A",
        "LTS002B"
      )
    ) {
      
      if (
        grepl("client", view_lower)
      ) {
        return("clients")
      }
      
      if (
        grepl("review", view_lower)
      ) {
        return("reviews")
      }
      
      return("reviews")
    }
    
    # -------------------------------
    # STS001
    # Requests
    # -------------------------------
    if (measure_code == "STS001") {
      return("requests")
    }
    
    # -------------------------------
    # STS002A and STS002B
    # ST-Max episodes or clients
    # -------------------------------
    if (
      measure_code %in% c(
        "STS002A",
        "STS002B"
        
      )
    ) {
      
      if (
        identical(trimws(view_value), "5 - Clients")
      ) {
        return("clients")
      }
      
      return("ST-Max episodes")
    }
    
    "activity"
  }
  
  
  get_row_activity_unit <- function(row_data) {
    
    if (
      "Measure" %in% names(row_data) &&
      "View" %in% names(row_data)
    ) {
      return(
        get_activity_unit(
          row_data$Measure[1],
          row_data$View[1]
        )
      )
    }
    
    df <- filtered_data()
    
    if (
      "Measure" %in% names(df) &&
      "View" %in% names(df)
    ) {
      
      unique_measures <- unique(
        na.omit(
          as.character(df$Measure)
        )
      )
      
      unique_views <- unique(
        na.omit(
          as.character(df$View)
        )
      )
      
      if (
        length(unique_measures) == 1 &&
        length(unique_views) == 1
      ) {
        return(
          get_activity_unit(
            unique_measures[1],
            unique_views[1]
          )
        )
      }
    }
    
    "activity"
  }
  
  
  get_row_measure_description <- function(row_data) {
    
    if ("Measure" %in% names(row_data)) {
      return(
        get_measure_description(
          row_data$Measure[1]
        )
      )
    }
    
    df <- filtered_data()
    
    if ("Measure" %in% names(df)) {
      
      unique_measures <- unique(
        na.omit(
          as.character(df$Measure)
        )
      )
      
      if (length(unique_measures) == 1) {
        return(
          get_measure_description(
            unique_measures[1]
          )
        )
      }
    }
    
    "ASC activity"
  }
  
  
  get_row_view_text <- function(row_data) {
    
    if ("View" %in% names(row_data)) {
      
      view_value <- as.character(row_data$View[1])
      
      if (
        !is.na(view_value) &&
        trimws(view_value) != ""
      ) {
        return(view_value)
      }
    }
    
    df <- filtered_data()
    
    if ("View" %in% names(df)) {
      
      unique_views <- unique(
        na.omit(
          as.character(df$View)
        )
      )
      
      if (length(unique_views) == 1) {
        return(unique_views[1])
      }
    }
    
    ""
  }
  
  
  get_geo_text <- function(row_data) {
    
    geography_parts <- character(0)
    
    if ("GeographyName" %in% names(row_data)) {
      
      geography_name <- as.character(row_data$GeographyName[1])
      
      if (
        !is.na(geography_name) &&
        trimws(geography_name) != ""
      ) {
        geography_parts <- c(
          geography_parts,
          geography_name
        )
      }
    }
    
    if ("DHGeographyName" %in% names(row_data)) {
      
      dh_geography_name <- as.character(row_data$DHGeographyName[1])
      
      if (
        !is.na(dh_geography_name) &&
        trimws(dh_geography_name) != "" &&
        !dh_geography_name %in% geography_parts
      ) {
        geography_parts <- c(
          geography_parts,
          dh_geography_name
        )
      }
    }
    
    if (length(geography_parts) > 0) {
      return(
        paste(
          geography_parts,
          collapse = " / "
        )
      )
    }
    
    df <- filtered_data()
    
    if ("GeographyName" %in% names(df)) {
      
      unique_geo <- unique(
        na.omit(
          as.character(df$GeographyName)
        )
      )
      
      unique_geo <- unique_geo[
        trimws(unique_geo) != ""
      ]
      
      if (length(unique_geo) == 1) {
        return(unique_geo[1])
      }
    }
    
    if ("DHGeographyName" %in% names(df)) {
      
      unique_dh_geo <- unique(
        na.omit(
          as.character(df$DHGeographyName)
        )
      )
      
      unique_dh_geo <- unique_dh_geo[
        trimws(unique_dh_geo) != ""
      ]
      
      if (length(unique_dh_geo) == 1) {
        return(unique_dh_geo[1])
      }
    }
    
    "the selected geography"
  }
  
  
  format_detail_label <- function(column_name) {
    
    labels <- c(
      Measure = "measure",
      View = "view",
      GeographyLevel = "geography level",
      GeographyName = "geography",
      DHGeographyName = "DH geography",
      AggregationLevel = "aggregation level",
      AgeBand = "age band",
      Gender = "gender",
      PrimarySupportReason = "primary support reason",
      ServiceType = "service type",
      HasUnpaidCarer = "unpaid carer status",
      Ethnicity = "ethnicity",
      EthnicityGrouped = "ethnicity group",
      DeliveryMechanism = "delivery mechanism",
      ClientFundingStatus = "client funding status",
      RouteOfAccess = "route of access",
      SequelToRequestGrouped = "sequel to request",
      LongTermSupportSetting = "long-term support setting",
      ShortTermCarePurpose = "short-term care purpose",
      NoFurtherActionOrEventType = "no further action or event type",
      ClientTotals = "client total category",
      AccommodationStatusGroupAtHome = "accommodation status at home",
      AccommodationStatusGroupSupported = "supported accommodation status",
      EmploymentStatus = "employment status"
    )
    
    if (column_name %in% names(labels)) {
      return(labels[[column_name]])
    }
    
    # Fallback for other columns not listed above.
    gsub(
      "([a-z])([A-Z])",
      "\\1 \\2",
      column_name
    )
  }
  
  
  build_detail_parts <- function(row_data) {
    
    exclude_columns <- c(
      "Total",
      "Records",
      "Measure",
      "View",
      "Gender",                  
      
      # add this
      "GeographyLevel",
      "GeographyName",
      "DHGeographyName",
      "AggregationLevel",
      "Group",
      "GroupCombined",
      "InsightMeasureDescription"
    )    
    detail_columns <- setdiff(
      names(row_data),
      exclude_columns
    )
    
    detail_parts <- character(0)
    
    for (column_name in detail_columns) {
      
      value <- as.character(row_data[[column_name]][1])
      
      if (
        is.na(value) ||
        trimws(value) == "" ||
        value == "NA" ||
        value == "[c]"
      ) {
        next
      }
      
      text <- switch(
        
        column_name,
        
        AgeBand =
          paste0("aged ", value),
        
        Gender =
          paste0("for ", value),
        
        Ethnicity =
          paste0("for people of ", value, " ethnicity"),
        
        EthnicityGrouped =
          paste0("for people in the ", value, " ethnicity group"),
        
        PrimarySupportReason =
          paste0("whose primary support reason is ", value),
        
        ServiceType =
          paste0("receiving ", value),
        
        ClientFundingStatus =
          paste0("with funding status ", value),
        
        HasUnpaidCarer =
          paste0("with unpaid carer status ", value),
        
        DeliveryMechanism =
          paste0("through ", value),
        
        RouteOfAccess =
          paste0("accessed via ", value),
        
        LongTermSupportSetting =
          paste0("in ", value),
        
        ShortTermCarePurpose =
          paste0("for ", value),
        
        SequelToRequestGrouped =
          paste0("resulting in ", value),
        
        AccommodationStatusGroupAtHome =
          paste0("living ", value),
        
        AccommodationStatusGroupSupported =
          paste0("with supported accommodation status ", value),
        
        EmploymentStatus =
          if (
            tolower(trimws(value)) == "unknown"
          ) {
            " whose employment status is unknown"
          } else {
            paste0("who are ", value)
          },
        
        paste0(
          tolower(format_detail_label(column_name)),
          " ",
          value
        )
      )
      
      detail_parts <- c(detail_parts, text)
    }
    
    detail_parts
  }  
  
  combine_detail_parts <- function(detail_parts) {
    
    if (length(detail_parts) == 0) {
      return("")
    }
    
    if (length(detail_parts) == 1) {
      return(detail_parts[1])
    }
    
    paste(
      paste(
        detail_parts[-length(detail_parts)],
        collapse = ", "
      ),
      "and",
      detail_parts[length(detail_parts)]
    )
  }
  
  
  build_detailed_insight_sentence <- function(row_data) {
    
    activity_unit <- get_row_activity_unit(row_data)
    geography_text <- get_geo_text(row_data)
    view_text <- get_row_view_text(row_data)
    
    total_value <- row_data$Total[1]
    
    gender_text <- ""
    
    if (
      "Gender" %in% names(row_data) &&
      !is.na(row_data$Gender[1]) &&
      trimws(as.character(row_data$Gender[1])) != ""
    ) {
      gender_text <- paste0(
        tolower(as.character(row_data$Gender[1])),
        " "
      )
    }
    
    detail_parts <- build_detail_parts(row_data)
    detail_text <- combine_detail_parts(detail_parts)
    
    # Start directly with the geography.
    # The measure description will be shown once as a heading in the Key Insights section.
    base_sentence <- paste0(
      "In ",
      geography_text,
      ", there are ",
      scales::comma(total_value),
      " ",
      gender_text,
      activity_unit
    )    
    if (
      !is.na(view_text) &&
      trimws(view_text) != ""
    ) {
      
      view_phrase_lookup <- c(
        
        # LTS004
        "1 - Client employment" = "in employment",
        "2 - Client accommodation" = "by accommodation status",
        
        # LTS001A
        "1 - Clients" = "",
        "4 - Clients with funding status" = "with funding status",
        
        # LTS001B
        "1 - Clients" = "",
        "2 - Clients that has paid carer" = "with a paid carer",
        "3 - Clients" = "",
        "4 - Clients with funding status" = "with funding status",
        
        # LTS001C
        "4 - Clients with funding status" = "with funding status",
        
        # STS001
        "1 - Requests for 18 to 64 age band" = "from people aged 18 to 64",
        "2 - Requests for 65 and over age band" = "from people aged 65 and over",
        "3 - Requests with unknown age band" = "with unknown age band",
        
        # LTS002A / LTS002B
        "1 - Unplanned reviews" = "that were unplanned",
        "1 - Clients receiving unplanned reviews" = "recieving unplanned reviews",
        
        "2 - Planned reviews" = "that were planned",
        "2 - Clients receiving planned reviews" = "recieving planned reviews",
        
        "3 - Clients receiving both planned and unplanned reviews" =
          "recieving both planned and unplanned reviews",
        
        "4 - Reviews of unknown type" =
          "of unknown review type",
        
        "4 - Clients recieving reviews of unknown type" =
          "recieving reviews of unknown type",
        
        "5 - Clients receiving reviews of any type" =
          "recieving reviews of any type",
        
        # STS002A
        "1 - New clients ST-Max episodes" =
          "for new clients",
        "2 - New clients ST-Max episodes" =
          "for new clients",
        "3 - New clients ST-Max episodes" =
          "for new clients",
        "4 - New clients ST-Max episodes" =
          "for new clients",
        
        # STS002B
        "1 - Existing clients ST-Max episodes" =
          "for existing clients",
        "2 - Existing clients ST-Max episodes" =
          "for existing clients",
        "3 - Existing clients ST-Max episodes" =
          "for existing clients",
        "4 - Existing clients ST-Max episodes" =
          "for existing clients",
        
        # STS002A and STS002B
        "5 - Clients" = ""
      )
      
      if (view_text %in% names(view_phrase_lookup)) {
        
        view_phrase <- view_phrase_lookup[[view_text]]
        
        # Remove "in employment" when EmploymentStatus is Unknown
        if (
          view_text == "1 - Client employment" &&
          "EmploymentStatus" %in% names(row_data) &&
          !is.na(row_data$EmploymentStatus[1]) &&
          trimws(as.character(row_data$EmploymentStatus[1])) == "Unknown"
        ) {
          view_phrase <- ""
        }
        
        if (view_phrase != "") {
          base_sentence <- paste(
            base_sentence,
            view_phrase
          )
        }
        
      }  else {
        
        base_sentence <- paste(
          base_sentence,
          "for",
          tolower(view_text)
        )
        
      }
    }
    
    # Do not include Records here.
    # This removes:
    # "across 124 underlying record(s)"
    
    if (detail_text != "") {
      base_sentence <- paste0(
        base_sentence,
        " ",
        detail_text
      )
    }
    
    paste0(
      base_sentence,
      "."
    )
  }
  
  
  detailed_insight_groups <- reactive({
    
    result <- summary_data_for_visuals()
    
    validate(
      need(
        nrow(result) > 0,
        "No insights available."
      )
    )
    
    # Add one measure description per row.
    # This allows the insights to be grouped by measure, so the app does not
    # repeat "For accommodation and employment..." for every row.
    result[, InsightMeasureDescription := vapply(
      seq_len(.N),
      function(i) {
        get_row_measure_description(result[i])
      },
      character(1)
    )]
    
    measure_groups <- split(
      result,
      result$InsightMeasureDescription
    )
    
    insight_groups <- lapply(
      names(measure_groups),
      function(measure_description) {
        
        group_data <- measure_groups[[measure_description]]
        
        sentences <- character(0)
        
        for (i in seq_len(nrow(group_data))) {
          
          row_data <- group_data[i]
          
          sentences <- c(
            sentences,
            build_detailed_insight_sentence(row_data)
          )
        }
        
        list(
          measure_description = measure_description,
          sentences = sentences
        )
      }
    )
    
    insight_groups
  })
  
  
  insight_text <- reactive({
    
    groups <- detailed_insight_groups()
    
    text_blocks <- lapply(
      groups,
      function(group) {
        
        paste0(
          group$measure_description,
          "\n",
          paste(
            paste0("- ", group$sentences),
            collapse = "\n"
          )
        )
      }
    )
    
    paste(
      text_blocks,
      collapse = "\n\n"
    )
  })
  
  insight_rows_to_show <- reactiveVal(15)
  
  observeEvent(input$show_more_insights, {
    
    insight_rows_to_show(
      insight_rows_to_show() + 15
    )
    
  })
  
  observeEvent(
    summary_data_for_visuals(),
    {
      insight_rows_to_show(15)
    },
    ignoreInit = TRUE
  )
  
  output$key_insights <- renderUI({
    
    groups <- detailed_insight_groups()
    
    tags$div(
      class = "insight-box",
      tags$h4("Detailed Key Insights"),
      lapply(
        groups,
        function(group) {
          
          tags$div(
            tags$p(
              tags$b(
                paste0(
                  "For ",
                  tolower(group$measure_description),
                  ":"
                )
              )
            ),
            {
              # Extract common geography from sentences
              first_sentence <- group$sentences[1]
              
              geo_text <- sub(
                "^In ([^,]+),.*$",
                "\\1",
                first_sentence
              )
              
              # Remove "In Geography," from every sentence
              cleaned_sentences <- gsub(
                paste0("^In ", geo_text, ",\\s*"),
                "",
                group$sentences
              )
              
              tagList(
                tags$p(
                  paste0("In ", geo_text, ",")
                ),
                {
                  visible_sentences <- head(
                    cleaned_sentences,
                    insight_rows_to_show()
                  )
                  
                  tagList(
                    
                    tags$ol(
                      lapply(
                        visible_sentences,
                        tags$li
                      )
                    ),
                    
                    if (
                      length(cleaned_sentences) >
                      insight_rows_to_show()
                    ) {
                      
                      actionLink(
                        inputId = "show_more_insights",
                        label = paste0(
                          "...more (",
                          length(cleaned_sentences) -
                            insight_rows_to_show(),
                          " more)"
                        )
                      )
                      
                    }
                    
                  )
                }
              )
            }
          )
        }
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # KPI outputs
  # ----------------------------------------------------------
  
  output$total_activity <- renderText({
    
    total_value <- sum(
      filtered_data()$ITEMVALUE_NUM,
      na.rm = TRUE
    )
    
    comma(total_value)
  })
  
  
  output$row_count <- renderText({
    
    comma(nrow(filtered_data()))
  })
  
  
  output$dataset_name <- renderText({
    
    req(input$selected_datasets)
    
    paste(input$selected_datasets, collapse = ", ")
  })
  
  # ----------------------------------------------------------
  # Create a visualisation label from the selected View
  # ----------------------------------------------------------
  
  get_visual_activity_label <- function(result) {
    
    view_values <- character(0)
    
    # First look in the aggregated results
    if ("View" %in% names(result)) {
      
      view_values <- unique(
        trimws(as.character(result$View))
      )
      
    } else {
      
      # Otherwise use the filtered data
      df <- filtered_data()
      
      if ("View" %in% names(df)) {
        
        view_values <- unique(
          trimws(as.character(df$View))
        )
        
      }
    }
    
    # Remove blanks and missing values
    view_values <- view_values[
      !is.na(view_values) &
        view_values != ""
    ]
    
    # If only one View is present, use it
    if (length(view_values) == 1) {
      
      visual_label <- sub(
        "^\\s*[0-9]+\\s*-\\s*",
        "",
        view_values[1]
      )
      
      if (
        !is.na(visual_label) &&
        trimws(visual_label) != ""
      ) {
        return(visual_label)
      }
    }
    
    # If multiple Views are present
    if (length(view_values) > 1) {
      return("Activity across selected Views")
    }
    
    # Fallback
    return("Activity")
  }
  
  
  # ----------------------------------------------------------
  # Chart output
  # ----------------------------------------------------------
  # The chart now uses summary_data_for_visuals().
  # This means it responds to:
  # - Apply filters
  # - Choose aggregation
  # - Aggregated results table search/filter
  # - Aggregated results table row selection
  # ----------------------------------------------------------
  
  output$main_chart <- renderPlotly({
    
    result <- summary_data_for_visuals()
    group_vars <- input$group_vars
    
    # ----------------------------------------------------------
    # Variables allowed to appear in visualisation labels
    # ----------------------------------------------------------
    # Measure, View and AggregationLevel are retained in the
    # aggregated results but are not displayed in chart labels.
    display_group_vars <- setdiff(
      group_vars,
      c(
        "Measure",
        "View",
        "AggregationLevel"
      )
    )
    
    # Use a neutral y-axis label.
    # This prevents the selected View from appearing on the y-axis.
    visual_activity_label <- get_visual_activity_label(result)
    
    if (
      is.null(display_group_vars) ||
      length(display_group_vars) == 0
    ) {
      
      # If only Measure, View and AggregationLevel were selected,
      # show one neutral category rather than displaying them.
      result[
        ,
        GroupCombined := "All selected data"
      ]
      
      group_label <- "GroupCombined"
      
    } else if (length(display_group_vars) == 1) {
      
      # Use the one meaningful breakdown variable.
      group_label <- display_group_vars[1]
      
    } else {
      
      # Combine only meaningful breakdown variables.
      # Measure, View and AggregationLevel are excluded.
      result[
        ,
        GroupCombined := do.call(
          paste,
          c(.SD, sep = " | ")
        ),
        .SDcols = display_group_vars
      ]
      
      group_label <- "GroupCombined"
    }
    
    chart_data <- copy(result)
    
    if (!group_label %in% names(chart_data)) {
      
      chart_data[
        ,
        GroupCombined := "All selected data"
      ]
      
      group_label <- "GroupCombined"
    }
    
    chart_data <- chart_data[
      order(-Total)
    ][
      1:min(input$top_n, .N)
    ]
    
    
    # ==========================================
    # BAR CHART
    # ==========================================
    
    if (input$chart_type == "Bar chart") {
      
      p <- ggplot(
        chart_data,
        aes(
          x = reorder(.data[[group_label]], Total),
          y = Total,
          text = paste0(
            group_label, ": ", .data[[group_label]],
            "<br>Total: ", comma(Total),
            "<br>Records: ", comma(Records)
          )
        )
      ) +
        geom_col(fill = "#2C7FB8") +
        coord_flip() +
        scale_y_continuous(labels = comma) +
        labs(
          x = "",  y = visual_activity_label
          
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
      
      
      # ==========================================
      # STACKED BAR CHART
      # ==========================================
      
    } else if (input$chart_type == "Stacked bar chart") {
      
      # ======================================================
      # GENERIC GROUPED BAR CHART
      # ======================================================
      # This section works across all ASC datasets.
      #
      # Structural fields are not used automatically as the
      # x-axis or colour variable.
      #
      # One breakdown variable:
      #   x-axis = that variable
      #
      # Two or more breakdown variables:
      #   x-axis = last selected breakdown variable
      #   colour = second-to-last selected breakdown variable
      #
      # All categories are retained. top_n is not applied.
      # ======================================================
      
      validate(
        need(
          !is.null(group_vars) &&
            length(group_vars) > 0,
          "Please select at least one aggregation variable."
        )
      )
      
      # Keep only grouping variables that actually exist in
      # the aggregated result.
      valid_group_vars <- intersect(
        group_vars,
        names(result)
      )
      
      # These are structural ASC fields. They remain part of
      # the aggregation but are not normally useful as chart
      # axes when breakdown variables are available.
      structural_vars <- c(
        "Measure",
        "View",
        "AggregationLevel",
        "GeographyLevel",
        "FYEnding",
        "SourceFile"
      )
      
      # Candidate variables for the chart axes.
      chart_vars <- setdiff(
        valid_group_vars,
        structural_vars
      )
      
      # If there are no non-structural variables, fall back to
      # any valid grouping variable rather than showing an error.
      if (length(chart_vars) == 0) {
        
        chart_vars <- valid_group_vars
      }
      
      validate(
        need(
          length(chart_vars) > 0,
          paste(
            "No suitable aggregation variable is available",
            "for this chart."
          )
        )
      )
      
      # Use the full aggregated result.
      # Do not use chart_data because chart_data has already
      # been restricted by the top_n control.
      full_chart_data <- copy(result)
      
      # ======================================================
      # ONE BREAKDOWN VARIABLE
      # ======================================================
      
      if (length(chart_vars) == 1) {
        
        x_var <- chart_vars[1]
        
        plot_data <- full_chart_data[
          ,
          .(
            Total = sum(Total, na.rm = TRUE),
            Records = sum(Records, na.rm = TRUE)
          ),
          by = .(
            XValue = as.character(get(x_var))
          )
        ]
        
        # Remove only genuinely blank categories.
        plot_data <- plot_data[
          !is.na(XValue) &
            trimws(XValue) != ""
        ]
        
        validate(
          need(
            nrow(plot_data) > 0,
            paste(
              "No usable values are available for",
              x_var,
              "after applying the current filters."
            )
          )
        )
        
        # Order categories by their total while retaining every
        # category, including those with small values.
        x_order <- plot_data[
          ,
          .(
            XTotal = sum(Total, na.rm = TRUE)
          ),
          by = XValue
        ][
          order(-XTotal),
          XValue
        ]
        
        plot_data[
          ,
          XValue := factor(
            XValue,
            levels = x_order
          )
        ]
        
        p <- ggplot(
          plot_data,
          aes(
            x = XValue,
            y = Total,
            text = paste0(
              x_var,
              ": ",
              XValue,
              "<br>Total: ",
              comma(Total),
              "<br>Records: ",
              comma(Records)
            )
          )
        ) +
          geom_col(
            fill = "#2C7FB8",
            width = 0.7
          ) +
          scale_y_continuous(
            labels = comma,
            expand = expansion(mult = c(0, 0.05))
          ) +
          labs(
            x = x_var,
            y = visual_activity_label
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(
              angle = 45,
              hjust = 1
            )
          )
        
        ggplotly(
          p,
          tooltip = "text"
        )
        
        # ======================================================
        # TWO OR MORE BREAKDOWN VARIABLES
        # ======================================================
        
      } else {
        
        # The last selected breakdown variable is placed on the
        # x-axis. The previous selected breakdown variable is
        # represented by different coloured bars.
        #
        # Examples:
        #
        # AgeBand, ServiceType:
        #   x-axis = ServiceType
        #   colour = AgeBand
        #
        # Gender, DHGeographyName:
        #   x-axis = DHGeographyName
        #   colour = Gender
        x_var <- chart_vars[length(chart_vars)]
        
        colour_var <- chart_vars[
          length(chart_vars) - 1
        ]
        
        plot_data <- full_chart_data[
          ,
          .(
            Total = sum(Total, na.rm = TRUE),
            Records = sum(Records, na.rm = TRUE)
          ),
          by = .(
            XValue = as.character(get(x_var)),
            FillValue = as.character(get(colour_var))
          )
        ]
        
        # Remove rows only when either chart category is blank.
        plot_data <- plot_data[
          !is.na(XValue) &
            trimws(XValue) != "" &
            !is.na(FillValue) &
            trimws(FillValue) != ""
        ]
        
        validate(
          need(
            nrow(plot_data) > 0,
            paste(
              "No usable combination of",
              x_var,
              "and",
              colour_var,
              "is available after applying the current filters."
            )
          )
        )
        
        # Retain every x-axis category and order the categories
        # by the combined total.
        x_order <- plot_data[
          ,
          .(
            XTotal = sum(Total, na.rm = TRUE)
          ),
          by = XValue
        ][
          order(-XTotal),
          XValue
        ]
        
        plot_data[
          ,
          XValue := factor(
            XValue,
            levels = x_order
          )
        ]
        
        p <- ggplot(
          plot_data,
          aes(
            x = XValue,
            y = Total,
            fill = FillValue,
            text = paste0(
              x_var,
              ": ",
              XValue,
              "<br>",
              colour_var,
              ": ",
              FillValue,
              "<br>Total: ",
              comma(Total),
              "<br>Records: ",
              comma(Records)
            )
          )
        ) +
          geom_col(
            position = position_dodge2(
              width = 0.85,
              preserve = "single"
            ),
            width = 0.75,
            colour = "white",
            linewidth = 0.2
          ) +
          scale_y_continuous(
            labels = comma,
            expand = expansion(mult = c(0, 0.05))
          ) +
          labs(
            x = x_var,
            y = visual_activity_label,
            fill = colour_var
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(
              angle = 45,
              hjust = 1
            ),
            legend.position = "right"
          )
        
        ggplotly(
          p,
          tooltip = "text"
        )
      }       
      
      # ==========================================
      # LINE CHART
      # ==========================================
      
    }    else if (input$chart_type == "Line chart") {
      
      p <- ggplot(
        chart_data,
        aes(
          x = .data[[group_label]],
          y = Total,
          group = 1,
          text = paste0(
            group_label, ": ", .data[[group_label]],
            "<br>Total: ", comma(Total),
            "<br>Records: ", comma(Records)
          )
        )
      ) +
        geom_line(colour = "#2C7FB8", linewidth = 1) +
        geom_point(colour = "#08306B", size = 2) +
        scale_y_continuous(labels = comma) +
        labs(
          x = group_label,
          y = visual_activity_label  
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggplotly(p, tooltip = "text")
      
      
      # ==========================================
      # HISTOGRAM
      # ==========================================
      
    } else if (input$chart_type == "Histogram") {
      
      raw_data <- filtered_data_for_visuals()
      
      validate(
        need(
          nrow(raw_data) > 0,
          "No raw data available for the selected aggregated rows."
        )
      )
      
      p <- ggplot(
        raw_data,
        aes(
          x = ITEMVALUE_NUM
        )
      ) +
        geom_histogram(
          bins = 30,
          fill = "#2C7FB8",
          colour = "white"
        ) +
        scale_x_continuous(labels = comma) +
        labs(
          x = "Activity Value",
          y = "Frequency"
        ) +
        theme_minimal()
      
      ggplotly(p)
      
      
      # ==========================================
      # TREEMAP
      # ==========================================
      
    } else if (input$chart_type == "Treemap") {
      
      validate(
        need(
          length(display_group_vars) >= 1,
          paste(
            "Please select at least one breakdown variable",
            "other than Measure, View or AggregationLevel."
          )
        )
      )
      
      tree_data <- copy(chart_data)
      
      if (length(display_group_vars) == 1) {
        
        treemap_label_var <- display_group_vars[1]
        
        plot_ly(
          tree_data,
          type = "treemap",
          labels = tree_data[[treemap_label_var]],
          parents = "",
          values = tree_data$Total,
          textinfo = "label+value+percent root",
          hovertemplate = paste0(
            treemap_label_var,
            ": %{label}<br>",
            "Total: %{value}<br>",
            "<extra></extra>"
          )
        )
        
      } else {
        
        treemap_parent_var <- display_group_vars[1]
        treemap_label_var <- display_group_vars[2]
        
        plot_ly(
          tree_data,
          type = "treemap",
          labels = tree_data[[treemap_label_var]],
          parents = tree_data[[treemap_parent_var]],
          values = tree_data$Total,
          textinfo = "label+value+percent parent",
          hovertemplate = paste0(
            treemap_parent_var,
            ": %{parent}<br>",
            treemap_label_var,
            ": %{label}<br>",
            "Total: %{value}<br>",
            "<extra></extra>"
          )
        )
      }
      
      # ==========================================
      # PIE CHART
      # ==========================================
      
    } else {
      
      plot_ly(
        chart_data,
        labels = ~get(group_label),
        values = ~Total,
        type = "pie",
        textinfo = "label+percent",
        hovertemplate = paste(
          "%{label}<br>",
          "Total: %{value}<br>",
          "<extra></extra>"
        )
      )
    }
  }) 
  
  # ----------------------------------------------------------
  # RESPONSIVE LOCAL AUTHORITY MAP USING GEOJSON POLYGONS
  # ----------------------------------------------------------
  # This map does not modify:
  # - filtered_data()
  # - summary_data()
  # - summary_data_for_visuals()
  # - ITEMVALUE_NUM
  #
  # It applies the same existing map calculation:
  #
  # sum(ITEMVALUE_NUM, na.rm = TRUE)
  #
  # Values are grouped by GeographyCode and then joined to the
  # GeoJSON boundary field CTYUA25CD.
  # ----------------------------------------------------------
  
  map_display_data <- reactive({
    
    # Use the same raw rows associated with the currently visible
    # or selected rows in the Aggregated results table.
    raw_data <- copy(
      filtered_data_for_visuals()
    )
    
    validate(
      need(
        nrow(raw_data) > 0,
        paste(
          "No underlying data is available",
          "for the current map selection."
        )
      )
    )
    
    validate(
      need(
        "GeographyCode" %in% names(raw_data),
        paste(
          "The selected ASC dataset does not contain",
          "a GeographyCode column."
        )
      )
    )
    
    validate(
      need(
        "ITEMVALUE_NUM" %in% names(raw_data),
        paste(
          "The selected ASC dataset does not contain",
          "ITEMVALUE_NUM."
        )
      )
    )
    
    # Create a temporary geography code for the map join.
    # This does not change GeographyCode in filtered_data().
    raw_data[
      ,
      MapGeographyCode := trimws(
        as.character(GeographyCode)
      )
    ]
    
    raw_data <- raw_data[
      !is.na(MapGeographyCode) &
        MapGeographyCode != ""
    ]
    
    validate(
      need(
        nrow(raw_data) > 0,
        paste(
          "No usable local-authority geography codes",
          "are available after the current filters."
        )
      )
    )
    
    # --------------------------------------------------------
    # MAP VALUES
    # --------------------------------------------------------
    # This keeps the same map-value calculation already used
    # in your coordinate map.
    # --------------------------------------------------------
    
    map_values <- raw_data[
      ,
      .(
        Total = sum(
          ITEMVALUE_NUM,
          na.rm = TRUE
        ),
        Records = .N
      ),
      by = MapGeographyCode
    ]
    
    map_values[
      ,
      Total := suppressWarnings(
        as.numeric(Total)
      )
    ]
    
    map_values[
      ,
      Records := suppressWarnings(
        as.numeric(Records)
      )
    ]
    
    map_values <- map_values[
      is.finite(Total)
    ]
    
    validate(
      need(
        nrow(map_values) > 0,
        paste(
          "No numeric activity values are available",
          "for the current map selection."
        )
      )
    )
    
    # Prepare a fresh copy of the GeoJSON boundaries.
    map_boundaries <- local_authority_boundaries[
      ,
      c(
        "CTYUA25CD",
        "CTYUA25NM",
        "geometry"
      )
    ]
    
    # Join:
    #
    # CTYUA25CD       from the GeoJSON
    # MapGeographyCode from the ASC data
    #
    # merge() preserves the sf polygon geometry.
    map_data <- merge(
      map_boundaries,
      as.data.frame(map_values),
      by.x = "CTYUA25CD",
      by.y = "MapGeographyCode",
      all = FALSE,
      sort = FALSE
    )
    
    validate(
      need(
        nrow(map_data) > 0,
        paste(
          "No GeographyCode values in the selected ASC data",
          "matched CTYUA25CD in the local-authority GeoJSON."
        )
      )
    )
    
    validate(
      need(
        inherits(map_data, "sf"),
        "The joined local-authority map data has lost its polygon geometry."
      )
    )
    
    # Rename the displayed fields without changing the join code.
    map_data$MapGeographyCode <- map_data$CTYUA25CD
    map_data$MapGeographyName <- map_data$CTYUA25NM
    
    map_data
  })
  
  
  
  output$uk_map <- renderLeaflet({
    
    map_data <- map_display_data()
    
    visual_activity_label <- get_visual_activity_label(
      summary_data_for_visuals()
    )
    
    validate(
      need(
        nrow(map_data) > 0,
        "No local-authority polygons are available for the map."
      )
    )
    
    validate(
      need(
        is.numeric(map_data$Total),
        "The map Total field is not numeric."
      )
    )
    
    # Create the colour scale from the existing Total values.
    colour_palette <- leaflet::colorNumeric(
      palette = c(
        "#EFF3FF",
        "#BDD7E7",
        "#6BAED6",
        "#3182BD",
        "#08519C"
      ),
      domain = map_data$Total,
      na.color = "#D9D9D9"
    )
    
    # Hover label displayed when the pointer is over a boundary.
    hover_labels <- sprintf(
      paste0(
        "<strong>%s</strong>",
        "<br/>Geography code: %s",
        "<br/>Total activity: %s",
        "<br/>Records: %s"
      ),
      htmltools::htmlEscape(
        map_data$MapGeographyName
      ),
      htmltools::htmlEscape(
        map_data$MapGeographyCode
      ),
      scales::comma(
        map_data$Total
      ),
      scales::comma(
        map_data$Records
      )
    )
    
    hover_labels <- lapply(
      hover_labels,
      htmltools::HTML
    )
    
    # Information displayed when an authority is clicked.
    popup_text <- paste0(
      "<b>",
      htmltools::htmlEscape(
        map_data$MapGeographyName
      ),
      "</b>",
      "<br><b>Geography code:</b> ",
      htmltools::htmlEscape(
        map_data$MapGeographyCode
      ),
      "<br><b>Total activity:</b> ",
      scales::comma(
        map_data$Total
      ),
      "<br><b>Underlying records:</b> ",
      scales::comma(
        map_data$Records
      ),
      "<br><b>Financial Year(s):</b> ",
      htmltools::htmlEscape(
        paste(
          input$fy_ending,
          collapse = ", "
        )
      ),
      "<br><b>Dataset(s):</b> ",
      htmltools::htmlEscape(
        paste(
          input$selected_datasets,
          collapse = ", "
        )
      )
    )
    
    popup_text <- lapply(
      popup_text,
      htmltools::HTML
    )
    
    # Obtain the geographical extent of the selected polygons.
    map_bounds <- sf::st_bbox(map_data)
    
    map <- leaflet::leaflet(
      data = map_data,
      options = leaflet::leafletOptions(
        minZoom = 4
      )
    ) %>%
      
      # Add the standard OpenStreetMap background.
      # Do not pass the word `options` by itself because
      # options is also the name of an R function.
      leaflet::addTiles(
        options = leaflet::tileOptions(
          minZoom = 4
        )
      ) %>%
      
      # Draw the local-authority polygons from the GeoJSON.
      leaflet::addPolygons(
        fillColor = ~colour_palette(Total),
        fillOpacity = 0.75,
        color = "#FFFFFF",
        weight = 1,
        opacity = 1,
        label = hover_labels,
        popup = popup_text,
        highlightOptions = leaflet::highlightOptions(
          weight = 3,
          color = "#333333",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      
      # Add a legend based on the existing Total calculation.
      leaflet::addLegend(
        position = "bottomright",
        pal = colour_palette,
        values = ~Total,
        title = visual_activity_label,
        opacity = 0.8,
        labFormat = leaflet::labelFormat(
          big.mark = ",",
          digits = 0
        )
      ) %>%
      
      # Zoom to the boundaries currently available after filtering.
      leaflet::fitBounds(
        lng1 = as.numeric(map_bounds["xmin"]),
        lat1 = as.numeric(map_bounds["ymin"]),
        lng2 = as.numeric(map_bounds["xmax"]),
        lat2 = as.numeric(map_bounds["ymax"])
      )
    
    map
  })
  
  
  
  
  
  # ----------------------------------------------------------
  # Summary table
  # ----------------------------------------------------------
  # This version keeps row selection enabled and also makes
  # categorical columns in the Aggregated results table use
  # dropdown filters at the top.
  #
  # The dropdown filters work directly inside the table.
  # When the user filters the table, the chart and key insights
  # update because summary_data_for_visuals() uses
  # input$summary_table_rows_all.
  # ----------------------------------------------------------
  
  output$summary_table <- renderDT({
    
    result <- copy(summary_data())
    
    # Convert text/categorical columns to factor so DT creates
    # dropdown filters instead of free-text filters.
    #
    # This is what gives you the "All" dropdown at the top of
    # the Aggregated results table.
    dropdown_columns <- names(result)[
      sapply(
        result,
        function(x) {
          is.character(x) || is.factor(x) || is.logical(x)
        }
      )
    ]
    
    # Do not treat these numeric summary columns as dropdown filters.
    dropdown_columns <- setdiff(
      dropdown_columns,
      c(
        "Total",
        "Records"
      )
    )
    
    if (length(dropdown_columns) > 0) {
      
      for (column_name in dropdown_columns) {
        
        values <- sort(
          unique(
            as.character(result[[column_name]])
          )
        )
        
        values <- values[
          !is.na(values) &
            values != ""
        ]
        
        result[
          ,
          (column_name) := factor(
            as.character(get(column_name)),
            levels = values
          )
        ]
      }
    }
    
    datatable(
      result,
      filter = list(
        position = "top",
        clear = FALSE
      ),
      rownames = FALSE,
      selection = list(
        mode = "multiple",
        target = "row"
      ),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        search = list(
          regex = TRUE,
          smart = FALSE
        ),
        autoWidth = TRUE
      )
    ) %>%
      formatRound(
        columns = "Total",
        digits = 0
      )
    
  }, server = FALSE)  
  
  
  # ----------------------------------------------------------
  # Download filtered data
  # ----------------------------------------------------------
  
  output$download_filtered_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "ASC_Filtered_Data_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    
    content = function(file) {
      
      fwrite(
        filtered_data(),
        file
      )
    }
  )
  
  output$download_insights <- downloadHandler(
    
    filename = function(){
      
      paste0(
        "ASC_Visual_and_Insights_",
        Sys.Date(),
        ".html"
      )
      
    },
    
    content = function(file){
      
      chart_image <- input$plot_image
      
      insights <- insight_text()
      
      html_content <- paste0(
        "
      <html>
      <head>
      <title>ASC Visual and Insights</title>
      </head>
      <body>

      <h1>ASC Activity Explorer</h1>

      <h2>Visualisation</h2>

      <img
        src='", chart_image, "'
        style='width:100%;max-width:1200px;'
      >

      <h2>Key Insights</h2>

      <pre>",
        insights,
        "</pre>

      </body>
      </html>
      "
      )
      
      writeLines(
        html_content,
        file
      )
      
    }
    
  )
  
  
  # ----------------------------------------------------------
  # Filtered underlying data table
  # ----------------------------------------------------------
  
  output$filtered_table <- renderDT({
    
    datatable(
      filtered_data(),
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
    
  })
  
  
  
}

# ============================================================
# 6. RUN APP
# ============================================================

shinyApp(ui, server)

