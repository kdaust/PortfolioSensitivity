
library(ccissdev)
library(shiny)
library(leaflet)
library(DT)
library(RPostgres)
library(plotly)
library(pool)
library(ggplot2)
library(ggthemes)
library(rhandsontable)
library(sf)

pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("BCGOV_DB"),
    host = Sys.getenv("BCGOV_HOST"),
    port = 5432, 
    user = Sys.getenv("BCGOV_USR"),
    password = Sys.getenv("BCGOV_PWD")
)
onStop(function() {
    poolClose(pool)
})
poolclim <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "bgc_climate_data",
    host = Sys.getenv("BCGOV_HOST"),
    port = 5432, 
    user = Sys.getenv("BCGOV_USR"),
    password = Sys.getenv("BCGOV_PWD")
)
onStop(function() {
    poolClose(poolclim)
})
bcgov_tileserver <- Sys.getenv("BCGOV_TILESERVER")
bcgov_tilelayer <- Sys.getenv("BCGOV_TILELAYER")
if (is.null(bcgov_tileserver) || is.null(bcgov_tilelayer)) stop("tileserver env not set")
mbtk <- Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbstyle <- Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbhsstyle <- Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")

ui <- fluidPage(
  fluidRow(
    column(3,
           # Actions on points
           actionButton("upload_button", "Upload", icon("upload"), width=120),
           actionButton("add_dialog", "Add", icon("plus"), width=120),
           actionButton("delete_button", "Delete", icon("trash-alt"), width=120),
           actionButton("clear_button", "Clear", icon("broom"), width=120),
           
           # Points
           DT::DTOutput("points_table", width="100%"),
           
           # Control results
           splitLayout(
             checkboxGroupInput(
               "rcp_scenario",
               "RCP Scenario:",
               selected = "rcp45",
               c("4.5 W/m2" = "rcp45", "8.5 W/m2" = "rcp85")
             ),
             radioButtons(
               "aggregation",
               "Multiple Point Aggregation:",
               c("Individual" = "FALSE", "Averaged by BGC Zone" = "TRUE")
             )
           ),
           
           # Generate results
           actionButton("generate_results", label = "Generate results", icon = icon("plus-square"),
                        style = "width:100%; background-color:#003366; color: #FFF")
           ),
    column(9,
           leafletOutput("bec_map",height = "85vh")
           )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    uData <- session$userData
    library(sf)
    tOut <- st_read("./TileOutline.gpkg")
    tOut2 <- st_read("./RCB_Outline.gpkg")
    tOut <- st_union(tOut,tOut2)
    portfolio_results <- reactiveValues(data = NULL)
    source("Server.R", local = TRUE)
    source("points.R", local = TRUE)
    source("generate.R", local = TRUE)
    source("map.R", local = TRUE)

}

# Run the application 
shinyApp(ui = ui, server = server)
