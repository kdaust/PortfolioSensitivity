library(ccissdev)
library(data.table)
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
library(foreach)
library(scales)
library(dplyr)
library(magrittr)
source("port_functions.R")
Rcpp::sourceCpp("port_cfns.cpp")

pestMat <- fread("Pest_Host_Conifer.csv")
pestLookup <- pestMat[,.(PestCode,PestName)]
pestMat[,c("Group","PestName") := NULL]
pestMat <- melt(pestMat, id.vars = "PestCode",variable.name = "Spp",value.name = "Host")
pestMat <- na.omit(pestMat)
pestProb <- data.table(Pest = pestLookup$PestName,Prob = 0.005)

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
treeOpts <- c("Py","Fd","At","Pl","Sx","Bl","Cw","Hw","Pw","Ss","Lw","Ba","Hm","Dr","Mb")
SuitProb <- data.frame("Suit" = c(1,2,3,4), "ProbDead" = c(0.1,0.5,1,4), "NoMort" = c(95,85,75,50))

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Select Sites",
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
                          selected = c("ssp126","ssp245","ssp370"),
                          c("2.6 W/m2" = "ssp126", "4.5 W/m2" = "ssp245", "7.0 W/m2" = "ssp370", "8.5 W/m2" = "ssp585")
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
             ),
    tabPanel("Portfolio",
             sidebarLayout(
               sidebarPanel(
                            h2("Data Options"),
                            selectInput("port_bgc",label = "Select BGC:", choices = character()),
                            radioButtons("port_ss",label = "Select Site Postion:",choices = c("B2","Zonal","D6"), selected = "Zonal"),
                            selectInput("tree_species",label = "Included Species:", choices = treeOpts, selected = treeOpts, multiple = T),
                            radioButtons("port_length",label = "Optimisation Period (Rotation Length):",
                                         choiceNames = c("Current Period","20 Year","40 Year","60 Year","80 Year"),
                                         choiceValues = c(1991,2021,2041,2061,2081)),
                            h2("Portfolio Parameters"),
                            rHandsontableOutput("setbounds"),
                            sliderInput("return_level","Specified Return:", min = 0.5, max = 1,value = 0.9),
                            sliderInput("min_accept","Minimum allowed weight:",min = 0.01,max = 0.2,value = 0.05),
                            sliderInput("num_sims","Number of Portfolio Simulations:", min = 5, max = 100, value = 25),
                            h2("Testing Parameters"),
                            radioButtons("SI_Class","Site Index Precision Classes: ", choices = c(10,5,3,1),
                                         selected = 1, inline = T),
                            sliderInput("prob_clim","Severity of Climate Loss:", min = 0.01, max = 0.25, value = 0.08, step = 0.01),
                            # sliderInput("prob_pest","Probability of Pest Outbreak:", min = 0, max = 0.1, value = 0.02),
                            rHandsontableOutput("pest_prob"),
                            br(),
                            rHandsontableOutput("env_params"),
                            br(),
                            actionButton("run_simulation", label = "Example Simulation", icon = icon("plus-square"),
                                         style = "width:100%; background-color:#003366; color: #FFF"),
                            br(),
                            actionButton("generate_portfolio", label = "Run Portfolio", icon = icon("plus-square"),
                                         style = "width:100%; background-color:#003366; color: #FFF"),                            
                            width = 3),
               mainPanel(fluidRow(
                        column(6,
                                plotOutput("efficient_frontier", width = "100%", height = "60vh")
                                ),
                         column(6,
                                plotOutput("growth_sim",  width = "100%", height = "60vh"),
                                DTOutput("port_sssum")
                                )
                          ),
                        width = 9
                         )
             )
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
