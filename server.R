
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    uData <- session$userData

    library(sf)
    tOut <- st_read("inputs/TileOutline.gpkg")
    tOut2 <- st_read("inputs/RCB_Outline.gpkg")
    tOut <- st_union(tOut,tOut2)
    portfolio_results <- reactiveValues(data = NULL,climVar = NULL,sppLimits = NULL)
    source("ServerFiles/Server_Functions.R", local = TRUE)
    source("ServerFiles/points.R", local = TRUE)
    source("ServerFiles/generate.R", local = TRUE)
    source("ServerFiles/map.R", local = TRUE)

}
