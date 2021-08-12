#portfolio setup
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
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
source("ServerFiles/port_functions.R")
Rcpp::sourceCpp("ServerFiles/port_cfns.cpp")

pestMat <- fread("inputs/Pest_Host_Conifer.csv")
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
load("inputs/Feas_CovMat.rda")
