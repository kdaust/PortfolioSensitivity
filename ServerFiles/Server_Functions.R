
output$env_params <- renderRHandsontable({
  rhandsontable(SuitProb)
})

output$pest_prob <- renderRHandsontable({
  rhandsontable(pestProb)
})

# observeEvent(input$generate_results, priority = 100, {
#   
#   ticker <- tic("Save Map Inputs")
#   # On generate click, we are taking a snapshot of the current points
#   # and calculating results. All relevant results will be stored in the
#   # userdata environment for further reuse. User has the ability to update
#   # results on demand instead of on app state change. This reduce the load
#   # on the app and give some room in case computation get more costly
#   # in the future. Shared functions will be stored in userdata environment
#   # as well as they will be reused to build report. uData is an alias for
#   # the userdata environment.
#   
#   # Input from the app
#   avg             <- uData$avg             <- as.logical(input$aggregation)
#   rcp             <- uData$rcp             <- input$rcp_scenario
#   pts             <- uData$pts             <- userpoints$dt
#   
#   bgc             <- uData$bgc             <- bgc(pool, pts$Site, avg, rcp)
#   cciss           <- uData$cciss           <- cciss(bgc)
#   cciss_results   <- uData$cciss_results   <- cciss_results(cciss, pts, avg)
#   cciss_summary   <- uData$cciss_summary   <- cciss_summary(cciss, pts, avg)
#   
#   siterefs        <- uData$siterefs        <- sort(unique(bgc$SiteRef))
#   ss_opts <- sort(unique(uData$sspreds$SS_NoSpace))
#   bgc_opts <- unique(uData$bgc$BGC)
#   
#   ##prepare tree choices for portfolio selection
#   suitTrees <- copy(cciss_summary)
#   #print(colnames(suitTrees))
#   suitTrees <- suitTrees[NewSuit %in% c(1,2,3,4),.(Spp, BGC = ZoneSubzone)]
#   suitTrees <- unique(suitTrees)
#   tree_opts <- suitTrees[BGC == bgc_opts[1],Spp]
#   updateSelectInput(inputId = "tree_species",
#                     choices = tree_opts,selected = tree_opts)
#   uData$tree_opts <- suitTrees
#   
#   ssl <- lapply(siterefs, function(sr) {
#     ss <- sort(unique(cciss_results[SiteRef %in% sr]$SS_NoSpace))
#     names(ss) <- paste(
#       ss,
#       stocking_info$SiteSeriesName[match(ss, stocking_info[, paste(ZoneSubzone, SiteSeries, sep = "/")])]
#     )
#     ss
#   })
#   names(ssl) <- siterefs
#   
#   ssa <- sort(unique(cciss_results$SS_NoSpace))
#   names(ssa) <- paste(
#     ssa,
#     stocking_info$SiteSeriesName[match(ssa, stocking_info[, paste(ZoneSubzone, SiteSeries, sep = "/")])]
#   )
#   
#   siteseries_list <- uData$siteseries_list <- ssl
#   siteseries_all  <- uData$siteseries_all  <- ssa
#   
#   if (!isTRUE(avg)) {
#     # ordering choices to match order in points table and create a name for each choice
#     siterefs <- pts[Site %in% siterefs,
#                     {x <- Site; names(x) <- paste(ID, Site, sep = " / "); return(x)}
#     ]
#     uData$siterefs <- siterefs
#   }
#   
#   # Dynamic UI select choices that depends on previous select choice
#   siteref <- head(siterefs, 1)
#   siteseries <- siteseries_list[[siteref]]
# 
#   updateSelectInput(inputId = "port_bgc", choices = bgc_opts, select = bgc_opts[1])
# 
#   session$sendCustomMessage(type="jsCode", list(
#     code= "$('#download_report_span').show()"))
#   session$sendCustomMessage(type="jsCode", list(
#     code= "$('#download_data_span').show()"))
#   session$sendCustomMessage(type="jsCode", list(
#     code= "$('#generate_results').prop('disabled', true)"))
#   updateActionButton(inputId = "generate_results", label = "Refresh results")
#   
#   # Render models info + timings in About
# })

# generateState <- function() {
#   # This prevent the generate button from being enabled when
#   # points do not have valid geometry. There is another
#   # validation in new_points to make sure the newly
#   # added points are located inside the cciss geometry.
#   # Only valid points are used to calculated
#   if (nrow(userpoints$dt[!is.na(Long) & !is.na(Lat)])) {
#     session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', false)"))
#   } else {
#     session$sendCustomMessage(type="jsCode", list(code= "$('#generate_results').prop('disabled', true)"))
#   }
# }
# 
# # These are the triggers to check if we need to change button state
# observeEvent(userpoints$dt, {generateState()})
# observeEvent(input$aggregation, {generateState()})
# observeEvent(input$rcp_scenario, {generateState()})
# 
# # Data processing
# bgc <- function(con, siteno, avg, rcp) {
#   siteno <- siteno[!is.na(siteno)]
#   withProgress(message = "Processing...", detail = "Futures", {
#     dbGetCCISS(con, siteno, avg, rcp)
#   })
# }
# 
# ##bgc <- dbGetCCISS(pool,siteno = c(4532735,4546791,4548548),avg = T, scn = "ssp370")
# # bgc <- sqlTest(pool,siteno = c(6476259,6477778,6691980,6699297),avg = T, scn = "ssp370")
# 
# cciss <- function(bgc) {
#   SSPred <- edatopicOverlap(bgc, Edatope = E1)
#   setorder(SSPred,SiteRef,SS_NoSpace,FuturePeriod,BGC.pred,-SSratio)
#   uData$eda_out <- SSPred
#   SSPred2 <- SSPred[,.(SSLab = paste(SS.pred,collapse = "<br>")), 
#                     by = .(SiteRef,SS_NoSpace,FuturePeriod,BGC.pred,BGC.prop)]
#   uData$sspreds <- SSPred2
#   ccissOutput(SSPred = SSPred, suit = S1, rules = R1, feasFlag = F1) ##prob don't need this
# }

#####portfolio#####
makeColScale <- function(Trees){ ##make a new one of these for the line graphs
  cols <- TreeCols
  myPal <- cols$HexColour
  names(myPal) <- cols$TreeCode
  myColours <- data.table(TreeCode = Trees)
  myColours <- cols[myColours, on = "TreeCode"]
  myColours <- myColours[!is.na(HexColour),]
  pal <- myColours$HexColour
  names(pal) <- myColours$TreeCode
  colScale <- scale_fill_manual(name = "variable", values = pal)
  return(colScale)
}

makeColScale_line <- function(Trees){ ##make a new one of these for the line graphs
  cols <- TreeCols
  myPal <- cols$HexColour
  names(myPal) <- cols$TreeCode
  myColours <- data.table(TreeCode = Trees)
  myColours <- cols[myColours, on = "TreeCode"]
  myColours <- myColours[!is.na(HexColour),]
  pal <- myColours$HexColour
  names(pal) <- myColours$TreeCode
  colScale <- scale_colour_manual(name = "Spp", values = pal)
  return(colScale)
}

##update possible species
observeEvent(input$port_bgc,{
  if(input$generate_results > 0){
    suitTrees <- copy(uData$cciss_summary)
    #print(colnames(suitTrees))
    suitTrees <- suitTrees[NewSuit %in% c(1,2,3,4),.(Spp, BGC = ZoneSubzone)]
    suitTrees <- unique(suitTrees)
    tree_opts <- suitTrees[BGC == input$port_bgc,Spp]
    tree_opts <- tree_opts[!tree_opts %in% c("Ac","Sb")]
    updateSelectInput(inputId = "tree_species",
                      choices = tree_opts,selected = tree_opts)
  }
  
})

output$setbounds <- renderRHandsontable({
  Trees <- input$tree_species
  boundDat <- data.table(Spp = Trees)
  boundDat[,`:=`(minWt = 0, maxWt = 1)]
  rhandsontable(boundDat)
})

output$setclimbounds <- renderRHandsontable({
  Trees <- input$tree_species
  dat <- data.table(Spp = Trees,LossSeverity = 0.08)
  rhandsontable(dat)
})

observeEvent(input$get_clim,{
  BGC <- input$port_bgc
  FutScn <- "ssp245"
  SuitTable <- copy(S1)
  setnames(SuitTable,old = "Feasible",new = "Suitability",skip_absent = T)
  Trees <- treeList <- input$tree_species
  show_modal_spinner()
  portfolio_results$climVar <- dbGetClimSum_kd(poolclim,BGC,FutScn)
  portfolio_results$sppLimits <- dbGetSppLimits_kd(poolclim,SuitTable,Trees)
  remove_modal_spinner()
})

observeEvent(input$run_simulation,{
  timePeriods <- input$port_length
  Trees <- treeList <- input$tree_species
  ProbPest <- input$prob_pest
  BGC <- input$port_bgc
  siteLoc <- input$port_ss
  FutScn <- "ssp245"
  SiteList <- uData$pts$Site
  boundDat <- hot_to_r(input$setbounds)
  climLoss <- as.data.table(hot_to_r(input$setclimbounds))

  allPeriods <- c(1961,1991,2021,2041,2061,2081)
  selectPer <- which(allPeriods == timePeriods)
  timePeriods <- allPeriods[1:selectPer]
  SuitProb <- as.data.table(hot_to_r(input$env_params))
  adjPestProb <- as.data.table(hot_to_r(input$pest_prob))
  SuitTable <- copy(S1)
  setnames(SuitTable,old = "Feasible",new = "Suitability",skip_absent = T)
  SSPredOrig <- copy(uData$eda_out)
  SSPredOrig[,allOverlap := NULL]
  setnames(SSPredOrig, old = c("BGC","SiteRef"), new = c("MergedBGC","SiteNo"))
  SSPredOrig <- SSPredOrig[,.(MergedBGC,SS_NoSpace,SSratio,SSprob,SS.pred,FuturePeriod,SiteNo)]
  adjPestProb[pestLookup,PestCode := i.PestCode, on = c(Pest = "PestName")]
  adjPestProb <- adjPestProb[,.(PestCode,Prob)]
  adjPestProb <- rbind(adjPestProb, data.table(PestCode = "None",Prob = 0.8))
  
  SSPredFull <- edatopicSubset_kd(SSPredOrig,E1,pos = siteLoc) ##this should be changeable
  nSpp <- length(treeList)
  Units <- unique(SSPredFull$BGC_analysis)
  SSPredBGC <- SSPredFull[BGC_analysis == BGC,-("BGC_analysis")]
  SSList <- unique(SSPredBGC$SS_NoSpace)
  selectBGC = SSList[1] ##this is where we change it to allow multiple BGCs
  SSPredAll <- SSPredBGC[SSPredBGC$SS_NoSpace == selectBGC,]
  SiteList <- unique(SSPredAll$SiteNo)
  SSPredAll <- SSPredAll[SiteNo %in% SiteList & !is.na(SSprob),]
  
  if(is.null(portfolio_results$climVar)){
    showModal(modalDialog(
      h2("Please get climate data first by clicking the above button")
    ))
  }else{
    climVar <- portfolio_results$climVar
    sppLimits <- portfolio_results$sppLimits
    
    SL <- SiteList
    port_results <- run_simulation(SL,climVar,SSPredAll,SIBEC,SuitTable,
                                   Trees,timePeriods,selectBGC,SuitProb,
                                   sppLimits,PestSpp = pestMat,
                                   ProbPest = adjPestProb,
                                   SI_Class = as.numeric(input$SI_Class),
                                   climLoss = climLoss)
    output$single_sim <- renderPlot({
      colScale <- makeColScale_line(unique(port_results$Spp))
      ggplot(port_results, aes(x = Year, y = Returns, color = Spp)) +
        geom_line() +
        colScale +
        theme_few() + 
        expand_limits(y = 0)
    })
    showModal(modalDialog(
      plotOutput("single_sim"),
      easyClose = T
    ))
  }
  
})

observeEvent(input$generate_portfolio,{
  timePeriods <- input$port_length
  returnValue <- input$return_level
  Trees <- treeList <- input$tree_species
  minAccept <- input$min_accept
  ProbPest <- input$prob_pest
  BGC <- input$port_bgc
  siteLoc <- input$port_ss
  FutScn <- "ssp245"
  #EdaPos <- input$eda_pos
  SiteList <- uData$pts$Site
  boundDat <- hot_to_r(input$setbounds)
  ##setup climate_data connection
  allPeriods <- c(1961,1991,2021,2041,2061,2081)
  selectPer <- which(allPeriods == timePeriods)
  timePeriods <- allPeriods[1:selectPer]
  SuitProb <- as.data.table(hot_to_r(input$env_params))
  adjPestProb <- as.data.table(hot_to_r(input$pest_prob))
  climLoss <- as.data.table(hot_to_r(input$setclimbounds))
  simCovMat <- input$cov_type
  
  withProgress(message = "Optimising...", detail = "Lots of calculations...", {
    #Trees <- treeList <- c("Py","Fd","At","Pl","Sx","Bl","Cw","Hw","Pw","Ss","Lw","Ba","Hm","Dr","Mb")
    #timePeriods = c(1961,1991,2021,2041,2061)
    #returnValue = 0.9
    #FutScn <- "ssp370"
    #minAccept <- 0.01
    SuitTable <- copy(S1)
    setnames(SuitTable,old = "Feasible",new = "Suitability",skip_absent = T)
    
    # siteids <- c(6487982,6484391,6484900,6485410,6485920)
    # bgcDat <- dbGetCCISS(pool,siteids,avg = F, scn = "ssp370")
    # sspreds <- edatopicOverlap(bgcDat,Edatope = E1) ##reuse from uData$sspreds
    # SSPredOrig <- sspreds
    #browser()
    SSPredOrig <- copy(uData$eda_out)
    SSPredOrig[,allOverlap := NULL]
    setnames(SSPredOrig, old = c("BGC","SiteRef"), new = c("MergedBGC","SiteNo"))
    SSPredOrig <- SSPredOrig[,.(MergedBGC,SS_NoSpace,SSratio,SSprob,SS.pred,FuturePeriod,SiteNo)]
    
    SSPredFull <- edatopicSubset_kd(SSPredOrig,E1,pos = siteLoc) ##this should be changeable
    nSpp <- length(treeList)
    Units <- unique(SSPredFull$BGC_analysis)
    SSPredBGC <- SSPredFull[BGC_analysis == BGC,-("BGC_analysis")]
    SSList <- unique(SSPredBGC$SS_NoSpace)
    selectBGC = SSList[1] ##this is where we change it to allow multiple BGCs
    SSPredAll <- SSPredBGC[SSPredBGC$SS_NoSpace == selectBGC,]
    SiteList <- unique(SSPredAll$SiteNo)
    SSPredAll <- SSPredAll[SiteNo %in% SiteList & !is.na(SSprob),]
    ##process pest data
    adjPestProb[pestLookup,PestCode := i.PestCode, on = c(Pest = "PestName")]
    adjPestProb <- adjPestProb[,.(PestCode,Prob)]
    adjPestProb <- rbind(adjPestProb, data.table(PestCode = "None",Prob = 0.8))
    
    incProgress()
    if(is.null(portfolio_results$climVar)){
      showModal(modalDialog(
        h2("Please get climate data first by clicking the above button")
      ))
    }else{
      climVar <- portfolio_results$climVar
      sppLimits <- portfolio_results$sppLimits
    
      SL <- SiteList
      numTimes <- as.integer(as.numeric(input$num_sims)/length(SL))
      SL <- rep(SL, each = numTimes)
      port_results <- run_portfolio_kd(SL,climVar,SSPredAll,SIBEC,SuitTable,
                                    Trees,timePeriods,selectBGC,SuitProb,returnValue,
                                    sppLimits,minAccept,boundDat,PestSpp = pestMat,
                                    ProbPest = adjPestProb,
                                    SI_Class = as.numeric(input$SI_Class),
                                    climLoss = climLoss,simCovMat = simCovMat)
      incProgress(amount = 0.6)
      print("Done Portfolio")
      #print(port_results$raw)
      portfolio_results$data <- port_results
    }
  })
  
})

output$efficient_frontier <- renderPlot({
  if(is.null(portfolio_results$data)) return(NULL)
  
  colScale <- makeColScale(input$tree_species)
  dat <- copy(portfolio_results$data)
  print(ef_plot(dat$raw,dat$summary,colScale))
})

output$growth_sim <- renderPlot({
  if(is.null(portfolio_results$data)) return(NULL)
  dat <- copy(portfolio_results$data$simulated)
  colScale <- makeColScale_line(unique(dat$Spp))
  # dat[,It := as.factor(It)]
  dat <- dat[,.(Returns = mean(Returns)), by = .(Year,Spp)]
  ggplot(dat, aes(x = Year, y = Returns, color = Spp)) +
    geom_line() +
    colScale +
    theme_few() + 
    expand_limits(y = 0)
})

output$port_table <- renderTable({
  if(is.null(portfolio_results$data)) return(NULL)
  dat <- copy(portfolio_results$data)
  print(dat$simulated)
  temp <- dat$summary
  temp <- temp[!Spp %chin% c("RealRet","Sd"),.(Spp,Sharpe_Opt,Set_Return)]
  setnames(temp,c("Species","Sharpe","SetReturn"))
  temp
})

output$port_sssum <- renderDT({
  if(is.null(portfolio_results$data)) return(NULL)
  dat <- copy(portfolio_results$data$ssdat)
  dat <- melt(dat, id.vars = c("Spp","FuturePeriod"))
  dat2 <- dcast(dat, Spp + FuturePeriod ~ variable, fun.aggregate = mean)
  setnames(dat2, old = "FuturePeriod",new = "Period")
  formatRound(datatable(dat2,caption = "Mean SI and Feasibility"),columns = "MeanSI")
})

observeEvent(input$show_sssum,{
  showModal(modalDialog(
    h2("Site Index and Feasibility Values"),
    DTOutput("port_sssum"),
    size = "l"
  ))
})
