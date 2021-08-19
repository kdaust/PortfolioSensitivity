source("ServerFiles/AppSetup.R")

ui <- fluidPage(theme = shinytheme("lumen"),
                tabsetPanel(
                  tabPanel("Select Sites",
                           br(),
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
                               
                                      radioButtons(
                                        "aggregation",
                                        "Multiple Point Aggregation:",
                                        c("Individual" = "FALSE", "Averaged by BGC Zone" = "TRUE")
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
                           useShinyjs(),
                           fluidPage(
                             column(3,
                                    h2("Portfolio Options"),
                                    br(),
                                    panel(style = "overflow-y:scroll; max-height: 900px; position:relative; align: centre",
                                          h2("Instructions"),
                                          p("Before running any portfolios, make sure you have clicked Generate Results
                                            on the previous tab. Next, select  your data options, and click Get Climatic
                                            Limits. This will calculate the limits for each species. You can then change
                                            the other parameters, and click either Example Simulation to quickly
                                            show an example of simulated growth, or Generate Portfolio to create a 
                                            portfolio based on the selected parameters. Note that if you change the Data Options,
                                            you must recalculate the species limits."),
                                          h2("Data Options"),
                                          selectInput("port_bgc",label = "Select BGC:", choices = character()),
                                          radioButtons("port_ss",label = "Select Site Postion:",choices = c("B2","Zonal","D6"), selected = "Zonal"),
                                          selectInput("tree_species",label = "Included Species:", choices = treeOpts, selected = treeOpts, multiple = T),
                                          actionButton("get_clim", label = "Get Climatic Limits", icon = icon("plus-square"),
                                                       style = "width:100%; background-color:#003366; color: #FFF"),
                                          
                                          h2("Portfolio Parameters"),
                                          radioButtons("port_length",label = "Optimisation Period (Rotation Length):",
                                                       choiceNames = c("Current Period","20 Year","40 Year","60 Year","80 Year"),
                                                       choiceValues = c(1991,2021,2041,2061,2081)),
                                          rHandsontableOutput("setbounds"),
                                          sliderInput("return_level","Specified Return:", min = 0.5, max = 1,value = 0.9),
                                          sliderInput("min_accept","Minimum allowed weight:",min = 0.01,max = 0.2,value = 0.05),
                                          sliderInput("num_sims","Number of Portfolio Simulations:", min = 5, max = 100, value = 25),
                                          h2("Testing Parameters"),
                                          radioButtons("cov_type", "Covariance Type",
                                                       c("From Simulations" = TRUE,"From Feasibility" = FALSE),
                                                       selected = TRUE),
                                          radioButtons("SI_Class","Site Index Precision Classes: ", choices = c(10,5,3,1),
                                                       selected = 1, inline = T),
                                          ## sliderInput("prob_clim","Severity of Climate Loss:", min = 0.01, max = 0.25, value = 0.08, step = 0.01),
                                          # sliderInput("prob_pest","Probability of Pest Outbreak:", min = 0, max = 0.1, value = 0.02),
                                          h4("Climatic Loss Severity"),
                                          rHandsontableOutput("setclimbounds"),
                                          hr(),
                                          h4("Annual Probability of Pest"),
                                          rHandsontableOutput("pest_prob"),
                                          hr(),
                                          h4("Probability of Environmental Loss"),
                                          rHandsontableOutput("env_params"),
                                          br(),
                                          actionButton("run_simulation", label = "Example Simulation", icon = icon("plus-square"),
                                                       style = "width:100%; background-color:#003366; color: #FFF"),
                                          br(),
                                          actionButton("generate_portfolio", label = "Run Portfolio", icon = icon("plus-square"),
                                                       style = "width:100%; background-color:#003366; color: #FFF")
                                    )
                             ),
                             column(9,
                                    fluidRow(
                                      column(6,
                                             h2("Efficient Frontier"),
                                             plotOutput("efficient_frontier", width = "100%", height = "60vh")
                                      ),
                                      column(6,
                                             h2("Mean of Simulations"),
                                             plotOutput("growth_sim",  width = "100%", height = "60vh"),
                                             br(),
                                             h2("SI and Feasibility Values"),
                                             DTOutput("port_sssum")
                                      )
                                    )
                             )
                           )
                  )
                )
                
)
