##################################################
#              Polya Urns Simulations
#              Laura Caron
#              Columbia University
#         This version: September 8, 2023
##################################################

##################################################
#                      Set up
#           Loads the packages we need
##################################################


## Set up renv for package version control
if (!require("remotes"))
  install.packages("remotes")

#library(renv)
#renv::restore()

library(remotes)
library(tidyverse)
library(plotly)
library(gridExtra)
library(pryr)
library(scales)
library(shiny)
library(shinythemes)
library(rsconnect)
library(shinyMatrix)
library(shinyBS)


##################################################
#                      UI
#       Controls the interface of app
##################################################

# Set app theme
ui <- fluidPage(theme=shinytheme("flatly"),
# Application title
navbarPage("Polya Urns", id="nav",
  # About page
   tabPanel("About", 
    fluidRow(column(8, uiOutput("help"), offset=2 ))
   ),
  # Simulations page
   tabPanel("Simulations",           
            sidebarLayout(
              
              # Sidebar for simulation parameters
              
              sidebarPanel(
                    # Additional formatting 
                    tags$head(
                      tags$style(
                        HTML(".vue-input td {background-color: white; border: 1px solid #dce4ec !important;}
                              .vue-input th {border: 0px !important; text-align:center;}
                              .vue-input td {border-radius:4px !important;}")
                      )),
                
                # enable Latex input
                withMathJax(),
                
                # First section: Parameters
                h3("Simulation Parameters"),
                fluidRow(
                  column(6, numericInput("I", "Number of urns to simulate", 100, min=1, step=5)),
                  column(6, numericInput("N", "Number of draws from each urn", 100, min=1, step=10))),
                fluidRow(
                  column(12, numericInput("seed", "Seed", 1234, min=0))),
                
                # Second section: Initial state
                h3("Initial Urn Contents"),
                fluidRow(
                  column(5, numericInput("w_0", "Initial number of white balls", 10, min=0)), 
                  column(5, numericInput("m_0", "Initial number of maroon balls", 40, min=0))),
                
                # Third section: Replacement/addition 
                h3("Addition Scheme"),
                fluidRow(
                  column(12, radioButtons("multidraw", label=NULL, choices=c("Single draw"="single", "Multiple draws"="multi")))),
                conditionalPanel(condition = "input.multidraw=='multi'", 
                                 fluidRow(column(12, checkboxInput("multi_interp", "Interpret multiple draw as selection intervention?", value=TRUE), 
                                                 bsTooltip(id = "multi_interp", 
                                                           title = "When this option is turned on, the number of M and W selected will correspond to the number replaced. When this option is turned off, the number of M and W selected will correspond to the number drawn.")))),
                
                # Single draw options
                conditionalPanel(condition = "input.multidraw=='single'",
                  div(
                    p("Note: the original ball is replaced and addition follows the rules below."),
                    h4("If a white ball is drawn:"),
                    
                    #options for stochastic replacement -- only appear when selected
                    fluidRow(column(12, radioButtons("woman_stochastic", label=NULL, choices=c("Deterministic addition"="none", "Stochastic addition (correlated)" = "balanced", "Stochastic addition (uncorrelated)"= "unbalanced"), selected="none"))),
                    conditionalPanel(condition = "input.woman_stochastic!= 'none'",
                         fluidRow(                       
                           column(12, radioButtons("woman_depends", label="Depends on", choices=c("None"="none", "\\(X = \\) share of white balls in urn"="urn", "\\(X = \\) share of white balls in selected candidates"="selected"), selected="none"))),
                         fluidRow(
                           conditionalPanel(condition="input.woman_depends=='none'",
                                            column(5, numericInput("p_w_w", "Probability of white ball added", 1, step=0.1))),
                           conditionalPanel(condition="input.woman_depends!='none'",
                                            column(5, radioButtons("w_w_function", label="Probability of white ball added", choices=c("\\(p_{w_w} = c-bX^a\\)"="linear","\\(p_{w_w} = \\frac{1}{1+bX^a}\\)"="inverse", "\\(p_{w_w} = \\frac{1}{1+b*\\exp(cX)}\\)"="inverseexp"))))
                         ),
                         fluidRow(
                           conditionalPanel(condition="input.w_w_function!='inverseexp'&input.woman_depends!='none'", 
                                            column(4, numericInput("w_w_a", "\\(a\\)", 1, step = 0.1)), 
                                            column(4, numericInput("w_w_b", "\\(b\\)", 1, step=0.1)),
                                            column(4, numericInput("w_w_c", "\\(c\\)", 1, step=0.1))), 
                           conditionalPanel(condition="input.w_w_function=='inverseexp'", 
                                            column(5, numericInput("w_w_b", "\\(b\\)", 1, step=0.1)), 
                                            column(5, numericInput("w_w_c", "\\(c\\)", 1, step=0.1)))  ) ,
                         
                         conditionalPanel(condition="input.woman_stochastic=='unbalanced'",
                                          fluidRow(
                                            conditionalPanel(condition="input.woman_depends=='none'",
                                                             column(5, numericInput("p_m_w", "Probability of maroon ball added", 1, step=0.1))),
                                            conditionalPanel(condition="input.woman_depends!='none'",
                                                             column(5, radioButtons("m_w_function", label="Probability of maroon ball added", choices=c("\\(p_{m_w} = c-b(1-X)^a\\)"="linear","\\(p_{m_w} = \\frac{1}{1+b(1-X)^a}\\)"="inverse", "\\(p_{m_w} = \\frac{1}{1+b*\\exp(c(1-X))}\\)"="inverseexp"))))
                                          )),
                         fluidRow(
                           conditionalPanel(condition="input.m_w_function!='inverseexp'&input.woman_depends!='none' & input.woman_stochastic=='unbalanced'", 
                                            column(4, numericInput("m_w_a", "\\(a\\)", 1, step = 0.1)), 
                                            column(4, numericInput("m_w_b", "\\(b\\)", 1, step=0.1)),
                                            column(4, numericInput("m_w_c", "\\(c\\)", 1, step=0.1))), 
                           conditionalPanel(condition="input.m_w_function=='inverseexp'", 
                                            column(5, numericInput("m_w_b", "\\(b\\)", 1, step=0.1)), 
                                            column(5, numericInput("m_w_c", "\\(c\\)", 1, step=0.1)))  )            
        ),
                    fluidRow(
                      column(5, numericInput("w_w", "Number of white balls added", 1)), 
                      column(5, numericInput("m_w", "Number of maroon balls added", 0))
                    ),
        
        
                    h4("If a maroon ball is drawn:"),
                    fluidRow(column(12, radioButtons("man_stochastic", label=NULL, choices=c("Deterministic addition"="none", "Stochastic addition (correlated)" = "balanced", "Stochastic addition (uncorrelated)"= "unbalanced"), selected="none"))),
                    conditionalPanel(condition = "input.man_stochastic!= 'none'",
                         fluidRow(                       
                           column(12, radioButtons("man_depends", label="Depends on", choices=c("None"="none", "\\(X = \\) share of white balls in urn"="urn", "\\(X = \\) share of white balls in selected candidates"="selected"), selected="none"))),
                         fluidRow(
                           conditionalPanel(condition="input.man_depends=='none'",
                                            column(5, numericInput("p_w_m", "Probability of white balls added", 1, step=0.1))),
                           conditionalPanel(condition="input.man_depends!='none'",
                                            column(5, radioButtons("w_m_function", label="Probability of white balls added", choices=c("\\(p_{w_m} = c-bX^a\\)"="linear","\\(p_{w_m} = \\frac{1}{1+bX^a}\\)"="inverse", "\\(p_{w_m} = \\frac{1}{1+b*\\exp(cX)}\\)"="inverseexp"))))
                         ),
                         fluidRow(
                           conditionalPanel(condition="input.w_m_function!='inverseexp'&input.man_depends!='none'", 
                                            column(4, numericInput("w_m_a", "\\(a\\)", 1, step = 0.1)), 
                                            column(4, numericInput("w_m_b", "\\(b\\)", 1, step=0.1)),
                                            column(4, numericInput("w_m_c", "\\(c\\)", 1, step=0.1))), 
                           conditionalPanel(condition="input.w_m_function=='inverseexp'", 
                                            column(5, numericInput("w_m_b", "\\(b\\)", 1, step=0.1)), 
                                            column(5, numericInput("w_m_c", "\\(c\\)", 1, step=0.1)))  ) ,
                         
                         conditionalPanel(condition="input.man_stochastic=='unbalanced'",
                                          fluidRow(
                                            conditionalPanel(condition="input.man_depends=='none'",
                                                             column(5, numericInput("p_m_m", "Probability of maroon balls added", 1, step=0.1))),
                                            conditionalPanel(condition="input.man_depends!='none'",
                                                             column(5, radioButtons("m_m_function", label="Probability of maroon balls added", choices=c("\\(p_{m_m} = 1-b(1-X)^a\\)"="linear","\\(p_{m_m} = \\frac{1}{1+b(1-X)^a}\\)"="inverse", "\\(p_{m_m} = \\frac{1}{1+b*\\exp(c(1-X))}\\)"="inverseexp"))))
                                          )),
                         fluidRow(
                           conditionalPanel(condition="input.m_m_function!='inverseexp'&input.man_depends==true & input.man_stochastic=='unbalanced'", 
                                            column(4, numericInput("m_m_a", "\\(a\\)", 1, step = 0.1)), 
                                            column(4, numericInput("m_m_b", "\\(b\\)", 1, step=0.1)),
                                            column(4, numericInput("m_m_c", "\\(c\\)", 1, step=0.1))), 
                           conditionalPanel(condition="input.m_m_function=='inverseexp'", 
                                            column(5, numericInput("m_m_b", "\\(b\\)", 1, step=0.1)), 
                                            column(5, numericInput("m_m_c", "\\(c\\)", 1, step=0.1)))  )            
                    ),
                    fluidRow(
                      column(5, numericInput("w_m", "Number of white balls added", 0)), 
                      column(5, numericInput("m_m", "Number of maroon balls added", 1))
                    )
                  )),
                  # Multiple draws
                conditionalPanel(condition = "input.multidraw=='multi'", div(
                    fluidRow(
                      column(6, numericInput("num_draws", "Number of draws", 1, step=1, min=1))
                    ),
                    h4("If __ is drawn,"),
                    fluidRow(
                      column(12, uiOutput("matrixIn")))
                  )),
                # Fourth section: Exit
                h3("Exit Options"),
                fluidRow(column(12, checkboxInput("exit_selected", "Balls exit from pool of selected (oldest first)", value=F))),
                conditionalPanel(condition="input.exit_selected==true", 
                                 fluidRow(column(12, numericInput("prob_exit", "Probability of exit in each round", value=0.01, min=0, max=1, step=0.1)))),
    
                # Fourth section: Interventions
                # Only show for single draw 
                conditionalPanel("input.multidraw=='single'", div(
                h3("Interventions"),
                fluidRow(column(12, radioButtons("intervention", "Affirmative Action", selected="none", choices=c("None"="none", "Draw two, if at least one is a woman, select a woman (deterministic)"="atleast","Draw two, if at least one is a woman, select a woman with probability"="atleast_stochastic","Draw one and add accordingly, plus always add one woman each round"="alwayswoman", "Hiring quota"="quota")))),
                conditionalPanel(condition="input.intervention=='atleast_stochastic'", 
                     fluidRow(column(12, numericInput("prob_atleast", "Probability of selecting second-best woman", value=1, min=0, max=1, step=0.1)))),
                conditionalPanel(condition="input.intervention=='quota'", 
                                 column(6, numericInput("quota_per", "Select at least __ W", value=1, min=1, max=2, step=1)),
                                 column(6, numericInput("quota_window", "every __ draws", value=1, min=1, max=2, step=1)),
                                 #column(6, numericInput("quota", "Continue until W make up __", value=0.5, min=0, max=1, step=0.1)),
                                 #column(6, radioButtons("quota_group", label="", choices=c("of selected candidates"="selected", "of urn"="urn"))),
                                 #column(6, numericInput("quota_start", "Start after draw (enter 0 for start at beginning)", value=0, min=0, step=1))
                                 ),
                     fluidRow(column(12, conditionalPanel(condition = "input.intervention != 'none'",
                         radioButtons("stopintervention", "When to stop?", selected="continue", choices=c("Continue forever"="continue", "Stop if white balls more than __ in each urn"="majority","Stop if white balls more than __ among selected for each urn"="majority_selected", "Stop after X draws"="temp", "Stop if white balls more than __ in average urn" = "avg", "Stop if white balls more than __ in average selected candidates"="avg_selected")),
                         ))
                ), 
                fluidRow(column(6, conditionalPanel(condition = "input.intervention != 'none'", numericInput("aa_start", "Start after draw (enter 0 for start at beginning)", value=0, min=0, step=1))),
                         conditionalPanel(condition="input.stopintervention=='temp'& input.intervention!='none'", 
                                                     column(6, numericInput("stopafter", "Stop after", 30))),
                                    conditionalPanel(condition="(input.stopintervention=='avg' | input.stopintervention=='majority') & input.intervention!='none'", 
                                                     column(6, numericInput("cutoff", "Use AA until white balls make up __ of the urn", value=0.5, min=0, max=1, step=0.1))),
                                conditionalPanel(condition="(input.stopintervention=='avg_selected' | input.stopintervention=='majority_selected') & input.intervention!='none'", 
                                                 column(6, numericInput("cutoff", "Use AA until white balls make up __ of the pool of selected candidates", value=0.5, min=0, max=1, step=0.1)))),
                )),
                # Fifth section: Graph options
                h3("Graph options"), 
                fluidRow(column(12,
                  radioButtons("graph_auto", "Dimensions", choices=c("Automatic"="auto", "Custom"="custom")),
                  fluidRow(conditionalPanel(condition="input.graph_auto=='custom'",
                            column(6, numericInput("graph_dim", "Graph Dimensions", value=100, min=0,step=50)),
                            column(6, numericInput("graph_origin", "Origin", value=0, min=0,step=10))))
                )),
                
                
                # Button to refresh simulation results 
                fluidRow(column(5, actionButton("rerun", "Re-run Simulation")))
              ),
              
              # Results section
              mainPanel(
                tabsetPanel(
                  # First tab
                  tabPanel("Distribution of Share of White Balls", 
                           # histogram and ratio over time
                           fluidRow(column(6,plotlyOutput("ratio_over_time", height="50%")),
                                    column(6, plotlyOutput("ratio_s_over_time", height="50%"))),
                           # density plot and cdf
                           fluidRow(column(6, plotlyOutput("histogram", height="50%")),
                                    column(6, plotlyOutput("cdf", height="50%")))),
                  # Second tab 
                  tabPanel("Urn Paths Over Time", 
                           # Dynamic graph titles
                           fluidRow(uiOutput("distribution_title")),
                           # Urn paths over time
                           fluidRow(column(9, plotlyOutput("rayplot", height="50%"))),
                           # Probability of selecting woman over time
                           fluidRow(column(8, plotlyOutput("prob_w_over_time", height="50%")))
                  ),
                  # Third tab
                  tabPanel("Selected Candidates", 
                           h4("Statistics on the stock (history) of those selected:"),
                           # Stock of selected women and men
                           fluidRow(column(6, plotlyOutput("stockplot", height="50%"))),
                           # Share of women in the stock of selected candidates and Histogram of share of women in the stock of selected candidates
                           fluidRow(column(6, plotlyOutput("stock_composition", height="50%")),
                                    column(6, plotlyOutput("stock_composition_bar", height="50%"))),
                           h4("Statistics on the selection at each draw:"),
                           # Avg. rank and share selecting the best  
                           fluidRow(column(6, plotlyOutput("share_best", height="50%")),
                                    column(6, plotlyOutput("avg_rank", height="50%"))),                           
                           # Probability of selecting best candidate 
                           fluidRow(column(9, plotlyOutput("prob_best", height="50%")))
                           
                  ),
                  # Fourth tab
                  tabPanel("About the Urn & AA",
                           # Replacement matrix
                           uiOutput("matrix"),
                           # End of AA 
                           fluidRow(column(9, plotlyOutput("hist_firstend", height="50%")))
                  )
                )
              ))
   )
  
  # Data page
 # tabPanel("Data Tools",           
  #         sidebarLayout( 
  #           sidebarPanel(
  #             h3("Data Analysis"),
  #             fluidRow(
  #               column(9, selectInput("datasource", "Data Source", choices=c("NSF")))
  #             )
  #           ),
  #           mainPanel()
  #           ))
             
  # Set the default tab to be the Simulations tab
   , selected ="Simulations"))

##############################################################
#                          Server
#             Controls all of the dynamic output
##############################################################



server <- function(input, output){
  # Matrix size for multidraws 
  output$matrixIn <- renderUI({
    possibledraws <- combn(c(rep("W", input$num_draws), rep("M", input$num_draws)), input$num_draws) 
    possibledraws <- apply(possibledraws, 2, function(x) paste(sort(x,decreasing=T), collapse = "")) %>% unique()
    defaultvalueW <- str_count(possibledraws, "W")
    defaultvalueM <- str_count(possibledraws, "M")
    defaultmat <-  matrix(cbind(defaultvalueW, defaultvalueM) , nrow=length(possibledraws), ncol=2, dimnames=list(possibledraws, c("Add||W", "Add||M")))
    
    matrixInput("multi_matrix", rows = list(names=TRUE), cols=list(names=TRUE, multiheader=TRUE),value=defaultmat)
  })
  
  
  list_output<- reactive ({
    
    # Trigger to rerun
    input$rerun
    
    # Isolate forces it not to refresh until the rerun button is pressed
    isolate({
      
      # Set the random number seed
      set.seed(input$seed)
      
      # Set the initial urn contents
      w_0 <- input$w_0
      m_0 <- input$m_0
      
      # Number of urns
      I <-input$I
      # Trials for each urn
      N <- input$N
      
      # Initialize some data frames we will store data in 
      paths_w_n <- data.frame(matrix(nrow=N+1,ncol=I))
      colnames(paths_w_n) <- paste0("Urn", seq(1:I))
      paths_m_n <- data.frame(matrix(nrow=N+1,ncol=I))
      colnames(paths_m_n) <- paste0("Urn", seq(1:I))
      paths_ratio <- matrix(nrow=N+1,ncol=I)
      
      paths_prob_w_n <- data.frame(matrix(nrow=N,ncol=I))
      colnames(paths_prob_w_n) <- paste0("Urn", seq(1:I))
      paths_prob_w_w_replace_n <- data.frame(matrix(nrow=N,ncol=I))
      colnames(paths_prob_w_w_replace_n) <- paste0("Urn", seq(1:I))
      paths_prob_w_m_replace_n <- data.frame(matrix(nrow=N,ncol=I))
      colnames(paths_prob_w_m_replace_n) <- paste0("Urn", seq(1:I))
      
      paths_selected <- data.frame(matrix(nrow=N,ncol=I))
      colnames(paths_selected) <- paste0("Urn", seq(1:I))    
      paths_selected_rank <- data.frame(matrix(nrow=N,ncol=I))
      colnames(paths_selected_rank) <- paste0("Urn", seq(1:I))      
      paths_selected_w <- data.frame(matrix(nrow=N,ncol=I))
      colnames(paths_selected_w) <- paste0("Urn", seq(1:I))    
      paths_selected_m <- data.frame(matrix(nrow=N,ncol=I))
      colnames(paths_selected_m) <- paste0("Urn", seq(1:I))    
      
      # Some options for AA 
      
      stopintervention <- ifelse(input$intervention == "none", "na", input$stopintervention)
      
      # Set parameters for stochastic balanced replacement
      p_w_w <- ifelse(input$woman_stochastic=="none", 1, input$p_w_w)
      p_w_m <- ifelse(input$woman_stochastic=="none", 1, input$p_w_m)
      p_m_m <- ifelse(input$man_stochastic=="balanced", 1-input$p_w_m, ifelse(input$man_stochastic == "none", 1, input$p_m_m))
      p_m_w <- ifelse(input$woman_stochastic=="balanced", 1-input$p_w_w, ifelse(input$woman_stochastic == "none", 1, input$p_m_w))
      
      # Set conditions to be able to handle removing balls
      w_w_added <- ifelse(input$w_w >=0, input$w_w, 0)
      m_w_added <- ifelse(input$m_w >=0, input$m_w, 0)
      w_m_added <- ifelse(input$w_m >=0, input$w_m, 0)
      m_m_added <- ifelse(input$m_m >=0, input$m_m, 0)
      
      w_w_removed <- ifelse(input$w_w <0, -input$w_w, 0)
      m_w_removed <- ifelse(input$m_w <0, -input$m_w, 0)
      w_m_removed <- ifelse(input$w_m <0, -input$w_m, 0)
      m_m_removed <- ifelse(input$m_m <0, -input$m_m, 0)    
      
      # Add progress bar during the simulations
      withProgress(message = 'Running simulation', value = 0, {
        # Main simulation loop
          
          # Reset the urns to initial state
          # Each column is one urn, one row for each ball 
          urn <- matrix(rep(c(rep("w", w_0),rep("m", m_0)), I), ncol = I, byrow=FALSE)
          
          # Initialize some vectors
          w_n <- rep(w_0, I)
          m_n <- rep(m_0, I)
          prob_w_n <- NULL
          prob_best_n <- NULL
          prob_w_w_replace_n <- NULL
          prob_w_m_replace_n <- NULL
          selected <- NULL
          selected_w <- NULL
          selected_m <- NULL
          selected_rank <- NULL
          end <- NA 
          firstend <- rep(NA, I)
          
          for (n in 1:(N)){
            # Save the previous number of women and men and share
            previous_w <- colSums(urn=="w", na.rm=T)
            previous_m <- colSums(urn=="m", na.rm=T)
            previous_share <- previous_w/(previous_w+previous_m)
            previous_share_selected = if(n == 1) previous_share else selected_w[n-1,]/(selected_w[n-1,] + selected_m[n-1,])
            
            previous_share_avg <- mean(previous_share)
            previous_share_selected_avg <- mean(previous_share_selected)
            
            # Set probability of drawing woman when it depends on urn contents
            if(input$woman_depends=="urn"){
              p_w_w <- if(input$w_w_function=="linear"){input$w_w_c-input$w_w_b * previous_share^input$w_w_a
                } else if(input$w_w_function=="inverse"){1/(1+input$w_w_b*previous_share^input$w_w_a) 
                } else if(input$w_w_function=="inverseexp"){1/(1+input$w_w_b*exp(input$w_w_c * previous_share))
                } else NA
              p_m_w <- if(input$woman_stochastic=="balanced"){1-p_w_w
                } else if(input$m_w_function=="linear"){input$m_w_c-input$m_w_b * (1-previous_share)^input$m_w_a
                } else if(input$m_w_function=="inverse"){1/(1+input$m_w_b*(1-previous_share)^input$m_w_a)
                } else if(input$m_w_function=="inverseexp"){1/(1+input$m_w_b*exp(input$m_w_c * (1-previous_share)) )
                } else NA
              
            }
            
            if(input$man_depends=="urn"){
              p_w_m <- if(input$w_m_function=="linear"){ input$w_m_c-input$w_m_b * previous_share^input$w_m_a 
                          } else if(input$w_m_function=="inverse") {1/(1+input$w_m_b*previous_share^input$w_m_a) 
                          } else if(input$w_m_function=="inverseexp") {1/(1+input$w_m_b*exp(input$w_m_c * previous_share))
                          }  else NA 
                
              p_m_m <- if(input$man_stochastic=="balanced") {1-p_w_m
                          } else if(input$m_m_function=="linear"){input$m_m_c-input$m_m_b * (1-previous_share)^input$m_m_a
                          } else if(input$m_m_function=="inverse"){ 1/(1+input$m_m_b*(1-previous_share)^input$m_m_a)
                          } else if(input$m_m_function=="inverseexp") {1/(1+input$m_m_b*exp(input$m_m_c * (1-previous_share)) )
                          } else NA
              
            }
            if(input$woman_depends=="selected"){
              p_w_w <- if(input$w_w_function=="linear"){input$w_w_c-input$w_w_b * previous_share_selected^input$w_w_a
                          } else if(input$w_w_function=="inverse") {1/(1+input$w_w_b*previous_share_selected^input$w_w_a)
                          } else if(input$w_w_function=="inverseexp") {1/(1+input$w_w_b*exp(input$w_w_c * previous_share_selected))
                          } else NA
              p_m_w <- if(input$woman_stochastic=="balanced"){1-p_w_w
                          } else if (input$m_w_function=="linear"){ input$m_w_c-input$m_w_b * (1-previous_share_selected)^input$m_w_a
                          } else if (input$m_w_function=="inverse"){ 1/(1+input$m_w_b*(1-previous_share_selected)^input$m_w_a)
                          } else if (input$m_w_function=="inverseexp") {1/(1+input$m_w_b*exp(input$m_w_c * (1-previous_share_selected)) )
                          } else NA
              
            }
            
            if(input$man_depends=="selected"){
              p_w_m <- if(input$w_m_function=="linear"){input$w_m_c-input$w_m_b * previous_share_selected^input$w_m_a
                          } else if(input$w_m_function=="inverse") { 1/(1+input$w_m_b*previous_share_selected^input$w_m_a) 
                          } else if(input$w_m_function=="inverseexp") {1/(1+input$w_m_b*exp(input$w_m_c * previous_share_selected))
                          } else NA
              p_m_m <- if(input$man_stochastic=="balanced") {1-p_w_m
                          } else if(input$m_m_function=="linear") {input$m_m_c-input$m_m_b * (1-previous_share_selected)^input$m_m_a
                          } else if(input$m_m_function=="inverse") {1/(1+input$m_m_b*(1-previous_share_selected)^input$m_m_a) 
                          } else if(input$m_m_function=="inverseexp") {1/(1+input$m_m_b*exp(input$m_m_c * (1-previous_share_selected)) )
                          } else NA
              
            }
            
            p_w_w <- if(length(p_w_w)==1) rep(p_w_w, I) else p_w_w
            p_w_m <- if(length(p_w_m)==1) rep(p_w_m, I) else p_w_m
            p_m_w <- if(length(p_m_w)==1) rep(p_m_w, I) else p_m_w
            p_m_m <- if(length(p_m_m)==1) rep(p_m_m, I) else p_m_m
            
            # Conditions for ending of affirmative action
            end <- ifelse(input$intervention=="none" & !is.na(previous_share), NA, 
                     #ifelse(input$intervention=="quota"& input$quota_group == "selected" & (previous_share_selected > input$quota | end %in% 1) & n > input$quota_start,1, 
                        #ifelse(input$intervention=="quota"& input$quota_group == "urn" & (previous_share > input$quota | end %in% 1) & n > input$quota_start,1, 
                          ifelse(stopintervention=="continue" & !is.na(previous_share), 0, 
                            ifelse(stopintervention=="majority" & (previous_share>=input$cutoff | end %in% 1), 1, 
                              ifelse(stopintervention=="majority_selected" & (previous_share_selected >=input$cutoff | end %in% 1)& n > input$aa_start, 1,
                              ifelse(stopintervention=="temp" & n > input$stopafter & !is.na(previous_share), 1, 
                                ifelse(stopintervention=="avg" & (previous_share_avg>=input$cutoff | end %in% 1 ), 1, 
                                  ifelse(stopintervention=="avg_selected" & (previous_share_selected_avg >= input$cutoff | end %in% 1), 1, 0)))))))#))
             
            firstend <- ifelse(is.na(firstend) & end*((stopintervention=="majority"  | stopintervention=="majority_selected" | stopintervention=="avg" | stopintervention=="avg_selected" | input$intervention =="quota")), n , firstend)
            
            # Probability of woman selected
              ### CHECK: WITH OR WITHOUT REPLACEMENT 
            prob_w_selected <- ifelse(input$multidraw=="multi" & input$multi_interp==T & !(end %in% 0),  1 - dhyper(input$num_draws, previous_m, previous_w, input$num_draws), 
                                 ifelse(input$intervention=="atleast" & (end %in% 0) & n > input$aa_start, 1-dhyper(2, previous_m, previous_w, 2), 
                                  ifelse(input$intervention=="atleast_stochastic" & (end %in% 0) & n > input$aa_start, previous_share+(1-previous_share)*(previous_share)*input$prob_atleast,
                                    ifelse(input$intervention=="quota"& (end %in% 0) & n > input$aa_start,1/input$quota_window, previous_share))))
              
            
            # Random draws done ahead of time for the case of balanced replacement
            r_w_w <- sapply(seq(1:I), function(x) rbinom(1,1,p_w_w[x]))
            r_w_m <- sapply(seq(1:I), function(x) rbinom(1,1,p_w_m[x]))
            r_m_w <- if(input$woman_stochastic=="balanced") 1-r_w_w else sapply(seq(1:I), function(x) rbinom(1,1,p_m_w[x]))
            r_m_m <- if(input$man_stochastic=="balanced") 1-r_w_m else sapply(seq(1:I), function(x) rbinom(1,1,p_m_m[x]))
            
            r_exit <- if(input$exit_selected==T) sapply(seq(1:I), function(x) rbinom(1,1,input$prob_exit)) else NULL
            
            rank <- rep(1, I)
            prob_best <- rep(1, I)
            ####
            # Draw, replace, remove balls for each AA case
            
            ## SINGLE DRAW OPTIONS          
            if (input$multidraw=="single" & 1==1){
            #if (input$intervention=="none" | (n < input$quota_start & input$intervention=="quota") | (n < input$aa_start & input$intervention%in% c("atleast", "atleast_stochastic")  )){
              ball_drawn <- apply(urn,2, function(x) sample(na.omit(x),1) )
              #ball_drawn <- ifelse(end %in% 1, NA, ball_drawn)
              
              ball_replaced <- sapply(seq(1:I), function(x){
                rball <- if(ball_drawn[x] == "w") c(rep("w", w_w_added*r_w_w[x]), rep("m", m_w_added*r_m_w[x])) else c(rep("w", w_m_added*r_w_m[x]), rep("m", m_m_added*r_m_m[x]))
                if(is_empty(rball)) 0 else rball
            })
              
              ball_removed <- sapply(seq(1:I), function(x){
                mball <- if(ball_drawn[x] == "w") c(rep("w", w_w_removed*r_w_w[x]), rep("m", m_w_removed*r_m_w[x])) else c(rep("w", w_m_removed*r_w_m[x]), rep("m", m_m_removed*r_m_m[x]))
                if(is_empty(mball)) 0 else mball
              })
            }
            
            if (input$multidraw=="single" & input$intervention=="atleast" & n > input$aa_start){
              ball_drawn_aa <- apply(urn,2, function(x) sample(na.omit(x),2, replace =TRUE) )
              
              rank_aa <- apply(ball_drawn_aa, 2, function(x) ifelse("w" %in% x, min(which(x=="w")), 1) )
              # Prob best = P(MM) + P(WW) + P(WM) 
              # = P(MM) + P(W first) 
              prob_best_aa <- dhyper(2, previous_m, previous_w, 2) + previous_share
              ball_drawn_aa <- apply(ball_drawn_aa, 2, function(x) ifelse("w" %in% x, "w", "m") )
              ball_replaced_aa <- sapply(seq(1:I), function(x){
                rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added*r_w_w[x]), rep("m", m_w_added*r_m_w[x])) else c(rep("w", w_m_added*r_w_m[x]), rep("m", m_m_added*r_m_m[x]))
                if(is_empty(rball)) 0 else rball
              })
              
              ball_removed_aa <- sapply(seq(1:I), function(x){
                mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed*r_w_w[x]), rep("m", m_w_removed*r_m_w[x])) else c(rep("w", w_m_removed*r_w_m[x]), rep("m", m_m_removed*r_m_m[x]))
                if(is_empty(mball)) 0 else mball
              })
            }
            if (input$multidraw=="single" & input$intervention=="atleast_stochastic" & n > input$aa_start){
              ball_drawn_aa <- apply(urn,2, function(x) sample(na.omit(x),2, replace =TRUE) )
              
              rank_aa <- apply(ball_drawn_aa, 2, function(x) ifelse("w" %in% x, min(which(x=="w")), 1) )

              ball_drawn_aa <- apply(ball_drawn_aa, 2, function(x) ifelse( min(which(x=="w"))==1, "w", ifelse(min(which(x=="w"))==2, sample(x, 1, prob=c(1-input$prob_atleast, input$prob_atleast)), "m")))
              
              #rank_aa <- sapply(ball_drawn_aa, function(x) ifelse("m" %in% x,1, rank_aa) )

              ball_replaced_aa <- sapply(seq(1:I), function(x){
                rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added*r_w_w[x]), rep("m", m_w_added*r_m_w[x])) else c(rep("w", w_m_added*r_w_m[x]), rep("m", m_m_added*r_m_m[x]))
                if(is_empty(rball)) 0 else rball
              })
              
              ball_removed_aa <- sapply(seq(1:I), function(x){
                mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed*r_w_w[x]), rep("m", m_w_removed*r_m_w[x])) else c(rep("w", w_m_removed*r_w_m[x]), rep("m", m_m_removed*r_m_m[x]))
                if(is_empty(mball)) 0 else mball
              })
            }        
            if (input$multidraw=="single" & input$intervention=="alwayswoman" ){
              ball_drawn_aa <- apply(urn,2, function(x) sample(na.omit(x),1) )
              
              rank_aa <- 1
              
              ball_replaced_aa <- sapply(seq(1:I), function(x){
                rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added*r_w_w[x]), rep("m", m_w_added*r_m_w[x])) else c(rep("w", w_m_added*r_w_m[x]), rep("m", m_m_added*r_m_m[x]))
                #add one woman to the count
                if(is_empty(rball)) "w" else c(rball, "w")
              })
              ball_removed_aa <- sapply(seq(1:I), function(x){
                mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed*r_w_w[x]), rep("m", m_w_removed*r_m_w[x])) else c(rep("w", w_m_removed*r_w_m[x]), rep("m", m_m_removed*r_m_m[x]))
                if(is_empty(mball)) 0 else mball
              })
            }        
            if (input$multidraw=="single" & input$intervention=="quota" & n>input$aa_start){
              
              # Quota with window > 1: selection strategy
              # Need to select at least one W every k draws
              # Select W in this round if min(rank of W) < expected rank of W if choosing M this round
              
              # Check if we still have free choices 
              # Hire W every round = no free choices 
              # Hire W every two = free choice in round 1, free choice in round 2 only if W in round 1
              # Otherwise, count previous W in window 
              draw_in_window <- input$quota_window - n %% input$quota_window
              if (draw_in_window > 1) {
              w_so_far <- apply(urn, 2, function(x) sum(x[(length(x)-(draw_in_window-2)):length(x)]=="w"))
              }
              else {
              w_so_far <- rep(0, ncol(urn))
              }
              free_choice <-(n %% input$quota_window==input$quota_per - w_so_far )
              free_choice <- ifelse(rep(draw_in_window, ncol(urn)) == 1 & rep(input$quota_window, ncol(urn)) > 1, T, free_choice)

                # Forecast expected value
                forecast_urn <- rbind(urn, rep("m", ncol(urn)))
                
                # Bootstrap if you want, but it's slow 
                find_best_W <- function(forecast_urn){
                  forecast_draws <- apply(forecast_urn,2, function(x) sample(x,nrow(forecast_urn)))
                  rank <- apply(forecast_draws, 2, function(x) min(which(x=="w")) )
                  return(rank)
                }
                #forecast_rank <- t(replicate(20, find_best_W(forecast_urn)))
                #expected_rank_W <- apply(forecast_rank, 2, function(x) mean(forecast_rank[, x]))
                
                # Otherwise, use the formula 
                m <- previous_m + 1
                w <- previous_w 
                numer <- function(s, w, m) {
                  prod(sapply(0:max(s-2, 0), function(k) max(0, m - k))) 

                  }
                denom <- function(s, w, m) {
                  prod(sapply(0:max(s-1, 0), function(j) w+m-j)) 
                }
                sum1 <- sapply(seq(1:ncol(urn)), function(x) 1 + sum( sapply(seq(1:(m[x]+1)), function(z) z*(dhyper(0, w[x], m[x], z) - dhyper(0, w[x], m[x], z+1)) )))
                  
                  #(w[x]/(w[x]+m[x])) + sum(sapply(2:(m[x]+1), function(s) (w[x])*s*numer(s, w[x], m[x])/denom(s, w[x], m[x]))))
                expected_rank_W <- sum1
                
                # Compare to best rank of W this round 
                draws_aa  <- apply(urn,2, function(x) sample(x,nrow(urn)) )
                rank_aa <- apply(draws_aa, 2, function(x) min(which(x=="w")) )
                
                draw_aa <- ifelse(rank_aa <= expected_rank_W, rank_aa, 1)
                # If choice is not free, replace it with W 
                draw_aa <- ifelse(free_choice==FALSE, rank_aa, draw_aa)
                
                ball_drawn_aa <- sapply(seq(1:ncol(urn)), function(x) draws_aa[draw_aa[x],x])
              
                rank_aa <- draw_aa

              # Prob best this round = prob(W best) + free_choice*prob(choose M this round) 
              prob_best_aa <- previous_share + sapply(seq(1:ncol(urn)), function(x) 1- dhyper(0, previous_w[x], previous_m[x], floor(expected_rank_W[x])))*sapply(seq(1:ncol(urn)), function(x) 1- dhyper(0, previous_w[x], previous_m[x], floor(expected_rank_W[x])))
            
              ball_replaced_aa <- sapply(seq(1:I), function(x){
                rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added*r_w_w[x]), rep("m", m_w_added*r_m_w[x])) else c(rep("w", w_m_added*r_w_m[x]), rep("m", m_m_added*r_m_m[x]))
                if(is_empty(rball)) 0 else rball
              })
              
              ball_removed_aa <- sapply(seq(1:I), function(x){
                mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed*r_w_w[x]), rep("m", m_w_removed*r_m_w[x])) else c(rep("w", w_m_removed*r_w_m[x]), rep("m", m_m_removed*r_m_m[x]))
                if(is_empty(mball)) 0 else mball
              })
              
            }
            
            ## MULTIPLE DRAW OPTIONS
            if (input$multidraw=="multi"){
              ball_drawn <- sapply(seq(1:I), function(x) sample(na.omit(urn[,x]),input$num_draws) ) 
              
              if ("character" %in% class(ball_drawn)) ball_drawn <- matrix(ball_drawn, ncol=I)

              ball_replaced <- lapply(seq(1:I), function(x){
                total_W = ifelse(input$num_draws ==1,str_count(ball_drawn[,x], "w"), str_count(paste(ball_drawn[,x], collapse=""), "w")) 
                total_M = ifelse(input$num_draws ==1,str_count(ball_drawn[,x], "m"), str_count(paste(ball_drawn[,x], collapse=""), "m")) 
                # total W is the matrix row index for the input replacement matrix
                rball <- c(rep("w", as.numeric(input$multi_matrix[total_M+1, 1])), rep("m", as.numeric(input$multi_matrix[total_M+1, 2])))
                if(is_empty(rball)) 0 else rball
              })
              
              if (input$multi_interp == TRUE){
                rank <- apply(ball_drawn, 2, function(x) ifelse("w" %in% x, min(which(x=="w")), 1 ))
                # Prob best
                # P(W first) = previous_share
                # P(M only) = (1-previous_share)^2
                prob_best <- dhyper(input$num_draws, previous_m, previous_w, input$num_draws) + previous_share
                
              }
              
              # Not currently used--removals not allowed for multiple draw 
              ball_removed <- lapply(seq(1:I), function(x){
                mball <- if("w" %in% ball_drawn[,x]) c(rep("w", w_w_removed*r_w_w[x]), rep("m", m_w_removed*r_m_w[x])) else c(rep("w", w_m_removed*r_w_m[x]), rep("m", m_m_removed*r_m_m[x]))
                if(is_empty(mball)) 0 else mball
              })
            }
            
            # For urns undergoing AA, use those draws instead 
            if (input$multidraw=="single" & input$intervention != "none" & input$intervention!="quota" & n > input$aa_start | (input$multidraw=="single" & input$intervention=="quota" & n > input$aa_start) ){
            ball_drawn <- ifelse(end %in% 0, ball_drawn_aa, ball_drawn)
            ball_replaced_aa <- if(!("list" %in% class(ball_replaced_aa))) sapply(ball_replaced_aa, list) else ball_replaced_aa
            ball_replaced <- sapply(seq(1:I), function(x) if(end[x] %in% 0) ball_replaced_aa[[x]] else ball_replaced[x])
            prob_best <- sapply(seq(1:I), function(x) if(end[x] %in% 0) prob_best_aa[[x]] else prob_best[x])
            
            ball_removed_aa <- if(!("list" %in% class(ball_removed_aa))) sapply(ball_removed_aa, list) else ball_removed_aa
            ball_removed <- sapply(seq(1:I), function(x) if(end[x] %in% 0) ball_removed_aa[[x]] else ball_removed[x])
            
            rank <- ifelse(end %in% 0, rank_aa, rank)
            
            }
            
            # For urns with exit, incorporate that 
            if (input$exit_selected == T & n > 1) {
              list.selected <- split(t(selected), seq(nrow(t(selected)))) 
              list.selected <- lapply(list.selected, function(x) x[!is.na(x)])
              ball_exit_selected <- sapply(seq(1:I), function(x) if(r_exit[x] == 1) list.selected[[x]][1] else NA ) %>% unlist
              list.selected <- sapply(seq(1:I), function(x) if(r_exit[x] == 1) {
                if(length(list.selected[[x]])==0) NA else list.selected[[x]][-1] }
                else list.selected[[x]] )
              list.selected <- lapply(list.selected, function(x) if(length(x) < n) c(x, rep(NA, n-length(x)-1)) else x)
              selected <- list.selected %>% unlist() %>% matrix(nrow=n-1, ncol = I)
            }  
            
            # Save results 
            new_w <- sapply(seq(1:I), function(x) previous_w[x] + sum(ball_replaced[[x]]=="w") - sum(ball_removed[[x]]=="w"))
            new_w <- ifelse(new_w <0, 0, new_w)
            new_m <- sapply(seq(1:I), function(x) previous_m[x] + sum(ball_replaced[[x]]=="m") - sum(ball_removed[[x]]=="m"))
            new_m <- ifelse(new_m<0, 0 , new_m)
            
            add_w <- sapply(seq(1:I), function(x) sum(ball_replaced[[x]]=="w") - sum(ball_removed[[x]]=="w"))
            add_w <- ifelse(add_w <0, 0, add_w)
            add_m <- sapply(seq(1:I), function(x) sum(ball_replaced[[x]]=="m") - sum(ball_removed[[x]]=="m"))
            add_m <- ifelse(add_m<0, 0 , add_m)
            
            add <- sapply(seq(1:length(add_w)), function(x) c(rep("w", add_w[x]), rep("m", add_m[x])))
            
            #urn_l <- lapply(seq(1:I), function(x) matrix(c(rep("w", new_w[x]), rep("m", new_m[x])), ncol=1) )
            #urn <- matrix(nrow= max(unlist(lapply(urn_l, nrow))), ncol=I)
            urn <- sapply(seq(1:ncol(urn)), function(x) c(urn[,x], add[x]))
            
            #for (i in 1:I) {
            #  urn[1:length(urn_l[[i]]),i] <- urn_l[[i]]
            #}
            
            # For urns with multidraw intervention interpretation, change how "selected" is defined
            ball_selected <- ball_drawn 
            if(input$multidraw == "multi" & input$multi_interp == T){
              ball_selected <- ball_replaced
            }
            
            selected <- rbind(selected, ball_selected)
            selected_rank <- if(n > 1) rbind(selected_rank, rank) else rank
            
            if (class(selected[1,1]) == "character"){
            selected_w <- rbind(selected_w, sapply(seq(1:I), function(x) sum(selected[, x]=="w")))
            selected_m <- rbind(selected_m, sapply(seq(1:I), function(x) sum(selected[, x]=="m")))
            }
            
            if (class(selected[1,1]) == "list"){
              selected_w <- rbind(selected_w, sapply(seq(1:I), function(x) sum(unlist(selected[, x]) =="w")))
              selected_m <- rbind(selected_m, sapply(seq(1:I), function(x) sum(unlist(selected[, x])=="m")))
            }
            
            w_n <- rbind(w_n, new_w)
            m_n <- rbind(m_n, new_m)
            prob_w_n <- rbind(prob_w_n, prob_w_selected)
            prob_best_n <- rbind(prob_best_n, prob_best)
            prob_w_w_replace_n<- rbind(prob_w_w_replace_n, p_w_w)
            prob_w_m_replace_n<- rbind(prob_w_m_replace_n, p_w_m)
            

            incProgress(1/N, detail = paste("Percent completed", round(n*100/N, 1), "%"))
            
            }
            
          # Output all the results 
            
          paths_w_n <- w_n
          colnames(paths_w_n) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_w_n) <- NULL

          paths_m_n <- m_n
          colnames(paths_m_n) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_m_n) <- NULL
          
          paths_ratio <- sapply(seq(1:I), function(x) w_n[,x]/ (m_n[,x] + w_n[,x]))
          
          paths_prob_w_n <- prob_w_n
          colnames(paths_prob_w_n) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_prob_w_n) <- NULL
          rm(w_n,m_n)
          
          paths_prob_best <- prob_best_n
          colnames(paths_prob_best) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_prob_best) <- NULL

          paths_prob_w_w_replace_n <- prob_w_w_replace_n
          colnames(paths_prob_w_w_replace_n) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_prob_w_w_replace_n) <- NULL

          paths_prob_w_m_replace_n <- prob_w_m_replace_n
          colnames(paths_prob_w_m_replace_n) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_prob_w_m_replace_n) <- NULL     
          rm(prob_w_w_replace_n, prob_w_m_replace_n)
          
          paths_selected <- selected
          colnames(paths_selected) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_selected) <- NULL    

          paths_selected_rank <- selected_rank
          colnames(paths_selected_rank) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_selected_rank) <- NULL  
          
          paths_selected_w <- selected_w
          colnames(paths_selected_w) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_selected_w) <- NULL     
          
          paths_selected_m <- selected_m
          colnames(paths_selected_m) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_selected_m) <- NULL            
        
          rm(selected, selected_rank, selected_w, selected_m)
          

      })  

      # Save the urn functions for probability later
      w_w_function_t <- if(input$woman_depends=="none"){
        paste0("X \\sim Bern(", p_w_w, ")")
      }
      else if(input$w_w_function=="linear"){
        paste0("X \\sim Bern(1-", input$w_w_b, "*share_w^", input$w_w_a, ")")
      }
      else if(input$w_w_function=="inverse"){
        paste0("X \\sim Bern(\\frac{1}{1+", input$w_w_b, "*(share_w)^", input$w_w_a, " } )")
      }
      else if(input$w_w_function=="inverseexp"){
        paste0("X \\sim Bern(\\frac{1}{1+", input$w_w_b, "*\\exp(", input$w_w_c, "*share_w)} )")
      }
      
      m_w_function_t <- if(input$woman_depends=="none" & (input$woman_stochastic=="balanced" | input$woman_stochastic=="none")){
        paste0("1-X")
      }
      else if(input$woman_depends=="none" & input$woman_stochastic=="unbalanced") {
        paste0("Bern(", p_m_w, ")")
      }
      else if(input$woman_depends=="urn") {
        if (input$woman_stochastic=="balanced") {
          paste0("1-X")
          
        }
        else if(input$m_w_function=="linear") {
          paste0("1-", input$m_w_b, "*share_m^", input$m_w_a)
        }
        else if(input$m_w_function=="inverse") {
          paste0("\\frac{1}{1+", input$m_w_b, "*(share_m)^", input$m_w_a, " }")
        }
        else if(input$m_w_function=="inverseexp"){
          paste0("\\frac{1}{1+", input$m_w_b, "*\\exp(", input$m_w_c, "*share_m)}")
        }    
      }
      
      
      w_m_function_t <- if(input$man_depends=="none"){
        paste0("Y \\sim Bern(", p_w_m, ")")
      }
      else if(input$w_m_function=="linear"){
        paste0("Y \\sim Bern(1-", input$w_m_b, "*share_w^", input$w_m_a, ")")
      }
      else if(input$w_m_function=="inverse"){
        paste0("Y \\sim Bern(\\frac{1}{1+", input$w_m_b, "*(share_w)^", input$w_m_a, " } )")
      }
      else if(input$w_m_function=="inverseexp"){
        paste0("Y \\sim Bern(\\frac{1}{1+", input$w_m_b, "*\\exp(", input$w_m_c, "*share_w)} )")
      }
      
      m_m_function_t <- if(input$man_depends=="none" & input$man_stochastic=="balanced"){
        paste0("1-Y")
      }
      else if(input$man_depends=="none" & input$man_stochastic=="unbalanced") {
        paste0("Y \\sim Bern(", p_m_m, ")")
      }
      else if(input$man_depends=="urn") {
        if (input$man_stochastic=="balanced") {
          paste0("1-Y")
          
        }
        else if(input$m_m_function=="linear") {
          paste0("1-", input$m_m_b, "*share_m^", input$m_m_a)
        }
        else if(input$m_m_function=="inverse") {
          paste0("\\frac{1}{1+", input$m_m_b, "*(share_m)^", input$m_m_a, " }")
        }
        else if(input$m_m_function=="inverseexp"){
          paste0("\\frac{1}{1+", input$m_m_b, "*\\exp(", input$m_m_c, "*share_m)}")
        }    
      }    
      

      # Create a vector of the parameters to save for later
      parameters <- list("p_w_w"=p_w_w, "p_w_m"=p_w_m, "p_m_m"=p_m_m, "p_m_w"=p_m_w, 
                         "w_w_added"=w_w_added, "w_m_added"=w_m_added, "m_w_added"=m_w_added, "m_m_added"=m_m_added, 
                         "w_w_removed"=w_w_removed, "w_m_removed"=w_m_removed, "m_w_removed"=m_w_removed, "m_m_removed"=m_m_removed, 
                         "w_w_function" = w_w_function_t, "w_m_function"=w_m_function_t, "m_w_function"=m_w_function_t, "m_m_function"=m_m_function_t)
      # Create a list with all the outputs
      outputlist <- list(paths_ratio=paths_ratio, paths_w_n=paths_w_n, paths_m_n=paths_m_n, paths_prob_w_w_replace_n=paths_prob_w_w_replace_n, paths_prob_w_m_replace_n=paths_prob_w_m_replace_n, paths_prob_w_n=paths_prob_w_n, paths_selected=paths_selected, paths_selected_rank=paths_selected_rank, paths_selected_w=paths_selected_w, paths_selected_m=paths_selected_m, parameters=parameters, firstend=firstend, paths_prob_best=paths_prob_best)
      return(outputlist)
      
    })
    
    
  })
  
  ### Histogram 
  output$histogram <- renderPlotly({
    
    input$rerun
    
    isolate({  
      # Get the ratios and prepare data
      outputlist <- list_output()
      paths_ratio <- outputlist$paths_ratio
      hist_data <- as.data.frame(paths_ratio[nrow(paths_ratio),])
      colnames(hist_data) <- "Share of white balls in urn after trials"
      
      # Plot
      bin.width <- 1/(sqrt(input$N))

      hist <- ggplot(hist_data) + 
        geom_histogram(aes(x=`Share of white balls in urn after trials`), bins=sqrt(input$N), fill="gray")+
        geom_density(aes(x=`Share of white balls in urn after trials`, y =after_stat(count*bin.width)), color="#18bc9c")+
        scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) + 
        labs(title ="Distribution of final share of white balls in the urn")
     
      g <- ggplotly(hist) %>%
        layout(xaxis=list(title = "Share of white balls in the urn after trials", range=c(0,1)), yaxis=list(title="Frequency", titlefont = list(size = 16)))
      
      text_y <- paste(g$x$data[[1]]$y, 'urns have white ball <br>share', round(g$x$data[[1]]$x, 2), "-", round(g$x$data[[1]]$x+g$x$data[[1]]$width,2))
                      
      g %>% style(text=text_y, traces =1) %>%
        layout(hovermode="x") %>%
        style(text=NA, hoverInfo="skip", traces=2)
      
      
    })
    
  })
  
  ### Density plots
  output$density <- renderPlotly({
    
    input$rerun
    
    isolate({  
      
      # Get and prepare data
      outputlist <- list_output()
      paths_ratio <- outputlist$paths_ratio
      hist_data <- as.data.frame(paths_ratio[nrow(paths_ratio),])
      colnames(hist_data) <- "Share of white balls in urn after trials"
      
      # Plot
      density <- ggplot(hist_data) + 
        geom_density(aes(x=`Share of white balls in urn after trials`))+
        scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) + 
        labs(title ="Distribution of final share of white balls")
      
      plot_ly(x = ~density(hist_data$`Share of white balls in urn after trials`)$x, y = ~density(hist_data$`Share of white balls in urn after trials`)$y, type = 'scatter', mode = 'lines')  %>%
        layout(xaxis=list(title = "Share of white balls in the urn after trials", range=c(0,1)), yaxis=list(title="Density"), hovermode="x unified)")
      
    })
    
  })
  
  ### CDF plot
  output$cdf <- renderPlotly({
    
    input$rerun
    
    isolate({  
      
      # Get and prepare data
      outputlist <- list_output()
      paths_ratio <- outputlist$paths_ratio
      hist_data <- as.data.frame(paths_ratio[nrow(paths_ratio),])
      colnames(hist_data) <- "Share of white balls in urn after trials"
      hist_data <- arrange(hist_data, `Share of white balls in urn after trials`)  

      # Plot
      cdf <- ggplot(hist_data) + 
        stat_ecdf(aes(x=`Share of white balls in urn after trials`, text=paste0(..y.. * 100, '% of urns have less than<br>', round(..x.., 2)*100, '% white balls')), geom="step")+
        geom_vline(xintercept=0.5, color="chartreuse3", linetype="dashed", alpha=0.5)+
        scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=12)
        ) + 
        labs(y="Fraction of urns", title ="CDF of final share of white balls in the urn")
      
      ggplotly(cdf, tooltip="text") %>% layout(hovermode="x unified)", 
                               yaxis = list(hoverformat = '.2f'), 
                               xaxis = list(hoverformat = '.2f')) 
      #%>%
        #style(hovertemplate = paste('y: %{y:.2f}','<br>Share of white balls: %{x:.4f}<br>'), traces = 1) %>%
        #style(hoverinfo="skip", traces = 2)
      
    })
    
  })
  
  ### Share of women over time plot
  output$ratio_over_time <- renderPlotly({
    input$rerun
    
    isolate({
      
      # Get and prepare data
      outputlist <- list_output()
      paths_ratio <- outputlist$paths_ratio
      paths_ratio <- paths_ratio %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio <- paths_ratio %>% pivot_longer(-draw)
      average_ratio <- paths_ratio %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text1 <- paste0('After draw ', paths_ratio$draw -1,', avg. urn has<br>',round(average_ratio, digits=4)* 100, '% white balls')
      
      # Plot
      r <- ggplot() + 
        geom_line(data=ratio, aes(x=draw-1, y=value, group=name), color="lightgray") +
        geom_point(aes(x=rep(0:(input$N)), y=average_ratio, text=text1), size=0.1, color="blue") +
        geom_line(aes(x=rep(0:(input$N)), y=average_ratio), color="blue") +
        geom_hline(aes(yintercept=0.5), color="chartreuse3",linetype = "dashed") +
        scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=12)) + 
        labs(title ="Share of white balls in urn over time, average highlighted", x="Draw", y="White balls' share in the urn ($r_t$)")
      
      
      
      ggplotly(r, tooltip="text") %>%
        layout(hovermode="x unified)")
       
    })
    
  })
  
  ### Share of women in stock over time plot
  output$ratio_s_over_time <- renderPlotly({
    input$rerun
    
    isolate({
      
      # Get and prepare data
      outputlist <- list_output()
      paths_selected_w <- outputlist$paths_selected_w
      paths_selected_m <- outputlist$paths_selected_m
      paths_selected_ratio <- paths_selected_w/(paths_selected_w + paths_selected_m)
      
      paths_selected_ratio <- paths_selected_ratio %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio <- paths_selected_ratio %>% pivot_longer(-draw)
      average_ratio <- paths_selected_ratio %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text1 <- paste0('After draw ', paths_selected_ratio$draw ,', avg. pool of selected has<br>',round(average_ratio, digits=4)* 100, '% white balls')
      
      # Plot
      r <- ggplot() + 
        geom_line(data=ratio, aes(x=draw-1, y=value, group=name), color="lightgray") +
        geom_point(aes(x=rep(1:(input$N)), y=average_ratio, text=text1), size=0.1, color="purple") +
        geom_line(aes(x=rep(1:(input$N)), y=average_ratio), color="purple") +
        geom_hline(aes(yintercept=0.5), color="chartreuse3",linetype = "dashed") +
        scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=12)
        ) + 
        labs(title ="Share of white balls among selected, average highlighted", x="Draw", y="White balls' share among selected ($\\rho_t$)")
      
      
      
      ggplotly(r, tooltip="text") %>%
        layout(hovermode="x unified)")
      
    })
    
  })
  
  ### Probability of selecting a woman over time
  output$prob_w_over_time <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare the data
      outputlist <- list_output()
      paths_prob_w_n <- outputlist$paths_prob_w_n %>% as.data.frame
      paths_prob_w_n <- paths_prob_w_n  %>% mutate(draw=row_number()) 
      prob_w_n <- paths_prob_w_n %>% pivot_longer(-draw)
      average_prob_w <- paths_prob_w_n %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      
      # Plot
      r <- ggplot() + 
        geom_line(data=prob_w_n, aes(x=draw, y=value, group=name), color="lightgray") +
        geom_line(aes(x=rep(1:(input$N)), y=average_prob_w), color="blue") +
        geom_hline(aes(yintercept=0.5), color="chartreuse3",linetype = "dashed" ) +
        scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) + 
        labs(title ="Probability of selecting a white ball, average highlighted", y="Probability of selecting a white ball", x="Draw")
      
      ggplotly(r) %>% style(hoverinfo = "skip", traces = c(1,3)) %>%
        style(hovertemplate = paste('At draw %{x:.0f},',
                                    '<br>avg. Prob(select white ball) = %{y:.2%}<br><extra></extra>'), traces = 2) %>%
        layout(hovermode="x unified)")
    })
    
  })
  
  ### Number of women and men in urn over time
  output$rayplot <- renderPlotly({
    
    input$rerun
    
    isolate({
      
      # Get and prepare data
      outputlist <- list_output()
      paths_w_n <- outputlist$paths_w_n %>% as.data.frame
      paths_m_n <- outputlist$paths_m_n %>% as.data.frame
      
      # Prepare the rays
      w_n_long <- mutate(paths_w_n, draw = row_number()) %>% pivot_longer(cols=starts_with("Urn"), names_to="Urn")
      colnames(w_n_long)[3] <- "W"
      m_n_long <- pivot_longer(paths_m_n, cols=starts_with("Urn"), names_to="Urn")
      colnames(m_n_long)[2] <- "M"
      
      ray_data <- cbind(w_n_long, m_n_long$M)
      colnames(ray_data)[4] <- "M"  
      
      # Get the graph limits
      limits<-c(ifelse(input$graph_auto=="auto", min(input$w_0, input$m_0), input$graph_origin), ifelse(input$graph_auto=="auto", input$N + max(input$w_0, input$m_0), input$graph_dim))
      
      # Plot
      rays <- ggplot(ray_data) +
        stat_density_2d(aes(x=W, y=M, fill=..density.., alpha = sqrt(..density..)), geom = "raster", contour = FALSE) +
        geom_line(aes(x=W, y=M, group=Urn, text=paste("Draw:", as.character(draw))),color="black", size = 0.1, alpha=min(1, 50/input$I)) +
        geom_abline(aes(slope=1, intercept=0), color="chartreuse3", linetype="dashed") +
        scale_fill_distiller(palette= "Spectral", direction=-1) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Number of white balls", y="Number of maroon balls", title = "Evolution of urn contents")
      
      
      rays <- if(input$graph_auto=="auto") rays + scale_x_continuous() + scale_y_continuous() else rays + scale_x_continuous(limits=limits) + scale_y_continuous(limits=limits)
      
      
      ggplotly(rays, tooltip=c("x", "y", "text", "Urn"))
      
    })
    
  })
  
  ### Stock of men and women selected over time
  output$stockplot <- renderPlotly({
    
    input$rerun
    
    isolate({
      
      # Get and prepare data
      outputlist <- list_output()
      paths_selected <- outputlist$paths_selected %>% as.data.frame
      paths_selected_w <- outputlist$paths_selected_w %>% as.data.frame
      paths_selected_m <- outputlist$paths_selected_m %>% as.data.frame
      
      
      # Prepare the rays
      w_n_long <- mutate(paths_selected_w, draw=row_number()) %>% pivot_longer(cols=starts_with("Urn"), names_to="Urn") 
      colnames(w_n_long)[3] <- "W"
      m_n_long <- pivot_longer(paths_selected_m, cols=starts_with("Urn"), names_to="Urn")
      colnames(m_n_long)[2] <- "M"
      
      stock_data <- cbind(w_n_long, m_n_long$M)
      colnames(stock_data)[4] <- "M"  
      stock_data[is.na(stock_data)]<- 0
      average <- stock_data %>% as.data.frame() %>% group_by(draw) %>% summarize(mean_w = mean(W), mean_m = mean(M))
      
      # Get the graph limits
      limits<-c(ifelse(input$graph_auto=="auto", min(input$w_0, input$m_0), input$graph_origin), ifelse(input$graph_auto=="auto", input$N + max(input$w_0, input$m_0), input$graph_dim))
      
      # Plot
      stock <- ggplot() +
        geom_line(data=stock_data, aes(x=W, y=M, group=Urn),size = 0.1, alpha=min(1, 50/input$I)) +
        geom_line(data=average, aes(x=mean_w, y=mean_m),color="blue") +
        geom_abline(aes(slope=1, intercept=0), color="chartreuse3", linetype="dashed") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Number of white balls selected", y="Number of maroon balls selected",title = "Average highlighted in blue")
     
       stock <- if(input$graph_auto=="auto") stock + scale_x_continuous() + scale_y_continuous() else stock + scale_x_continuous(limits=limits) + scale_y_continuous(limits=limits)
      
      
      ggplotly(stock) %>%
        layout(hovermode="x unified)") %>%
        style(hovertemplate=paste('Avg. maroon: %{y}<br>', 'Avg. white: %{x} <extra></extra>'), traces=2) %>%
        style(hoverinfo="skip", traces=c(1,3))

      
    })
    
  })
  
  
  ### Graph of share of women in the stock selected
  output$stock_composition <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist <- list_output()
      paths_selected_rank <- outputlist$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(name) %>% arrange(draw) %>% mutate(count_best=cumsum(value==1), share_best=cumsum(value==1)/draw) %>% ungroup()
      
      average <- paths_selected_rank %>% group_by(draw) %>% summarize(mean_share = mean(share_best))
      
      # Plot
      stock <- ggplot(paths_selected_rank) +
        geom_line(aes(x=draw, y=share_best, group=name),size = 0.1, alpha=min(1, 50/input$I)) +
        geom_line(data=average, aes(x=draw, y=mean_share), color="blue") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Share of selected that are the best candidate")
      
      ggplotly(stock) %>% style(hoverinfo = "skip", traces = 1) %>%
        style(hovertemplate = paste('After draw %{x:.0f},',
                                    '<br>%{y:.0%} are the best candidate<br><extra></extra>'), traces = 2) %>%
        layout(hovermode="x unified)")
      
      
    })
    
  })
  
  ### Graph of share of women in the stock selected
  output$stock_composition <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist <- list_output()
      paths_selected_rank <- outputlist$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(name) %>% arrange(draw) %>% mutate(count_best=cumsum(value==1), share_best=cumsum(value==1)/draw) %>% ungroup()
      
      average <- paths_selected_rank %>% group_by(draw) %>% summarize(mean_share = mean(share_best))
      
      # Plot
      stock <- ggplot(paths_selected_rank) +
        geom_line(aes(x=draw, y=share_best, group=name),size = 0.1, alpha=min(1, 50/input$I)) +
        geom_line(data=average, aes(x=draw, y=mean_share), color="blue") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Share of selected that are the best candidate")
      
      ggplotly(stock) %>% style(hoverinfo = "skip", traces = 1) %>%
        style(hovertemplate = paste('After draw %{x:.0f},',
                                    '<br>%{y:.0%} are the best candidate<br><extra></extra>'), traces = 2) %>%
        layout(hovermode="x unified)")
          })
  })
  
  ## Graph of share of urns choosing best candidate
  output$share_best <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist <- list_output()
      paths_selected_rank <- outputlist$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% summarize(count_best=sum(value==1), share_best=sum(value==1)/input$I) %>% ungroup()
      
      # Plot
      stock <- ggplot(paths_selected_rank) +
        geom_line(aes(x=draw, y=share_best), color="blue") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Share of selected that are the best candidate")
      
      ggplotly(stock) %>%
        style(hovertemplate = paste('At draw %{x:.0f},',
                                    '<br>%{y:.0%} of urns select the best candidate<br><extra></extra>'), traces = 1) %>%
        layout(hovermode="x unified)")
    })
  })  
  
  ## Graph of average rank of selected candidate
  output$avg_rank <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist <- list_output()
      paths_selected_rank <- outputlist$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% summarize(avg_rank = mean(value)) %>% ungroup()
      
      # Plot
      stock <- ggplot(paths_selected_rank) +
        geom_line(aes(x=draw, y=avg_rank), color="blue") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Average rank of selected candidate")
      
      ggplotly(stock) %>%
        style(hovertemplate = paste('At draw %{x:.0f},',
                                    '<br>the average candidate is the %{y:.0}th draw <br><extra></extra>'), traces = 1) %>%
        layout(hovermode="x unified)")
    })
  })  
  
  
  ### Graph of probability of choosing best candidate
  output$prob_best <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist <- list_output()
      paths_prob_best <- outputlist$paths_prob_best %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% mutate(mean_share=mean(value)) %>% ungroup()
    
      # Plot
      stock <- ggplot(paths_prob_best) +
        geom_line(aes(x=draw, y=value, group=name),size = 0.1, alpha=min(1, 50/input$I)) +
        geom_line(aes(x=draw, y=mean_share), color="blue") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Prob of urns selecting best candidate in each round")
      
      ggplotly(stock) %>% style(hoverinfo = "skip", traces = 1) %>% 
        style(hovertemplate = paste('At draw %{x:.0f},',
                                    '<br>prob. of selecting best candidate is %{y:.0%}<br><extra></extra>'), traces = 2) %>%
        layout(hovermode="x unified)")
    })
  })
  
  ### Histogram of share of women in stock selected
  output$stock_composition_bar <- renderPlotly({
    
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist <- list_output()
      paths_selected_rank <- outputlist$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(name) %>% arrange(draw) %>% mutate(count_best=cumsum(value==1), share_best=cumsum(value==1)/draw) %>% ungroup() %>% filter(draw==input$N)
      
      # Plot
      plot_ly(x =~paths_selected_rank$share_best,type="histogram",  nbinsx = sqrt(input$I))  %>%
        layout(xaxis=list(title = "Share of selected that are the best candidate after trials", range=c(0,1)), yaxis=list(title="Frequency")) %>%
        style(hovertemplate = paste('%{y:.0f} urns have <br>best candidate share','%{x} <extra></extra>'), hovermode='x unified)') 
      
      
      
    })
    
  })
  
  ### Histogram of end of AA 
  output$hist_firstend <- renderPlotly({
    
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist <- list_output()
      m <- mean(outputlist$firstend, na.rm=T)
      num_end <- sum(!is.na(outputlist$firstend))
      # Plot
      p <- plot_ly(x =~outputlist$firstend,type="histogram", name="Freq.") %>%
        layout(xaxis=list(title = paste("When AA ended (ended in", num_end, "out of", input$I, "urns)"), range=c(0,input$N)), yaxis=list(title="Frequency"))%>%
        layout(hovermode="x unified)") %>%
        style(hovertemplate = paste('%{y:.0f} urns have ended<br>AA after','%{x} draws<extra></extra>')) 
      
      if (!is.na(m)){
       p<- p %>%  add_segments(x=m, y=0, xend=m, yend=100, line=list(color="red", width = 4), name="Mean") %>%
         style(hovertemplate = paste('Mean: %{x:.1f}'), traces = 2)
       
      }
      
      p
      
    })
    
  })  
  
  ### Dynamic graph title
  output$distribution_title<- renderText({
    input$rerun
    
    isolate({
      paste0(input$I, " Polya Urns over ", input$N, " trials, W_0=", input$w_0, ", M_0=", input$m_0)
      
    })})
  
  ### Replacement matrix
  output$matrix <- renderUI({
    input$rerun
    
    isolate({
      outputlist <- list_output()
      parameters <- outputlist$parameters
      withMathJax(
        if(input$woman_stochastic=="none" & input$man_stochastic=="none"){
          paste0("Ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]],
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]],"\\end{pmatrix}$$")
        }
        else if(input$woman_stochastic!="none" & input$man_stochastic=="none"){
          paste0("Ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "*", parameters[["w_w_function"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]], "*(", parameters[["m_w_function"]], ")",
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]],"\\end{pmatrix}$$")
        }
        
        else if(input$woman_stochastic!="none" & input$man_stochastic!="none"){
          paste0("Ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "*", parameters[["w_w_function"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]], "*(", parameters[["m_w_function"]], ")",
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "*", parameters[["w_m_function"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]], "*(", parameters[["m_m_function"]], ") \\end{pmatrix}$$")
        }   
        else if(input$woman_stochastic=="none" & input$man_stochastic!="none"){
          paste0("Ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]],
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "*", parameters[["w_m_function"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]], "*(", parameters[["m_m_function"]], ") \\end{pmatrix}$$")
        }        
      )   
      
      
    })
  })
  
  ### Help text
  output$help <-renderUI({
    input$rerun
    
  isolate({
      outputlist <- list_output()
      parameters <- outputlist$parameters
      withMathJax(
        HTML(paste0(
          h2("About the Urn Simulator"),
          p("Updated September 2021."), 
          p("This simulator accompanies the paper \"Women, Men, and Polya Urns. Underrepresentation at Equal Talent in the Absence of Discrimination\" by Laura Caron, Alessandra Casella, and Victoria Mooers at Columbia University."),
          p("Note: you need to press \"re-run\" simulation in order to refresh the results. Some graphs may be slow to appear."),
          h3("Simulation Parameters"),
          p("You may choose the number of rounds from each urn and the number of urns to be simulated. You may also change the seed for the random number generator in order to get different results."),
          h3("Initial Urn Contents"), 
          p("Input the initial number of each color ball in the urn, \\(w_0\\) and \\(m_0\\)."),
          h3("Addition Scheme"), 
          p("All urns perform draws with replacement. To specify draws without replacement, choose a negative value for the number of women or men to be added after each draw. You may specify the ball addition matrix in the form:
  $$\\begin{pmatrix} w_w & m_w \\\\ w_m & m_m \\end{pmatrix} $$
  
  where \\(m_w\\) represents the number of men added when a woman is drawn, and so on."),
          h3("Stochastic addition options"), 
          
          HTML(paste0("Stochastic addition is currently:<b>", if(input$woman_stochastic!="none") " enabled " else " disabled", "</b> when a woman is drawn and <b>",if(input$man_stochastic!="none") " enabled " else " disabled ", "</b> when a man is drawn. <br></br>", 
                      "The stochastic addition may be either correlated or uncorrelated. In the correlated variety, balls are always added to the urn in each draw, regardless of the state of any random variables. The 2 \\(\\times\\) 2 case where \\(x\\) balls are added in each round can be written in terms of 2 Bernoulli random variables:

$$

\\begin{pmatrix}
w_w \\times X \\sim Bern(p) & m_w (1-X) \\\\
w_m \\times Y \\sim Bern(q) & m_m (1-Y)
\\end{pmatrix}

$$", 
                      "In that case, when a white ball is drawn, \\(p\\) gives the probability that \\(w_w\\) white balls are added. Otherwise, \\(m_w\\) maroon balls are added. <br></br>",
                      "<br></br> Correlated stochastic addition is currently: <b>", if(input$woman_stochastic=="balanced") " enabled" else " disabled", "</b> when a woman is drawn and <b>",if(input$man_stochastic=="balanced") " enabled " else " disabled ", "</b>when a man is drawn. <br></br>", 
                      
                      "The case with uncorrelated addition is written in terms of four Bernoulli random variables:

$$

\\begin{pmatrix}
w_w \\times Bern(p_{w_w}) & m_w \\times Bern(p_{m_w})\\\\
w_m \\times Bern(p_{w_m}) & m_m \\times Bern(p_{m_m})
\\end{pmatrix}

$$",
                      
"Stochastic addition may depend on the urn contents. In this case, the idea is that, e.g., the probability \\(p_{w_w}\\) is a decreasing function of the share of white balls in the earn. As a probability, this function should be bounded between 0 and 1 for shares of white balls between 0 and 1.

The most direct of such functions would be:
  $$p_{w_w} = 1-share_w^a $$
  with parameter \\(a \\in (0,\\infty) \\). We could also imagine various nonlinear functions, such as 

$$p_{w_w} = \\frac{1}{1+b share_w^a}$$
  with parameters \\(a,b \\in(0,\\infty) \\).

Another possiblity:
  $$p_{w_w} = \\frac{1}{1+b \\exp(c*share_w)}$$
  with parameters \\(b,c \\in(0,\\infty) \\).

As above, this may be done in the correlated or uncorrelated case. ", 
h3("Affirmative Action"), 
p("The app allows simulation of various affirmative action policies. The first type is one where two balls are drawn from the urn, representing the best and second-best candidates. If the best candidate is a woman, they are selected. If not, the second-best candidate is considered and is selected if they are a woman (surely, in the determinisitc case, or with a certain probability, in the stochastic case). Otherwise, the best man candidate is selected."), 
p("The second type of affirmative action adds one woman to the urn in every round, regardless of draw and addition."), 
p("The third type is a hiring quota, where only women are selected until the stopping conditions are met. We continue drawing until a woman is selected."), 
h4("Stopping conditions"), 
p("The affirmative action may continue forever, stop when women become the majority in the sample (urn), stop when women become the majority among those selected, or stop after a certain number of draws. The hiring quota may be stopped when women make up a certain percentage of those selected.")
                      
                      
                      
          ))
          
        )))
    })
  })
  
  ### Memory counter
  output$memory<-renderUI({
    input$rerun
    
    isolate({
      paste(ceiling(mem_used()/1000000), "MB used")
    })
  })
}

shinyApp(ui, server)
