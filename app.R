##################################################
#              Polya Urns Simulations
#              Laura Caron
#              Columbia University
#         This version: February 26, 2026
##################################################

##################################################
#                      Set up
#           Loads the packages we need
##################################################

## Set up renv for package version control
if (!require("remotes"))
  install.packages("remotes")

#library(renv)
options(renv.config.activate.prompt = FALSE)
renv::restore()

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
library(shinyWidgets)
library(zoo)

##################################################
#                      UI
#       Controls the interface of app
##################################################

# Create a function to hold the simulation parameters panel so don't have to duplicate code 
simParamPanel <- function(num) {
  div(
    # First section: Parameters
    # tabPanel(paste("Sim", num), 
    fluidRow(
      column(6, numericInput(paste0("I", num), "Number of urns to simulate", 100, min=1, step=5)),
      column(6, numericInput(paste0("N", num), "Number of draws from each urn", 100, min=1, step=10))),
    fluidRow(
      column(12, numericInput(paste0("seed",num), "Seed", 1234, min=0))),
    
    # Second section: Initial state
    h3("Initial Urn Contents"),
    fluidRow(
      column(5, numericInput(paste0("w_0", num), "Initial number of white balls", 10*num, min=0)), 
      column(5, numericInput(paste0("m_0", num), "Initial number of maroon balls", 40, min=0))),
    
    # Third section: Replacement/addition 
    h3("Addition Scheme"),
    fluidRow(
      column(12, radioButtons(paste0("multidraw", num), label=NULL, choices=c("Single draw"="single", "Multiple draws"="multi")))),
    conditionalPanel(condition = paste0("input.multidraw", num, "=='multi' &input.intervention", num, "=='none'"), 
                     fluidRow(column(12, checkboxInput(paste0("multi_interp", num), "Interpret multiple draw as selection intervention?", value=FALSE), 
                                     bsTooltip(id = paste0("multi_interp", num), 
                                               title = "When this option is turned on, the number of M and W selected will correspond to the number replaced. When this option is turned off, the number of M and W selected will correspond to the number drawn.")))),
    
    # Single draw options
    conditionalPanel(condition = paste0("input.multidraw", num, "=='single'"),
                     div(
                       p("Note: the original ball is replaced and addition follows the rules below."),
                       h4("If a white ball is drawn:"),
                       
                       #options for stochastic replacement -- only appear when selected
                       fluidRow(column(12, radioButtons(paste0("woman_stochastic", num), label=NULL, choices=c("Deterministic addition"="none", "Stochastic addition (correlated)" = "balanced", "Stochastic addition (uncorrelated)"= "unbalanced"), selected="none"))),
                       conditionalPanel(condition = paste0("input.woman_stochastic", num, "!= 'none'"),
                                        fluidRow(                       
                                          column(12, radioButtons(paste0("woman_depends", num), label="Depends on", choices=c("None"="none", "\\(X = \\) share of white balls in urn"="urn", "\\(X = \\) share of white balls in selected candidates"="selected"), selected="none"))),
                                        fluidRow(
                                          conditionalPanel(condition=paste0("input.woman_depends", num, "=='none'"),
                                                           column(5, numericInput(paste0("p_w_w", num), "Probability of white ball added", 1, step=0.1))),
                                          conditionalPanel(condition=paste0("input.woman_depends", num, "!='none'"),
                                                           column(5, radioButtons(paste0("w_w_function", num), label="Probability of white ball added", choices=c("\\(p_{w_w} = c-bX^a\\)"="linear","\\(p_{w_w} = \\frac{1}{1+bX^a}\\)"="inverse", "\\(p_{w_w} = \\frac{1}{1+b*\\exp(cX)}\\)"="inverseexp"))))
                                        ),
                                        fluidRow(
                                          conditionalPanel(condition=paste0("input.w_w_function", num, "!='inverseexp'&input.woman_depends", num, "!='none'"), 
                                                           column(4, numericInput(paste0("w_w_a", num), "\\(a\\)", 1, step = 0.1)), 
                                                           column(4, numericInput(paste0("w_w_b", num), "\\(b\\)", 1, step=0.1)),
                                                           column(4, numericInput(paste0("w_w_c", num), "\\(c\\)", 1, step=0.1))), 
                                          conditionalPanel(condition=paste0("input.w_w_function", num, "=='inverseexp'"), 
                                                           column(5, numericInput(paste0("w_w_b", num), "\\(b\\)", 1, step=0.1)), 
                                                           column(5, numericInput(paste0("w_w_c", num), "\\(c\\)", 1, step=0.1)))  ) ,
                                        
                                        conditionalPanel(condition=paste0("input.woman_stochastic", num, "=='unbalanced'"),
                                                         fluidRow(
                                                           conditionalPanel(condition=paste0("input.woman_depends", num, "=='none'"),
                                                                            column(5, numericInput(paste0("p_m_w", num), "Probability of maroon ball added", 1, step=0.1))),
                                                           conditionalPanel(condition=paste0("input.woman_depends", num, "!='none'"),
                                                                            column(5, radioButtons(paste0("m_w_function", num), label="Probability of maroon ball added", choices=c("\\(p_{m_w} = c-b(1-X)^a\\)"="linear","\\(p_{m_w} = \\frac{1}{1+b(1-X)^a}\\)"="inverse", "\\(p_{m_w} = \\frac{1}{1+b*\\exp(c(1-X))}\\)"="inverseexp"))))
                                                         )),
                                        fluidRow(
                                          conditionalPanel(condition=paste0("input.m_w_function", num, "!='inverseexp'&input.woman_depends", num, "!='none' & input.woman_stochastic", num, "=='unbalanced'"), 
                                                           column(4, numericInput(paste0("m_w_a", num), "\\(a\\)", 1, step = 0.1)), 
                                                           column(4, numericInput(paste0("m_w_b", num), "\\(b\\)", 1, step=0.1)),
                                                           column(4, numericInput(paste0("m_w_c", num), "\\(c\\)", 1, step=0.1))), 
                                          conditionalPanel(condition=paste0("input.m_w_function", num, "=='inverseexp'"), 
                                                           column(5, numericInput(paste0("m_w_b", num), "\\(b\\)", 1, step=0.1)), 
                                                           column(5, numericInput(paste0("m_w_c", num), "\\(c\\)", 1, step=0.1)))  )            
                       ),
                       fluidRow(
                         column(5, numericInput(paste0("w_w", num), "Number of white balls added", 1)), 
                         column(5, numericInput(paste0("m_w", num), "Number of maroon balls added", 0))
                       ),
                       
                       
                       h4("If a maroon ball is drawn:"),
                       fluidRow(column(12, radioButtons(paste0("man_stochastic", num), label=NULL, choices=c("Deterministic addition"="none", "Stochastic addition (correlated)" = "balanced", "Stochastic addition (uncorrelated)"= "unbalanced"), selected="none"))),
                       conditionalPanel(condition = paste0("input.man_stochastic", num, "!= 'none'"),
                                        fluidRow(                       
                                          column(12, radioButtons(paste0("man_depends", num), label="Depends on", choices=c("None"="none", "\\(X = \\) share of white balls in urn"="urn", "\\(X = \\) share of white balls in selected candidates"="selected"), selected="none"))),
                                        fluidRow(
                                          conditionalPanel(condition=paste0("input.man_depends", num, "=='none'"),
                                                           column(5, numericInput(paste0("p_w_m", num), "Probability of white balls added", 1, step=0.1))),
                                          conditionalPanel(condition=paste0("input.man_depends", num, "!='none'"),
                                                           column(5, radioButtons(paste0("w_m_function", num), label="Probability of white balls added", choices=c("\\(p_{w_m} = c-bX^a\\)"="linear","\\(p_{w_m} = \\frac{1}{1+bX^a}\\)"="inverse", "\\(p_{w_m} = \\frac{1}{1+b*\\exp(cX)}\\)"="inverseexp"))))
                                        ),
                                        fluidRow(
                                          conditionalPanel(condition=paste0("input.w_m_function", num, "!='inverseexp'&input.man_depends", num, "!='none'"), 
                                                           column(4, numericInput(paste0("w_m_a", num), "\\(a\\)", 1, step = 0.1)), 
                                                           column(4, numericInput(paste0("w_m_b", num), "\\(b\\)", 1, step=0.1)),
                                                           column(4, numericInput(paste0("w_m_c", num), "\\(c\\)", 1, step=0.1))), 
                                          conditionalPanel(condition=paste0("input.w_m_function", num, "=='inverseexp'"), 
                                                           column(5, numericInput(paste0("w_m_b", num), "\\(b\\)", 1, step=0.1)), 
                                                           column(5, numericInput(paste0("w_m_c", num), "\\(c\\)", 1, step=0.1)))  ) ,
                                        
                                        conditionalPanel(condition=paste0("input.man_stochastic", num, "=='unbalanced'"),
                                                         fluidRow(
                                                           conditionalPanel(condition=paste0("input.man_depends", num, "=='none'"),
                                                                            column(5, numericInput(paste0("p_m_m", num), "Probability of maroon balls added", 1, step=0.1))),
                                                           conditionalPanel(condition=paste0("input.man_depends", num, "!='none'"),
                                                                            column(5, radioButtons(paste0("m_m_function", num), label="Probability of maroon balls added", choices=c("\\(p_{m_m} = 1-b(1-X)^a\\)"="linear","\\(p_{m_m} = \\frac{1}{1+b(1-X)^a}\\)"="inverse", "\\(p_{m_m} = \\frac{1}{1+b*\\exp(c(1-X))}\\)"="inverseexp"))))
                                                         )),
                                        fluidRow(
                                          conditionalPanel(condition=paste0("input.m_m_function", num, "!='inverseexp'&input.man_depends", num, "==true & input.man_stochastic", num, "=='unbalanced'"), 
                                                           column(4, numericInput(paste0("m_m_a", num), "\\(a\\)", 1, step = 0.1)), 
                                                           column(4, numericInput(paste0("m_m_b", num), "\\(b\\)", 1, step=0.1)),
                                                           column(4, numericInput(paste0("m_m_c", num), "\\(c\\)", 1, step=0.1))), 
                                          conditionalPanel(condition=paste0("input.m_m_function", num, "=='inverseexp'"), 
                                                           column(5, numericInput(paste0("m_m_b", num), "\\(b\\)", 1, step=0.1)), 
                                                           column(5, numericInput(paste0("m_m_c", num), "\\(c\\)", 1, step=0.1)))  )            
                       ),
                       fluidRow(
                         column(5, numericInput(paste0("w_m", num), "Number of white balls added", 0)), 
                         column(5, numericInput(paste0("m_m", num), "Number of maroon balls added", 1))
                       )
                     )),
    # Multiple draws
    conditionalPanel(condition = paste0("input.multidraw", num, "=='multi'"), div(
      fluidRow(
        column(6, numericInput(paste0("num_draws", num), "Number of draws", 1, step=1, min=1))
      ),
      h4("If __ is drawn,"),
      fluidRow(
        column(12, uiOutput(paste0("matrixIn", num))))
    )),
    # Fourth section: Exit
    h3("Exit Options"),
    fluidRow(column(12, checkboxInput(paste0("exit_selected", num), "Balls exit from pool of selected (oldest first)", value=F))),
    conditionalPanel(condition=paste0("input.exit_selected", num, "==true"), 
                     fluidRow(column(12, numericInput(paste0("prob_exit",num), "Probability of exit in each round", value=0.01, min=0, max=1, step=0.1)))),
    
    # Fourth section: Interventions
    # Only show for single draw 
    conditionalPanel(paste0("input.multidraw", num, "=='single'"), div(
      h3("Interventions"),
      #fluidRow(column(12, radioButtons("intervention", "Affirmative Action", selected="none", choices=c("None"="none", "Draw K, if at least one is a woman, select a woman (deterministic)"="atleast","Draw two, if at least one is a woman, select a woman with probability"="atleast_stochastic","Draw one and add accordingly, plus always add one woman each round"="alwayswoman", "Hiring quota"="quota")))),
      fluidRow(column(12, radioButtons(paste0("intervention", num), "Affirmative Action", selected="none", choices=c("None"="none", "Draw K, if at least one is a woman, select a woman (deterministic)"="atleast", "Hiring quota"="quota", "Expand role models for W"="recruit")))),
      conditionalPanel(condition=paste0("input.intervention", num, "=='atleast_stochastic'"), 
                       fluidRow(column(12, numericInput(paste0("prob_atleast", num), "Probability of selecting second-best woman", value=1, min=0, max=1, step=0.1)))),
      conditionalPanel(condition=paste0("input.intervention", num, "=='atleast'"), 
                       fluidRow(
                         column(6, numericInput(paste0("num_draws_aa", num), "Number of draws", 2, step=1, min=1)))),
      conditionalPanel(condition=paste0("input.intervention", num, "=='quota'"), 
                       column(6, numericInput(paste0("quota_per", num), "Select at least __ W", value=1, min=1, max=2, step=1)),
                       column(6, numericInput(paste0("quota_window", num), "every __ draws", value=1, min=1, max=3, step=1)),
                       column(12, numericInput(paste0("smoothing", num), "Factor for smoothing graphs on selected candidates", value=1, min=1, step=1)),
                       #column(6, numericInput("quota", "Continue until W make up __", value=0.5, min=0, max=1, step=0.1)),
                       #column(6, radioButtons("quota_group", label="", choices=c("of selected candidates"="selected", "of urn"="urn"))),
                       #column(6, numericInput("quota_start", "Start after draw (enter 0 for start at beginning)", value=0, min=0, step=1))
      ),
      conditionalPanel(condition=paste0("input.intervention", num, "=='recruit'"), 
                       fluidRow(
                         column(8, numericInput(paste0("prob_recruit", num), "Add 1 W to candidates with prob __", 1, step=.1, min=0)))),
      fluidRow(column(12, conditionalPanel(condition = paste0("input.intervention", num, "!= 'none'"),
                                           radioButtons(paste0("stopintervention", num), "When to stop?", selected="continue", choices=c("Continue forever"="continue", "Stop if white balls more than __ in each urn"="majority","Stop if white balls more than __ among selected for each urn"="majority_selected", "Stop after X draws"="temp", "Stop if white balls more than __ in average urn" = "avg", "Stop if white balls more than __ in average selected candidates"="avg_selected")),
      ))
      ), 
      fluidRow(column(6, conditionalPanel(condition = paste0("input.intervention", num, "!= 'none'"), numericInput(paste0("aa_start", num), "Start after draw (enter 0 for start at beginning)", value=0, min=0, step=1))),
               conditionalPanel(condition=paste0("input.stopintervention", num, "=='temp'& input.intervention", num, "!='none'"), 
                                column(6, numericInput(paste0("stopafter", num), "Stop after", 30))),
               conditionalPanel(condition=paste0("(input.stopintervention", num, "=='avg' | input.stopintervention", num, "=='majority') & input.intervention", num, "!='none'"), 
                                column(6, numericInput(paste0("cutoff", num), "Use AA until white balls make up __ of the urn", value=0.5, min=0, max=1, step=0.1))),
               conditionalPanel(condition=paste0("(input.stopintervention", num, "=='avg_selected' | input.stopintervention", num, "=='majority_selected') & input.intervention", num, "!='none'"), 
                                column(6, numericInput(paste0("cutoff", num), "Use AA until white balls make up __ of the pool of selected candidates", value=0.5, min=0, max=1, step=0.1)))),
    )),
    # Interventions for multidraw  
    conditionalPanel(paste0("input.multidraw", num, "=='multi'"), div(
      h3("Interventions"),
      fluidRow(column(12, radioButtons(paste0("intervention", num), "Affirmative Action", selected="none", choices=c("None"="none", "Draw K extra candidates, hire all as many W as possible"="atleast.multi", "Hiring quota: at least __ hires must be W in each round"="quota.multi")))),
      conditionalPanel(condition=paste0("input.intervention", num, "=='atleast.multi'"), 
                       fluidRow(
                         column(6, numericInput(paste0("num_draws_aa.multi", num), "Number of extra draws", 1, step=1, min=1)))),
      conditionalPanel(condition=paste0("input.intervention", num, "=='quota.multi'"), 
                       column(6, numericInput(paste0("quota_per.multi", num), "Select at least __ W", value=1, min=1, max=paste0("input.num_draws", num), step=1)),
      ),
      fluidRow(column(12, conditionalPanel(condition = paste0("input.intervention", num, "!= 'none'"),
                                           radioButtons(paste0("stopintervention", num), "When to stop?", selected="continue", choices=c("Continue forever"="continue", "Stop if white balls more than __ in each urn"="majority","Stop if white balls more than __ among selected for each urn"="majority_selected", "Stop after X draws"="temp", "Stop if white balls more than __ in average urn" = "avg", "Stop if white balls more than __ in average selected candidates"="avg_selected")),
      ))
      ), 
      fluidRow(column(6, conditionalPanel(condition = paste0("input.intervention", num, "!= 'none'"), numericInput(paste0("aa_start", num), "Start after draw (enter 0 for start at beginning)", value=0, min=0, step=1))),
               conditionalPanel(condition=paste0("input.stopintervention", num, "=='temp'& input.intervention", num, "!='none'"), 
                                column(6, numericInput(paste0("stopafter", num), "Stop after", 30))),
               conditionalPanel(condition=paste0("(input.stopintervention", num, "=='avg' | input.stopintervention", num, "=='majority') & input.intervention", num, "!='none'"), 
                                column(6, numericInput(paste0("cutoff", num), "Use AA until white balls make up __ of the urn", value=0.5, min=0, max=1, step=0.1))),
               conditionalPanel(condition=paste0("(input.stopintervention", num, "=='avg_selected' | input.stopintervention", num, "=='majority_selected') & input.intervention", num, "!='none'"), 
                                column(6, numericInput(paste0("cutoff", num), "Use AA until white balls make up __ of the pool of selected candidates", value=0.5, min=0, max=1, step=0.1)))),
    )),
    # Fifth section: Graph options
    h3("Graph options"), 
    fluidRow(column(12,
                    radioButtons(paste0("graph_auto", num), "Dimensions", choices=c("Automatic"="auto", "Custom"="custom")),
                    fluidRow(conditionalPanel(condition=paste0("input.graph_auto", num, "=='custom'"),
                                              column(6, numericInput(paste0("graph_dim", num), "Graph Dimensions", value=100, min=0,step=50)),
                                              column(6, numericInput(paste0("graph_origin", num), "Origin", value=0, min=0,step=10))))
    )),
    
    #  )
  )
}

# Add CSS styling 
css <- HTML(
  ".radiobtn {", 
 # "background-color:red",
 "border-radius:4px !important;",
  "}"
)

# Set app theme
ui <- fluidPage(theme=shinytheme("flatly"),
tags$head(tags$style(css)),
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
               
                # Parameters panel 
                h3("Simulation Parameters"),
                fluidRow(column(12, 
                        conditionalPanel("input.enable2s==true && input.enable3s==true", 
                                         radioGroupButtons("sim_num", "Currently editing", 
                                                 choiceNames =c('Simulation 1 (solid blue)', 
                                                                 'Simulation 2 (dashed red)',
                                                                  'Simulation 3 (dotted yellow)'), 
                                                  choiceValues=c(1,2,3))), 
                            conditionalPanel("input.enable2s==false && input.enable3s==true", 
                                             radioGroupButtons("sim_num", "Currently editing", 
                                                               choiceNames =c('Simulation 1 (solid blue)', 
                                                                              'Simulation 2 (disabled)',
                                                                              'Simulation 3 (dotted yellow)'), 
                                                               choiceValues=c(1,2,3))), 
                           conditionalPanel("input.enable2s==true && input.enable3s==false", 
                                            radioGroupButtons("sim_num", "Currently editing", 
                                                              choiceNames =c('Simulation 1 (solid blue)', 
                                                                             'Simulation 2 (dashed red)',
                                                                             'Simulation 3 (disabled)'), 
                                                              choiceValues=c(1,2,3))),                    
                           conditionalPanel("input.enable2s==false && input.enable3s==false", 
                                            radioGroupButtons("sim_num", "Currently editing", 
                                                              choiceNames =c('Simulation 1 (solid blue)', 
                                                                             'Simulation 2 (disabled)',
                                                                             'Simulation 3 (disabled)'), 
                                                              choiceValues=c(1,2,3)))
                                )),
                
                conditionalPanel("input.sim_num==1", uiOutput("simParamPanel1") ),
                conditionalPanel("input.sim_num==2", div(
                  fluidRow(column(12, materialSwitch("enable2s", label="Enable simulation", value=FALSE))), 
                                  uiOutput("simParamPanel2") 
                  )),
                conditionalPanel("input.sim_num==3", div(
                  fluidRow(column(12, materialSwitch("enable3s", label="Enable simulation", value=FALSE))), 
                                  uiOutput("simParamPanel3") 
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
                           #fluidRow(uiOutput("distribution_title")),
                           # Urn paths over time
                           fluidRow(column(9, plotlyOutput("rayplot"))),
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
                           fluidRow(column(6, plotlyOutput("prob_best", height="50%")))
                           
                  ),
                  # Fourth tab
                  tabPanel("About the Urn & AA",
                           # Replacement matrix
                           fluidRow(column(4, uiOutput("matrix1")), column(4, uiOutput("matrix2")),column(4, uiOutput("matrix3"))),
                           # End of AA 
                           fluidRow(column(9, plotlyOutput("hist_firstend", height="50%")))
                  ), 
                 # Fifth tab
                 tabPanel("Extra graphs",
                          # share over time for 1 urn, colored
                          fluidRow(column(6,plotlyOutput("ratio_over_time2", height="50%")))
                                   
                 ), 
                )
              ))
   )

  # Set the default tab to be the Simulations tab
   , selected ="About"))

##############################################################
#                          Server
#             Controls all of the dynamic output
##############################################################



server <- function(input, output){
  output$simParamPanel1 <- renderUI({
    withMathJax(
    # Generate input elements using the function
    simParamPanel(1))
  
  })

  output$simParamPanel2 <- renderUI({
    # Generate input elements using the function
    withMathJax(simParamPanel(2))
    
  })  
  
  output$simParamPanel3 <- renderUI({
    # Generate input elements using the function
    withMathJax(simParamPanel(3))
    
  })  
  
multidrawMatrix <- function(num){
  
  if(is.na(input[[paste0("num_draws", num)]])){
    defaultmat <-  matrix(cbind(2, 2) , nrow=length(possibledraws), ncol=2, dimnames=list(possibledraws, c("Add||W", "Add||M")))
    matrixInput(paste0("multi_matrix", num), rows = list(names=TRUE), cols=list(names=TRUE, multiheader=TRUE),value=defaultmat)
    
  }
  else{
  possibledraws <- combn(c(rep("W", input[[paste0("num_draws", num)]]), rep("M", input[[paste0("num_draws", num)]])), input[[paste0("num_draws", num)]])
  possibledraws <- apply(possibledraws, 2, function(x) paste(sort(x,decreasing=T), collapse = "")) %>% unique()
  defaultvalueW <- str_count(possibledraws, "W")
  defaultvalueM <- str_count(possibledraws, "M")
  defaultmat <-  matrix(cbind(defaultvalueW, defaultvalueM) , nrow=length(possibledraws), ncol=2, dimnames=list(possibledraws, c("Add||W", "Add||M")))
  
  matrixInput(paste0("multi_matrix", num), rows = list(names=TRUE), cols=list(names=TRUE, multiheader=TRUE),value=defaultmat)
  }
}
  
  # Matrix size for multidraws 
  output$matrixIn1 <- renderUI({
      multidrawMatrix(1)
  })
  
  output$matrixIn2 <- renderUI({
      multidrawMatrix(2)
  })
  
  output$matrixIn3 <- renderUI({
    multidrawMatrix(3)
  })
  
  
runSimulation <- function(num) {
  
  # Check that inputs are in place
  #req(input$N1)
  t <- input$N1
  if(is.null(t)){
    input <- default_inputs
  }
  
  # To disable a simulation, set parameters the same as simulation 1 and turn down urns and draws to save computing power
  if (input[["enable2s"]] ==0 & num == 2) {
    updateNumericInput(inputId = "I2", value=1)
    updateNumericInput(inputId = "N2", value=1)
    
    if (class(input)!="list") {
      input <- reactiveValuesToList(input)  
    }
    base_names <- sapply(names(input[str_ends(names(input),"1")]), function(x) substr(x, 1, nchar(x)-1)) 
    input[paste0(base_names, "2")] <- input[paste0(base_names,"1")]
    input$N2 <- 1
    input$I2 <- 1
    
  }
  if (input[["enable3s"]] ==0 & num == 3) {
    updateNumericInput(inputId = "I3", value=1)
    updateNumericInput(inputId = "N3", value=1)
    
    if (class(input)!="list") {
      input <- reactiveValuesToList(input)  
    }
    base_names <- sapply(names(input[str_ends(names(input),"1")]), function(x) substr(x, 1, nchar(x)-1)) 
    input[paste0(base_names, "3")] <- input[paste0(base_names,"1")]
    input$N3 <- 1
    input$I3 <- 1
  }
  
  # Set the random number seed
  set.seed(input[[paste0("seed", num)]])
    
  # Set the initial urn contents
  w_0 <- input[[paste0("w_0", num)]]
  m_0 <- input[[paste0("m_0", num)]]
  
  # Number of urns
  I <- input[[paste0("I", num)]]
  # Trials for each urn
  N <- input[[paste0("N", num)]]
  
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
  stopintervention <- ifelse(input[[paste0("intervention", num)]]=="none" , "na", input[[paste0("stopintervention", num)]])
  
  # Set parameters for stochastic balanced replacement
  p_w_w <- ifelse(input[[paste0("woman_stochastic", num)]]=="none", 1, input[[paste0("p_w_w", num)]])
  p_w_m <- ifelse(input[[paste0("woman_stochastic", num)]]=="none", 1, input[[paste0("p_w_m", num)]])
  p_m_m <- ifelse(input[[paste0("man_stochastic", num)]]=="balanced", 1-input[[paste0("p_w_m", num)]], 
                  ifelse(input[[paste0("man_stochastic", num)]] == "none", 1, input[[paste0("p_m_m", num)]]))
  p_m_w <- ifelse(input[[paste0("woman_stochastic", num)]]=="balanced", 1-input[[paste0("p_w_w", num)]], 
                  ifelse(input[[paste0("woman_stochastic", num)]] == "none", 1, input[[paste0("p_m_w", num)]]))
  
  # Set conditions to be able to handle removing balls
  w_w_added <- ifelse(input[[paste0("w_w", num)]] >= 0, input[[paste0("w_w", num)]], 0)
  m_w_added <- ifelse(input[[paste0("m_w", num)]] >= 0, input[[paste0("m_w", num)]], 0)
  w_m_added <- ifelse(input[[paste0("w_m", num)]] >= 0, input[[paste0("w_m", num)]], 0)
  m_m_added <- ifelse(input[[paste0("m_m", num)]] >= 0, input[[paste0("m_m", num)]], 0)
  
  w_w_removed <- ifelse(input[[paste0("w_w", num)]] < 0, -input[[paste0("w_w", num)]], 0)
  m_w_removed <- ifelse(input[[paste0("m_w", num)]] < 0, -input[[paste0("m_w", num)]], 0)
  w_m_removed <- ifelse(input[[paste0("w_m", num)]] < 0, -input[[paste0("w_m", num)]], 0)
  m_m_removed <- ifelse(input[[paste0("m_m", num)]] < 0, -input[[paste0("m_m", num)]], 0)
  
  # Add progress bar during the simulations
  withProgress(message = paste('Running simulation', num), value = 0, {
    # Main simulation loop
    
    # Reset the urns to initial state
    # Each column is one urn, one row for each ball 
      #urn <- matrix(rep(c(rep("w", w_0),rep("m", m_0)), I), ncol = I, byrow=FALSE)
    urn <- lapply(seq(1:I), function(x) rep(c(rep("w", w_0),rep("m", m_0))))
    
    # Initialize some vectors
    w_n <- rep(w_0, I)
    m_n <- rep(m_0, I)
    prob_w_n <- NULL
    prob_best_n <- NULL
    prob_best_aa <- NULL
    prev1_prob_best_aa <- NULL
    prev2_prob_best_aa <- NULL
    rank_aa <- NULL
    prev1_rank_aa <- NULL
    prev2_rank_aa <- NULL
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
        #previous_w <- colSums(urn=="w", na.rm=T)
        #previous_m <- colSums(urn=="m", na.rm=T)
      previous_w <- sapply(urn, function(x) sum(x=="w", na.rm=T))
      previous_m <- sapply(urn, function(x) sum(x=="m", na.rm=T))
      previous_share <- previous_w/(previous_w+previous_m)
      previous_share_selected = if(n == 1) previous_share else selected_w[n-1,]/(selected_w[n-1,] + selected_m[n-1,])
      
      previous_share_avg <- mean(previous_share)
      previous_share_selected_avg <- mean(previous_share_selected)
      
      # Set probability of drawing woman when it depends on urn contents
      if(input[[paste0("woman_depends", num)]]=="urn"){
        p_w_w <- if(input[[paste0("w_w_function", num)]]=="linear"){input[[paste0("w_w_c", num)]]-input[[paste0("w_w_b", num)]] * previous_share^input[[paste0("w_w_a", num)]]
        } else if(input[[paste0("w_w_function", num)]]=="inverse") {
          1/(1+input[[paste0("w_w_b", num)]]*previous_share^input[[paste0("w_w_a", num)]])
        } else if(input[[paste0("w_w_function", num)]]=="inverseexp") {
          1/(1+input[[paste0("w_w_b", num)]]*exp(input[[paste0("w_w_c", num)]] * previous_share))
        } else NA
        
        p_m_w <- if(input[[paste0("woman_stochastic", num)]]=="balanced") {
          1-p_w_w
        } else if(input[[paste0("m_w_function", num)]]=="linear") {
          input[[paste0("m_w_c", num)]]-input[[paste0("m_w_b", num)]] * (1-previous_share)^input[[paste0("m_w_a", num)]]
        } else if(input[[paste0("m_w_function", num)]]=="inverse") {
          1/(1+input[[paste0("m_w_b", num)]]*(1-previous_share)^input[[paste0("m_w_a", num)]])
        } else if(input[[paste0("m_w_function", num)]]=="inverseexp") {
          1/(1+input[[paste0("m_w_b", num)]]*exp(input[[paste0("m_w_c", num)]] * (1-previous_share)))
        } else NA
        
        
      }
      
      if(input[[paste0("man_depends", num)]]=="urn"){
        p_w_m <- if(input[[paste0("w_m_function", num)]]=="linear") {
          input[[paste0("w_m_c", num)]]-input[[paste0("w_m_b", num)]] * previous_share^input[[paste0("w_m_a", num)]]
        } else if(input[[paste0("w_m_function", num)]]=="inverse") {
          1/(1+input[[paste0("w_m_b", num)]]*previous_share^input[[paste0("w_m_a", num)]])
        } else if(input[[paste0("w_m_function", num)]]=="inverseexp") {
          1/(1+input[[paste0("w_m_b", num)]]*exp(input[[paste0("w_m_c", num)]] * previous_share))
        } else NA
        
        p_m_m <- if(input[[paste0("man_stochastic", num)]]=="balanced") {
          1-p_w_m
        } else if(input[[paste0("m_m_function", num)]]=="linear") {
          input[[paste0("m_m_c", num)]]-input[[paste0("m_m_b", num)]] * (1-previous_share)^input[[paste0("m_m_a", num)]]
        } else if(input[[paste0("m_m_function", num)]]=="inverse") {
          1/(1+input[[paste0("m_m_b", num)]]*(1-previous_share)^input[[paste0("m_m_a", num)]])
        } else if(input[[paste0("m_m_function", num)]]=="inverseexp") {
          1/(1+input[[paste0("m_m_b", num)]]*exp(input[[paste0("m_m_c", num)]] * (1-previous_share)))
        } else NA
      }
      
      if(input[[paste0("woman_depends", num)]]=="selected"){
        p_w_w <- if(input[[paste0("w_w_function", num)]]=="linear") {
          input[[paste0("w_w_c", num)]]-input[[paste0("w_w_b", num)]] * previous_share_selected^input[[paste0("w_w_a", num)]]
        } else if(input[[paste0("w_w_function", num)]]=="inverse") {
          1/(1+input[[paste0("w_w_b", num)]]*previous_share_selected^input[[paste0("w_w_a", num)]])
        } else if(input[[paste0("w_w_function", num)]]=="inverseexp") {
          1/(1+input[[paste0("w_w_b", num)]]*exp(input[[paste0("w_w_c", num)]] * previous_share_selected))
        } else NA
        
        p_m_w <- if(input[[paste0("woman_stochastic", num)]]=="balanced"){
          1-p_w_w
        } else if(input[[paste0("m_w_function", num)]]=="linear"){
          input[[paste0("m_w_c", num)]]-input[[paste0("m_w_b", num)]] * (1-previous_share_selected)^input[[paste0("m_w_a", num)]]
        } else if(input[[paste0("m_w_function", num)]]=="inverse"){
          1/(1+input[[paste0("m_w_b", num)]]*(1-previous_share_selected)^input[[paste0("m_w_a", num)]])
        } else if(input[[paste0("m_w_function", num)]]=="inverseexp"){
          1/(1+input[[paste0("m_w_b", num)]]*exp(input[[paste0("m_w_c", num)]] * (1-previous_share_selected)))
        } else NA
      }
      
      if(input[[paste0("man_depends", num)]]=="selected"){
        p_w_m <- if(input[[paste0("w_m_function", num)]]=="linear"){
          input[[paste0("w_m_c", num)]]-input[[paste0("w_m_b", num)]] * previous_share_selected^input[[paste0("w_m_a", num)]]
        } else if(input[[paste0("w_m_function", num)]]=="inverse") {
          1/(1+input[[paste0("w_m_b", num)]]*previous_share_selected^input[[paste0("w_m_a", num)]])
        } else if(input[[paste0("w_m_function", num)]]=="inverseexp") {
          1/(1+input[[paste0("w_m_b", num)]]*exp(input[[paste0("w_m_c", num)]] * previous_share_selected))
        } else NA
        
        p_m_m <- if(input[[paste0("man_stochastic", num)]]=="balanced") {
          1-p_w_m
        } else if(input[[paste0("m_m_function", num)]]=="linear") {
          input[[paste0("m_m_c", num)]]-input[[paste0("m_m_b", num)]] * (1-previous_share_selected)^input[[paste0("m_m_a", num)]]
        } else if(input[[paste0("m_m_function", num)]]=="inverse") {
          1/(1+input[[paste0("m_m_b", num)]]*(1-previous_share_selected)^input[[paste0("m_m_a", num)]])
        } else if(input[[paste0("m_m_function", num)]]=="inverseexp") {
          1/(1+input[[paste0("m_m_b", num)]]*exp(input[[paste0("m_m_c", num)]] * (1-previous_share_selected)))
        } else NA
      }
      
      
      p_w_w <- if(length(p_w_w)==1) rep(p_w_w, I) else p_w_w
      p_w_m <- if(length(p_w_m)==1) rep(p_w_m, I) else p_w_m
      p_m_w <- if(length(p_m_w)==1) rep(p_m_w, I) else p_m_w
      p_m_m <- if(length(p_m_m)==1) rep(p_m_m, I) else p_m_m
      
      # Conditions for ending of affirmative action
      end <- ifelse(input[[paste0("intervention", num)]] == "none" & !is.na(previous_share), NA, 
                    #ifelse(input[[paste0("intervention", num)]] == "quota" & input[[paste0("quota_group", num)]] == "selected" & (previous_share_selected > input[[paste0("quota", num)]]) | end %in% 1 & n > input[[paste0("quota_start", num)]], 1, 
                    #ifelse(input[[paste0("intervention", num)]] == "quota" & input[[paste0("quota_group", num)]] == "urn" & (previous_share > input[[paste0("quota", num)]]) | end %in% 1 & n > input[[paste0("quota_start", num)]], 1, 
                    ifelse(stopintervention == "continue" & !is.na(previous_share), 0, 
                           ifelse(stopintervention == "majority" & (previous_share >= input[[paste0("cutoff", num)]]) | end %in% 1, 1, 
                                  ifelse(stopintervention == "majority_selected" & (previous_share_selected >= input[[paste0("cutoff", num)]]) | end %in% 1 & n > input[[paste0("aa_start", num)]], 1,
                                         ifelse(stopintervention == "temp" & n > input[[paste0("stopafter", num)]] & !is.na(previous_share), 1, 
                                                ifelse(stopintervention == "avg" & (previous_share_avg >= input[[paste0("cutoff", num)]]) | end %in% 1, 1, 
                                                       ifelse(stopintervention == "avg_selected" & (previous_share_selected_avg >= input[[paste0("cutoff", num)]]) | end %in% 1, 1, 
                                                              0)))))))
  
firstend <- ifelse(is.na(firstend) & end*((stopintervention=="majority"  | stopintervention=="majority_selected" | stopintervention=="avg" | stopintervention=="avg_selected" | input[[paste0("intervention", num)]] =="quota")), n , firstend)

# Probability of woman selected
### CHECK: WITH OR WITHOUT REPLACEMENT 
prob_w_selected <- ifelse(input[[paste0("multidraw", num)]]=="multi" & input[[paste0("multi_interp", num)]]==T & !(end %in% 0),  1 - dhyper(input[[paste0("num_draws", num)]], previous_m, previous_w, input[[paste0("num_draws", num)]]), 
                          ifelse(input[[paste0("intervention", num)]]=="atleast" & (end %in% 0) & n > input[[paste0("aa_start", num)]], 1 - dhyper(input[[paste0("num_draws_aa", num)]], previous_m, previous_w, input[[paste0("num_draws_aa", num)]]), 
                                 ifelse(input[[paste0("intervention", num)]]=="atleast_stochastic" & (end %in% 0) & n > input[[paste0("aa_start", num)]], previous_share+(1-previous_share)*(previous_share)*input[[paste0("prob_atleast", num)]],
                                        ifelse(input[[paste0("intervention", num)]]=="quota"& (end %in% 0) & n > input[[paste0("aa_start", num)]],1/input[[paste0("quota_window", num)]], previous_share))))

# Random draws done ahead of time for the case of balanced replacement
r_w_w <- sapply(seq(1:I), function(x) rbinom(1,1,p_w_w[x]))
r_w_m <- sapply(seq(1:I), function(x) rbinom(1,1,p_w_m[x]))
r_m_w <- if(input[[paste0("woman_stochastic", num)]]=="balanced") 1-r_w_w else sapply(seq(1:I), function(x) rbinom(1,1,p_m_w[x]))
r_m_m <- if(input[[paste0("man_stochastic", num)]]=="balanced") 1-r_w_m else sapply(seq(1:I), function(x) rbinom(1,1,p_m_m[x]))

r_exit <- if(input[[paste0("exit_selected", num)]]==T) sapply(seq(1:I), function(x) rbinom(1,1,input[[paste0("prob_exit", num)]])) else NULL

rank <- rep(1, I)
prob_best <- rep(1, I)
####
# Draw, replace, remove balls for each AA case

## SINGLE DRAW OPTIONS          
if (input[[paste0("multidraw", num)]] == "single" & 1 == 1) {
  ball_drawn <- lapply(urn, function(x) sample(na.omit(x), 1))
  ball_selected <- ball_drawn 
  
  ball_replaced <- lapply(seq(1:I), function(x) {
    rball <- if(ball_drawn[[x]] == "w") c(rep("w", w_w_added * r_w_w[x]), rep("m", m_w_added * r_m_w[x])) else c(rep("w", w_m_added * r_w_m[x]), rep("m", m_m_added * r_m_m[x]))
    if(is_empty(rball)) 0 else rball
  })
  
  ball_removed <- lapply(seq(1:I), function(x) {
    mball <- if(ball_drawn[[x]] == "w") c(rep("w", w_w_removed * r_w_w[x]), rep("m", m_w_removed * r_m_w[x])) else c(rep("w", w_m_removed * r_w_m[x]), rep("m", m_m_removed * r_m_m[x]))
    if(is_empty(mball)) 0 else mball
  })
}

## RANK POLICY 
if (input[[paste0("multidraw", num)]] == "single" & input[[paste0("intervention", num)]] == "atleast" & n > input[[paste0("aa_start", num)]]) {
  ball_drawn_aa <- lapply(urn,function(x) sample(na.omit(x), input[[paste0("num_draws_aa", num)]], replace = FALSE))
  rank_aa <- sapply(ball_drawn_aa, function(x) ifelse("w" %in% x, min(which(x == "w")), 1))
  
  prob_best_aa <- dhyper(input[[paste0("num_draws_aa", num)]], previous_m, previous_w, input[[paste0("num_draws_aa", num)]]) + previous_share
  ball_drawn_aa <- sapply(ball_drawn_aa, function(x) ifelse("w" %in% x, "w", "m"))
  ball_selected_aa <- ball_drawn_aa
  
  ball_replaced_aa <- lapply(seq(1:I), function(x) {
    rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added * r_w_w[x]), rep("m", m_w_added * r_m_w[x])) else c(rep("w", w_m_added * r_w_m[x]), rep("m", m_m_added * r_m_m[x]))
    if(is_empty(rball)) 0 else rball
  })
  
  ball_removed_aa <- sapply(seq(1:I), function(x) {
    mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed * r_w_w[x]), rep("m", m_w_removed * r_m_w[x])) else c(rep("w", w_m_removed * r_w_m[x]), rep("m", m_m_removed * r_m_m[x]))
    if(is_empty(mball)) 0 else mball
  })
}

## STOCHASTIC RANK POLICY 
if (input[[paste0("multidraw", num)]] == "single" & input[[paste0("intervention", num)]] == "atleast_stochastic" & n > input[[paste0("aa_start", num)]]) {
  ball_drawn_aa <- sapply(urn, function(x) sample(na.omit(x), 2, replace = TRUE))
  
  rank_aa <- apply(ball_drawn_aa, 2, function(x) ifelse("w" %in% x, min(which(x == "w")), 1))
  
  ball_drawn_aa <- apply(ball_drawn_aa, 2, function(x) ifelse(min(which(x == "w")) == 1, "w", ifelse(min(which(x == "w")) == 2, sample(x, 1, prob = c(1 - input[[paste0("prob_atleast", num)]], input[[paste0("prob_atleast", num)]])), "m")))
  ball_selected_aa <- ball_drawn_aa
  
  ball_replaced_aa <- sapply(seq(1:I), function(x) {
    rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added * r_w_w[x]), rep("m", m_w_added * r_m_w[x])) else c(rep("w", w_m_added * r_w_m[x]), rep("m", m_m_added * r_m_m[x]))
    if(is_empty(rball)) 0 else rball
  })
  
  ball_removed_aa <- sapply(seq(1:I), function(x) {
    mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed * r_w_w[x]), rep("m", m_w_removed * r_m_w[x])) else c(rep("w", w_m_removed * r_w_m[x]), rep("m", m_m_removed * r_m_m[x]))
    if(is_empty(mball)) 0 else mball
  })
}
## RECRUITMENT POLICY 
if (input[[paste0("multidraw", num)]] == "single" & input[[paste0("intervention", num)]] == "recruit") {
  ball_drawn_aa <- lapply(urn, function(x) sample(na.omit(x), 1))
  ball_selected_aa <- ball_drawn_aa
  
  rank_aa <- rep(1, I)
  prob_best_aa <- rep(1, I)
  
  # Check if we will add extra W to candidates this time 
  r_recruit <- rbinom(I, 1, input[[paste0("prob_recruit", num)]])
  
  ball_replaced_aa <- lapply(seq(1:I), function(x) {
    rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added * r_w_w[x]), rep("m", m_w_added * r_m_w[x])) else c(rep("w", w_m_added * r_w_m[x]), rep("m", m_m_added * r_m_m[x]))
    if(is_empty(rball) & r_recruit[x] == 1) "w" 
    else if (r_recruit[x] == 1) c(rball, "w")
    else rball  
  })
  
  ball_removed_aa <- lapply(seq(1:I), function(x) {
    mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed * r_w_w[x]), rep("m", m_w_removed * r_m_w[x])) else c(rep("w", w_m_removed * r_w_m[x]), rep("m", m_m_removed * r_m_m[x]))
    if(is_empty(mball)) 0 else mball
  })
}
## QUOTA POLICY 
if (input[[paste0("multidraw", num)]] == "single" & input[[paste0("intervention", num)]] == "quota" & n > input[[paste0("aa_start", num)]]) {
  
  draw_in_window <- ifelse(n %% input[[paste0("quota_window", num)]] > 0, n %% input[[paste0("quota_window", num)]], input[[paste0("quota_window", num)]])
  draws_left <- input[[paste0("quota_window", num)]] - draw_in_window
  
  if (draw_in_window > 1) {
    w_so_far <- lapply(selected, function(x) sum(x[(length(x) - (draw_in_window - 2)):length(x)] == "w"))
    #w_so_far <- lapply(urn, function(x) sum(x[(length(x) - (draw_in_window - 2)):length(x)] == "w"))
  } else {
    w_so_far <- rep(0, I)
  }
  
  free_choice <- sapply(w_so_far, function(x) draws_left >= input[[paste0("quota_per", num)]] - x)
  free_choice <- ifelse(rep(draw_in_window, I) == 1 & rep(input[[paste0("quota_window", num)]], I) > 1, TRUE, free_choice)
  
  if (input[[paste0("quota_window", num)]] - draw_in_window <= 1) {
    forecast_urn <- lapply(urn, function(x) c(x, "m"))
    
    find_best_W <- function(forecast_urn) {
      forecast_draws <- lapply(forecast_urn, function(x) sample(x, length(x)))
      rank <- lapply(forecast_draws, function(x) min(which(x == "w")))
      return(rank)
    }
    
    # Allow for probabilistic replacement after drawing M. M if M is replaced; W if W is replaced, etc. 
    m_if_m <- previous_m + input[[paste0("m_m", num)]]
    m_if_w <- previous_m
    w_if_m <- previous_w 
    w_if_w <- previous_w + input[[paste0("w_m", num)]]
    
      
    numer <- function(s, w, m) {
      prod(sapply(0:max(s - 2, 0), function(k) max(0, m - k)))
    }
    denom <- function(s, w, m) {
      prod(sapply(0:max(s - 1, 0), function(j) w + m - j))
    }
    
    sum1 <- sapply(seq(1:I), function(x) {
                   
                  # Deterministic addition 
                  # Simplified formula speeds up calculation 
                   if(input[[paste0("man_stochastic", num)]]=="none"){
                   return(  #1 + sum(sapply(seq(1:(m_if_m[x])), function(z) z * (dhyper(0, w_if_w[x], m_if_m[x], z) - dhyper(0, w_if_w[x], m_if_m[x], z + 1))))
                     1+(m_if_m[x])/(1+w_if_w[x])
                   )
                   }
                   
                   # Correlated stochastic addition 
                   if(input[[paste0("man_stochastic", num)]]=="balanced"){
                     return(
                     1 +
                     # M added 
                     #p_m_m * sum(sapply(seq(1:(m_if_m[x])), function(z) z * (dhyper(0, w_if_m[x], m_if_m[x], z) - dhyper(0, w_if_m[x], m_if_m[x], z + 1)))) +
                     p_m_m * ((m_if_m[x])/(1+w_if_m[x])) +
                     # W added 
                     #p_w_m * sum(sapply(seq(1:(m_if_w[x])), function(z) z * (dhyper(0, w_if_w[x], m_if_w[x], z) - dhyper(0, w_if_w[x], m_if_w[x], z + 1))))
                     p_w_m * ((m_if_w[x])/(1+w_if_w[x]))
                     ) 
                   }
                   
                   # Uncorrelated stochastic addition 
                   if(input[[paste0("man_stochastic", num)]]=="unbalanced"){
                    return(1+
                   # Only M added 
                   #p_m_m*(1-p_w_m) * sum(sapply(seq(1:(m_if_m[x])), function(z) z * (dhyper(0, w_if_m[x], m_if_m[x], z) - dhyper(0, w_if_m[x], m_if_m[x], z + 1)))) +
                    p_m_m*(1-p_w_m)*((m_if_m[x])/(1+w_if_m[x])) +
                   # Only W added 
                   # p_w_m*(1-p_m_m) * sum(sapply(seq(1:(m_if_w[x])), function(z) z * (dhyper(0, w_if_w[x], m_if_w[x], z) - dhyper(0, w_if_w[x], m_if_w[x], z + 1)))) +
                     p_w_m*(1-p_m_m)*((m_if_w[x])/(1+w_if_w[x])) +
                   # Both added 
                   # p_m_m*p_w_m * sum(sapply(seq(1:(m_if_m[x])), function(z) z * (dhyper(0, w_if_w[x], m_if_m[x], z) - dhyper(0, w_if_w[x], m_if_m[x], z + 1)))) + 
                   p_m_m*p_w_m*((m_if_m[x])/(1+w_if_w[x])) +
                  # None added 
                   #(1-p_m_m)*(1-p_w_m) * sum(sapply(seq(1:(previous_m[x])), function(z) z * (dhyper(0, previous_w[x], previous_m[x], z) - dhyper(0, previous_w[x], previous_m[x], z + 1))))
                    (1-p_m_m)*(1-p_w_m)*((previous_m[x])/(1+previous_w[x]))
                    )
                   }
    
    })
    expected_rank_W <- sum1
  }
  
  if (input[[paste0("quota_window", num)]] - draw_in_window > 1) {
    # Allow for probabilistic replacement after drawing M. M if M is replaced; W if W is replaced, etc. 
    m_if_mm <- previous_m + 2*input[[paste0("m_m", num)]]
    m_if_mw <- previous_m + input[[paste0("m_m", num)]]
    m_if_ww <- previous_m 
      
    w_if_mm <- previous_w
    w_if_mw <- previous_w + input[[paste0("w_m", num)]]
    w_if_ww <- previous_w + 2*input[[paste0("w_m", num)]]
      
    sum2.m <- sapply(seq(1:I), function(x) {
      # Deterministic addition 
      if(input[[paste0("man_stochastic", num)]]=="none"){
        return(# 1 + sum(sapply(seq(1:(m_if_mm[x])), function(z) z * (dhyper(0, w_if_ww[x], m_if_mm[x], z) - dhyper(0, w_if_ww[x], m_if_mm[x], z + 1))))
          (1+(m_if_mm[x])/(1+w_if_ww[x]))
        )
      }
      # Correlated addition 
      if(input[[paste0("man_stochastic", num)]]=="balanced"){
        return(1 +
                    # p_m_m^2 * sum(sapply(seq(1:(m_if_mm[x])), function(z) z * (dhyper(0, w_if_mm[x], m_if_mm[x], z) - dhyper(0, w_if_mm[x], m_if_mm[x], z + 1)))) +
                      p_m_m^2*((m_if_mm[x])/(1+w_if_mm[x])) +
                     #p_w_m^2 * sum(sapply(seq(1:(m_if_ww[x])), function(z) z * (dhyper(0, w_if_ww[x], m_if_ww[x], z) - dhyper(0, w_if_ww[x], m_if_ww[x], z + 1)))) + 
                      p_w_m^2*((m_if_ww[x])/(1+w_if_ww[x])) +
                     # (1-p_m_m^2 - p_w_m^2) * sum(sapply(seq(1:(m_if_mw[x])), function(z) z * (dhyper(0, w_if_mw[x], m_if_mw[x], z) - dhyper(0, w_if_mw[x], m_if_mw[x], z + 1))))
                      (1-p_m_m^2 - p_w_m^2)*((m_if_mw[x])/(1+w_if_mw[x]))
        )
      }
      })
    # Case 2: hire one M then one W 
    m <- previous_m + 1
    w <- previous_w + 1
    
    m_if_mm <- previous_m + input[[paste0("m_m", num)]] + input[[paste0("m_w", num)]]
    m_if_mw <- previous_m + input[[paste0("m_m", num)]]
    m_if_wm <- previous_m + input[[paste0("m_w", num)]]
    m_if_ww <- previous_m 
    
    w_if_mm <- previous_w
    w_if_mw <- previous_w + input[[paste0("w_w", num)]]
    w_if_wm <- previous_m + input[[paste0("w_m", num)]]
    w_if_ww <- previous_w + input[[paste0("w_m", num)]] + input[[paste0("w_w", num)]]
    
    # Deterministic addition 
    if(input[[paste0("man_stochastic", num)]]=="none" & input[[paste0("woman_stochastic", num)]]=="none"){
      sum2.w <- sapply(seq(1:I), function(x) 1 +
                             sum(sapply(seq(1:(sum2.m[x])), function(z) z * (dhyper(0, w_if_ww[x], m_if_mm[x], z) - dhyper(0, w_if_ww[x], m_if_mm[x], z + 1)))) 
                             
      )
    }
    
    # Correlated addition 
    if(input[[paste0("man_stochastic", num)]]=="balanced" & input[[paste0("woman_stochastic", num)]]=="balanced"){
    # If M replaced in period 1
    sum2.w_if_m <- sapply(seq(1:I), function(x) 1 +
                     p_m_w * sum(sapply(seq(1:(sum2.m[x])), function(z) z * (dhyper(0, w_if_mm[x], m_if_mm[x], z) - dhyper(0, w_if_mm[x], m_if_mm[x], z + 1)))) +
                     p_w_w * sum(sapply(seq(1:(sum2.m[x])), function(z) z * (dhyper(0, w_if_mw[x], m_if_mw[x], z) - dhyper(0, w_if_mw[x], m_if_mw[x], z + 1))))

    )
    # if W replaced in period 1
    sum2.w_if_w <- sapply(seq(1:I), function(x) 1 + 
                     p_m_w * sum(sapply(seq(1:(sum2.m[x])), function(z) z * (dhyper(0, w_if_wm[x], m_if_wm[x], z) - dhyper(0, w_if_wm[x], m_if_wm[x], z + 1)))) +
                     p_w_w * sum(sapply(seq(1:(sum2.m[x])), function(z) z * (dhyper(0, w_if_ww[x], m_if_ww[x], z) - dhyper(0, w_if_ww[x], m_if_ww[x], z + 1))))
                     
    )
    sum2.w <- p_m_m*sum2.w_if_m + p_w_m * sum2.w_if_w  
    
    }
    
    # Prob of hiring W in period 2 after hiring M in period 1
    # 1 -prob of no W with rank less than sum2.w in period 2
    m_if_m <- previous_m + input[[paste0("m_m", num)]]
    m_if_w <- previous_m
    w_if_m <- previous_w 
    w_if_w <- previous_w + input[[paste0("w_m", num)]]
    
    # Deterministic addition 
    if(input[[paste0("man_stochastic", num)]]=="none" & input[[paste0("woman_stochastic", num)]]=="none"){
      prob.sum2 <- sapply(seq(1:I), function(x) ( dhyper(0, w_if_w[x], m_if_m[x], floor(sum2.w[x])) )
      )
      
    }
    # Correlated addition 
    if(input[[paste0("man_stochastic", num)]]=="balanced" & input[[paste0("woman_stochastic", num)]]=="balanced"){
      
    prob.sum2 <- sapply(seq(1:I), function(x) (p_m_m * dhyper(0, w_if_m[x], m_if_m[x], floor(sum2.w_if_m[x])) ) +
                                                p_w_m * dhyper(0, w_if_w[x], m_if_w[x], floor(sum2.w_if_w[x]))
                    )
    }
    #expected_rank_W <- sum2.m * (1 - prob.sum2) + sum2.w * (prob.sum2)
    expected_rank_W <- sum2.m * (prob.sum2) + sum2.w * (1-prob.sum2)
  }
  
  draws_aa <- lapply(urn, function(x) sample(x, length(x)))
  bestW_aa <- lapply(draws_aa, function(x) min(which(x == "w")))
  
  draw_aa <- ifelse(bestW_aa <= expected_rank_W & w_so_far != input[[paste0("quota_per", num)]], bestW_aa, 1)
  draw_aa <- ifelse(free_choice == FALSE, bestW_aa, draw_aa)
  
  ball_drawn_aa <- lapply(seq(1:I), function(x) draws_aa[[x]][[draw_aa[[x]]]])
  ball_selected_aa <- ball_drawn_aa
  
  prev2_rank_aa <- prev1_rank_aa
  prev1_rank_aa <- rank_aa 
  rank_aa <- draw_aa
  
  quota_done <- w_so_far >= input[[paste0("quota_per", num)]]
  
  prev2_prob_best_aa <- prev1_prob_best_aa 
  prev1_prob_best_aa <- prob_best_aa 
  prob_best_aa <- quota_done + (1 - quota_done) * (free_choice * sapply(seq(1:I), function(x) dhyper(0, previous_w[x], previous_m[x], floor(expected_rank_W[x]))) + previous_share)
  
  if (draw_in_window==2 & draws_left==0 ){
    prob_best_aa <- (prob_best_aa + prev1_prob_best_aa)/2
    prev1_prob_best_aa <- prob_best_aa 
    
    rank_aa <- lapply(seq(1:I), function(x) (rank_aa[[x]] + prev1_rank_aa[[x]])/2 )
    prev1_rank_aa <- rank_aa 
  }
  if (draw_in_window==3 & draws_left==0 ){
    prob_best_aa <- (prob_best_aa + prev1_prob_best_aa + prev2_prob_best_aa)/3
    prev1_prob_best_aa <- prob_best_aa 
    prev2_prob_best_aa <- prob_best_aa 
    
    rank_aa <-  lapply(seq(1:I), function(x) (rank_aa[[x]] + prev1_rank_aa[[x]] + prev2_rank_aa[[x]])/3)
    prev1_rank_aa <- rank_aa 
    prev2_rank_aa <- rank_aa 
  }
  
  ball_replaced_aa <- lapply(seq(1:I), function(x) {
    rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added * r_w_w[x]), rep("m", m_w_added * r_m_w[x])) else c(rep("w", w_m_added * r_w_m[x]), rep("m", m_m_added * r_m_m[x]))
    if(is_empty(rball)) 0 else rball
  })
  
  ball_removed_aa <- lapply(seq(1:I), function(x) {
    mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed * r_w_w[x]), rep("m", m_w_removed * r_m_w[x])) else c(rep("w", w_m_removed * r_w_m[x]), rep("m", m_m_removed * r_m_m[x]))
    if(is_empty(mball)) 0 else mball
  })
  
  
  
}



      
## MULTIPLE DRAW OPTIONS
if (input[[paste0("multidraw", num)]] == "multi") {
  ball_drawn <- lapply(urn, function(x) sample(na.omit(x), input[[paste0("num_draws", num)]])) 
  ball_selected <- ball_drawn
  
  if ("character" %in% class(ball_drawn)) ball_drawn <- matrix(ball_drawn, ncol=I)

  ball_replaced <- lapply(seq(1:I), function(x) {
    total_W = ifelse(input[[paste0("num_draws", num)]] == 1, str_count(ball_drawn[[x]], "w"), str_count(paste(ball_drawn[[x]], collapse=""), "w")) 
    total_M = ifelse(input[[paste0("num_draws", num)]] == 1, str_count(ball_drawn[[x]], "m"), str_count(paste(ball_drawn[[x]], collapse=""), "m")) 
    # total W is the matrix row index for the input replacement matrix
    rball <- c(rep("w", as.numeric(input[[paste0("multi_matrix", num)]][total_M+1, 1])), rep("m", as.numeric(input[[paste0("multi_matrix", num)]][total_M+1, 2])))
    if(is_empty(rball)) 0 else rball
  })
  
  if (input[[paste0("multi_interp", num)]] == TRUE) {
    rank <- lapply(ball_drawn, function(x) ifelse("w" %in% x, min(which(x == "w")), 1))
    # Prob best
    # P(W first) = previous_share
    # P(M only) = (1-previous_share)^2
    prob_best <- dhyper(input[[paste0("num_draws", num)]], previous_m, previous_w, input[[paste0("num_draws", num)]]) + previous_share
  }
  
  if (input[[paste0("multi_interp", num)]] == FALSE) {
    rank <- lapply(ball_drawn, function(x) seq(1: length(x)))
    prob_best <- rep(1, I)
  }
  
  # Not currently used--removals not allowed for multiple draw 
  ball_removed <- as.list(rep(0, I))
  #ball_removed <- lapply(seq(1:I), function(x) {
  #  mball <- if("w" %in% ball_drawn[,x]) c(rep("w", w_w_removed * r_w_w[x]), rep("m", m_w_removed * r_m_w[x])) else c(rep("w", w_m_removed * r_w_m[x]), rep("m", m_m_removed * r_m_m[x]))
  #  if(is_empty(mball)) 0 else mball
  #})
}

## AA for multiple draws 
  # Rank policy 
if (input[[paste0("multidraw", num)]] == "multi" & input[[paste0("intervention", num)]] == "atleast.multi") {
  ball_drawn_aa <- lapply(urn, function(x) sample(na.omit(x), input[[paste0("num_draws", num)]] + input[[paste0("num_draws_aa.multi", num)]])) 
  
  ball_selected_aa <- lapply(seq(1:I), function(x) {
    # Take all W up to num_draws 
    total_W = ifelse(input[[paste0("num_draws", num)]] == 1, str_count(ball_drawn_aa[[x]], "w"), str_count(paste(ball_drawn_aa[[x]], collapse=""), "w")) 
    total_W = ifelse(total_W > input[[paste0("num_draws", num)]], input[[paste0("num_draws", num)]], total_W)
    total_M = input[[paste0("num_draws", num)]] - total_W
    c(rep("w", total_W), rep("m", total_M))
  })
    
  ball_replaced_aa <- lapply(ball_selected_aa, function(x) {
    total_W = ifelse(input[[paste0("num_draws", num)]] == 1, str_count(x, "w"), str_count(paste(x, collapse=""), "w")) 
    total_M = ifelse(input[[paste0("num_draws", num)]] == 1, str_count(x, "m"), str_count(paste(x, collapse=""), "m")) 
    # total W is the matrix row index for the input replacement matrix
    rball <- c(rep("w", as.numeric(input[[paste0("multi_matrix", num)]][total_M+1, 1])), rep("m", as.numeric(input[[paste0("multi_matrix", num)]][total_M+1, 2])))
    if(is_empty(rball)) 0 else rball
  })
  ## CHECK
      # rank = best W and best M given how many are selected 
    rank_M <- lapply(seq(1:I), function(x) which(ball_drawn_aa[[x]]=="m")[1:sum(ball_selected_aa[[x]]=="m")])
    rank_M <- ifelse(lapply(ball_selected_aa, function(x) sum(x=="m")) ==0, NA, rank_M)
    rank_W <- lapply(seq(1:I), function(x) which(ball_drawn_aa[[x]]=="w")[1:sum(ball_selected_aa[[x]]=="w")])
    rank_W <- ifelse(lapply(ball_selected_aa, function(x) sum(x=="w")) ==0, NA, rank_W)
    rank_aa <- lapply(seq(1:I), function(x) c(rank_M[[x]], rank_W[[x]]))
    rank_aa <- lapply(rank_aa, na.omit)
    
    # Prob best
    # Probability that the best candidate is W or that an M is hired and M is best  
    # M hired if W drawn < num of hires -> at least v W out of total draws 
    prob_best_aa <- previous_share + (1-previous_share)*phyper(input[[paste0("num_draws", num)]]-1, previous_w, previous_m, input[[paste0("num_draws", num)]] + input[[paste0("num_draws_aa.multi", num)]]-1 )
    ball_removed_aa <- as.list(rep(0, I))
  
}
# Quota policy 
if (input[[paste0("multidraw", num)]] == "multi" & input[[paste0("intervention", num)]] == "quota.multi") {
  ball_drawn_aa <- lapply(urn, function(x) sample(na.omit(x), length(x))) 

  ball_selected_aa <- lapply(seq(1:I), function(x) {
    # Take best W candidates 
    total_W = ifelse(input[[paste0("num_draws", num)]] == 1, str_count(ball_drawn_aa[[x]][1], "w"), str_count(paste(ball_drawn_aa[[x]][1:input[[paste0("num_draws", num)]]], collapse=""), "w")) 
    # Fill in any extra W needed for the quota 
    total_W = ifelse(total_W < input[[paste0("quota_per.multi", num)]], input[[paste0("quota_per.multi", num)]], total_W)
    # Truncate max number of hires 
    total_W = ifelse(total_W > input[[paste0("num_draws", num)]], input[[paste0("num_draws", num)]], total_W)
    # M is the rest 
    total_M = input[[paste0("num_draws", num)]] - total_W

    c(rep("w", total_W), rep("m", total_M))
  })
  
  ball_replaced_aa <- lapply(ball_selected_aa, function(x) {
    total_W = ifelse(input[[paste0("num_draws", num)]] == 1, str_count(x, "w"), str_count(paste(x, collapse=""), "w")) 
    total_M = ifelse(input[[paste0("num_draws", num)]] == 1, str_count(x, "m"), str_count(paste(x, collapse=""), "m")) 
    # total W is the matrix row index for the input replacement matrix
    rball <- c(rep("w", as.numeric(input[[paste0("multi_matrix", num)]][total_M+1, 1])), rep("m", as.numeric(input[[paste0("multi_matrix", num)]][total_M+1, 2])))
    if(is_empty(rball)) 0 else rball
  })
  
  ## CHECK
  # rank = best W and best M given how many are selected 
  rank_M <- lapply(seq(1:I), function(x) which(ball_drawn_aa[[x]]=="m")[1:sum(ball_selected_aa[[x]]=="m")])
  rank_M <- ifelse(lapply(ball_selected_aa, function(x) sum(x=="m")) ==0, NA, rank_M)
  rank_W <- lapply(seq(1:I), function(x) which(ball_drawn_aa[[x]]=="w")[1:sum(ball_selected_aa[[x]]=="w")])
  rank_W <- ifelse(lapply(ball_selected_aa, function(x) sum(x=="w")) ==0, NA, rank_W)
  rank_aa <- lapply(seq(1:I), function(x) c(rank_M[[x]], rank_W[[x]]))
  rank_aa <- lapply(rank_aa, na.omit)
  
  # Prob best = 1 if quota < num hires 
  # Otherwise, prob best = prob W is best 
  prob_best_aa <- if(input[[paste0("num_draws", num)]] >= input[[paste0("quota_per.multi", num)]]) rep(1, I) else previous_share
  
  ball_removed_aa <- as.list(rep(0, I))
  
}
if (input[[paste0("multidraw", num)]] == "multi" & input[[paste0("multi_interp", num)]] == FALSE) {
  print("debug") 
}
ball_selected <- ball_drawn
# For urns undergoing AA, use those draws instead 
if (input[[paste0("intervention", num)]] != "none" & n > input[[paste0("aa_start", num)]] ) {
  ball_drawn <- ifelse(end %in% 0, ball_drawn_aa, ball_drawn)
  ball_replaced_aa <- if(!("list" %in% class(ball_replaced_aa))) sapply(ball_replaced_aa, list) else ball_replaced_aa
  ball_replaced <- lapply(seq(1:I), function(x) if(end[x] %in% 0) ball_replaced_aa[[x]] else ball_replaced[x])
  prob_best <- lapply(seq(1:I), function(x) if(end[x] %in% 0) prob_best_aa[[x]] else prob_best[x])
  
  ball_removed_aa <- if(!("list" %in% class(ball_removed_aa))) sapply(ball_removed_aa, list) else ball_removed_aa
  ball_removed <- lapply(seq(1:I), function(x) if(end[x] %in% 0) ball_removed_aa[[x]] else ball_removed[x])
  
  rank <- ifelse(end %in% 0, rank_aa, rank)
  ball_selected <- ifelse(end %in% 0, ball_selected_aa, ball_selected)
}

# For urns with exit, incorporate that 
if (input[[paste0("exit_selected", num)]] == TRUE & n > 1) {
  ' list.selected <- split(t(selected), seq(nrow(t(selected)))) 
    list.selected <- lapply(list.selected, function(x) x[!is.na(x)])
    ball_exit_selected <- sapply(seq(1:I), function(x) if(r_exit[x] == 1) list.selected[[x]][min(which(!is.na(list.selected[[x]]) & list.selected[[x]] != "no"))] else NA ) %>% unlist
    list.selected <- sapply(seq(1:I), function(x) if(r_exit[x] == 1) {
      if(length(list.selected[[x]]) == 0) NA else list.selected[[x]][-1]
    } else list.selected[[x]])
    list.selected <- lapply(list.selected, function(x) if(length(x) < n) c(x, rep("no", n - length(x) - 1)) else x)
    selected <- list.selected %>% unlist() %>% matrix(nrow = n - 1, ncol = I)'
  
  oldest <- sapply(seq(1:I), function(x) min(which(selected[,x] == "w" | selected[,x] == "m")))
  selected[oldest, r_exit == 1] <- "no"
  #selected <- sapply(seq(1:I), function(x) if(r_exit[x] == 1) c("no", selected[(min(which(selected[,x] == "w" | selected[,x] == "m")) + 1):length(selected[,x]), x]) else selected[,x])
}  

# Save results 
new_w <- sapply(seq(1:I), function(x) previous_w[x] + sum(ball_replaced[[x]] == "w") - sum(ball_removed[[x]] == "w"))
new_w <- ifelse(new_w < 0, 0, new_w)
new_m <- sapply(seq(1:I), function(x) previous_m[x] + sum(ball_replaced[[x]] == "m") - sum(ball_removed[[x]] == "m"))
new_m <- ifelse(new_m < 0, 0, new_m)

add_w <- sapply(seq(1:I), function(x) sum(ball_replaced[[x]] == "w") - sum(ball_removed[[x]] == "w"))
add_w <- ifelse(add_w < 0, 0, add_w)
add_m <- sapply(seq(1:I), function(x) sum(ball_replaced[[x]] == "m") - sum(ball_removed[[x]] == "m"))
add_m <- ifelse(add_m < 0, 0, add_m)

add <- lapply(seq(1:length(add_w)), function(x) c(rep("w", add_w[x]), rep("m", add_m[x])))

if(is.null(dim(add))) {
  add <- as.list(add)
}

urn <- lapply(seq(1:I), function(x) c(urn[[x]], add[[x]]))
#urn <- mapply(c, urn, add, SIMPLIFY = FALSE )
# For urns with multidraw intervention interpretation, change how "selected" is defined
if (input[[paste0("multidraw", num)]] == "multi" & input[[paste0("multi_interp", num)]] == TRUE) {
  ball_selected <- ball_replaced
}

#selected <- rbind(selected, ball_selected)
#selected_rank <- if (n > 1) rbind(selected_rank, rank) else rank

selected <- lapply(seq(1:I), function(x) rbind(selected[[x]], ball_selected[[x]]))
selected_rank <- if (n > 1) lapply(seq(1:I), function(x) rbind(selected_rank[[x]], rank[[x]])) else as.list(rank)

selected_w <- rbind(selected_w, sapply(seq(1:I), function(x) sum(selected[[x]] == "w")))
selected_m <- rbind(selected_m, sapply(seq(1:I), function(x) sum(selected[[x]] == "m")))


w_n <- rbind(w_n, new_w)
m_n <- rbind(m_n, new_m)
prob_w_n <- rbind(prob_w_n, prob_w_selected)
prob_best_n <- rbind(prob_best_n, prob_best)
  # For quota policy, go back and edit prior probabilities 
  if (!is.null(prev1_prob_best_aa)){
    if(min(prev1_prob_best_aa == prob_best_aa)==1){
    prob_best_n[(n-1), ] <- lapply(seq(1:I), function(x) if(end[x] %in% 0) prob_best_aa[[x]] else prob_best_n[[(n-1), x]])
    selected_rank <- lapply(seq(1:I), function(x) {
      if(end[x] %in% 0) {
        selected_rank[[x]][(n-1), ] <- unlist(selected_rank[[x]][(n), ] )
      }
      selected_rank[[x]]
      })
    } 
  }
  if (!is.null(prev2_prob_best_aa)){
    if (min(prev1_prob_best_aa == prev2_prob_best_aa) ==1 & min(prev1_prob_best_aa %in% prob_best_aa)==1){
    prob_best_n[(n-1), ] <- lapply(seq(1:I), function(x) if(end[x] %in% 0) prob_best_aa[[x]] else prob_best_n[[(n-1), x]])
    prob_best_n[(n-2), ] <- lapply(seq(1:I), function(x) if(end[x] %in% 0) prob_best_aa[[x]] else prob_best_n[[(n-2), x]])
    
    selected_rank <- lapply(seq(1:I), function(x) {
      if(end[x] %in% 0) {
        selected_rank[[x]][n-1, ] <- unlist(selected_rank[[x]][n, ] )
      }
      selected_rank[[x]]
    })
    
    selected_rank <- lapply(seq(1:I), function(x) {
      if(end[x] %in% 0) {
        selected_rank[[x]][n-2, ] <- unlist(selected_rank[[x]][n, ] )
      }
      selected_rank[[x]]
    })
    }
  }

prob_w_w_replace_n <- rbind(prob_w_w_replace_n, p_w_w)
prob_w_m_replace_n <- rbind(prob_w_m_replace_n, p_w_m)

incProgress(1 / N, detail = paste(round(n * 100 / N, 1), "%"))

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
    names(paths_selected) = sapply(seq(1:I), function(x) paste0("Urn", x))

    paths_selected_rank <- selected_rank
    names(paths_selected_rank) = sapply(seq(1:I), function(x) paste0("Urn", x))

    paths_selected_w <- selected_w
    colnames(paths_selected_w) = sapply(seq(1:I), function(x) paste0("Urn", x))
    rownames(paths_selected_w) <- NULL     
    
    paths_selected_m <- selected_m
    colnames(paths_selected_m) = sapply(seq(1:I), function(x) paste0("Urn", x))
    rownames(paths_selected_m) <- NULL            
    
    rm(selected, selected_rank, selected_w, selected_m)
    
    
  })  
  
  # Save the urn functions for probability later
  
  w_w_function_t <- if(input[[paste0("woman_depends", num)]] == "none") {
    paste0("X \\sim Bern(", input[[paste0("p_w_w", num)]], ")")
  }
  else if(input[[paste0("w_w_function", num)]] == "linear") {
    paste0("X \\sim Bern(1-", input[[paste0("w_w_b", num)]], "*share_w^", input[[paste0("w_w_a", num)]], ")")
  }
  else if(input[[paste0("w_w_function", num)]] == "inverse") {
    paste0("X \\sim Bern(\\frac{1}{1+", input[[paste0("w_w_b", num)]], "*(share_w)^", input[[paste0("w_w_a", num)]], " } )")
  }
  else if(input[[paste0("w_w_function", num)]] == "inverseexp") {
    paste0("X \\sim Bern(\\frac{1}{1+", input[[paste0("w_w_b", num)]], "*\\exp(", input[[paste0("w_w_c", num)]], "*share_w)} )")
  }
  
  m_w_function_t <- if(input[[paste0("woman_depends", num)]] == "none" & 
                       (input[[paste0("woman_stochastic", num)]] == "balanced" | input[[paste0("woman_stochastic", num)]] == "none")) {
    paste0("1-X")
  }
  else if(input[[paste0("woman_depends", num)]] == "none" & input[[paste0("woman_stochastic", num)]] == "unbalanced") {
    paste0("Bern(", input[[paste0("p_m_w", num)]], ")")
  }
  else if(input[[paste0("woman_depends", num)]] == "urn") {
    if (input[[paste0("woman_stochastic", num)]] == "balanced") {
      paste0("1-X")
    }
    else if(input[[paste0("m_w_function", num)]] == "linear") {
      paste0("1-", input[[paste0("m_w_b", num)]], "*share_m^", input[[paste0("m_w_a", num)]])
    }
    else if(input[[paste0("m_w_function", num)]] == "inverse") {
      paste0("\\frac{1}{1+", input[[paste0("m_w_b", num)]], "*(share_m)^", input[[paste0("m_w_a", num)]], " }")
    }
    else if(input[[paste0("m_w_function", num)]] == "inverseexp") {
      paste0("\\frac{1}{1+", input[[paste0("m_w_b", num)]], "*\\exp(", input[[paste0("m_w_c", num)]], "*share_m)}")
    }    
  }
  
  w_m_function_t <- if(input[[paste0("man_depends", num)]] == "none") {
    paste0("Y \\sim Bern(", input[[paste0("p_w_m", num)]], ")")
  }
  else if(input[[paste0("w_m_function", num)]] == "linear") {
    paste0("Y \\sim Bern(1-", input[[paste0("w_m_b", num)]], "*share_w^", input[[paste0("w_m_a", num)]], ")")
  }
  else if(input[[paste0("w_m_function", num)]] == "inverse") {
    paste0("Y \\sim Bern(\\frac{1}{1+", input[[paste0("w_m_b", num)]], "*(share_w)^", input[[paste0("w_m_a", num)]], " } )")
  }
  else if(input[[paste0("w_m_function", num)]] == "inverseexp") {
    paste0("Y \\sim Bern(\\frac{1}{1+", input[[paste0("w_m_b", num)]], "*\\exp(", input[[paste0("w_m_c", num)]], "*share_w)} )")
  }
  
  m_m_function_t <- if(input[[paste0("man_depends", num)]] == "none" & input[[paste0("man_stochastic", num)]] == "balanced") {
    paste0("1-Y")
  }
  else if(input[[paste0("man_depends", num)]] == "none" & input[[paste0("man_stochastic", num)]] == "unbalanced") {
    paste0("Y \\sim Bern(", input[[paste0("p_m_m", num)]], ")")
  }
  else if(input[[paste0("man_depends", num)]] == "urn") {
    if (input[[paste0("man_stochastic", num)]] == "balanced") {
      paste0("1-Y")
    }
    else if(input[[paste0("m_m_function", num)]] == "linear") {
      paste0("1-", input[[paste0("m_m_b", num)]], "*share_m^", input[[paste0("m_m_a", num)]])
    }
    else if(input[[paste0("m_m_function", num)]] == "inverse") {
      paste0("\\frac{1}{1+", input[[paste0("m_m_b", num)]], "*(share_m)^", input[[paste0("m_m_a", num)]], " }")
    }
    else if(input[[paste0("m_m_function", num)]] == "inverseexp") {
      paste0("\\frac{1}{1+", input[[paste0("m_m_b", num)]], "*\\exp(", input[[paste0("m_m_c", num)]], "*share_m)}")
    }    
  }
  
  
  # Create a vector of the parameters to save for later
  parameters <- list("p_w_w"=input[[paste0("p_w_w", num)]], "p_w_m"=input[[paste0("p_w_m", num)]], "p_m_m"=input[[paste0("p_m_m", num)]], "p_m_w"=input[[paste0("p_m_w", num)]], 
                     "w_w_added"=w_w_added, "w_m_added"=w_m_added, "m_w_added"=m_w_added, "m_m_added"=m_m_added, 
                     "w_w_removed"=w_w_removed, "w_m_removed"=w_m_removed, "m_w_removed"=m_w_removed, "m_m_removed"=m_m_removed, 
                     "w_w_function" = w_w_function_t, "w_m_function"=w_m_function_t, "m_w_function"=m_w_function_t, "m_m_function"=m_m_function_t)
  # Create a list with all the outputs
  outputlist <- list(paths_ratio=paths_ratio, paths_w_n=paths_w_n, paths_m_n=paths_m_n, paths_prob_w_w_replace_n=paths_prob_w_w_replace_n, paths_prob_w_m_replace_n=paths_prob_w_m_replace_n, paths_prob_w_n=paths_prob_w_n, paths_selected=paths_selected, paths_selected_rank=paths_selected_rank, paths_selected_w=paths_selected_w, paths_selected_m=paths_selected_m, parameters=parameters, firstend=firstend, paths_prob_best=paths_prob_best)
  return(outputlist)
  
}

default_inputs.0 <- list("I" = 100,
                      "N" = 100,
                      "seed" = 1234,
                      "w_0" = 10,
                      "m_0" = 40,
                      "multidraw" = "single",
                      "multi_matrix" = "single",
                      "multi_interp" = FALSE,
                      "woman_stochastic" = "none",
                      "woman_depends" = "none",
                      "p_w_w" = 1,
                      "w_w_function" = "linear",
                      "w_w_a" = 1,
                      "w_w_b" = 1,
                      "w_w_c" = 1,
                      "p_m_w" = 1,
                      "m_w_function" = "linear",
                      "m_w_a" = 1,
                      "m_w_b" = 1,
                      "m_w_c" = 1,
                      "w_w" = 1,
                      "m_w" = 0,
                      "man_stochastic" = "none",
                      "man_depends" = "none",
                      "p_w_m" = 1,
                      "w_m_function" = "linear",
                      "w_m_a" = 1,
                      "w_m_b" = 1,
                      "w_m_c" = 1,
                      "p_m_m" = 1,
                      "m_m_function" = "linear",
                      "m_m_a" = 1,
                      "m_m_b" = 1,
                      "m_m_c" = 1,
                      "w_m" = 0,
                      "m_m" = 1,
                      "num_draws" = 1,
                      "exit_selected"= FALSE,
                      "prob_exit" = 0,
                      "intervention" = "none",
                      "stopintervention" = "continue",
                      "aa_start" = 0,
                      "stopafter" = 30,
                      "cutoff" = 0.5,
                      "graph_auto" = "auto",
                      "graph_dim" = 100,
                      "graph_origin" = 0
                      )

default_inputs <- rep(default_inputs.0, 3)
names(default_inputs)<- c(paste0(names(default_inputs.0), "1"), paste0(names(default_inputs.0), "2"),paste0(names(default_inputs.0), "3"))
default_inputs$w_02 <- 20
default_inputs$w_03 <- 30
default_inputs$enable2s <- FALSE
default_inputs$enable3s <- FALSE

#https://packages.tesselle.org/khroma/articles/tol.html


color1 <- "#004488"
color2 <- "#BB5566"
color3 <- "#117733"
color3 <- "#DDAA33"
color3 <- "#997700"


color1.light <- "#BBCCEE"
color2.light <- "#FFCCCC"
color3.light <- "#CCDDAA"
color3.light <- "#EEEEBB"

color2.light <- "#EE99AA"
color3.light <- "#EECC66"



coloreq <- "#555555"

  list_output1<- reactive ({
    
    # Trigger to rerun
    input$rerun > 1
    
    # Isolate forces it not to refresh until the rerun button is pressed
    isolate({
      input <- reactiveValuesToList(input)
      
     runSimulation(1)
    })
    
    
  })

  list_output2<- reactive ({
    
    # Trigger to rerun
    input$rerun > 1
      
      # Isolate forces it not to refresh until the rerun button is pressed
      isolate({
        input <- reactiveValuesToList(input)

        runSimulation(2)
      })

    
  })
  
  list_output3<- reactive ({
    
    # Trigger to rerun
    input$rerun > 1
    
    # Isolate forces it not to refresh until the rerun button is pressed
    isolate({
      input <- reactiveValuesToList(input)
      
      runSimulation(3)
    })
    
    
  })
  
  
  ### Histogram 
  output$histogram <- renderPlotly({
    
    input$rerun
    
    isolate({  
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      }
      
      # Get the ratios and prepare data: Sim 1
      outputlist <- list_output1()
      paths_ratio <- outputlist$paths_ratio
      hist_data <- as.data.frame(paths_ratio[nrow(paths_ratio),])
      colnames(hist_data) <- "Share of white balls in urn after trials"
      
      # Get the ratios and prepare data: Sim 2
      outputlist2 <- list_output2()
      paths_ratio2 <- outputlist2$paths_ratio
      hist_data2 <- as.data.frame(paths_ratio2[nrow(paths_ratio2),])
      colnames(hist_data2) <- "Share of white balls in urn after trials"  
      
      # Get the ratios and prepare data: Sim 3
      outputlist3 <- list_output3()
      paths_ratio3 <- outputlist3$paths_ratio
      hist_data3 <- as.data.frame(paths_ratio3[nrow(paths_ratio3),])
      colnames(hist_data3) <- "Share of white balls in urn after trials"  
      
      N1 <- nrow(hist_data)
      N2 <- nrow(hist_data2)
      N3 <- nrow(hist_data3)
        
      # Plot
      bin.width1 <- 1/(sqrt(N1))
      bin.width2 <- 1/(sqrt(N2))
      bin.width3 <- 1/(sqrt(N3))
      bins1 <- floor(sqrt(N1))
      bins2 <- floor(sqrt(N2))
      bins3 <- floor(sqrt(N3))
      
      hist <- ggplot() + 
        # Sim 3
        geom_histogram(data=hist_data3, aes(x=`Share of white balls in urn after trials`), bins=bins3, fill=color3.light, alpha=.3)+
        geom_density(data=hist_data3, aes(x=`Share of white balls in urn after trials`, y =after_stat(count*bin.width3)), color=color3, linetype="dotted")+
        # Sim 2
        geom_histogram(data=hist_data2, aes(x=`Share of white balls in urn after trials`), bins=bins2, fill=color2.light, alpha=.3)+
        geom_density(data=hist_data2, aes(x=`Share of white balls in urn after trials`, y =after_stat(count*bin.width2)), color=color2, linetype="dashed")+
        # Sim 1
        geom_histogram(data=hist_data, aes(x=`Share of white balls in urn after trials`), bins=bins1, fill=color1.light, alpha=.3)+
        geom_density(data=hist_data, aes(x=`Share of white balls in urn after trials`, y =after_stat(count*bin.width1)), color=color1)+
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
      
      
      # When there are identical traces, ggplotly drops them
      text_y3 <- paste("Simulation 3:<br>", g$x$data[[1]]$y, 'urns have white ball <br>share', round(g$x$data[[1]]$x, 2), "-", round(g$x$data[[1]]$x+g$x$data[[1]]$width,2))
      if(length(g$x$data)>4){
      text_y2 <- paste("Simulation 2:<br>", g$x$data[[3]]$y, 'urns have white ball <br>share', round(g$x$data[[3]]$x, 2), "-", round(g$x$data[[3]]$x+g$x$data[[3]]$width,2))
      text_y1 <- paste("Simulation 1:<br>",g$x$data[[5]]$y, 'urns have white ball <br>share', round(g$x$data[[5]]$x, 2), "-", round(g$x$data[[5]]$x+g$x$data[[5]]$width,2))
      }
      else{
        text_y2 <- paste("Simulation 2:<br>", g$x$data[[1]]$y, 'urns have white ball <br>share', round(g$x$data[[1]]$x, 2), "-", round(g$x$data[[1]]$x+g$x$data[[1]]$width,2))
        text_y1 <- paste("Simulation 1:<br>", g$x$data[[1]]$y, 'urns have white ball <br>share', round(g$x$data[[1]]$x, 2), "-", round(g$x$data[[1]]$x+g$x$data[[1]]$width,2))
      }
      
      g %>% style(text=text_y3, traces =1) %>%
        style(text=text_y2, traces =3) %>%
        style(text=text_y1, traces =5) %>%
        layout(hovermode="x") %>%
        style(text=NA, hoverInfo="skip", traces=c(2, 4, 6))
      
      
    })
    
  })
  
  ### Density plots
  output$density <- renderPlotly({
    
    input$rerun
    
    isolate({  
      
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_ratio1 <- outputlist1$paths_ratio
      hist_data1 <- as.data.frame(paths_ratio1[nrow(paths_ratio1),])
      colnames(hist_data1) <- "Share of white balls in urn after trials"
      
      outputlist2 <- list_output2()
      paths_ratio2 <- outputlist2$paths_ratio
      hist_data2 <- as.data.frame(paths_ratio2[nrow(paths_ratio2),])
      colnames(hist_data2) <- "Share of white balls in urn after trials"  
      
      outputlist3 <- list_output3()
      paths_ratio3 <- outputlist3$paths_ratio
      hist_data3 <- as.data.frame(paths_ratio3[nrow(paths_ratio3),])
      colnames(hist_data3) <- "Share of white balls in urn after trials"  
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
      
      plot_ly(x = ~density(hist_data3$`Share of white balls in urn after trials`)$x, y = ~density(hist_data3$`Share of white balls in urn after trials`)$y, type = 'scatter', mode = 'lines')  %>%
        add_trace(x = ~density(hist_data2$`Share of white balls in urn after trials`)$x, y = ~density(hist_data2$`Share of white balls in urn after trials`)$y, type = 'scatter', mode = 'lines')  %>%
        add_trace(x = ~density(hist_data1$`Share of white balls in urn after trials`)$x, y = ~density(hist_data1$`Share of white balls in urn after trials`)$y, type = 'scatter', mode = 'lines')  %>%
        layout(xaxis=list(title = "Share of white balls in the urn after trials", range=c(0,1)), yaxis=list(title="Density"), hovermode="x unified)")
      
    })
    
  })
  
  ### CDF plot
  output$cdf <- renderPlotly({
    
    input$rerun
    
    isolate({  
      
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_ratio1 <- outputlist1$paths_ratio
      hist_data1 <- as.data.frame(paths_ratio1[nrow(paths_ratio1),])
      colnames(hist_data1) <- "Share of white balls in urn after trials"
      hist_data1 <- arrange(hist_data1, `Share of white balls in urn after trials`)  

      outputlist2 <- list_output2()
      paths_ratio2 <- outputlist2$paths_ratio
      hist_data2 <- as.data.frame(paths_ratio2[nrow(paths_ratio2),])
      colnames(hist_data2) <- "Share of white balls in urn after trials"
      hist_data2 <- arrange(hist_data2, `Share of white balls in urn after trials`)  
      
      outputlist3 <- list_output3()
      paths_ratio3 <- outputlist3$paths_ratio
      hist_data3 <- as.data.frame(paths_ratio3[nrow(paths_ratio3),])
      colnames(hist_data3) <- "Share of white balls in urn after trials"
      hist_data3 <- arrange(hist_data3, `Share of white balls in urn after trials`)  
      
      # Plot
      cdf <- ggplot() + 
        stat_ecdf(data = hist_data3, aes(x=`Share of white balls in urn after trials`, text=paste0("Simulation 3:<br>", ..y.. * 100, '% of urns have less than<br>', round(..x.., 2)*100, '% white balls')), geom="step", color=color3, linetype="dotted")+
        stat_ecdf(data = hist_data2, aes(x=`Share of white balls in urn after trials`, text=paste0("Simulation 2:<br>", ..y.. * 100, '% of urns have less than<br>', round(..x.., 2)*100, '% white balls')), geom="step", color=color2, linetype="dashed")+
        stat_ecdf(data = hist_data1, aes(x=`Share of white balls in urn after trials`, text=paste0("Simulation 1:<br>", ..y.. * 100, '% of urns have less than<br>', round(..x.., 2)*100, '% white balls')), geom="step", color=color1, linetype="solid")+
        geom_vline(xintercept=0.5, color=coloreq, linetype="solid")+
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
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      }
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_ratio1 <- outputlist1$paths_ratio
      paths_ratio1 <- paths_ratio1 %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio1 <- paths_ratio1 %>% pivot_longer(-draw)
      average_ratio1 <- paths_ratio1 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text1 <- paste0('Simulation 1: <br>After draw ', paths_ratio1$draw -1,', avg. urn has<br>',round(average_ratio1, digits=4)* 100, '% white balls')
      
      outputlist2 <- list_output2()
      paths_ratio2 <- outputlist2$paths_ratio
      paths_ratio2 <- paths_ratio2 %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio2 <- paths_ratio2 %>% pivot_longer(-draw)
      average_ratio2 <- paths_ratio2 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text2 <- paste0('Simulation 2: <br>After draw ', paths_ratio2$draw -1,', avg. urn has<br>',round(average_ratio2, digits=4)* 100, '% white balls')
     
      outputlist3 <- list_output3()
      paths_ratio3 <- outputlist3$paths_ratio
      paths_ratio3 <- paths_ratio3 %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio3 <- paths_ratio3 %>% pivot_longer(-draw)
      average_ratio3 <- paths_ratio3 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text3 <- paste0('Simulation 3: <br>After draw ', paths_ratio3$draw -1,', avg. urn has<br>',round(average_ratio3, digits=4)* 100, '% white balls')
      
      # Plot
      r <- ggplot() + 
        geom_line(data=ratio3, aes(x=draw-1, y=value, group=name), color=color3.light, linetype="solid", alpha=.3) +
        geom_line(data=ratio2, aes(x=draw-1, y=value, group=name), color=color2.light, linetype="solid", alpha=.3) +
        geom_line(data=ratio1, aes(x=draw-1, y=value, group=name), color=color1.light, alpha=.3) +
        geom_point(aes(x=rep(0:(length(average_ratio3)-1)), y=average_ratio3, text=text3), size=0.1, color=color3, alpha=0) +
        geom_line(aes(x=rep(0:(length(average_ratio3)-1)), y=average_ratio3), color=color3, linetype="dotted") +
        geom_point(aes(x=rep(0:(length(average_ratio2)-1)), y=average_ratio2, text=text2), size=0.1, color=color2, alpha=0) +
        geom_line(aes(x=rep(0:(length(average_ratio2)-1)), y=average_ratio2), color=color2, linetype="dashed") +
        geom_point(aes(x=rep(0:(length(average_ratio1)-1)), y=average_ratio1, text=text1), size=0.1, color=color1, alpha=0) +
        geom_line(aes(x=rep(0:(length(average_ratio1)-1)), y=average_ratio1), color=color1) +
        geom_hline(aes(yintercept=0.5), color=coloreq,linetype = "solid") +
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
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      } 
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_selected_ratio1 <- outputlist1$paths_selected_w/(outputlist1$paths_selected_w + outputlist1$paths_selected_m)
      paths_selected_ratio1 <- paths_selected_ratio1 %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio1 <- paths_selected_ratio1 %>% pivot_longer(-draw)
      average_ratio1 <- paths_selected_ratio1 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text1 <- paste0('Simulation 1: <br>After draw ', paths_selected_ratio1$draw ,', avg. pool of selected has<br>',round(average_ratio1, digits=4)* 100, '% white balls')
      
      outputlist2 <- list_output2()
      paths_selected_ratio2 <- outputlist2$paths_selected_w/(outputlist2$paths_selected_w + outputlist2$paths_selected_m)
      paths_selected_ratio2 <- paths_selected_ratio2 %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio2 <- paths_selected_ratio2 %>% pivot_longer(-draw)
      average_ratio2 <- paths_selected_ratio2 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text2 <- paste0('Simulation 2: <br>After draw ', paths_selected_ratio2$draw ,', avg. pool of selected has<br>',round(average_ratio2, digits=4)* 100, '% white balls')
      
      outputlist3 <- list_output3()
      paths_selected_ratio3 <- outputlist3$paths_selected_w/(outputlist3$paths_selected_w + outputlist3$paths_selected_m)
      paths_selected_ratio3 <- paths_selected_ratio3 %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio3 <- paths_selected_ratio3 %>% pivot_longer(-draw)
      average_ratio3 <- paths_selected_ratio3 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text3 <- paste0('Simulation 3: <br>After draw ', paths_selected_ratio3$draw ,', avg. pool of selected has<br>',round(average_ratio3, digits=4)* 100, '% white balls')
      
      # Plot
      r <- ggplot() + 
        geom_line(data=ratio3, aes(x=draw-1, y=value, group=name), color=color3.light, alpha=.3) +
        geom_line(data=ratio2, aes(x=draw-1, y=value, group=name), color=color2.light, alpha=.3) +
        geom_line(data=ratio1, aes(x=draw-1, y=value, group=name), color=color1.light, alpha=.3) +
        geom_point(aes(x=rep(1:(length(average_ratio3))), y=average_ratio3, text=text3), size=0.1, color=color3, alpha=0) +
        geom_line(aes(x=rep(1:(length(average_ratio3))), y=average_ratio3), color=color3, linetype="dotted") +
        geom_point(aes(x=rep(1:(length(average_ratio2))), y=average_ratio2, text=text2), size=0.1, color=color2, alpha=0) +
        geom_line(aes(x=rep(1:(length(average_ratio2))), y=average_ratio2), color=color2, linetype="dashed") +
        geom_point(aes(x=rep(1:(length(average_ratio1))), y=average_ratio1, text=text1), size=0.1, color=color1, alpha=0) +
        geom_line(aes(x=rep(1:(length(average_ratio1))), y=average_ratio1), color=color1, linetype="solid") +
        geom_hline(aes(yintercept=0.5), color=coloreq,linetype = "solid") +
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
        layout(hovermode="x unified")
      
    })
    
  })
  
  ### Probability of selecting a woman over time
  output$prob_w_over_time <- renderPlotly({
    input$rerun
    # Check that inputs are in place
    t <- input$N1
    if(is.null(t)){
      input <- default_inputs
    }
    isolate({
      # Get and prepare the data
      outputlist1 <- list_output1()
      paths_prob_w_n1 <- outputlist1$paths_prob_w_n %>% as.data.frame
      paths_prob_w_n1 <- paths_prob_w_n1  %>% mutate(draw=row_number()) 
      prob_w_n1 <- paths_prob_w_n1 %>% pivot_longer(-draw)
      average_prob_w1 <- paths_prob_w_n1 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
    
      outputlist2 <- list_output2()
      paths_prob_w_n2 <- outputlist2$paths_prob_w_n %>% as.data.frame
      paths_prob_w_n2 <- paths_prob_w_n2  %>% mutate(draw=row_number()) 
      prob_w_n2 <- paths_prob_w_n2 %>% pivot_longer(-draw)
      average_prob_w2 <- paths_prob_w_n2 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()  
 
      outputlist3 <- list_output3()
      paths_prob_w_n3 <- outputlist3$paths_prob_w_n %>% as.data.frame
      paths_prob_w_n3 <- paths_prob_w_n3  %>% mutate(draw=row_number()) 
      prob_w_n3 <- paths_prob_w_n3 %>% pivot_longer(-draw)
      average_prob_w3 <- as.vector(paths_prob_w_n3 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()  )
      
      # Plot
      r <- ggplot() + 
        geom_line(data=prob_w_n3, aes(x=draw, y=value, group=name), color=color3.light, alpha=.3) +
        geom_line(aes(x=rep(1:(length(average_prob_w3))), y=average_prob_w3), color=color3, linetype="dotted") +
        geom_line(data=prob_w_n2, aes(x=draw, y=value, group=name), color=color2.light, alpha=.3) +
        geom_line(aes(x=rep(1:(length(average_prob_w2))), y=average_prob_w2), color=color2, linetype="dashed") +
        geom_line(data=prob_w_n1, aes(x=draw, y=value, group=name), color=color1.light, alpha=.3) +
        geom_line(aes(x=rep(1:(length(average_prob_w1))), y=average_prob_w1), color=color1) +
        geom_hline(aes(yintercept=0.5), color=coloreq,linetype = "solid" ) +
        scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) + 
        labs(title ="Probability of selecting a white ball, average highlighted", y="Probability of selecting a white ball", x="Draw")
        
      ggplotly(r) %>% style(hoverinfo = "skip", traces = c(1,3,5)) %>%
        style(hovertemplate = paste('Simulation 3:<br>At draw %{x:.0f},',
                                    '<br>avg. Prob(select white ball) = %{y:.2%}<br><extra></extra>'), traces = 2) %>%
        style(hovertemplate = paste('Simulation 2:<br>At draw %{x:.0f},',
                                    '<br>avg. Prob(select white ball) = %{y:.2%}<br><extra></extra>'), traces = 4) %>%
        style(hovertemplate = paste('Simulation 1:<br>At draw %{x:.0f},',
                                    '<br>avg. Prob(select white ball) = %{y:.2%}<br><extra></extra>'), traces = 6) %>%
        layout(hovermode="x unified)")
    })
    
  })
  
  ### Number of women and men in urn over time
  output$rayplot <- renderPlotly({
    
    input$rerun
    
    isolate({
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      }
      
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_w_n1 <- outputlist1$paths_w_n %>% as.data.frame
      paths_m_n1 <- outputlist1$paths_m_n %>% as.data.frame
      w_n_long1 <- mutate(paths_w_n1, draw = row_number()) %>% pivot_longer(cols=starts_with("Urn"), names_to="Urn")
      colnames(w_n_long1)[3] <- "W"
      m_n_long1 <- pivot_longer(paths_m_n1, cols=starts_with("Urn"), names_to="Urn")
      colnames(m_n_long1)[2] <- "M"
      ray_data1 <- cbind(w_n_long1, m_n_long1$M, rep(1, length(m_n_long1$M)))
      colnames(ray_data1)[4] <- "M"  
      colnames(ray_data1)[5] <- "one"  
      average1 <- ray_data1 %>% group_by(draw) %>% summarize(mean_w = mean(W), mean_m = mean(M))
      
      outputlist2 <- list_output2()
      paths_w_n2 <- outputlist2$paths_w_n %>% as.data.frame
      paths_m_n2 <- outputlist2$paths_m_n %>% as.data.frame
      w_n_long2 <- mutate(paths_w_n2, draw = row_number()) %>% pivot_longer(cols=starts_with("Urn"), names_to="Urn")
      colnames(w_n_long2)[3] <- "W"
      m_n_long2 <- pivot_longer(paths_m_n2, cols=starts_with("Urn"), names_to="Urn")
      colnames(m_n_long2)[2] <- "M"
      ray_data2 <- cbind(w_n_long2, m_n_long2$M, rep(1, length(m_n_long2$M)))
      colnames(ray_data2)[4] <- "M"  
      colnames(ray_data2)[5] <- "one"  
      average2 <- ray_data2 %>% group_by(draw) %>% summarize(mean_w = mean(W), mean_m = mean(M))
      
      outputlist3 <- list_output3()
      paths_w_n3 <- outputlist3$paths_w_n %>% as.data.frame
      paths_m_n3 <- outputlist3$paths_m_n %>% as.data.frame
      w_n_long3 <- mutate(paths_w_n3, draw = row_number()) %>% pivot_longer(cols=starts_with("Urn"), names_to="Urn")
      colnames(w_n_long3)[3] <- "W"
      m_n_long3 <- pivot_longer(paths_m_n3, cols=starts_with("Urn"), names_to="Urn")
      colnames(m_n_long3)[2] <- "M"
      ray_data3 <- cbind(w_n_long3, m_n_long3$M, rep(1, length(m_n_long3$M)))
      colnames(ray_data3)[4] <- "M"  
      colnames(ray_data3)[5] <- "one"  
      average3 <- ray_data3 %>% group_by(draw) %>% summarize(mean_w = mean(W), mean_m = mean(M))
      
      # Get the graph limits
      limits<-c(ifelse(input$graph_auto1=="auto", min(input$w_01, input$m_01,input$w_02, input$m_02, input$m_03, input$w_03), input$graph_origin1), ifelse(input$graph_auto1=="auto", input$N1 + max(input$w_01, input$m_01, input$w_02, input$m_02, input$w_03, input$m_03), input$graph_dim1))
      
      # Plot
      rays <- ggplot() +
        geom_line(data=ray_data3, aes(x=W, y=M, group=Urn),alpha=.3, color=color3.light) +
        geom_line(data=average3, aes(x=mean_w, y=mean_m),color=color3, linetype="dotted") +  
        geom_line(data=ray_data2, aes(x=W, y=M, group=Urn),alpha=.3, color=color2.light) +
        geom_line(data=average2, aes(x=mean_w, y=mean_m),color=color2, linetype="dashed") +
        geom_line(data=ray_data1, aes(x=W, y=M, group=Urn),alpha=.3, color=color1.light) +
        geom_line(data=average1, aes(x=mean_w, y=mean_m),color=color1) +
        geom_abline(aes(slope=1, intercept=0), color=coloreq, linetype="solid") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Number of white balls", y="Number of maroon balls",title = "Average highlighted")
      
      # Plot
      rays <- if(input$graph_auto1=="auto") rays + scale_x_continuous() + scale_y_continuous() else rays + scale_x_continuous(limits=limits) + scale_y_continuous(limits=limits)
      
      
      ggplotly(rays, tooltip=c("x", "y", "text", "Urn")) %>%
        layout(hovermode="x unified)")
      
    })
    
  })
  
  ### Stock of men and women selected over time
  output$stockplot <- renderPlotly({
    
    input$rerun
    
    isolate({
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      }      
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_selected1 <- outputlist1$paths_selected %>% as.data.frame
      paths_selected_w1 <- outputlist1$paths_selected_w %>% as.data.frame
      paths_selected_m1 <- outputlist1$paths_selected_m %>% as.data.frame
      w_n_long1 <- mutate(paths_selected_w1, draw=row_number()) %>% pivot_longer(cols=starts_with("Urn"), names_to="Urn") 
      colnames(w_n_long1)[3] <- "W"
      m_n_long1 <- pivot_longer(paths_selected_m1, cols=starts_with("Urn"), names_to="Urn")
      colnames(m_n_long1)[2] <- "M"
      stock_data1 <- cbind(w_n_long1, m_n_long1$M)
      colnames(stock_data1)[4] <- "M"  
      stock_data1[is.na(stock_data1)]<- 0
      average1 <- stock_data1 %>% as.data.frame() %>% group_by(draw) %>% summarize(mean_w = mean(W), mean_m = mean(M))
      
      outputlist2 <- list_output2()
      paths_selected2 <- outputlist2$paths_selected %>% as.data.frame
      paths_selected_w2 <- outputlist2$paths_selected_w %>% as.data.frame
      paths_selected_m2 <- outputlist2$paths_selected_m %>% as.data.frame
      w_n_long2 <- mutate(paths_selected_w2, draw=row_number()) %>% pivot_longer(cols=starts_with("Urn"), names_to="Urn") 
      colnames(w_n_long2)[3] <- "W"
      m_n_long2 <- pivot_longer(paths_selected_m2, cols=starts_with("Urn"), names_to="Urn")
      colnames(m_n_long2)[2] <- "M"
      stock_data2 <- cbind(w_n_long2, m_n_long2$M)
      colnames(stock_data2)[4] <- "M"  
      stock_data2[is.na(stock_data2)]<- 0
      average2 <- stock_data2 %>% as.data.frame() %>% group_by(draw) %>% summarize(mean_w = mean(W), mean_m = mean(M))

      outputlist3 <- list_output3()
      paths_selected3 <- outputlist3$paths_selected %>% as.data.frame
      paths_selected_w3 <- outputlist3$paths_selected_w %>% as.data.frame
      paths_selected_m3 <- outputlist3$paths_selected_m %>% as.data.frame
      w_n_long3 <- mutate(paths_selected_w3, draw=row_number()) %>% pivot_longer(cols=starts_with("Urn"), names_to="Urn") 
      colnames(w_n_long3)[3] <- "W"
      m_n_long3 <- pivot_longer(paths_selected_m3, cols=starts_with("Urn"), names_to="Urn")
      colnames(m_n_long3)[2] <- "M"
      stock_data3 <- cbind(w_n_long3, m_n_long3$M)
      colnames(stock_data3)[4] <- "M"  
      stock_data3[is.na(stock_data3)]<- 0
      average3 <- stock_data3 %>% as.data.frame() %>% group_by(draw) %>% summarize(mean_w = mean(W), mean_m = mean(M))
      
      # Get the graph limits
      limits<-c(ifelse(input$graph_auto1=="auto", min(input$w_01, input$m_01, input$w_02, input$m_02, inputs$w_03, inputs$m_03), input$graph_origin1), ifelse(input$graph_auto1=="auto", input$N1 + max(input$w_01, input$m_01, input$w_02, input$m_02, input$w_03, input$m_03), input$graph_dim1))
      
      # Plot
      stock <- ggplot() +
        geom_line(data=stock_data3, aes(x=W, y=M, group=Urn),alpha=.3, color=color3.light) +
        geom_line(data=average3, aes(x=mean_w, y=mean_m),color=color3, linetype="dotted") +
        geom_line(data=stock_data2, aes(x=W, y=M, group=Urn),alpha=.3, color=color2.light) +
        geom_line(data=average2, aes(x=mean_w, y=mean_m),color=color2, linetype="dashed") +
        geom_line(data=stock_data1, aes(x=W, y=M, group=Urn),alpha=.3, color=color1.light) +
        geom_line(data=average1, aes(x=mean_w, y=mean_m),color=color1) +
        geom_abline(aes(slope=1, intercept=0), color=coloreq, linetype="solid") +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Number of white balls selected", y="Number of maroon balls selected",title = "Average highlighted")
           
       stock <- if(input$graph_auto1=="auto") stock + scale_x_continuous() + scale_y_continuous() else stock + scale_x_continuous(limits=limits) + scale_y_continuous(limits=limits)
      
      
      ggplotly(stock) %>%
        style(hovertemplate=paste('Simulation 1:<br>Avg. maroon: %{y}<br>', 'Avg. white: %{x} <extra></extra>'), traces=6) %>%
        style(hovertemplate=paste('Simulation 2:<br>Avg. maroon: %{y}<br>', 'Avg. white: %{x} <extra></extra>'), traces=4) %>%
        style(hovertemplate=paste('Simulation 4:<br>Avg. maroon: %{y}<br>', 'Avg. white: %{x} <extra></extra>'), traces=2) %>%
        style(hoverinfo="skip", traces=c(1,3, 5)) %>%
        layout(hovermode="x unified)") 
        
      
    })
    
  })
  
  
  ### Graph of share of women in the stock selected
  output$stock_composition <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_selected_rank1 <- outputlist1$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(name) %>% arrange(draw) %>% mutate(count_best=cumsum(value==1), share_best=cumsum(value==1)/draw) %>% ungroup()
      average1 <- paths_selected_rank1 %>% group_by(draw) %>% summarize(mean_share = mean(share_best))

      outputlist2 <- list_output2()
      paths_selected_rank2 <- outputlist2$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(name) %>% arrange(draw) %>% mutate(count_best=cumsum(value==1), share_best=cumsum(value==1)/draw) %>% ungroup()
      average2 <- paths_selected_rank2 %>% group_by(draw) %>% summarize(mean_share = mean(share_best))   

      outputlist3 <- list_output3()
      paths_selected_rank3 <- outputlist3$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(name) %>% arrange(draw) %>% mutate(count_best=cumsum(value==1), share_best=cumsum(value==1)/draw) %>% ungroup()
      average3 <- paths_selected_rank3 %>% group_by(draw) %>% summarize(mean_share = mean(share_best))   
      
      # Plot
      stock <- ggplot() +
        geom_line(data=paths_selected_rank3, aes(x=draw, y=share_best, group=name),size = 0.1, alpha=min(1, 50/input$I3), color=color3.light) +
        geom_line(data=average3, aes(x=draw, y=mean_share), color=color3, linetype="dotted") +
        geom_line(data=paths_selected_rank2, aes(x=draw, y=share_best, group=name),size = 0.1, alpha=min(1, 50/input$I2), color=color2.light) +
        geom_line(data=average2, aes(x=draw, y=mean_share), color=color2, linetype="dashed") +
        geom_line(data=paths_selected_rank1, aes(x=draw, y=share_best, group=name),size = 0.1, alpha=min(1, 50/input$I1), color=color1.light) +
        geom_line(data=average1, aes(x=draw, y=mean_share), color=color1) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Share of selected that are the best candidate")
      
      ggplotly(stock) %>% style(hoverinfo = "skip", traces = c(1, 3, 5)) %>%
        style(hovertemplate = paste('Simulation 3:<br>After draw %{x:.0f},',
                                    '<br>%{y:.0%} are the best candidate<br><extra></extra>'), traces = 2) %>%
        style(hovertemplate = paste('Simulation 2:<br>After draw %{x:.0f},',
                                    '<br>%{y:.0%} are the best candidate<br><extra></extra>'), traces = 4) %>%
        style(hovertemplate = paste('Simulation 1:<br>After draw %{x:.0f},',
                                    '<br>%{y:.0%} are the best candidate<br><extra></extra>'), traces = 6) %>%
        layout(hovermode="x unified)")
      
      
    })
    
  })
  
  ### Graph of share of women in the stock selected
  output$stock_composition <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_selected_rank1 <- outputlist1$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% mutate(name=ifelse(!is.na(str_locate(name,"\\.")[,1]), substr(name, 1, str_locate(name,"\\.")[,1]-1), name)) %>%
        group_by(name, draw) %>% summarize(share_best=mean(value==1)) %>% ungroup() %>% group_by(name) %>% mutate(share_best = cumsum(share_best)/cumsum(share_best==share_best))
      average1 <- paths_selected_rank1 %>% group_by(draw) %>% summarize(mean_share = mean(share_best))
     
      outputlist2 <- list_output2()
      paths_selected_rank2 <- outputlist2$paths_selected_rank  %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% mutate(name=ifelse(!is.na(str_locate(name,"\\.")[,1]), substr(name, 1, str_locate(name,"\\.")[,1]-1), name)) %>%
        group_by(name, draw) %>% summarize(share_best=mean(value==1)) %>% ungroup() %>% group_by(name) %>% mutate(share_best = cumsum(share_best)/cumsum(share_best==share_best))
      average2 <- paths_selected_rank2 %>% group_by(draw) %>% summarize(mean_share = mean(share_best))
      
      outputlist3 <- list_output3()
      paths_selected_rank3 <- outputlist3$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% mutate(name=ifelse(!is.na(str_locate(name,"\\.")[,1]), substr(name, 1, str_locate(name,"\\.")[,1]-1), name)) %>%
        group_by(name, draw) %>% summarize(share_best=mean(value==1)) %>% ungroup() %>% group_by(name) %>% mutate(share_best = cumsum(share_best)/cumsum(share_best==share_best))
      average3 <- paths_selected_rank3 %>% group_by(draw) %>% summarize(mean_share = mean(share_best))
      
      # Plot
      stock <- ggplot() +
        geom_line(data=paths_selected_rank3, aes(x=draw, y=share_best, group=name),color=color3.light, alpha=.3) +
        geom_line(data=average3, aes(x=draw, y=mean_share), color=color3) +
        geom_line(data=paths_selected_rank2, aes(x=draw, y=share_best, group=name),color=color2.light, alpha=.3) +
        geom_line(data=average2, aes(x=draw, y=mean_share), color=color2) +
        geom_line(data=paths_selected_rank1, aes(x=draw, y=share_best, group=name),color=color1.light, alpha=.3) +
        geom_line(data=average1, aes(x=draw, y=mean_share), color=color1) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Share of selected that are the best candidate")
      
      ggplotly(stock) %>% style(hoverinfo = "skip", traces = c(1,3,5)) %>%
        style(hovertemplate = paste('Simulation 3:<br>After draw %{x:.0f},',
                                    '<br>%{y:.0%} are the best candidate<br><extra></extra>'), traces = 2) %>%
        style(hovertemplate = paste('Simulation 2:<br>After draw %{x:.0f},',
                                    '<br>%{y:.0%} are the best candidate<br><extra></extra>'), traces = 4) %>%
        style(hovertemplate = paste('Simulation 1:<br>After draw %{x:.0f},',
                                    '<br>%{y:.0%} are the best candidate<br><extra></extra>'), traces = 6) %>%
        layout(hovermode="x unified)")
          })
  })
  
  ## Graph of share of urns choosing best candidate
  output$share_best <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_selected_rank1 <- outputlist1$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% summarize(count_best=sum(value==1), share_best=sum(value==1)/length(outputlist1$paths_selected_rank)) %>% ungroup()

      outputlist2 <- list_output2()
      paths_selected_rank2 <- outputlist2$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% summarize(count_best=sum(value==1), share_best=sum(value==1)/length(outputlist2$paths_selected_rank)) %>% ungroup()
      
      outputlist3 <- list_output3()
      paths_selected_rank3 <- outputlist3$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% summarize(count_best=sum(value==1), share_best=sum(value==1)/length(outputlist3$paths_selected_rank)) %>% ungroup()
      
      # Smoothing factor
      smooth1 <- ifelse(input$intervention1 == "quota", input$smoothing1, 1)
      if(input$enable2s==TRUE){
      smooth2 <- ifelse(input$intervention2 == "quota", input$smoothing2, 1)
      } 
      else {
        smooth2 <- 1
      }
      if(input$enable3s==TRUE){
      smooth3 <- ifelse(input$intervention3 == "quota", input$smoothing3, 1)
      }
      else{
        smooth3 <-1 
      }
      
      # Plot
      stock <- ggplot() +
        geom_line(data=paths_selected_rank3, aes(x=draw, y=rollmean(share_best, smooth3, fill=NA)), color=color3) +
        geom_line(data=paths_selected_rank2, aes(x=draw, y=rollmean(share_best, smooth2, fill=NA)), color=color2) +
        geom_line(data=paths_selected_rank1, aes(x=draw, y=rollmean(share_best, smooth1, fill=NA)), color=color1) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Share of selected that are the best candidate")
      
      ggplotly(stock) %>%
        style(hovertemplate = paste('Simulation 1:<br>At draw %{x:.0f},',
                                    '<br>%{y:.0%} of urns select the best candidate<br><extra></extra>'), traces = 3) %>%
        style(hovertemplate = paste('Simulation 2:<br>At draw %{x:.0f},',
                                    '<br>%{y:.0%} of urns select the best candidate<br><extra></extra>'), traces = 2) %>%
        style(hovertemplate = paste('Simulation 3:<br>At draw %{x:.0f},',
                                    '<br>%{y:.0%} of urns select the best candidate<br><extra></extra>'), traces = 1) %>%
        layout(hovermode="x unified)")
    })
  })  
  
  ## Graph of average rank of selected candidate
  output$avg_rank <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_selected_rank1 <- outputlist1$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% summarize(avg_rank = mean(value)) %>% ungroup()

      outputlist2 <- list_output2()
      paths_selected_rank2 <- outputlist2$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% summarize(avg_rank = mean(value)) %>% ungroup()      
      
      outputlist3 <- list_output3()
      paths_selected_rank3 <- outputlist3$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% summarize(avg_rank = mean(value)) %>% ungroup()
      
      # Smoothing factor
      smooth1 <- ifelse(input$intervention1 == "quota", input$smoothing1, 1)
      if(input$enable2s==TRUE){
        smooth2 <- ifelse(input$intervention2 == "quota", input$smoothing2, 1)
      } 
      else {
        smooth2 <- 1
      }
      if(input$enable3s==TRUE){
        smooth3 <- ifelse(input$intervention3 == "quota", input$smoothing3, 1)
      }
      else{
        smooth3 <-1 
      }
      # Plot
      stock <- ggplot() +
        geom_line(data=paths_selected_rank3, aes(x=draw, y=rollmean(avg_rank, smooth3, fill=NA)), color=color3) +
        geom_line(data=paths_selected_rank2, aes(x=draw, y=rollmean(avg_rank, smooth2, fill=NA)), color=color2) +
        geom_line(data=paths_selected_rank1, aes(x=draw, y=rollmean(avg_rank, smooth1, fill=NA)), color=color1) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Average rank of selected candidate")
      
      ggplotly(stock) %>%
        style(hovertemplate = paste('Simulation 3:<br>At draw %{x:.0f},',
                                    '<br>the average candidate is the %{y:.0}th draw <br><extra></extra>'), traces = 1) %>%
        style(hovertemplate = paste('Simulation 2:<br>At draw %{x:.0f},',
                                    '<br>the average candidate is the %{y:.0}th draw <br><extra></extra>'), traces = 2) %>%
        style(hovertemplate = paste('Simulation 1:<br>At draw %{x:.0f},',
                                    '<br>the average candidate is the %{y:.0}th draw <br><extra></extra>'), traces = 3) %>%
        layout(hovermode="x unified)")
    })
  })  
  
  
  ### Graph of probability of choosing best candidate
  output$prob_best <- renderPlotly({
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_prob_best1 <- outputlist1$paths_prob_best %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% mutate(value=unlist(value), mean_share=mean(value)) %>% ungroup()

      outputlist2 <- list_output2()
      paths_prob_best2 <- outputlist2$paths_prob_best %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% mutate(value=unlist(value), mean_share=mean(value)) %>% ungroup()
 
      outputlist3 <- list_output3()
      paths_prob_best3 <- outputlist3$paths_prob_best %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% 
        group_by(draw) %>% mutate(value=unlist(value), mean_share=mean(value)) %>% ungroup()
      
      # Smoothing factor
      smooth1 <- ifelse(input$intervention1 == "quota", input$smoothing1, 1)
      if(input$enable2s==TRUE){
        smooth2 <- ifelse(input$intervention2 == "quota", input$smoothing2, 1)
      } 
      else {
        smooth2 <- 1
      }
      if(input$enable3s==TRUE){
        smooth3 <- ifelse(input$intervention3 == "quota", input$smoothing3, 1)
      }
      else{
        smooth3 <-1 
      }
      
      paths_prob_best1 <- paths_prob_best1 %>%
        group_by(name) %>%
        mutate(value=rollmean(value, smooth1, fill=NA), 
               mean_share=rollmean(mean_share, smooth1, fill=NA))
 
     paths_prob_best2 <- paths_prob_best2 %>%
        group_by(name) %>%
        mutate(value=rollmean(value, smooth2, fill=NA), 
               mean_share=rollmean(mean_share, smooth2, fill=NA))
      
      paths_prob_best3 <- paths_prob_best3 %>%
        group_by(name) %>%
        mutate(value=rollmean(value, smooth3, fill=NA), 
               mean_share=rollmean(mean_share, smooth3, fill=NA))
      # Plot
      stock <- ggplot() +
        geom_line(data=paths_prob_best3, aes(x=draw, y=value, group=name), alpha=.3, color=color3.light) +
        geom_line(data=paths_prob_best3, aes(x=draw, y=mean_share), color=color3) +
        geom_line(data=paths_prob_best2, aes(x=draw, y=value, group=name), alpha=.3, color=color2.light) +
        geom_line(data=paths_prob_best2, aes(x=draw, y=mean_share), color=color2) +
        geom_line(data=paths_prob_best1, aes(x=draw, y=value, group=name), alpha=.3, color=color1.light) +
        geom_line(data=paths_prob_best1, aes(x=draw, y=mean_share), color=color1) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) + 
        labs(x="Draw", y="Prob of urns selecting best candidate in each round")
      
      ggplotly(stock) %>% style(hoverinfo = "skip", traces = c(1, 3, 5)) %>% 
        style(hovertemplate = paste('Simulation 1:<br>At draw %{x:.0f},',
                                    '<br>prob. of selecting best candidate is %{y:.0%}<br><extra></extra>'), traces = 6) %>%
        style(hovertemplate = paste('Simulation 2:<br>At draw %{x:.0f},',
                                    '<br>prob. of selecting best candidate is %{y:.0%}<br><extra></extra>'), traces = 4) %>%
        style(hovertemplate = paste('Simulation 3:<br>At draw %{x:.0f},',
                                    '<br>prob. of selecting best candidate is %{y:.0%}<br><extra></extra>'), traces = 2) %>%
        layout(hovermode="x unified)")
    })
  })
  
  ### Histogram of share of women in stock selected
  output$stock_composition_bar <- renderPlotly({
    
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_selected_rank1 <-  outputlist1$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% mutate(name=ifelse(!is.na(str_locate(name,"\\.")[,1]), substr(name, 1, str_locate(name,"\\.")[,1]-1), name)) %>%
        group_by(name) %>% summarize(share_best=mean(value==1)) %>%  ungroup()
      
      outputlist2 <- list_output2()
      paths_selected_rank2 <- outputlist2$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% mutate(name=ifelse(!is.na(str_locate(name,"\\.")[,1]), substr(name, 1, str_locate(name,"\\.")[,1]-1), name)) %>%
        group_by(name) %>% summarize(share_best=mean(value==1)) %>% ungroup()
 
      outputlist3 <- list_output3()
      paths_selected_rank3 <- outputlist3$paths_selected_rank %>% as.data.frame %>% mutate(draw=row_number()) %>% pivot_longer(-draw) %>% mutate(name=ifelse(!is.na(str_locate(name,"\\.")[,1]), substr(name, 1, str_locate(name,"\\.")[,1]-1), name)) %>%
        group_by(name) %>% summarize(share_best=mean(value==1)) %>% ungroup()
      
      # Plot
      plot_ly(x =~paths_selected_rank3$share_best,type="histogram",  nbinsx = sqrt(nrow(paths_selected_rank3)), marker=list(color=color3), opacity=.5)  %>%
        add_trace(x =~paths_selected_rank2$share_best,type="histogram",  nbinsx = sqrt(nrow(paths_selected_rank2)), marker=list(color=color2), opacity=.5, showlegend=FALSE) %>%
        add_trace(x =~paths_selected_rank1$share_best,type="histogram",  nbinsx = sqrt(nrow(paths_selected_rank1)), marker=list(color=color1), opacity=.5, showlegend=FALSE) %>%
        layout(xaxis=list(title = "Share of selected that are the best candidate after trials", range=c(0,1)), yaxis=list(title="Frequency")) %>%
        style(hovertemplate = paste('Simulation 3:<br>%{y:.0f} urns have <br>best candidate share','%{x} <extra></extra>'), traces = 1) %>%
        style(hovertemplate = paste('Simulation 2:<br>%{y:.0f} urns have <br>best candidate share','%{x} <extra></extra>'), traces = 2) %>%
        style(hovertemplate = paste('Simulation 1:<br>%{y:.0f} urns have <br>best candidate share','%{x} <extra></extra>'), traces = 3) %>%
        layout(hovermode='x unified)',
               barmode="overlay")
      
      
      
    })
    
  })
  
  ### Histogram of end of AA 
  output$hist_firstend <- renderPlotly({
    
    input$rerun
    
    isolate({
      # Get and prepare data
      outputlist1 <- list_output1()
      m1 <- mean(outputlist1$firstend, na.rm=T)
      num_end1 <- sum(!is.na(outputlist1$firstend))
      
      outputlist2 <- list_output2()
      m2 <- mean(outputlist2$firstend, na.rm=T)
      num_end2 <- sum(!is.na(outputlist2$firstend))
 
      outputlist3 <- list_output3()
      m3 <- mean(outputlist3$firstend, na.rm=T)
      num_end3 <- sum(!is.na(outputlist3$firstend))
      
      # Plot
      p <- plot_ly(x =~outputlist3$firstend,type="histogram", name="Freq.", marker=list(color=color3.light), opacity=.5, showlegend=FALSE) %>%
        add_trace(x =~outputlist2$firstend,type="histogram", name="Freq.", marker=list(color=color2.light), opacity=.5, showlegend=FALSE) %>%
        add_trace(x =~outputlist1$firstend,type="histogram", name="Freq.", marker=list(color=color1.light), opacity=.5, showlegend=FALSE) %>%
        layout(xaxis=list(title = paste("When AA ended<br>Simulation 1: AA ended in", num_end1, "out of", ncol(outputlist1$paths_ratio), "urns<br>Simulation 2: AA ended in", num_end2, "out of", ncol(outputlist2$paths_ratio), "urns"), range=c(0,max(nrow(outputlist1$paths_ratio), nrow(outputlist2$paths_ratio)))), yaxis=list(title="Frequency"))%>%
        layout(hovermode="x unified)", 
               barmode="overlay") 
      if (!is.na(m1) ){
       p<- p %>%  add_segments(x=m1, y=0, xend=m1, yend=100, line=list(color=color1, width = 4), opacity=1, marker=NULL, name="Mean", showlegend=FALSE) %>%
         style(hovertemplate = paste('Simulation 1:<br>Mean: %{x:.1f}'), traces = 4) %>%
         style(hovertemplate = paste('Simulation 1:<br>%{y:.0f} urns have ended<br>AA after','%{x} draws<extra></extra>'), traces =3 ) 
       
      }
      if (!is.na(m2) ){
        p<- p %>%  add_segments(x=m2, y=0, xend=m2, yend=100, line=list(color=color2, width = 4), opacity=1, marker=NULL, name="Mean", showlegend=FALSE) %>%
          style(hovertemplate = paste('Simulation 2:<br>Mean: %{x:.1f}'), traces = 5) %>%
          style(hovertemplate = paste('Simulation 2:<br>%{y:.0f} urns have ended<br>AA after','%{x} draws<extra></extra>'), traces =2)
        
        
      }
      if (!is.na(m3) ){
        p<- p %>%  add_segments(x=m3, y=0, xend=m3, yend=100, line=list(color=color3, width = 4), opacity=1, marker=NULL, name="Mean", showlegend=FALSE) %>%
          style(hovertemplate = paste('Simulation 3:<br>Mean: %{x:.1f}'), traces = 6) %>%
          style(hovertemplate = paste('Simulation 3:<br>%{y:.0f} urns have ended<br>AA after','%{x} draws<extra></extra>'), traces =1)
        
        
      }
      
      
      p
      
    })
    
  })  
  
  ### Plot of share of W over time, one urn only, urns have different colors 
  output$ratio_over_time2 <- renderPlotly({

    input$rerun
    
    isolate({
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      }
      # Get and prepare data
      outputlist1 <- list_output1()
      paths_ratio1 <- outputlist1$paths_ratio
      paths_ratio1 <- paths_ratio1 %>% as.data.frame %>% mutate(draw=row_number()) 
      ratio1 <- paths_ratio1 %>% pivot_longer(-draw)
      average_ratio1 <- paths_ratio1 %>% as.data.frame() %>% dplyr::select(-draw) %>% rowMeans()
      text1 <- paste0('Simulation 1: <br>After draw ', paths_ratio1$draw -1,', avg. urn has<br>',round(average_ratio1, digits=4)* 100, '% white balls')
      
      # Plot
      r <- ggplot() + 
        geom_line(data=ratio1, aes(x=draw-1, y=value, color=name), alpha=.9) +
        geom_point(aes(x=rep(0:(input$N1)), y=average_ratio1, text=text1), size=0.1, color=color1, alpha=0) +
        geom_line(aes(x=rep(0:(input$N1)), y=average_ratio1), color=color1) +
        geom_hline(aes(yintercept=0.5), color=coloreq,linetype = "solid") +
        scale_colour_brewer(palette = "Set3", type="qual", guide="none") +
        scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=12)) + 
        labs(title ="Share of white balls in urn over time, average highlighted", x="Draw", y="White balls' share in the urn ($r_t$)") +
        guides(color="none")
      
      
      ggplotly(r, tooltip="text") %>%
        layout(hovermode="x unified)") %>%
        style(showlegend=FALSE)
      
    })
  })  
  
  
  ### Dynamic graph title
  output$distribution_title<- renderText({
    input$rerun
    
    isolate({
      paste0(input$I, " Polya Urns over ", input$N, " trials, W_0=", input$w_0, ", M_0=", input$m_0)
      
    })})
  
  ### Replacement matrix
  output$matrix1 <- renderUI({
    input$rerun
    
    isolate({
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      }
      outputlist1 <- list_output1()
      parameters <- outputlist1$parameters
      withMathJax(
        if(input$woman_stochastic1=="none" & input$man_stochastic1=="none"){
          paste0("Simulation 1 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]],
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]],"\\end{pmatrix}$$")
        }
        else if(input$woman_stochastic1!="none" & input$man_stochastic1=="none"){
          paste0("Simulation 1 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "*", parameters[["w_w_function"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]], "*(", parameters[["m_w_function"]], ")",
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]],"\\end{pmatrix}$$")
        }
        
        else if(input$woman_stochastic1!="none" & input$man_stochastic1!="none"){
          paste0("Simulation 1 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "*", parameters[["w_w_function"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]], "*(", parameters[["m_w_function"]], ")",
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "*", parameters[["w_m_function"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]], "*(", parameters[["m_m_function"]], ") \\end{pmatrix}$$")
        }   
        else if(input$woman_stochastic1=="none" & input$man_stochastic1!="none"){
          paste0("Simulation 1 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]],
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "*", parameters[["w_m_function"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]], "*(", parameters[["m_m_function"]], ") \\end{pmatrix}$$")
        }        
      )   
      
      
    })
  })
  
  output$matrix2 <- renderUI({
    input$rerun
    
    isolate({
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      }
      
      outputlist2 <- list_output2()
      parameters <- outputlist2$parameters
      if(input$enable2s == TRUE){
      withMathJax(
        if(input$woman_stochastic2=="none" & input$man_stochastic2=="none"){
          paste0("Simulation 2 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]],
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]],"\\end{pmatrix}$$")
        }
        else if(input$woman_stochastic2!="none" & input$man_stochastic2=="none"){
          paste0("Simulation 2 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "*", parameters[["w_w_function"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]], "*(", parameters[["m_w_function"]], ")",
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]],"\\end{pmatrix}$$")
        }
        
        else if(input$woman_stochastic2!="none" & input$man_stochastic2!="none"){
          paste0("Simulation 2 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "*", parameters[["w_w_function"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]], "*(", parameters[["m_w_function"]], ")",
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "*", parameters[["w_m_function"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]], "*(", parameters[["m_m_function"]], ") \\end{pmatrix}$$")
        }   
        else if(input$woman_stochastic2=="none" & input$man_stochastic2!="none"){
          paste0("Simulation 2 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]],
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "*", parameters[["w_m_function"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]], "*(", parameters[["m_m_function"]], ") \\end{pmatrix}$$")
        }        
      )   
      }
      
    })
  })
  
  
  output$matrix3 <- renderUI({
    input$rerun
    
    isolate({
      # Check that inputs are in place
      t <- input$N1
      if(is.null(t)){
        input <- default_inputs
      }
      
      outputlist3 <- list_output3()
      parameters <- outputlist3$parameters
      
      if(input$enable3s == TRUE){
      withMathJax(
        if(input$woman_stochastic3=="none" & input$man_stochastic3=="none"){
          paste0("Simulation 3 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]],
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]],"\\end{pmatrix}$$")
        }
        else if(input$woman_stochastic3!="none" & input$man_stochastic3=="none"){
          paste0("Simulation 3 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "*", parameters[["w_w_function"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]], "*(", parameters[["m_w_function"]], ")",
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]],"\\end{pmatrix}$$")
        }
        
        else if(input$woman_stochastic3!="none" & input$man_stochastic3!="none"){
          paste0("Simulation 3 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "*", parameters[["w_w_function"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]], "*(", parameters[["m_w_function"]], ")",
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "*", parameters[["w_m_function"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]], "*(", parameters[["m_m_function"]], ") \\end{pmatrix}$$")
        }   
        else if(input$woman_stochastic3=="none" & input$man_stochastic3!="none"){
          paste0("Simulation 3 ball replacement matrix: $$\\begin{pmatrix}", parameters[["w_w_added"]]-parameters[["w_w_removed"]], "&", parameters[["m_w_added"]]-parameters[["m_w_removed"]],
                 "\\\\",parameters[["w_m_added"]]-parameters[["w_m_removed"]], "*", parameters[["w_m_function"]], "&", parameters[["m_m_added"]]-parameters[["m_m_removed"]], "*(", parameters[["m_m_function"]], ") \\end{pmatrix}$$")
        }        
      )   
      }
      
    })
  })
  ### Help text
  output$help <-renderUI({
    input$rerun
    
  isolate({
      outputlist <- list_output1()
      parameters <- outputlist$parameters
      withMathJax(
        HTML(paste0(
          h2("About the Urn Simulator"),
          p("Updated February 2026."), 
          p("This simulator accompanies the paper \"Women, Men, and Polya Urns. Underrepresentation at Equal Talent in the Absence of Discrimination\" by Laura Caron, Alessandra Casella, and Victoria Mooers at Columbia University."),
          p("Note: you need to press \"re-run\" simulation in order to refresh the results. Some graphs may be slow to appear."),
          h3("Simulation Parameters"),
          p("You may choose the number of rounds from each urn and the number of urns to be simulated. You may also change the seed for the random number generator in order to get different results."),
          h3("Initial Urn Contents"), 
          p("Input the initial number of each color ball in the urn, \\(w_0\\) and \\(m_0\\)."),
          h3("Addition Scheme"), 
          p("At each \\(t\\), let the urn contents be represented by \\(w_t\\) white balls and \\(m_t\\) mauve balls. For the case of a single draw, you may specify the ball addition matrix in the form:
  $$\\begin{pmatrix} d_{ww} & d_{wm} \\\\ d_{mw} & d_{mm} \\end{pmatrix} $$
  
  where \\(d_{wm}\\) represents the number of M added when a W is drawn, and so on. All urns perform draws with replacement. To specify draws without replacement, choose a negative value for the number of women or men to be added after each draw. "),
          h3("Affirmative Action"), 
          p("The app allows simulation of various affirmative action policies. The first type is one where two balls are drawn from the urn, representing the best and second-best candidates. If the best candidate is a woman, they are selected. If not, the second-best candidate is considered and is selected if they are a woman (surely, in the determinisitc case, or with a certain probability, in the stochastic case). Otherwise, the best man candidate is selected."), 
          p("The second type of affirmative action adds one woman to the urn in every round, regardless of draw and addition."), 
          p("These two types can also be simulated using the multiple draw options."),
          p("The third type is a hiring quota, where women are selected every \\(k\\) draws until the stopping conditions are met. When necessary, we continue drawing until a woman is selected."), 
          h4("Stopping conditions"), 
          p("The affirmative action may continue forever, stop when W become the majority in the sample (urn), stop when W become the majority among those selected, or stop after a certain number of draws."),
          h3("Stochastic addition options"), 
          
          HTML(paste0(
            #"Stochastic addition is currently:<b>", if(input$woman_stochastic!="none") " enabled " else " disabled", "</b> when W is drawn and <b>",if(input$man_stochastic!="none") " enabled " else " disabled ", "</b> when M is drawn. <br></br>", 
                      "The stochastic addition may be either correlated or uncorrelated. In the correlated variety, balls are always added to the urn in each draw, regardless of the state of any random variables. The 2 \\(\\times\\) 2 case can be written in terms of 2 Bernoulli random variables:

$$

\\begin{pmatrix}
d_{ww} \\times X \\sim Bern(p) & d_{wm} (1-X) \\\\
d_{mw} \\times Y \\sim Bern(q) & d_{mm} (1-Y)
\\end{pmatrix}

$$", 
                      "In that case, when a white ball is drawn, \\(p\\) gives the probability that \\(d_{ww}\\) white balls are added. Otherwise, \\(d_{wm}\\) mauve balls are added. <br></br>",
                      #"Correlated stochastic addition is currently: <b>", if(input$woman_stochastic=="balanced") " enabled" else " disabled", "</b> when a woman is drawn and <b>",if(input$man_stochastic=="balanced") " enabled " else " disabled ", "</b>when a man is drawn. <br></br>", 
                      
                      "The case with uncorrelated addition is written in terms of four Bernoulli random variables:

$$

\\begin{pmatrix}
d_{ww} \\times Bern(p_{ww}) & d_{wm} \\times Bern(p_{wm})\\\\
d_{mw} \\times Bern(p_{mw}) & d_{mm} \\times Bern(p_{mm})
\\end{pmatrix}

$$",
                      
"Stochastic addition may depend on the selected candidates. In this case, the idea is that, e.g., the probability \\(p_{ww}\\) is a function of the share of W among the candidates previous selected. As a probability, this function should be bounded between 0 and 1 for shares of white balls between 0 and 1.

The most direct of such functions would be:
  $$p_{ww} = c-b\\rho_t^a $$
  where \\(\\rho_t = \\frac{\\hat{w}_t}{t}\\) (the share of W among selected candidates) with parameters \\(a, b, c\\). We could also imagine various nonlinear functions, such as 

$$p_{w_w} = \\frac{1}{1+b \\rho_t^a}$$
  with parameters \\(a,b \\in(0,\\infty) \\).

Another possiblity:
  $$p_{w_w} = \\frac{1}{1+b \\exp(c*\\rho_t)}$$
  with parameters \\(b,c \\in(0,\\infty) \\).

As above, this may be done in the correlated or uncorrelated case. "
                      
                      
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
