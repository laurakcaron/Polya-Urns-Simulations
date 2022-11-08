##################################################
#              Polya Urns Simulations
#              Laura Caron
#              Columbia University
#         This version: November 2, 2022
##################################################

##################################################
#                      Set up
#           Loads the packages we need
##################################################


## Set up renv for package version control
if (!require("remotes"))
  install.packages("remotes")

#library(renv)
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
                h4("Note: draws are with replacement."),
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
                ),
                
               #options for replacement that depends on stock of selected candidates -- only appear when selected
    
    
    
                # Fourth section: Interventions
                h3("Interventions"),
                fluidRow(column(12, radioButtons("intervention", "Affirmative Action", selected="none", choices=c("None"="none", "Draw two, if at least one is a woman, select a woman (deterministic)"="atleast","Draw two, if at least one is a woman, select a woman with probability"="atleast_stochastic","Draw one and add accordingly, plus always add one woman each round"="alwayswoman", "Hiring quota"="quota")))),
                conditionalPanel(condition="input.intervention=='atleast_stochastic'", 
                     fluidRow(column(12, numericInput("prob_atleast", "Probability of selecting second-best woman", value=1, min=0, max=1, step=0.1)))),
                conditionalPanel(condition="input.intervention=='quota'", 
                                 column(6, numericInput("quota", "Select only women until they make up __ of the pool of selected candidates", value=0.5, min=0, max=1, step=0.1)),
                                 column(6, numericInput("quota_start", "Start after draw (enter 0 for start at beginning)", value=0, min=0, step=1))),
                     fluidRow(column(12, conditionalPanel(condition = "input.intervention != 'none' & input.intervention !='quota'",
                         radioButtons("stopintervention", "When to stop?", selected="continue", choices=c("Continue forever"="continue", "Stop if white balls majority in urn"="majority","Stop if white balls majority among selected for this urn"="majority_selected", "Stop after X draws"="temp")),
                         column(6, numericInput("aa_start", "Start after draw (enter 0 for start at beginning)", value=0, min=0, step=1))))
                ), 
                fluidRow(column(12, conditionalPanel(condition="input.stopintervention=='temp'& input.intervention!='none' & input.intervention!='quota'", numericInput("stopafter", "Stop after", 30)))),
                
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
                           fluidRow(column(6,plotlyOutput("histogram", height="50%")),
                                    column(6, plotlyOutput("ratio_over_time", height="50%"))),
                           # density plot and cdf
                           fluidRow(column(6, plotlyOutput("density", height="50%")),
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
                           # Stock of selected women and men
                           fluidRow(column(9, plotlyOutput("stockplot", height="50%"))),
                           # Share of women in the stock of selected candidates
                           fluidRow(column(9, plotlyOutput("stock_composition", height="50%"))),
                           # Histogram of share of women in the stock of selected candidates
                           fluidRow(column(9, plotlyOutput("stock_composition_bar", height="50%")))
                           
                  ),
                  # Fourth tab
                  tabPanel("About the Urn",
                           # Replacement matrix
                           uiOutput("matrix")
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
      
      stopintervention <- ifelse(input$intervention == "none" | input$intervention=="quota", "na", input$stopintervention)
      
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
      withProgress(message = 'Running simluation', value = 0, {
        # Main simulation loop
          
          # Reset the urns to initial state
          # Each column is one urn, one row for each ball 
          urn <- matrix(rep(c(rep("w", w_0),rep("m", m_0)), I), ncol = I, byrow=FALSE)
          
          # Initialize some vectors
          w_n <- rep(w_0, I)
          m_n <- rep(m_0, I)
          prob_w_n <- NULL
          prob_w_w_replace_n <- NULL
          prob_w_m_replace_n <- NULL
          selected <- NULL
          selected_w <- NULL
          selected_m <- NULL
          selected_rank <- NULL
          end <- NA 
          
          for (n in 1:(N)){
            # Save the previous number of women and men and share
            previous_w <- colSums(urn=="w", na.rm=T)
            previous_m <- colSums(urn=="m", na.rm=T)
            previous_share <- previous_w/(previous_w+previous_m)
            previous_share_selected = if(n == 1) previous_share else selected_w[n-1,]/(selected_w[n-1,] + selected_m[n-1,])
            
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
                     ifelse(input$intervention=="quota"& (previous_share_selected > input$quota | end %in% 1) & n > input$quota_start,1, 
                          ifelse(stopintervention=="continue" & !is.na(previous_share), 0, 
                            ifelse(stopintervention=="majority" & (previous_share>=0.5 | end %in% 1), 1, 
                              ifelse(stopintervention=="majority_selected" & (previous_share_selected >=0.5 | end %in% 1)& n > input$aa_start, 1,
                              ifelse(stopintervention=="temp" & n > input$stopafter & !is.na(previous_share), 1, 0))))))
             
            # Probability of woman selected
            prob_w_selected <- ifelse((input$intervention=="atleast" & (end %in% 0) & n > input$aa_start),(1-(1-previous_share)^2) , 
                                 ifelse((input$intervention=="atleast_stochastic" & (end %in% 0) & n > input$aa_start),previous_share+(1-previous_share)*(previous_share)*input$prob_atleast,
                                    ifelse(input$intervention=="quota"& (end %in% 0) & n > input$quota_start, 1, previous_share)))
            
            # Random draws done ahead of time for the case of balanced replacement
            r_w_w <- sapply(seq(1:I), function(x) rbinom(1,1,p_w_w[x]))
            r_w_m <- sapply(seq(1:I), function(x) rbinom(1,1,p_w_m[x]))
            r_m_w <- if(input$woman_stochastic=="balanced") 1-r_w_w else sapply(seq(1:I), function(x) rbinom(1,1,p_m_w[x]))
            r_m_m <- if(input$man_stochastic=="balanced") 1-r_w_m else sapply(seq(1:I), function(x) rbinom(1,1,p_m_m[x]))
            
            rank <- rep(1, I)
            ####
            # Draw, replace, remove balls for each AA case
            
            if (1==1){
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
                                     
            if (input$intervention=="atleast" & n > input$aa_start){
              ball_drawn_aa <- apply(urn,2, function(x) sample(na.omit(x),2, replace =TRUE) )
              
              rank_aa <- apply(ball_drawn_aa, 2, function(x) ifelse("w" %in% x, min(which(x=="w")), 1) )
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
            if (input$intervention=="atleast_stochastic" & n > input$aa_start){
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
            if (input$intervention=="alwayswoman" ){
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
            if (input$intervention=="quota" & n>input$quota_start){
              ball_drawn_aa  <- apply(urn,2, function(x) sample(x,nrow(urn)) )
              rank_aa <- apply(ball_drawn_aa, 2, function(x) min(which(x=="w")) )
              ball_drawn_aa <- sapply(seq(1:ncol(urn)), function(x) ball_drawn_aa[rank_aa[x],x])
             
              
              ball_replaced_aa <- sapply(seq(1:I), function(x){
                rball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_added*r_w_w[x]), rep("m", m_w_added*r_m_w[x])) else c(rep("w", w_m_added*r_w_m[x]), rep("m", m_m_added*r_m_m[x]))
                if(is_empty(rball)) 0 else rball
              })
              
              ball_removed_aa <- sapply(seq(1:I), function(x){
                mball <- if(ball_drawn_aa[x] == "w") c(rep("w", w_w_removed*r_w_w[x]), rep("m", m_w_removed*r_m_w[x])) else c(rep("w", w_m_removed*r_w_m[x]), rep("m", m_m_removed*r_m_m[x]))
                if(is_empty(mball)) 0 else mball
              })
              
            
            }
            
            # For urns undergoing AA, use those draws instead 
            print("d")
            
            if (input$intervention != "none"){
            ball_drawn <- ifelse(end %in% 0, ball_drawn_aa, ball_drawn)
            ball_replaced_aa <- if(!("list" %in% class(ball_replaced_aa))) sapply(ball_replaced_aa, list) else ball_replaced_aa
            ball_replaced <- sapply(seq(1:I), function(x) if(end[x] %in% 0) ball_replaced_aa[[x]] else ball_replaced[x])

            ball_removed_aa <- if(!("list" %in% class(ball_removed_aa))) sapply(ball_removed_aa, list) else ball_removed_aa
            ball_removed <- sapply(seq(1:I), function(x) if(end[x] %in% 0) ball_removed_aa[[x]] else ball_removed[x])
            
            rank <- ifelse(end %in% 0, rank_aa, rank)
            }
            # Save results 
            
            new_w <- sapply(seq(1:I), function(x) previous_w[x] + sum(ball_replaced[[x]]=="w") - sum(ball_removed[[x]]=="w"))
            new_w <- ifelse(new_w <0, 0, new_w)
            new_m <- sapply(seq(1:I), function(x) previous_m[x] + sum(ball_replaced[[x]]=="m") - sum(ball_removed[[x]]=="m"))
            new_m <- ifelse(new_m<0, 0 , new_m)
            urn_l <- lapply(seq(1:I), function(x) matrix(c(rep("w", new_w[x]), rep("m", new_m[x])), ncol=1) )
            urn <- matrix(nrow= max(unlist(lapply(urn_l, nrow))), ncol=I)
            for (i in 1:I) {
              urn[1:length(urn_l[[i]]),i] <- urn_l[[i]]
            }
            
            selected <- rbind(selected, ball_drawn)
            selected_rank <- if(n > 1) rbind(selected_rank, rank) else rank
            selected_w <- rbind(selected_w, sapply(seq(1:I), function(x) sum(selected[, x]=="w")))
            selected_m <- rbind(selected_m, sapply(seq(1:I), function(x) sum(selected[, x]=="m")))

            w_n <- rbind(w_n, new_w)
            m_n <- rbind(m_n, new_m)
            prob_w_n <- rbind(prob_w_n, prob_w_selected)
            prob_w_w_replace_n<- rbind(prob_w_w_replace_n, p_w_w)
            prob_w_m_replace_n<- rbind(prob_w_m_replace_n, p_w_m)
            

            incProgress(1/N, detail = paste("Percent completed", n*100/N, "%"))
            
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
          
          paths_prob_w_w_replace_n <- prob_w_w_replace_n
          colnames(paths_prob_w_w_replace_n) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_prob_w_w_replace_n) <- NULL

          paths_prob_w_m_replace_n <- prob_w_m_replace_n
          colnames(paths_prob_w_m_replace_n) = sapply(seq(1:I), function(x) paste0("Urn", x))
          rownames(paths_prob_w_m_replace_n) <- NULL     
          
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
      outputlist <- list(paths_ratio=paths_ratio, paths_w_n=paths_w_n, paths_m_n=paths_m_n, paths_prob_w_w_replace_n=paths_prob_w_w_replace_n, paths_prob_w_m_replace_n=paths_prob_w_m_replace_n, paths_prob_w_n=paths_prob_w_n, paths_selected=paths_selected, paths_selected_rank=paths_selected_rank, paths_selected_w=paths_selected_w, paths_selected_m=paths_selected_m, parameters=parameters)
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
      hist <- ggplot(hist_data) + 
        geom_histogram(aes(x=`Share of women in urn after trials`), bins=sqrt(input$N))+
        scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) + 
        labs(title ="Distribution of final share of white balls in the urn")
      
      plot_ly(x=~hist_data$`Share of white balls in urn after trials`, type="histogram",  histfunc="count" ,nbinsx = sqrt(input$I)) %>%
        layout(xaxis=list(title = "Share of white balls in the urn after trials", range=c(0,1)), yaxis=list(title="Frequency", titlefont = list(size = 16)))
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
        layout(xaxis=list(title = "Share of white balls in the urn after trials", range=c(0,1)), yaxis=list(title="Density"), hovermode="x unified")
      
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
        stat_ecdf(aes(x=`Share of white balls in urn after trials`), geom="step")+
        geom_vline(xintercept=0.5, color="chartreuse3", linetype="dashed", alpha=0.5)+
        scale_x_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) + 
        labs(y="Fraction of urns", title ="CDF of final share of white balls in the urn")
      
      ggplotly(cdf) %>% layout(hovermode="x unified")
      
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
      
      # Plot
      r <- ggplot() + 
        geom_line(data=ratio, aes(x=draw-1, y=value, group=name), color="lightgray") +
        geom_line(aes(x=rep(0:(input$N)), y=average_ratio), color="blue") +
        geom_hline(aes(yintercept=0.5), color="chartreuse3",linetype = "dashed" ) +
        scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))+
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
        ) + 
        labs(title ="Share of white balls in urn over time, average highlighted", x="Draw", y="White balls' share in the urn")
      
      
      
      ggplotly(r) %>% style(hoverinfo = "skip", traces = 1) %>%
        style(hovertemplate = paste('Draw: %{x:.0f}',
                                    '<br>Average share of white balls in urn: %{y:.4f}<br>'), traces = 2) %>%
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
      
      ggplotly(r) %>% style(hoverinfo = "skip", traces = 1) %>%
        style(hovertemplate = paste('Draw: %{x:.0f}',
                                    '<br>Average P(select white ball): %{y:.4f}<br>'), traces = 2) %>%
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
        labs(x="Number of white balls", y="Number of maroon balls",title = "Evolution of urn contents")
      
      
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
        style(hovertemplate = paste('Draw: %{x:.0f}',
                                    '<br>%{y:.4f}<br>'), traces = 2) %>%
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
        layout(xaxis=list(title = "Share of selected that are the best candidate after trials", range=c(0,1)), yaxis=list(title="Density"))
      
      
      
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
