library(shiny)
library(plotly)
library(shinythemes)
library(rsconnect)



# Define UI for application that draws a histogram
shinyUI(fluidPage(theme=shinytheme("flatly"),
  # Application title
  navbarPage("Polya Urns", id="nav",
  tabPanel("About", 
           fluidRow(column(8, uiOutput("help"), offset=2 ))
           ),
  tabPanel("Simulations",           
  sidebarLayout(
    sidebarPanel(
      withMathJax(),
      h3("Simulation Parameters"),
      fluidRow(
        column(6, numericInput("I", "Number of urns to simulate", 50, min=1, step=5)),
        column(6, numericInput("N", "Number of draws from each urn", 100, min=1, step=10))),
      fluidRow(
        column(12, numericInput("seed", "Seed", 1234, min=0))),
      h3("Initial Urn Contents"),
      fluidRow(
        column(5, numericInput("w_0", "Initial number of women (white balls)", 1, min=0)), 
        column(5, numericInput("m_0", "Initial number of men (maroon balls)", 1, min=0))),
      h3("Replacement Scheme"),
      h4("Note: draws are with replacement."),
      h4("If a woman is drawn:"),
      fluidRow(column(12, radioButtons("woman_stochastic", label=NULL, choices=c("Deterministic replacement"="none", "Stochastic replacement (dependent)" = "balanced", "Stochastic replacement (independent)"= "unbalanced"), selected="none"))),
      conditionalPanel(condition = "input.woman_stochastic!= 'none'",
       fluidRow(                       
         column(8, checkboxInput("woman_depends", "Depends on urn contents?", value=FALSE))),
       fluidRow(
         conditionalPanel(condition="input.woman_depends==false",
            column(5, numericInput("p_w_w", "Probability of women added", 1, step=0.1))),
         conditionalPanel(condition="input.woman_depends==true",
            column(5, radioButtons("w_w_function", label="Probability of women added", choices=c("\\(p_{w_w} = 1-b*(share_w)^a\\)"="linear","\\(p_{w_w} = \\frac{1}{1+b*(share_w)^a}\\)"="inverse", "\\(p_{w_w} = \\frac{1}{1+b*\\exp(c*share_w)}\\)"="inverseexp"))))
       ),
       fluidRow(
            conditionalPanel(condition="input.w_w_function!='inverseexp'&input.woman_depends==true", 
              column(5, numericInput("w_w_a", "\\(a\\)", 1, step = 0.1)), 
              column(5, numericInput("w_w_b", "\\(b\\)", 1, step=0.1))), 
            conditionalPanel(condition="input.w_w_function=='inverseexp'", 
               column(5, numericInput("w_w_b", "\\(b\\)", 1, step=0.1)), 
               column(5, numericInput("w_w_c", "\\(c\\)", 1, step=0.1)))  ) ,
       
            conditionalPanel(condition="input.woman_stochastic=='unbalanced'",
        fluidRow(
            conditionalPanel(condition="input.woman_depends==false",
               column(5, numericInput("p_m_w", "Probability of men added", 1, step=0.1))),
            conditionalPanel(condition="input.woman_depends==true",
              column(5, radioButtons("m_w_function", label="Probability of men added", choices=c("\\(p_{m_w} = 1-b*(share_m)^a\\)"="linear","\\(p_{m_w} = \\frac{1}{1+b*(share_m)^a}\\)"="inverse", "\\(p_{m_w} = \\frac{1}{1+b*\\exp(c*share_m)}\\)"="inverseexp"))))
         )),
       fluidRow(
         conditionalPanel(condition="input.m_w_function!='inverseexp'&input.woman_depends==true & input.woman_stochastic=='unbalanced'", 
            column(5, numericInput("m_w_a", "\\(a\\)", 1, step = 0.1)), 
            column(5, numericInput("m_w_b", "\\(b\\)", 1, step=0.1))), 
         conditionalPanel(condition="input.m_w_function=='inverseexp'", 
                          column(5, numericInput("m_w_b", "\\(b\\)", 1, step=0.1)), 
                          column(5, numericInput("m_w_c", "\\(c\\)", 1, step=0.1)))  )            
         ),
      fluidRow(
         column(5, numericInput("w_w", "Number of women added", 1)), 
         column(5, numericInput("m_w", "Number of men added", 0))
        ),
      h4("If a man is drawn:"),
      fluidRow(column(12, radioButtons("man_stochastic", label=NULL, choices=c("Deterministic replacement"="none", "Stochastic replacement (dependent)" = "balanced", "Stochastic replacement (independent)"= "unbalanced"), selected="none"))),
      conditionalPanel(condition = "input.man_stochastic!= 'none'",
        fluidRow(                       
          column(8, checkboxInput("man_depends", "Depends on urn contents?", value=FALSE))),
        fluidRow(
          conditionalPanel(condition="input.man_depends==false",
                           column(5, numericInput("p_w_m", "Probability of women added", 1, step=0.1))),
          conditionalPanel(condition="input.man_depends==true",
                           column(5, radioButtons("w_m_function", label="Probability of women added", choices=c("\\(p_{w_m} = 1-b*(share_w)^a\\)"="linear","\\(p_{w_m} = \\frac{1}{1+b*(share_w)^a}\\)"="inverse", "\\(p_{w_m} = \\frac{1}{1+b*\\exp(c*share_w)}\\)"="inverseexp"))))
        ),
        fluidRow(
          conditionalPanel(condition="input.w_m_function!='inverseexp'&input.man_depends==true", 
                           column(5, numericInput("w_m_a", "\\(a\\)", 1, step = 0.1)), 
                           column(5, numericInput("w_m_b", "\\(b\\)", 1, step=0.1))), 
          conditionalPanel(condition="input.w_m_function=='inverseexp'", 
                           column(5, numericInput("w_m_b", "\\(b\\)", 1, step=0.1)), 
                           column(5, numericInput("w_m_c", "\\(c\\)", 1, step=0.1)))  ) ,
        
        conditionalPanel(condition="input.man_stochastic=='unbalanced'",
                         fluidRow(
                           conditionalPanel(condition="input.man_depends==false",
                                            column(5, numericInput("p_m_m", "Probability of men added", 1, step=0.1))),
                           conditionalPanel(condition="input.man_depends==true",
                                            column(5, radioButtons("m_m_function", label="Probability of men added", choices=c("\\(p_{m_m} = 1-b*(share_m)^a\\)"="linear","\\(p_{m_m} = \\frac{1}{1+b*(share_m)^a}\\)"="inverse", "\\(p_{m_m} = \\frac{1}{1+b*\\exp(c*share_m)}\\)"="inverseexp"))))
                         )),
        fluidRow(
          conditionalPanel(condition="input.m_m_function!='inverseexp'&input.man_depends==true & input.man_stochastic=='unbalanced'", 
                           column(5, numericInput("m_m_a", "\\(a\\)", 1, step = 0.1)), 
                           column(5, numericInput("m_m_b", "\\(b\\)", 1, step=0.1))), 
          conditionalPanel(condition="input.m_m_function=='inverseexp'", 
                           column(5, numericInput("m_m_b", "\\(b\\)", 1, step=0.1)), 
                           column(5, numericInput("m_m_c", "\\(c\\)", 1, step=0.1)))  )            
      ),
      fluidRow(
        column(5, numericInput("w_m", "Number of women added", 0)), 
        column(5, numericInput("m_m", "Number of men added", 1))
      ),   
      h3("Interventions"),
      fluidRow(column(12, radioButtons("intervention", "Affirmative Action", selected="none", choices=c("None"="none", "Draw two, if at least one is a woman, select a woman (deterministic)"="atleast","Draw two, if at least one is a woman, select a woman with probability"="atleast_stochastic","Draw one and replace accordingly, plus always replace one woman each round"="alwayswoman")))),
               conditionalPanel(condition="input.intervention=='atleast_stochastic'", 
                                fluidRow(column(12, numericInput("prob_atleast", "Probability of selecting second-best woman", value=1, min=0, max=1, step=0.1)))),
      fluidRow(column(12, conditionalPanel(condition = "input.intervention != 'none'",
        radioButtons("stopintervention", "When to stop?", selected="continue", choices=c("Continue forever"="continue", "Stop if women majority"="balanced"))))
      ), 
      h3("Graph options"), 
      fluidRow(column(12,
              radioButtons("graph_auto", "Dimensions", choices=c("Automatic"="auto", "Custom"="custom")),
                fluidRow(conditionalPanel(condition="input.graph_auto=='custom'",
                          column(6, numericInput("graph_dim", "Graph Dimensions", value=100, min=0,step=50)),
                          column(6, numericInput("graph_origin", "Origin", value=0, min=0,step=10))))
      )),
      
      fluidRow(column(5, actionButton("rerun", "Re-run Simulation")))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Distribution of Share of Women", 
          fluidRow(column(6,plotlyOutput("histogram", height="50%")),
                   column(6, plotlyOutput("ratio_over_time", height="50%"))),
          fluidRow(column(6, plotlyOutput("density", height="50%")),
                   column(6, plotlyOutput("cdf", height="50%")))),
        tabPanel("Urn Paths Over Time", 
          fluidRow(uiOutput("distribution_title")),
          fluidRow(column(9, plotlyOutput("rayplot", height="50%"))),
          fluidRow(column(8, plotlyOutput("prob_w_over_time", height="50%")))
        ),
        tabPanel("Selected Candidates", 
          fluidRow(column(9, plotlyOutput("stockplot", height="50%"))),
          fluidRow(column(9, plotlyOutput("stock_composition", height="50%"))),
          fluidRow(column(9, plotlyOutput("stock_composition_bar", height="50%")))
          
        ),
        tabPanel("About the Urn",
          uiOutput("matrix")
                 )
    )
  ))
  )
, selected ="Simulations")))