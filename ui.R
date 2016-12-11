#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny) #this package is the one that allows the web app to exist

shinyUI(fluidPage(
  titlePanel("BIOL201 Population simulations"),
  tabsetPanel(type="tabs",
              tabPanel("Lotka-Volterra Competition",
                       h3("Population Parameters"),
                       fluidRow(
                         column(3, wellPanel(
                           h4("Population 1:"),
                           numericInput("LVC.pop1.n", label=HTML(paste("Starting population size, N", paste(tags$sub(1), "(0):", sep=""), sep="")), 10),
                           numericInput("LVC.pop1.r", label=HTML(paste("Intrinsic growth rate, r", paste(tags$sub(1), ":", sep=""), sep="")), .9, step=.1),
                           numericInput("LVC.pop1.K", label=HTML(paste("Carrying capacity, K", paste(tags$sub(1), ":", sep=""), sep="")), 500),
                           numericInput("LVC.alpha12", label=HTML(paste("Competition coefficient, alpha",paste(tags$sub(12), ":", sep=""),sep="")), .6, step=.1)
                         )),
                         column(3, wellPanel(
                           h4("Population 2:"),
                           numericInput("LVC.pop2.n", label=HTML(paste("Starting population size, N", paste(tags$sub(2), "(0):", sep=""), sep="")), 20),
                           numericInput("LVC.pop2.r", label=HTML(paste("Intrinsic growth rate, r", paste(tags$sub(2), ":", sep=""), sep="")), .5, step=.1),
                           numericInput("LVC.pop2.K", label=HTML(paste("Carrying capacity, K", paste(tags$sub(2), ":", sep=""), sep="")), 700),
                           numericInput("LVC.alpha21", label=HTML(paste("Competition coefficient, alpha",paste(tags$sub(21), ":", sep=""),sep="")), .7, step=.1)
                         )),
                         column(3, wellPanel(
                           h4("Run time:"),
                           numericInput("LVC.time", label="Number of generations to run:", 100)),
                           submitButton("Update graphs")
                         )),
                       h3("Population trajectories"),
                       fluidRow(
                         column(6, h4("Population sizes over time"),plotOutput("LVC.plot")),
                         column(6, h4("Phase plane with combined trajectory"), plotOutput("LVC.phaseplane"))
                       )
              ),
              tabPanel("Single population growth",
                       sidebarLayout(
                         sidebarPanel(h3("Population parameters")),
                         mainPanel(h3("Population trajectories"))
                       )),
              tabPanel("Predator-prey",
                       sidebarLayout(
                         sidebarPanel(h3("Population parameters")),
                         mainPanel(h3("Population trajectory"))
                       ))
  )
))


