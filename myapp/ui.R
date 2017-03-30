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
  titlePanel("Population Simulations for Introductory Ecology"),
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
                       ),
                       p(em("Developed by"), a(href="http://audreylkelly.web.unc.edu", em("Audrey L. Kelly")))
              ),
              tabPanel("Single population growth",
                       tabsetPanel(type='tabs',
                                   tabPanel("Density-Independent Growth (continuous exponential)",
                                            h3("Population Parameters"),
                                            fluidRow(
                                              column(3, wellPanel(
                                                h4("Population 1:"),
                                                numericInput("DIG.pop1.n", label=HTML(paste("Starting population size, N", paste(tags$sub(1), "(0)", sep=""), sep="")), 10),
                                                numericInput("DIG.pop1.r", label=HTML(paste("Intrinsic growth rate, r", paste(tags$sub(1), ":", sep=""), sep="")), .1)
                                                )),
                                              column(3, wellPanel(
                                                h4("Population 2:"),
                                                numericInput("DIG.pop2.n", label=HTML(paste("Starting population size, N", paste(tags$sub(2), "(0)", sep=""), sep="")), 10),
                                                numericInput("DIG.pop2.r", label=HTML(paste("Intrinsic growth rate, r", paste(tags$sub(2), ":", sep=""), sep="")), .1)
                                              )),
                                              column(3, wellPanel(
                                                h4("Run time:"),
                                                numericInput("DIG.time", label="Number of generations to run:", 25),
                                                h4("Populations to graph:"),
                                                checkboxInput("DIG.pop1.check", label="Population 1", value=TRUE),
                                                checkboxInput("DIG.pop2.check", label="Population 2", value=FALSE),
                                                submitButton("Update graphs")
                                              ))
                                            ),
                                            h3("Population Trajectories"),
                                            fluidRow(
                                              column(6, h4("Population size versus time"), plotOutput("DIG.plot.NvT")),
                                              column(6, h4("Population growth rate versus population size"), plotOutput("DIG.plot.dNdTvN"))
                                            ),
                                            fluidRow(
                                              column(6, h4("Log population size versus time"), plotOutput("DIG.plot.logNvT")),
                                              column(6, h4("Population size-corrected growth rate versus population size"), plotOutput("DIG.plot.dNNdTvN"))
                                            )
                                            ),
                                   tabPanel("Density-Dependent Growth (continuous logistic)",
                                            h3("Population Parameters"),
                                            fluidRow(
                                              column(3, wellPanel(
                                                h4("Population 1:"),
                                                numericInput("DDG.pop1.n", label=HTML(paste("Starting population size, N", paste(tags$sub(1), "(0):", sep=""), sep="")), 10),
                                                numericInput("DDG.pop1.r", label=HTML(paste("Intrinsic growth rate, r", paste(tags$sub(1), ":", sep=""), sep="")), .1),
                                                numericInput("DDG.pop1.K", label=HTML(paste("Carrying capacity, K", paste(tags$sub(1), ":", sep=""), sep="")), 250)
                                              )),
                                              column(3, wellPanel(
                                                h4("Population 2:"),
                                                numericInput("DDG.pop2.n", label=HTML(paste("Starting population size, N", paste(tags$sub(2), "(0):", sep=""), sep="")), 10),
                                                numericInput("DDG.pop2.r", label=HTML(paste("Intrinsic growth rate, r", paste(tags$sub(2), ":", sep=""), sep="")), .1),
                                                numericInput("DDG.pop2.K", label=HTML(paste("Carrying capacity, K", paste(tags$sub(2), ":", sep=""), sep="")), 250)
                                              )),
                                              column(3, wellPanel(
                                                h4("Run time:"),
                                                numericInput("DDG.time", label="Number of generations to run:", 100),
                                                h4("Populations to graph:"),
                                                checkboxInput("DDG.pop1.check", label="Population 1", value = TRUE),
                                                checkboxInput("DDG.pop2.check", label="Population 2", value = FALSE),
                                                submitButton("Update graphs")
                                              ))
                                            ),
                                            h3("Population Trajectories"),
                                            fluidRow(
                                              column(6, h4("Population size versus time"), plotOutput("DDG.plot.NvT")),
                                              column(6, h4("Population growth rate versus population size"), plotOutput("DDG.plot.dNdTvN"))
                                            ),
                                            fluidRow(
                                              column(6, h4("Log population size versus time"), plotOutput("DDG.plot.logNvT")),
                                              column(6, h4("Population size-corrected growth rate versus population size"), plotOutput("DDG.plot.dNNdTvN"))
                                            )
                                            ),
                                   p(em("Developed by"), a(href="http://audreylkelly.web.unc.edu", em("Audrey L. Kelly")))
                       )
                       )
              )
  )
)

