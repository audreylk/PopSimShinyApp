#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny) #this is the main package that allows for this web app to exist
library(ggplot2) #this package makes the graphs
library(tidyverse) #this package just helps us manipulate data in easy ways
library(deSolve) #this package solves differential equations, and the population change equations are differential equations

shinyServer(function(input, output) {
  output$LVC.plot <- renderPlot({
    LotVmod.competition <- function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dx = x*rone*((Kone - x - alpha*y)/Kone)
        dy = y*rtwo*((Ktwo - y - beta*x)/Ktwo)
        return(list(c(dx, dy)))
      })
    }
    LVCpars <- c(rone = input$LVC.pop1.r, alpha = input$LVC.alpha12, Kone = input$LVC.pop1.K, 
                 rtwo = input$LVC.pop2.r, beta = input$LVC.alpha21, Ktwo = input$LVC.pop2.K)
    LVCstate <- c(x = input$LVC.pop1.n, y = input$LVC.pop2.n)
    LVCtime <- seq(0, input$LVC.time, by = 1)
    as.data.frame(ode(func = LotVmod.competition, y = LVCstate, parms = LVCpars, times = LVCtime)) %>%
      gather("pop", "popsize", 2:3) %>%
      ggplot(aes(x=time, y=popsize, colour=pop)) + geom_line() + theme_classic() +
      labs(x="Time", y="Population size") +
      scale_color_manual(name=NULL, labels=c("Population 1", "Population 2"), values=c("darkorange", "turquoise")) +
      scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
      theme(legend.position = "top", legend.direction = "horizontal")
  })
  output$LVC.phaseplane <- renderPlot({
    LotVmod.competition <- function(Time, State, Pars) {
      with(as.list(c(State, Pars)), {
        dx = x*rone*((Kone - x - alpha*y)/Kone)
        dy = y*rtwo*((Ktwo - y - beta*x)/Ktwo)
        return(list(c(dx, dy)))
      })
    }
    LVCpars <- c(rone = input$LVC.pop1.r, alpha = input$LVC.alpha12, Kone = input$LVC.pop1.K, 
                 rtwo = input$LVC.pop2.r, beta = input$LVC.alpha21, Ktwo = input$LVC.pop2.K)
    LVCstate <- c(x = input$LVC.pop1.n, y = input$LVC.pop2.n)
    LVCtime <- seq(0, input$LVC.time, by = 1)
    LVCdf <- as.data.frame(ode(func = LotVmod.competition, y = LVCstate, parms = LVCpars, times = LVCtime))
    ggplot(LVCdf, aes(x=x, y=y)) + geom_path(aes(colour="Combined trajectory over time")) + 
      geom_point(aes(x=input$LVC.pop1.n, y=input$LVC.pop2.n), colour="black", shape=1) +
      geom_point(aes(x=tail(LVCdf$x, n=1), y=tail(LVCdf$y, n=1)), colour="black") +
      theme_classic() +
      geom_abline(aes(intercept = (input$LVC.pop1.K/input$LVC.alpha12), slope = -(1/input$LVC.alpha12), colour="Population 1 isocline")) +
      geom_abline(aes(intercept = input$LVC.pop2.K, slope = -(input$LVC.alpha21), colour="Population 2 isocline"))+
      scale_x_continuous(expand=c(0,0), 
                         limits=c(0, (max(input$LVC.pop2.K/input$LVC.alpha21, input$LVC.pop1.K, input$LVC.pop1.n)))) +
      scale_y_continuous(expand=c(0,0), 
                         limits=c(0, (max(input$LVC.pop1.K/input$LVC.alpha12, input$LVC.pop2.K, input$LVC.pop2.n)))) +
      annotate("text", x=input$LVC.pop1.n, y=input$LVC.pop2.n, label="Time = 0", hjust=-0.1) +
      annotate("text", x=tail(LVCdf$x, n=1), y=tail(LVCdf$y, n=1), label=paste("Time = ", input$LVC.time, sep=""), hjust=-0.1) +
      scale_colour_manual(values=c("black", "darkorange", "turquoise"), name=NULL) +
      theme(legend.position="top", legend.direction="horizontal") +
      labs(x="Population 1 size", y="Population 2 size")
  })
})
