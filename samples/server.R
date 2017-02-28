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
theme_set(theme_classic() + theme(legend.position="top", legend.direction = "horizontal"))
colourblind_custom <- c("darkorange", "turquoise", "orangered", "blue")

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
      ggplot(aes(x=time, y=popsize, colour=pop)) + geom_line() + 
      labs(x="Time", y="Population size") +
      scale_color_manual(name=NULL, labels=c("Population 1", "Population 2"), values=c("darkorange", "turquoise")) +
      scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
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
      geom_abline(aes(intercept = (input$LVC.pop1.K/input$LVC.alpha12), slope = -(1/input$LVC.alpha12), colour="Population 1 isocline")) +
      geom_abline(aes(intercept = input$LVC.pop2.K, slope = -(input$LVC.alpha21), colour="Population 2 isocline"))+
      scale_x_continuous(expand=c(0,0), 
                         limits=c(0, (max(input$LVC.pop2.K/input$LVC.alpha21, input$LVC.pop1.K, input$LVC.pop1.n)))) +
      scale_y_continuous(expand=c(0,0), 
                         limits=c(0, (max(input$LVC.pop1.K/input$LVC.alpha12, input$LVC.pop2.K, input$LVC.pop2.n)))) +
      annotate("text", x=input$LVC.pop1.n, y=input$LVC.pop2.n, label="Time = 0", hjust=-0.1) +
      annotate("text", x=tail(LVCdf$x, n=1), y=tail(LVCdf$y, n=1), label=paste("Time = ", input$LVC.time, sep=""), hjust=-0.1) +
      scale_colour_manual(values=c("black", "darkorange", "turquoise"), name=NULL) +
      labs(x="Population 1 size", y="Population 2 size")
  })
  output$DIG.plot.NvT <- renderPlot({
    DIGmod.pop1 <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = N*r
        return(list(dNdt))
      })
    }
    DIGpars.1 <- c(r = input$DIG.pop1.r)
    DIGstate.1 <- c(N = input$DIG.pop1.n)
    DIGtime <- seq(0, input$DIG.time, by = 1)
    DIGdf.1 <- as.data.frame(ode(func = DIGmod.pop1, y = DIGstate.1, parms = DIGpars.1, times = DIGtime))
    ggplot(DIGdf.1, aes(x=time, y=N)) + geom_path(aes(colour="Population 1")) + 
      scale_colour_manual(values=colourblind_custom, name = NULL) +
      labs(x = "Time (generations)", y = "Population size (# individuals)")
  })
  output$DIG.plot.dNdTvN <- renderPlot({
    DIGmod.pop1 <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = N*r
        return(list(dNdt, State))
      })
    }
    DIGpars.1 <- c(r = input$DIG.pop1.r)
    DIGstate.1 <- c(N = input$DIG.pop1.n)
    DIGtime <- seq(0, input$DIG.time, by = 1)
    DIGdf.1 <- as.data.frame(ode(func = DIGmod.pop1, y = DIGstate.1, parms = DIGpars.1, times = DIGtime))
    ggplot(DIGdf.1, aes(x = N, y = N*input$DIG.pop1.r)) + geom_path(aes(colour="Population 1")) +
      scale_colour_manual(values=colourblind_custom, name=NULL) +
      labs(x="Population size (# individuals)", y="Population growth rate (# individuals/generation)")
  })
  output$DIG.plot.logNvT <- renderPlot({
    DIGmod.pop1 <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = N*r
        return(list(dNdt))
      })
    }
    DIGpars.1 <- c(r = input$DIG.pop1.r)
    DIGstate.1 <- c(N = input$DIG.pop1.n)
    DIGtime <- seq(0, input$DIG.time, by = 1)
    DIGdf.1 <- as.data.frame(ode(func = DIGmod.pop1, y = DIGstate.1, parms = DIGpars.1, times = DIGtime))
    ggplot(DIGdf.1, aes(y = log(N), x = time)) + geom_path(aes(colour="Population 1")) +
      scale_colour_manual(values = colourblind_custom, name=NULL) +
      labs(x = "Time (generations)", y = "Log population size")
  })
  output$DIG.plot.dNNdTvN <- renderPlot({
    DIGmod.pop1 <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = N*r
        return(list(dNdt))
      })
    }
    DIGpars.1 <- c(r = input$DIG.pop1.r)
    DIGstate.1 <- c(N = input$DIG.pop1.n)
    DIGtime <- seq(0, input$DIG.time, by = 1)
    DIGdf.1 <- as.data.frame(ode(func = DIGmod.pop1, y = DIGstate.1, parms = DIGpars.1, times = DIGtime))
    ggplot(DIGdf.1, aes(x = N, y = input$DIG.pop1.r)) + geom_path(aes(colour="Population 1")) +
      scale_colour_manual(values=colourblind_custom, name=NULL) +
      labs(x = "Population size (# individuals)", y = "Population size-corrected growth rate\n(# individuals/generation)")
  })
  output$DDG.plot.NvT <- renderPlot({
    DDGmod.pop1 <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = r * N *((K - N)/K)
        return(list(dNdt))
      })
    }
    DDGpars.1 <- c(r = input$DDG.pop1.r, K = input$DDG.pop1.K)
    DDGstate.1 <- c(N = input$DDG.pop1.n)
    DDGtime <- seq(0, input$DDG.time, by = 1)
    DDGdf.1 <- as.data.frame(ode(func = DDGmod.pop1, y = DDGstate.1, parms = DDGpars.1, times = DDGtime))
    ggplot(DDGdf.1, aes(y = N, x = time)) + geom_path(aes(colour="Population 1")) +
      scale_colour_manual(values=colourblind_custom, name=NULL) +
      labs(x = "Time (generations)", y = "Population size (# individuals)")
  })
  output$DDG.plot.dNdTvN <- renderPlot({
    DDGmod.pop1 <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = r * N *((K - N)/K)
        return(list(dNdt))
      })
    }
    DDGpars.1 <- c(r = input$DDG.pop1.r, K = input$DDG.pop1.K)
    DDGstate.1 <- c(N = input$DDG.pop1.n)
    DDGtime <- seq(0, input$DDG.time, by = 1)
    DDGdf.1 <- as.data.frame(ode(func = DDGmod.pop1, y = DDGstate.1, parms = DDGpars.1, times = DDGtime))
    ggplot(DDGdf.1, aes(y = input$DDG.pop1.r * N *((input$DDG.pop1.K - N)/input$DDG.pop1.K), x = N)) + geom_path(aes(colour="Population 1")) +
      scale_colour_manual(values=colourblind_custom, name=NULL) +
      labs(x = "Population size (# individuals)", y = "Population growth rate (# individuals/generation)")
  })
  output$DDG.plot.logNvT <- renderPlot({
    DDGmod.pop1 <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = r * N *((K - N)/K)
        return(list(dNdt))
      })
    }
    DDGpars.1 <- c(r = input$DDG.pop1.r, K = input$DDG.pop1.K)
    DDGstate.1 <- c(N = input$DDG.pop1.n)
    DDGtime <- seq(0, input$DDG.time, by = 1)
    DDGdf.1 <- as.data.frame(ode(func = DDGmod.pop1, y = DDGstate.1, parms = DDGpars.1, times = DDGtime))
    ggplot(DDGdf.1, aes(y = log(N), x = time)) + geom_path(aes(colour="Population 1")) +
      scale_colour_manual(values=colourblind_custom, name=NULL) +
      labs(x = "Time (generations)", y = "Log population size (# individuals)")
  })
  output$DDG.plot.dNNdTvN <- renderPlot({
    DDGmod.pop1 <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = r * N *((K - N)/K)
        return(list(dNdt))
      })
    }
    DDGpars.1 <- c(r = input$DDG.pop1.r, K = input$DDG.pop1.K)
    DDGstate.1 <- c(N = input$DDG.pop1.n)
    DDGtime <- seq(0, input$DDG.time, by = 1)
    DDGdf.1 <- as.data.frame(ode(func = DDGmod.pop1, y = DDGstate.1, parms = DDGpars.1, times = DDGtime))
    ggplot(DDGdf.1, aes(y = input$DDG.pop1.r * ((input$DDG.pop1.K - N)/input$DDG.pop1.K), x = N)) + geom_path(aes(colour="Population 1")) +
      scale_colour_manual(values=colourblind_custom, name=NULL) +
      labs(x = "Population size (# individuals)", y = "Population size-corrected growth rate\n(# individuals/generation)")
  })
})
