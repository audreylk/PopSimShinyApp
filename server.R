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
    DIGmod <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = N*r
        return(list(dNdt))
      })
    }
    DIG.NvTplot <- ggplot()
    if(input$DIG.pop1.check == TRUE){
      DIGpars.1 <- c(r = input$DIG.pop1.r)
      DIGstate.1 <- c(N = input$DIG.pop1.n)
      DIGtime <- seq(0, input$DIG.time, by = 1)
      DIG.NvTplot <- DIG.NvTplot + 
        geom_path(data = as.data.frame(ode(func = DIGmod, y = DIGstate.1, parms = DIGpars.1, times = DIGtime)),
                  aes(x=time, y=N, colour="Population 1")) + 
        scale_colour_manual(values=colourblind_custom, name = NULL) +
        labs(x = "Time (generations)", y = "Population size (# individuals)")
    }
    if(input$DIG.pop2.check == TRUE){
      DIGpars.2 <- c(r = input$DIG.pop2.r)
      DIGstate.2 <- c(N = input$DIG.pop2.n)
      DIGtime <- seq(0, input$DIG.time, by = 1)
      DIG.NvTplot <- DIG.NvTplot +
        geom_path(data = as.data.frame(ode(func = DIGmod, y = DIGstate.2, parms = DIGpars.2, times = DIGtime)),
                  aes(x=time, y=N, colour="Population 2")) +
        scale_colour_manual(values=colourblind_custom, name = NULL) +
        labs(x = "Time (generations)", y = "Population size (# individuals)")
    }
    DIG.NvTplot
  })
  output$DIG.plot.dNdTvN <- renderPlot({
    DIGmod <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = N*r
        return(list(dNdt, State))
      })
    }
    DIG.dNdTvNplot <- ggplot()
    if(input$DIG.pop1.check == TRUE){
      DIGpars.1 <- c(r = input$DIG.pop1.r)
      DIGstate.1 <- c(N = input$DIG.pop1.n)
      DIGtime <- seq(0, input$DIG.time, by = 1)
      DIG.dNdTvNplot <- DIG.dNdTvNplot + 
        geom_path(data = as.data.frame(ode(func = DIGmod, y = DIGstate.1, parms = DIGpars.1, times = DIGtime)),
                  aes(x = N, y = N*input$DIG.pop1.r, colour="Population 1")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x="Population size (# individuals)", y="Population growth rate (# individuals/generation)")
    }
    if(input$DIG.pop2.check == TRUE){
      DIGpars.2 <- c(r = input$DIG.pop2.r)
      DIGstate.2 <- c(N = input$DIG.pop2.n)
      DIGtime <- seq(0, input$DIG.time, by = 1)
      DIG.dNdTvNplot <- DIG.dNdTvNplot + 
        geom_path(data = as.data.frame(ode(func = DIGmod, y = DIGstate.2, parms = DIGpars.2, times = DIGtime)),
                  aes(x = N, y = N*input$DIG.pop2.r, colour="Population 2")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x="Population size (# individuals)", y="Population growth rate (# individuals/generation)")
    }
    DIG.dNdTvNplot
  })
  output$DIG.plot.logNvT <- renderPlot({
    DIGmod <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = N*r
        return(list(dNdt))
      })
    }
    DIG.logNvTplot <- ggplot()
    if(input$DIG.pop1.check == TRUE){
      DIGpars.1 <- c(r = input$DIG.pop1.r)
      DIGstate.1 <- c(N = input$DIG.pop1.n)
      DIGtime <- seq(0, input$DIG.time, by = 1)
      DIG.logNvTplot <- DIG.logNvTplot + 
        geom_path(data = as.data.frame(ode(func = DIGmod, y = DIGstate.1, parms = DIGpars.1, times = DIGtime)),
                  aes(y = log(N), x = time, colour="Population 1")) +
        scale_colour_manual(values = colourblind_custom, name=NULL) +
        labs(x = "Time (generations)", y = "Log population size")
    }
    if(input$DIG.pop2.check == TRUE){
      DIGpars.2 <- c(r = input$DIG.pop2.r)
      DIGstate.2 <- c(N = input$DIG.pop2.n)
      DIGtime <- seq(0, input$DIG.time, by = 1)
      DIG.logNvTplot <- DIG.logNvTplot + 
        geom_path(data = as.data.frame(ode(func = DIGmod, y = DIGstate.2, parms = DIGpars.2, times = DIGtime)),
                  aes(y = log(N), x = time, colour="Population 2")) +
        scale_colour_manual(values = colourblind_custom, name=NULL) +
        labs(x = "Time (generations)", y = "Log population size")
    }
    DIG.logNvTplot
  })
  output$DIG.plot.dNNdTvN <- renderPlot({
    DIGmod<- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = N*r
        return(list(dNdt))
      })
    }
    DIG.dNNdTvNplot <- ggplot()
    if(input$DIG.pop1.check == TRUE){
      DIGpars.1 <- c(r = input$DIG.pop1.r)
      DIGstate.1 <- c(N = input$DIG.pop1.n)
      DIGtime <- seq(0, input$DIG.time, by = 1)
      DIG.dNNdTvNplot <- DIG.dNNdTvNplot +
        geom_path(data=as.data.frame(ode(func = DIGmod, y = DIGstate.1, parms = DIGpars.1, times = DIGtime)),
                  aes(x = N, y = input$DIG.pop1.r, colour="Population 1")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Population size (# individuals)", y = "Population size-corrected growth rate\n(# individuals/generation)")
    }
    if(input$DIG.pop2.check == TRUE){
      DIGpars.2 <- c(r = input$DIG.pop2.r)
      DIGstate.2 <- c(N = input$DIG.pop2.n)
      DIGtime <- seq(0, input$DIG.time, by = 1)
      DIG.dNNdTvNplot <- DIG.dNNdTvNplot +
        geom_path(data=as.data.frame(ode(func = DIGmod, y = DIGstate.2, parms = DIGpars.2, times = DIGtime)),
                  aes(x = N, y = input$DIG.pop2.r, colour="Population 2")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Population size (# individuals)", y = "Population size-corrected growth rate\n(# individuals/generation)")
    }
    DIG.dNNdTvNplot
  })
  output$DDG.plot.NvT <- renderPlot({
    DDGmod <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = r * N *((K - N)/K)
        return(list(dNdt))
      })
    }
    DDG.NvTplot <- ggplot()
    if(input$DDG.pop1.check == TRUE){
      DDGpars.1 <- c(r = input$DDG.pop1.r, K = input$DDG.pop1.K)
      DDGstate.1 <- c(N = input$DDG.pop1.n)
      DDGtime <- seq(0, input$DDG.time, by = 1)
      DDG.NvTplot <- DDG.NvTplot + 
        geom_path(data = as.data.frame(ode(func = DDGmod, y = DDGstate.1, parms = DDGpars.1, times = DDGtime)),
                  aes(y = N, x = time, colour="Population 1")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Time (generations)", y = "Population size (# individuals)")
    }
    if(input$DDG.pop2.check == TRUE){
      DDGpars.2 <- c(r = input$DDG.pop2.r, K = input$DDG.pop2.K)
      DDGstate.2 <- c(N = input$DDG.pop2.n)
      DDGtime <- seq(0, input$DDG.time, by = 1)
      DDG.NvTplot <- DDG.NvTplot + 
        geom_path(data = as.data.frame(ode(func = DDGmod, y = DDGstate.2, parms = DDGpars.2, times = DDGtime)),
                  aes(y = N, x = time, colour="Population 2")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Time (generations)", y = "Population size (# individuals)")
    }
    DDG.NvTplot
  })
  output$DDG.plot.dNdTvN <- renderPlot({
    DDGmod<- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = r * N *((K - N)/K)
        return(list(dNdt))
      })
    }
    DDG.dNdTvNplot <- ggplot()
    if(input$DDG.pop1.check == TRUE){
      DDGpars.1 <- c(r = input$DDG.pop1.r, K = input$DDG.pop1.K)
      DDGstate.1 <- c(N = input$DDG.pop1.n)
      DDGtime <- seq(0, input$DDG.time, by = 1)
      DDG.dNdTvNplot <- DDG.dNdTvNplot + 
        geom_path(data=as.data.frame(ode(func = DDGmod, y = DDGstate.1, parms = DDGpars.1, times = DDGtime)),
                  aes(x = N, y = input$DDG.pop1.r * N *((input$DDG.pop1.K - N)/input$DDG.pop1.K), colour="Population 1")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Population size (# individuals)", y = "Population growth rate (# individuals/generation)")
    }
    if(input$DDG.pop2.check == TRUE){
      DDGpars.2 <- c(r = input$DDG.pop2.r, K = input$DDG.pop2.K)
      DDGstate.2 <- c(N = input$DDG.pop2.n)
      DDGtime <- seq(0, input$DDG.time, by = 1)
      DDG.dNdTvNplot <- DDG.dNdTvNplot + 
        geom_path(data=as.data.frame(ode(func = DDGmod, y = DDGstate.2, parms = DDGpars.2, times = DDGtime)),
                  aes(x = N, y = input$DDG.pop2.r * N *((input$DDG.pop2.K - N)/input$DDG.pop2.K), colour="Population 2")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Population size (# individuals)", y = "Population growth rate (# individuals/generation)")
    }
    DDG.dNdTvNplot
  })
  output$DDG.plot.logNvT <- renderPlot({
    DDGmod <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = r * N *((K - N)/K)
        return(list(dNdt))
      })
    }
    DDG.logNvTplot <- ggplot()
    if(input$DDG.pop1.check == TRUE){
      DDGpars.1 <- c(r = input$DDG.pop1.r, K = input$DDG.pop1.K)
      DDGstate.1 <- c(N = input$DDG.pop1.n)
      DDGtime <- seq(0, input$DDG.time, by = 1)
      DDG.logNvTplot <- DDG.logNvTplot + 
        geom_path(data = as.data.frame(ode(func = DDGmod, y = DDGstate.1, parms = DDGpars.1, times = DDGtime)),
                  aes(y = log(N), x = time, colour="Population 1")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Time (generations)", y = "Log population size (# individuals)")
    }
    if(input$DDG.pop2.check == TRUE){
      DDGpars.2 <- c(r = input$DDG.pop2.r, K = input$DDG.pop2.K)
      DDGstate.2 <- c(N = input$DDG.pop2.n)
      DDGtime <- seq(0, input$DDG.time, by = 1)
      DDG.logNvTplot <- DDG.logNvTplot + 
        geom_path(data = as.data.frame(ode(func = DDGmod, y = DDGstate.2, parms = DDGpars.2, times = DDGtime)),
                  aes(y = log(N), x = time, colour="Population 2")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Time (generations)", y = "Log population size (# individuals)")
    }
    DDG.logNvTplot
  })
  output$DDG.plot.dNNdTvN <- renderPlot({
    DDGmod <- function(Time, State, Pars){
      with(as.list(c(State, Pars)), {
        dNdt = r * N *((K - N)/K)
        return(list(dNdt))
      })
    }
    DDG.dNNdTvNplot <- ggplot()
    if(input$DDG.pop1.check == TRUE){
      DDGpars.1 <- c(r = input$DDG.pop1.r, K = input$DDG.pop1.K)
      DDGstate.1 <- c(N = input$DDG.pop1.n)
      DDGtime <- seq(0, input$DDG.time, by = 1)
      DDG.dNNdTvNplot <- DDG.dNNdTvNplot + 
        geom_path(data=as.data.frame(ode(func = DDGmod, y = DDGstate.1, parms = DDGpars.1, times = DDGtime)),
                  aes(x = N, y = input$DDG.pop1.r * ((input$DDG.pop1.K - N)/input$DDG.pop1.K), colour="Population 1")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Population size (# individuals)", y = "Population size-corrected growth rate\n(# individuals/generation)")
    }
    if(input$DDG.pop2.check == TRUE){
      DDGpars.2 <- c(r = input$DDG.pop2.r, K = input$DDG.pop2.K)
      DDGstate.2 <- c(N = input$DDG.pop2.n)
      DDGtime <- seq(0, input$DDG.time, by = 1)
      DDG.dNNdTvNplot <- DDG.dNNdTvNplot + 
        geom_path(data=as.data.frame(ode(func = DDGmod, y = DDGstate.2, parms = DDGpars.2, times = DDGtime)),
                  aes(x = N, y = input$DDG.pop2.r * ((input$DDG.pop2.K - N)/input$DDG.pop2.K), colour="Population 2")) +
        scale_colour_manual(values=colourblind_custom, name=NULL) +
        labs(x = "Population size (# individuals)", y = "Population size-corrected growth rate\n(# individuals/generation)")
    }
    DDG.dNNdTvNplot
  })
})
