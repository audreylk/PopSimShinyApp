library(deSolve)
library(ggplot2)
library(tidyverse)

LotVmod.PredPrey <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)

as.data.frame(ode(func = LotVmod.PredPrey, y = State, parms = Pars, times = Time)) %>%
  gather("pop", "popsize", 2:3) %>%
  ggplot(aes(x=time, y=popsize, colour=pop)) + geom_line() + theme_classic() +
  labs(x="Time", y="Population size") +
  scale_color_discrete(name=NULL, labels=c("Prey", "Predators")) +
  theme(legend.position = c(.9,.9))
  

LotVmod.competition <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*rone*(1-((x + alpha*y)/Kone))
    dy = y*rtwo*(1-((y + beta*x)/Ktwo))
    return(list(c(dx, dy)))
  })
}

Pars2 <- c(rone = .9, alpha = .6, Kone = 500, rtwo = .5, beta = .7, Ktwo = 640)
State2 <- c(x = 10, y = 20)
Time2 <- seq(0, 100, by = 1)

as.data.frame(ode(func = LotVmod.competition, y = State2, parms = Pars2, times = Time2)) %>%
  gather("pop", "popsize", 2:3) %>%
  ggplot(aes(x=time, y=popsize, colour=pop)) + geom_line() + theme_classic() +
  labs(x="Time", y="Population size") +
  scale_color_discrete(name=NULL, labels=c("Population 1", "Population 2"), values=c("darkorange", "turquoise")) +
  theme(legend.position = c(.9,.9))

#isocline for species 1 in terms of N1
yintercept = Kone/beta
slope = -1/beta

#isocline for species2 in terms of N1
yintercept = Ktwo
slope = -alpha

#plotting
as.data.frame(ode(func = LotVmod.competition, y = State2, parms = Pars2, times = Time2)) %>%
  ggplot(aes(x=x, y=y)) + geom_path(arrow = arrow(ends="last")) + theme_classic() +
  geom_abline(aes(intercept = 500/.7, slope = -(1/.7)), colour="darkorange") +
  geom_abline(aes(intercept = 640, slope = -.6), colour="turquoise") +
  scale_x_continuous(expand=c(0,0), limits=c(0, 640/.6)) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 500/.7))
