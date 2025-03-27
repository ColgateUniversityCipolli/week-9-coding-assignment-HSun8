# Henry Sun
# Homework 9 
library(tidyverse)
################################################################################
# Problem 1

# cleaning data
precipitation.data <- read_csv("agacis.csv")
precipitation.long <- precipitation.data |>
  pivot_longer(cols = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                        "Nov","Dec"), 
               names_to = "Month", 
               values_to = "Precipitation") |>
  select(-Annual) |>
  mutate(Precipitation = if_else(Precipitation == "M", 
                                 NA_character_, Precipitation)) |>
  mutate(Precipitation = as.numeric(Precipitation))

# gamma dist
llgamma <- function(par, data, neg = F){
  # parameters
  alpha <- exp(par[1])
  sigma <- exp(par[2])
  # lambda <- 1/sigma
  lgamma <- sum(log(dgamma(x=data, shape=alpha, scale=sigma)),na.rm=T)
  
  return(ifelse(neg, -lgamma, lgamma))
}

MLEs.gamma <- optim(fn = llgamma,
                    par = c(1,1),
                    data = precipitation.long$Precipitation,
                    neg = T)
(MLEs.gamma$par <- exp(MLEs.gamma$par)) # transform

gamma.alpha <- MLEs.gamma$par[1]
gamma.sigma <- MLEs.gamma$par[2]


