# Henry Sun
# Homework 9 
library(tidyverse)
################################################################################
# Problem 1

# gamma dist
precipitation.data <- read_csv("agacis.csv")
precipitation.long <- precipitation.data |>
  pivot_longer(cols = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
                        "Nov","Dec"), 
               names_to = "Month", 
               values_to = "Precipitation") |>
  select(-Annual) |>
  mutate(Precipitation = if_else(Precipitation == "M", 
                                 NA_character_, Precipitation))

llgamma <- function(par, data, neg = F){
  # parameters
  alpha <- par[1]
  sigma <- par[2]
  # lambda <- 1/sigma
  lgamma <- sum(log(dgamma(x=data, shape=alpha, scale =sigma)))
  
  return(ifelse(neg, -lgamma, lgamma))
}

MLEs.gamma <- optim(par = c(1,1),
                    fn = llgamma,
                    data = )