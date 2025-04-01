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
MLEs.gamma$par <- exp(MLEs.gamma$par) # transform

gamma.alpha <- MLEs.gamma$par[1]
gamma.sigma <- MLEs.gamma$par[2]

# lognormal dist
lllognorm <- function(par, data, neg = F){
  # parameters
  mu <- exp(par[1])
  sigma <- exp(par[2])
  #mean <- par[1]
  #sd <- par[2]
  llognorm <- sum(log(dlnorm(x=data, meanlog = mu, sdlog = sigma)),na.rm=T)
  
  return(ifelse(neg, -llognorm, llognorm))
}

# can i do this for lognorm?
#par1 = mean(precipitation.long$Precipitation, na.rm =T)
#par2 = sd(precipitation.long$Precipitation, na.rm = T)
MLEs.lognorm <- optim(fn = lllognorm,
                      par = c(1,1),
                      data = precipitation.long$Precipitation,
                      neg = T)
MLEs.lognorm$par <- exp(MLEs.lognorm$par) # transform
lognorm.mu <- MLEs.lognorm$par[1]
lognorm.sigma <- MLEs.lognorm$par[2]

# compare to weibull 
llweibull <- function(par, data, neg=F){
  alpha <- exp(par[1]) # go from (-inf,inf) to (0,inf)
  sigma <- exp(par[2]) # go from (-inf,inf) to (0,inf)
  # alpha <- par[1]
  # sigma <- par[2]
  lweibull <- sum(log(dweibull(x=data, shape=alpha, scale=sigma)), na.rm=T)
  
  return(ifelse(neg, -lweibull, lweibull))
}

MLEs.weibull <- optim(fn = llweibull,
                      par = c(1,1),
                      data = precipitation.long$Precipitation,
                      neg=T)

MLEs.weibull$par <- exp(MLEs.weibull$par) # transform
weibull.alpha <- MLEs.weibull$par[1]
weibull.sigma <- MLEs.weibull$par[2]

# generating pdfs to check plots
pdfs <- tibble(x = seq(0,13,length.out=1000)) |>
  mutate(gamma.pdf = dgamma(x=x, shape = gamma.alpha, scale = gamma.sigma),
         lognorm.pdf = dlnorm(x=x, meanlog = lognorm.mu, sdlog = lognorm.sigma),
         weibull.pdf = dweibull(x=x, shape = weibull.alpha, scale = weibull.sigma))

# good enough...
pdf.plots <- ggplot() +
  geom_histogram(data=precipitation.long,
                 aes(x=Precipitation, y=after_stat(density)),
                 breaks = seq(0,13,1)) +
  geom_line(data = pdfs, aes(x=x, y= gamma.pdf, color = "gamma"))+
  geom_line(data = pdfs, aes(x=x, y=lognorm.pdf, color = "lognorm"))+
  geom_line(data = pdfs, aes(x=x, y=weibull.pdf, color = "weibull"))


# as we want the realized likelihood using the parameters found with MLE,
# we used log() to cancel out the exp() to use the actual parameters in the
# realized likelihood

# parts c-e calculations
weibull.ll <-  llweibull(par = log(c(weibull.alpha,weibull.sigma)),
                              data = precipitation.long$Precipitation, neg = F)

gamma.ll <- llgamma(par = log(c(gamma.alpha, gamma.sigma)), 
                    data = precipitation.long$Precipitation, neg = F)

lognorm.ll <- lllognorm(par = log(c(lognorm.mu, lognorm.sigma)),
                        data = precipitation.long$Precipitation, neg = F) 

# q1 < 1, so the gamma distribution has a better fit
partc.ratio <- exp(weibull.ll - gamma.ll)

# q2 > 1, so the weibull distribution has a better fit
partd.ratio <- exp(weibull.ll - lognorm.ll)

# q3 > 1, so the gamma distribution has a better fit 
parte.ratio <- exp(gamma.ll - lognorm.ll)

# if you look at the graph, it makes more sense
# weibull seems to underestimate a bit 
# lognorm seems to overestimate a bit 
