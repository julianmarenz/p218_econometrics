##############################
# Author: Julian Marenz
# Econometrics: Problem Set 2
##############################


# Set working directory, load required packages and specify overleaf path
rm(list=ls())
setwd("C:/Users/jmarenz/OneDrive - London Business School/PhD_Courses/2022_03AUT_Econometrics/PS2")
overleaf = "C:/Users/jmarenz/Dropbox/Apps/Overleaf/MarenzJulian_Econometrics_HW2/"

# Define a vector of packages that the script will use and check whether they are already installed
pkgs_required <- c("readxl", "tidyverse", "haven", "stargazer")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already installed, install it
if (length(pkgs_install) > 0) {
  install.packages(pkgs_install, dependencies=TRUE)
}

lapply(pkgs_required, require, character.only = TRUE) # load packages

# Exercise 1. (d) ---------------------------------------------------------------------
sp500 = read_xlsx("SP500Index.xlsx") # read the excel file
sp500$level_l = dplyr::lag(sp500$`Level of the S&P 500 Index`) # generate lag of the level
sp500$x_t = log(sp500$`Level of the S&P 500 Index`/sp500$`Level of the S&P 500 Index`[1]) # calculate x_t
sp500$x_t_l = dplyr::lag(sp500$x_t) # generate lag of x_t

# focus on observations 2 - 702 as January 1960 is t = 0 and observation for t - 1 is undefined.
sp500 = sp500[2:702,]

# The ML estimator for delta is x_T/T:
sp500$delta_ML = sp500$x_t[nrow(sp500)]/nrow(sp500)

# The ML estimator for sigma^2 is:
sp500$sigma_ML = sqrt(sum((sp500$x_t-sp500$delta_ML-sp500$x_t_l)^2)/nrow(sp500))

# The SD of the monthly return is:
sd(log(sp500$`Level of the S&P 500 Index`) - log(sp500$level_l))



## Exercise 2. ---------------------------------------------------------------------

rm(list=setdiff(ls(), "overleaf")) # clean up
youth_survey = read_dta("ps1.dta") # load in the data

# create variables needed
youth_survey$lwage = log(youth_survey$w0)
youth_survey$educ = youth_survey$ed0
youth_survey$exper = youth_survey$a0-(youth_survey$educ + 6)

## (a)

# create dummies
youth_survey$educ_dummy = as.factor(youth_survey$educ)
youth_survey$exper_dummy = as.factor(youth_survey$exper)

reg_u = lm(lwage ~ educ_dummy + exper_dummy, data = youth_survey) # run unrestricted regression
alpha_hat = reg_u$coefficients # save coefficients

## (b)

reg_r = lm(lwage ~ educ + exper, data = youth_survey) # run regression

ssr_r = sum(reg_r$residuals^2) # save the SSR restricted
ssr_u = sum(reg_u$residuals^2) # save the SSR unrestricted

k = length(!is.na(alpha_hat)) # check the number of variables we include
p = k - 2
n = nrow(youth_survey) # check the number of observations

F_stat = ((ssr_r-ssr_u)/p)/(ssr_u/(n-k)) # calculate the F statistics

p_value = pf(F_stat, p, (n-k), lower.tail = FALSE) # calculate associated p-value


## Exercise 3.

rm(list=setdiff(ls(), "overleaf")) # clear up

# define a function to run Monte Carlo simulations
montecarlo_fun = function(N, S) {
  #' This function takes as arguments
  #' N: The number of observations for the simulation
  #' S: The number of times the simulation is repeated
  #' And it returns a dataframe with:
  #' constant: The estimated constant
  #' beta: The estimated coefficient
  #' rss: The sum of squared residuals
  intercept_ = list() # create a list that stores estimates for the intercept
  beta_ = list() # create a list that stores estimates for beta values 
  rss_ = list()
  for(i in c(1:S)) {
    monte_carlo = data.frame(z = rchisq(N, 3)) # make df with i observations of z values
    monte_carlo$error = runif(N, -1, 1) # add to it a random error
    monte_carlo$y = 1 + 0.5 * monte_carlo$z + monte_carlo$error # calculate the y values
    reg = lm(y ~ z, data = monte_carlo) # run the regression
    intercept_[i] = reg$coefficients[1] # add coefficient for constant to list
    beta_[i] = reg$coefficients[2] # add coefficient for beta to list
    rss_[i] = sum(reg$residuals^2) # add coefficient for beta to list
  }
  return(data.frame(intercept_est = unlist(intercept_), 
                    beta_est = unlist(beta_),
                    rss = unlist(rss_)))
}

## (a)

bias_df = montecarlo_fun(1000, 10000)

print(mean(bias_df$intercept_est)) # almost indistinguishable from 1
print(mean(bias_df$beta_est)) # almost inddistinguishable from 0.5

## (b) 

# for consistency, repeat monte carlo simulations for different sample sizes

consistency_est = list() # create empty list
i = 1

for(obs in c(25, 200, 800, 3200)) { # for 25, 200, 800, and 3200 observations
  consistency_est[i] = list(montecarlo_fun(obs, 1000)) # outcomes from 1,000 simulations
  
  # look at consistency by making histograms of estimated intercept and coefficient for different sample size
  
  ggplot(aes(x = intercept_est), data = consistency_est[[i]]) + # intercept
    geom_histogram(color = "#000000", fill = "#0099F8") # make histgram
  ggsave(paste0(overleaf, "figures/histogram_inter_n", as.character(obs), ".pdf"), width = 8, height = 6) # save histogram
  
  ggplot(aes(x = beta_est), data = consistency_est[[i]]) + # beta
    geom_histogram(color = "#000000", fill = "#0099F8") # make histogram
  ggsave(paste0(overleaf, "figures/histogram_beta_n", as.character(obs), ".pdf"), width = 8, height = 6) # save histogram
  i = i + 1
}


## (c) 

asym = montecarlo_fun(100000, 1000) # run 1000 monte carlo simulations with sample size of 100,000

# now create a normal CDF with steps of 0.001 for quantiles, with:
# N = 100,000
# beta_mean = 0.5
# sigma_sq = 1/3
# Var(z) = 2 * 3

asym_norm = data.frame(a = qnorm(c(seq(0, 1, 0.001)), mean = 0.5, sd = sqrt((1/100000)*(1/3)/6)))

#' it is a good idea to determine the empirical CDF and compare this to the normal distribution to 
#' gauge whether our estimator is normally distributed.

ggplot(asym, aes(beta_est)) +
  stat_ecdf(geom = "step") +
  stat_ecdf(geom = "step", data = asym_norm, aes(a), color = "red")

ggsave(paste0(overleaf, "figures/normality_beta", as.character(obs), ".pdf"), width = 8, height = 6) # save histogram

## (d)

print(mean(bias_df$rss/(1000-2))) # almost exactly 1/3

