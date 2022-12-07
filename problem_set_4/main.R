##############################
# Author: Julian Marenz
# Econometrics: Problem Set 4
##############################

# turn off scientific notation
options(scipen=999)

# Set working directory, load required packages and specify overleaf path
rm(list=ls())
setwd("C:/Users/jmarenz/OneDrive - London Business School/PhD_Courses/2022_03AUT_Econometrics/PS4")
overleaf = "C:/Users/jmarenz/Dropbox/Apps/Overleaf/MarenzJulian_Econometrics_HW4/"

# Define a vector of packages that the script will use and check whether they are already installed
pkgs_required <- c("readxl", "tidyverse", "haven", "stargazer", "gmm", "lfe")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already installed, install it
if (length(pkgs_install) > 0) {
  install.packages(pkgs_install, dependencies=TRUE)
}

lapply(pkgs_required, require, character.only = TRUE) # load packages

# Exercise 2. (a) ---------------------------------------------------------------------

ccapm = read_stata("ccapm.dta") # load in data
ccapm = as.data.frame(ccapm) # convert to data.frame
ccapm$cratio_l1 = lag(ccapm$cratio) # create lag
ccapm$rrate_l1 = lag(ccapm$rrate) # create lag
ccapm$e_l1 = lag(ccapm$e) # create lag

g = function(theta, df) { # define the function g(.) that includes the moments we set to zero
  beta = theta[1] # parameter 1
  gamma = theta[2] # parameter 2
  m1 = beta*(df$cratio)^(-gamma)*df$rrate-1 # first moment
  m2 = (beta*(df$cratio)^(-gamma)*df$rrate-1)*df$cratio_l1 # second moment
  m3 = (beta*(df$cratio)^(-gamma)*df$rrate-1)*df$rrate_l1 # third moment
  m4 = (beta*(df$cratio)^(-gamma)*df$rrate-1)*df$e_l1 # fourth moment
  g = cbind(m1, m2, m3, m4) # bind together
  return(g) # return
}

gmm1 = gmm(g, # run gmm with starting values c(1, 1)
           as.data.frame(ccapm) %>% filter(!is.na(cratio_l1)), 
           c(beta = 1, gamma = 1))

summary(gmm1) # print summary

# Exercise 2. (b) ---------------------------------------------------------------------

gmm2 = gmm(g, # run gmm with starting values c(1, 1), vcov that is HAC, a Bartlett Kernel and bandwidth = 5
           as.data.frame(ccapm) %>% filter(!is.na(cratio_l1)), 
           c(beta = 1, gamma = 1),
           vcov = "HAC",
           kernel = "Bartlett",
           bw = 5)

summary(gmm2)  
      

# Exercise 2. (c) ---------------------------------------------------------------------

linearHypothesis(gmm2, c("beta = 0.98")) # test wheter coef is equal to 0.98


# Exercise 4. (a) ---------------------------------------------------------------------

murder = read_stata("MURDER.dta") # load in data
reg1 = lm(mrdrte ~ exec + unem + d90 + d93, data = murder)
stargazer(reg1, type = "latex", out = paste0(overleaf, "reg1.tex"))

# Exercise 4. (b) ---------------------------------------------------------------------

freg1 = felm(mrdrte ~ exec + unem + d90 + d93 | id + year, data = murder)
stargazer(freg1, type = "latex", out = paste0(overleaf, "freg1.tex"))
