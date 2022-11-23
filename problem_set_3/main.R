##############################
# Author: Julian Marenz
# Econometrics: Problem Set 3
##############################


# Set working directory, load required packages and specify overleaf path
rm(list=ls())
setwd("C:/Users/jmarenz/OneDrive - London Business School/PhD_Courses/2022_03AUT_Econometrics/PS3/")
overleaf = "C:/Users/jmarenz/Dropbox/Apps/Overleaf/MarenzJulian_Econometrics_HW3/"

# Define a vector of packages that the script will use and check whether they are already installed
pkgs_required <- c("readxl", "tidyverse", "stargazer", "olsrr", "car", "xtable", "ivreg", "lmtest", "sandwich")
pkgs_install  <- pkgs_required[!(pkgs_required %in% installed.packages()[,"Package"])]

# If any package is not already installed, install it
if (length(pkgs_install) > 0) {
  install.packages(pkgs_install, dependencies=TRUE)
}

lapply(pkgs_required, require, character.only = TRUE) # load packages

# Exercise 2. (a) ---------------------------------------------------------------------
nerlove = read_xlsx("Nerlove1963/Nerlove1963.xlsx") # load in the Nerlove data

# create log variables and square the log of output
nerlove$log_TC = log(nerlove$Cost)
nerlove$log_PL = log(nerlove$Plabor)
nerlove$log_PK = log(nerlove$Pcapital)
nerlove$log_PF = log(nerlove$Pfuel)
nerlove$log_Q = log(nerlove$output)
nerlove$log_Q_sq = (nerlove$log_Q)^2

lm_1 = lm(data = nerlove, log_TC ~ log_Q + log_PL + log_PK + log_PF) # run first regression without square
lm_2 = lm(data = nerlove, log_TC ~ log_Q + log_Q_sq + log_PL + log_PK + log_PF) # run second regression with square

stargazer(lm_1, lm_2, out = paste0(overleaf, "nerlove.tex")) # export output to overleaf

ols_mallows_cp(lm_1, lm_2) # calculate the value of Mallow's CP which gives the F-statistic cutoff
linearHypothesis(lm_2, "log_Q_sq = 0") # calculate F-test statistic for coefficient on log_Q_sq being zero


# Exercise 2. (b) ---------------------------------------------------------------------
lower_bound = sort(nerlove$log_Q)[16] # sort values and look at the threshold for at least 15 values below
upper_bound = sort(nerlove$log_Q)[length(nerlove$log_Q)-15] # sort values and look at the threshold for at least 15 values above

# create the variables for the restriction
nerlove$restr_1 = nerlove$log_PK - nerlove$log_PL
nerlove$restr_2 = nerlove$log_PF - nerlove$log_PL


# create a list with 1,000 different values for z that are calculated by taking the same step size from the
# lower bound to the upper bound of logQ.
beta_7 = seq(lower_bound, upper_bound, by = (upper_bound-lower_bound)/999) 
reg_df_list = list() # make an empty list for the different regression df's

for(i in c(1:1000)) { # for 1,000 values
  df = nerlove # copy the df
  df$z = df$log_Q * ((1 + exp(beta_7[i] - df$log_Q))^(-1)) # calculate the z var for corresponding beta_7
  reg_df_list[[i]] = df # add to the list
}

# now write a loop where we run the regression for each of the different values of beta_7 that we 
# have calculated. Store the SSR associated with each regression and find the minimum.

reg_list = list()
ssr = list() # create empty list to store ssr

for(i in c(1:1000)) {
  reg = lm(data = reg_df_list[[i]], log_TC ~ log_Q + offset(log_PL) + restr_1 + restr_2 + z) # run 1,000 regressions
  reg_list[[i]] = reg
  ssr[[i]] = sum(reg$residuals^2) # store ssr 
} 

nlls_df = data.frame(beta_7 = beta_7, ssr = unlist(ssr)) # create nlls dataframe to find minimum of ssr and associated beta_7
beta_7 = nlls_df[nlls_df["ssr"] == min(nlls_df$ssr)][1] # find the row for which ssr is minimized and save beta_7
    
# Exercise 2. (d) ---------------------------------------------------------------------
best_model = reg_list[[which(nlls_df == min(nlls_df$ssr), arr.ind = TRUE)[1]]] # extract the best model from the regression list
coefs = c(best_model$coefficients, beta_7)

SSR_model = (t(best_model$residuals)%*%best_model$residuals)[1] # calculate SSR
n = nrow(best_model$model) # number of observations
k = length(best_model$coefficients)+1 # we have to add one because we used the concentration method for beta_7
deriv_mtx = matrix(c(rep(1, n), 
                      nerlove$log_Q, 
                      nerlove$restr_1, 
                      nerlove$restr_2,
                      nerlove$log_Q*(1+exp(beta_7-nerlove$log_Q))^(-1),
                      -best_model$coefficients["z"][[1]]*nerlove$log_Q*(1+exp(beta_7-nerlove$log_Q))^(-2)*exp(beta_7-nerlove$log_Q)),
                   ncol = 6)
VCOV = (SSR_model/(n-k))*solve(t(deriv_mtx)%*%deriv_mtx) # calculate the VCOV matrix
coefs_std_err = diag(sqrt(VCOV)) # calculate the std. errors

# we are still missing: i) the coefficient of beta_3 and the std. error of beta_3

# coef is easy to calculate. Add to the coef vector we already have
coefs = c(coefs, 1-coefs["restr_1"][[1]]-coefs["restr_2"][[1]])
names(coefs) = c("beta_1", "beta_2", "beta_4", "beta_5", "beta_6", "beta_7", "beta_3")

# std error is just a linear combination of the std. errors of beta_4 and beta_5
D = c(0, 0, -1, -1, 0, 0)
coefs_std_err = c(coefs_std_err, sqrt(t(D) %*% VCOV %*% D))
names(coefs_std_err) = c("beta_1", "beta_2", "beta_4", "beta_5", "beta_6", "beta_7", "beta_3")

# now the coefficients are
table = bind_rows(coefs, coefs_std_err) %>%
  relocate(beta_3, .after = beta_2)

table = bind_cols(" " = c("coefficient", "std_error"), table)
print(xtable(table), include.rownames=FALSE)


# Exercise 3. ---------------------------------------------------------------------

1- pnorm(1.64*(sqrt(2)/2), mean = 0, sd = 1)

# Exercise 6. ---------------------------------------------------------------------

rm(list=setdiff(ls(), "overleaf")) # clean up
ps4_data = read_xls("PS4data.xls") # load in the data
 

# Exercise 6. (a) ---------------------------------------------------------------------

ps4_data$c_pc = (ps4_data$`real consumption of nondurables`+ps4_data$`real consumption of services`)/ps4_data$population # create pc consumption
ps4_data$c_pc_l1 = lag(ps4_data$c_pc) # lag1
ps4_data$c_pc_l2 = lag(ps4_data$c_pc_l1) # lag2
ps4_data$c_pc_l3 = lag(ps4_data$c_pc_l2) # lag3
ps4_data$c_pc_l4 = lag(ps4_data$c_pc_l3) # lag4
ps4_data$c_pc_l5 = lag(ps4_data$c_pc_l4) # lag5
ps4_data$c_pc_l6 = lag(ps4_data$c_pc_l5) # lag6

reg_1_f = lm(c_pc ~ c_pc_l1 + c_pc_l2 + c_pc_l3 + c_pc_l4, data = ps4_data) # run regression
linearHypothesis(reg_1_f, c("c_pc_l2 = 0", "c_pc_l3 = 0", "c_pc_l4 = 0")) # test if coef's are zero

stargazer(reg_1_f, out = paste0(overleaf, "hall_1.tex")) # export output to overleaf

# Exercise 6. (c) ---------------------------------------------------------------------

# create real disposable income lag
ps4_data$y_pc = ps4_data$`real disposable income`/ps4_data$population # create per capita disposable income
ps4_data$y_pc_l1 = lag(ps4_data$y_pc) # create lag 

# create log differences of variables we need 
ps4_data$log_c_pc_diff = log(ps4_data$c_pc) - log(ps4_data$c_pc_l1) # dependent variable
ps4_data$log_y_pc_diff = log(ps4_data$y_pc) - log(ps4_data$y_pc_l1) # independent variable
ps4_data$log_c_pc_diff_2 = log(ps4_data$c_pc_l2) - log(ps4_data$c_pc_l3) # instrument 1
ps4_data$log_c_pc_diff_3 = log(ps4_data$c_pc_l3) - log(ps4_data$c_pc_l4) # instrument 2
ps4_data$log_c_pc_diff_4 = log(ps4_data$c_pc_l4) - log(ps4_data$c_pc_l5) # instrument 3
ps4_data$log_c_pc_diff_5 = log(ps4_data$c_pc_l5) - log(ps4_data$c_pc_l6) # instrument 4

iv_reg_1 = ivreg(log_c_pc_diff ~ log_y_pc_diff | log_c_pc_diff_2 + log_c_pc_diff_3 + log_c_pc_diff_4 + log_c_pc_diff_5, data = ps4_data) # run iv regression
iv_reg_1_rse = coeftest(iv_reg_1, vcov. = function(x) vcovHC(x, type="HC0")) # adjust standard errors to be robust (White)
summ_iv_reg_1 = summary(iv_reg_1, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics = T) # get robust tests

stargazer(iv_reg_1,
          se = list(c(iv_reg_1_rse[1, 2], iv_reg_1_rse[2, 2])),
          add.lines = list(c("Wu-Hausman (p-value)", round(summ_iv_reg_1$diagnostics[2, 4], 3)),
                           c("Sargan (p-value)", round(summ_iv_reg_1$diagnostics[3, 4], 3))),
          digits = 3,
          out = paste0(overleaf, "ivreg_1.tex")) # export output to overleaf

