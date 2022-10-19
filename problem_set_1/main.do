// Code written by Julian Marenz //
// This do-file is solving exercise 5. of problem set 1. //
// You will need the following packages from ssc: estout //

cd "~/OneDrive - London Business School/PhD_Courses/2022_03AUT_Econometrics/PS1/" // set wd
use PS1, clear // open file


// create variables that we need and assign labels//
gen exp = a0 - (ed0 + 6) // create experience variable
gen exp_sq = exp^2 // create the square 
gen log_w0 = log(w0) // create log of wage

label variable a0 "Age"
label variable ed0 "Education"
label variable exp "Experience"
label variable exp_sq "Experience Squared"
label variable log_w0 "log(wage)"

// run regressions //
est clear

eststo: reg log_w0 ed0 exp exp_sq // run regression
esttab est1 using "regression1.tex", replace  ///
 b(3) se(3) nomtitle label star(* 0.10 ** 0.05 *** 0.01) booktabs title("Dep. Var. log(wage) \label{reg1}")
predict log_w0_fit // extract fitted values
label variable log_w0_fit "Fitted log(wage)"

eststo: reg log_w0 ed0 exp log_w0_fit // re-do regression including as predictor fitted values
esttab est2 using "regression2.tex", replace  ///
 b(3) se(3) nomtitle label star(* 0.10 ** 0.05 *** 0.01) booktabs title("Dep. Var. log(wage) \label{reg2}")
 
reg log_w0 ed0 exp_sq // partial out log(wage)  
predict log_w0_partial, residual // extract esiduals
label variable log_w0_fit "Fitted log(wage), part."

reg exp ed0 exp_sq // partial out experience
predict exp_partial, residual  // extract residuals
label variable exp_partial "Fitted experience, part."

eststo: reg log_w0_partial exp_partial // regress partialled out log(wage) on partialled out experience
esttab est3 using "regression3.tex", replace  ///
 b(3) se(3) nomtitle label star(* 0.10 ** 0.05 *** 0.01) booktabs title("Dep. var. predicted log(wage) from regression on constant, education, experience squared \label{reg4}")

eststo: reg log_w0 exp_partial // regress log(wage) on partialled out experience
esttab est4 using "regression4.tex", replace  ///
 b(3) se(3) nomtitle label star(* 0.10 ** 0.05 *** 0.01) booktabs title("Dep. var. predicted log(wage) from regression on constant, education, experience squared \label{reg5}")
