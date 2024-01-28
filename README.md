# PPP - Parametric Portfolio Policies

## PMP Academic team implementation of the Parametric Portfolio Policies (PPP) approach.

Reference Paper:  Brandt, Santa-Clara, Valkanov (2009): Parametric Portfolio Policies: Exploiting Characteristics in the Cross-Section of Equity Returns. 
The Review of Financial Studies, Vol. 22, Issue 9, pp. 3411-3447, 2009, Available at SSRN: https://ssrn.com/abstract=1468198

Model Implementation: https://www.tidy-finance.org/r/index.html

This README is a work in progress. 

Topics: 
* factors (basline idea, which factos? using firm fundamentals, non-linearity & interactions)
* data (data sources, issues with cleaning)
* data pre-processing (standardization, macro, truncating returns, dealing with penny-stocks)
* utility (accounting for risk)
* optimization (routine, rolling vs. expanding window, optimizing over alternative objective functions)
* weights (constraints, shorting, leverage)
* benchmark portfolios (equally weighted, value weighted)
* factor coefficients (non-singular solutions --> rescaling)
* diagnostics (benchmark portfolios, monthly returns [skewness, kurtosis], risk adjusted returns (sharpe ratio, sortino ratio), CAPM alpha, market beta, factor coefficients)

## Model specifications
Optimization window: 10 year (120 month) rolling window. 
Optimization routine: Particle swarm optimization (PSO) 
Portfolio weight constraints: long only, top 50 weight, 1% <= weight_i <= 5%

![Histogram of monthly excess returns](/material/PPP_6coef_pso_10y_returnhist.png?raw=true "Coefficients of PPP")
![Accumulated monthly Returns](/material/PPP_6coef_pso_10y_accreturn.png?raw=true "Accumulated excess returns")
![Coefficients of PPP](/material/PPP_6coef_pso_10y_coefplot.png?raw=true "Coefficients of PPP")


