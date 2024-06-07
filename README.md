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


To dos: 
* attribution of factors to weights
* weight contraints (shorting, leverage)
* better labels for the portfolios
* 

## Parametric Portfolio Policies - General Concept


## Factor Investing


## Data 

## Utility



## Optimization Routines
While in theory, any optimization routine will do, it makes sense to shortly illustrate the optimization problem at hand in order to think about possible strengths and weaknesses of different optimization routines. 

The PPP optimizes:

$$ 
\begin{equation}
\max_{\{w_i,t\}^{N_t}_{i=1}} \mathop{\mathbb{E_t}}[u(r_{p,t+1})] = \mathop{\mathbb{E_t}}\left[u\left( \sum w_{i,t}r_{i,t+1}\right) \right] 
\end{equation}
$$

Where the weights $w_{i,t}$ are:

$$ 
\begin{equation}
\text{equation 2}
\end{equation}
$$

The algorithm thus constructs portfolios with initialization weights, computes the returns, then alters the weights 

There are several options for optimization routines. 

### Gradient descent
`optim` 

Old iterations of the PPP used a gradient descent (ascend) method. However, this method tends to get "stuck" in the first local optimum from the point of initialization. 
The point on initalization can be freely chosen and it makes sense to start with a vector of **0**. This will lead to the 

### Partical Swarm Optimization
Particle Swarm Optimization is a global optimization routine. It 

Problem: 
The problem with particle swarm optimization is that it too needs 


## Diagnostics

## Pseudo Code Summary

The algorithm can be summarized in the following way:
>while not convergence:
>
>> compute $\nabla(J)$ 
>>
>> Define 
>>
>> $\theta_0 := \theta_0 - \alpha\nabla(J)_0$
>>
>> $\theta_1 := \theta_1 - \alpha\nabla(J)_1$
>
>end while
## FAQ

## Weights / Output

## Variables / Interactions / Non-linearities
The PPP rests on 



## Additions
- exclusion lists
- include more asset classes
- attribution of effects to the weights
- LOO cross validation
- benchmarking to other portfolios / strategies