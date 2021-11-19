## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, error = FALSE,
					  fig.align = 'center', fig.height = 4)

set.seed(2112)

library(tidyverse)
library(visualMLE)
library(psych)
library(car)

## -----------------------------------------------------------------------------
study <- data.frame(
	Hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,
			3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
	Pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

ggplot(study, aes(x = Hours, y = Pass)) + 
	geom_point(aes(color = factor(Pass))) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

## -----------------------------------------------------------------------------
linear_regression <- lm(Pass ~ Hours, data = study)
linear_regression

## -----------------------------------------------------------------------------
ggplot(study, aes(x = Hours, y = Pass)) + 
	geom_point(aes(color = factor(Pass))) +
	geom_abline(slope = linear_regression$coefficients[2],
				intercept = linear_regression$coefficients[1]) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

## -----------------------------------------------------------------------------
study$linear_resid <- resid(linear_regression)
ggplot(study, aes(x = Hours, y = linear_resid)) + geom_point()

## -----------------------------------------------------------------------------
ols_linear <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	predicted <- a + b * predictor
	residuals <- outcome - predicted
	ss <- sum(residuals^2)
	return(ss)
}

## -----------------------------------------------------------------------------
optim_ols_linear <- optim(
	c(0, 1), # Initial values
	ols_linear,
	method = "L-BFGS-B",
	predictor = study$Hours,
	outcome = study$Pass
)
optim_ols_linear$par

## -----------------------------------------------------------------------------
logit <- function(x, beta0, beta1) {
	return( 1 / (1 + exp(-beta0 - beta1 * x)) )
}

ggplot() + 
	stat_function(fun = logit, args = list(beta0 = 0, beta1 = 1)) +
	xlim(-5, 5)

## -----------------------------------------------------------------------------
min_abs_resid <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	p <- logit(predictor, a, b)
	resid <- outcome - p
	return(sum(abs(resid)))
}

## -----------------------------------------------------------------------------
optim_min_abs_resid <- optim(
	c(0, 1), # Initial values
	min_abs_resid,
	method = "L-BFGS-B",
	predictor = study$Hours,
	outcome = study$Pass
)
optim_min_abs_resid$par

## -----------------------------------------------------------------------------
ggplot(data = study, aes(x = Hours, y = Pass)) + 
	geom_point(aes(color = factor(Pass))) +
	geom_function(fun = logit, args = list(beta0 = optim_min_abs_resid$par[1], 
										   beta1 = optim_min_abs_resid$par[2])) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

## -----------------------------------------------------------------------------
ols_logistic <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	p <- logit(predictor, a, b)
	resid <- outcome - p
	return(sum(resid^2))
}

## -----------------------------------------------------------------------------
optim_ols_logistic <- optim(
	c(0, 1), # Initial values
	ols_logistic,
	method = "L-BFGS-B",
	predictor = study$Hours,
	outcome = study$Pass
	
)
optim_ols_logistic$par

## -----------------------------------------------------------------------------
ggplot(data = study, aes(x = Hours, y = Pass)) + 
	geom_point(aes(color = factor(Pass))) +
	geom_function(fun = logit, args = list(beta0 = optim_ols_logistic$par[1], 
										   beta1 = optim_ols_logistic$par[2])) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

## -----------------------------------------------------------------------------
study$predict_ols_logistic <- logit(study$Hours,
									beta0 = optim_ols_logistic$par[1],
									beta1 = optim_ols_logistic$par[2])
study$resid_ols_logistc <- study$Pass - study$predict_ols_logistic
ggplot(study, aes(x = Hours, y = resid_ols_logistc)) + geom_point()

## -----------------------------------------------------------------------------
loglikelihood.binomial <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	p <- logit(predictor, a, b)
	ll <- sum( outcome * log(p) + (1 - outcome) * log(1 - p))
	return(ll)
}

## -----------------------------------------------------------------------------
optim_binomial <- optim_save(
	c(0, 1), # Initial values
	loglikelihood.binomial,
	method = "L-BFGS-B",
	control = list(fnscale = -1),
	predictor = study$Hours,
	outcome = study$Pass
)
optim_binomial$par

## -----------------------------------------------------------------------------
ggplot(data = study, aes(x = Hours, y = Pass)) + 
	geom_point(aes(color = factor(Pass))) +
	geom_function(fun = logit, args = list(beta0 = optim_ols_logistic$par[1], 
										   beta1 = optim_ols_logistic$par[2]),
				  linetype = 2) +
	geom_function(fun = logit, args = list(beta0 = optim_binomial$par[1], 
										   beta1 = optim_binomial$par[2])) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

## -----------------------------------------------------------------------------
ggplot() + geom_function(fun = log) + xlim(0, 1)

## -----------------------------------------------------------------------------
study$likelihood <- logit(study$Hours,								   
						  beta0 = optim_binomial$par[1], 
						  beta1 = optim_binomial$par[2])
study$log_likelihood <- study$Pass * log(study$likelihood) +         # If Pass == 1
	                    (1 - study$Pass) * log(1 - study$likelihood) # If Pass == 0

## -----------------------------------------------------------------------------
ggplot(data = study, aes(x = Hours, y = Pass)) + 
	geom_smooth(method = 'glm', method.args = list(family=binomial(link='logit')), se = FALSE, alpha = 0.2) +
	geom_hline(yintercept = 0) +
	geom_function(fun = logit, color = 'grey50', size = 1,
				  args = list(beta0 = optim_binomial$par[1], beta1 = optim_binomial$par[2])) +
	geom_segment(aes(xend = Hours, y = 1 - Pass, yend = likelihood, color = factor(Pass)), alpha = 0.5) +
	geom_point(aes(y = likelihood, color = factor(Pass), shape = 'Predicted'), size = 3) +
	geom_point(aes(color = factor(Pass), shape = 'Observed'), size = 3) +
	geom_point(aes(y = log_likelihood, color = factor(Pass), shape = 'Log Likelihood'), size = 3) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

## -----------------------------------------------------------------------------
q <- quantile(study$Hours, probs = seq(0, 1, 0.2))
study$quantile <- cut(study$Hours, breaks = q, include.lowest = TRUE)
probs <- prop.table(table(study$quantile, study$Pass), 1)[,2]
# Since we have probablies of exactly 0 and 1, the log will be undefined unless we fix.
probs[which(probs == 0)] <- .01
probs[which(probs == 1)] <- .99
logits <- log(probs / (1 - probs))
medians <- psych::describeBy(study$Hours, group = study$quantile, mat = TRUE)$median
plot(medians, logits, ylab = 'Log-Odds', xlab = 'Median Hours Studied')

## -----------------------------------------------------------------------------
lr.out <- glm(Pass ~ Hours, data = study, family = binomial(link='logit'))
study$logodds <- lr.out$linear.predictors
ggplot(study, aes(x = logodds, y = Hours)) + geom_point()
car::boxTidwell(logodds ~ Hours, data = study) # Looking for a non-significat p-value

