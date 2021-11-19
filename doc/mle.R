## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, error = FALSE,
					  fig.align = 'center')

set.seed(2112)

library(tidyverse)
library(ggplot2)
library(reshape2)
library(gganimate)
library(magick)
library(cowplot)
library(DT)

data(mtcars)

## ---- echo = FALSE, fig.cap = 'Figure 1. Scatter plot of weight versus miles per gallan.'----
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

## ----echo = FALSE, fig.cap = 'Figure 2 Scatter plot with residuals.'----------
lm.out <- lm(mpg ~ wt, data = mtcars)
mtcars$predicted <- predict(lm.out)
mtcars$resid <- resid(lm.out)
ggplot(mtcars, aes(x = wt, y = mpg)) +
	geom_point() +
	geom_abline(intercept = lm.out$coefficients[1],
				slope = lm.out$coefficients[2], color = 'blue') +
	geom_segment(aes(xend = wt, yend = predicted), color = '#e31a1c')

## ----class.source = 'fold-show'-----------------------------------------------
y <- mtcars$mpg
x <- mtcars$wt
mean.y <- mean(y)
mean.x <- mean(x)
sd.y <- sd(y)
sd.x <- sd(x)
ols <- tibble(
	r = seq(-1, 1, by = 0.025),            # Correlation
	m = r * (sd.y / sd.x),                 # Slope
	b = mean.y - m * mean.x                # Intercept
) %>% rowwise() %>%
	mutate(ss = sum((y - (m * x + b))^2)) %>% # Sum of squares residuals
	as.data.frame()
datatable(ols) %>% formatRound(columns = names(ols), digits=3)

## ---- echo = FALSE, fig.cap = 'Figure 3. Residual sum of squares.'------------
ggplot(ols, aes(x = r, y = ss)) + geom_path() + geom_point()

## ----class.source = 'fold-show'-----------------------------------------------
ols %>% dplyr::filter(ss == min(ss)) # Select the row with the smallest sum of squares residuals

## ----class.source = 'fold-show'-----------------------------------------------
residual_sum_squares <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	predicted <- a + b * predictor
	residuals <- outcome - predicted
	ss <- sum(residuals^2)
	return(ss)
}

## ----class.source = 'fold-show'-----------------------------------------------
residual_sum_squares(c(37, -5), mtcars$wt, mtcars$mpg)

## ----class.source = 'fold-show'-----------------------------------------------
optim_save <- function(par, fn, ...) {
	iterations <- list()
	wrap_fun <- function(parameters, ...) {
		n <- length(iterations)
		result <- fn(parameters, ...)
		iterations[[n + 1]] <<- c(parameters, result)
		return(result)
	}
	optim_out <- stats::optim(par, wrap_fun, ...)
	optim_out$iterations <- iterations
	optim_out$iterations_df <- as.data.frame(do.call(rbind, iterations))
	names(optim_out$iterations_df) <- c(paste0('Param', 1:length(par)), 'Result')
	optim_out$iterations_df$Iteration <- 1:nrow(optim_out$iterations_df)
	return(optim_out)
}

## ----class.source = 'fold-show'-----------------------------------------------
optim.rss <- optim_save(
	par = runif(2),
	fn = residual_sum_squares, 
	method = "L-BFGS-B",
	predictor = mtcars$wt,
	outcome = mtcars$mpg
)

## ----class.source = 'fold-show'-----------------------------------------------
optim.rss$par

## ----class.source = 'fold-show'-----------------------------------------------
lm.out <- lm(mpg ~ wt, data = mtcars)
lm.out$coefficients

## ---- fig.cap = 'Figure 4. Output of the optimizaiton procedure at each iteration.'----
df <- optim.rss$iterations_df
names(df) <- c('Intercept', 'Slope', 'ResidualSumSquares', 'Iteration')
df %>% melt(id.var = 'Iteration') %>%
	ggplot(aes(x = Iteration, y = value, color = variable)) +
	geom_point(size = 1) + geom_path() +
	facet_wrap(~ variable, scales = "free_y", ncol = 1) +
	xlab('Iteration') + ylab('') + theme(legend.position = 'none')

## ---- echo = FALSE, fig.cap='Figure 5. Scatter plot with residuals for one observation.'----
pt <- 1 # Which observation do we want to explore
mtcars$fitted_mpg <- fitted(lm.out)
a <- lm.out$coefficients[1]
b <- lm.out$coefficients[2]
sigma <- summary(lm.out)$sigma
fitted.pt <- mtcars[pt,] * a + b
ggplot(mtcars, aes(x = wt, y = mpg)) +
	geom_point() +
	geom_segment(data = mtcars[pt,], color = 'red', size = 1,
				 aes(x = wt, xend = wt, y = mpg, yend = fitted_mpg)) +
	geom_point(data = mtcars[pt,], color = 'red', size = 4) +
	geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)

## ---- echo = FALSE, fig.cap = 'Figure 6. Probability distribution of miles per gallan.'----
ggplot() +
	stat_function(fun = dnorm, n = 101, geom = "line",
				  args = list(mean = mean(mtcars$mpg),
				  			  sd = sd(mtcars$mpg))) +
	stat_function(fun = dnorm, n = 101, geom = "area", fill = "steelblue",
				  args = list(mean = mean(mtcars$mpg),
				  			sd = sd(mtcars$mpg)),
				  xlim = c(mean(mtcars$mpg) - 3 * sd(mtcars$mpg), mtcars[pt,]$mpg)) +
	geom_segment(aes(x = mtcars[pt,]$mpg, xend = mtcars[pt,]$mpg),
				 y = 0, yend = dnorm(y[pt], mean(mtcars$mpg), sd(mtcars$mpg))) +
	xlim(mean(mtcars$mpg) - 3 * sd(mtcars$mpg), mean(mtcars$mpg) + 3 * sd(mtcars$mpg)) +
	xlab('Miles Per Gallon') + ylab('Density')

## ---- echo = FALSE, fig.cap = 'Figure 7. Likelihood of a car having the observed mpg given the model parameters for two observations.'----
pt1 <- 1
p1 <- ggplot() +
	stat_function(fun = dnorm, n = 101,
				  args = list(mean = a + b * mtcars[pt1,]$wt,
				  			  sd = sigma)) +
	geom_segment(aes(x = mtcars[pt1,]$mpg, xend = mtcars[pt1,]$mpg),
				     y = 0, yend = dnorm(y[pt1], a + b * x[pt1], sigma)) +
	geom_point(aes(x = mtcars[pt1,]$mpg, y = dnorm(y[pt1], a + b * x[pt1], sigma)),
			   color = 'red', size = 4) +
	xlim(mean(y) - 3 * sd(y), mean(y) + 3 * sd(y)) +
	xlab('') + ylab('Density')
pt2 <- 5
p2 <- ggplot() +
	stat_function(fun = dnorm, n = 101,
				  args = list(mean = a + b * mtcars[pt2,]$wt,
				  			  sd = sigma)) +
	geom_segment(aes(x = mtcars[pt2,]$mpg, xend = mtcars[pt2,]$mpg),
				     y = 0, yend = dnorm(y[pt2], a + b * x[pt2], sigma)) +
	geom_point(aes(x = mtcars[pt2,]$mpg, y = dnorm(y[pt2], a + b * x[pt2], sigma)),
			   color = 'red', size = 4) +
	xlim(mean(y) - 3 * sd(y), mean(y) + 3 * sd(y)) +
	# xlim((a + b * x[pt2]) - 3 * sigma, (a + b * x[pt2]) + 3 * sigma) +
	xlab('Miles per Gallon') + ylab('Density')
plot_grid(p1, p2, ncol = 1)

## ----class.source = 'fold-show'-----------------------------------------------
loglikelihood <- function(parameters, predictor, outcome) {
	a <- parameters[1]     # intercept
	b <- parameters[2]     # slope / beta coefficient
	sigma <- parameters[3] # error
	ll.vec <- dnorm(outcome, a + b * predictor, sigma, log = TRUE)
	return(sum(ll.vec))
}

## ----class.source = 'fold-show'-----------------------------------------------
loglikelihood(c(37, -5, sd(mtcars$mpg)),
			  predictor = mtcars$wt,
			  outcome = mtcars$mpg)

## ----class.source = 'fold-show'-----------------------------------------------
optim.ll <- optim_save(
	runif(3),                     # Random initial values
	loglikelihood,                # Log-likelihood function
	lower = c(-Inf, -Inf, 1.e-5), # The lower bounds for the values, note sigma (error), cannot be negative
	method = "L-BFGS-B",
	control = list(fnscale = -1), # Indicates that the maximum is desired rather than the minimum
	predictor = mtcars$wt,
	outcome = mtcars$mpg
)

## ----class.source = 'fold-show'-----------------------------------------------
optim.ll$par[1:2]
lm.out$coefficients

## ---- echo = FALSE, cache = TRUE, fig.cap = 'Figure 8. Animation of parameter estimates for each iteration of the optimization procedure.'----
df <- optim.ll$iterations_df
names(df) <- c('Intercept', 'Slope', 'Sigma', 'LogLikelihood', 'Iteration')
p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
	geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
	geom_abline(data = df, aes(intercept = Intercept, slope = Slope)) +
	geom_point(data = mtcars, aes(x = wt, y = mpg)) +
	transition_time(Iteration) +
	labs(title = "Iteration: {frame_time}") +
	shadow_wake(wake_length = 0.1, alpha = FALSE)
p1_gif <- animate(p1, width = 480, height = 480)

df.melt <- df %>% melt(id.var = 'Iteration')
p2 <- ggplot(df.melt, aes(x = Iteration, y = value, color = variable)) +
	geom_vline(data = data.frame(Iteration2 = df$Iteration),
			   aes(xintercept = Iteration2, frame = Iteration2)) +
	geom_path() +
	facet_wrap(~ variable, scales = "free_y", ncol = 1) +
	xlab('Iteration') + ylab('Parameter Estimate') +
	transition_time(Iteration2)
p2_gif <- animate(p2, width = 480, height = 480)

new_gif <- image_append(c(p1_gif[1], p2_gif[1]))
for(i in 2:100){
	combined <- image_append(c(p1_gif[i], p2_gif[i]))
	new_gif <- c(new_gif, combined)
}
new_gif

## ---- echo = TRUE, fig.cap = 'Figure 9. Likelihood for one observeration superimposed on scatter plot.'----
visualMLE::plot_likelihood(x = mtcars$wt, 
						   y = mtcars$mpg,
						   pt = 2,
						   intercept = optim.ll$par[1],
						   slope = optim.ll$par[2],
						   sigma = optim.ll$par[3])

## ---- cache = TRUE, fig.cap = 'Figure 10. Likelihoods of the first 16 observations for the final parameter estimates.'----
tmp <- df %>% dplyr::filter(Iteration == nrow(df))
plots <- list()
nplots <- 16 #nrow(mtcars)
for(i in 1:min(nplots, nrow(mtcars))) {
	a <- tmp[1,]$Intercept
	b <- tmp[1,]$Slope
	sigma <- tmp[1,]$Sigma
	predictor <- mtcars$wt[i]
	predicted.out <- a + b * predictor
	outcome <- mtcars$mpg[i]
	d <- dnorm(outcome, predicted.out, sigma)
	plots[[i]] <- ggplot() +
		stat_function(fun = dnorm,
					  n = 101,
					  args = list(mean = predicted.out, sd = sigma)) +
		annotate(geom = 'segment', x = outcome, y = 0, xend = outcome, yend = d, color = 'red') +
		annotate(geom = 'point', x = outcome, y = d, color = 'red', size = 2) +
		xlim(c(min(mtcars$mpg, predicted.out - 3 * sigma),
			   max(mtcars$mpg, predicted.out + 3 * sigma))) +
		ylim(c(0, .2)) +
		ylab('') + xlab(row.names(mtcars)[i])
}
plot_grid(plotlist = plots)

## -----------------------------------------------------------------------------
optim.ll$par[3]
sqrt(sum(resid(lm.out)^2) / nrow(mtcars))

## ---- fig.cap = 'Figure 11. Logistic curve'-----------------------------------
logistic <- function(t) { 
	return(1 / (1 + exp(-t))) 
}
ggplot() +
	stat_function(fun = logistic, n = 101) +
	xlim(-4, 4) + xlab('x')

## ----class.source = 'fold-show'-----------------------------------------------
study <- data.frame(
    Hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,
            3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
    Pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

## ---- echo=FALSE, fig.cap = 'Figure 12. Boxplot of hours studied by passing.'----
ggplot(study, aes(x = factor(Pass), y = Hours)) + geom_boxplot() + xlab('Pass') + ylab('Hours Studied')

## ----class.source = 'fold-show'-----------------------------------------------
logit <- function(x, beta0, beta1) {
	return( 1 / (1 + exp(-beta0 - beta1 * x)) )
}
loglikelihood.binomial <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	p <- logit(predictor, a, b)
	ll <- sum( outcome * log(p) + (1 - outcome) * log(1 - p))
	return(ll)
}

## ----class.source = 'fold-show'-----------------------------------------------
optim.binomial <- optim_save(
	c(0, 1), # Initial values
	loglikelihood.binomial,
	method = "L-BFGS-B",
	control = list(fnscale = -1),
	predictor = study$Hours,
	outcome = study$Pass
)

## -----------------------------------------------------------------------------
optim.binomial$par
lr.out <- glm(Pass ~ Hours, data = study, family = binomial(link = 'logit'))
lr.out$coefficients

## -----------------------------------------------------------------------------
# Redefine the logistic function to include parameter estimates
logistic <- function(x, beta0, beta1) {
    return(1 / (1 + exp(-1 * (beta0 + beta1 * x)) ))
}

beta0 <- optim.binomial$par[1]
beta1 <- optim.binomial$par[2]

ggplot(study, aes(x = Hours, y = Pass)) +
	geom_point(aes(color = logistic(Hours, beta0, beta1) > 0.5)) +
	stat_function(fun = logistic, n = 101, 
				  args = list(beta0 = beta0, beta1 = beta1) ) +
	scale_color_hue('Predicted Pass > 0.5') +
	theme(legend.position = c(0.85, 0.15))

## ---- echo = FALSE, cache = TRUE----------------------------------------------
df <- optim.binomial$iterations_df
names(df) <- c('Intercept', 'Hours', 'LogLikelihood', 'Iteration')
xlim <- c(0, 6) # Hard coding for now
df2 <- data.frame(Iteration = rep(1:nrow(df), each = 100))
xvals <- seq(xlim[1], xlim[2], length.out = 100)
tmp <- apply(
	df, 1, FUN = function(x) {
		logistic(xvals, x[1], x[2])
	}
) %>% as.data.frame()
names(tmp) <- 1:ncol(tmp)
tmp <- melt(tmp)
names(tmp) <- c('Iteration', 'Pass')
tmp$Hours <- rep(xvals, nrow(df))

nFrames <- nrow(df) * 2
p1 <- ggplot() + 
	geom_smooth(data = study, aes(x = Hours, y = Pass),
		method = 'glm', formula = y ~ x, se = FALSE, alpha = 0.5,
		method.args = list(family = binomial(link = 'logit'))) +
	geom_point(data = study, aes(x = Hours, y = Pass)) + 
	geom_path(data = tmp, aes(x = Hours, y = Pass, group = Iteration)) +
	transition_states(Iteration) +
	labs(title = "Iteration: {round(frame/2)}") +
	shadow_wake(wake_length = 0.1, alpha = FALSE) +
	ease_aes("cubic-in")
p1_gif <- animate(p1, nframes = nFrames, width = 480, height = 480)

df.melt <- df %>% melt(id.var = 'Iteration')
p2 <- ggplot(df.melt, aes(x = Iteration, y = value, color = variable)) +
	geom_vline(data = data.frame(Iteration2 = df$Iteration),
			   aes(xintercept = Iteration2, frame = Iteration2)) +
	geom_path() +
	facet_wrap(~ variable, scales = "free_y", ncol = 1) +
	xlab('Iteration') + ylab('Parameter Estimate') +
	theme(legend.position = 'none') +
	transition_time(Iteration2)
p2_gif <- animate(p2, nframes = nFrames, width = 480, height = 480)

new_gif <- image_append(c(p1_gif[1], p2_gif[1]))
for(i in 2:nFrames){
	combined <- image_append(c(p1_gif[i], p2_gif[i]))
	new_gif <- c(new_gif, combined)
}
new_gif

## ---- echo = FALSE------------------------------------------------------------
pt <- 1
beta0 <- optim.binomial$par[1]
beta1 <- optim.binomial$par[2]

df2 <- data.frame(x = xvals,
				  p = logistic(xvals, beta0, beta1))

ggplot(df2, aes(x = x, y = p)) + 
	geom_path() + 
	geom_segment(aes(x = study[pt,]$Hours, xend = study[pt,]$Hours,
					 y = study[pt,]$Pass, yend = logit(study[pt,]$Hours, beta0, beta1)),
				 color = 'red', size = 1) +
	geom_point(aes(x = study[pt,]$Hours, y = logit(study[pt,]$Hours, beta0, beta1)),
			   color = 'red', size = 4)

study$p <- logit(study$Hours, beta0, beta1)
ggplot(df2, aes(x = x, y = p)) + 
	geom_path() + 
	geom_segment(data = study, aes(x = Hours, xend = Hours,
								   y = Pass, yend = p), color = 'red', size = 1) +
	geom_point(data = study, aes(x = Hours, y = p), color = 'red', size = 4)


