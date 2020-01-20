## Create data-set ####
y <- c(142.3, 144.0, 148.6, 146.9, 142.9, 147.4, 133.8, 133.2,
       134.9, 146.3, 145.2, 146.3, 125.9, 127.6, 108.9, 107.5,
       148.6, 156.5, 148.6, 153.1, 135.5, 138.9, 132.1, 149.7,
       152.0, 151.4, 149.7, 152.0, 142.9, 142.3, 141.7, 141.2)

trigly <- data.frame(y = y,
                     day = factor(rep(1:4, each = 8)),
                     machine = factor(rep(rep(1:4, each = 2), 2)))
str(trigly)

## verify number of observations
xtabs(~ day + machine, data = trigly)

## Plot data ####
with(trigly, interaction.plot(x.factor = day, trace.factor = machine,
                              response = y))

library(lmerTest)
fit.lme <- lmer(y ~ (1 | day) + (1 | machine) + (1 | machine:day), 
                control = lmerControl(optimizer = "bobyqa"), ## otherwise convergence warning
                data = trigly)
summary(fit.lme)

confint(fit.lme, oldNames = FALSE)
ranova(fit.lme) ## conservative, could also use package RLRsim

coef(fit.lme)
ranef(fit.lme)

## Residual analysis
plot(fit.lme) ## Tukey-Anscombe plot

par(mfrow = c(2, 2))
## QQ-plots of random effects
qqnorm(ranef(fit.lme)$day[,1], main = "day")
qqnorm(ranef(fit.lme)$machine[,1], main = "machine")
qqnorm(ranef(fit.lme)$'machine:day'[,1], main = "machine:day")
## QQ-plots of residuals
qqnorm(resid(fit.lme), main = "residuals")

par(mfrow = c(1, 1)) ## re-set plotting options

## OLD ##

## Classical approach ####
fit <- aov(y ~ day * machine, data = trigly)
summary(fit)

## tests: manually

## day
pf(444.8 / 87.3, df1 = 3, df2 = 9,  lower.tail = FALSE)
## machine
pf(549.1 / 87.3, df1 = 3, df2 = 9,  lower.tail = FALSE)
## interaction
pf(87.3 / 17.9,  df1 = 9, df2 = 16, lower.tail = FALSE)
