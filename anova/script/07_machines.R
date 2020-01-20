library(lmerTest)

## Change to contr.sum for an easier comparison with fixed effects
## model at the end
options(contrasts = c("contr.sum", "contr.poly"))

## Get data + overview #####
data("Machines", package = "nlme")

str(Machines)
## technical detail:
Machines$Worker <- factor(Machines$Worker, ordered = FALSE)

head(Machines)
xtabs(~ Worker + Machine, data = Machines)

## Visualize data set ####
with(Machines, interaction.plot(x.factor = Machine, trace.factor = Worker,
                                response = score))

## a fancier plot using the ggplot2 package
library(ggplot2)
ggplot(Machines, aes(x = Machine, y = score, group = Worker, col = Worker)) + 
  geom_point() + stat_summary(fun.y = mean, geom = "line")

## Fit mixed effects model ####
fit <- lmer(score ~ Machine + (1 | Worker) + (1 | Worker:Machine), 
            data = Machines)
summary(fit)
fixef(fit) ## estimates of fixed effects

anova(fit) ## F-test for Machine
rand(fit)  ## conservative tests for variance components ## or use ranova
confint(fit, oldNames = FALSE) ## confidence intervals for all parameters

## Residual analysis ####
plot(fit)
par(mfrow = c(2, 2))
qqnorm(ranef(fit)$Worker[,1])
qqnorm(ranef(fit)$'Worker:Machine'[,1])
qqnorm(resid(fit))

## Compare with fitting a classical fixed effects model
fit2 <- aov(score ~ Machine * Worker, data = Machines)
summary(fit2)
## fixed effects model is much more significant

coef(fit2) 
## fixed effects coefficients look the same!

## Old approach
fit3 <- aov(score ~ Machine + Error(Worker + Machine:Worker), data = Machines)
summary(fit3)
## now aov get's the same p-value for the main-effect of machine
coef(fit3)

## equivalent formulation
fit4 <- aov(score ~ Machine + Error(Worker/Machine), data = Machines)
summary(fit4)
