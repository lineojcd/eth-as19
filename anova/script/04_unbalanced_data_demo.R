## Read data ####
running <- read.table("http://stat.ethz.ch/~meier/teaching/data/running.dat", header = TRUE)
str(running)

## Visualize data ####
xtabs(~ gender + drink, data = running)

with(running, interaction.plot(drink, gender, y))

## Analyze data ####

## Type I sum of squares
options(contrasts = c("contr.sum", "contr.sum"))

fit <- aov(y ~ gender * drink, data = running)
summary(fit)

fit2 <- aov(y ~ drink * gender, data = running)
summary(fit2) ## sum of squares change!

coef(fit)
coef(fit2) ## the coefficients are of course the same

## Type II sum of squares
library(car)
Anova(fit, type = "II")

## Type III sum of squares

drop1(fit, scope = ~., test = "F")
## main effects tests typically not very meaningful
## if we have a *significant* interaction in the model

## We can also drop multiple terms at the same time
## (using the model comparison approach)
fit.A <- aov(y ~ gender, data = running)
fit.B <- aov(y ~ gender * drink, data = running)
anova(fit.A, fit.B)
