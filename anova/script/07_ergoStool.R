library(lmerTest)
library(multcomp)

## Get data & overview #####
data("ergoStool", package = "nlme")

str(ergoStool)
## technical detail:
ergoStool$Subject <- factor(ergoStool$Subject, ordered = FALSE)

## Have a look at data set
head(ergoStool)
xtabs(~ Subject + Type, data = ergoStool)

with(ergoStool, interaction.plot(x.factor = Type, trace.factor = Subject,
                                 response = effort))

## Fit mixed effects model ####
fit <- lmer(effort ~ Type + (1 | Subject), data = ergoStool)
summary(fit) ## no dummy.coef function available here

anova(fit) ## F-test for Type
rand(fit)  ## conservative tests for variance components
confint(fit, oldNames = FALSE) ## confidence intervals for all parameters

## Residual analysis ####
plot(fit)
qqnorm(ranef(fit)$Subject[,1])
qqnorm(resid(fit))

## side remark: can also use multcomp package with lmer-objects
mult <- glht(fit, linfct = mcp(Type = "Tukey"))
plot(confint(mult))

## Fit classical fixed effects model with aov
fit2 <- aov(effort ~ Type + Subject, data = ergoStool)
summary(fit2)
coef(fit2)