cheese.data <- read.table("http://stat.ethz.ch/~meier/teaching/data/cheese.dat",
                          header = TRUE)
str(cheese.data)
cheese.data[, "rater"] <- factor(cheese.data[, "rater"])
str(cheese.data)
head(cheese.data)

## Visualize data ####
plot.design(cheese.data)
with(cheese.data, interaction.plot(x.factor = cheese, 
                                   trace.factor = interaction(background, rater),
                                   response = y, 
                                   col = rep(c("red", "blue"), times = 10)))

## more sophisticated plot
library(ggplot2)
ggplot(cheese.data, 
       aes(x = cheese, y = y, group = interaction(background, rater), 
           color = background)) + 
  stat_summary(fun.y = mean, geom = "line")

## Analyze data with lmer ####
library(lmerTest)

fit <- lmer(y ~ background * cheese + (1 | rater:background) +
              (1 | rater:background:cheese), data = cheese.data)
summary(fit)
anova(fit)  
## also compare with interaction plot!
## intuition about DenDF: See lecture notes Chapter 7.2.2

rand(fit)

confint(fit, oldNames = FALSE)

## Residual Analysis
plot(fit)
qqnorm(ranef(fit)$"rater:background"[,1])
qqnorm(ranef(fit)$"rater:background:cheese"[,1])
qqnorm(resid(fit))
