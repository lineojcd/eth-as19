## Define data set ####
combinations <- expand.grid(location = c("Taglieda", "Pfyn", "Rheinau"),
                            exposure = c("short", "long", "permanent"))
data <- data.frame(rbind(combinations, combinations), 
                   y = c(25, 45, 50, 42, 62, 52, 62, 80, 88, 
                         25, 42, 50, 38, 58,62, 55, 75, 95))

## Visualize data set ####

## first count the number of observations
xtabs(~ location + exposure, data = data)

## mean value per factor level (completely ignoring the other factor)
plot.design(y ~ ., data = data)

## Plot for every combination
## interaction command combines two factors into one
## (have a look at interaction(data$exposure, data$location))
stripchart(y ~ interaction(exposure, location), data = data, pch = 1, 
           vertical = TRUE,
           method = "jitter") ## jitter because of replicates

## better: use interaction plot
interaction.plot(x.factor = data$exposure, 
                 trace.factor = data$location, 
                 response = data$y)

## more elegat legend: Use function "with"
with(data, interaction.plot(x.factor = exposure, trace.factor = location,
                            response = y))
## conclusions?

## Fit two-way ANOVA model ####
fit <- aov(y ~ location * exposure, data = data)
#fit <- aov(y ~ location + exposure + location:exposure, data = data) ## equivalent version

summary(fit)

coef(fit)
dummy.coef(fit)

## compare with options(contrasts = c("contr.sum", "contr.sum"))


#> dummy.coef(fit)
#Full coefficients are 
#
# (Intercept):                      25                                                              
# location:                   Taglieda       Pfyn       Rheinau                                     
#                                  0.0       18.5          25.0                                     
# exposure:                      short       long     permanent                                     
#                                  0.0       15.0          33.5                                     
# location:exposure:    Taglieda:short Pfyn:short Rheinau:short Taglieda:long Pfyn:long Rheinau:long
#                                  0.0        0.0           0.0           0.0       1.5         -8.0
# 
# (Intercept):                                                             
#   location:                                                                
#   
#   exposure:                                                                
#   
#   location:exposure:    Taglieda:permanent Pfyn:permanent Rheinau:permanent
#                                        0.0            0.5               8.0