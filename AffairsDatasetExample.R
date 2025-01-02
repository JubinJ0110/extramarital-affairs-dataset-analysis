data("Affairs")
library(AER)
library(graphics)
print(summary(Affairs))
## Greene (2003)
## Tab. 22.3 and 22.4
fm_ols <- lm(affairs ~ age + yearsmarried + religiousness + occupation + rating,
             data = Affairs)
fm_probit <- glm(I(affairs > 0) ~ age + yearsmarried + religiousness + occupation + rating,
                 data = Affairs, family = binomial(link = "probit"))
fm_tobit <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
                  data = Affairs)
fm_tobit2 <- tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
                   right = 4, data = Affairs)
fm_pois <- glm(affairs ~ age + yearsmarried + religiousness + occupation + rating,
               data = Affairs, family = poisson)
library("MASS")
fm_nb <- glm.nb(affairs ~ age + yearsmarried + religiousness + occupation + rating,
                data = Affairs)
## Tab. 22.6
library("pscl")
fm_zip <- zeroinfl(affairs ~ age + yearsmarried + religiousness + occupation + rating | age +
                     yearsmarried + religiousness + occupation + rating, data = Affairs)


print(summary(fm_ols))
print(summary(fm_probit))
print(summary(fm_tobit))
print(summary(fm_tobit2))
print(summary(fm_pois))
print(summary(fm_nb))
print(summary(fm_zip))

plot(fm_ols)
plot(fm_ols, las=1)
summary(fm_ols)$r.squared
summary(fm_ols)$adj.r.squared
AIC(fm_ols)    
BIC(fm_ols)

anova(fm_ols)
predict.lm(fm_ols)
rstandard(fm_ols)

