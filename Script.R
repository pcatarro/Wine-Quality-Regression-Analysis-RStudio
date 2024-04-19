##import dataset##
dados = read.csv("C:/Users/Admin_User/Downloads/winequality-red.csv")

##Linear Regression##
modelo <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
               residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
               density + pH + sulphates + alcohol, data = dados)

##Testing heteroskedasticity## 

#Breusch-Pagan Test#
library(lmtest)
bptest(modelo)

#white test full
bptest(modelo, studentize = FALSE)

#White Test (Special)

 # Create a formula for the White Test (Special)
formula_special <- as.formula(paste("quality ~", "fixed.acidity + volatile.acidity + citric.acid +",
"residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide +",
"density + pH + sulphates + alcohol +",
"I(fixed.acidity^2) + I(volatile.acidity^2) + I(citric.acid^2) +",
"I(residual.sugar^2) + I(chlorides^2) + I(free.sulfur.dioxide^2) +",
"I(total.sulfur.dioxide^2) + I(density^2) + I(pH^2) +",
"I(sulphates^2) + I(alcohol^2) +",
"fixed.acidity:volatile.acidity + fixed.acidity:citric.acid +",
"fixed.acidity:residual.sugar + fixed.acidity:chlorides +",
"fixed.acidity:free.sulfur.dioxide + fixed.acidity:total.sulfur.dioxide +",
"fixed.acidity:density + fixed.acidity:pH + fixed.acidity:sulphates +",
"fixed.acidity:alcohol + volatile.acidity:citric.acid +",
"volatile.acidity:residual.sugar + volatile.acidity:chlorides +",
"volatile.acidity:free.sulfur.dioxide + volatile.acidity:total.sulfur.dioxide +",
"volatile.acidity:density + volatile.acidity:pH + volatile.acidity:sulphates +",
"volatile.acidity:alcohol + citric.acid:residual.sugar +",
"citric.acid:chlorides + citric.acid:free.sulfur.dioxide +",
"citric.acid:total.sulfur.dioxide + citric.acid:density +",
"citric.acid:pH + citric.acid:sulphates + citric.acid:alcohol +",
"residual.sugar:chlorides + residual.sugar:free.sulfur.dioxide +",
"residual.sugar:total.sulfur.dioxide + residual.sugar:density +",
"residual.sugar:pH + residual.sugar:sulphates + residual.sugar:alcohol +",
"chlorides:free.sulfur.dioxide + chlorides:total.sulfur.dioxide +",
"chlorides:density + chlorides:pH + chlorides:sulphates +",
"chlorides:alcohol + free.sulfur.dioxide:total.sulfur.dioxide +",
"free.sulfur.dioxide:density + free.sulfur.dioxide:pH +",
"free.sulfur.dioxide:sulphates + free.sulfur.dioxide:alcohol +",
"total.sulfur.dioxide:density + total.sulfur.dioxide:pH +",
"total.sulfur.dioxide:sulphates + total.sulfur.dioxide:alcohol +",
"density:pH + density:sulphates + density:alcohol +",
"pH:sulphates + pH:alcohol + sulphates:alcohol"))

modelo_white_special <- lm(formula_special, data = dados)

##Robust Estimation##
robust_se <- coeftest(modelo, vcov = hccm)

##Reset Test##
RESETreg <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid +
                 residual.sugar + chlorides + free.sulfur.dioxide +
                 total.sulfur.dioxide + density + pH + sulphates + alcohol +
                 I(fitted(modelo)^2) + I(fitted(modelo)^3),
               data = dados)

