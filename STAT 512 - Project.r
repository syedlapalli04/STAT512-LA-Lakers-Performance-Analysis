#lebron_total <- read.csv("/Users/shamitay/STAT 512/Project/lebron_james_total.csv")
lebron_per_game <- read.csv("/Users/shamitay/STAT 512/Project/lebron_james_per_game.csv")
my_subset <- lebron_per_game[, c("FT.", "X2P.", "X3P.", "TOV", "PTS")]
plot(my_subset)
model2 <- lm(PTS~FT.+X2P.+X3P.+TOV, lebron_per_game)
summary(model2)
anova(model2)
plot(model2)
#library(car)
residualPlots(model2)
cor(my_subset)

#if dev.list()
#dev.new()


#Start Here
per_game <- read.csv("/Users/shamitay/STAT 512/Project/per_game_combined.csv")
# Need to fill in NA values because there are a total of 13 rows that cannot be used unless done so.
per_game [is.na(per_game)] <- 0

my_subset <- per_game[, c("FT.", "X2P.", "X3P.", "TOV", "PTS")]
plot(my_subset)

model2 <- lm(PTS~FT.+X2P.+X3P.+TOV, per_game)
summary(model2)
anova(model2)

model2R <- lm(PTS~1, per_game)
anova(model2R, model2)

#library(car)
residualPlots(model2)

#if dev.list() = NULL
#dev.new()
plot(model2)
#Boxplot of residuals? boxplot(residuals(model2))

#maybe add x vs residual plots?
plot(per_game$FT., residuals(model2))
plot(per_game$X2P., residuals(model2))
plot(per_game$X3P., residuals(model2))
plot(per_game$TOV, residuals(model2))
#and y_hat vs residual plot
plot(fitted(model2), residuals(model2))

cor(my_subset)

#Shapiro Test
shapiro.test(residuals(model2))

#BP Test
#library(lmtest)
bptest(model2)

#Box-Cox Transformation
temp <- per_game[per_game$PTS > 0,]
library(MASS)
bc <- boxcox(lm(PTS~FT.+X2P.+X3P.+TOV, temp), lambda=seq(-3, 3, by=0.1))
lambda <- bc$x[which.max(bc$y)]
lambda #0.5757576

temp$Trans_PTS <- temp$PTS^0.5757576
model2bc <- lm(Trans_PTS~FT.+X2P.+X3P.+TOV, temp)
summary(model2bc)

#Shapiro Test
shapiro.test(residuals(model2bc))

#BP Test
#library(lmtest)
bptest(model2bc)















#No NA - Start Here
per_game <- read.csv("/Users/shamitay/STAT 512/Project/per_game_combined.csv")
per_game2 <- per_game [!is.na(per_game$FT.) & !is.na(per_game$X2P.) & !is.na(per_game$X3P.),]

my_subset <- per_game2[, c("FT.", "X2P.", "X3P.", "TOV", "PTS")]
plot(my_subset)

model2 <- lm(PTS~FT.+X2P.+X3P.+TOV, per_game2)
summary(model2)
anova(model2)

model2R <- lm(PTS~TOV, per_game2)
anova(model2R, model2)

#library(car)
residualPlots(model2)

plot(model2)

shapiro.test(residuals(model2)) #p-value = 0.009281
#library(lmtest)
bptest(model2) #p-value = 0.002365

library(MASS)
bc <- boxcox(model2, lambda=seq(-3, 3, by=0.1))
lambda <- bc$x[which.max(bc$y)]
lambda #0.6969697

#Box Cox Transformation
per_game2$PTS_Trans <- (per_game2$PTS)^lambda

model2Trans <- lm(PTS_Trans~FT.+X2P.+X3P.+TOV, per_game2)
summary(model2Trans)
anova(model2Trans)

model2TransR <- lm(PTS_Trans~TOV, per_game2)
anova(model2TransR, model2Trans)

#Transformed Residual vs Predictor Plots
library(car)
residualPlots(model2Trans)

#Transformed QQ Plot, Residual Plot
plot(model2Trans)

shapiro.test(residuals(model2Trans)) #p-value: 0.09773
#library(lmtest)
bptest(model2Trans) #p-value = 0.1817

#Best Subset
library(ALSM)
bs <- BestSub(per_game2[,c(15, 18, 22, 29)], per_game2$PTS_Trans, num=3)
bs

#Influential Cases
#DFFITS
dffits(model2)
#Cook's Distance
cooks.distance(model2)
per2 <- pf(cooks.distance(model2), 5, 144-5)
per2
#DFBETAS
dfbetas(model2)

library(car)
influencePlot(model2)

#Multicollinearity
vif(model2)
vif(lm(FT.~X2P.+X3P.+TOV, per_game2))
vif(lm(X2P.~FT.+X3P.+TOV, per_game2))
vif(lm(X3P.~FT.+X2P.+TOV, per_game2))
vif(lm(TOV~FT.+X2P.+X3P., per_game2))

vif(lm(FT.~X2P.+X3P., per_game2))
vif(lm(X2P.~FT.+X3P., per_game2))
vif(lm(X3P.~FT.+X2P., per_game2))


library(fmsb)
VIF(lm(FT.~X2P.+X3P.+TOV, per_game2))
VIF(lm(X2P.~FT.+X3P.+TOV, per_game2))
VIF(lm(X3P.~FT.+X2P.+TOV, per_game2))
VIF(lm(TOV~FT.+X2P.+X3P., per_game2))
VIF(lm(FT.~X2P.+X3P., per_game2))
VIF(lm(X2P.~FT.+X3P., per_game2))
VIF(lm(X3P.~FT.+X2P., per_game2))
VIF(lm(FT.~X2P., per_game2))
VIF(lm(FT.~X3P., per_game2))
VIF(lm(FT.~TOV, per_game2))
VIF(lm(X2P.~FT., per_game2))
VIF(lm(X2P.~X3P., per_game2))
VIF(lm(X2P.~TOV, per_game2))
VIF(lm(X3P.~TOV, per_game2))


#K-Fold Cross Validation
library(MASS)
library(leaps)
library(caret)
set.seed(123)
train.control <- trainControl(method="cv", number=10)
kfold.model2 <- train(PTS_Trans~FT.+X2P.+X3P.+TOV, data=per_game2, method="lm", trControl=train.control)
kfold.model2$results

#K-Fold Cross Validation on Original Model
library(MASS)
library(leaps)
library(caret)
set.seed(123)
train.control <- trainControl(method="cv", number=10)
kfold.model2 <- train(PTS~FT.+X2P.+X3P.+TOV, data=per_game2, method="lm", trControl=train.control)
kfold.model2$results