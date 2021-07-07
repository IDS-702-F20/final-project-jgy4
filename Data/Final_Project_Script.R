library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
library(grid)
library(rms)
library(stargazer)
library(lme4)
library(stringr)
require(dplyr)
library(dplyr)
library(lme4)
require(tab)
library(sjPlot)

counties <- read.csv("Documents/MIDS/final-project-jgy4/Data/counties.csv",header=T)
counties$year <- counties$year - mean(counties$year)
counties$year <- factor(counties$year)
counties$pct.white <- counties$pct.white - mean(counties$pct.white)
counties$pct.af.am <- counties$pct.af.am - mean(counties$pct.af.am)
counties$pct.hispanic <- counties$pct.hispanic - mean(counties$pct.hispanic)
counties$pct.asian <- counties$pct.asian - mean(counties$pct.asian)
summary(counties)
counties_c <- na.omit(counties)
summary(counties_c)

##Let's figure out an ideal response variable
par(mfrow=c(2,2))
hist(counties$eviction.filings)
hist(counties$evictions)
hist(counties$eviction.rate)
hist(counties$eviction.filing.rate)
#Log significantly improves distributions of our possible response variables
par(mfrow=c(2,2))
hist(log(counties$eviction.filings))
hist(log(counties$evictions))
hist(log(counties$eviction.rate))
hist(log(counties$eviction.filing.rate))

#Let's check some other variables
par(mfrow=c(2,2))
#Population is heavily skewed, might want to try bucketing
hist(counties$population)
#poverty rate looks good
hist(counties$poverty.rate)
hist(counties$renter.occupied.households)
#Use percent renter occupied instead of renter occupied, more normal distribution!
hist(counties$pct.renter.occupied)

#these all seem to look okay too
par(mfrow=c(2,2))
hist(counties$median.gross.rent)
#Slightly skewed, could bucket household income
hist(counties$median.household.income)
#Slightly skewed, could bucket median property value
hist(counties$median.property.value)
hist(counties$rent.burden)

#Lets look at relationships with our response variables
par(mfrow=c(2,2))
plot(counties$population,log(counties$eviction.rate))
plot(counties$poverty.rate,log(counties$eviction.rate))
plot(counties$pct.renter.occupied,log(counties$eviction.rate))
plot(counties$renter.occupied.households,log(counties$eviction.rate))

par(mfrow=c(2,2))
plot(counties$median.gross.rent,log(counties$eviction.rate))
plot(counties$median.household.income,log(counties$eviction.rate))
plot(counties$median.property.value,log(counties$eviction.rate))
plot(counties$rent.burden,log(counties$eviction.rate))

par(mfrow=c(2,2))
plot(counties$pct.white,log(counties$eviction.rate))
plot(counties$pct.af.am,log(counties$eviction.rate))
plot(counties$pct.hispanic,log(counties$eviction.rate))
plot(counties$pct.asian,log(counties$eviction.rate))

#I can't really explore interactions with everything numeric

#We decide to bucket population, median household income, and median property value
counties_c$pop_cat = 0
counties_c$pop_cat[counties_c$population > 50000] = 1
counties_c$pop_cat[counties_c$population > 200000] = 2
counties_c$pop_cat[counties_c$population > 400000] = 3
counties_c$pop_cat <- factor(counties_c$pop_cat)


counties_c$mhi_cat = 0
counties_c$mhi_cat[counties_c$median.household.income > 35000] = 1
counties_c$mhi_cat[counties_c$median.household.income > 50000] = 2
counties_c$mhi_cat <- factor(counties_c$mhi_cat)

counties_c$mpv_cat = 0
counties_c$mpv_cat[counties_c$median.property.value > 100000] = 1
counties_c$mpv_cat[counties_c$median.property.value > 150000] = 2
counties_c$mpv_cat[counties_c$median.property.value > 250000] = 3
counties_c$mpv_cat <- factor(counties_c$mpv_cat)

counties_c$eviction.rate[counties_c$eviction.rate == 0] <- 0.0001
counties_c$log_er <- log(counties_c$eviction.rate)
counties_c <- na.omit(counties_c)
summary(counties_c)

#Lets try a beginning model
summary(counties_c)

Model1 <- lm(log_er~population+poverty.rate+pct.renter.occupied+
               median.gross.rent+median.household.income+median.property.value+
               rent.burden+pct.white+pct.af.am+pct.hispanic+pct.asian,
             data=counties_c)
summary(Model1)

#Now some model assessment
ggplot(counties_c,aes(x=log(eviction.rate), y=Model1$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Eviction Rate",x="log(Eviction Rate)",y="Residuals")
#Clearly, some problems!!!

plot(Model1,which=1:5,col=c("blue4"))


####Model 2 with our new bucketed variables and YEAR####


Model2 <- lm(log_er~pop_cat+poverty.rate+pct.renter.occupied+
               median.gross.rent+mhi_cat+mpv_cat+year+
               rent.burden+pct.white+pct.af.am+pct.hispanic+pct.asian,
             data=counties_c)
summary(Model2)

#Now some model assessment
ggplot(counties_c,aes(x=log(eviction.rate), y=Model2$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Eviction Rate",x="log(Eviction Rate)",y="Residuals")
#Clearly, some problems!!!

plot(Model2,which=1:5,col=c("blue4"))


##### Trying Hierarchical Model ########
counties_c$name <- factor(counties_c$name)
#Here we use county name
Model3 <- lmer(log_er ~ pop_cat+poverty.rate+pct.renter.occupied+
                 median.gross.rent+mhi_cat+mpv_cat+year+
                 rent.burden+pct.white+pct.af.am+pct.hispanic+pct.asian+ 
                 (1 | name), data = counties_c)
summary(Model3)
confint(Model3)

dotplot(ranef(Model3, condVar=TRUE))

#Check Assumptions - Linearity
res <- residuals(Model3)
ggplot(counties_c, aes(log_er, y=res)) +
  geom_point(alpha = .5,colour="blue3") +
  geom_line(y=0, col="red3") +
  geom_smooth(method = "lm", col = "red3") +
  xlab("EE") +
  ylab("Residuals") +
  labs(caption="Residual vs EE") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20)) +
  facet_wrap(~name,ncol = 5)
#Independence + Equal Variance
pred <- predict(Model3)
pred_res <- data.frame(pred, res)
ggplot(pred_res, aes(pred, y=res)) +
  geom_point(alpha = .5,colour="blue3") +
  #geom_line(y = 0, col = "red3") +
  geom_smooth(method="lm",col="red3") +
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(caption="Residuals vs Fitted values") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20))
#Normality
std_res <- (res - mean(res)) / sd(res)
std_res_df <- data.frame(std_res)
qplot(sample = std_res, data = std_res_df, color=I("blue3"), alpha=.5) +
  geom_abline(intercept = 0, slope = 1, col="red3") +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  labs(caption="Normal Q-Q") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20), legend.position = "none")

#Here we use Year
Model4 <- lmer(log_er ~ pop_cat+poverty.rate+pct.renter.occupied+
                 median.gross.rent+mhi_cat+mpv_cat+
                 rent.burden+pct.white+pct.af.am+pct.hispanic+pct.asian+ 
                 (1 | year), data = counties_c)
summary(Model4)
confint(Model4)

dotplot(ranef(Model4, condVar=TRUE))
#Check Assumptions - Linearity
res2 <- residuals(Model4)
ggplot(counties_c, aes(log_er, y=res2)) +
  geom_point(alpha = .5,colour="blue3") +
  geom_line(y=0, col="red3") +
  geom_smooth(method = "lm", col = "red3") +
  xlab("EE") +
  ylab("Residuals") +
  labs(caption="Residual vs EE") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20)) +
  facet_wrap(~year,ncol = 5)
#Independence + Equal Variance
pred <- predict(Model4)
pred_res <- data.frame(pred, res2)
ggplot(pred_res, aes(pred, y=res2)) +
  geom_point(alpha = .5,colour="blue3") +
  #geom_line(y = 0, col = "red3") +
  geom_smooth(method="lm",col="red3") +
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(caption="Residuals vs Fitted values") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20))
#Normality
std_res2 <- (res2 - mean(res2)) / sd(res2)
std_res_df2 <- data.frame(std_res2)
qplot(sample = std_res2, data = std_res_df2, color=I("blue3"), alpha=.5) +
  geom_abline(intercept = 0, slope = 1, col="red3") +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  labs(caption="Normal Q-Q") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20), legend.position = "none")

#year and name
Model5 <- lmer(log_er ~ pop_cat+poverty.rate+pct.renter.occupied+
                 median.gross.rent+mhi_cat+mpv_cat+
                 rent.burden+pct.white+pct.af.am+pct.hispanic+pct.asian+ 
                 (1 | year) + (1 | name), data = counties_c)
summary(Model5)
confint(Model5)

dotplot(ranef(Model5, condVar=TRUE))
#Check Assumptions - Linearity
res2 <- residuals(Model5)
ggplot(counties_c, aes(log_er, y=res2)) +
  geom_point(alpha = .5,colour="blue3") +
  geom_line(y=0, col="red3") +
  geom_smooth(method = "lm", col = "red3") +
  xlab("EE") +
  ylab("Residuals") +
  labs(caption="Residual vs EE") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20)) +
  facet_wrap(~year,ncol = 5)
#Independence + Equal Variance
pred <- predict(Model5)
pred_res <- data.frame(pred, res2)
ggplot(pred_res, aes(pred, y=res2)) +
  geom_point(alpha = .5,colour="blue3") +
  #geom_line(y = 0, col = "red3") +
  geom_smooth(method="lm",col="red3") +
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(caption="Residuals vs Fitted values") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20))
#Normality
std_res2 <- (res2 - mean(res2)) / sd(res2)
std_res_df2 <- data.frame(std_res2)
qplot(sample = std_res2, data = std_res_df2, color=I("blue3"), alpha=.5) +
  geom_abline(intercept = 0, slope = 1, col="red3") +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  labs(caption="Normal Q-Q") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20), legend.position = "none")

##### NOTES #####
#According to anova tests Model 3 is our superior model, lets fine tune within Model 3
Model3 <- lmer(log_er ~ pop_cat+poverty.rate+pct.renter.occupied+
                 median.gross.rent+mhi_cat+mpv_cat+year+
                 rent.burden+pct.white+pct.af.am+pct.hispanic+pct.asian+ 
                 (1 | name), data = counties_c)
summary(Model3)
confint(Model3)
#remove race
Model3a <- lmer(log_er ~ pop_cat+poverty.rate+pct.renter.occupied+
                 median.gross.rent+mhi_cat+mpv_cat+year+
                 rent.burden+ (1 | name), data = counties_c)
summary(Model3a)
confint(Model3a)
#remove year
Model3b <- lmer(log_er ~ pop_cat+poverty.rate+pct.renter.occupied+
                  median.gross.rent+mhi_cat+mpv_cat+
                  rent.burden+pct.white+pct.af.am+pct.hispanic+pct.asian+ 
                  (1 | name), data = counties_c)
summary(Model3b)
confint(Model3b)
#remove pct. white for multicollinearity
Model3c <- lmer(log_er ~ pop_cat+poverty.rate+pct.renter.occupied+
                  median.gross.rent+mhi_cat+mpv_cat+year+
                  rent.burden+pct.af.am+pct.hispanic+pct.asian+ 
                  (1 | name), data = counties_c)
summary(Model3c)
confint(Model3c)
#Model 3 still reigns supreme, race and year are important based on anova
#Model 3 and Model 3c have no significant difference

# Check for multicollinearity
vcov(Model3,complete=TRUE) 
car::vif(Model3c)
#mean centering did not help multicollinearity for year, pct.white and pct. Af. Am.
#we remove pct. white to help multicollinearity (pct. white and pct. af am. go down)

tab_model(Model3c)

#Now lets look at our final regression assumptions with model 3c
#Check Assumptions - Linearity
res3c <- residuals(Model3c)
ggplot(counties_c, aes(log_er, y=res3c)) +
  geom_point(alpha = .5,colour="blue3") +
  geom_line(y=0, col="red3") +
  geom_smooth(method = "lm", col = "red3") +
  xlab("log(eviction rate)") +
  ylab("Residuals") +
  labs(caption="Residual vs log(eviction rate) by Year") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20)) +
  facet_wrap(~year,ncol = 5)
#Independence + Equal Variance
pred <- predict(Model3c)
pred_res <- data.frame(pred, res3c)
ggplot(pred_res, aes(pred, y=res3c)) +
  geom_point(alpha = .5,colour="blue3") +
  #geom_line(y = 0, col = "red3") +
  geom_smooth(method="lm",col="red3") +
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(caption="Residuals vs Fitted values") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20))
#Normality
std_res3c <- (res3c - mean(res3c)) / sd(res3c)
std_res_df3c <- data.frame(std_res2)
qplot(sample = std_res3c, data = std_res_df3c, color=I("blue3"), alpha=.5) +
  geom_abline(intercept = 0, slope = 1, col="red3") +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  labs(caption="Normal Q-Q") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20), legend.position = "none")

dotplot(ranef(Model3c, condVar=TRUE))
