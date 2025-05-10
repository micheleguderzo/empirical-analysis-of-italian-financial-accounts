#######################################
##                                   ##
##  Advanced Econometrics - Project  ##
##                                   ##
#######################################


rm(list=ls())

gc()
Sys.setenv(LANG = "en")
setwd("...")
options(scipen = 100)

requiredPackages = c("stargazer", "lmtest", "sandwich", "tseries", "car",
                     "haven", "MASS", "plm", "zoo", "aod", "htmltools", "mfx",
                     "logistf", "DescTools", "maxLik", "pscl", "ucminf",
                     "ordinal", "reshape", "generalhoslem", "oglmx", "brant",
                     "nnet", "Formula", "miscTools", "mlogit", "survival",
                     "AER", "installr", "coin", "vcd", "vcdExtra", "censReg",
                     "truncreg", "sampleSelection", "mvtnorm", "xts", "fBasics",
                     "urca", "fUnitRoots", "dynlm", "devtools", "akima",
                     "ggplot2", "viridisLite", "readxl", "ARDL", "dynamac",
                     "forecast", "TSstudio", "rstatix", "dplyr", "tidyverse")

# Install packages
for(i in requiredPackages){
    if(!require(i, character.only = TRUE))
    install.packages(i)
}

# Load packages
for(i in requiredPackages){
    if(!require(i, character.only = TRUE))
    library(i, character.only = TRUE)
}


# -------------------------------------------------------------------------- #


## ------------------------- ##
## Import and adjust dataset ##
## ------------------------- ##

itec <- read_excel("dataset_cleaned.xlsx")
#View(itec)
head(itec)
#tail(itec)
attach(itec)

# We remove the NA values (if any)
itec <- na.omit(itec)

# First of all, we check if the variable year is a "Date" type
class(year)
# It is not, we need to change it
itec$year <- as.Date(as.character(itec$year), "%Y")
class(itec$year)
itec$year
head(year)
# It works, even if the function automatically added the current month and day,
# but I don't think it's a problem

# We need to change all our data into xts objects (or zoo) except the Date
# variable (year)
itec <- xts(itec[,-1], itec$year)
class(itec)
head(itec)

# We will start analyzing the variable that indicates the GDP because it will
# be our dependent variable

# Let's change the name of the variable that indicates GDP
colnames(itec)
names(itec)[3] <- "gdp"

# Let's put the gdp column as first
itec <- itec[, c("gdp", "va_tot", "net_indir_tax", "imp", "tot_supply", "exp",
               "total_cons", "tot_fixed_inv", "invent_variation",
               "tot_invest", "tot_users")]
#View(itec)

# Now we plot the GDP
plot(itec$gdp, main = "GDP")
# The plot shows that the gdp remains at low levels for the first 10-15 years,
# then rises quite abruptly until 2011 following what appears to be a linear
# trend. However, considering the whole years, it maybe follows a quadratic
# trend as well

# Let's try to remove the first 15 observations
#itec <- itec[-c(1:15), ]
#View(itec)

# Plot again
#plot(itec$gdp, main = "Italy GDP 1961-2011")



## ---------------- ##
## Stationary tests ##
## ---------------- ##

## GDP

# Now we will begin to perform the stationary tests, importing the preloaded
# function
source("function_testdf2.R")

# Since the plot shows an hypothetic quadratic trend, we will use the type
# "c", i.e. random walk with drift

## First test: Let's perform the II-type of Dickey-Fuller (ADF) test

testdf2(variable = itec$gdp,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# Here we need to have no autocorrelation in residuals, i.e. we need to find
# the first row that presents Breusch-Pagan p_values > 0.05 till order 5,
# which is the second row (augmentation 1). Thus, we check the corresponding
# p_adf in the same row: 0.99 --> we can't reject the null hypothesis of
# non-stationarity, so the process seems to be a random walk with drift

# We can try also to test the III-type of the DF test ("ct")
testdf2(variable = itec$gdp,
        test.type = "ct",
        max.augmentations = 3,
        max.order = 5)
# The correct row is the same (2nd), but now we have very lower evidence
# against the alternative hypothesis of stationarity (p_adf=0.31182501)
# Our first intuition was correct --> it's better to think that this variable
# follows a random walk process with drift

# However, we need to have a stationary variable --> let's calculate the
# first differences and plot them
plot(diff(itec$gdp), main="dGDP")
# It doesn't seem so stationary, plus the variance is increasing

testdf2(variable = diff(itec$gdp),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# Considering up to the fifth order, the 1st row is the correct one, and also
# p_adf (0.02761296) < 0.05 --> we reject the null hypothesis of
# non-stationarity, it means that the first difference of gdp is, instead,
# stationary
# Probably it's because the first observations are all around 0, maybe it
# shouldn't be stationary. Infact, if we increase the value of max.order, we
# will find autocorrelation in residuals. However, for the purpose of the 
# analyses, we will consider it stationary

# We calculate the same test for log(gdp) and diff(log(gdp)), just to check
plot(log(itec$gdp))
plot(diff(log(itec$gdp)))

testdf2(variable = log(itec$gdp),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)

testdf2(variable = diff(log(itec$gdp)),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)

# Check the second differences
testdf2(variable = diff(diff(log(itec$gdp))),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# Only the second differences are stationary, but they are really good

# --> Better the normal gdp because we need to differentiate only one time


## Second test: let's perform the Phillips-Perron (PP) test

pp.test <- ur.pp(itec$gdp,
                 type = c("Z-tau"),
                 model = c("constant"))
summary(pp.test)
# Value of the test-statistic Z-tau (1,7799) is higher than the 5% critical
# value (-2.920204). The critical region is (-infinity, -2.92) so we can not
# reject the null hypothesis about non-stationarity of GDP

# Let's check for the first differences of GDP
pp.test.d <- ur.pp(diff(itec$gdp),
                 type = c("Z-tau"),
                 model = c("constant"))
summary(pp.test.d)
# The value of Z-tau (-2.9728) is lower than the critical value at 5%
# (-2.921459). It means that we can reject H0, so the first differences of
# GDP appear to be stationary (the same conclusion as DF test)


## Even if no necessary, let's perform the
## Third test: KPSS test

kpss.test <- ur.kpss(itec$gdp, type = c("mu"))
summary(kpss.test)
# We need to keep in mind that here H0 and H1 are reversed
# The value of test-statistic is 1.3277, higher than the 5% critical value
# (0.463), so we reject the null hypotesis about stationarity of GDP

kpss.test.d <- ur.kpss(diff(itec$gdp), type = c("mu"))
summary(kpss.test.d)
# The value of the test-statistic (0.6933) is still higher than the 5%
# critical value (0.463), So it seems that the first differences of GDP are
# not stationary either. However, since we have two (out of three) tests which
# say that the first differences of this variable are stationary, we consider
# them as such
# Moreover, we need to keep in mind that it can be a realization of type one
# error

# Conclusions: GDP is a random walk with drift process, I(1)


##### [

## Let's try the same alternative tests with the log version of gdp

# PP test
#pp.test.log <- ur.pp(log(itec$gdp),
#                 type = c("Z-tau"),
#                 model = c("constant"))
#summary(pp.test.log)
# Ok, value higher to the critical one

#pp.test.d.log <- ur.pp(diff(log(itec$gdp)),
#                 type = c("Z-tau"),
#                 model = c("constant"))
#summary(pp.test.d.log)

# KPSS test
#kpss.test.log <- ur.kpss(itec$gdp, type = c("mu"))
#summary(kpss.test.log)

#kpss.test.d.log <- ur.kpss(diff(itec$gdp), type = c("mu"))
#summary(kpss.test.d.log)

##### ]

## --> Performing all the tests, the better model is the normal one


### Since we don't want any non-stationary variable to be part of our model, 
### we will perform these test for all the other variables, starting with:

## VA TOT

# We will keep the DF test type "c" because is the one we think suits the most
# our data, and we will perform only the DF test for semplicity

plot(itec$va_tot, main = "VA TOT")

testdf2(variable = itec$va_tot,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 2 --> non-stationary

testdf2(variable = diff(itec$va_tot),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 1 --> stationary


## NET INDIR TAX

plot(itec$net_indir_tax, main = "NET INDIR TAX")

testdf2(variable = itec$net_indir_tax,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 1 --> non-stationary

testdf2(variable = diff(itec$net_indir_tax),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 1 --> stationary


## IMP

plot(itec$imp, main = "IMP")

testdf2(variable = itec$imp,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 4 --> non-stationary

testdf2(variable = diff(itec$imp),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 1 --> stationary


## TOT SUPPLY

plot(itec$tot_supply, main = "TOT SUPPLY")

testdf2(variable = itec$tot_supply,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 1 --> non-stationary

testdf2(variable = diff(itec$tot_supply),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 1 --> stationary


## EXP

plot(itec$exp, main = "EXP")

testdf2(variable = itec$exp,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 1 --> non-stationary

testdf2(variable = diff(itec$exp),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 1 --> stationary


## TOTAL CONS

plot(itec$total_cons, main = "TOTAL CONS")

testdf2(variable = itec$total_cons,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 2 --> non-stationary

testdf2(variable = diff(itec$total_cons),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# [!] H0 not rejected at row 1 --> non-stationary, bad

# We need to apply the second differences
testdf2(variable = diff(diff(itec$total_cons)),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# Okay, H0 rejected at row 1 --> stationary


## TOT FIXED INV

plot(itec$tot_fixed_inv, main = "TOT FIXED INV")

testdf2(variable = itec$tot_fixed_inv,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 2 --> non-stationary

testdf2(variable = diff(itec$tot_fixed_inv),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 1 --> stationary


## INVENT VARIATION

plot(itec$invent_variation, main = "INVENT VARIATION")

testdf2(variable = itec$invent_variation,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 4 --> non-stationary

testdf2(variable = diff(itec$invent_variation),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 3 --> stationary


## TOT INVEST

plot(itec$tot_invest, main = "TOT INVEST")

testdf2(variable = itec$tot_invest,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 1 --> non-stationary

testdf2(variable = diff(itec$tot_invest),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 1 --> stationary


## TOT USERS

plot(itec$tot_users, main = "TOT USERS")

testdf2(variable = itec$tot_users,
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 not rejected at row 1 --> non-stationary

testdf2(variable = diff(itec$tot_users),
        test.type = "c",
        max.augmentations = 3,
        max.order = 5)
# H0 rejected at row 1 --> stationary


## ---------- ##
## ARDL Model ##
## ---------- ##

## Now let's try to build the ARDL model

# We will begin directly with ARDL because of economic theory (...)

# As a reference
#dl_1 <- dynlm(d(consumption) ~ d(dpi) + L(d(dpi)), data=USA, start=c(1960, 1))
#summary(dl_1)

#model1.1 <- dynlm(d(gdp) ~ L(d(gdp),c(1:3)) + d(va_tot) + d(net_indir_tax) +
#                    d(imp) + d(tot_supply) + d(exp) + d(d(total_cons)) +
#                    d(tot_fixed_inv) + L(d(invent_variation),2) +
#                    d(tot_invest) + d(tot_users),
#                data=itec,
#                na.action = "na.exclude")
#summary(model1.1)
# Error --> searching the internet is due to NAs in the data (not our case) or
# "bad transformations" in the data

# Check NAs in data
#all(is.na(diff(itec)))  # FALSE
#na.omit(itec$tot_supply)

# Let's try to remove the differences
#model1.2 <- dynlm(gdp ~ gdp + va_tot + net_indir_tax + imp +
#                     tot_supply + exp + total_cons + tot_fixed_inv +
#                     invent_variation + tot_invest + tot_users,
#               data=itec)
#summary(model1.2)

# the problem is not in the differences because it gives the same kind of error
# even doing a normal lm (just to try)
# Probably it is directly related to dynlm function: when executed, it looks
# like that it creates some subsets of our data with all NAs

class(itec$tot_users)

# We managed to handle this problem transforming our data into a zoo object
itec <- as.zoo(itec)
class(itec)
# Okay

model1.zoo <- dynlm(d(d(gdp)) ~ L(d(d(gdp)),c(1:2)) + d(va_tot) +
                                d(net_indir_tax) + d(imp) + d(tot_supply) +
                                d(exp) + d(d(total_cons)) + d(tot_fixed_inv) +
                                L(d(invent_variation),2) + d(tot_invest) +
                                d(tot_users),
                    data = itec,
                    na.action = "na.exclude")
# We considered the second differences because, as we said in the plot of
# diff(gdp), the plot doesn't appear stationary. Furthermore, in this case,
# the results appear to be more reliable
# Also, we chose the second augmentation of diff(invent_variation) because
# it is the first one that doesn't have autocorrelation in residuals, so
# p_adf is reliable only at lag 2
# Trying with several lags on gdp, the best fit appears to be with 2 lags
summary(model1.zoo)

#model.nd.zoo <- dynlm(d(gdp) ~ L(d(gdp),c(1:3)) + va_tot + net_indir_tax +
#                        imp + tot_supply + exp +
#                        total_cons + tot_fixed_inv +
#                        invent_variation + tot_invest +
#                        tot_users,
#                    data = itec, 
#                    na.action = "na.exclude")
#summary(model.nd.zoo)
# It works but it should be a case of spurious regression because we introduced
# non-stationary variables into the model

# We will now remove the non-significant variables from our model, i.e.
# d(tot_supply) and L(d(invent_variation),2)
model2.zoo <- dynlm(d(d(gdp)) ~ L(d(d(gdp)),c(1:2)) + d(va_tot) +
                        d(net_indir_tax) + d(imp) + d(exp) + d(d(total_cons)) +
                        d(tot_fixed_inv) + d(tot_invest) + d(tot_users),
                    data = itec, 
                    na.action = "na.exclude")
summary(model2.zoo)
# Okay

# Let's compare the two model with the "anova" function
anova(model1.zoo,model2.zoo)
# H0: all the variables we removed are jointly insignificant
# We cannot reject the null hypothesis (p-value = 0.7756 > 0.05) --> model2.zoo
# is better than model1.zoo


## ----------- ##
## Diagnostics ##
## ----------- ##

## Ramsey's RESET Test
# Not sure if it makes sense to do this because it tests the linearity of the
# constructed model (so it must be OLS?)
#resettest(model2.zoo, power=2:3, type="fitted")

# Let's try to modify something
#gdp_l1 <- Hmisc::Lag(itec$gdp, 1)
#gdp_l2 <- Hmisc::Lag(itec$gdp, 2)

#model2.1.zoo <- lm(diff(diff(gdp)) ~ diff(gdp_l1) + diff(gdp_l2) + diff(va_tot) +
#                        diff(net_indir_tax) + diff(imp) +
#                        diff(exp) + diff(diff(total_cons)) + diff(tot_fixed_inv) +
#                        diff(tot_invest) +
#                        diff(tot_users),
#                    data = itec, 
#                    na.action = "na.exclude")
# Error
#resettest(model2.1.zoo, power=2:3, type="fitted")

## Breusch-Pagan Test
# (Brief description)
bptest(model2.zoo, studentize=TRUE)
# H0 not rejected --> Homoskedasticity of residuals

## Breusch-Godfrey Test
# Brief description
bgtest(residuals(model2.zoo)~1, order = 1)
bgtest(residuals(model2.zoo)~1, order = 2)
bgtest(residuals(model2.zoo)~1, order = 3)
bgtest(residuals(model2.zoo)~1, order = 4)
bgtest(residuals(model2.zoo)~1, order = 5)
# H0 not rejected --> No autocorrelation in residuals

## Jarque-Bera Test
# Brief description
jarque.bera.test(model2.zoo$residuals)
# H0 not rejected --> residuals are normally distributed


## ------------ ##
## ARIMA Models ##
## ------------ ##

# In order to successfully generate an ARIMA model, we have to retransform the
# data into xts object
itec <- as.xts(itec)
class(itec)
# Okay

# Let's see the lags of gdp in ACF and PACF
par(mfrow=c(1,2))
acf(diff(itec$gdp),
    lag.max = 36,
    lwd = 5,
    col = "dark green",
    na.action = na.pass,
    main="dGDP",
    xlim=c(0,25))
# Gradually decay towards 0
pacf(diff(itec$gdp), 
     lag.max = 36, 
     lwd = 5,
     col = "dark green",
     na.action = na.pass,
     main="dGDP",
     xlim=c(0,25))
# Abrupt dropoff after the first lag
par(mfrow=c(1,1))
# --> These two plots indicate that we are likely dealing with an AR(1) process

# Let's calculate the first ARIMA model based on the lags we have seen in ACF
# and PACF functions
arima1 <- Arima(itec$gdp,
                  order = c(1,1,0))
summary(arima1)
# The summary doesn't show the the levels of significance

# Check for significance of the lags with coeftest(...)
coeftest(arima1)

# Check residuals with Ljung-Box test
Box.test(resid(arima1), type = "Ljung-Box", lag = 10)
# H0 not rejected --> no autocorrelation in residuals up to the selected lag

# All good

# Let's calculate the same model but now we will consider the drift
arima1.1 <- Arima(itec$gdp,
                  order = c(1,1,0),
                  fixed = c(NA,
                            NA),
                  include.constant = TRUE)
summary(arima1.1)
coeftest(arima1.1)
Box.test(resid(arima1.1), type = "Ljung-Box", lag = 10)

# Let's try to use the auto.arima function to see which are the values of p
# and q that it will finds
arima.best.AIC <- auto.arima(itec$gdp,
                             d = 1,     # parameter d of ARIMA model
                             max.p = 7,    # Maximum value of p
                             max.q = 7,     # Maximum value of q
                             max.order = 8,   # maximum p+q
                             start.p = 1,    # Starting value of p in stepwise procedure
                             start.q = 1,    # Starting value of q in stepwise procedure
                             ic = "aic",     # Information criterion to be used in model selection.
                             stepwise = FALSE,  # if FALSE considers all models
                             allowdrift = TRUE, # include a constant
                             trace = TRUE)
# Here the best model is an ARIMA(4,1,2) --> strange but we know that the AIC
# criterion gives us more advanced models (so generally with more lags)
# compare with BIC criterion, which is more tolerant but shows less tolerance 
# at higher numbers. Moreover, since we have yearly data, the sufficient number
# of lags with this frequency is usually 3 or 4, and this is in line with the
# output found
summary(arima.best.AIC)
coeftest(arima.best.AIC)
# There are several non-significant lags
Box.test(resid(arima.best.AIC), type = "Ljung-Box", lag = 10)

# Let's now remove the insignificant lags and perform the same tests
arima.best.AIC.cut <- Arima(itec$gdp,
                    order = c(4,1,2),
                    fixed = c(NA,0,0,NA,
                              0,0,
                              0),
                    include.constant = TRUE)
summary(arima.best.AIC.cut)
coeftest(arima.best.AIC.cut)
Box.test(resid(arima.best.AIC.cut), type = "Ljung-Box", lag = 10)

# Let's now compare these models by calculating the information criteria AIC
# and BIC
#AIC
AIC(arima1)
AIC(arima1.1)
AIC(arima.best.AIC)
AIC(arima.best.AIC.cut)
# BIC
BIC(arima1)
BIC(arima1.1)
BIC(arima.best.AIC)
BIC(arima.best.AIC.cut)

# The auto.arima is the best for AIC but the worst for the BIC criterion.
# However, the same one with the removed lags is the best among all the ARIMA
# model we tried

### ARDL vs ARIMA
# Now we will compare this last model with the previous ARDL model by using
# the same criteria
# AIC
AIC(arima.best.AIC.cut)
AIC(model2.zoo)
# BIC
BIC(arima.best.AIC.cut)
BIC(model2.zoo)
# As we thought, since the ARIMA model doesn't make sense according to the
# theory, it is better to consider the ARDL model because it shows a smaller
# value of both AIC and BIC


## ------ ##
## FINISH ##
## ------ ##
