################################################################################
################################### Titles #####################################
################################################################################

# Title: Homework 2
# Class: Survival and Longitudinal Data Analysis
# Professor: Schulz, Juliana
# University: HEC Montréal
# Team members: LastName, FirstName, ID
# Team members: Beauchamp, Gabriel, 11254208
# Submission Date: Thursday, April 11th 2024

################################################################################
############################ Prepare environment ###############################
################################################################################

#### Set locale to English language ####
Sys.setlocale("LC_TIME", "C")

#### Set work environment ####
work_env_path = "/Users/Gabriel/Desktop/Survival & Longitudinal/Homework2/"
data_filename = "data_wages.csv"

setwd(work_env_path)
data_raw = read.csv(data_filename, header=TRUE, stringsAsFactors=FALSE)

################################################################################
########################### Set up data properly ###############################
################################################################################

#### Look at data ####
head(data_raw)
# Reminder: the variable wage is on the log-scale.

str(data_raw)
# Most variables are coded as integers instead of categorical (factors).

summary(data_raw)
# The last 3 variables will be coded as categorical variables

table(data_raw$union)
# Since we want to implement union as a categorical variable with reference level
# union = 0 as per the instructions.

table(data_raw$married)
# Since we want to implement married as a categorical variable with reference level
# married = 0 as per the instructions.

table(data_raw$manufacturing)
# Since we want to implement manufacturing as a categorical variable with reference level
# manufacturing = 0 as per the instructions.

#### Modify data ####
df = data_raw

#### Modify data types ####
df$union          = as.factor(df$union)           #explanatory variable
df$married        = as.factor(df$married)         #explanatory variable
df$manufacturing  = as.factor(df$manufacturing)   #explanatory variable

#### Confirm modifications ####
str(df)
# The variables union, married and manufacturing already have an appropriate reference level.


################################################################################
############################## Question 1 ######################################
################################################################################

#### Question #1 a) ####
for (year in unique(df$yr)) {
  print(length(unique(df[df$yr==year,'id'])))
}
# We have 545 unique observations based on the id variable at every year of the survey.

#### Question #1 b) ####

# TODO à améliorer

summary(df[df$yr==0,4:8])
summary(df[df$yr==1,4:8])
summary(df[df$yr==2,4:8])
summary(df[df$yr==3,4:8])
summary(df[df$yr==4,4:8])
summary(df[df$yr==5,4:8])
summary(df[df$yr==6,4:8])
summary(df[df$yr==7,4:8])

# The variable exper appears to move in a perfectly linear fashion through time. This might be problematic.
# The variable school does not move over time. The stats remain the same during all
# those years, suggesting that nobody has completed additional schooling during the study.
# The variable union changes over time, but not much it is steady.
# The variable married shows that the number of married people increases over time.
# The variable manufacturing shows that the number of people working in manufacturing
# increases slightly over time.

#### Question #1 c) ####
first10individuals = head(unique(df[,'id']),10)

library(ggplot2)

ggplot(data = df[df$id %in% first10individuals,],
       aes(x = yr, y = wage, group = id, color = id)) + 
  geom_point() + 
  geom_line()


#### Question #1 d) ####

# TODO à améliorer? Ce n'est pas trop parlant
ggplot(data = df[df$id %in% first10individuals,],
       aes(x = yr, y = wage, group = id, color = union)) + 
  geom_point() + 
  geom_line() +
  facet_grid(. ~ manufacturing)


################################################################################
############################## Question 2 ######################################
################################################################################

# Consider the following model
formula1 = wage ~ exper + school + union + married + manufacturing + exper*union + exper*married + yr
linear_model <- gls(formula1,data = df)

#### Question #2 a) ####
library(nlme)
ecs_model <- gls(formula1,
              correlation= corCompSymm(form=~1|id),
              data = df)

summary(ecs_model) # rho_hat is 0.4616466
ecs_model$sigma.   # sigma_hat is 0.4796523, so sigma_squared is 0.2300663
marg_ecs = getVarCov(ecs_model, individual = 1, type = "marginal") # cov_hat is 0.10621

ecs_model$coefficients

# The estimate of the variance here is 0.2300663 while the estimate for the covariance
# is 0.10621 In a exchangeable correlation structure, for a group, the values for
# each correlation estimate within a group are the same, we have 0.10621/0.23007=0.46164

# We assume independence between observations from different groups, which gives
# an estimate of 0 for the second correlation.

#### Question #2 b) ####

# Since the interval is regular, there is no need to precise it in the model.
ar1_model <- gls(formula1, correlation= corAR1(form=~1|id), data = df)

summary(ar1_model) # rho_hat is 0.5722048
ar1_model$sigma.   # sigma_hat is 0.4799857, so sigma_squared is 0.2303863

ar1_model$coefficients

# Since the distance between the two observations is of 2 years, we will be looking
# at the variance multiplied by rho_hat-squared, which 0.0754327 in our case.
# We assume independence between observations from different groups, which gives
# an estimate of 0 for the second covariance.

#### Question #2 c) ####

arh1_model <- gls(formula1, correlation= corAR1(form=~1|id),
                 weights = varIdent(form = ~ 1 | yr), data = df)

summary(arh1_model) # rho_hat is 0.5811615
arh1_model$sigma    # sigma_hat is 0.5671192

(arh1_model$sigma*c(1.0000000, 0.9440093, 0.8080079, 0.757875,
                    0.8458105, 0.8406528, 0.8319493, 0.7469942))^2

arh1_model$coefficients

# Our first estimate is thus sqrt(sigma_hat_j^2*sigma_hat_k^2)*rho_hat^abs(j-k).
# It gives sqrt(0.3216242*0.2866166)*0.5811615^1 = 0.1764501.

# Our second estimate is also sqrt(sigma_hat_j^2*sigma_hat_k^2)*rho_hat^abs(j-k).
# It gives sqrt(0.3216242*0.1847328)*0.5811615^3 = 0.04784503

#### Question #2 d) ####

# This is a standard test, which means we do not have to adapt the anova for the LR.
# The null hypothesis: rho = 0
# The alternate hypothesis: rho is not = 0
# The test statistic is D, which is given by D = {−2LL(reduced)} − {−2LL(complete)}.
# The degree of freedom is 1 on a chi-squared distribution.

anova(linear_model, arh1_model,type="LR")

# The value of the test statistic D is {2989.540} − {2212.942} = 776.598.
# The matching p-value is <.0001, which is significant at the alpha = 5% level.
# We thus reject the null hypothesis and conclude that the linear model with an 
# independent correlation structure is not an adequate simplification of the model
# with an ARH(1) correlation structure on the random errors.

#### Question #2 e) ####

data.frame(AIC(linear_model), AIC(ecs_model), AIC(ar1_model), AIC(arh1_model))

# All fixed effects are the same in all four of those models, which makes them comparable.
# Our criterion is to choose the model with the lowest AIC, in our case the model
# with an ARH(1) correlation structure on the random errors.

#### Question #2 f) ####

coefficients(arh1_model)[1]

# The value of the intercept is the base value of an individual's wage when all 
# covariates have a value of 0. In our case, an individual with 0 years of experience,
# 0 years of schooling, who is not part of an union, who is not married and does
# not work in manufacturing, has a average wage of 0.1361474. Remember that the
# wage variable has been transformed on the log-scale and that the expected value
# of its transformation is not the same as the transformation of its expected value.

#### Question #2 g) ####

coefficients(arh1_model)[4]

# The variable union is categorical/binary and can take either value 0 or 1.
# The variable union is also part of an interaction with exper.
# According to the fitted model, when an individual is unionized, meaning that the
# value of union is 1, their wage increases by 0.1571003 - 0.01127027*exper,
# keeping all other variables the same.

#### Question #2 h) ####

# Question (i)

# TODO

# Sub question (i)(a)

# The null hypothesis: beta_hat_exper_union1 = 0
# The alternate hypothesis: beta_hat_exper_union1 is not = 0
# The test statistic is is given by t from a T-test.
# The degree of freedom is 1 on a chi-squared distribution.

summary(arh1_model)

# The value of the test statistic t is -1.853507.
# The matching p-value is 0.0639, which is not significant at the alpha = 1% level.
# We do not reject the null hypothesis and conclude that the effect of exper does
# not depend on whether or not an individual is part of a union.

# Sub question (i)(b)

# The null hypothesis: beta_hat_exper_married1 = 0
# The alternate hypothesis: beta_hat_exper_married1 is not = 0
# The test statistic is is given by t from a T-test.
# The degree of freedom is 1 on a chi-squared distribution.

summary(arh1_model)

# The value of the test statistic t is -1.942803.
# The matching p-value is 0.0521, which is not significant at the alpha = 1% level.
# We do not reject the null hypothesis and conclude that the effect of exper does
# not depend on whether or not an individual is married.


#### Question #2 i) ####

# The null hypothesis: beta_hat_school = 0
# The alternate hypothesis: beta_hat_school is not = 0
# The test statistic is D, which is given by t from a T-test.
# The degree of freedom is 1 on a chi-squared distribution.

summary(arh1_model)

# The value of the test statistic t is 11.076734.
# The matching p-value is 0.0000, which is significant at the alpha = 1% level.
# We thus reject the null hypothesis and conclude that the variable school has an 
# impact on on an individual's hourly wage.

#### Question #2 j) ####

# TODO

#### Question #2 k) ####

cbind(sqrt(diag(vcov(linear_model))),
      sqrt(diag(vcovCR(linear_model, form = "sandwich",type="CR0",cluster = df$id))))

# The standard errors of the regression coefficients in the independent model
# are smaller than those obtained using the sandwich estimation approach. Some
# variables, such as school, have a standard error almost twice the size in the sandwich 
# estimation compared to model based. This is indicative that we should look at the
# p-values to see whether the variables are significant in both methods.


#### Question #2 l) ####

cbind(sqrt(diag(vcov(arh1_model))),
      sqrt(diag(vcovCR(arh1_model, form = "sandwich",type="CR0"))))

# The standard errors of the regression coefficients in the independent model
# are similar to those obtained using the sandwich estimation approach. Some
# variables, such as married, have a standard error almost identical in both 
# methods. If we were to look at the p-values for significance, we would feel
# confident, although not certain, to find that covariates with similar significances.

################################################################################
############################## Question 3 ######################################
################################################################################

#### Question #3 a) ####

random_intercept_model<-lme(formula1, random = ~ 1  | id, data = df)

random_intercept_model$coefficients$fixed

cond_rand_intercept <- getVarCov(random_intercept_model, individual = 1, type = "conditional")
marg_rand_intercept1 <- getVarCov(random_intercept_model, individual = 1, type = "marginal")

cond_rand_intercept
marg_rand_intercept1

isTRUE(all.equal(coef(ecs_model), fixef(random_intercept_model)))
# The fixed effects coefficients are the same.

marg_ecs
marg_rand_intercept1
# The marginal covariance matrices are the same.

# In a marginal context
# TODO

#### Question #3 b) ####

ctrl <- lmeControl(opt='optim') # I had convergence errors previously.
random_effects_model <- lme(formula1, random = ~ 1 + school| id, control=ctrl, data = df)

#### Question #3 c) ####

# TODO

#### Question #3 d) ####

# TODO

#### Question #3 e) ####

# TODO

#### Question #3 f) ####

# TODO

#### Question #3 g) ####

# TODO

#### Question #3 h) ####

# TODO

#### Question #3 i) ####

# TODO

#### Question #3 j) ####

# TODO

#### Question #3 k) ####

# TODO

################################################################################
############################## Question 4 ######################################
################################################################################

#### Question #4 a) ####

# TODO

#### Question #4 b) ####

# TODO

#### Question #4 c) ####

# TODO

