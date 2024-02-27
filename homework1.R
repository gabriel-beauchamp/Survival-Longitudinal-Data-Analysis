################################################################################
################################### Titles #####################################
################################################################################

# Title: Homework 1
# Class: Survival and Longitudinal Data Analysis
# Professor: Schulz, Juliana
# University: HEC Montréal
# Team members: LastName, FirstName, ID
# Team members: Beauchamp, Gabriel, 11254208
# Submission Date: Tuesday, February 27th 2024

################################################################################
############################ Prepare environment ###############################
################################################################################

#### Set locale to English language ####
Sys.setlocale("LC_TIME", "C")

#### Set work environment ####
work_env_path = "/Users/Gabriel/Desktop/Survival & Longitudinal/Homework/"
data_filename = "data_disability.csv"

setwd(work_env_path) 
data_raw = read.csv(data_filename, header=TRUE, stringsAsFactors=FALSE)

################################################################################
########################### Set up data properly ###############################
################################################################################

#### Look at data ####
head(data_raw)
# The variable age is continuous, but does not need to be.

str(data_raw)
# Most variables are coded as integers instead of categorical (factors).

summary(data_raw)
# We might want to hardcode the reference level for the variables reason, collective, policy.
# Depending on definition and size, it makes sense to use the largest group as reference level.

table(data_raw$month)
# Since we want to implement season as a categorical variable, we will have to chose 
# an appropriate reference level. In our case, it will be 1 since it has the most observations.

table(data_raw$season)
# Since we want to implement season as a categorical variable, we will have to chose 
# an appropriate reference level. In our case, it will be 4 since it has the most observations.

#### Modify data ####
df = data_raw

#### Modify data types ####
df$sex        = as.factor(df$sex)         #explanatory variable
df$reason     = as.factor(df$reason)      #explanatory variable
df$policy     = as.factor(df$policy)      #explanatory variable
df$collective = as.factor(df$collective)  #explanatory variable
df$month      = as.factor(df$month)       #explanatory variable
df$season     = as.factor(df$season)      #explanatory variable

#### Confirm modifications ####
str(df)

#### Choose the reference level for different variables ####
# sex and month already have an appropriate reference level
df = within(df, reason <- relevel(reason, ref = '1'))
df = within(df, collective <- relevel(collective, ref = '1'))
df = within(df, policy <- relevel(policy, ref = '1'))
df = within(df, season <- relevel(season, ref = '4'))

#### Confirm reference level changes ####
str(df)

################################################################################
############################## Question 1 ######################################
################################################################################

#### Question #1 a) ####

# Import library with survival analysis functions
library(survival)

output <- survfit(Surv(df$time, df$status)~1,conf.type="none")
# Keeping the numeric type for status keeps the KP estimator correct

# Output from the function; provides certain descriptive statistics
output

# To view the KM estimator for all possible time points
summary(output)

# Graph of KM estimator
plot(output)

# The curve goes down drastically until 50 days and then stabilizes until infinity (or 250 in our case).
# A possible explanation for this particular behavior could be that most people
# recover after 1.5-2 months from their injuries and cases that exceed that threshold
# can go on for a very long time.

#### Question #1 b) ####

print(output, print.rmean=TRUE)
# Restricted Mean: 42.6 days
# Median: 14 days

#### Question #1 c) ####

# Import library with actuarial method
library(KMsurv)

# Create a sensible vector of endpoints
interval_size = 10
vector1 = seq(0, max(df$time), by=interval_size)

# Preparing data for the lifetab function
interval_vector = vector1
initial_subjects = nrow(df)
censored_subjects = vector()
observed = vector()

# Filling the censored subjects vector
cen_i = 0
for (end_point in interval_vector[2:length(interval_vector)]) {
  
  for (time in df[df$status == 0, ]$time) {
    if (time <= end_point && time > end_point-interval_size) {
      cen_i = cen_i+1
    }
  }
  print(end_point)
  print(cen_i)
  censored_subjects = append(censored_subjects, cen_i)
  cen_i=0

}

# Filling the observed subjects vector
obs_i = 0
for (end_point in interval_vector[2:length(interval_vector)]) {
  
  for (time in df[df$status == 1, ]$time) {
    if (time <= end_point && time > end_point-interval_size) {
      obs_i = obs_i+1
    }
  }
  print(end_point)
  print(obs_i)
  observed = append(observed, obs_i)
  obs_i=0
  
}

# Presenting the results of the actuarial method.
lifetab(tis = interval_vector, ninit = initial_subjects,
        nlost = censored_subjects, nevent = observed)

# TODO create interval and plot and comment

#### Question #1 d) ####
library(survminer)
output2 <- survfit(Surv(time,status) ~ policy, data = df, conf.type="log-log")

# visual:
ggsurvplot(
  output2,
  fun = "pct",
  linetype = "strata",
  pval = FALSE, 
  pval.method = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  fontsize = 3, # used in risk table
  surv.median.line = "hv", # median horizontal and vertical ref lines
  ggtheme = theme_light(),
  title = "Kaplan-Meier Survival Function Estimate"
)

# Obviously, there is a large difference in the amount of observations in each category.
# We would expect the curve with policy=0 to have wider confidence intervals and
# it is the case. We can also see that the curve with policy=0 is higher than the
# one with policy=1 and that the confidence intervals do not overlap for some time
# between the 12th and 50th day. The curves themselves do not cross after the 10th
# day (approximately).

#### Question #1 e) ####

print(output2,print.rmean=TRUE)
# Median for policy=1: 13
# Median for policy=0: 26

# The median number of days on disability of the specific policy  is twice the size of the standard policy.
# The results are not surprising, considering that the curve is higher for the
# specific policy, that the intervals of both curves do not intersect for the period
# between 12 and 50 days (aprox.). Visually, the difference appears clear, but
# we would have to conduct a formal test to make a proper statement about that.

#### Question #1 f) ####

# For this question, we will use the log-rank statistical test to compare whether
# there is a significant difference in the length of disability based on the type of policy.

# Here is the log-rank formal test.
survdiff(Surv(df$time, df$status)~df$policy,rho=0)

# The null hypothesis: H0 :S1(t)=S2(t) where S1(t) is the survival function for policy=1
# and S2(t) is the survival function for policy=0.

# The alternate hypothesis: H1: S1(t)=/=S2(t) for at least one time t

# The value of the test statistic is the Chi-Square at 18.6 on 1 degree of freedom.
# The corresponding p-value for this test statistic is 2e-05.

# The conclusion of this test: We reject the null hypothesis H0 as the p-value of
# the corresponding test statistic is lower than 0.05 (at 2e-05), the significance
# level we deem appropriate. There is enough evidence to support that individuals
# having different policies can make survival functions differ for individuals.

#### Question #1 g) ####

output2 <- survfit(Surv(time,status) ~ season, data = df, conf.type="log-log")

# visual:
ggsurvplot(
  output2,
  fun = "pct",
  linetype = "strata",
  pval = FALSE, 
  pval.method = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  fontsize = 3, # used in risk table
  surv.median.line = "hv", # median horizontal and vertical ref lines
  ggtheme = theme_light(),
  title = "Kaplan-Meier Survival Function Estimate"
)

# In this graph, it appears like there are two groups of similar-looking curves.
# Group 1 has season=4 and season=3, while Group 2 has season=1 and season=2.
# Group 1 has curves higher than Group 2, which intuitively tells us that people
# tend to stay longer on disability, on average when the lesion occurs in fall or summer.
# However, we would have to investigate further to make any formal conclusion.

print(output2,print.rmean=TRUE)
# Median for season=4: 17
# Median for season=1: 12
# Median for season=2: 13
# Median for season=3: 18

# The medians for the number of days on disability of the season we grouped into
# Group 1 are indeed similar and higher than those in Group 2. The difference
# is however smaller than what we would expect because our eyes were captivate by
# the wider margins that appear later in the curves.

# We will use once again the log-rank statistical test to compare whether
# there is a significant difference in the length of disability based on the season.

# Here is the log-rank formal test.
survdiff(Surv(df$time, df$status)~df$season,rho=0)

# The null hypothesis: H0 :S1(t)=S2(t)=S3(t)=S4(t) where S1(t) is the survival
# function for season=1, S2(t) is the survival function for season=2,
# S3(t) is the survival function for season=3 and S3(t) is the survival function for season=4.

# The alternate hypothesis: H1: at least two of the survival functions differ for at least one t

# The value of the test statistic is the Chi-Square at 19.8 on 3 degrees of freedom.
# The corresponding p-value for this test statistic is 2e-04.

# The conclusion of this test: We reject the null hypothesis H0 as the p-value of
# the corresponding test statistic is lower than 0.05 (at 2e-04), the significance
# level we deem appropriate. There is enough evidence to support individuals
# having the event that caused the disability in different seasons can make
# survival functions differ for individuals.

################################################################################
############################## Question 2 ######################################
################################################################################

#### Question #2 a) ####

# AFT Exponential model
aft.exponential<-survreg(Surv(time, status)~ age + reason + policy + season + reason*season,
                     dist="exponential",data=df)

# AFT Weibull model
aft.weibull<-survreg(Surv(time, status)~ age + reason + policy + season + reason*season,
                     dist="weibull",data=df)

# AFT Log-Normal model
aft.lognormal<-survreg(Surv(time, status)~ age + reason + policy + season + reason*season,
                     dist="lognormal",data=df)

# AFT Log-Logistic model
aft.loglogistic<-survreg(Surv(time, status)~ age + reason + policy + season + reason*season,
                         dist="loglogistic",data=df)

# AFT Generalized Gamma model
library(flexsurv)

aft.gengamma<-flexsurvreg(Surv(time, status)~ age + reason + policy + season + reason*season,
                     dist="gengamma",data=df)

# Testing the different nested models
library(lmtest)

# We will compare the nested models (Exponential, Weibull and Log-Normal are all 
# nested models of the Generalized Gamma). The idea here is that the null hypothesis
# considers that that the “complete” and “reduced” models are not significantly
# different in terms of model adequacy while the alternate hypothesis is that the
# “reduced” model is NOT an adequate simplification of the “complete” model. 
# The specific hypotheses differ slightly between different nested models and they 
# will be presented below.

lrtest(aft.gengamma, aft.exponential)
# The null hypothesis: H0 : σ=δ=1

# The alternate hypothesis: H1: at least one σ=/=1 or δ=/=1

# The value of the test statistic D is 286.12 on 2 degrees of freedom.
# The corresponding p-value for this test statistic is 2.2e-16.

# The conclusion of this test: We reject the null hypothesis H0 as the p-value of
# the corresponding test statistic is lower than 0.05 (at 2.2e-16), the significance
# level we deem appropriate. There is enough evidence to support that the Exponential 
# model is NOT an adequate simplification of the Generalized Gamma model

lrtest(aft.gengamma, aft.weibull)
# The null hypothesis: H0 : δ=1

# The alternate hypothesis: H1: δ=/=1

# The value of the test statistic D is 213.3 on 1 degree of freedom.
# The corresponding p-value for this test statistic is 2.2e-16.

# The conclusion of this test: We reject the null hypothesis H0 as the p-value of
# the corresponding test statistic is lower than 0.05 (at 2.2e-16), the significance
# level we deem appropriate. There is enough evidence to support that the Weibull 
# model is NOT an adequate simplification of the Generalized Gamma model

lrtest(aft.gengamma, aft.lognormal)
# The null hypothesis: H0 : δ=0

# The alternate hypothesis: H1: δ=/=0

# The value of the test statistic D is 27.631 on 1 degree of freedom.
# The corresponding p-value for this test statistic is 1.468e-07.

# The conclusion of this test: We reject the null hypothesis H0 as the p-value of
# the corresponding test statistic is lower than 0.05 (at 1.468e-07), the significance
# level we deem appropriate. There is enough evidence to support that the Log-Normal 
# model is NOT an adequate simplification of the Generalized Gamma model

# Use AIC and BIC to compare all models
AIC_BIC_Table = data.frame(matrix(nrow = 5, ncol = 3))
colnames(AIC_BIC_Table)=c('Distribution', 'AIC', 'BIC')

AIC_BIC_Table[1,]=c('Exponential',AIC(aft.exponential),BIC(aft.exponential))
AIC_BIC_Table[2,]=c('Weibull',AIC(aft.weibull),BIC(aft.weibull))
AIC_BIC_Table[3,]=c('Log-Normal',AIC(aft.lognormal),BIC(aft.lognormal))
AIC_BIC_Table[4,]=c('Log-Logistic',AIC(aft.loglogistic),BIC(aft.loglogistic))
AIC_BIC_Table[5,]=c('Generalized Gamma',AIC(aft.gengamma),BIC(aft.gengamma))
AIC_BIC_Table

# The AFT model with the Generalized Gamma distribution has both a lower AIC and
# a lower BIC than the AFT model with the Log-Logistic distribution. It is also
# the case for all other distributions in comparison to the Generalized Gamma.
# Even though it is not a formal test, these results would indicate that the
# Generalized Gamma AFT model is the best one to select for further endeavors.

# As a result of both the Likelihood Ratio tests and the analysis of the AIC and BIC,
# we would determine that the Generalized Gamma distribution is most appropriate
# distribution for modelling the length of disability for this data.

#### Question #2 b) ####

cs = coxsnell_flexsurvreg(aft.gengamma)
# We produce here the Cox-Snell residuals for all observations based on the fitted
# AFT model using the Generalized Gamma distribution.

surv <- survfit(Surv(cs$est, df$status) ~ 1)
# We will compare the residuals as they should form a censored sample from an Exponential(1).

plot(surv, fun="cumhaz")
abline(0, 1, col="red")

# The non-parametric estimate of the cumulative hazard follows the expected trajectory.
# The variance seems to increase in the end, but it is to be expected considering
# the number of observations remaining.We would deem the model chosen to be adequate
# enough to represent the disability data we have.

#### Question #2 c) ####

summary(aft.lognormal)
# It is to be noted that the p-value for the following covariates have a value 
# greater than 0.05: reason0, season3, reason0:season2 and reason0:season3.
# This would mean that those covariates do not have a significant effect on the 
# value of the predicted variable in this model. However, it does not mean that
# we cannot interpret their  estimated effects.

# Season4 (Fall)
exp(aft.lognormal$coefficients['reason0'])
# The estimated effect of the variable reason for season4 (which is in fact the 
# reference level) is exp(β_reason0)=exp(0.0883)=1.092316.
# On average, the duration of disability for people that have a disability due to
# a work accident is 1.09 times that of people that have a disability due to illness,
# in season4, holding all other variables constant.

# Season1 (Winter)
exp(aft.lognormal$coefficients['reason0'] + aft.lognormal$coefficients['reason0:season1'])
# The estimated effect of the variable reason for season1 is 
# exp(β_reason0+β_reason0:season1)=exp(0.0883+0.9805)=2.911813.
# On average, the duration of disability for people that have a disability due to
# a work accident is 2.91 times that of people that have a disability due to illness,
# in season1, holding all other variables constant.

# Season2 (Spring)
exp(aft.lognormal$coefficients['reason0'] + aft.lognormal$coefficients['reason0:season2'])
# The estimated effect of the variable reason for season2 is 
# exp(β_reason0+β_reason0:season2)=exp(0.0883+0.2041)=1.33959.
# On average, the duration of disability for people that have a disability due to
# a work accident is 1.34 times that of people that have a disability due to illness,
# in season3, holding all other variables constant.

# Season3 (Summer)
exp(aft.lognormal$coefficients['reason0'] + aft.lognormal$coefficients['reason0:season3'])
# The estimated effect of the variable reason for season1 is 
# exp(β_reason0+β_reason0:season1)=exp(0.0883+0.4501)=1.713327.
# On average, the duration of disability for people that have a disability due to
# a work accident is 1.71 times that of people that have a disability due to illness,
# in season3, holding all other variables constant.

#### Question #2 d) ####

exp(aft.lognormal$coefficients['reason0'])
# The main effect of the variable reason is exp(β_reason0)=exp(0.0883)=1.092316.
# On average, the duration of disability for people that have a disability due to
# a work accident is 1.09 times that of people that have a disability due to illness,
# holding all other variables constant.

#### Question #2 e) ####

# The interactions that exist within the model makes it more complete than a model
# who would not have such interactions. Therefore, the one without the interactions
# can be treated as a nested model of the other. With a Likelihood Ratio Test, we
# can look if the interactions contribute to explaining the time in disability.
# The difference with the LRT we have done above is that we will look at the
# the betas.

# Nested AFT Log-Normal model (we remove the interactions between reason and season)
aft.lognormal.nested<-survreg(Surv(time, status)~ age + reason + policy + season,
                              dist="lognormal",data=df)

lrtest(aft.lognormal, aft.lognormal.nested)
# The null hypothesis: H0 : B_reason0:season1=B_reason0:season2=B_reason0:season3=0

# The alternate hypothesis: H1: at least one of B_reason0:season1,B_reason0:season2
# or B_reason0:season3=/=0

# The value of the test statistic D is 8.304.
# The corresponding p-value for this test statistic is 0.04013.

# The conclusion of this test: We reject the null hypothesis H0 as the p-value of
# the corresponding test statistic is lower than 0.05 (at 0.04013), the significance
# level we deem appropriate. There is enough evidence to support that the Log-Normal 
# model without the interactions is NOT an adequate simplification of the
# Log-Normal model with the interactions.

################################################################################
############################## Question 3 ######################################
################################################################################

#### Question #3 a) ####

aft.weibull2<-survreg(Surv(time, status)~ age + reason + policy + season + reason*season,
                     dist="weibull",data=df)
# Here, we built another AFT model using the Weibull distribution. It is the same one as above.
# The predict function will not give us precisely the mean with the Weibull distribution,
# so we'll have to keep that in mind later.

df_test = df[0,]
# We create a dataset with new individuals to predict.

df_test[1,] = (c(40,NA,NA,0,NA,1,NA,NA,3))
# Here are the details we have about the first individual.

# Giving a point prediction and interval
pred1<-predict(aft.weibull,newdata=df_test[1,])
pred1

qweibull(0.025,scale=pred1,shape=1/aft.weibull$scale)
qweibull(0.975,scale=pred1,shape=1/aft.weibull$scale)

#### Question #3 b) ####

df_test[2,] = (c(60,NA,NA,1,NA,0,NA,NA,1))

pred2<-predict(aft.weibull,newdata=df_test[2,])
pred2

################################################################################
############################## Question 4 ######################################
################################################################################

#### Question #4 a) ####

coxmod<-coxph(Surv(time, status)~ age + reason + policy + season + reason*season,
              data=df,method="exact")
summary(coxmod)$coefficient

# Here is the model: TODO en Word

#### Question #4 b) ####

# For the variable Policy:

coxmod$coefficients['policy0']
exp(coxmod$coefficients['policy0'])
# According to the fitted model, provided that the proportional hazards model is appropriate:
# The hazard ratio for policy0 vs policy1 (the reference level) is estimated at 
# exp(-0.461582842) = 0.6302852, i.e., the risk of coming back from disability
# decreases by 37.0% for people with policy0 than for people with policy1
# (holding all other variables constant). This accentuates the results we had in 
# question #1 d), where we found that the strata with policy0 had a higher survival
# curve, indicating more time in disability.

# For the variable Age:

coxmod$coefficients['age']
exp(coxmod$coefficients['age'])
# According to the fitted model, provided that the proportional hazards model is appropriate:
# For every one year increase in age, the hazard ratio is multiplied by a factor
# of exp(−0.01865735) = 0.9815156, i.e., for every one year increase in age, the risk of
# someone getting off disability decreases by 1.2% (when all other variables remain unchanged).


#### Question #4 c) ####



#### Question #4 d) ####

summary(coxmod)
# There are 5 covariates that are significant at the 0.05-level, which are age,
# policy0, season1, season2 and reason0:season1.

summary(aft.lognormal)
# There are also 5 covariates that are significant at the 0.05-level, which are age,
# policy0, season1, season2 and reason0:season1.

# Both models have the same significant covariates and most of all covariates that 
# have a positive coefficient in the AFT model have a negative coefficient in the 
# Cox model, with the exception of reason0:season2. Both model parameters help explain
# opposite phenomena: the AFT model parameters have a multiplicative effect on the lifetime
# while the Cox model parameters have a multiplicative effect on the hazard. It is then
# fairly intuitive to say that the coefficient should have opposite polarities when
# comparing the two aforementionned models on the same data.

#### Question #4 e) ####

# Situation i)
df.500days = df
df.500days[df.500days$status==0,2]=500
# Data where censored individuals go to 500 days

coxmod.500days<-coxph(Surv(time, status)~ age + reason + policy + season + reason*season,
              data=df.500days,method="exact")
coxmod.500days

# Situation ii)
df.recover = df
df.recover[df.recover$status==0,'status']=1
# Data where censored individuals go to 500 days

coxmod.recover<-coxph(Surv(time, status)~ age + reason + policy + season + reason*season,
                      data=df.recover,method="exact")
coxmod.recover

# Creating the table of the parameters
Sensitivity_Table = data.frame(matrix(nrow = 9, ncol = 4))
colnames(Sensitivity_Table)=c('Variable', 'Original Model', '500 Days', 'Recover')

Sensitivity_Table[,1]=c('age','reason0','policy0','season1','season2','season3',
                        'reason0:season1','reason0:season2','reason0:season3')
Sensitivity_Table[,2]=coxmod$coefficients
Sensitivity_Table[,3]=coxmod.500days$coefficients
Sensitivity_Table[,4]=coxmod.recover$coefficients

Sensitivity_Table

# Creating the table of the exp(parameters)
Sensitivity_Table2 = data.frame(matrix(nrow = 9, ncol = 4))
colnames(Sensitivity_Table2)=c('Variable', 'Original Model', '500 Days', 'Recover')

Sensitivity_Table2[,1]=c('age','reason0','policy0','season1','season2','season3',
                        'reason0:season1','reason0:season2','reason0:season3')
Sensitivity_Table2[,2]=exp(coxmod$coefficients)
Sensitivity_Table2[,3]=exp(coxmod.500days$coefficients)
Sensitivity_Table2[,4]=exp(coxmod.recover$coefficients)

Sensitivity_Table2

# The first noticeable observation is that for the variables reason0, season3,
# reason0:season2 and reason0:season3, the exp() of the parameter differs in 
# the direction for at least one of the models. For age, season1 and reason0:season1,
# the exp() of the parameter are fairly similar. In general, we would not consider
# this specific model to be robust.
  
################################################################################
############################## Question 5 ######################################
################################################################################

#### Question #5 a) ####

#### Question #5 b) ####

#### Question #5 c) ####
