################################################################################
################################### Titles #####################################
################################################################################

# Title: Homework 1
# Class: Survival and Longitudinal Data Analysis
# Professor: Schulz, Juliana
# University: HEC Montréal
# Team members: LastName, FirstName, ID
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


lifetab(tis = interval_vector, ninit = initial_subjects,
        nlost = censored_subjects, nevent = observed)



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

#### Question #1 a) ####

#### Question #1 b) ####

#### Question #1 c) ####

#### Question #1 d) ####

#### Question #1 e) ####

################################################################################
############################## Question 3 ######################################
################################################################################

#### Question #1 a) ####

aft.weibull<-survreg(Surv(time, status)~ age + reason + policy + season + reason*season,
                     dist="weibull",data=df)

df_test = df[0,]
df_test[1,] = (c(40,NA,NA,0,NA,1,NA,NA,3))
df_test[2,] = (c(60,NA,NA,1,NA,0,NA,NA,1))

pred1<-predict(aft.weibull,newdata=df_test[1,])
pred1

qweibull(0.025,scale=1/(0.0106),shape=1/1.1682)
qweibull(0.975,scale=1/(0.0106),shape=1/1.1682)

pred2<-predict(aft.weibull,newdata=df_test[2,])
pred2

#### Question #1 b) ####

################################################################################
############################## Question 4 ######################################
################################################################################

#### Question #1 a) ####

#### Question #1 b) ####

#### Question #1 c) ####

#### Question #1 d) ####

#### Question #1 e) ####

################################################################################
############################## Question 5 ######################################
################################################################################

#### Question #1 a) ####

#### Question #1 b) ####

#### Question #1 c) ####
