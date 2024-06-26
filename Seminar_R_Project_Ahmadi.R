# Before running the code, please edit the file's location in part 2.4. Also make sure all of the libraries in part 2 are already installed. Moreover, in each part depends on the concept, I may use different dataset. But the methods and codes are general and we could apply to any dataset.**

##### Before start:
# In the **first** part I introduce the problem from the theoretical aspects, and I mention reasons, challenges, and problems related to unbalancedness and missing data. The **second** part is a quick review on balancedness analysis including important checks and commands. The **third** part is about how we can face with unbalancing and missing values. I explain creating unbalanced dataset, patterns, missing and observed data relations, impution, and deletion method. In the **fourth** part, with respect to what we learnt about handling unbalancing dataset, I jump into application in econometrics. I apply different methods of handling unbalancedness in panel analysis, especially in TWFE. Finally in **fifth** part, I explain the applications of the methods that we have learnt in event studies with the focus on staggered adoption DiD. 

### 1. Introduction

#### 1.1 Balanced and unbalanced panel data

# A **balanced panel** refers to a dataset where all units are observed consistently across all time periods. This means that every unit has complete information for the entire duration of the study. In other words, the dataset is symmetrical with equal observations for each unit and time period.

# In contrast, an **unbalanced panel** arises when some units have missing observations or are observed for fewer time periods than others. This occurs due to various factors such as non-response, attrition, or the addition/removal of units during the study period.
# 
# The main **difference** between balanced and unbalanced panels lies in the completeness and uniformity of the observations across units and time periods. Balanced panels provide equal and consistent information for all units throughout the study, while unbalanced panels exhibit variation in the number of observations.
# 
# #### 1.2 The reasons for the unbalancedness
# 
# 1. **Non-response**: Some units may not provide data for specific time periods due to non-response or refusal to participate. 
# 
# 2. **Attrition**: Attrition refers to the loss of units from the panel over time. It can occur when individuals or entities drop out of the study or become unavailable for data collection.
# 
# 3. **Entry/Exit of units**: Unbalancedness can arise when new units enter the panel or existing units exit during the study period. 
# 
# 4. **Sample selection**: Unbalancedness can also occur if the sample selection process is not uniform across units and time periods. 
# 
# 5. **Study design**: The design of the study itself can contribute to unbalancedness. For example, if data collection efforts are concentrated in certain time periods or for specific subgroups, it can lead to an uneven distribution of observations.
# 
# Each type of the unbalancedness can be considered as **missing data**. 
# 
# #### 1.3 Different types of missing data
# 
# Missing data are errors because your data don’t represent the true values of what you set out to measure. The reason for the missing data is important to consider, because it helps you determine the type of missing data and what you need to do about it.
# 
# 1. **Missing completely at random (MCAR)**: Missing data are randomly distributed across the variable and unrelated to other variables.
# 
# 2. **Missing at random (MAR)**: Missing data are not randomly distributed but they are accounted for by other observed variables.
# 
# 3. **Missing not at random (MNAR)**: Missing data systematically differ from the observed values.
# 
# #### 1.4 Econometrics methods challenges/problems with Unbalanced Data
# 
# In econometrics, unbalanced data refers to a situation where the number of observations or individuals varies across different groups or time periods. Here are a few reasons why unbalanced data can be problematic in these econometric techniques:
# 
# 1. **Potential selection bias**: Unbalancedness can introduce selection bias if the missing observations are not missing at random. If there is a systematic reason why certain observations drop out or are missing, it could lead to biased estimates. 
# 
# 2. **Loss of efficiency**: Unbalanced data can lead to a loss of efficiency in the estimation. In TWFE and DID models, efficiency is achieved by utilizing within-group or within-time variation.
# 
# 3. **Limited generalizability**: If the unbalancedness is substantial and the missing observations are not representative of the overall population, the generalizability of the estimated effects may be limited.
# 
# 4. **Model identification**: In TWFE and DID models, a key assumption is that there is variation in treatment or policy exposure over time or across groups. 
# 
# ### 2. Balancedness analysis
# 
# The following R packages are required:

library(plm)
library(dplyr)
library(Amelia)
library(texreg)
library(fixest)
library(data.table)
library(ggplot2)
library(titanic)
library(cowplot)
library(mice)
library(missForest)
library(finalfit) 
library(naniar)


#### 2.1 Balanced and unbalanced panel data

# In this section, The practical examples are built around Grunfeld’s Investment Data contained in R package plm. This dataset is a panel of 10 observational units (firms) from 1935 to 1954.

data("Grunfeld", package = "plm")

# Since no zeros can be found in this structure, that is no missing information in any given year for each firm, this panel is considered to be **balanced**.

Grunfeld %>%
  select(year, firm) %>%
  table()

# With package `plm` this can be examined with function `is.pbalanced()`.

Grunfeld %>%
  is.pbalanced()

# In package `plm` there is another panel dataset named `EmplUK`. Testing whether this panel is balanced gives the result `FALSE`.

data("EmplUK", package="plm")
EmplUK %>%
  is.pbalanced()

# I am computing a twoway table of the first 10 indexes of firms and find that firm 1 to 4 have no records for the year 1976 and firms 5 to 10 have no data for the year 1983. This panel is considered to be **unbalanced**.

EmplUK %>%
  select(year, firm) %>%
  filter(firm %in% c(1:10)) %>%
  table()

#### 2.2 Time dimension gaps

# Furthermore, the function `is.pconsecutive()` tests whether the entities in the data have any gaps in their time series. Testing the Grunfeld data shows that no gaps in the time periods are present for any firm.

Grunfeld %>%
  is.pconsecutive()

# Next I drop the year 1940 for firm 1, 3 and 7 and repeat the test. The output shows that now there are gaps in the time series for firms 1, 3 and 7!

Grunfeld %>%
  filter(!(firm %in% c(1,3,7) & year == 1940)) %>%
  is.pconsecutive()

# With function `make.pconsecutive()` the time gaps can be identified and closed with `NA` values.

#### 2.3 First attempt for balancing 

# With function `make.pbalanced()` an unbalanced panel can be balanced. There are different ways how to do this and the argument `balance.type=` must be supplied with one out of three options.
# 
# Using `fill` creates a new row with `NAs` for each missing time point. The columns of firm 1, formerly missing for the year 1976, are now filled with NA.

EmplUK.balanced1 <- make.pbalanced(EmplUK, balance.type = "fill")
EmplUK.balanced1[1:8,]

# Using `shared.times` keeps all available firms in the dataset but drops all time periods where at least one firm has no data. Only the years 1978-1982 remain since they are shared by all firms.

EmplUK.balanced2 <- make.pbalanced(EmplUK, balance.type = "shared.times")
EmplUK.balanced2[1:10,]

# By using `shared.individuals` all available time periods are kept but only for those firms which have information for each of them. Only firm 127 to 140 remain since they share all available time periods.

EmplUK.balanced3 <- make.pbalanced(EmplUK, balance.type = "shared.individuals")
EmplUK.balanced3 %>%
  group_by(firm) %>%
  slice(1)

#### 2.4 Create an unbalanced dataset

# We create an unbalanced dataset from Data. 

load("~/Desktop/Data_BIS_ForClass.RData")
new_Data <- Data[, c("countrypair", "time", "lbs.l.all.nonbanks", "treaty.signed", "landlinesp100_cparty", 
                     "pop_cparty", "gdp_cparty")]

# Replace 400 cells with NaN in each column
cols_to_modify <- c("lbs.l.all.nonbanks", "treaty.signed", "landlinesp100_cparty", "pop_cparty", "gdp_cparty")

for (col in cols_to_modify) {
  indices <- sample(which(!is.na(new_Data[[col]])), 400)
  new_Data[indices, col] <- NA
}

# Get unique values in the "countrypair" column
unique_values <- unique(new_Data$countrypair)

# Create a mapping dictionary to replace names with numbers
mapping <- setNames(1:length(unique_values), unique_values)

# Replace values in the "countrypair" column with numbers
new_Data$countrypair <- mapping[new_Data$countrypair]

# Extract the year from the "time" column
new_Data$time <- substr(new_Data$time, 1, 4)

new_Data


#### 2.5 Typical balancedness check

# Now check the balancedness of dataset:

new_Data %>%
  is.pbalanced()

# Here we can see if each `countrypair` has 4 records in each year (showing only first 27 `countrypair`):

new_Data %>%
  select(time, countrypair) %>%
  filter(countrypair %in% c(1:27)) %>%
  table()

# `missmap` draws a map of the **missingness** in a dataset using the image function. The columns are reordered to put the most missing variable farthest to the left. The y-axis shows row number. 

missmap(new_Data, margins = c(7, 7))

# Lastly, check how many rows have complete information (having values in all columns):

sum(complete.cases(new_Data))

# Here is total number of rows: 

nrow(new_Data)


# ### 3. Dip dive in handling missing data
# 
# #### 3.1 Create MCAR and MAR variables to the dataset

# Using the colon_s cancer dataset that comes with `finalfit`, we are interested in exploring the association between a cancer obstructing the bowel and 5-year survival, accounting for other patient and disease characteristics.
# 
# For demonstration purposes, we will create **random MCAR and MAR** smoking variables to the dataset.

# Create some extra missing data
## Smoking missing completely at random
set.seed(1)
colon_s$smoking_mcar = 
  sample(c("Smoker", "Non-smoker", NA), 
    dim(colon_s)[1], replace=TRUE, 
    prob = c(0.2, 0.7, 0.1)) %>% 
  factor() %>% 
    ff_label("Smoking (MCAR)")

## Smoking missing conditional on patient sex
colon_s$smoking_mar[colon_s$sex.factor == "Female"] = 
  sample(c("Smoker", "Non-smoker", NA), 
    sum(colon_s$sex.factor == "Female"), 
    replace = TRUE,
    prob = c(0.1, 0.5, 0.4))

colon_s$smoking_mar[colon_s$sex.factor == "Male"] = 
  sample(c("Smoker", "Non-smoker", NA), 
    sum(colon_s$sex.factor == "Male"), 
    replace=TRUE, prob = c(0.15, 0.75, 0.1))
colon_s$smoking_mar = factor(colon_s$smoking_mar) %>% 
    ff_label("Smoking (MAR)")

# Examine with ff_glimpse
explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor",  
  "smoking_mcar", "smoking_mar")
dependent = "mort_5yr"

colon_s %>% 
  ff_glimpse(dependent, explanatory)

#### 3.2 Look for patterns of missingness

# In detecting patterns of missingness, this plot is useful. Row number is on the x-axis and all included variables are on the y-axis. Associations between missingness and observations can be easily seen, as can relationships of missingness between variables.

colon_s %>%
  select(age, sex.factor, nodes, obstruct.factor, smoking_mcar, smoking_mar, mort_5yr) %>%
  missing_plot()

# `missing_pattern` simply wraps `mice::md.pattern` using `finalfit` grammar. This produces a table and a plot showing the pattern of **missingness** between variables.

explanatory = c("age", "sex.factor", 
  "obstruct.factor",  
  "smoking_mcar", "smoking_mar")
dependent = "mort_5yr"

colon_s %>% 
  missing_pattern(dependent, explanatory)

# This allows us to look for patterns of missingness between variables. There are 14 patterns in this data. The number and pattern of missingness help us to determine the likelihood of it being random rather than systematic.
# 
# #### 3.3 Check for associations between missing and observed data
# 
# In deciding whether data is MCAR or MAR, one approach is to explore patterns of missingness between levels of included variables. This is particularly important for a primary outcome measure/dependent variable.
# 
# Take for example “death”. When that outcome is missing it is often for a particular reason. For example, perhaps patients undergoing emergency surgery were less likely to have complete records compared with those undergoing planned surgery. And of course, death is more likely after emergency surgery.
# 
# missing_pairs uses functions from the **GGally** package. It produces pairs plots to show **relationships between missing values and observed values** in all variables.

explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor",  
  "smoking_mcar", "smoking_mar")
dependent = "mort_5yr"
colon_s %>% 
  missing_pairs(dependent, explanatory)

# For continuous variables (age and nodes), the distributions of observed and missing data can immediately be visually compared. For example, look at Row 1 Column 2. The age of patients who’s mortality data is known is the blue box plot, and the age of patients with missing mortality data is the grey box plot.
# 
# For categorical data, the comparisons are presented as counts. To be able to compare proportions, we can add the `position = "fill"` argument:

colon_s %>% 
  missing_pairs(dependent, explanatory, position = "fill")

# Find the two sets of bar plots that show the proportion of missing smoking data for sex (bottom of Column 3). Missingness in Smoking (MCAR) does not relate to sex - females and males have the same proportion of missing data. Missingness in Smoking (MAR), however, does differ by sex as females have more missing data than men here. 
# 
# We can also confirm this by using `missing_compare()`:

explanatory <- c("age", "sex.factor", 
                 "nodes", "obstruct.factor")
dependent <- "smoking_mcar"

missing_mcar <- colon_s %>% 
  missing_compare(dependent, explanatory)

missing_mcar

dependent <- "smoking_mar"

missing_mar <- colon_s %>% 
  missing_compare(dependent, explanatory)

missing_mar

# It takes dependent and explanatory variables, and in this context “dependent” refers to the variable being tested for missingness against the explanatory variables.As expected, a relationship is seen between sex and smoking (MAR) but not smoking (MCAR).
# 
# 
# #### 3.4 How to handle missing data

# 5. Sensitivity analysis
# 
# ##### 3.4.1 Handling missing data: MNAR
# 
# Missing not at random data is tough in economics and healthcare. To determine if data are MNAR for definite, we need to know their value in a subset of observations (patients).
# 
# Imagine that smoking status is poorly recorded in patients admitted to hospital as an emergency with an obstructing bowel cancer. Obstructing bowel cancers may be larger or their position may make the prognosis worse. Smoking may relate to the aggressiveness of the cancer and may be an independent predictor of prognosis. The missing values for smoking may therefore not be random. Smoking may be more common in the emergency patients and may be more common in those that die.
# 
# There is no easy way to handle this. If at all possible, try to get the missing data. Otherwise, be careful when drawing conclusions from analyses where data are thought to be missing not at random.
# 
# ##### 3.4.2 Handling missing data: MCAR
# 
# Using the examples, we identify that smoking (MCAR) is missing completely at **random**.
# 
# We know nothing about the missing values themselves, but we know of no plausible reason that the values of the missing data, for say, people who died should be different to the values of the missing data for those who survived. The pattern of missingness is therefore not felt to be MNAR.
# 
# **Common solution: row-wise deletion**: 

explanatory = c("age", "sex.factor", 
  "nodes", "obstruct.factor",  
  "smoking_mcar")
dependent = "mort_5yr"
colon_s %>%
  summary_factorlist(dependent, explanatory, 
  p=TRUE, add_dependent_label=TRUE) -> t1
knitr::kable(t1, align=c("l", "l", "r", "r", "r"))

##### 3.4.3 Handling missing data: MAR

# Considering that the smoking variable is more likely to be missing if the patient is female (missing_compare shows a relationship). But, say, that the missing values are not different from the observed values. Missingness is then MAR.
# 
# If we simply drop all the patients for whom smoking is missing (list-wise deletion), then we drop **relatively more females than men**. This may have consequences for our conclusions if sex is associated with our explanatory variable of interest or outcome.
# 
# One of the best approaches to dealing with MAR is impution. In the next part we study how we can use this approach. 
# 
# #### 3.5 Top Ways for Imputing Missing Data
# 
# To showing effectiveness of the ways, in this part we use another dataset. But do not get confused! We can easily apply them to any dataset that we want. 
# 
# We will use the training portion of the **Titanic dataset** and try to impute missing values for the Age column:

titanic_train$Age[1:50]

# Before diving into the imputation, let’s visualize the distribution of our variable:

ggplot(titanic_train, aes(Age)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

##### 3.5.1 Simple Value Imputation with Built-in Functions

# You don’t actually need an R package to impute missing values. You can do the whole thing manually, provided the imputation techniques are simple. We’ll cover constant, mean, and median imputations in this section and compare the results.
# 
# The value_imputed variable will store a data.frame of the imputed ages. The imputation itself boils down to replacing a column subset that has a value of NA with the value of our choice. This will be:
# 
# 1. **Zero**: constant imputation, feel free to change the value.
# 
# 2. **Mean (average)**: average age after when all NA‘s are removed.
# 
# 3. **Median**: median age after when all NA‘s are removed.

value_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_zero = replace(titanic_train$Age, is.na(titanic_train$Age), 0),
  imputed_mean = replace(titanic_train$Age, is.na(titanic_train$Age), mean(titanic_train$Age, na.rm = TRUE)),
  imputed_median = replace(titanic_train$Age, is.na(titanic_train$Age), median(titanic_train$Age, na.rm = TRUE))
)
value_imputed[1:20,]

# Let’s take a look at the variable distribution changes introduced by imputation:

h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

# All imputation methods severely impact the distribution. There are a lot of missing values, so setting a single constant value doesn’t make much sense. Zero imputation is the worst, as it’s highly unlikely for close to 200 passengers to have the age of zero. Maybe mode imputation would provide better results, but it general choosing which one is better depends on the problem and data that you have.

##### 3.5.2 Impute Missing Values with MICE

# **MICE** stands for Multivariate Imputation via Chained Equations, and it’s one of the most common packages for R users. It assumes the missing values are **missing at random (MAR)**. 

library(mice)

titanic_numeric <- titanic_train %>%
  select(Survived, Pclass, SibSp, Parch, Age)

md.pattern(titanic_numeric)

# Onto the imputation now. We’ll use the following MICE imputation methods:

# 1. **pmm**: Predictive mean matching.
# 
# 2. **cart**: Classification and regression trees.
# 
# 3. **laso.norm**: Lasso linear regression.

mice_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_pmm = complete(mice(titanic_numeric, method = "pmm"))$Age,
  imputed_cart = complete(mice(titanic_numeric, method = "cart"))$Age,
  imputed_lasso = complete(mice(titanic_numeric, method = "lasso.norm"))$Age
)

mice_imputed[1:20,]


# Let’s take a look at the variable distribution changes introduced by MICE:

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)


##### 3.5.3 Imputation with missForest

# The **Miss Forest imputation** technique is based on the **Random Forest** algorithm. It’s a non-parametric imputation method, which means it doesn’t make explicit assumptions about the function form, but instead tries to estimate the function in a way that’s closest to the data points. In other words, it builds a random forest model for each variable and then uses the model to predict missing values. ([More about this algorithm in details](https://rpubs.com/lmorgan95/MissForest#:~:text=MissForest%20is%20a%20random%20forest,then%20predicts%20the%20missing%20part.))

library(missForest)

missForest_imputed <- data.frame(
  original = titanic_numeric$Age,
  imputed_missForest = missForest(titanic_numeric)$ximp$Age
)
missForest_imputed[1:20,]

# Let’s visualize the distributions:

h1 <- ggplot(missForest_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("missForest-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, nrow = 1, ncol = 2)

##### 3.5.4 Imputation with amelia

# This package also performs multiple imputation (generate imputed data sets) to deal with missing values. Multiple imputation helps to reduce bias and increase efficiency. It is enabled with bootstrap based EMB algorithm which makes it faster and robust to impute many variables including cross sectional, time series data etc. ([More about this algorithm in details](https://www.rdocumentation.org/packages/Amelia/versions/1.7.5/topics/amelia))


amelia_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_amelia = amelia(titanic_numeric)$imputations[[1]]$Age
)
amelia_imputed[1:20,]

# Let’s visualize the distributions:

h1 <- ggplot(amelia_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(amelia_imputed, aes(x = imputed_amelia)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("amelia-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, nrow = 1, ncol = 2)

### 4. Panel Analysis

#### 4.1 Pooled OLS

# For now, I use `new_Data` from part `2.4` which is an unbalanced dataset with `lm()` Function:

pooled_ols <- lm(log(lbs.l.all.nonbanks) ~ treaty.signed + landlinesp100_cparty +pop_cparty + gdp_cparty, data = new_Data)

#### 4.2 Individual fixed effects panel

# Individual level fixed effects within transformed on `countrypair` level. Within transformations: 

new_Data[, logliabs := log(lbs.l.all.nonbanks)]
new_Data[, liabs_within_individual := logliabs - mean(logliabs, na.rm = T), by = countrypair]
new_Data[, treaty_within_individual := treaty.signed - mean(treaty.signed, na.rm = T), by = countrypair]
new_Data[, pop_within_individual := pop_cparty - mean(pop_cparty, na.rm = T), by = countrypair]
new_Data[, landlines_within_individual := landlinesp100_cparty - mean(landlinesp100_cparty, na.rm = T),
     by = countrypair]
new_Data[, gdp_within_individual := gdp_cparty - mean(gdp_cparty, na.rm = T), by = countrypair]


# With `lm()` Function:

individual_fe <- lm(liabs_within_individual ~ - 1 + treaty_within_individual + landlines_within_individual + 
                      pop_within_individual + gdp_within_individual, data = new_Data)

#### 4.3 Time fixed effects panel

# Time level fixed effects within transformed on time level. Within transformations: 

new_Data[, logliabs := log(lbs.l.all.nonbanks)]
new_Data[, liabs_within_time := logliabs - mean(logliabs, na.rm = T), by = time]
new_Data[, treaty_within_time := treaty.signed - mean(treaty.signed, na.rm = T), by = time]
new_Data[, pop_within_time := pop_cparty - mean(pop_cparty, na.rm = T), by = time]
new_Data[, landlines_within_time := landlinesp100_cparty - mean(landlinesp100_cparty, na.rm = T), by = time]
new_Data[, gdp_within_time := gdp_cparty - mean(gdp_cparty, na.rm = T), by = time]

# With `lm()` Function:

time_fe <- lm(liabs_within_time ~ - 1 + treaty_within_time + landlines_within_time + pop_within_time + gdp_within_time, 
              data = new_Data)

#### 4.4 Two ways fixed effects panel (Individual first)

# Demean these variables also by time:

new_Data[, liabs_within_twoways_individual_first := liabs_within_individual - 
       mean(liabs_within_individual, na.rm = T), by = time]
new_Data[, pop_within_twoways_individual_first := pop_within_individual - 
       mean(pop_within_individual, na.rm = T), by = time]
new_Data[, landlines_within_twoways_individual_first := landlines_within_individual - 
       mean(landlines_within_individual, na.rm = T), by = time]
new_Data[, gdp_within_twoways_individual_first := gdp_within_individual - 
       mean(gdp_within_individual, na.rm = T), by = time]
new_Data[, treaty_within_twoways_individual_first := treaty_within_individual - 
       mean(treaty_within_individual, na.rm = T), by = time]

# With `lm()` Function:

twfe_individual_first <- lm(liabs_within_twoways_individual_first ~ -1 + treaty_within_twoways_individual_first + 
     landlines_within_twoways_individual_first + pop_within_twoways_individual_first + 
     gdp_within_twoways_individual_first,
   data = new_Data)

#### 4.5 Two ways fixed effects panel (Time first)

# Demean these variables also by time:

new_Data[, liabs_within_twoways_time_first := liabs_within_time - 
       mean(liabs_within_time, na.rm = T), by = countrypair]
new_Data[, pop_within_twoways_time_first := pop_within_time - 
       mean(pop_within_time, na.rm = T), by = countrypair]
new_Data[, landlines_within_twoways_time_first := landlines_within_time - 
       mean(landlines_within_time, na.rm = T), by = countrypair]
new_Data[, gdp_within_twoways_time_first := gdp_within_time - 
       mean(gdp_within_time, na.rm = T), by = countrypair]
new_Data[, treaty_within_twoways_time_first := treaty_within_time - 
       mean(treaty_within_time, na.rm = T), by = countrypair]

# With `lm()` Function:

twfe_time_first <- lm(liabs_within_twoways_time_first ~ -1 + treaty_within_twoways_time_first + 
     landlines_within_twoways_time_first + pop_within_twoways_time_first + 
     gdp_within_twoways_time_first,
   data = new_Data)

# **Compare the results (with deleting missing values):**

modelnames <- c("Pooled OLS", "Individual FE", "Time FE", "TWFE (Individual first)", "TWFE (Time first)")
h <- htmlreg(list(pooled_ols,individual_fe,time_fe,twfe_individual_first,twfe_time_first), include.ci = FALSE, caption.above = TRUE, caption = "Panel analysis performance (with deleting missing values)", custom.model.names = modelnames,include.adjr = TRUE, include.rsquared = TRUE, include.rmse = TRUE, include.nobs = TRUE, digits = 5)

htmltools::HTML(h)

# **Important**: We can see that when there are missing values in the dataset, in TWFE it is important to consider which one to be first: time or individual. The reason is it is important for the first stage which data are deleted and it affects second stage. As much as the unbalancedness get bigger, the difference will be more significant. 
# 
# #### 4.6 Comparing original dataset, imputed dataset, and deleted (missing values) dataset
# 
# In this part, we want to compare the performance of two way fixed effect on original dataset (without missing values), imputed dataset, and deleted (missing values) dataset. The original dataset is produced in part 2.4 and deleted dataset is from previous part. in The following we generate imputed dataset with **MICE** and then we can see the results.

# for original dataset

# individual FE
Data[, logliabs := log(lbs.l.all.nonbanks)]
Data[, liabs_within_individual := logliabs - mean(logliabs, na.rm = T), by = countrypair]
Data[, treaty_within_individual := treaty.signed - mean(treaty.signed, na.rm = T), by = countrypair]
Data[, pop_within_individual := pop_cparty - mean(pop_cparty, na.rm = T), by = countrypair]
Data[, landlines_within_individual := landlinesp100_cparty - mean(landlinesp100_cparty, na.rm = T),by = countrypair]
Data[, gdp_within_individual := gdp_cparty - mean(gdp_cparty, na.rm = T), by = countrypair]

# time FE
Data[, logliabs := log(lbs.l.all.nonbanks)]
Data[, liabs_within_time := logliabs - mean(logliabs, na.rm = T), by = time]
Data[, treaty_within_time := treaty.signed - mean(treaty.signed, na.rm = T), by = time]
Data[, pop_within_time := pop_cparty - mean(pop_cparty, na.rm = T), by = time]
Data[, landlines_within_time := landlinesp100_cparty - mean(landlinesp100_cparty, na.rm = T), by = time]
Data[, gdp_within_time := gdp_cparty - mean(gdp_cparty, na.rm = T), by = time]

# TWFE individual first
Data[, liabs_within_twoways_individual_first := liabs_within_individual - 
       mean(liabs_within_individual, na.rm = T), by = time]
Data[, pop_within_twoways_individual_first := pop_within_individual - 
       mean(pop_within_individual, na.rm = T), by = time]
Data[, landlines_within_twoways_individual_first := landlines_within_individual - 
       mean(landlines_within_individual, na.rm = T), by = time]
Data[, gdp_within_twoways_individual_first := gdp_within_individual - 
       mean(gdp_within_individual, na.rm = T), by = time]
Data[, treaty_within_twoways_individual_first := treaty_within_individual - 
       mean(treaty_within_individual, na.rm = T), by = time]

twfe_individual_first_original <- lm(liabs_within_twoways_individual_first ~ -1 + treaty_within_twoways_individual_first + 
     landlines_within_twoways_individual_first + pop_within_twoways_individual_first + 
     gdp_within_twoways_individual_first,
   data = Data)


# TWFE time first
Data[, liabs_within_twoways_time_first := liabs_within_time - 
       mean(liabs_within_time, na.rm = T), by = countrypair]
Data[, pop_within_twoways_time_first := pop_within_time - 
       mean(pop_within_time, na.rm = T), by = countrypair]
Data[, landlines_within_twoways_time_first := landlines_within_time - 
       mean(landlines_within_time, na.rm = T), by = countrypair]
Data[, gdp_within_twoways_time_first := gdp_within_time - 
       mean(gdp_within_time, na.rm = T), by = countrypair]
Data[, treaty_within_twoways_time_first := treaty_within_time - 
       mean(treaty_within_time, na.rm = T), by = countrypair]

twfe_time_first_original <- lm(liabs_within_twoways_time_first ~ -1 + treaty_within_twoways_time_first + 
     landlines_within_twoways_time_first + pop_within_twoways_time_first + 
     gdp_within_twoways_time_first,
   data = Data)

# Convert your data.table object to a data.frame
new_Data_df <- as.data.frame(new_Data)

# Specify the columns with missing values
cols_with_missing <- c("lbs.l.all.nonbanks", "treaty.signed", "landlinesp100_cparty", "pop_cparty", "gdp_cparty")

# Perform multiple imputations using MICE
imputed_Data_df <- mice(new_Data_df[cols_with_missing], m = 5, method = "pmm")

# Convert the imputed data back to a data.table object
imputed_Data <- as.data.table(complete(imputed_Data_df))

# Add "countrypair" and "time" columns from new_Data
imputed_Data[, c("countrypair", "time") := new_Data[, .(countrypair, time)]]

imputed_Data

# for imputed dataset

# individual FE
imputed_Data[, logliabs := log(lbs.l.all.nonbanks)]
imputed_Data[, liabs_within_individual := logliabs - mean(logliabs, na.rm = T), by = countrypair]
imputed_Data[, treaty_within_individual := treaty.signed - mean(treaty.signed, na.rm = T), by = countrypair]
imputed_Data[, pop_within_individual := pop_cparty - mean(pop_cparty, na.rm = T), by = countrypair]
imputed_Data[, landlines_within_individual := landlinesp100_cparty - mean(landlinesp100_cparty, na.rm = T),by = countrypair]
imputed_Data[, gdp_within_individual := gdp_cparty - mean(gdp_cparty, na.rm = T), by = countrypair]

# time FE
imputed_Data[, logliabs := log(lbs.l.all.nonbanks)]
imputed_Data[, liabs_within_time := logliabs - mean(logliabs, na.rm = T), by = time]
imputed_Data[, treaty_within_time := treaty.signed - mean(treaty.signed, na.rm = T), by = time]
imputed_Data[, pop_within_time := pop_cparty - mean(pop_cparty, na.rm = T), by = time]
imputed_Data[, landlines_within_time := landlinesp100_cparty - mean(landlinesp100_cparty, na.rm = T), by = time]
imputed_Data[, gdp_within_time := gdp_cparty - mean(gdp_cparty, na.rm = T), by = time]

# TWFE individual first
imputed_Data[, liabs_within_twoways_individual_first := liabs_within_individual - 
       mean(liabs_within_individual, na.rm = T), by = time]
imputed_Data[, pop_within_twoways_individual_first := pop_within_individual - 
       mean(pop_within_individual, na.rm = T), by = time]
imputed_Data[, landlines_within_twoways_individual_first := landlines_within_individual - 
       mean(landlines_within_individual, na.rm = T), by = time]
imputed_Data[, gdp_within_twoways_individual_first := gdp_within_individual - 
       mean(gdp_within_individual, na.rm = T), by = time]
imputed_Data[, treaty_within_twoways_individual_first := treaty_within_individual - 
       mean(treaty_within_individual, na.rm = T), by = time]

twfe_individual_first_imputed <- lm(liabs_within_twoways_individual_first ~ -1 + treaty_within_twoways_individual_first + 
     landlines_within_twoways_individual_first + pop_within_twoways_individual_first + 
     gdp_within_twoways_individual_first,
   data = imputed_Data)


# TWFE time first
imputed_Data[, liabs_within_twoways_time_first := liabs_within_time - 
       mean(liabs_within_time, na.rm = T), by = countrypair]
imputed_Data[, pop_within_twoways_time_first := pop_within_time - 
       mean(pop_within_time, na.rm = T), by = countrypair]
imputed_Data[, landlines_within_twoways_time_first := landlines_within_time - 
       mean(landlines_within_time, na.rm = T), by = countrypair]
imputed_Data[, gdp_within_twoways_time_first := gdp_within_time - 
       mean(gdp_within_time, na.rm = T), by = countrypair]
imputed_Data[, treaty_within_twoways_time_first := treaty_within_time - 
       mean(treaty_within_time, na.rm = T), by = countrypair]

twfe_time_first_imputed <- lm(liabs_within_twoways_time_first ~ -1 + treaty_within_twoways_time_first + 
     landlines_within_twoways_time_first + pop_within_twoways_time_first + 
     gdp_within_twoways_time_first,
   data = imputed_Data)


# **Compare the results:**
# 
# Here you can see the results for comparing performance of TWFE on original dataset *without missing values), imputed dataset, and deleted (missing values) dataset:

modelnames <- c("TWFE (Individual first) original data", "TWFE (Time first) original data", "TWFE (Individual first) deleted data", "TWFE (Time first) deleted data", "TWFE (Individual first) imputed data", "TWFE (Time first) imputed data")
h <- htmlreg(list(twfe_individual_first_original, twfe_time_first_original, twfe_individual_first, twfe_time_first, twfe_individual_first_imputed, twfe_time_first_imputed), include.ci = FALSE, caption.above = TRUE, caption = "Comparing performance of TWFE on original dataset *without missing values), imputed dataset, and deleted (missing values) dataset", custom.model.names = modelnames,include.adjr = TRUE, include.rsquared = TRUE, include.rmse = TRUE, include.nobs = TRUE, digits = 5)

htmltools::HTML(h)

# As shown above, the results of imputed dateset have different from the results of original dataset. Also, the performance of deleted dataset is closer to original dataset. As we discussed, it really depends on how munch unbalancedness you have and what kind of unbalancedness you are facing with. As a result, in many cases that you are facing with not too munch unbalancing, it is worth to use deletion method. whenever your missing values become more important, you need to think about using an imputing method. These kinds of problems have not a unique solution. You need to engineer the dataset, make some analysis, and consider different scenarios to obtain the best results. 
# 
# Moreover, remember the data and numbers in econometrics have meaning. As a result, it is not always easy to interpret the generated and imputed data in the context of economics. So, be careful!


### 5. Staggered Adoption Difference-in-Differences

#### 5.1 Event Studies

# In this section, we create a new simple dataset to show to the analysis: 

set.seed(123)
Data <- data.table(unit = rep(paste("unit", 1:1000), each = 10),
                   year = rep(2001:2010, 1000),
                   y = rnorm(10000),
                   treatment_group = rep(c(1,0), each = 5000))
Data

# Lets start with one specific treatment for all treated units:

Data[treatment_group == 1 & year == 2005, y := y + 0.5]
Data[treatment_group == 1 & year == 2006, y := y + 0.25]

# Define a treatment dummy that takes value 1 in 2005 only:

Data[, dummy_2005_es := ifelse(year == 2005, 1, 0)]

# Interact this dummy with the dummy indicating treatment status:

Data[, dummy_2005_es_treat := dummy_2005_es * treatment_group]

# Since we want to see the timing of effects, lets lag this dummy for all periods:

Data[, dummy_m4_treat := shift(dummy_2005_es_treat, 4, type = "lead", fill = 0), by = unit]
Data[, dummy_m3_treat := shift(dummy_2005_es_treat, 3, type = "lead", fill = 0), by = unit]
Data[, dummy_m2_treat := shift(dummy_2005_es_treat, 2, type = "lead", fill = 0), by = unit]
Data[, dummy_m1_treat := shift(dummy_2005_es_treat, 1, type = "lead", fill = 0), by = unit]
Data[, dummy_0_treat := dummy_2005_es_treat]
Data[, dummy_1_treat := shift(dummy_2005_es_treat, 1, type = "lag", fill = 0), by = unit]
Data[, dummy_2_treat := shift(dummy_2005_es_treat, 2, type = "lag", fill = 0), by = unit]
Data[, dummy_3_treat := shift(dummy_2005_es_treat, 3, type = "lag", fill = 0), by = unit]
Data[, dummy_4_treat := shift(dummy_2005_es_treat, 4, type = "lag", fill = 0), by = unit]
Data[, dummy_5_treat := shift(dummy_2005_es_treat, 5, type = "lag", fill = 0), by = unit]
Data

# Now we can run a panel regression:
my_fixest <- feols(y ~ i(year, treatment_group, 2004),
                   data = Data)
print(my_fixest)


#### 5.2 Dynamic specification: time trend

# This is not a staggered adoption because we only have **one treatment time**. 

Data <- data.table(unit = rep(paste0("unit",1:1000), each =10),
                   year = rep(2001:2010, 1000),
                   y = rnorm(10000),
                   treatment = rep(c(1,0), each = 5000))
Data[treatment == 1 & year == 2005, y := y + 0.5]
Data[treatment == 1 & year == 2006, y := y + 0.25]
Data[, y := y + year]

my_fixest <- feols(y ~ i(year, treatment, 2004),
                   data = Data)
print(my_fixest)

#### 5.3 Staggered adoption

# We have **different treatment years** for each unit:

set.seed(123)
Data <- data.table(unit = rep(paste0("unit",1:1000), each =10),
                   year = rep(2001:2010, 1000),
                   y = rnorm(10000),
                   treatment = rep(c(1,0), each = 5000))

Data[treatment == 1, treatment_year := sample(year, 1), by = unit]
# event time:
Data[, event_time := year - treatment_year]

Data[treatment == 1 & event_time == 1, y := y - 0.8]
Data[treatment == 1 & event_time == 2, y := y - 0.4]
Data[treatment == 1 & event_time == 3, y := y - 0.2]

my_fixest <- feols(y ~ i(event_time, treatment, 0) ,
                   data = Data)
print(my_fixest)

# Assume **units are different**, and adding **unit fixed effects**:

Data[, unit_time_invariant_effect := sample(1:100, 1), by = unit]
Data[, y := y + unit_time_invariant_effect]

my_fixest <- feols(y ~ i(event_time, treatment, 0) |
                     unit,
                   data = Data)
print(my_fixest)

# Lets say we have a time trend in our data, and adding **time fixed effects**:

Data[, y := y + year]

my_fixest <- feols(y ~ i(event_time, treatment, 0) |
                     unit + year,
                   data = Data)
print(my_fixest)

#### 5.4 Comparing original dataset, imputed dataset, and deleted (missing values) dataset

# In the previous part, we explained the results of staggered adoption DiD with different assumption on original dataset. In this part, we want to evaluate on imputed dataset (MICE method) and deleted (missing values) dataset. 
# 
# Firstly, we generate an unbalanced dataset from the original version:

set.seed(123)
# Generate random indices to change 3000 values to NA
indices <- sample(1:nrow(Data), 3000)
# Change the selected values in "y" column to NA
Data[indices, y := NA]

# With omitting NA values, we produce **deleted dataset**:

# Remove rows with NA values in the "y" column
Data_deleted <- Data[!is.na(y)]

# Now, we reproduce the result of previous part but with **deleted dataset** (**unit and time fixed effect**):

my_fixest <- feols(y ~ i(event_time, treatment, 0) |
                     unit + year,
                   data = Data_deleted)
print(my_fixest)

# For the last step, we want to generate the result with imputed dataset. To achieving this goal, firstly I produce the imputed dataset:

# Convert your data.table object to a data.frame
Data_df <- as.data.frame(Data)

# Create a copy of the original data.frame for imputation
Data_impute <- Data_df

# Specify the columns to be imputed
cols_to_impute <- "y"

# Perform multiple imputations using MICE
imputed_data <- mice(Data_impute, method = "pmm", m = 5)
imputed_data <- complete(imputed_data)

# Replace the "y" column in the original data.table with the imputed values
Data[, y := imputed_data$y]

# Now, we reproduce the result of previous part but with **imputed dataset** (**unit and time fixed effect**):

my_fixest <- feols(y ~ i(event_time, treatment, 0) |
                     unit + year,
                   data = Data)
print(my_fixest)

# As we can see, the performance of deleted dataset is much better than imputed dataset. Here is two points that we should be curios about them:
# 
# 1. Mathematics behind imputation method is important. Do not consider it as a black box which certainly help us the best. 
# 
# 2. In the most of the time, deletion method could be the safest approach. Be careful with the dataset because numbers have meanings behind!
# 
# 3. Before using any approach to handle missing values and unbalanced dataset; we should decide which kind of missing we have. Is it MAR, MCAR, or MNAR? Based on the answer, we are allowed to use some methods. Again, I should emphasize that in most of the times, it is better to see the performance on different methods because it may make the decision simpler. 
# 
# 4. In part 1.4 I counted some challenges/problems of unbalancedness. Check them again, because at the first point our solution should be validated.