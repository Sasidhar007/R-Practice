# Jane Smith 2016 project analysis
rm(list = ls())

# Read in data ----
wd <- "C:/Users/msasi/Desktop/Analysts Institute/data_01042018/"
rand_data <- read.csv(paste0(wd, "data/smith_rand.csv"), stringsAsFactors = F)
covar_data <- read.csv(paste0(wd, "data/smith_covariates.csv"), stringsAsFactors = F)
outcome_data <- read.csv(paste0(wd, "data/smith_outcomes.csv"), stringsAsFactors = F)

#convert to upper case I had done this as there were few records wherein the ai_id were in lowercase
rand_data$ai_id <- toupper(rand_data$ai_id)
covar_data$ai_id <- toupper(covar_data$ai_id)
outcome_data$ai_id <- toupper(outcome_data$ai_id)

# Combine randomization, covariate, and survey outcome data
df <- merge(rand_data, covar_data, by = "ai_id")
df <- merge(df, outcome_data, by = "ai_id")


# Get rid of duplicated ai_ids
df <- df[!duplicated(df$ai_id),]

# Clean Data -----
# Take covariate data from covariate file
df$gender <- df$gender.y
df$race <- df$race.x
df$age <- df$age.x

# Cut down number of race categories
df$race_clean <- NA
df$race_clean[df$race == "caucasian"] <- "0White"
df$race_clean[df$race == "black"] <- "1Black"
df$race_clean[df$race == "hispanic"] <- "2Hispanic"
df$race_clean[df$race == "asian" |
                df$race == "middleEastern" |
                df$race == "unknown"|
                df$race == "nativeAmerican"] <- "3Other"
#in the above native americans weren't included and hence there were missing values

# Bin age
df$age_bin <- NA
df$age_bin[df$age >= 18 & df$age <= 34] <- "18-34"
df$age_bin[df$age >= 35 & df$age <= 44] <- "35-44"
df$age_bin[df$age >= 45 & df$age <= 64] <- "45-64"
df$age_bin[df$age >= 65 & df$age != 999] <- "65+"
df$age_bin[df$age == 999] <- "Unknown"

# Check for balance across assignment -----
# So I know we want to check that there is no statistically significant
# relationship between treatment and any of the covariates. I'm not sure
# how to do because there are 3 treatment categories.

# Estimate voter persuasion treatment effects -----
# Code outcome variable

df$democrat <- NA
df$democrat[df$outcome == "Democrat Jane Smith" |
              df$outcome == "Lean Democrat Jane Smith"] <- TRUE
df$democrat[df$outcome == "Republican Paul Jones" |
              df$outcome == "Independent Ashley Canner" |
              df$outcome == "Lean Republican Paul Jones" |
              df$outcome == "Undecided" |
              df$outcome == "Lean Independent Ashley Canner" |
              is.na(df$outcome[df$turnout2016 == 'FALSE'])] <- FALSE


persuade_no_covar <- glm(democrat ~ canvass_treat, data = df, family = "binomial")
summary(persuade_no_covar)

# Control for race, age bins, gender, partisanship, and whether they turned
# out in 2016.

persuade_covar <- glm(democrat ~ canvass_treat + race_clean + age + gender +
                        partisanscore + turnout2016,
                      data = df, family = "binomial")
summary(persuade_covar)



# Looks like ProGreen was effective and ProEcon wasn't.

# Estimate voter mobilization treatment effects -----

turnout_no_covar <- glm(turnout2016 ~ canvass_treat, data = df,
                        family = "binomial")
summary(turnout_no_covar)

turnout_covar <- glm(turnout2016 ~ canvass_treat + race_clean + age + gender +
                       partisanscore,
                     data = df, family = "binomial")
summary(turnout_covar)

# Looks like both ProGreen and ProEcon increased turnout, but ProEcon by more.

# Graphs ------
# I didn't have time to create the graphs for the presentation. Could you do it?
# I know it needs to have the treatment effect in all groups and 90% error bars.

# Graphed mean values of non-NA turnout & outcome relative to each cofactor
# This hinges on the concept that booleans are treated in mean() as '1' or '0'.
# Recalling that gender is now gender.y, given NA values in gender.x

genderTurnoutSuccess <- tapply(df$turnout2016[!is.na(df$turnout2016)], list(df$canvass_treat[!is.na(df$turnout2016)],df$gender[!is.na(df$turnout2016)]), mean)
ageTurnoutSuccess <- tapply(df$turnout2016[!is.na(df$turnout2016)], list(df$canvass_treat[!is.na(df$turnout2016)],df$age_bin[!is.na(df$turnout2016)]),mean)
raceTurnoutSuccess <- tapply(df$turnout2016[!is.na(df$turnout2016)], list(df$canvass_treat[!is.na(df$turnout2016)], df$race_clean[!is.na(df$turnout2016)]),mean)

# Gender turnout Barplot

barplot(genderTurnoutSuccess, beside = TRUE, legend = rownames(genderTurnoutSuccess), xlab = "Gender", main = "Gender Turnout Success", ylab = "percentage")

# Age turnout barplot

barplot(ageTurnoutSuccess, beside = TRUE, legend = rownames(ageTurnoutSuccess), xlab = "Age", main = "Age Turnout Success", ylab = "percentage")

#Race turnout barplot

barplot(raceTurnoutSuccess, beside = TRUE, legend = rownames(raceTurnoutSuccess), xlab = "race", main = "Race Turnout Success", ylab = "percentage")

#