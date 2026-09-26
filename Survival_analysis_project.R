# install.packages("TimeDepFrail")   # run once only

library(TimeDepFrail)

data(data_dropout)

## Observation

dim(data_dropout)
head(data_dropout)
str(data_dropout)
summary(data_dropout)

# Convert time-to-event from text to numeric

data_dropout$time_to_event <- as.numeric(data_dropout$time_to_event)
str(data_dropout$time_to_event)
sum(is.na(data_dropout$time_to_event))

# Create event : Dropout vs Censored

data_dropout$event <- ifelse(data_dropout$time_to_event <= 6, 1, 0)

# Factor categorical variables

data_dropout$Gender <- factor(data_dropout$Gender)
data_dropout$group  <- factor(data_dropout$group)

# Check
sum(is.na(data_dropout))
table(data_dropout$event)
prop.table(table(data_dropout$event))


## Time until drop out

summary(data_dropout$time_to_event[data_dropout$event == 1])

hist(data_dropout$time_to_event[data_dropout$event == 1],
     breaks = seq(1, 6, by = 0.5),
     main = "Time of dropout",
     xlab = "Time (semester)",
     ylab = "Student count")

## Gender

table(data_dropout$Gender, data_dropout$event)
prop.table(table(data_dropout$Gender, data_dropout$event), margin = 1)


## CFUP

hist(data_dropout$CFUP, main = "1st semester credits (standardized)", xlab = "CFUP")

tapply(data_dropout$CFUP, data_dropout$event, summary)

boxplot(CFUP ~ event, data = data_dropout,
        names = c("Censored (0)", "Drop out (1)"),
        main = "CFUP by event",
        ylab = "CFUP")

## Group

table(data_dropout$group)

rate_group <- tapply(data_dropout$event, data_dropout$group, mean)
round(sort(rate_group), 3)

barplot(sort(rate_group),
        las = 2,
        main = "Dropout rate by group",
        ylab = "Dropout rate")


## Observations
# - 4448 students, no missing data. 680 dropouts (15.3%); the 3768 others are
#   censored at the end of follow-up (coded 6.1).
# - Dropouts occur early: median time 2.2 semesters, about one third
#   before the end of semester 2.
# - Gender: dropout proportion higher among men (16.2%) than women (11.9%).
# - CFUP: much lower among dropouts (median -1.66, the minimum value,
#   probably no credits) than among the others (median 0.28).
#   Likely the strongest factor.
# - Group: dropout proportion ranges from 8.7% (CosC) to 19.4% (CosK).
# - Caution: these are raw proportions. They ignore time and censoring,
#   and no statistical test has been done yet -> next step: Kaplan-Meier.



## Kaplan-Meier (all students)

library(survival)

km_all <- survfit(Surv(time_to_event, event) ~ 1, data = data_dropout)

km_all
summary(km_all)

plot(km_all,
     ylim = c(0.8, 1),          # Zoom on usefull zone
     lwd = 2,
     xlab = "Time (semester)",
     ylab = "Probability of not having dropped out",
     main = "Kaplan-Meier estimate, all students")

summary(km_all, times = c(2, 4, 6))


## Kaplan-Meier by Gender

km_gender <- survfit(Surv(time_to_event, event) ~ Gender, data = data_dropout)

km_gender                                # nombre d'étudiants et d'abandons par groupe
summary(km_gender, times = c(2, 4, 6))   # survie à 2, 4, 6 semestres, par groupe

plot(km_gender,
     col = c("darkorange", "darkcyan"),
     lwd = 2,
     ylim = c(0.8, 1),
     xlab = "Time (semester)",
     ylab = "Probability of not having dropped out",
     main = "Kaplan-Meier estimate by Gender")

legend("bottomleft",
       legend = levels(data_dropout$Gender),
       col = c("darkorange", "darkcyan"),
       lwd = 2)

## Log-rank by Gender

survdiff(Surv(time_to_event, event) ~ Gender, data = data_dropout)
#The probability of remaining enrolled differed between female and male students (log-rank test, χ² = 9.9, df = 1, p = 0.002), with fewer dropouts than expected among female students (111 observed vs 144 expected).


## Kaplan-Meier by group

km_group <- survfit(Surv(time_to_event, event) ~ group, data = data_dropout)

km_group
summary(km_group, times = 6)   # probability of not having dropped out at 6 semesters, by group

plot(km_group,
     col = 1:16,
     ylim = c(0.75, 1),
     xlab = "Time (semester)",
     ylab = "Probability of not having dropped out",
     main = "Kaplan-Meier estimate by group")
# 16 curves -> not readable in detail but to see the overall spread

## Log-rank by group

survdiff(Surv(time_to_event, event) ~ group, data = data_dropout)


## CFUP in classes (quartiles)

cfup_breaks <- quantile(data_dropout$CFUP, probs = c(0, 0.25, 0.5, 0.75, 1))
cfup_breaks

data_dropout$CFUP_class <- cut(data_dropout$CFUP,
                               breaks = cfup_breaks,
                               include.lowest = TRUE,
                               labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"))

table(data_dropout$CFUP_class)
# Classes are not of equal size : many students share the same CFUP value (665 students have the minimum value, probably no credits)

tapply(data_dropout$event, data_dropout$CFUP_class, mean)

## Kaplan-Meier by CFUP class

km_cfup <- survfit(Surv(time_to_event, event) ~ CFUP_class, data = data_dropout)

km_cfup
summary(km_cfup, times = c(2, 4, 6))

plot(km_cfup,
     col = c("darkorange", "darkcyan", "purple", "darkgreen"),
     lwd = 2,
     ylim = c(0.6, 1),
     xlab = "Time (semester)",
     ylab = "Probability of not having dropped out",
     main = "Kaplan-Meier estimate by CFUP class")

legend("bottomleft",
       legend = levels(data_dropout$CFUP_class),
       col = c("darkorange", "darkcyan", "purple", "darkgreen"),
       lwd = 2)

## Log-rank by CFUP class

survdiff(Surv(time_to_event, event) ~ CFUP_class, data = data_dropout)


## Stratified log-rank: Gender, stratified by group

# Why : the proportion of women varies a lot between groups
table(data_dropout$group, data_dropout$Gender)
round(prop.table(table(data_dropout$group, data_dropout$Gender), margin = 1), 2)

# If some groups have both more women and fewer dropouts, the Gender difference
# seen before could partly come from the group. The stratified test compares
# women and men within each group, then combines the results.
survdiff(Surv(time_to_event, event) ~ Gender + strata(group), data = data_dropout)


## Observations
# - group : no statistically significant difference between the 16 groups
#   (log-rank chi2 = 20, df = 15, p = 0.2). Probability of not having dropped
#   out at 6 semesters ranges from 0.81 (CosK) to 0.91 (CosC), but groups are
#   small and confidence intervals overlap. The differences seen in the raw
#   proportions are compatible with chance.
# - CFUP : strong gradient between classes (log-rank chi2 = 755, df = 3,
#   p < 0.001). Probability of not having dropped out at 6 semesters:
#   Q1 0.65, Q2 0.92, Q3 0.96, Q4 0.98. Q1 (lowest credits) accounts for
#   539 of the 680 dropouts (79%). Curves separate from the first semesters
#   and the gap keeps growing over time.
# - Gender stratified by group : result almost unchanged (chi2 = 10.8, df = 1,
#   p = 0.001 vs p = 0.002 without stratification). The difference between
#   women and men persists within groups: it is not explained by the group.