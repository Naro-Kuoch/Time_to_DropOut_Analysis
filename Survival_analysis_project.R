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
     xlab = "Time (semester)",
     ylab = "Probability of not having dropped out")

summary(km_all, times = c(2, 4, 6))