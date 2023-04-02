# Assignment: Mortality and Longevity of Companion Dogs
#           ACTL3141/ACTL5104, T1 2023

library(dplyr)
library(ggplot2)
library(moments)
library(readxl)
library(survival)
library(KMsurv)
library(km.ci)
library(RColorBrewer)
library(lifecontingencies)

source("lifetable.R")

df <- read_excel("the_dataset.xlsx")
summary(df)
#attach(df)

male_df <- filter(df, sex=="Male")
female_df <- filter(df, sex=="Female")
neutered_df <- filter(df, neuter=="Neutered")
entire_df <- filter(df, neuter=="Entire")
crossbred_df <- filter(df, pure_cross=="Crossbred")
purebred_df <- filter(df, pure_cross=="Purebred")
uninsured_df <- filter(df, insured=="Uninsured")
insured_df <- filter(df, insured=="Insured")

gundog_df <- filter(df, breed_group=="Gundog")
hound_df <- filter(df, breed_group=="Hound")
not_kc_recognised_df <- filter(df, breed_group=="Not_KC_Recognised")
pastoral_df <- filter(df, breed_group=="Pastoral")
terrier_df <- filter(df, breed_group=="Terrier")
toy_df <- filter(df, breed_group=="Toy")
utility_df <- filter(df, breed_group=="Utility")
working_df <- filter(df, breed_group=="Working")

# 1. Descriptive analysis of the profile of dogs in the data
set.seed(123)
trainIndex <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
train <- na.omit(df[trainIndex, ])
test <- na.omit(df[-trainIndex, ])

#formula = lifespan ~ sex + neuter + breed_vc + pure_cross + breed_group + insured,
model <- lm(formula = lifespan ~ sex + neuter + pure_cross + insured + breed_group, data = train)
summary(model)
predictions <- predict(model, newdata = test)
(rmse <- sqrt(mean(test$lifespan - predictions)^2))

ggplot(df, aes(x=lifespan)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=0.5) +
  geom_density(alpha=0.2, fill="lightblue") +
  geom_vline(aes(xintercept=mean(lifespan)), color="blue", linetype="dashed", size=0.5) +
  ggtitle("Histogram for Lifespan of UK Companion Dogs") +
  xlab("Lifespan in Years") + ylab("Density") + 
  theme(plot.title = element_text(hjust = 0.5))

plot_density_by <- function(df, category, cat) {
  p <- ggplot(df, aes(x = lifespan, color = category, fill = category)) +
    geom_density(alpha = 0.5) +
    scale_color_manual(values = brewer.pal(8, "Set1")) +
    scale_fill_manual(values = brewer.pal(8, "Set1")) +
    ggtitle(paste("Densities of Lifespan of UK Companion Dogs by", cat)) +
    xlab("Lifespan in Years") + ylab("Density") + 
    theme(plot.title = element_text(hjust = 0.5))
  return (p)
}

plot_density_by(df, df$sex, "Sex")
plot_density_by(df, df$neuter, "Neutering Status")
plot_density_by(na.omit(df), na.omit(df)$pure_cross, "Pure Cross")
plot_density_by(df, df$insured, "Insurance Status")
plot_density_by(na.omit(df), na.omit(df)$breed_group, "Breed Group")

dim(df)
df %>% count(sex)
df %>% count(neuter)
df %>% count(pure_cross)
df %>% count(insured)
df %>% count(breed_group)
df %>% count(breed_vc)

breed_vc <- df %>%
  group_by(breed_vc) %>%
  summarize(mean_value = mean(lifespan), count = n()) %>%
  filter(count > 10) %>%
  arrange(desc(mean_value))

(breed_vc_with_highest_mean_lifespan <- head(breed_vc, 1))
t.test(filter(df, breed_vc=="Tibetan Spaniel")$lifespan, conf.level=0.95)
median(filter(df, breed_vc=="Tibetan Spaniel")$lifespan)
IQR(filter(df, breed_vc=="Tibetan Spaniel")$lifespan)
(breed_vc_with_lowest_mean_lifespan <- tail(breed_vc, 1))
t.test(filter(df, breed_vc=="Cockapoo")$lifespan, conf.level=0.95)
median(filter(df, breed_vc=="Cockapoo")$lifespan)
IQR(filter(df, breed_vc=="Cockapoo")$lifespan)

t.test(df$lifespan, conf.level=0.95)
t.test(male_df$lifespan, conf.level=0.95)
t.test(female_df$lifespan, conf.level=0.95)
t.test(neutered_df$lifespan, conf.level=0.95)
t.test(entire_df$lifespan, conf.level=0.95)
t.test(crossbred_df$lifespan, conf.level=0.95)
t.test(purebred_df$lifespan, conf.level=0.95)
t.test(uninsured_df$lifespan, conf.level=0.95)
t.test(insured_df$lifespan, conf.level=0.95)
t.test(gundog_df$lifespan, conf.level=0.95)
t.test(hound_df$lifespan, conf.level=0.95)
t.test(not_kc_recognised_df$lifespan, conf.level=0.95)
t.test(pastoral_df$lifespan, conf.level=0.95)
t.test(terrier_df$lifespan, conf.level=0.95)
t.test(toy_df$lifespan, conf.level=0.95)
t.test(utility_df$lifespan, conf.level=0.95)
t.test(working_df$lifespan, conf.level=0.95)

IQR(df$lifespan)
IQR(male_df$lifespan)
IQR(female_df$lifespan)
IQR(neutered_df$lifespan)
IQR(entire_df$lifespan)
IQR(crossbred_df$lifespan)
IQR(purebred_df$lifespan)
IQR(uninsured_df$lifespan)
IQR(insured_df$lifespan)
IQR(gundog_df$lifespan)
IQR(hound_df$lifespan)
IQR(not_kc_recognised_df$lifespan)
IQR(pastoral_df$lifespan)
IQR(terrier_df$lifespan)
IQR(toy_df$lifespan)
IQR(utility_df$lifespan)
IQR(working_df$lifespan)
  
lifespan_by <- rbind(summary(df$lifespan),
                     summary(male_df$lifespan),
                     summary(female_df$lifespan),
                     summary(neutered_df$lifespan),
                     summary(entire_df$lifespan),
                     summary(crossbred_df$lifespan),
                     summary(purebred_df$lifespan),
                     summary(uninsured_df$lifespan), 
                     summary(insured_df$lifespan),
                     summary(gundog_df$lifespan),
                     summary(hound_df$lifespan),
                     summary(not_kc_recognised_df$lifespan),
                     summary(pastoral_df$lifespan),
                     summary(terrier_df$lifespan),
                     summary(toy_df$lifespan),
                     summary(utility_df$lifespan), 
                     summary(working_df$lifespan))
rownames(lifespan_by) <- c("Population", "Male", "Female", "Neutered", "Entire", "Crossbred", "Purebred", "Uninsured", "Insured",
                           "Gundog", "Hound", "Unrecognised", "Pastoral", "Terrier", "Toy", "Utility", "Working")
lifespan_by

#skewness(filter(df, sex=="Male")$lifespan)
#kurtosis(filter(df, sex=="Male")$lifespan)
wilcox.test(male_df$lifespan, female_df$lifespan)
wilcox.test(neutered_df$lifespan, entire_df$lifespan)
wilcox.test(crossbred_df$lifespan, purebred_df$lifespan)
wilcox.test(uninsured_df$lifespan, insured_df$lifespan)

# 2. Survival Analysis
# Use KM, NA and Cox regression to analyse the mortality and survival of dogs.
# Study the differences in dog mortality by 
# 1. age
# 2. sex
# 3. breed
# 4. neutering status
# 5. insured status

surv.obj <- Surv(na.omit(df)$lifespan)
# Kaplan-Meier estimate
km.dogs <- survfit(surv.obj~1, conf.int = 0.95, conf.type = "log")
summary(km.dogs)
plot(km.dogs, main = "KM estimate with 95% confidence intervals",
     xlab = "Lifespan (in years)", ylab = "Survival function", col = "blue")
plot(km.dogs$time, -log(km.dogs$surv), xlab = "time", ylab = "cumulative hazard",
     main = "cumulative hazards", type = "s")

km.sex <- survfit(surv.obj~na.omit(df)$sex, conf.int = 0.95, conf.type = "log")
summary(km.sex)
plot(km.sex, main = "KM estimates for different sexs", col = c("blue", "red"),
     xlab = "Lifespan (in years)", ylab = "Survival function")
legend("topright", c("Female", "Male"), lty = c("solid", "solid"), col = c("blue", "red"))

km.neuter <- survfit(surv.obj~na.omit(df)$neuter, conf.int = 0.95, conf.type = "log")
summary(km.neuter)
plot(km.neuter, main = "KM estimates for different neutering status", col = c("blue", "red"),
     xlab = "Lifespan (in years)", ylab = "Survival function")
legend("topright", c("Entire", "Neutered"), lty = c("solid", "solid"), col = c("blue", "red"))

km.pure_cross <- survfit(surv.obj~na.omit(df)$pure_cross, conf.int = 0.95, conf.type = "log")
summary(km.pure_cross)
plot(km.pure_cross, main = "KM estimates for pure/cross bred", col = c("blue", "red"),
     xlab = "Lifespan (in years)", ylab = "Survival function")
legend("topright", c("Crossbred", "Purebred"), lty = c("solid", "solid"), col = c("blue", "red"))

km.insured <- survfit(surv.obj~na.omit(df)$insured, conf.int = 0.95, conf.type = "log")
summary(km.insured)
plot(km.insured, main = "KM estimates for different insurance status", col = c("blue", "red"),
     xlab = "Lifespan (in years)", ylab = "Survival function")
legend("topright", c("Insured", "Uninsured"), lty = c("solid", "solid"), col = c("blue", "red"))

km.breed_groups <- survfit(surv.obj~na.omit(df)$breed_group, conf.int = 0.95, conf.type = "log")
summary(km.breed_groups)
plot(km.breed_groups, main = "KM estimates for different breed groups", col = brewer.pal(8, "Set1"),
     xlab = "Lifespan (in years)", ylab = "Survival function")
legend("topright", c("Gundog", "Hound", "Unrecognised", "Pastoral", "Terrier", "Toy", "Utility", "Working"), 
       lty = rep("solid", 8), col = brewer.pal(8, "Set1"))

# Nelson-Aalen estimator
na.dogs <- survfit(surv.obj~1, conf.int = 0.95, conf.type = "log", type = "fh")
plot(na.dogs, main = "NA estimate with 95% confidence intervals",
     xlab = "Lifespan (in years)", ylab = "Survival function")

# Cox Proportional Hazard Model
#  + as.factor(breed_vc) + as.factor(pure_cross) + as.factor(breed_group)
cox.dogs <- coxph(surv.obj ~ as.factor(na.omit(df)$sex) + as.factor(na.omit(df)$pure_cross) + as.factor(na.omit(df)$insured) + as.factor(na.omit(df)$neuter) + as.factor(na.omit(df)$breed_group), method = "breslow")
print(cox.zph(cox.dogs)) # test the proportional hazards assumption for a Cox regression model fit

(sex.logrank <- survdiff(surv.obj ~ as.factor(na.omit(df)$sex), rho = 0))
(pure_cross.logrank <- survdiff(surv.obj ~ as.factor(na.omit(df)$pure_cross), rho = 0))
(neuter.logrank <- survdiff(surv.obj ~ as.factor(na.omit(df)$neuter), rho = 0))
(insured.logrank <- survdiff(surv.obj ~ as.factor(na.omit(df)$insured), rho = 0))
(breed_group.logrank <- survdiff(surv.obj ~ as.factor(na.omit(df)$breed_group), rho = 0))

# 3. Life tables for insured and uninsured dogs
#LT.insured <- lifetable_ci(insured_df, size = nrow(insured_df), times = 10, top = 21)
LT.insured <- lifetable_0(insured_df)
LT.uninsured <- lifetable_ci(uninsured_df, size = nrow(uninsured_df), times = 10)

# insured_lifespans <- sort(insured_df$lifespan)
# max_insured_lifespan <- ceiling(max(insured_lifespans))
# insured_age_intervals <- paste0(seq(0, max_insured_lifespan-1), "-", seq(1, max_insured_lifespan))
# 
# insured_num_alive <- sapply(0:(max_insured_lifespan-1), function(x) sum(insured_lifespans >= x))
# insured_num_deaths <- insured_num_alive - c(insured_num_alive[-1],0)
# insured_death_probs <- insured_num_deaths / insured_num_alive
# 
# insured_lifetable <- data.frame(Age_Interval = insured_age_intervals,
#                                 Num_Deaths = insured_num_deaths,
#                                 Num_Alive = insured_num_alive,
#                                 Prob_Dying = insured_death_probs)
# 
# insured_lt <- new("lifetable", x = seq(0, max_insured_lifespan-1),
#                   lx = insured_num_alive,
#                   name = "Lifetable for Insured Dogs")
# 
# LT.insured <- probs2lifetable(probs=insured_death_probs, type="qx",
#                               radix=5188, name = "Lifetable for Insured Dogs")
# plot(LT.insured, type="l")
# summary(LT.insured)
