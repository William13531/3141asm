# Assignment: Mortality and Longevity of Companion Dogs
#           ACTL3141/ACTL5104, T1 2023

library(dplyr)
library(ggplot2)
library(moments)
library(readxl)
library(survival)
library(KMsurv)
library(km.ci)

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

# 1. Descriptive analysis of the profile of dogs in the data
set.seed(123)
trainIndex <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
train <- na.omit(df[trainIndex, ])
test <- na.omit(df[-trainIndex, ])

#formula = lifespan ~ sex + neuter + breed_vc + pure_cross + breed_group + insured,
model <- lm(formula = lifespan ~ sex + neuter + pure_cross + insured, data = train)
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
    scale_color_manual(values = c("blue", "red", "orange")) +
    scale_fill_manual(values = c("blue", "red", "orange")) +
    ggtitle(paste("Densities of Lifespan of UK Companion Dogs by", cat)) +
    xlab("Lifespan in Years") + ylab("Density") + 
    theme(plot.title = element_text(hjust = 0.5))
  return (p)
}

plot_density_by(df, df$sex, "Sex")
plot_density_by(df, df$neuter, "Neutering Status")
plot_density_by(na.omit(df), na.omit(df)$pure_cross, "Breed")
plot_density_by(df, df$insured, "Insurance Status")

dim(df)
df %>% count(sex)
df %>% count(neuter)
df %>% count(pure_cross)
df %>% count(insured)

t.test(df$lifespan, conf.level=0.95)
t.test(male_df$lifespan, conf.level=0.95)
t.test(female_df$lifespan, conf.level=0.95)
t.test(neutered_df$lifespan, conf.level=0.95)
t.test(entire_df$lifespan, conf.level=0.95)
t.test(crossbred_df$lifespan, conf.level=0.95)
t.test(purebred_df$lifespan, conf.level=0.95)
t.test(uninsured_df$lifespan, conf.level=0.95)
t.test(insured_df$lifespan, conf.level=0.95)

IQR(df$lifespan)
IQR(male_df$lifespan)
IQR(female_df$lifespan)
IQR(neutered_df$lifespan)
IQR(entire_df$lifespan)
IQR(crossbred_df$lifespan)
IQR(purebred_df$lifespan)
IQR(uninsured_df$lifespan)
IQR(insured_df$lifespan)
  
lifespan_by <- rbind(summary(df$lifespan),
                     summary(male_df$lifespan),
                     summary(female_df$lifespan),
                     summary(neutered_df$lifespan),
                     summary(entire_df$lifespan),
                     summary(crossbred_df$lifespan),
                     summary(purebred_df$lifespan),
                     summary(uninsured_df$lifespan), 
                     summary(insured_df$lifespan))
rownames(lifespan_by) <- c("Population", "Male", "Female", "Neutered", "Entire", "Crossbred", "Purebred", "Uninsured", "Insured")
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

surv.obj <- Surv(df$lifespan)
# Kaplan-Meier estimate
km.dogs <- survfit(surv.obj~1, conf.int = 0.95, conf.type = "log")
plot(km.dogs, main = "KM estimate with 95% confidence intervals",
     xlab = "Lifespan (in years)", ylab = "Survival function", col = "blue")
plot(km.dogs$time, -log(km.dogs$surv), xlab = "time", ylab = "cumulative hazard",
     main = "cumulative hazards", type = "s")

# Nelson-Aalen estimator
na.dogs <- survfit(surv.obj~1, conf.int = 0.95, conf.type = "log", type = "fh")
plot(na.dogs, main = "NA estimate with 95% confidence intervals",
     xlab = "Lifespan (in years)", ylab = "Survival function")

# Cox Proportional Hazard Model
#  + as.factor(breed_vc) + as.factor(pure_cross) + as.factor(breed_group)
cox.dogs <- coxph(surv.obj ~ as.factor(sex) + as.factor(pure_cross) + as.factor(insured) + as.factor(neuter), method = "breslow")
print(cox.zph(cox.dogs)) # test the proportional hazards assumption for a Cox regression model fit

(sex.logrank <- survdiff(surv.obj ~ as.factor(df$sex), rho = 0))
(pure_cross.logrank <- survdiff(surv.obj ~ as.factor(df$pure_cross), rho = 0))
(neuter.logrank <- survdiff(surv.obj ~ as.factor(df$neuter), rho = 0))
(insured.logrank <- survdiff(surv.obj ~ as.factor(df$insured), rho = 0))

