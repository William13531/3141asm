# Assignment: Mortality and Longevity of Companion Dogs
#           ACTL3141/ACTL5104, T1 2023

library(dplyr)
library(ggplot2)
library(moments)
library("readxl")
library(survival)
library("KMsurv")
library("km.ci")

df <- read_excel("the_dataset.xlsx")
summary(df)

# 1. Descriptive analysis of the profile of dogs in the data
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
t.test(filter(df, sex=="Male")$lifespan, conf.level=0.95)
t.test(filter(df, sex=="Female")$lifespan, conf.level=0.95)
t.test(filter(df, neuter=="Neutered")$lifespan, conf.level=0.95)
t.test(filter(df, neuter=="Entire")$lifespan, conf.level=0.95)
t.test(filter(df, pure_cross=="Crossbred")$lifespan, conf.level=0.95)
t.test(filter(df, pure_cross=="Purebred")$lifespan, conf.level=0.95)
t.test(filter(df, insured=="Uninsured")$lifespan, conf.level=0.95)
t.test(filter(df, insured=="Insured")$lifespan, conf.level=0.95)
  
lifespan_by <- rbind(summary(df$lifespan),
                     summary(filter(df, sex=="Male")$lifespan),
                     summary(filter(df, sex=="Female")$lifespan),
                     summary(filter(df, neuter=="Neutered")$lifespan),
                     summary(filter(df, neuter=="Entire")$lifespan),
                     summary(filter(df, pure_cross=="Crossbred")$lifespan),
                     summary(filter(df, pure_cross=="Purebred")$lifespan),
                     summary(filter(df, insured=="Uninsured")$lifespan), 
                     summary(filter(df, insured=="Insured")$lifespan))
rownames(lifespan_by) <- c("Population", "Male", "Female", "Neutered", "Entire", "Crossbred", "Purebred", "Uninsured", "Insured")
lifespan_by

#skewness(filter(df, sex=="Male")$lifespan)
#kurtosis(filter(df, sex=="Male")$lifespan)
wilcox.test(filter(df, sex=="Male")$lifespan, filter(df, sex=="Female")$lifespan)
wilcox.test(filter(df, neuter=="Neutered")$lifespan, filter(df, neuter=="Entire")$lifespan)
wilcox.test(filter(df, pure_cross=="Crossbred")$lifespan, filter(df, pure_cross=="Purebred")$lifespan)
wilcox.test(filter(df, insured=="Uninsured")$lifespan, filter(df, insured=="Insured")$lifespan)

# 2. Survival Analysis
# Use KM and Cox regression to analyse the mortality and survival of dogs.
# Study the differences in dog mortality by 
# 1. age
# 2. sex
# 3. breed
# 4. neutering status
# 5. insured status


