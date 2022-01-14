#Social support, personality and (mal)adaptive
#analysis after data wrangling, cleaning and multiple imputation imputation
#load packages
library(readxl)
library(mice)
library(miceadds)
library(MKmisc)
library(tidyverse)
library(yarrr)
library(kableExtra)
#load subsetted mids object
data_list2 <- readRDS("dataimp2.RDs")
dataimp2 <- miceadds::datlist2mids(data_list2)
dataComplete <- complete(dataimp2, "long")


####correlations for study 1#### (pooled)
dataComplete <- complete(dataimp2, "long")
dataComplete <- dataComplete[,-c(1:2)]
match(c("age", "diagnosis_year", "adaptiveCoping", "maladaptiveCoping", 
        "familySocialSupport", "friendSocialSupport", "importantPersonsSocialSupport", "social_support_medical",
        "optimism", "pesimism", "neuroticism",
        "pain", "sadness"), names(dataComplete))
miceadds::micombine.cor(dataimp2, 
                        variables = c(2,3,4,5,6,7,8,9,10,11,12,13,14), 
                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

#comparing groups
library(MKmisc)
mi.t.test(data_list2, x = "adaptiveCoping", y = "gender", var.equal = FALSE)
mi.t.test(data_list2, x = "adaptiveCoping", y = "relaps", var.equal = FALSE)
mi.t.test(data_list2, x = "adaptiveCoping", y = "cancerSupportGroup", var.equal = FALSE)
mi.t.test(data_list2, x = "maladaptiveCoping", y = "gender", var.equal = FALSE)
mi.t.test(data_list2, x = "maladaptiveCoping", y = "relaps", var.equal = FALSE)
mi.t.test(data_list2, x = "maladaptiveCoping", y = "cancerSupportGroup", var.equal = FALSE)
miceadds::mi.anova(dataimp2, "adaptiveCoping ~ educationGroup")
miceadds::mi.anova(dataimp2, "maladaptiveCoping ~ educationGroup")

####multiple hierarchical linear regression for study1#### (pooled)
####adaptive coping as a criterion
#block1
block1a <- with(dataimp2, stats::lm(adaptiveCoping ~ age + gender + educationGroup))
a1 <- summary(mice::pool(block1a))
a1 %>% 
  kbl() %>% 
  kable_material()

pool.r.squared(block1a)
####standardized betas and standard errors
block1as <- with(dataimp2, stats::lm(scale(adaptiveCoping) ~ scale(age) + gender + educationGroup))
as1 <- summary(mice::pool(block1as))
as1 %>% 
  kbl() %>% 
  kable_material()
#block2
block2a <- with(dataimp2, stats::lm(adaptiveCoping ~ age + gender + educationGroup  
                                    + diagnosis_year + relaps + pain + sadness))
a2 <- summary(mice::pool(block2a))
a2 %>% 
  kbl() %>% 
  kable_material()

pool.r.squared(block2a)
####standardized betas and standard errors
block2as <- with(dataimp2, stats::lm(scale(adaptiveCoping) ~ scale(age) + gender + educationGroup  
                                     + scale(diagnosis_year) + relaps + scale(pain) + scale(sadness)))
as2 <- summary(mice::pool(block2as))
as2 %>% 
  kbl() %>% 
  kable_material()
#block3
block3a <- with(dataimp2, stats::lm(adaptiveCoping ~ age + gender + educationGroup 
                                    + diagnosis_year + relaps + pain + sadness 
                                    + familySocialSupport + friendSocialSupport + importantPersonsSocialSupport + social_support_medical + cancerSupportGroup))
a3 <- summary(mice::pool(block3a))
a3 %>% 
  kbl() %>% 
  kable_material()

pool.r.squared(block3a)
####standardized betas and standard errors
block3as <- with(dataimp2, stats::lm(scale(adaptiveCoping) ~ scale(age) + gender + educationGroup 
                                     + scale(diagnosis_year) + relaps + scale(pain) + scale(sadness) 
                                     + scale(familySocialSupport) + scale(friendSocialSupport) + scale(importantPersonsSocialSupport) + scale(social_support_medical) + cancerSupportGroup))
as3 <- summary(mice::pool(block3as))
as3 %>% 
  kbl() %>% 
  kable_material()
#block4
block4a <- with(dataimp2, stats::lm(adaptiveCoping ~ age + gender + educationGroup  
                                    + diagnosis_year + relaps + pain + sadness 
                                    + familySocialSupport + friendSocialSupport + importantPersonsSocialSupport + social_support_medical + cancerSupportGroup
                                    + neuroticism + pesimism + optimism))
a4 <- summary(mice::pool(block4a))
a4 %>% 
  kbl() %>% 
  kable_material()

pool.r.squared(block4a)
####standardized betas and standard errors
block4as <- with(dataimp2, stats::lm(scale(adaptiveCoping) ~ scale(age) + gender + educationGroup  
                                     + scale(diagnosis_year) + relaps + scale(pain) + scale(sadness) 
                                     + scale(familySocialSupport) + scale(friendSocialSupport) + scale(importantPersonsSocialSupport) + scale(social_support_medical) + cancerSupportGroup
                                     + scale(neuroticism) + scale(pesimism) + scale(optimism)))
as4 <- summary(mice::pool(block4as))
as4 %>% 
  kbl() %>% 
  kable_material()

anova(block1a,block2a,block3a,block4a) #comparing models

####maladaptive coping as a criterion
block1b <- with(dataimp2, stats::lm(maladaptiveCoping ~ age + gender + educationGroup))
b1 <- summary(mice::pool(block1b))
b1 %>% 
  kbl() %>% 
  kable_material()
pool.r.squared(block1b)
####standardized betas and standard errors
block1bs <- with(dataimp2, stats::lm(scale(maladaptiveCoping) ~ scale(age) + gender + educationGroup))
bs1 <- summary(mice::pool(block1bs))
bs1 %>% 
  kbl() %>% 
  kable_material()
#block2
block2b <- with(dataimp2, stats::lm(maladaptiveCoping ~ age + gender + educationGroup  
                                    + diagnosis_year + relaps + pain + sadness))
b2 <- summary(mice::pool(block2b))
b2 %>% 
  kbl() %>% 
  kable_material()
pool.r.squared(block2b)
####standardized betas and standard errors
block2bs <- with(dataimp2, stats::lm(scale(maladaptiveCoping) ~ scale(age) + gender + educationGroup  
                                     + scale(diagnosis_year) + relaps + scale(pain) + scale(sadness)))
bs2 <- summary(mice::pool(block2bs))
bs2 %>% 
  kbl() %>% 
  kable_material()
#block3
block3b <- with(dataimp2, stats::lm(maladaptiveCoping ~ age + gender + educationGroup  
                                    + diagnosis_year + relaps + pain + sadness 
                                    + familySocialSupport + friendSocialSupport + importantPersonsSocialSupport + social_support_medical + cancerSupportGroup))
b3 <- summary(mice::pool(block3b))
b3 %>% 
  kbl() %>% 
  kable_material()
pool.r.squared(block3b)

####standardized betas and standard errors
block3bs <- with(dataimp2, stats::lm(scale(maladaptiveCoping) ~ scale(age) + gender + educationGroup 
                                     + scale(diagnosis_year) + relaps + scale(pain) + scale(sadness) 
                                     + scale(familySocialSupport) + scale(friendSocialSupport) + scale(importantPersonsSocialSupport) + scale(social_support_medical) + cancerSupportGroup))
bs3 <- summary(mice::pool(block3bs))
bs3 %>% 
  kbl() %>% 
  kable_material()
#block4
block4b <- with(dataimp2, stats::lm(maladaptiveCoping ~ age + gender + educationGroup  
                                    + diagnosis_year + relaps + pain + sadness 
                                    + familySocialSupport + friendSocialSupport + importantPersonsSocialSupport + social_support_medical + cancerSupportGroup
                                    + neuroticism + pesimism + optimism))
b4 <- summary(mice::pool(block4b))
b4 %>% 
  kbl() %>% 
  kable_material()
pool.r.squared(block4b)
####standardized betas and standard errors
block4bs <- with(dataimp2, stats::lm(scale(maladaptiveCoping) ~ scale(age) + gender + educationGroup  
                                     + scale(diagnosis_year) + relaps + scale(pain) + scale(sadness) 
                                     + scale(familySocialSupport) + scale(friendSocialSupport) + scale(importantPersonsSocialSupport) + scale(social_support_medical) + cancerSupportGroup
                                     + scale(neuroticism) + scale(pesimism) + scale(optimism)))
bs4 <- summary(mice::pool(block4bs))
bs4 %>% 
  kbl() %>% 
  kable_material()

anova(block1b,block2b,block3b,block4b) #model comparison

#####Supplementary figures for study------------------------------------------
dataSupplementaryFigures <- read_csv("dataSupplementaryFigures.csv")
###ADAPTIVE COPING###

pdf("SupplementaryFigures.pdf",height=10,width=16)
#relaps of cancer
yarrr::pirateplot(adaptiveCoping ~ relaps, 
                  data = dataSupplementaryFigures, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Adaptive coping", xlab = "relapse of cancer",
                  main = "Adaptive coping in group of patients with and without cancer relapse")
#cancer support group
yarrr::pirateplot(adaptiveCoping ~ cancerSupportGroup, 
                  data = dataSupplementaryFigures, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Adaptive coping", xlab = "Attendance of cancer support group",
                  main = "Adaptive coping in group of patients with and without attendance of cancer support group")
#age of patients
ggplot(dataSupplementaryFigures, aes(x = age, y = adaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Adaptive coping") +
  xlab("Age of patient") +
  ggtitle("Adaptive coping with cancer in the context of age of patients") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#time since cancer being diagnosed
ggplot(dataSupplementaryFigures, aes(x = diagnosis_year, y = adaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Adaptive coping") +
  xlab("Time since being diagnosed by cancer (in months)") +
  ggtitle("Adaptive coping with cancer in the context of time since a cancer being diagnosed") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#optimism
ggplot(dataSupplementaryFigures, aes(x = optimism, y = adaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Adaptive coping") +
  xlab("Optimism") +
  ggtitle("Adaptive coping with cancer in the context of optimism of patients") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))


###MALADAPTIVE COPING###
#relaps of cancer
yarrr::pirateplot(maladaptiveCoping ~ relaps, 
                  data = dataSupplementaryFigures, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Maladaptive coping", xlab = "relapse of cancer",
                  main = "Maladaptive coping in group of patients with and without cancer relapse")

#cancer support group
yarrr::pirateplot(maladaptiveCoping ~ cancerSupportGroup, 
                  data = dataSupplementaryFigures, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Maladaptive coping", xlab = "Attendance of cancer support group",
                  main = "Maladaptive coping in group of patients with and without attendance of cancer support group")

#education
yarrr::pirateplot(maladaptiveCoping ~ educationGroup, 
                  data = dataSupplementaryFigures, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Maladaptive coping", xlab = "Level of education",
                  main = "Maladaptive coping in group based on different education level")

#age of patients
ggplot(dataSupplementaryFigures, aes(x = age, y = maladaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Maladaptive coping") +
  xlab("Age of patient") +
  ggtitle("Maladaptive coping with cancer in the context of age of patients") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#time since cancer being diagnosed
ggplot(dataSupplementaryFigures, aes(x = diagnosis_year, y = maladaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Maladaptive coping") +
  xlab("Time since being diagnosed by cancer (in months)") +
  ggtitle("Maladaptive coping with cancer in the context of time since a cancer being diagnosed") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#social support from friends
ggplot(dataSupplementaryFigures, aes(x = friendSocialSupport, y = maladaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Maladaptive coping") +
  xlab("Social support from the friends") +
  ggtitle("Maladaptive coping with cancer in the context of social support from the friends of patients") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#social support from medical staff
ggplot(dataSupplementaryFigures, aes(x = social_support_medical, y = maladaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Maladaptive coping") +
  xlab("Social support from the medical staff") +
  ggtitle("Maladaptive coping with cancer in the context of social support from the medical staff of patients") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#pain
ggplot(dataSupplementaryFigures, aes(x = pain, y = maladaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Maladaptive coping") +
  xlab("Intensity of pain") +
  ggtitle("Maladaptive coping with cancer in the context of pain of patients") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#sadness
ggplot(dataSupplementaryFigures, aes(x = sadness, y = maladaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Maladaptive coping") +
  xlab("Intensity of sadness") +
  ggtitle("Maladaptive coping with cancer in the context of sadness of patients") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#pesimism
ggplot(dataSupplementaryFigures, aes(x = pesimism, y = maladaptiveCoping)) +
  geom_point() +
  stat_smooth() +
  ylab("Maladaptive coping") +
  xlab("Pesimism") +
  ggtitle("Maladaptive coping with cancer in the context of pesimism of patients") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

#clear plots
dev.off()


#social support variables variance distribution
p1 <- ggplot(data=dataComplete, aes(dataComplete$familySocialSupport)) + 
  geom_histogram(col="red", 
                 fill="green", 
                 alpha=.2)

p2 <- ggplot(data=dataComplete, aes(dataComplete$friendSocialSupport)) + 
  geom_histogram(col="red", 
                 fill="green", 
                 alpha=.2)

p3 <- ggplot(data=dataComplete, aes(dataComplete$importantPersonsSocialSupport)) + 
  geom_histogram(col="red", 
                 fill="green", 
                 alpha=.2)

p4 <- ggplot(data=dataComplete, aes(dataComplete$social_support_medical)) + 
  geom_histogram(col="red", 
                 fill="green", 
                 alpha=.2)

library(patchwork)
(p1+p2)/(p3+p4)


