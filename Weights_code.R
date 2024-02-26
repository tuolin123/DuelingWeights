library(weights)
library(Hmisc)
library(psych)
library(sfsmisc)
library(spatstat)
library(survey)
library(permute)
library(tidyverse)
library(dplyr)
library(geepack)
library(designmatch)
setwd("../nhanes")
demo <- data.frame(read.csv("demographic.csv"))
diet <- data.frame(read.csv("diet.csv"))
exam <- data.frame(read.csv("examination.csv"))
quest <- data.frame(read.csv("questionnaire.csv"))
lab <- data.frame(read.csv("labs.csv"))
nhanes <- Reduce(function(...) merge(...,by='SEQN', all=TRUE), 
                 list(demo, diet, exam, quest, lab))

#BMI: nhanes$BMXBMI
#Diet: nhanes$DRQSDIET
#weight: nhanes$WTDRD1 
# LBXSCU: copper, MCQ053: anaemia
# Age: nhanes$RIDAGEYR
# Gender: nhanes$RIAGENDR
nhanes.comp <- nhanes %>%
  filter(WTDRD1 != 0) %>%
  select(LBXSCU, MCQ053, RIAGENDR, RIDAGEYR, WTDRD1) %>% 
  drop_na %>%
  filter(MCQ053 != 9) %>%
  filter(MCQ053 != 7) %>%
  mutate(anemia = ifelse(MCQ053 == 1, 1, 0)) %>%
  mutate(RIAGENDR = as.factor(RIAGENDR))

## weighted Logistic regression
fit = glm(anemia ~ LBXSCU + RIAGENDR + RIDAGEYR, family=binomial,
          data=nhanes.comp, weights = WTDRD1/min(nhanes.comp$WTDRD1))

fit1 = geeglm(anemia ~ LBXSCU + RIAGENDR + RIDAGEYR, family = binomial,
              data = nhanes.comp, id = 1:nrow(nhanes.comp),
              weights = WTDRD1/min(nhanes.comp$WTDRD1)) # table 2

## survey weights
nhanes.comp$w = nhanes.comp$WTDRD1/min(nhanes.comp$WTDRD1)
datsvy <- svydesign(id=~1,weights=~w, data=nhanes.comp)
fit2 = svyglm(anemia ~ LBXSCU + RIAGENDR + RIDAGEYR, design = datsvy,
              family = binomial)

## weight = 1
fit_w1 = glm(anemia ~ LBXSCU + RIAGENDR + RIDAGEYR, family=binomial,
          data=nhanes.comp) # table 3

fit1_w1 = geeglm(anemia ~ LBXSCU + RIAGENDR + RIDAGEYR, family = binomial,
              data = nhanes.comp, id = 1:nrow(nhanes.comp)) # table 3

## aggregate the data
LBXSCU_break = quantile(nhanes.comp$LBXSCU,c(0.2, 0.4, 0.6, 0.8))
nhanes.comp.cat = nhanes.comp %>%
  mutate(LBXSCU_cat = cut(LBXSCU, breaks=c(-Inf, LBXSCU_break, Inf),
                          label = 1:5)) 

nhanes.comp.cat$type = as.factor(interaction(nhanes.comp.cat$LBXSCU_cat,
                                              nhanes.comp.cat$RIAGENDR))

fit3 = glm(anemia ~ type, family=binomial,
           data=nhanes.comp.cat) # table 4 GLM

nhanes.comp.agg = nhanes.comp.cat %>%
  group_by(type) %>%
  summarise(case = sum(anemia),
            tot = length(anemia),
            LBXSCU = mean(as.numeric(LBXSCU_cat)),
            RIAGENDR = mean(as.numeric(RIAGENDR)))

fit4 = glm(case/tot ~ type, family=binomial,
           data=nhanes.comp.agg, weights=tot) # table 4 WGLM

datsvy <- svydesign(id=~1,weights=~tot, data=nhanes.comp.agg)
fit5 = svyglm(case/tot ~ type, design = datsvy,
              family = binomial)  # table 4 WEE

fit5 = geeglm(case/tot ~ type, family = binomial, weights = tot, 
              data = nhanes.comp.agg, id = 1:nrow(nhanes.comp.agg)) # table 4 WEE

############# Observational Study #################
## table 5
data(lalonde)

colnames(lalonde)

ps <- glm(treatment ~ age + education + nodegree + re74 + re75,
         data = lalonde, family = binomial())

lalonde$ps <- predict(ps, type = "response")

lalonde$weight <- ifelse(lalonde$treatment == 1, 1/lalonde$ps, 1/(1-lalonde$ps))

fit_lm <- lm(re78 ~ treatment + black + hispanic + married, data = lalonde, weights = weight)

fit_slm <- geeglm(re78 ~ treatment + black + hispanic + married, data = lalonde,
                  id = 1:nrow(lalonde), weights = weight)
