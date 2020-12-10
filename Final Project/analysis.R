# Stats 506, F20
# Final Project
# 
# This scripts contains code to load, merge, and clean data downloaded from 
#   NHANES; 2 days' dietary recall data on nutrient intakes and foods consumed, 
#   demographic data on the participants, health and nutrition examination 
#   survey on the participants, the answers to a diet behavior questionnaire, 
#   the answers to a early childhood questionnaire, and answers to a 
#   questionnaire on physical activity.
#
# 
#
# Author: Chuwen Li (chuwenli@umich.edu).
# Updated: Dec 5, 2020
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(SASxport) 
library(tidyverse)
library(survey)
library(sjPlot)

# load data: ------------------------------------------------------------------
## 2017 - 2018 

Demographic_data = read.xport('./Data/DEMO_J.XPT')
BMI_data = read.xport('./Data/BMX_J.XPT')
Brefed_data = read.xport('./Data/DBQ_J.XPT')
PhyAct_data = read.xport('./Data/PAQY_J.XPT')
Birthwt_data = read.xport('./Data/ECQ_J.XPT')


# data cleaning: --------------------------------------------------------------
## join
df = left_join(
  Brefed_data %>% 
    dplyr::mutate(DBD030 = replace_na(DBD030, 0)) %>% 
    dplyr::select('id'= SEQN, 'breastfed' = DBQ010, 
                  'breastfed_length' = DBD030),
  Demographic_data %>% 
    dplyr::select('id'= SEQN, 'age' = RIDAGEYR, 'sex' = RIAGENDR,
                  'ethnicity' = RIDRETH3, 'wtmec2yr' = WTMEC2YR,
                  'psu' = SDMVPSU, 'strata' = SDMVSTRA), by = 'id') %>%
  left_join(., BMI_data %>% 
              dplyr::select('id'= SEQN, 'BMI' = BMXBMI), by = 'id') %>% 
  left_join(., PhyAct_data %>% 
              dplyr::select('id'= SEQN, 'activity_days' = PAQ706), 
            by = 'id') %>% 
  left_join(., Birthwt_data %>% 
              dplyr::select('id'= SEQN, 'birth_wt' = ECD070A,
                            'mom_smoked' = ECQ020), 
            by = 'id') %>%
  filter(breastfed == 1 | breastfed == 2) %>% 
  filter(age >= 3 & activity_days < 10 & birth_wt != 9999) %>% 
  drop_na()

## Set factor variables
df = df %>% 
  mutate(breastfed = ifelse(breastfed == 1, 'Yes', 'No'),
         sex = ifelse(sex == 2, 'Female', 'Male'),
         mom_smoked = ifelse(mom_smoked == 1, 'Yes', 'No'),
         ethnicity = factor(ethnicity, labels = c(
           'MexicanAmerican', 'Hispanic', 'White',
           'AfricanAmerican', 'Asian', 'Other')),
         activity_days = as.factor(activity_days)
         ) 


# EDA: ------------------------------------------------------------------------
svy = svydesign(id = ~psu, strat = ~strata, weights = ~wtmec2yr,
          nest = TRUE, data = df)

## between age
age_bmi = svyby(~BMI,~breastfed + age, design = svy, svymean, 
           vartype = 'ci', na.rm = TRUE)

age_bmi = age_bmi %>% 
  mutate(n = svytotal(~interaction(breastfed, age),
                      svy, na.rm = TRUE)) %>% 
  left_join(., df %>% 
              group_by(age, breastfed) %>% 
              summarise(obs = n(), .groups = 'drop'),
            by = c('breastfed', 'age'))

p1 = age_bmi %>% 
  ggplot( aes(x = factor(age), y = BMI, group = breastfed)) +
  geom_bar(aes(fill = breastfed), stat = 'identity',
           position = position_dodge(0.9)) +
  geom_errorbar( aes(ymin = ci_l, ymax = ci_u), width = 0.2, 
                position = position_dodge(0.9)) + 
  scale_fill_brewer(
    palette='Pastel1',
    name = 'Breast Fed') +
  theme_bw() +
  ylab(expression ('BMI'~(kg/m^2))) + 
  xlab('Age (year)')

### between sex
sex_bmi = svyby(~BMI,~breastfed + sex, design = svy, svymean, 
                vartype = 'ci', na.rm = TRUE)
p2 = sex_bmi %>% 
  ggplot( aes(x = sex, y = BMI, group = breastfed)) +
  geom_col(aes(fill = breastfed),
           position = position_dodge(0.9)) +
  geom_errorbar( aes(ymin = ci_l, ymax = ci_u), width = 0.2, 
                 position = position_dodge(0.9)) + 
  scale_fill_brewer(
    palette='Pastel1',
    name = 'Breast Fed') +
  theme_bw() +
  ylab(expression ('BMI'~(kg/m^2))) + 
  xlab('Sex')

### between ethnicity
race_bmi = svyby(~BMI,~breastfed + ethnicity, design = svy, svymean, 
                vartype = 'ci', na.rm = TRUE)

p3 = race_bmi %>% 
  ggplot( aes(x = ethnicity, y = BMI, group = breastfed)) +
  geom_col(aes(fill = breastfed),
           position = position_dodge(0.9)) +
  geom_errorbar( aes(ymin = ci_l, ymax = ci_u), width = 0.2, 
                 position = position_dodge(0.9)) + 
  scale_fill_brewer(
    palette='Pastel1',
    name = 'Breast Fed',
    labels = c('No', 'Yes')) +
  theme_bw() +
  ylab(expression ('BMI'~(kg/m^2))) + 
  xlab('Race/Ethnicity') +
  scale_x_discrete(labels = c('Mexcican\nAmerican', 'Hispanic',
                              'White', 'African\nAmerican', 'Asian',
                              'Other'))

### between mom smoke
smoke_bmi = svyby(~BMI,~breastfed + mom_smoked, design = svy, svymean, 
                vartype = 'ci', na.rm = TRUE)

p4 = smoke_bmi %>% 
  ggplot( aes(x = mom_smoked, y = BMI, group = breastfed)) +
  geom_col(aes(fill = breastfed),
           position = position_dodge(0.9)) +
  geom_errorbar( aes(ymin = ci_l, ymax = ci_u), width = 0.2, 
                 position = position_dodge(0.9)) + 
  scale_fill_brewer(
    palette='Pastel1',
    name = 'Breast Fed',
    labels = c('No', 'Yes')) +
  theme_bw() +
  ylab(expression ('BMI'~(kg/m^2))) + 
  xlab('Mother smoked when pregnant')

# Continuous outcome: ---------------------------------------------------------
## Sequence of models
mod1 = svyglm(BMI ~ breastfed + age + sex + 
                mom_smoked + birth_wt, design = svy)

### add race variable
mod2 = svyglm(BMI ~ breastfed + age + sex + 
                mom_smoked + birth_wt + ethnicity,  design = svy)


## plot relationship between birth weight & BMI
p5 = svyplot(BMI ~ birth_wt, style = 'trans', design = svy, legend = 0,
             xlab = 'Birth Weight (pounds)',
             ylab = expression ('BMI'~(kg/m^2)), pch = 19, alpha = c(0, 0.3))

### add splines
library(splines)
smod1 = svyglm(BMI ~ breastfed + ethnicity + ns(birth_wt, 2) +age + sex + 
                 mom_smoked,  design = svy)


smod2 = svyglm(BMI ~ breastfed*ns(birth_wt, 2) + ethnicity,  design = svy)



# Binary outcome: -------------------------------------------------------------
## BMI > 90 percentile to be overweight
BMI90 = svyquantile(~BMI, svy, quantiles = c(0.90), na.rm = TRUE)
svy = update(svy, overweight = BMI >= BMI90[1])

## do a survey-corrected chi-square test for independence
cat = svyby(formula = ~overweight, by = ~breastfed, 
            design = svy, FUN = svymean, na.rm=T)
chi_test = svychisq(~ overweight + breastfed, design = svy)

## models
bmod1 = svyglm(overweight ~ breastfed + birth_wt+ age +
                 sex,  
              design = svy, family = quasibinomial)

bmod2 = svyglm(overweight ~ breastfed + birth_wt + age +
                 sex + ethnicity,  
               design = svy, family = quasibinomial)


bmod3 = svyglm(overweight ~ breastfed + birth_wt + age +
                 sex + ethnicity + mom_smoked ,  
               design = svy, family = quasibinomial)

tab0 = tab_model(bmod1, bmod2, bmod3, smod1, digits = 3,
                 dv.labels = c('Overweight<br>(BMI \u2265 90th Percentile)', 
                               'Overweight<br>(BMI \u2265 90th Percentile)',
                               'Overweight<br>(BMI \u2265 90th Percentile)',
                               'BMI<br>(kg/m^2)'), p.style = 'star')


bmod4 = svyglm(overweight ~ breastfed * ethnicity + ns(birth_wt, 2),  
               design = svy, family = quasibinomial)


# Birth weight subset:  -------------------------------------------------------
birth_wt25 = svyquantile(~birth_wt, svy, quantiles = c(0.25), na.rm = TRUE)
birth_wt75 = svyquantile(~birth_wt, svy, quantiles = c(0.75), na.rm = TRUE)

svy_bwt1 = subset(svy, birth_wt <= birth_wt25[1])
svy_bwt2 = subset(svy, birth_wt < birth_wt75[1] & birth_wt > birth_wt25[1])
svy_bwt3 = subset(svy, birth_wt >= birth_wt75[1])

mod_bwt1 = svyglm(overweight ~ breastfed + ethnicity,  
               design = svy_bwt1, family = quasibinomial)
mod_bwt2 = svyglm(overweight ~ breastfed + ethnicity,  
                  design = svy_bwt2, family = quasibinomial)
mod_bwt3 = svyglm(overweight ~ breastfed + ethnicity,  
                  design = svy_bwt3, family = quasibinomial)
tab1 = tab_model(mod_bwt1, mod_bwt2, mod_bwt3, digits = 3,
                 dv.labels = c('Low birthweight', 
                               'Medium birthweight', 'High birthweight'))

# Race subset:  --------------------------------------------------------------
svy_sub1 = subset(svy, ethnicity == 'AfricanAmerican')
svy_sub2 = subset(svy, ethnicity != 'AfricanAmerican' )
mod_sub1 = svyglm(overweight ~ breastfed  + birth_wt,  
               design = svy_sub1, family = quasibinomial)

mod_sub2 = svyglm(overweight ~ breastfed  + birth_wt,  
                 design = svy_sub2, family = quasibinomial)

tab2 = tab_model(mod_sub1, mod_sub2, digits = 3, 
                 dv.labels = c('African American', 'Non-African American'))

# Sex subset:  --------------------------------------------------------------
svy_sub1 = subset(svy, sex == 'Female')
svy_sub2 = subset(svy, sex != 'Female' )

mod_sub1 = svyglm(overweight ~ breastfed  + birth_wt + ethnicity,  
                design = svy_sub1, family = quasibinomial)

mod_sub2 = svyglm(overweight ~ breastfed  + birth_wt + ethnicity,  
                 design = svy_sub2, family = quasibinomial)

tab3 = tab_model(mod_sub1, mod_sub2, digits = 3, 
                 dv.labels = c('Female', 'Male'))

# Mom smoked subset:  ---------------------------------------------------------
svy_sub1 = subset(svy, mom_smoked == 'Yes')
svy_sub2 = subset(svy, mom_smoked != 'Yes' )

mod_sub1 = svyglm(overweight ~ breastfed  + birth_wt + ethnicity,  
               design = svy_sub1, family = quasibinomial)

mod_sub2 = svyglm(overweight ~ breastfed  + birth_wt + ethnicity,  
               design = svy_sub2, family = quasibinomial)

tab4 = tab_model(mod_sub1, mod_sub2, digits = 3, 
                 dv.labels = c('Mother smoked', 'Mother Not smoked'))

# Effect of breastfed length: -------------------------------------------------
svy_fedT = subset(svy, breastfed == 'Yes')

## model breastfed duration with BMI
mod3 = svyglm(BMI ~ breastfed_length + ethnicity + birth_wt + age +
                sex,  design = svy_fedT)


# Matching non breastfed children to breast fed :------------------------------
library(MatchIt)
## create a group variable to indicate 'treatment' and 'control'
matchdf = df[, c('ethnicity', 'birth_wt', 'age', 'mom_smoked', 'breastfed', 'id')]
matchdf$breastfed = as.logical(matchdf$breastfed == 'Yes')

## perform the matching process
bfed_match = matchit(breastfed ~ age + ethnicity + birth_wt + mom_smoked,
                    data = matchdf, method = 'nearest', ratio = 1)

## the quality of the match
a = summary(bfed_match)
tab_m1 = knitr::kable( a$nn, digits = 2, align = 'c',
      caption = 'Table 2: Sample sizes')

## compare stats of two groups
tab_m2 = knitr::kable(a$sum.matched[c(1, 2, 4)], digits = 2, align = 'c',
      acption = 'Summary of balance for matched data')

jitter_plot = plot(bfed_match, type = 'jitter', interactive = FALSE)

## Create new data frame w breastfed and matches only
df_ref = match.data(bfed_match)[1:ncol(matchdf)] 
df_m = df[df$id %in% df_ref$id,]

rm(matchdf, bfed_match, a, adm)

# Define new Survey
options(survey.lonely.psu = 'adjust') 
svy_AM = subset(svy, id %in% df_m$id)

b = svyby( ~BMI, ~breastfed, design = svy_AM, svymean, 
           vartype="ci", na.rm=TRUE) 

svy_AM = update(svy_AM, overweight = BMI >= BMI90[1])

bmod_AN = svyglm(overweight ~ breastfed + ns(birth_wt, 2) + 
                 sex + mom_smoked + ethnicity,  
               design = svy, family = quasibinomial)

tab5 = tab_model(bmod1, bmod_AN, digits = 3, 
                 dv.labels = c('Before Match', 'After Match'))


# 79: -------------------------------------------------------------------------

