library('tidyverse')
library('lmerTest')
library('Rmisc')
library('patchwork')
source('helper.R')

### Exp1a and Exp1b
dat <- read.csv('./../Data/exp1a+1b.csv', header = T, stringsAsFactors = T)
model_predictions <- bonus_model_predictions(dat)
dat$subject <- as.factor(dat$subject)
dat$contestant <- as.factor(dat$contestant)
model_predictions$contestant <- as.factor(model_predictions$contestant)
model_predictions <- model_predictions %>% mutate(type = case_when(s1 == s2 ~ 'Same strength',
                                                                   e1 == e2 ~ 'Same effort',
                                                                   T ~ 'Same force'))
dat <- merge(dat, model_predictions) %>% 
  dplyr::rename(bonus_data = bonus)
dat <- arrange(dat, exp_index, subject, scenario, contestant)

## Condition main effect on pooled data
for (idx in c('1a', '1b')) {
  condition_lme <- lmer(bonus_data ~ condition + (condition|subject),
                        data = dat %>% filter(exp_index == idx))
  print(summary(condition_lme))
}

## descriptive stats
# same strength
for (idx in c('1a', '1b')) {
  same_strength <- lmer(bonus_data ~ condition + contestant + (condition|subject) + (contestant|subject),
                        data = dat %>% filter(exp_index == idx & s1 == s2))
  print(summary(same_strength))
}

# same effort
for (idx in c('1a', '1b')) {
  same_effort <- lmer(bonus_data ~ condition + contestant + (condition|subject) + (contestant|subject),
                      data = dat %>% filter(exp_index == idx & e1 == e2))
  print(summary(same_effort))
}

# same force
# Exp 1a
same_force <- lmer(bonus_data ~ condition + contestant + (condition|subject) + (contestant|subject),
                   data = dat %>% filter(exp_index == '1a' & s1 != s2 & e1 != e2))
print(summary(same_force))

# Exp 1b (only has one condition)
same_force <- lmer(bonus_data ~ contestant + (contestant|subject),
                   data = dat %>% filter(exp_index == '1b' & s1 != s2 & e1 != e2))
print(summary(same_force))

# cor.test
dat <- read.csv('./../Data/exp1a+1b.csv', header = T, stringsAsFactors = T) %>% 
  dplyr::rename(response = bonus)
model_predictions <- bonus_model_predictions(dat)

dat_merge_long <- merge_long(dat, model_predictions)
for (exp in unique(dat_merge_long$exp_index)) {
  for (m in unique(dat_merge_long$model)) {
    cor_fail <- cor.test(dat_merge_long$avg_response[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$prediction[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
    cor_lift <- cor.test(dat_merge_long$avg_response[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$prediction[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
  }
}

### Exp2a and Exp2b
dat <- read.csv('./../Data/exp2a+2b.csv', header = T, stringsAsFactors = T) %>% 
  mutate(subject = as_factor(subject),
         is_secret = as_factor(is_secret))

for (idx in c('2a', '2b')) {
  lme <- lmer(bonus ~ effort*is_secret + condition + (effort*is_secret|subject) + (condition|subject), 
              data = dat %>% filer(exp_index == idx))
  print(summary(lme))
}
