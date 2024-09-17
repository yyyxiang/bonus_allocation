library('tidyverse')
library('lmerTest')

############ Table 1 ############
## Bonus ~ Condition + (1 + Condition | Participant)
dat <- read.csv('./../Data/exp1a+1b.csv', header = T, stringsAsFactors = T)
dat$subject <- as.factor(dat$subject)
dat$contestant <- as.factor(dat$contestant)

for (idx in unique(dat$exp_index)) {
  condition_lme <- lmer(bonus ~ condition + (1 + condition|subject),
                        data = dat %>% filter(exp_index == idx), control = lmerControl(optimizer = 'bobyqa'))
  print(summary(condition_lme))
}


############ Table 2 ############
## Bonus ~ Condition + Contestant (1 + Condition + Contestant | Participant)
dat <- read.csv('./../Data/exp1a+1b.csv', header = T, stringsAsFactors = T)
dat$subject <- as.factor(dat$subject)
dat$contestant <- as.factor(dat$contestant)

# same strength
for (idx in unique(dat$exp_index)) {
  same_strength <- lmer(bonus ~ condition + contestant + (1 + condition + contestant|subject),
                        data = dat %>% filter(exp_index == idx & s1 == s2), control = lmerControl(optimizer = 'bobyqa'))
  print(summary(same_strength))
}

# same effort
for (idx in unique(dat$exp_index)) {
  same_effort <- lmer(bonus ~ condition + contestant + (1 + condition + contestant|subject),
                      data = dat %>% filter(exp_index == idx & e1 == e2), control = lmerControl(optimizer = 'bobyqa'))
  print(summary(same_effort))
}

# same force
# Exp 1a
same_force <- lmer(bonus ~ condition + contestant + (1 + condition + contestant|subject),
                   data = dat %>% filter(exp_index == '1a' & s1 != s2 & e1 != e2), control = lmerControl(optimizer = 'bobyqa'))
print(summary(same_force))

# Exp 1b (dropping Condition regressor because the experiment only has one condition)
same_force <- lmer(bonus ~ contestant + (1 + contestant|subject),
                   data = dat %>% filter(exp_index == '1b' & s1 != s2 & e1 != e2), control = lmerControl(optimizer = 'bobyqa'))
print(summary(same_force))


############ Table 3 ############
## Bonus ~ Trial + (1 + Trial | Participant)
dat <- read.csv('./../Data/exp1a+1b.csv', header = T, stringsAsFactors = T)
dat$subject <- as.factor(dat$subject)

for (idx in unique(dat$exp_index)) {
  mdl <- lmer(bonus ~ trial + (1 + trial|subject), 
              data = dat %>% filter(exp_index == idx) %>% mutate(trial = scale(trial)),
              control = lmerControl(optimizer = 'bobyqa'))
  print(summary(mdl))
}


############ Table 4 ############
## Bonus ~ Effort*Secrecy + Condition + (1 + Effort*Secrecy + Condition | Participant)
dat <- read.csv('./../Data/exp2a+2b.csv', header = T, stringsAsFactors = T) %>% 
  mutate(subject = as_factor(subject),
         is_secret = as_factor(is_secret))

for (idx in unique(dat$exp_index)) {
  lme <- lmer(bonus ~ effort*is_secret + condition + (1 + effort*is_secret + condition|subject),
              data = dat %>% filter(exp_index == idx) %>% mutate(effort = scale(effort)), 
              control = lmerControl(optimizer = 'bobyqa'))
  print(summary(lme))
}

