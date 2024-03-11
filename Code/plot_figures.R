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
dat <- dat %>% mutate(type = case_when(s1 == s2 ~ 'Same strength',
                                       e1 == e2 ~ 'Same effort',
                                       T ~ 'Same force'))

# Line plots
p1_1a_bonus <- line_plots(dat, '1a', 'bonus', 'Bonus', 'Exp 1a Bonus')
p1_1b_bonus <- line_plots(dat, '1b', 'bonus', 'Bonus', 'Exp 1b Bonus')  

dat <- merge(dat, model_predictions) %>% 
  dplyr::rename(response = bonus) %>% 
  arrange(exp_index, subject, scenario, contestant)

# BIC plots
p2_bonus <- bic_plot(dat) + theme(legend.position = 'none')
p3_bonus <- single_participant_bic_plot(dat) + theme(legend.position = 'none')

# scatter plots
dat <- read.csv('./../Data/exp1a+1b.csv', header = T, stringsAsFactors = T) %>% 
  dplyr::rename(response = bonus)
model_predictions <- bonus_model_predictions(dat)
dat_merge_long <- merge_long(dat, model_predictions)
text_labels <- correlations(dat_merge_long)

p4_1a_bonus <- cor_plt(dat_merge_long %>% filter(exp_index == '1a'), 'prediction', 'avg_response', 'lci_response', 'uci_response', text_labels %>% filter(exp_index == '1a')) +
  labs(title = 'Exp 1a Bonus', x = 'Model', y = 'Data') +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

p4_1b_bonus <- cor_plt(dat_merge_long %>% filter(exp_index == '1b'), 'prediction', 'avg_response', 'lci_response', 'uci_response', text_labels %>% filter(exp_index == '1b')) +
  labs(title = 'Exp 1b Bonus', x = 'Model', y = NULL) + 
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'none')

# Responsibility attribution data from Xiang et al. (2023)
dat <- read.csv('./../Data/resp_data.csv', header = T, stringsAsFactors = T)
model_predictions <- resp_model_predictions(dat)
dat$subject <- as.factor(dat$subject)
dat$contestant <- as.factor(dat$contestant)
model_predictions$contestant <- as.factor(model_predictions$contestant)
dat <- dat %>% mutate(type = case_when(s1 == s2 ~ 'Same strength',
                                       e1 == e2 ~ 'Same effort',
                                       T ~ 'Same force'))

p1_2a_resp <- line_plots(dat, '2a', 'resp', 'Responsibility', 'Responsibility')
p1_2b_resp <- line_plots(dat, '2b', 'resp', 'Responsibility', 'Responsibility')  

dat <- merge(dat, model_predictions) %>% 
  dplyr::rename(response = resp) %>% 
  arrange(exp_index, subject, scenario, contestant)

# BIC plots
p2_resp <- bic_plot(dat) + labs(color = 'Model')
p3_resp <- single_participant_bic_plot(dat) + labs(fill = 'Model')

# scatter plots
dat <- read.csv('./../Data/resp_data.csv', header = T, stringsAsFactors = T) %>% 
  dplyr::rename(response = resp)
model_predictions <- resp_model_predictions(dat)
dat_merge_long <- merge_long(dat, model_predictions)
text_labels <- correlations(dat_merge_long)

p4_2a_resp <- cor_plt(dat_merge_long %>% filter(exp_index == '2a'), 'prediction', 'avg_response', 'lci_response', 'uci_response', text_labels %>% filter(exp_index == '2a')) +
  labs(title = 'Responsibility Exp 2a', x = 'Model', y = NULL) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(hjust = 0.5))

p4_2b_resp <- cor_plt(dat_merge_long %>% filter(exp_index == '2b'), 'prediction', 'avg_response', 'lci_response', 'uci_response', text_labels %>% filter(exp_index == '2b')) +
  labs(title = 'Responsibility Exp 2b', x = 'Model', y = NULL) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none')


# fig2
pdf('fig2.pdf', onefile = T, width = 12, height = 8)
p1_1a_bonus | p1_2a_resp | p1_1b_bonus | p1_2b_resp
dev.off()

# fig3
pdf('fig3.pdf', onefile = T, width = 10, height = 6)
p2_bonus + p2_resp + p3_bonus + p3_resp + plot_layout(nrow = 2)
dev.off()

# figure4
pdf('fig4.pdf', onefile = T, width = 12, height = 8)
p4_1a_bonus | p4_2a_resp | p4_1b_bonus | p4_2b_resp
dev.off()


### Exp2a and Exp2b
dat <- read.csv('./../Data/exp2a+2b.csv', header = T, stringsAsFactors = T) %>% 
  mutate(subject = as_factor(subject),
         is_secret = as_factor(is_secret))

avg_bonus_binned <- dat %>% 
  mutate(effort_binned = cut(dat$effort, c(0, 30, 60, 90, 100), c("0-30", "30-60", "60-90", "90-100"))) %>%
  group_by(exp_index, subject, condition, is_secret, effort_binned) %>%
  dplyr::summarize(average_bonus = mean(bonus)) %>% 
  pivot_wider(names_from = is_secret, values_from = average_bonus) %>% 
  mutate(bonus_diff = `0` - `1`)

# fig5
pdf('fig5.pdf', onefile = T, width = 8, height = 3)
p5 <- avg_bonus_binned %>% 
  ggplot(aes(effort_binned, bonus_diff)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar', width = .1) +
  stat_summary(fun = 'mean', geom = 'point') +
  stat_summary(fun = 'mean', geom = 'line', group = 1) +
  facet_grid(condition ~ exp_index, 
             labeller = as_labeller(c('2a' = 'Exp 2a', '2b' = 'Exp 2b', 'fail' = 'Fail', 'lift' = 'Lift')))
p5 + labs(x = 'Effort (%)', y = 'Within-subject bonus difference') + theme_bw()
dev.off()
