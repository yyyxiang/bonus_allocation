bonus_model_predictions <- function(data) {
  
  data$force <- data$strength*data$effort/100
  data$total_force <- data$s1*data$e1/100 + data$s2*data$e2/100  
  df <- data %>% filter(subject == 1) %>% mutate(subject = NULL)
  
  df_fail <- df %>% filter(condition == 'fail')
  df_fail$probability <- apply(array(1:dim(df_fail)[1]), 1, function(i){
    mean(apply(array(seq(from = 1, to = 100 - df_fail$effort[i], by = 1)), 1, function(x){
      x/100*df_fail$strength[i] + df_fail$total_force[i] >= df_fail$box_weight[i]
    }))
  })
  
  df_lift <- df %>% filter(condition == 'lift')
  df_lift$probability <- apply(array(1:dim(df_lift)[1]), 1, function(i){
    mean(apply(array(seq(from = 1, to = df_lift$effort[i], by = 1)), 1, function(x){
      df_lift$total_force[i] - x/100*df_lift$strength[i] < df_lift$box_weight[i]
    }))
  })
  
  df <- rbind(df_fail, df_lift)
  
  df_flipped <- df %>% 
    mutate(contestant = 3-contestant) %>% 
    mutate(resp_NFA = (1-probability)*10) %>% 
    select(exp_index, scenario, contestant, resp_NFA)
  df <- merge(df, df_flipped)
  
  model_predictions <- df %>% 
    mutate(prediction_F = force,
           prediction_S = ifelse(condition == 'fail', 10-strength, strength),
           prediction_E = effort/10,
           prediction_FA = ifelse(condition == 'fail', 10-probability*10, probability*10),
           prediction_NFA = ifelse(condition == 'fail', 10-resp_NFA, resp_NFA),
           prediction_BA = (prediction_FA + prediction_NFA)/2,
           prediction_EBA = (prediction_E + prediction_BA)/2)
  model_predictions <- arrange(model_predictions, exp_index, scenario)
  model_predictions <- model_predictions[, c('exp_index','condition','scenario','box_weight','contestant',
                                             'strength','effort','s1','s2','e1','e2',
                                             'prediction_E','prediction_S','prediction_F',
                                             'prediction_BA','prediction_FA','prediction_NFA',
                                             'prediction_EBA')]
  
  return(model_predictions)
}

resp_model_predictions <- function(data) {
  
  data$force <- data$strength*data$effort/100
  data$total_force <- data$s1*data$e1/100 + data$s2*data$e2/100  
  df <- data %>% filter(subject == 1) %>% mutate(subject = NULL)
  
  df_fail <- df %>% filter(condition == 'fail')
  df_fail$probability <- apply(array(1:dim(df_fail)[1]), 1, function(i){
    mean(apply(array(seq(from = 1, to = 100 - df_fail$effort[i], by = 1)), 1, function(x){
      x/100*df_fail$strength[i] + df_fail$total_force[i] >= df_fail$box_weight[i]
    }))
  })
  
  df_lift <- df %>% filter(condition == 'lift')
  df_lift$probability <- apply(array(1:dim(df_lift)[1]), 1, function(i){
    mean(apply(array(seq(from = 1, to = df_lift$effort[i], by = 1)), 1, function(x){
      df_lift$total_force[i] - x/100*df_lift$strength[i] < df_lift$box_weight[i]
    }))
  })
  
  df <- rbind(df_fail, df_lift)
  
  df_flipped <- df %>% 
    mutate(contestant = 3-contestant) %>% 
    mutate(resp_NFA = (1-probability)*10) %>% 
    select(exp_index, scenario, contestant, resp_NFA)
  df <- merge(df, df_flipped)
  
  model_predictions <- df %>% 
    mutate(prediction_F = case_when(condition == 'fail' ~ 10-force,
                              condition == 'lift' ~ force),
           prediction_S = strength,
           prediction_E = case_when(condition == 'fail' ~ (100-effort)/10,
                              condition == 'lift' ~ effort/10),
           prediction_FA = probability*10,
           prediction_NFA = resp_NFA,
           prediction_BA = (prediction_FA + prediction_NFA)/2,
           prediction_EBA = (prediction_E + prediction_BA)/2)
  model_predictions <- arrange(model_predictions, exp_index, scenario)
  model_predictions <- model_predictions[, c('exp_index','condition','scenario','box_weight','contestant',
                                             'strength','effort','s1','s2','e1','e2',
                                             'prediction_E','prediction_S','prediction_F',
                                             'prediction_BA','prediction_FA','prediction_NFA',
                                             'prediction_EBA')]
  
  return(model_predictions)
}

plt_data <- function(dat, x, y) {
  
  return(
    ggplot(dat, aes_string(x, y, group = 'scenario', color = 'condition')) +
      stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar', width = .1) +
      stat_summary(fun = 'mean', geom = 'point') +
      stat_summary(fun = 'mean', geom = 'line') +
      scale_color_manual(name = NULL, labels = c('Fail', 'Lift'),
                         values = c('#C77CFF', '#00BAE0'),
                         limits = c('fail', 'lift')) +
      coord_cartesian(ylim = c(0, 10)) +
      theme_bw()
  )
}

line_plots <- function(dat, idx, y, y_label, title) {

  subplot_a1 <- plt_data(dat %>% filter(exp_index == idx & s1==s2), 'contestant', y) +
    facet_wrap(~ type, strip.position ='right')

  subplot_b1 <- plt_data(dat %>% filter(exp_index == idx & e1==e2), 'contestant', y) +
    facet_wrap(~ type, strip.position ='right')

  subplot_c1 <- plt_data(dat %>% filter(exp_index == idx & (s1 != s2 & e1 != e2)), 'contestant', y) +
    facet_wrap(~ type, strip.position ='right')

  if (idx != '2a' & idx != '2b') {
    return(
      subplot_a1 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) +
        labs(x = NULL, y = y_label, title = title) +
        theme(legend.position = 'none', 
              plot.title = element_text(hjust = 0.5),
              strip.background = element_blank(),
              strip.text = element_blank()) +
        
        subplot_b1 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) +
        labs(x = NULL, y = y_label) +
        theme(legend.position = 'none',
              strip.background = element_blank(),
              strip.text = element_blank()) +
        
        subplot_c1 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) +
        labs(x = 'Contestant', y = y_label) +
        theme(legend.position = 'none',
              strip.background = element_blank(),
              strip.text = element_blank()) +
        
        plot_layout(nrow = 3)
    )
  } else if (idx == '2a') {
    return(
      subplot_a1 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) +
        labs(x = NULL, y = y_label, title = title) +
        theme(legend.position = 'none', 
              plot.title = element_text(hjust = 0.5)) +
        
        subplot_b1 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) +
        labs(x = NULL, y = y_label) +
        
        subplot_c1 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) +
        labs(x = 'Contestant', y = y_label) +
        theme(legend.position = 'none') +
        
        plot_layout(nrow = 3)
    )
  } else {
    return(
      subplot_a1 + scale_x_discrete(labels=c("1" = "Less effort", "2" = "More effort")) +
        labs(x = NULL, y = y_label, title = title) +
        theme(legend.position = 'none', 
              plot.title = element_text(hjust = 0.5)) +
        
        subplot_b1 + scale_x_discrete(labels=c("1" = "Less strength", "2" = "More strength")) +
        labs(x = NULL, y = y_label) +
        theme(legend.position = 'none') +
        
        subplot_c1 + scale_x_discrete(labels=c("1" = "Less strength/\nmore effort", "2" = "More strength/\nless effort")) +
        labs(x = 'Contestant', y = y_label) +
        theme(legend.position = 'none') +
        
        plot_layout(nrow = 3)
    )
  }
}

bic_plot <- function(dat) {
  bic_table <- NULL
  for (exp in unique(dat$exp_index)) {
    F_model <- lmer(response ~ prediction_F + condition + (prediction_F|subject) + (condition|subject),
                    data = dat %>% filter(exp_index == exp), REML = F)
    S_model <- lmer(response ~ prediction_S + condition + (prediction_S|subject) + (condition|subject),
                    data = dat %>% filter(exp_index == exp), REML = F)
    E_model <- lmer(response ~ prediction_E + condition + (prediction_E|subject) + (condition|subject),
                    data = dat %>% filter(exp_index == exp), REML = F)
    FA_model <- lmer(response ~ prediction_FA + condition + (prediction_FA|subject) + (condition|subject),
                     data = dat %>% filter(exp_index == exp), REML = F)
    NFA_model <- lmer(response ~ prediction_NFA + condition + (prediction_NFA|subject) + (condition|subject),
                      data = dat %>% filter(exp_index == exp), REML = F)
    BA_model <- lmer(response ~ prediction_BA + condition + (prediction_BA|subject) + (condition|subject),
                     data = dat %>% filter(exp_index == exp), REML = F)
    EBA_model <- lmer(response ~ prediction_EBA + condition + (prediction_EBA|subject) + (condition|subject),
                      data = dat %>% filter(exp_index == exp), REML = F)
    df <- BIC(F_model, S_model, E_model, FA_model, NFA_model, BA_model, EBA_model) %>% select(BIC)
    df$model <- rownames(df)
    rownames(df) <- NULL
    df$exp_index <- exp
    bic_table <- rbind(bic_table, df)
  }
  bic_table$model <- factor(bic_table$model, labels = c('F','S','E','FA','NFA','BA','EBA'), 
                            levels = c('F_model','S_model','E_model','FA_model',
                                       'NFA_model','BA_model',
                                       'EBA_model'), ordered = T)
  
  return(
    bic_table %>% 
    ggplot(aes(model, BIC, color = model)) +
    geom_point(size = 3) +
    facet_wrap(~ exp_index, nrow = 2, scales = 'free_y',
               labeller = as_labeller(c(`1a`='Bonus Exp 1a', `1b`='Bonus Exp 1b', `2a`='Responsibility', `2b`='Responsibility'))) +
    theme_bw() +
      labs(x = 'Model')
    )
}

single_participant_bic_plot <- function(dat) {
  full_df <- NULL
  full_p <- NULL
  full_ci <- NULL
  for (exp in unique(dat$exp_index)) {
    subset <- dat %>% filter(exp_index == exp)
    df <- data.frame(exp_index = exp, subject = unique(subset$subject), bic_e = NaN, bic_s = NaN, bic_f = NaN,
                     bic_ba = NaN, bic_fa = NaN, bic_nfa = NaN, bic_eba = NaN)
    for (s in df$subject) {
      F_model <- lm(response ~ prediction_F + condition, data = subset %>% filter(subject == s))
      S_model <- lm(response ~ prediction_S + condition, data = subset %>% filter(subject == s))
      E_model <- lm(response ~ prediction_E + condition, data = subset %>% filter(subject == s))
      FA_model <- lm(response ~ prediction_FA + condition, data = subset %>% filter(subject == s))
      NFA_model <- lm(response ~ prediction_NFA + condition, data = subset %>% filter(subject == s))
      BA_model <- lm(response ~ prediction_BA + condition, data = subset %>% filter(subject == s))
      EBA_model <- lm(response ~ prediction_EBA + condition, data = subset %>% filter(subject == s))
      bic_df <- BIC(E_model, S_model, F_model, BA_model, FA_model, NFA_model, EBA_model)
      if (is.infinite(bic_df$BIC)[1] == T) {
        df$lowest[df$subject == s] <- NaN
      } else{
      df$lowest[df$subject == s] <- row.names(bic_df[which(bic_df$BIC == min(bic_df$BIC)),])
      }
      df[df$subject == s, 3:(ncol(df)-1)] <- bic_df[,2]
      counts <- data.frame(rbind(table(df$lowest)))
      num_subject <- length(df$subject)
      p <- counts/num_subject
      ci <- sqrt(p*(1-p)/num_subject)*1.96
      p$exp_index <- exp
      ci$exp_index <- exp
    }
    full_df <- dplyr::bind_rows(full_df, df)
    full_p <- dplyr::bind_rows(full_p, p)
    full_ci <- dplyr::bind_rows(full_ci, ci)
  }
  
  full_p <- full_p %>% pivot_longer(cols = !exp_index, names_to = 'model', values_to = 'p')
  full_ci <- full_ci %>% pivot_longer(cols = !exp_index, names_to = 'model', values_to = 'ci')
  full_p <- merge(full_p, full_ci) %>% 
    replace_na(list(p = 0, ci = 0))
  full_p$model <- factor(full_p$model, labels = c('F','S','E','FA','NFA','BA','EBA'), 
                         levels = c('F_model','S_model','E_model','FA_model',
                                    'NFA_model','BA_model',
                                    'EBA_model'), ordered = T)
  
  return(
    full_p %>% ggplot(aes(model, p, fill = model)) +
    geom_col(position = 'dodge') +
    geom_errorbar(aes(ymin = p-ci, ymax = p+ci), width = .1,
                  position = position_dodge(.9)) +
    coord_cartesian(ylim = c(NA,1)) +
    facet_wrap(~ exp_index, nrow = 2,
               labeller = as_labeller(c(`1a`='Bonus Exp 1a', `1b`='Bonus Exp 1b', `2a`='Responsibility', `2b`='Responsibility'))) +
    theme_bw() +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
      labs(x = 'Model', y = 'Proportion')
  )
}

merge_long <- function(dat, model_predictions) {
  dat_summary <- dat %>%
    group_by(exp_index, condition, scenario, contestant) %>%
    dplyr::summarize(avg_response = mean(response),
                     uci_response = CI(response)[1],
                     lci_response = CI(response)[3])
  dat_merge <- merge(dat_summary, model_predictions)
  dat_merge_long <- dat_merge %>% pivot_longer(cols = c('prediction_E', 'prediction_BA', 'prediction_EBA'), 
                                               names_to = 'model',
                                               values_to = 'prediction')
  dat_merge_long$model <- substr(dat_merge_long$model, 12, nchar(dat_merge_long$model))
  dat_merge_long$model <- factor(dat_merge_long$model, levels = c('E', 'BA', 'EBA', ordered = T))
  return(dat_merge_long)
}

correlations <- function(dat_merge_long) {
  text_labels <- data.frame(exp_index = sort(rep(unique(dat_merge_long$exp_index),3)), model = rep(unique(dat_merge_long$model),2))
  for (exp in unique(dat_merge_long$exp_index)) {
    for (m in unique(dat_merge_long$model)) {
      cor_both <- cor(dat_merge_long$avg_response[dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$prediction[dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
      cor_both <- paste0('r = ', format(round(cor_both, 2), nsmall = 2))
      cor_fail <- cor(dat_merge_long$avg_response[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$prediction[dat_merge_long$condition == 'fail' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
      cor_fail <- paste0('r_fail = ', format(round(cor_fail, 2), nsmall = 2))
      cor_lift <- cor(dat_merge_long$avg_response[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], dat_merge_long$prediction[dat_merge_long$condition == 'lift' & dat_merge_long$exp_index == exp & dat_merge_long$model == m], method = 'pearson', use = 'complete.obs')
      cor_lift <- paste0('r_lift = ', format(round(cor_lift, 2), nsmall = 2))
      text_labels$text[text_labels$exp_index == exp & text_labels$model == m] <- paste0(cor_both, sep = '\n', cor_fail, sep = '\n', cor_lift)
      text_labels$x[text_labels$exp_index == exp & text_labels$model == m] = 8
      text_labels$y[text_labels$exp_index == exp & text_labels$model == m] = 1.5
    }
  }
  return(text_labels)
}

cor_plt <- function(data, x, y, ymin, ymax, text_data) {
  
  return(
    ggplot(data, aes_string(x, y, color = 'condition')) +
      geom_errorbar(aes_string(ymin = ymin, ymax = ymax, width = 0)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, lty = 3) +
      scale_color_manual(name = NULL, labels = c('Fail', 'Lift'),
                         values = c('#C77CFF', '#00BAE0'),
                         limits = c('fail', 'lift')) +
      scale_x_continuous(limits = c(0, 10), breaks = c(0,2.5,5,7.5,10)) +
      scale_y_continuous(limits = c(0,10), breaks = c(0,2.5,5,7.5,10)) +
      coord_fixed() +
      facet_wrap(~ model, nrow = 3, strip.position ='right',
                 labeller = as_labeller(c(`E` = 'Effort', `BA` = 'BA counterfactual', `EBA` = 'Ensemble',
                                          `1a` = 'Bonus Exp 1a', `1b` = 'Bonus Exp 1b', `2a` = 'Responsibility', `2b` = 'Responsibility'))) +
      theme_bw() +
      geom_text(data = text_data,
                mapping = aes(label = text, x = x , y = y),
                inherit.aes = F,
                fontface='italic')
    
  )
}
