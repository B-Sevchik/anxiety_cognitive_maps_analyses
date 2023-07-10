df_subject <- df %>%
  filter(subject == 'a2tbkohgx08vrk', taskName == 'oddOneOutTest')
df_subject

mean_acc <- mean(df_subject$acc)
mean_acc