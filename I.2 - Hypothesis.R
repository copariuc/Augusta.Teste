# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(coin)) install.packages("coin")

library(rstatix); library(ggpubr); library(coin)

# Hypothesis testing ####
H1 <- ds %>% t_test(formula = weight ~ 1, mu = 150, alternative = "two.sided", detailed = TRUE); H1
H2 <- ds %>% t_test(formula = weight ~ 1, mu = 150, alternative = "greater", detailed = TRUE); H2
H3 <- ds %>% t_test(formula = weight ~ 1, mu = 150, alternative = "less", detailed = TRUE); H3

# Effect size computing ####
es <- ds %>% cohens_d(formula = weight ~ 1, mu = 150); es

# Adjusting visual representation ####
grp.H1 <- grp +
  labs(subtitle = get_test_label(H1, detailed = T)); grp.H1
grp.H2 <- grp +
  labs(subtitle = get_test_label(H2, detailed = T)); grp.H2
grp.H3 <- grp +
  labs(subtitle = get_test_label(H3, detailed = T)); grp.H3

## Drawing density graph
dnst <- ggdensity(data = ds, x = "weight", rug = TRUE, fill = "dark green"); dnst                   # Building main density graph 
dnst <- dnst + scale_x_continuous(limits = c(100, 155)); dnst                                       # Limit the X-axis amplitude                          
dnst <- dnst + stat_central_tendency(type = "mean", color = "dark red", linetype = "dashed"); dnst  # Adding samples' mean
dnst <- dnst + geom_vline(xintercept = 150, color = "dark blue", linetype = "dashed"); dnst         # Adding theoretical mean
dnst <- dnst + labs(subtitle = get_test_label(H3, detailed = T)); dnst                              # Adding test's information


# One-Sample Wilcoxon signed rank test ####
H1 <- ds %>% rstatix::wilcox_test(formula = weight ~ 1, mu = 150, alternative = "two.sided"); H1
H2 <- ds %>% rstatix::wilcox_test(formula = weight ~ 1, mu = 150, alternative = "greater"); H2
H3 <- ds %>% rstatix::wilcox_test(formula = weight ~ 1, mu = 150, alternative = "less"); H3

# Effect size computing ####
es <- ds %>% wilcox_effsize(formula = weight ~ 1, mu = 150); es

# Adjusting visual representation ####
grp.H1 <- grp +
  labs(subtitle = get_test_label(H1, detailed = T)); grp.H1

# Sign test ####
H1 <- ds %>% rstatix::sign_test(formula = weight ~ 1, mu = 150, alternative = "two.sided", detailed = TRUE); H1
H2 <- ds %>% rstatix::sign_test(formula = weight ~ 1, mu = 150, alternative = "greater", detailed = TRUE); H2
H3 <- ds %>% rstatix::sign_test(formula = weight ~ 1, mu = 150, alternative = "less", detailed = TRUE); H3

# Adjusting visual representation ####
grp.H2 <- grp +
  labs(subtitle = get_test_label(H2, detailed = T)); grp.H2
