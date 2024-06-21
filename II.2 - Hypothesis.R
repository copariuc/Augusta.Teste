# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")

library(rstatix); library(ggpubr)

# Hypothesis testing - Welch test ####
H1 <- ds %>% 
  t_test(formula = Greutate ~ Grup, alternative = "two.sided", detailed = TRUE, var.equal = FALSE) %>% 
  add_significance(); H1
H2 <- ds %>% 
  t_test(formula = Greutate ~ Grup, alternative = "less", detailed = TRUE, var.equal = FALSE) %>% 
  add_significance(); H2
H3 <- ds %>% 
  t_test(formula = Greutate ~ Grup, alternative = "greater", detailed = TRUE, var.equal = FALSE) %>% 
  add_significance(); H3

# Effect size computing ####
es <- ds %>% cohens_d(Greutate ~ Grup, var.equal = FALSE); es

# Adjusting visual representation ####
H <- H1 %>% add_xy_position(x = "Grup")
grp.H1 <- grp +
  stat_pvalue_manual(H, tip.length = 0) +
  labs(subtitle = get_test_label(H, detailed = T)); grp.H1

H <- H2 %>% add_xy_position(x = "Grup")
grp.H2 <- grp +
  stat_pvalue_manual(H, tip.length = 0) +
  labs(subtitle = get_test_label(H, detailed = T)); grp.H2

H <- H3 %>% add_xy_position(x = "Grup")
grp.H3 <- grp +
  stat_pvalue_manual(H, tip.length = 0) +
  labs(subtitle = get_test_label(H, detailed = T)); grp.H3

# Wilcoxon rank sum test ####
H1 <- ds %>% 
  rstatix::wilcox_test(formula = Greutate ~ Grup, alternative = "two.sided", detailed = TRUE) %>% 
  add_significance(); H1
H2 <- ds %>% 
  rstatix::wilcox_test(formula = Greutate ~ Grup, alternative = "less", detailed = TRUE) %>% 
  add_significance(); H2
H2 <- ds %>% 
  rstatix::wilcox_test(formula = Greutate ~ Grup, alternative = "greater", detailed = TRUE) %>% 
  add_significance(); H2

# Effect size computing ####
es <- ds %>% wilcox_effsize(formula = Greutate ~ Grup); es

# Adjusting visual representation ####
H <- H1 %>% add_xy_position(x = "Grup")
grp.H1 <- grp +
  stat_pvalue_manual(H, tip.length = 0) +
  labs(subtitle = get_test_label(H, detailed = T)); grp.H1

# Sign test ####
H1 <- ds %>% 
  rstatix::sign_test(formula = Greutate ~ Grup, alternative = "two.sided", detailed = TRUE) %>% 
  add_significance(); H1
H2 <- ds %>% 
  rstatix::sign_test(formula = Greutate ~ Grup, alternative = "less", detailed = TRUE) %>% 
  add_significance(); H2
H3 <- ds %>% 
  rstatix::sign_test(formula = Greutate ~ Grup, alternative = "greater", detailed = TRUE) %>% 
  add_significance(); H3

# Adjusting visual representation ####
H <- H1 %>% add_xy_position(x = "Grup")
grp.H1 <- grp +
  stat_pvalue_manual(H, tip.length = 0) +
  labs(subtitle = get_test_label(H, detailed = T)); grp.H1
