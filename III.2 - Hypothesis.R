# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")

library(rstatix); library(ggpubr)

# Hypothesis testing - Paired t test ####
H1 <- ds.l %>% 
  t_test(formula = Scor ~ Grup, alternative = "two.sided", detailed = TRUE, paired = TRUE) %>% 
  add_significance(); H1
H2 <- ds.l %>% 
  t_test(formula = Scor ~ Grup, alternative = "less", detailed = TRUE, paired = TRUE) %>% 
  add_significance(); H2
H3 <- ds.l %>% 
  t_test(formula = Scor ~ Grup, alternative = "greater", detailed = TRUE, paired = TRUE) %>% 
  add_significance(); H3

# Effect size computing ####
es <- ds.l %>% cohens_d(Scor ~ Grup, paired = TRUE); es

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

# Wilcoxon Signed rank test ####
H1 <- ds.l %>% 
  rstatix::wilcox_test(formula = Scor ~ Grup, alternative = "two.sided", detailed = TRUE, paired = TRUE) %>% 
  add_significance(); H1
H2 <- ds.l %>% 
  rstatix::wilcox_test(formula = Scor ~ Grup, alternative = "less", detailed = TRUE, paired = TRUE) %>% 
  add_significance(); H2
H3 <- ds.l %>% 
  rstatix::wilcox_test(formula = Scor ~ Grup, alternative = "greater", detailed = TRUE, paired = TRUE) %>% 
  add_significance(); H3

# Effect size computing ####
es <- ds.l %>% wilcox_effsize(formula = Scor ~ Grup, paired = TRUE); es

# Adjusting visual representation ####
H <- H1 %>% add_xy_position(x = "Grup")
grp.H1 <- grp +
  stat_pvalue_manual(H, tip.length = 0) +
  labs(subtitle = get_test_label(H, detailed = T)); grp.H1

# Sign test ####
H1 <- ds.l %>% 
  rstatix::sign_test(formula = Scor ~ Grup, alternative = "two.sided", detailed = TRUE) %>% 
  add_significance(); H1
H2 <- ds.l %>% 
  rstatix::sign_test(formula = Scor ~ Grup, alternative = "less", detailed = TRUE) %>% 
  add_significance(); H2
H3 <- ds.l %>% 
  rstatix::sign_test(formula = Scor ~ Grup, alternative = "greater", detailed = TRUE) %>% 
  add_significance(); H3

# Adjusting visual representation ####
H <- H2 %>% add_xy_position(x = "Grup")
grp.H2 <- grp +
  stat_pvalue_manual(H, tip.length = 0) +
  labs(subtitle = get_test_label(H, detailed = T)); grp.H2
