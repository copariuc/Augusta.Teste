# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")

library(rstatix); library(ggpubr)

# Loading dataset
load(file = "Data 2.RData"); head(ds)
colnames(ds) <- c("id", "Grup", "Greutate")

# Compute descriptive statistics and visualize data ####
dsc <- ds %>% 
  group_by(Grup) %>% 
  get_summary_stats(Greutate, type = "full"); dsc
grp <- ggboxplot(data = ds, x = "Grup", y = "Greutate", fill = "Grup",
                 bxp.errorbar = TRUE, orientation = "vertical",
                 ylab = "Greutate (Kg)", xlab = FALSE, add = c("mean", "jitter"),
                 ggtheme = theme_pubr(), palette = "jco"); grp

# Assumption analysis ####
ds %>%
  group_by(Grup) %>% 
  identify_outliers(Greutate)    # No outliers
ds %>% 
  group_by(Grup) %>% 
  shapiro_test(Greutate)      # Normality assumption fulfilled
ggqqplot(data = ds, x = "Greutate", facet.by = "Grup", color = "dark green",
         xlab = "Distributia normala teoretica", ylab = "Distributia greutatii (Kg)")

var.test(Greutate ~ Grup, data = ds, alternative = "two.sided")     # F-test
ds %>% levene_test(Greutate ~ Grup, center = "mean")                # Levene test
fligner.test(Greutate ~ Grup, data = ds)                            # Fligner-Killeen Test



