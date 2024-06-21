# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")

library(rstatix); library(ggpubr)

# Loading dataset
load(file = "Data 1.RData"); head(ds)

# Compute descriptive statistics and visualize data ####
dsc <- ds %>% 
  get_summary_stats(weight, type = "full"); dsc
grp <- ggboxplot(data = ds$weight, bxp.errorbar = TRUE, orientation = "vertical",
                 ylab = "Greutate (Kg)", xlab = FALSE, add = c("mean", "jitter"),
                 ggtheme = theme_pubr(), fill = "green"); grp

# Assumption analysis ####
ds %>% identify_outliers(weight) # No outliers 
ds %>% shapiro_test(weight)      # Normality assumption fulfilled
ggqqplot(data = ds, x = "weight", color = "dark green",
         xlab = "Distributia normala teoretica", ylab = "Distributia greutatii (Kg)")
