# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")

library(rstatix); library(ggpubr)

# Loading dataset and reshape data in long format
load(file = "Data 3.RData"); head(ds)
ds.l <- ds %>% 
  gather(key = "Grup", value = "Scor", before, after)
ds.l$id <- factor(ds.l$id)
ds.l$Grup <- ordered(ds.l$Grup, levels = c("before", "after"), labels = c("Before", "After"))

# Compute descriptive statistics and visualize data ####
dsc <- ds.l %>% 
  group_by(Grup) %>% 
  get_summary_stats(Scor, type = "full"); dsc
grp <- ggpaired(data = ds.l, x = "Grup", y = "Scor", order = c("Before", "After"),
                bxp.errorbar = TRUE, orientation = "vertical", fill = "Grup",
                ylab = "Punctaj (0-100)", xlab = FALSE, add = c("mean", "jitter"),
                ggtheme = theme_pubr(), palette =  "jco"); grp

# Assumption analysis ####
ds <- ds %>% mutate(diff = after - before); head(ds)

ds %>% identify_outliers(diff)    # No outliers
ds %>% shapiro_test(diff)         # Normality assumption fulfilled
ggqqplot(data = ds, x = "diff", color = "dark green",
         xlab = "Distributia normala teoretica", ylab = "Distributia scorurilor")
