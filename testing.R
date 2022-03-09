library(tidyverse)
library(ggpubr)
library(rstatix)


waiting <- read.table("code (4) WaitingTime-table.csv",header = T , skip = 6, sep = ",")
waiting %>%
  group_by(max.patience) %>%
  get_summary_stats(waitingTime, type = "median_iqr")
bxp <- ggboxplot(waiting, x = "max.patience", y = "waitingTime",  
          color = "max.patience", palette = c("#00AFBB", "#E7B800", "#CC0066" ),
          ylab = "Waiting Time", xlab = "Patience")
bxp
stat.test <- waiting %>% 
  wilcox_test(waitingTime ~ max.patience) %>%
  add_significance()
stat.test

sum <- waiting %>% wilcox_effsize(waitingTime ~ max.patience)



masks2 <- read.table("code (4) masks2-table.csv",header = T , skip = 6, sep = ",")
set.seed(1234)
masks2 %>% sample_n_by(percent.wearing.masks, size = 1)
masks2 <- masks2 %>%
  reorder_levels(percent.wearing.masks, order = c("0", "50", "100"))
summarymasks2 <- masks2 %>% 
  group_by(percent.wearing.masks) %>%
  get_summary_stats(count.people.with..status....infected.., type = "common")

ggboxplot(masks2, x = "percent.wearing.masks", y = "count.people.with..status....infected..",  
          color = "percent.wearing.masks", palette = c("#00AFBB", "#E7B800", "#CC0066" ),
          ylab = "People Infected", xlab = "Percentage wearing a mask")

res.kruskal <- masks2 %>% kruskal_test(count.people.with..status....infected.. ~ percent.wearing.masks)
res.kruskal
effsizemask <- masks2 %>% kruskal_effsize(count.people.with..status....infected.. ~ percent.wearing.masks)

