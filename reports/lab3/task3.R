library(tidyverse)
library(ggplot2)

load(file='../../data/trades.RData')
data <- select(bind_rows(trades), -geo)
exp_imp <- filter(data, str_detect(indic_et, "Imports in|Exports in"))
exp_imp <- pivot_wider(exp_imp, names_from = indic_et, values_from = values) 
exp_imp <- rename(exp_imp, product = "sitc06", 
                  export = "Exports in million of ECU/EURO", 
                  import = "Imports in million of ECU/EURO")
exp_imp <- group_by(exp_imp, time)
sum_exp_imp <- summarise(exp_imp, export = sum(export), import = sum(import))

min_exp_imp <- slice(sum_exp_imp, which.min(export))
sum_exp_imp_wo_min <- slice(sum_exp_imp, -which.min(export))
ggplot() + 
  geom_line(data = sum_exp_imp, aes(x = time, y = export)) +
  geom_point(data = sum_exp_imp_wo_min, aes(x = time,y = export), size = 1) +
  geom_text(data = sum_exp_imp_wo_min, aes(x = time, y = export, label = export), 
            size = 2.5, vjust = 0, nudge_x = -100, nudge_y = 15000) +
  geom_point(data = min_exp_imp, aes(x = time, y = export), 
             size = 1, color = "red") +
  geom_text(data = min_exp_imp, aes(x = time, y = export, label = export), 
            size = 2.5, vjust = 0, nudge_x = 0, nudge_y = -30000, color = "red") +
  labs(title = "Total export changes", x = "Year", y = "Value, million of ECU/EURO")
