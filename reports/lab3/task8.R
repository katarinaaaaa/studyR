library(tidyverse)
library(ggplot2)

load(file='../../data/ExpImp.RData')

data <- filter(ExpImp, !str_detect(ExpImp$Регион, 
                                   'Федерация|федеральный округ|в том числе'))
for (col_name in colnames(data)[colnames(data) != "Регион"]) {
  data[[col_name]] <- as.numeric(gsub("-", NA, data[[col_name]]))
}

data_met <- select(data, c("Регион", "МетЭкспорт", "МетИмпорт"))

all_exp <- select_at(data, vars(matches("Экспорт")))
all_imp <- select_at(data, vars(matches("Импорт")))
data_met$СуммарныйЭкспорт <- rowSums(all_exp, na.rm = TRUE)
data_met$СуммарныйИмпорт <- rowSums(all_imp, na.rm = TRUE)

data_met <- data_met[complete.cases(data_met),]
data_sum <- select(data_met, -c("МетЭкспорт", "МетИмпорт"))
data_met <- select(data_met, c("Регион", "МетЭкспорт", "МетИмпорт"))

# first plot
plot_data <- pivot_longer(data_met, !Регион, names_to = "Показатель", values_to = "Значение")
ggplot(data = plot_data, mapping = aes(x = Регион, y = Значение, fill = Показатель)) +
  geom_col(color = 'black', position = 'dodge') +
  scale_fill_manual(labels = c("Импорт", "Экспорт"), values = c("blue", "green")) +
  ggtitle("Суммарный экспорт и импорт металлургической промышленности") +
  ylab('Объем, млн долл. США') + theme(legend.position='top') +
  coord_flip()

# second plot
data_met[,"МетЭкспорт"] <- -data_met[,"МетЭкспорт"]
plot_data <- pivot_longer(data_met, !Регион, names_to = "Показатель", values_to = "Значение")
ggplot(data = plot_data, mapping = aes(x = Регион, y = Значение, fill = Показатель)) +
  geom_col(color = 'black', position = 'dodge') +
  scale_fill_manual(labels = c("Импорт", "Экспорт"), values = c("blue", "green")) +
  geom_text(data = plot_data[plot_data$Показатель == "МетЭкспорт",], aes(x = Регион, y = Значение, 
            label = paste(-Значение, " (", 
                  round(-Значение / data_sum$СуммарныйЭкспорт * 100, digits = 1), 
                  "%)", sep = "")), 
            hjust = 1, nudge_y = -50, nudge_x = 0.26, color = "dark green") +
  geom_text(data = plot_data[plot_data$Показатель == "МетИмпорт",], aes(x = Регион, y = Значение, 
            label = paste(Значение, " (", 
                  round(Значение / data_sum$СуммарныйИмпорт * 100, digits = 1), 
                  "%)", sep = "")), 
            hjust = 0, nudge_y = 50, nudge_x = -0.23, color = "dark blue") +
  coord_flip(ylim = c(-6600, 7600)) +
  ggtitle("Суммарный экспорт и импорт металлургической промышленности") +
  ylab('Объем, млн долл. США') + theme(legend.position='top') +
  labs(caption = "В скобках указана доля, которую экспорт (импорт) металлургической
       промышленности соcтавляет в суммарном экспорте (импорте) по региону") +
  theme(plot.caption = element_text(hjust = 0))