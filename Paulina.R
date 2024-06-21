install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
data <- read_csv("C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/1C 2024 - Actualización técnica/RStudio Practice/AI_index_db.csv")
head(data)
data_filtered <- data %>%
  select(Country, Talent, Infrastructure) %>%
  slice(1:10) %>%
  pivot_longer(cols = c(Talent, Infrastructure), names_to = "Metric", values_to = "Score")
ggplot(data_filtered, aes(x = Country, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Score, 1)), position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_fill_manual(values = c("Talent" = "green", "Infrastructure" = "blue")) +
  labs(title = "Comparison of Talent and Infrastructure by Country",
       x = "Country",
       y = "Score",
       fill = "Metric") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)
ggsave("comparison_talent_infrastructure_colored.png")
