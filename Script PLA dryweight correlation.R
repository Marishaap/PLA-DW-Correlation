setwd(r"(C:/Users/Gebruiker/Downloads/Thesis data)")

Data_round1 <- read.table(
  file="Cleaned_data_round1.csv",
  header = TRUE,
  sep = ","
)

library(dplyr)

Data_round1 <- Data_round1 %>%
  filter(Round.Order == 103)

LowP <- Data_round1 %>%
  filter(TREATMENT == "LP") %>%
  select(LK_ID, Size, Dryweight)

LowP_clean <- LowP %>%
  group_by(LK_ID) %>%
  summarise(
    Size = mean(Size, na.rm = TRUE),
    Dryweight = mean(Dryweight, na.rm = TRUE)
  )


LowP_clean <- LowP_clean %>%
  filter(!is.na(Dryweight))


correlatie <- cor(LowP_clean$Size, LowP_clean$Dryweight, method = "spearman")

print(paste("Spearman correlatie tussen Size en Dryweight:", round(correlatie, 3)))

library(ggplot2)

ggplot(LowP_clean, aes(x = Size, y = Dryweight)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatterplot van Size vs Dryweight",
    x = "Size",
    y = "Dryweight"
  ) +
  theme_minimal()

