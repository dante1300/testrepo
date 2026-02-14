d1 <- c(33,90.499,90.318,88.997,15,88.997)
d2 <- c(44,90.617,90.085,89.385,18,89.385)
d3 <- c(77,91.200,90.186,89.586,17,89.586)
d4 <- c(16,90.691,90.010,89.678,15,89.678)
d5 <- c(10,90.848,90.513,89.809,15,89.809)
d6 <- c(3,90.795,90.222,89.927,18,89.927)
d7 <- c(4,90.902,90.099,89.974,18,89.974)
d8 <- c(55,91.653,90.009,90.215,17,90.215)
d9 <- c(14,90.863,90.595,90.249,15,90.249)
d10 <- c(18,91.261,90.624,90.601,15,90.601)
d11 <- c(11,91.165,90.659,NA,11,90.659)
d12 <- c(99,90.998,90.708,NA,12,90.708)
d13 <- c(22,90.607,91.203,NA,9,91.203)
d14 <- c(7,91.547,91.238,NA,12,91.238)
d15 <- c(63,91.316,93.430,NA,11,93.430)
d16 <- c(31,91.724,NA,NA,6,91.724)
d17 <- c(6,91.936,NA,NA,8,91.936)
d18 <- c(5,92.056,NA,NA,6,92.056)
d19 <- c(47,97.213,NA,NA,6,97.213)
d20 <- c(9,97.325,NA,NA,7,97.325)

bahrain21 <- matrix(c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13,
                      d14, d15, d16, d17, d18, d19, d20), nrow = 20, byrow = TRUE)

rownames(bahrain21) <- paste0("Driver ", seq_len(nrow(bahrain21)))
colnames(bahrain21) <- c("Nr.", paste0("Q", seq_len(3)), "Laps",
                         "Final Q. Time")

mean(bahrain21[,"Q1"])
min(bahrain21[,"Q1"])
time107 <- min(bahrain21[,"Q1"]) * 1.07
bahrain21 <- cbind(bahrain21, bahrain21[,"Q1"] <= time107)
colnames(bahrain21)[7] <- "Qualified?"
bahrain21

library(ggplot2)
library(dplyr)

bahrain21_df <- as.data.frame(bahrain21)

# Convert relevant columns to numeric
bahrain21_df$Q1 <- as.numeric(bahrain21_df$Q1)
bahrain21_df$Q2 <- as.numeric(bahrain21_df$Q2)
bahrain21_df$Q3 <- as.numeric(bahrain21_df$Q3)
bahrain21_df$`Final Q. Time` <- as.numeric(bahrain21_df$`Final Q. Time`)

# Add driver names from rownames
bahrain21_df$Driver <- rownames(bahrain21_df)

ggplot(bahrain21_df, aes(x = reorder(Driver, Q1), y = Q1)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = time107, color = "red", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Bahrain 2021 Qualifying - Q1 Times",
    x = "Driver",
    y = "Lap Time (seconds)"
  ) +
  theme_minimal()






ggplot(bahrain21_df, aes(x = reorder(Driver, Q1), y = Q1, fill = `Qualified?`)) +
  geom_col() +
  geom_hline(yintercept = time107, color = "black", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Bahrain 2021 Q1 - 107% Qualification Rule",
    x = "Driver",
    y = "Lap Time (seconds)",
    fill = "Qualified"
  ) +
  theme_minimal()




library(tidyr)

long_df <- bahrain21_df %>%
  select(Driver, Q1, Q2, Q3) %>%
  pivot_longer(cols = Q1:Q3, 
               names_to = "Session", 
               values_to = "Time")

ggplot(long_df, aes(x = Driver, y = Time, color = Session)) +
  geom_point(size = 3) +
  coord_flip() +
  labs(
    title = "Bahrain 2021 Qualifying Comparison",
    x = "Driver",
    y = "Lap Time (seconds)"
  ) +
  theme_minimal()