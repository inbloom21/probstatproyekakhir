library(dplyr)

# memahami dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")
str(data)
head(data)
dim(data)

# statistika deksriptif
summary(data)
data %>%
  summarise(
    rata_kemiskinan = mean(kemiskinan, na.rm = TRUE),
    median_kemiskinan = median(kemiskinan, na.rm = TRUE),
    sd_kemiskinan = sd(kemiskinan, na.rm = TRUE)
  )

