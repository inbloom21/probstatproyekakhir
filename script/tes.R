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

hasil <- data(
  Variabel = c("kemiskinan", "rata_lama_sekolah"),
  Mean = c(
    mean(data$kemiskinan, na.rm = TRUE),
    mean(data$rata_lama_sekolah, na.rm = TRUE)
  ),
  Median = c(
    median(data$kemiskinan, na.rm = TRUE),
    median(data$rata_lama_sekolah, na.rm = TRUE)
  ),
  Min = c(
    min(data$kemiskinan, na.rm = TRUE),
    min(data$rata_lama_sekolah, na.rm = TRUE)
  ),
  Q1 = c(
    quantile(data$kemiskinan, 0.25, na.rm = TRUE),
    quantile(data$rata_lama_sekolah, 0.25, na.rm = TRUE)
  ),
  Q3 = c(
    quantile(data$kemiskinan, 0.75, na.rm = TRUE),
    quantile(data$rata_lama_sekolah, 0.75, na.rm = TRUE)
  ),
  Max = c(
    max(data$kemiskinan, na.rm = TRUE),
    max(data$rata_lama_sekolah, na.rm = TRUE)
  ),
  SD = c(
    sd(data$kemiskinan, na.rm = TRUE),
    sd(data$rata_lama_sekolah, na.rm = TRUE)
  ),
  Varians = c(
    var(data$kemiskinan, na.rm = TRUE),
    var(data$rata_lama_sekolah, na.rm = TRUE)
  )
)

hasil
