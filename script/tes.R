library(dplyr)

# memahami dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")
str(data)
head(data)
dim(data)

# statistika deksriptif
summary(data)

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

# missing value (median imputation)
missing_value <- colSums(is.na(data))
missing_value

data <- data %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

colSums(is.na(data))

# outlier
boxplot(data$pengangguran,
        main = "Boxplot Pengangguran",
        ylab = "Nilai",
        col = "Red")

boxplot(data$rata_lama_sekolah,
        main = "Boxplot Rata-rata Lama Sekolah",
        ylab = "Nilai",
        col = "Red")

iqr_datapengangguran <- IQR(data$pengangguran)

batas_bawah <- quantile(data$pengangguran, 0.25) - (1.5 * iqr_datapengangguran)
batas_atas <- quantile(data$pengangguran, 0.75) + (1.5 * iqr_datapengangguran)
data <- data %>%
  filter(pengangguran >= batas_bawah & pengangguran <= batas_atas)

iqr_datalamasekolah <- IQR(data$rata_lama_sekolah)
batas_bawah <- quantile(data$rata_lama_sekolah, 0.25) - (1.5 * iqr_datalamasekolah)
batas_atas <- quantile(data$rata_lama_sekolah, 0.75) + (1.5 * iqr_datalamasekolah)
data <- data %>%
  filter(rata_lama_sekolah >= batas_bawah & rata_lama_sekolah <= batas_atas)
