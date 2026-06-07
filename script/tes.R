library(dplyr)
library(ggplot2)


# memahami dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")
str(data)
head(data)
dim(data)


summary(data)
sapply(data, class)

options(scipen = 999)

numerik <- data[sapply(data, is.numeric)]

statistik_deskriptif <- data.frame(
  Variabel = names(select(data, where(is.numeric))),
  Mean = sapply(select(data, where(is.numeric)), mean, na.rm = TRUE),
  Median = sapply(select(data, where(is.numeric)), median, na.rm = TRUE),
  Min = sapply(select(data, where(is.numeric)), min, na.rm = TRUE),
  Q1 = sapply(select(data, where(is.numeric)), quantile, probs = 0.25, na.rm = TRUE),
  Q2 = sapply(select(data, where(is.numeric)), quantile, probs = 0.50, na.rm = TRUE),
  Q3 = sapply(select(data, where(is.numeric)), quantile, probs = 0.75, na.rm = TRUE),
  Max = sapply(select(data, where(is.numeric)), max, na.rm = TRUE),
  SD = sapply(select(data, where(is.numeric)), sd, na.rm = TRUE),
  Varians = sapply(select(data, where(is.numeric)), var, na.rm = TRUE)
)

statistik_deskriptif

# Missing value (median imputation) (1.3.3)
missing_value <- colSums(is.na(data))
missing_value

data <- data %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

colSums(is.na(data))

# 0utlier (1.3.4)
boxplot(data$pdrb_perkapita,
        main = "Boxplot PDRB per Kapita",
        ylab = "Nilai",
        col = "Red")

boxplot(data$kemiskinan,
        main = "Boxplot Kemiskinan",
        ylab = "Nilai",
        col = "Red")

boxplot(data$pengangguran,
        main = "Boxplot Pengangguran",
        ylab = "Nilai",
        col = "Red")

boxplot(data$ipm,
        main = "Boxplot IPM",
        ylab = "Nilai",
        col = "Red")

boxplot(data$harapan_hidup,
        main = "Boxplot Harapan Hidup",
        ylab = "Nilai",
        col = "Red")

boxplot(data$rata_lama_sekolah,
        main = "Boxplot Rata-rata Lama Sekolah",
        ylab = "Nilai",
        col = "Red")

boxplot(data$akses_internet,
        main = "Boxplot Akses Internet",
        ylab = "Nilai",
        col = "Red")

boxplot(data$jalan_baik,
        main = "Boxplot Jalan Baik",
        ylab = "Nilai",
        col = "Red")

boxplot(data$air_bersih,
        main = "Boxplot Air Bersih",
        ylab = "Nilai",
        col = "Red")

iqr_pdrb <- IQR(data$pdrb_perkapita)
batas_bawah <- quantile(data$pdrb_perkapita, 0.25) - (1.5 * iqr_pdrb)
batas_atas <- quantile(data$pdrb_perkapita, 0.75) + (1.5 * iqr_pdrb)
data <- data %>%
  filter(pdrb_perkapita >= batas_bawah & pdrb_perkapita <= batas_atas)

iqr_kemiskinan <- IQR(data$kemiskinan)
batas_bawah <- quantile(data$kemiskinan, 0.25) - (1.5 * iqr_kemiskinan)
batas_atas <- quantile(data$kemiskinan, 0.75) + (1.5 * iqr_kemiskinan)
data <- data %>%
  filter(kemiskinan >= batas_bawah & kemiskinan <= batas_atas)

iqr_datapengangguran <- IQR(data$pengangguran)
batas_bawah <- quantile(data$pengangguran, 0.25) - (1.5 * iqr_datapengangguran)
batas_atas <- quantile(data$pengangguran, 0.75) + (1.5 * iqr_datapengangguran)
data <- data %>%
  filter(pengangguran >= batas_bawah & pengangguran <= batas_atas)

# Visualisasi data (1.3.5)

# 1. Visualisasi Barchart
daftarTahun <- levels(factor(data$tahun))
dataLaporan2020 <- data[data$tahun == 2020,]
dataLaporan2021 <- data[data$tahun == 2021,]
dataLaporan2022 <- data[data$tahun == 2022,]
dataLaporan2023 <- data[data$tahun == 2023,]
dataLaporan2024 <- data[data$tahun == 2024,]

data_visualisasi1 <- data.frame(
  tahun = daftarTahun,
  datalaporan1 = c(nrow(dataLaporan2020), 
                      nrow(dataLaporan2021), 
                      nrow(dataLaporan2022),
                      nrow(dataLaporan2023),
                      nrow(dataLaporan2024)
                      )
)

data_visualisasi1

ggplot(data_visualisasi1, aes(x=tahun, y=datalaporan1)) +
  geom_col()+
  labs(x="Tahun",
       y="Laporan",
       title = "Jumlah Laporan dari tahun 2020 sampai 2024"
       )

# 2. Visualisasi Histogram
data %>%
  select(rata_lama_sekolah) %>%
  ggplot(aes(x = rata_lama_sekolah))+
  geom_histogram(
    bins = 15,
    fill = "#4E79A7",
    color = "white",
    linewidth = 0.5
  )+
  labs(x="Rata Lama Sekolah",
       y="Frekuensi",
       title = "Distribusi rata lama sekolah")
  

# 3. Visualisasi Scatter Plot
data %>%
  select(kemiskinan, pdrb_perkapita) %>%
  ggplot(aes(kemiskinan, log10(pdrb_perkapita)))+
  geom_point()+
  labs(x="kemiskinan",
       y="pdrb_perkapita",
       title = "Pengaruh kemiskinan terhadap pdrb_perkapita")+
  geom_smooth(method = "lm", se=FALSE)




# 4. Visualisasi Scatter Plot
data %>%
  select(kemiskinan, rata_lama_sekolah) %>%
  ggplot(aes(kemiskinan, rata_lama_sekolah))+
  geom_point()+
  labs(x="kemiskinan",
       y="rata lama sekolah",
       title = "Pengaruh kemiskinan terhadap rata lama sekolah")+
  geom_smooth(method = "lm", se=FALSE)



# 5. Visualisai Barchart
daftarProvinsi <- levels(factor(data$provinsi))
dataLaporanBanten <- data[data$provinsi == "Banten",]
dataLaporanJawaTimur <- data[data$provinsi == "Jawa Timur",]
dataLaporanJakarta <- data[data$provinsi == "DKI Jakarta",]
dataLaporanJawaBarat <- data[data$provinsi == "Jawa Barat",]
dataLaporanJawaTengah <- data[data$provinsi == "Jawa Tengah",]
dataLaporanKalimantanTimur <- data[data$provinsi == "Kalimantan Timur",]
dataLaporanPapua <- data[data$provinsi == "Papua",]
dataLaporanSulawesiSelatan <- data[data$provinsi == "Sulawesi Selatan",]
dataLaporanSumateraBarat <- data[data$provinsi == "Sumatera Barat",]
dataLaporanSumateraUtara <- data[data$provinsi == "Sumatera Utara",]

View(dataLaporanJawaBarat)
data_visualisasi5 <- data.frame(
  provinsi = daftarProvinsi,
  datalaporan2 = c(nrow(dataLaporanBanten), 
                      nrow(dataLaporanJakarta), 
                      nrow(dataLaporanJawaBarat),
                      nrow(dataLaporanJawaTengah),
                      nrow(dataLaporanJawaTimur),
                      nrow(dataLaporanKalimantanTimur),
                      nrow(dataLaporanPapua),
                      nrow(dataLaporanSulawesiSelatan),
                      nrow(dataLaporanSumateraBarat),
                      nrow(dataLaporanSumateraUtara)
  )
)


ggplot(data_visualisasi5, aes(x=provinsi, y=datalaporan2)) +
  geom_col()+
  labs(x="Tahun",
       y="Laporan",
       title = "Jumlah Laporan dari berbagai provinsi dari tahun 2020 sampai 2024"
  )



# Analisis Korelasi (1.3.7)

data %>% 
  select(tahun, kemiskinan, pengangguran, harapan_hidup, ipm, rata_lama_sekolah, 
         pdrb_perkapita, akses_internet, jalan_baik, air_bersih) %>%
  cor()
  
cor.test(data$kemiskinan, data$rata_lama_sekolah, method = "pearson")


# Analisis Probabilitas dan Distribusi Data

data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")

hasil_awal <- data.frame(
  Variabel = "kemiskinan",
  Mean     = mean(data$kemiskinan, na.rm = TRUE),
  Median   = median(data$kemiskinan, na.rm = TRUE),
  Min      = min(data$kemiskinan, na.rm = TRUE),
  Q1       = as.numeric(quantile(data$kemiskinan, 0.25, na.rm = TRUE)),
  Q3       = as.numeric(quantile(data$kemiskinan, 0.75, na.rm = TRUE)),
  Max      = max(data$kemiskinan, na.rm = TRUE),
  SD       = sd(data$kemiskinan, na.rm = TRUE),
  Varians  = var(data$kemiskinan, na.rm = TRUE)
)

print("=== STATISTIK DESKRIPTIF AWAL ===")
print(hasil_awal)


# Imputasi nilai NA khusus pada variabel kemiskinan menggunakan nilai tengah
data <- data %>%
  mutate(kemiskinan = ifelse(is.na(kemiskinan), median(kemiskinan, na.rm = TRUE), kemiskinan))


# Menghitung batas pencilan untuk variabel kemiskinan
iqr_kemiskinan <- IQR(data$kemiskinan)
batas_bawah    <- quantile(data$kemiskinan, 0.25) - (1.5 * iqr_kemiskinan)
batas_atas     <- quantile(data$kemiskinan, 0.75) + (1.5 * iqr_kemiskinan)

# Menyaring data untuk mendapatkan dataset yang bersih
data_bersih <- data %>%
  filter(kemiskinan >= batas_bawah & kemiskinan <= batas_atas)


mean_kemiskinan <- mean(data_bersih$kemiskinan)
sd_kemiskinan   <- sd(data_bersih$kemiskinan)

hasil_akhir <- data.frame(
  Variabel = "kemiskinan",
  Mean_Mu  = mean_kemiskinan,
  Median   = median(data_bersih$kemiskinan),
  SD_Sigma = sd_kemiskinan,
  Min      = min(data_bersih$kemiskinan),
  Max      = max(data_bersih$kemiskinan)
)

print("=== PARAMETER DISTRIBUSI DATA AKHIR ===")
print(hasil_akhir)


# A. Histogram & Grafik Fungsi Kepadatan Probabilitas (PDF)
hist(data_bersih$kemiskinan, probability = TRUE, 
     main = "Perbandingan Distribusi Peluang Kemiskinan", 
     xlab = "Tingkat Kemiskinan (%)", col = "lightblue", border = "white",
     ylim = c(0, max(density(data_bersih$kemiskinan)$y) * 1.2))

# Kurva Densitas Empiris (Garis Merah berdasarkan data riil)
lines(density(data_bersih$kemiskinan), col = "red", lwd = 2)

# Kurva Distribusi Normal Teoritis Gauss (Garis Biru Putus-putus)
x_axis <- seq(min(data_bersih$kemiskinan), max(data_bersih$kemiskinan), length = 100)
y_axis <- dnorm(x_axis, mean = mean_kemiskinan, sd = sd_kemiskinan)
lines(x_axis, y_axis, col = "blue", lwd = 2, lty = 2)

legend("topright", legend = c("Empiris (Data Riil)", "Teoritis (Normal Gauss)"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)

# B. Q-Q Plot untuk Uji Validitas Distribusi Normal
qqnorm(data_bersih$kemiskinan, main = "Q-Q Plot Uji Normalitas Kemiskinan")
qqline(data_bersih$kemiskinan, col = "red", lwd = 2)


# Kasus Skenario: Berapa peluang suatu wilayah memiliki tingkat kemiskinan di bawah 10%?
batas_kasus <- 10

# A. Probabilitas Empiris (Berdasarkan frekuensi relatif data lapangan)
prob_empiris <- mean(data_bersih$kemiskinan < batas_kasus)

# B. Probabilitas Teoritis (Berdasarkan luasan kurva Distribusi Normal / CDF)
prob_teoritis <- pnorm(batas_kasus, mean = mean_kemiskinan, sd = sd_kemiskinan)

# C. Inverse CDF (Mencari nilai kritis untuk ambang batas peluang 15% terendah)
nilai_kritis_15 <- qnorm(0.15, mean = mean_kemiskinan, sd = sd_kemiskinan)


cat("\n==================================================\n")
cat("=== ANALISIS ELEMEN PROBABILITAS KEMISKINAN ===\n")
cat("==================================================\n")
cat("1. Peluang Empiris P(X < 10%)  :", prob_empiris, "\n")
cat("2. Peluang Teoritis P(X < 10%) :", prob_teoritis, "\n")
cat("3. Batas nilai kemiskinan untuk peluang kumulatif 15% terendah:", nilai_kritis_15, "%\n")
cat("==================================================\n")
