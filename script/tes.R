library(dplyr)
library(ggplot2)

# memahami dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")
str(data)
head(data)
dim(data)


summary(data)


statistik_variabel_terpilih <- data.frame(
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

statistik_variabel_terpilih

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
  
cor.test(data$pengangguran, data$harapan_hidup, method = "pearson")


# Analisis Probabilitas dan Distribusi Data

data_bersih <- data 

hasil_akhir <- data.frame(
  Variabel = c("kemiskinan", "rata_lama_sekolah", "pengangguran"),
  
  Mean = c(
    mean(data_bersih$kemiskinan),
    mean(data_bersih$rata_lama_sekolah),
    mean(data_bersih$pengangguran)
  ),
  Median = c(
    median(data_bersih$kemiskinan),
    median(data_bersih$rata_lama_sekolah),
    median(data_bersih$pengangguran)
  ),
  Min = c(
    min(data_bersih$kemiskinan),
    min(data_bersih$rata_lama_sekolah),
    min(data_bersih$pengangguran)
  ),
  Max = c(
    max(data_bersih$kemiskinan),
    max(data_bersih$rata_lama_sekolah),
    max(data_bersih$pengangguran)
  )
)

print(hasil_akhir)

# Hubungan 1: Apakah pendidikan berpengaruh ke kemiskinan?
korelasi_pendidikan_kemiskinan <- cor(data_bersih$rata_lama_sekolah, data_bersih$kemiskinan)
print(paste("Korelasi Pendidikan & Kemiskinan:", korelasi_pendidikan_kemiskinan))

# Hubungan 2: Apakah pengangguran berpengaruh ke kemiskinan?
korelasi_pengangguran_kemiskinan <- cor(data_bersih$pengangguran, data_bersih$kemiskinan)
print(paste("Korelasi Pengangguran & Kemiskinan:", korelasi_pengangguran_kemiskinan))



# Grafik Hubungan Pendidikan dan Kemiskinan
plot(data_bersih$rata_lama_sekolah, data_bersih$kemiskinan,
     main = "Hubungan Tingkat Pendidikan dengan Tingkat Kemiskinan",
     xlab = "Rata-rata Lama Sekolah (Tahun)",
     ylab = "Tingkat Kemiskinan (%)",
     col = "blue", 
     pch = 16)


abline(lm(kemiskinan ~ rata_lama_sekolah, data = data_bersih), col = "red", lwd = 2)


plot(data_bersih$pengangguran, data_bersih$kemiskinan,
     main = "Hubungan Tingkat Pengangguran dengan Tingkat Kemiskinan",
     xlab = "Tingkat Pengangguran (%)",
     ylab = "Tingkat Kemiskinan (%)",
     col = "darkgreen", 
     pch = 16)

abline(lm(kemiskinan ~ pengangguran, data = data_bersih), col = "red", lwd = 2)

print(hasil_akhir)



#Distribusi Variabel Kemiskinan
hist(data_bersih$kemiskinan, probability = TRUE, 
     main = "Distribusi Probabilitas Kemiskinan", 
     xlab = "Tingkat Kemiskinan (%)", col = "lightblue", border = "white")
lines(density(data_bersih$kemiskinan), col = "red", lwd = 2)

#Distribusi Variabel Rata-rata Lama Sekolah
hist(data_bersih$rata_lama_sekolah, probability = TRUE, 
     main = "Distribusi Probabilitas Rata-rata Lama Sekolah", 
     xlab = "Lama Sekolah (Tahun)", col = "lightgreen", border = "white")
lines(density(data_bersih$rata_lama_sekolah), col = "red", lwd = 2)



# Jika titik-titik mengikuti garis merah, maka data berdistribusi normal
qqnorm(data_bersih$kemiskinan, main = "Q-Q Plot Variabel Kemiskinan")
qqline(data_bersih$kemiskinan, col = "red", lwd = 2)
