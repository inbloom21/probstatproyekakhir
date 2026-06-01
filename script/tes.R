library(dplyr)
library(ggplot2)

# memahami dataset
data <- read.csv("dataset/pembangunan_wilayah_missing_outlier.csv")
str(data)
head(data)
dim(data)

# statistika deksriptif (1.3.1)
summary(data)

# missing value (median imputation) (1.3.3)
missing_value <- colSums(is.na(data))
missing_value

data <- data %>%
  mutate(across(where(is.numeric), 
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

colSums(is.na(data))

# outlier (1.3.4)
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

# Visualisasi data (1.3.5)

# 1. Visualisasi Barchat
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

# 2. Visualisasi Scatter Plot
data %>%
  select(pengangguran, pdrb_perkapita) %>%
  ggplot(aes(pengangguran, pdrb_perkapita))+
  geom_point()+
  labs(x="Pengangguran",
       y="pdrb_perkapita",
       title = "Pengaruh pengangguran terhadap pdrb_perkapita")
  

# 3. Visualisasi Sactter Plot
data %>%
  select(kemiskinan, pdrb_perkapita) %>%
  ggplot(aes(kemiskinan, scale(pdrb_perkapita)))+
  geom_point()+
  labs(x="Pengangguran",
       y="pdrb_perkapita",
       title = "Pengaruh pengangguran terhadap pdrb_perkapita")




# 4. Visualisasi Box Plot
data %>%
  select(ipm) %>%
  ggplot(aes(y=ipm))+
  geom_boxplot()+
  labs(y="ipm",
       title = "Penyebaran data ipm")


# 5. Visualisai Barchat
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
  select(tahun, kemiskinan, pengangguran, harapan_hidup, ipm, rata_lama_sekolah, pdrb_perkapita, 
         akses_internet, jalan_baik, air_bersih) %>%
  cor()
  
cor.test(data$pengangguran, data$harapan_hidup, method = "pearson")
