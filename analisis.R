

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



# 1. Distribusi Variabel Kemiskinan
hist(data_bersih$kemiskinan, probability = TRUE, 
     main = "Distribusi Probabilitas Kemiskinan", 
     xlab = "Tingkat Kemiskinan (%)", col = "lightblue", border = "white")
lines(density(data_bersih$kemiskinan), col = "red", lwd = 2)

# 2. Distribusi Variabel Rata-rata Lama Sekolah
hist(data_bersih$rata_lama_sekolah, probability = TRUE, 
     main = "Distribusi Probabilitas Rata-rata Lama Sekolah", 
     xlab = "Lama Sekolah (Tahun)", col = "lightgreen", border = "white")
lines(density(data_bersih$rata_lama_sekolah), col = "red", lwd = 2)


# --- TAMBAHAN DISTRIBUSI: Q-Q PLOT UNTUK UJI NORMALITAS ---

# Jika titik-titik mengikuti garis merah, maka data berdistribusi normal
qqnorm(data_bersih$kemiskinan, main = "Q-Q Plot Variabel Kemiskinan")
qqline(data_bersih$kemiskinan, col = "red", lwd = 2)

