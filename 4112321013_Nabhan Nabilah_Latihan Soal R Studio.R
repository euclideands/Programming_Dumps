# soal no 1
# hitung rata-rata dan standar deviasi
waktu_kerusakan <- c(0.19, 0.78, 0.96, 1.31, 2.78, 3.16, 4.15, 4.67, 4.85, 6.50, 
                     7.35, 8.01, 8.27, 12.06, 31.75, 32.52, 33.91, 36.71, 72.89)

xbar1 <- mean(waktu_kerusakan)
xbar1

sd1 <- sd(waktu_kerusakan)
sd1

# soal no 2
# hitung rata-rata dan standar deviasi
kekuatan_tutup <- c(96, 96, 102, 202, 104, 104, 108, 126, 126, 128, 128, 140,
                    156, 160, 160, 164, 170)

xbar2 <- mean(kekuatan_tutup)
xbar2

sd2 <- sd(kekuatan_tutup)
sd2

# soal no 3
suhu <- c(84, 49, 61, 40, 83, 67, 45, 66, 70, 69, 80, 58, 68, 60, 67, 72, 73,
          70, 57, 63, 70, 78, 52, 67, 53, 67, 75, 61, 70, 81, 76, 79, 75, 76,
          58, 31)

# hitung rata-rata dan standar deviasi
xbar3 <- mean(suhu)
xbar3

sd3 <- sd(suhu)
sd3

# buat diagram titik
library(ggplot2)
ggplot(data=NULL, aes(x=suhu)) +
  geom_dotplot(fill="blue",
               color="black",
               binwidth=2) +
  theme_minimal() +
  labs(title="Data Temperatur Cincin 0 (Dalam Fahrenheit)",
       y="Proportion",
       x="Suhu (Fahrenheit)")

# jika amatan dengan suhu 31 derajat Fahrenheit tidak disertakan, apa yang dapat
# disimpulkan?
suhu2 <- c(84, 49, 61, 40, 83, 67, 45, 66, 70, 69, 80, 58, 68, 60, 67, 72, 73,
           70, 57, 63, 70, 78, 52, 67, 53, 67, 75, 61, 70, 81, 76, 79, 75, 76,
           58)

# hitung rata-rata dan standar deviasi
xbar4 <- mean(suhu2)
xbar4

sd4 <- sd(suhu2)
sd4

# buat diagram titik
library(ggplot2)
ggplot(data=NULL, aes(x=suhu2)) +
  geom_dotplot(fill="yellow",
               color="black",
               binwidth=2) +
  theme_minimal() +
  labs(title="Data Temperatur Cincin 0 (Dalam Fahrenheit)\nTanpa Suhu 31",
       y="Proportion",
       x="Suhu (Fahrenheit)")

# kesimpulan:

# perubahan Rata-rata (Mean):
# ketika amatan dengan suhu 31 derajat fahrenheit diikutkan, nilai rata-rata 
# adalah 65.86. jika tidak diikutkan maka nilai rata-rata adalah 66.86. 
# penghilangan suhu 31 derajat fahrenheit akan mempengaruhi nilai rata-rata 
# secara signifikan, mengingat suhu tersebut jauh di bawah rata-rata sebelumnya.

# perubahan standar deviasi:
# ketika amatan dengan suhu 31 derajat fahrenheit diikutkan, nilai standar 
# deviasi adalah 12.16. jika tidak diikutkan maka nilai standar deviasi adalah 
# 10.74. penghilangan suhu 31 derajat Fahrenheit akan mengurangi variasi dalam 
# data, sehingga standar deviasinya akan lebih rendah.

# perubahan diagram titik:
# diagram titik memiliki pola yang berbeda karena pengamatan suhu 31 derajat 
# fahrenheit tidak lagi ada. selain itu jumlah titik juga berubah karena 
# berkurang satu.
# range atau interval yang mencakup data yang baru berubah sesuai karena nilai 
# minimum pada data yang awalnya adalah 31 kini berubah menjadi 40.

# soal no 4
tingkat_oktan <- c(88.5, 91.1, 88.2, 91.8, 89.8, 
                   94.7, 86.7, 88.5, 88.4, 92.7, 
                   84.3, 93.4, 93.3, 92.6, 93.3, 
                   90.1, 96.1, 87.4, 93.7, 86.7,
                   89, 89.6, 91.1, 96.5, 91, 
                   89.8, 90.4, 90.5, 84.3, 90.9,
                   91.6, 91.6, 100.3, 93.2, 89.9, 
                   90.3, 90.7, 87.6, 88.6, 91.8,
                   90, 88.6, 92.7, 88.7, 89.7,
                   91.5, 88.3, 87.9, 92.7, 92.2,
                   89.9, 94.2, 93, 89.3, 87.7, 
                   98.8, 85.3, 94.4, 91, 91,
                   88.3, 90.1, 90.4, 87.5, 90.1,
                   90.4, 89.3, 91.2, 87.8, 88.9,
                   91.2, 91.1, 86.7, 88.3, 92.3,
                   90.6, 92.2, 94.2, 89.2, 90.8)

# diagram batang daun
diagram_batang_daun <- stem(tingkat_oktan)

# hitung rata-rata dan standar deviasi
xbar5 <- mean(tingkat_oktan)
xbar5

sd5 <- sd(tingkat_oktan)
sd5

# hitung median dan kuartil 
# First quartile (25%)
# Second quartile / Median (50%)
# Third quartile (75%)
res <- quantile(tingkat_oktan, probs = c(0.25, 0.5, 0.75))
res

# soal no 5
# data sama dengan soal no 4
# tabel frekuensi
freq_table <- table(tingkat_oktan)
freq_tabel_df <- as.data.frame(freq_table)
freq_tabel_df

# histogram
histogram <- hist(tingkat_oktan, main="Histogram Tingkat Oktan",
                  xlab="Tingkat Oktan", ylab="Frekuensi", 
                  col="blue", border="black")

# soal no 6
# data sama dengan soal no 4
# plot box-whisker
boxplot <- boxplot(tingkat_oktan, main="Box Plot Tingkat Oktan", 
                   xlab="Tingkat Oktan", 
                   col="blue", border="black",
                   horizontal=TRUE)

# apakah terdapat outlier? bagaimana cara handle outlier tersebut?
# pada box plot terlihat 2 titik di luar whisker, artinya terdapat 2 data yang 
# merupakan outlier

# handling outlier akan dilakukan dengan censoring menggunakan metode 
# IQR Proximity Rule

# hitung IQR
Q1 <- quantile(tingkat_oktan, 0.25)
Q3 <- quantile(tingkat_oktan, 0.75)
IQR <- Q3 - Q1

# Hitung batas bawah dan batas atas untuk outlier
lower_censoring_bound <- Q1 - 1.5 * IQR
upper_censoring_bound <- Q3 + 1.5 * IQR

# identifikasi outlier
outliers <- tingkat_oktan[tingkat_oktan < lower_bound | tingkat_oktan > upper_bound]
outliers

# Censor outliers
tingkat_oktan_censored <- tingkat_oktan
tingkat_oktan_censored[tingkat_oktan_censored < lower_censoring_bound] <- lower_censoring_bound
tingkat_oktan_censored[tingkat_oktan_censored > upper_censoring_bound] <- upper_censoring_bound

# data tingkat oktan yang sudah tidak memiliki outlier
tingkat_oktan_censored

# plot box-whisker setelah dilakukan censoring menggunakan metode IQR Proximity Rule
boxplot <- boxplot(tingkat_oktan_censored, main="Box Plot Tingkat Oktan (Censored)", 
                   xlab="Tingkat Oktan", 
                   col="blue", border="black",
                   horizontal=TRUE)




