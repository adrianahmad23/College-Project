#Tugas Smoothing Perubahan Harga Emas (Adrian Ahmad)
library(ggplot2)
library(dplyr)

#Mengimpor File
file <- "D:\\Adrian's file\\Universitas Brawijaya\\SEM2\\Teknik Visualisasi Data\\Tugas Smoothing\\Harga Emas.csv"
emas <- read.csv(file)

#Mengubah format atribut 'Date' menjadi format tanggal
emas$Date <- as.Date(emas$Date, "%d-%m-%Y")

#Plot (before smoothing)
ggplot(data = emas, aes(x = Date, y = United.States.USD.)) +
  geom_point(size = 1) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Tahun",
       y = "Harga (USD)",
       title = "Harga Emas di Amerika",
       subtitle = "Januari 1990 - Juli 2021")

#Bin Smoothing
span <- 600
fit <- with(emas, 
            ksmooth(Date, United.States.USD., kernel = "box", bandwidth = span))

smoothed_df <- emas %>%
  mutate(smooth = fit$y)

ggplot() +
  geom_point(data = emas, aes(Date, United.States.USD.), size = 2, alpha = 0.5, color = "grey") +
  geom_line(data = smoothed_df, aes(Date, smooth), color = "blue") +
  labs(x = "Tahun",
       y = "Harga (USD)",
       title = "Harga Emas di Amerika - Bin Smoothing",
       subtitle = "Januari 1990 - Juli 2021")

#Kernel Smoothing
span <- 100
fitcb <- with(ihsg, ksmooth(Bulan, IHSG, kernel = "normal", bandwidth = span))

smoothed_values <- approx(fitcb$x, fitcb$y, xout = ihsg$Bulan)$y

smoothed_df <- ihsg %>%
  mutate(smooth = smoothed_values)

ggplot() +
  geom_point(data = ihsg, aes(Bulan, IHSG), size = 2, alpha = 0.5, color = "grey") +
  geom_line(data = smoothed_df, aes(Bulan, smooth), color = "red") +
  labs(x = "Tahun",
       y = "Harga (USD)",
       title = "Harga Emas di Amerika - Kernel Smoothing",
       subtitle = "Januari 1990 - Juli 2021")


#Perbandingan
span_bin <- 100
fit_bin <- with(emas, 
                ksmooth(Date, United.States.USD., kernel = "box", bandwidth = span_bin))
smoothed_df_bin <- emas %>%
  mutate(smooth_bin = fit_bin$y)

# Kernel Smoothing
span_kernel <- 600
fit_kernel <- with(emas, 
                   ksmooth(Date, United.States.USD., kernel = "normal", bandwidth = span_kernel))
smoothed_df_kernel <- emas %>%
  mutate(smooth_kernel = fit_kernel$y)

# Combine smoothed dataframes
combined_smoothed_df <- cbind(smoothed_df_bin, smooth_kernel = smoothed_df_kernel$smooth_kernel)

# Plot combined smoothing lines
ggplot(combined_smoothed_df, aes(Date)) +
  geom_point(aes(y = United.States.USD.), size = 2, alpha = 0.5, color = "grey") +
  geom_line(aes(y = smooth_bin), size = 0.5,color = "blue") +
  geom_line(aes(y = smooth_kernel), size = 0.5,color = "red") +
  labs(x = "Tahun",
       y = "Harga (USD)",
       title = "Harga Emas di Amerika - Combined Smoothing",
       subtitle = "Januari 1990 - Juli 2021")
