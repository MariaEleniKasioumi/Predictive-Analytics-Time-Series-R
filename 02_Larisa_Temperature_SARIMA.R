library(forecast)
library(tseries)
library(ggplot2)

#setwd("C:/Users/Mkasi/Downloads/PROJECTS/BprojectFILIPPAKHS/Askisi_2")
csv_path <- "larisa_mean_temperature_timeseries_FINAL.csv"
raw <- read.csv(csv_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

raw <- raw[order(raw$year, raw$month), ]

y <- ts(raw$Mean_Temperature, start = c(2016, 1), frequency = 12)

cat("Παρατηρήσεις:", length(y), "| Κενές τιμές:", sum(is.na(y)), "\n")
if (anyNA(y)) {
  stop("Υπάρχουν κενές τιμές στη χρονοσειρά. Ελέγξτε το αρχείο CSV.")
}
print(y)

dir.create("plots", showWarnings = FALSE)

save_base <- function(filename, draw_fun, width = 1800, height = 900, res = 200) {
  draw_fun()                                                   
  png(file.path("plots", filename), width = width, height = height, res = res)
  draw_fun()                                                   
  dev.off()
  invisible(NULL)
}

show_save_gg <- function(p, filename, width = 9, height = 5, dpi = 300) {
  print(p)                                                     
  ggsave(file.path("plots", filename), plot = p, width = width, height = height, dpi = dpi)
  invisible(p)
}

acf_pacf <- function(series, label) {
  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  acf (series, lag.max = 36, main = paste("ACF",  label), xlab = "Lag (έτη· 1.0 = 12 μήνες)")
  pacf(series, lag.max = 36, main = paste("PACF", label), xlab = "Lag (έτη· 1.0 = 12 μήνες)")
  par(op)
}

plot_df <- data.frame(
  date = seq(as.Date("2016-01-01"), by = "month", length.out = length(y)),
  temp = as.numeric(y)
)
plot_df$year      <- format(plot_df$date, "%Y")
plot_df$month_num <- as.integer(format(plot_df$date, "%m"))
plot_df$month     <- factor(plot_df$month_num, levels = 1:12,
                            labels = c("Ιαν","Φεβ","Μαρ","Απρ","Μάι","Ιουν",
                                       "Ιουλ","Αυγ","Σεπ","Οκτ","Νοε","Δεκ"))

p_2a_timeseries <- ggplot(plot_df, aes(x = date, y = temp)) +
  geom_line(color = "tomato", linewidth = 0.7) +
  labs(title = "Μέση Μηνιαία Θερμοκρασία Λάρισας (ΕΜΥ)",
       x = "Έτος", y = "Θερμοκρασία (°C)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
show_save_gg(p_2a_timeseries, "2a_1_timeseries.png")

monthly_means_df <- aggregate(temp ~ month_num + month, data = plot_df, FUN = mean)
p_2a_monthly <- ggplot(monthly_means_df, aes(x = month, y = temp, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 2.5) +
  labs(title = "Μέση Θερμοκρασία ανά Μήνα στη Λάρισα (2016–2025)",
       x = "Μήνας", y = "Μέση θερμοκρασία (°C)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
show_save_gg(p_2a_monthly, "2a_2_monthly_means.png", width = 8)

p_2a_seasonal <- ggplot(plot_df, aes(x = month, y = temp, group = year, color = year)) +
  geom_line(linewidth = 0.8, alpha = 0.75) +
  geom_point(size = 1.5, alpha = 0.75) +
  labs(title = "Εποχικό Διάγραμμα Μέσης Θερμοκρασίας στη Λάρισα",
       x = "Μήνας", y = "Μέση θερμοκρασία (°C)", color = "Έτος") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))
show_save_gg(p_2a_seasonal, "2a_3_seasonal.png")

save_base("2b_acf_pacf_initial.png", function() acf_pacf(y, "Αρχικής Σειράς"))

nsdiffs(y)  
ndiffs(y)   

sy <- diff(y, lag = 12)

sy_df <- data.frame(
  date  = seq(as.Date("2017-01-01"), by = "month", length.out = length(sy)),
  sdiff = as.numeric(sy)
)
p_2g_sdiff <- ggplot(sy_df, aes(x = date, y = sdiff)) +
  geom_line(color = "darkgreen", linewidth = 0.8) +
  geom_point(color = "darkgreen", size = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Πρώτες Εποχικές Διαφορές της Μέσης Θερμοκρασίας",
       x = "Έτος", y = "Εποχική διαφορά (°C)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
show_save_gg(p_2g_sdiff, "2c_1_seasonal_differences.png")

save_base("2c_2_acf_pacf_seasonal_diff.png", function() acf_pacf(sy, "Εποχικών Διαφορών"))

adf.test(sy)   

best_model <- auto.arima(y, seasonal = TRUE,
                         stepwise = FALSE, approximation = FALSE)
summary(best_model)

fit_safe <- function(...) tryCatch(Arima(y, ...), error = function(e) NULL)

cand <- list(
  "auto.arima"               = best_model,
  "SARIMA(1,0,0)(0,1,1)[12]" = fit_safe(order = c(1,0,0), seasonal = list(order = c(0,1,1), period = 12)),
  "SARIMA(0,0,1)(0,1,1)[12]" = fit_safe(order = c(0,0,1), seasonal = list(order = c(0,1,1), period = 12)),
  "SARIMA(0,0,0)(0,1,1)[12]" = fit_safe(order = c(0,0,0), seasonal = list(order = c(0,1,1), period = 12)),
  "SARIMA(1,0,1)(0,1,1)[12]" = fit_safe(order = c(1,0,1), seasonal = list(order = c(0,1,1), period = 12)),
  "SARIMA(1,0,0)(1,1,0)[12]" = fit_safe(order = c(1,0,0), seasonal = list(order = c(1,1,0), period = 12)),
  "SARIMA(0,0,1)(1,1,0)[12]" = fit_safe(order = c(0,0,1), seasonal = list(order = c(1,1,0), period = 12))
)
cand <- cand[!sapply(cand, is.null)]    

model_comparison <- data.frame(
  Model = names(cand),
  AIC   = sapply(cand, AIC),
  BIC   = sapply(cand, BIC),
  row.names = NULL
)
print(model_comparison)


model_base <- best_model
ord <- arimaorder(model_base)     


if (adf.test(sy)$p.value > 0.05) {
  dsy <- diff(sy)
  save_base("2e_acf_pacf_double_diff.png", function() acf_pacf(dsy, "Διπλών Διαφορών"))
  print(adf.test(dsy))
} else {
  cat("Οι εποχικές διαφορές είναι στάσιμες (ADF p < 0.05) -> αρκεί D = 1, d = 0.\n")
}


ext <- list(
  "Base: SARIMA(1,0,0)(0,1,1)[12]" = model_base,
  "SARIMA(2,0,0)(0,1,1)[12]"       = fit_safe(order = c(2,0,0), seasonal = list(order = c(0,1,1), period = 12)),
  "SARIMA(1,0,1)(0,1,1)[12]"       = fit_safe(order = c(1,0,1), seasonal = list(order = c(0,1,1), period = 12)),
  "SARIMA(1,0,0)(1,1,1)[12]"       = fit_safe(order = c(1,0,0), seasonal = list(order = c(1,1,1), period = 12)),
  "SARIMA(2,0,1)(0,1,1)[12]"       = fit_safe(order = c(2,0,1), seasonal = list(order = c(0,1,1), period = 12))
)
ext <- ext[!sapply(ext, is.null)]    

extended_comparison <- data.frame(
  Model = names(ext),
  AIC   = sapply(ext, AIC),
  BIC   = sapply(ext, BIC),
  row.names = NULL
)
print(extended_comparison)


final_model <- ext[[which.min(extended_comparison$AIC)]]
ford <- arimaorder(final_model)
cat(sprintf("\nSelected final model: SARIMA(%d,%d,%d)(%d,%d,%d)[%d]\n",
            ford[1], ford[2], ford[3], ford[4], ford[5], ford[6], ford[7]))
summary(final_model)


save_base("2z_residual_diagnostics_final.png", function() checkresiduals(final_model))


Box.test(residuals(final_model), lag = 24, type = "Ljung-Box",
         fitdf = ford[1] + ford[3] + ford[4] + ford[6])



fc <- forecast(final_model, h = 24)
print(fc)

p_forecast <- autoplot(fc) +
  ggtitle("Πρόβλεψη Μέσης Μηνιαίας Θερμοκρασίας στη Λάρισα (24 μήνες)") +
  xlab("Έτος") + ylab("Μέση θερμοκρασία (°C)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
show_save_gg(p_forecast, "2h_forecast_24m.png")


cat("\nΟλοκληρώθηκε. Τα διαγράμματα αποθηκεύτηκαν στον υποφάκελο 'plots'.\n")