#1. Сгенерировать выборки объема N (параметр N задать произвольно в диапазоне от 100 до 200) следующих распределений:
#    Экспоненциальное,
#    Гамма-распределение.
#Для каждого распределения параметры задать самостоятельно.
rm(list = ls());
N <- sample(100:200, 1); # объём выборки
lambda_exp <- 0.5  # Параметр экспоненциального распределения
exp_sample <- rexp(N, rate = lambda_exp) # выборка экспоненциального распределения
alpha_gamma <- 3  # параметр α
beta_gamma <- 2   # параметр β
gamma_sample <- rgamma(N, shape = alpha_gamma, rate = beta_gamma) # выборка гамма-распределения
#2. Построить гистограмму относительных частот и график плотности соответствующего теоретического распределения и оценки плотности.
# Гистограмма и плотность для экспоненциального распределения
hist(exp_sample, breaks = 20, freq = FALSE, main = "Экспоненциальное распределение", xlab = "Значения", ylab = "Плотность") #гистограмма
lines(density(exp_sample), col = "red", lwd = 2) # ядерная оценка плотности
curve(dexp(x, rate = lambda_exp), add = TRUE, col = "blue", lwd = 2) # теоретическая плотсность

hist(gamma_sample, freq = FALSE, breaks = 15, main = "Гамма-распределение", xlab = "Значения", ylab = "Плотность", col = "lightgreen")
curve(dgamma(x, shape = alpha_gamma, rate = beta_gamma), add = TRUE, col = "red", lwd = 2)
lines(density(gamma_sample), col = "blue", lwd = 2, lty = 2)

#3. Найти оценки числовых характеристик (выборочные среднее, дисперсию, СКО, моду, медиану, коэффициенты асимметрии и эксцесса.)
# Функция для расчета моды
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
gamma_mean <- mean(gamma_sample)
gamma_var <- var(gamma_sample)
gamma_sd <- sd(gamma_sample)
gamma_hist <- hist(gamma_sample, breaks = 15, plot = FALSE)
gamma_mode <- get_mode(gamma_sample)
gamma_median <- median(gamma_sample)
#gamma_skewness <- skewness(gamma_sample)
#gamma_kurtosis <- kurtosis(gamma_sample)

cat("\nОценки числовых характеристик:\n")
cat(paste("  Выборочное среднее:", round(gamma_mean, 4), "\n"))
cat(paste("  Выборочная дисперсия:", round(gamma_var, 4), "\n"))
cat(paste("  СКО:", round(gamma_sd, 4), "\n"))
cat(paste("  Мода (оценка):", round(gamma_mode, 4), "\n"))
cat(paste("  Медиана:", round(gamma_median, 4), "\n"))
#cat(paste("  Коэффициент асимметрии:", round(gamma_skewness, 4), "\n"))
#cat(paste("  Коэффициент эксцесса:", round(gamma_kurtosis, 4), "\n"))

exp_mean <- mean(exp_sample)
exp_var <- var(exp_sample)
exp_sd <- sd(exp_sample)
exp_mode <- get_mode(exp_sample)
exp_median <- median(exp_sample)
#exp_skewness <- moments::skewness(exp_sample)
#exp_kurtosis <- moments::kurtosis(exp_sample)

cat(paste("  Выборочное среднее:", round(exp_mean, 4), "\n"))
cat(paste("  Выборочная дисперсия:", round(exp_var, 4), "\n"))
cat(paste("  СКО:", round(exp_sd, 4), "\n"))
cat(paste("  Мода (оценка):", round(exp_mode, 4), "\n"))
cat(paste("  Медиана:", round(exp_median, 4), "\n"))
#cat(paste("  Коэффициент асимметрии:", round(exp_skewness, 4), "\n"))
#cat(paste("  Коэффициент эксцесса:", round(exp_kurtosis, 4), "\n"))

#4. Найти теоретические мат ожидание и дисперсию при заданных параметрах. Сравнить найденные точечные оценки с теоретическими характеристиками.
# Экспоненциальное распределение
theoretical_exp_mean <- 1 / lambda_exp
theoretical_exp_var <- 1 / lambda_exp^2
# Гамма-распределение
theoretical_gamma_mean <- alpha_gamma / beta_gamma
theoretical_gamma_var <- alpha_gamma / beta_gamma^2

cat("Экспоненциальное распределение:\n")
cat("Теоретическое среднее:", theoretical_exp_mean, "Выборочное среднее:", exp_mean, "\n")
cat("Теоретическая дисперсия:", theoretical_exp_var, "Выборочная дисперсия:", exp_var, "\n")

cat("Гамма-распределение:\n")
cat("Теоретическое среднее:", theoretical_gamma_mean, "Выборочное среднее:", gamma_mean, "\n")
cat("Теоретическая дисперсия:", theoretical_gamma_var, "Выборочная дисперсия:", gamma_var, "\n")
#5. Найти оценки параметров соответствующего распределения.
# Оценка параметра λ (метод моментов)
lambda_est <- 1 / exp_mean
cat(paste("Оценка параметра λ (метод моментов):", round(lambda_est, 4)), "\n")
cat(paste("Заданное значение λ:", lambda_exp))
#Оценка параметров гамма-распределения
beta_est <- gamma_mean / gamma_var
alpha_est <- gamma_mean * beta_est

cat(paste("\nОценка параметров (метод моментов):"))
cat(paste("\n  α:", round(alpha_est, 4)))
cat(paste("\n  β:", round(beta_est, 4)))
cat(paste("\nЗаданные значения: α =", alpha_gamma, ", β =", beta_gamma))
#6. Проверить гипотезу о виде распределения с помощью критерия хи-квадрат
# Проверка гипотезы о виде распределения с помощью критерия χ2
breaks_exp <- quantile(exp_sample, probs = seq(0, 1, length.out = 8))
breaks_exp[1] <- 0
breaks_exp[length(breaks_exp)] <- max(exp_sample) + 0.1

obs_exp <- hist(exp_sample, breaks = breaks_exp, plot = FALSE)$counts

# Теоретические вероятности для экспоненциального распределения
probs_exp <- pexp(breaks_exp[-1], rate = lambda_exp) - 
  pexp(breaks_exp[-length(breaks_exp)], rate = lambda_exp)
exp_freq_exp <- probs_exp * N

# Объединяем категории с ожидаемой частотой < 5
valid_exp <- exp_freq_exp >= 5
if (sum(valid_exp) < 2) {
  cat("\nСлишком мало категорий с ожидаемой частотой >= 5. Критерий χ2 не применяется.\n")
} else {
  obs_exp_final <- obs_exp[valid_exp]
  exp_exp_final <- exp_freq_exp[valid_exp]
  
  chi2_stat_exp <- sum((obs_exp_final - exp_exp_final)^2 / exp_exp_final)
  df_exp <- length(obs_exp_final) - 1 - 1  
  p_value_exp <- 1 - pchisq(chi2_stat_exp, df_exp)
  
  cat("\nПроверка гипотезы о виде распределения (критерий χ2):\n")
  cat(paste("  Статистика χ2:", round(chi2_stat_exp, 4), "\n"))
  cat(paste("  Число степеней свободы:", df_exp, "\n"))
  cat(paste("  p-значение:", round(p_value_exp, 4), "\n"))
  
  if (p_value_exp > 0.05) {
    cat("  Результат: Нет оснований отвергнуть гипотезу о экспоненциальном распределении (p > 0.05)\n")
  } else {
    cat("  Результат: Гипотеза о экспоненциальном распределении отвергается (p <= 0.05)\n")
  }
}
# Проверка гипотезы о виде распределения с помощью критерия χ2
breaks_gamma <- quantile(gamma_sample, probs = seq(0, 1, length.out = 8))
breaks_gamma[1] <- 0
breaks_gamma[length(breaks_gamma)] <- max(gamma_sample) + 0.1

obs_gamma <- hist(gamma_sample, breaks = breaks_gamma, plot = FALSE)$counts

# Теоретические вероятности для гамма-распределения
probs_gamma <- pgamma(breaks_gamma[-1], shape = alpha_gamma, rate = beta_gamma) - 
  pgamma(breaks_gamma[-length(breaks_gamma)], shape = alpha_gamma, rate = beta_gamma)
exp_freq_gamma <- probs_gamma * N

valid_gamma <- exp_freq_gamma >= 5
if (sum(valid_gamma) < 2) {
  cat("\nСлишком мало категорий с ожидаемой частотой >= 5. Критерий χ2 не применяется.\n")
} else {
  obs_gamma_final <- obs_gamma[valid_gamma]
  exp_gamma_final <- exp_freq_gamma[valid_gamma]
  
  chi2_stat_gamma <- sum((obs_gamma_final - exp_gamma_final)^2 / exp_gamma_final)
  df_gamma <- length(obs_gamma_final) - 1 - 2
  p_value_gamma <- 1 - pchisq(chi2_stat_gamma, df_gamma)
  
  cat("\nПроверка гипотезы о виде распределения (критерий χ2):\n")
  cat(paste("  Статистика χ2:", round(chi2_stat_gamma, 4), "\n"))
  cat(paste("  Число степеней свободы:", df_gamma, "\n"))
  cat(paste("  p-значение:", round(p_value_gamma, 4), "\n"))
  
  if (p_value_gamma > 0.05) {
    cat("  Результат: Нет оснований отвергнуть гипотезу о гамма-распределении (p > 0.05)\n")
  } else {
    cat("  Результат: Гипотеза о гамма-распределении отвергается (p <= 0.05)\n")
  }
}
