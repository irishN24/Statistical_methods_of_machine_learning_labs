#1. Сгенерировать выборки объема N (N = 100, 500, 1000) из нормального распределения с одинаковыми параметрами. Параметры задать самостоятельно
rm(list = ls())
N1 <- 100;
N2 <- 500;
N3 <- 1000;
a <- 5  # Математическое ожидание
sigma <- 2  # Стандартное отклонение
sample_1 <- rnorm(N1, mean = a, sd = sigma)
sample_2 <- rnorm(N2, mean = a, sd = sigma)
sample_3 <- rnorm(N3, mean = a, sd = sigma)
#2. Построить квантильные графики (qqplot) и гистограммы для построенных выборок.
qqnorm(sample_1, main = paste("QQ-plot, N =", N1))
qqline(sample_1, col = "red")
hist(sample_1, breaks = 30, main = paste("Гистограмма 1, N =", N1), xlab = "Value", col = "lightblue")

qqnorm(sample_2, main = paste("QQ-plot, N =", N2))
qqline(sample_2, col = "red")
hist(sample_2, breaks = 30, main = paste("Гистограмма 2, N =", N2), xlab = "Value", col = "lightblue")

qqnorm(sample_3, main = paste("QQ-plot, N =", N3))
qqline(sample_3, col = "red")
hist(sample_3, breaks = 30, main = paste("Гистограмма 3, N =", N3), xlab = "Value", col = "lightblue")

#3. Найти оценки числовых характеристик (выборочные среднее, дисперсию, СКО).
mean_est_1 <- mean(sample_1)
var_est_1 <- var(sample_1)
sd_est_1 <- sd(sample_1)
cat("N =", N1, "\n")
cat("Выборочное среднее:", mean_est_1, "\n")
cat("Выборочная дисперсия:", var_est_1, "\n")
cat("Выборочное СКО:", sd_est_1, "\n\n")

mean_est_2 <- mean(sample_2)
var_est_2 <- var(sample_2)
sd_est_2 <- sd(sample_2)
cat("N =", N2, "\n")
cat("Выборочное среднее:", mean_est_2, "\n")
cat("Выборочная дисперсия:", var_est_2, "\n")
cat("Выборочное СКО:", sd_est_2, "\n\n")

mean_est_3 <- mean(sample_3)
var_est_3 <- var(sample_3)
sd_est_3 <- sd(sample_3)
cat("N =", N3, "\n")
cat("Выборочное среднее:", mean_est_3, "\n")
cat("Выборочная дисперсия:", var_est_3, "\n")
cat("Выборочное СКО:", sd_est_3, "\n\n")

#4. Построить доверительные интервалы для математического ожидания при известном значении дисперсии на уровне значимости α = 0,05.
alpha <- 0.05
z_critical <- qnorm(1 - alpha/2)
#N = 100
sample_1_mean <- mean(sample_1)

se_known <- sigma / sqrt(N1)
# Границы доверительного интервала
lower_1 <- sample_1_mean - z_critical * se_known
upper_1 <- sample_1_mean + z_critical * se_known
  
ci_known1_lower <- lower_1
ci_known1_upper <- upper_1
  
cat(paste("Выборка объема N =", N1, ":\n"))
cat(paste("  Точечная оценка (выборочное среднее):", round(sample_1_mean, 4), "\n"))
cat(paste("  Доверительный интервал: [", round(lower_1, 4), ";", round(upper_1, 4), "]\n"))
  
if (lower_1 <= a && a <= upper_1) {
  cat("  Результат: Истинное значение a =", a, "попадает в интервал\n\n")
}else {
  cat("  Результат: Истинное значение a =", a, "НЕ попадает в интервал\n\n")}
# N = 500
sample_2_mean <- mean(sample_2)

se_known <- sigma / sqrt(N2)
# Границы доверительного интервала
lower_2 <- sample_2_mean - z_critical * se_known
upper_2 <- sample_2_mean + z_critical * se_known

ci_known2_lower <- lower_2
ci_known2_upper <- upper_2

cat(paste("Выборка объема N =", N2, ":\n"))
cat(paste("  Точечная оценка (выборочное среднее):", round(sample_2_mean, 4), "\n"))
cat(paste("  Доверительный интервал: [", round(lower_2, 4), ";", round(upper_2, 4), "]\n"))

if (lower_2 <= a && a <= upper_2) {
  cat("  Результат: Истинное значение a =", a, "попадает в интервал\n\n")
}else {
  cat("  Результат: Истинное значение a =", a, "НЕ попадает в интервал\n\n")}
# N = 1000
sample_3_mean <- mean(sample_3)

se_known <- sigma / sqrt(N3)
# Границы доверительного интервала
lower_3 <- sample_3_mean - z_critical * se_known
upper_3 <- sample_3_mean + z_critical * se_known

cat(paste("Выборка объема N =", N3, ":\n"))
cat(paste("  Точечная оценка (выборочное среднее):", round(sample_3_mean, 4), "\n"))
cat(paste("  Доверительный интервал: [", round(lower_3, 4), ";", round(upper_3, 4), "]\n"))

if (lower_3 <= a && a <= upper_3) {
  cat("  Результат: Истинное значение a =", a, "попадает в интервал\n\n")
}else {
  cat("  Результат: Истинное значение a =", a, "НЕ попадает в интервал\n\n")}

#Построить доверительные интервалы для математического ожидания при неизвестном значении дисперсии (оцененном по выборке) на уровне значимости α = 0,05.
#N = 100
# Квантиль распределения Стьюдента с (N-1) степенями свободы
t_crit <- qt(1 - alpha/2, df = N1 - 1)

se_unknown_1 <- sd_est_1 / sqrt(N1)

unknown_lower_1 <- mean_est_1 - t_crit * se_unknown_1
unknown_upper_1 <- mean_est_1 + t_crit * se_unknown_1

cat(paste("  Квантиль Стьюдента t_{", N1-1, ", 1-α/2} =", round(t_crit, 4), "\n"))
cat(paste("  Доверительный интервал: [", round(unknown_lower_1, 4), ";", round(unknown_upper_1, 4), "]\n"))

if (unknown_lower_1 <= a && a <= unknown_upper_1) {
  cat("  Результат: Истинное значение a =", a, "попадает в интервал\n\n")
} else {
  cat("  Результат: Истинное значение a =", a, "НЕ попадает в интервал\n\n")
}

#N = 500
# Квантиль распределения Стьюдента с (N-1) степенями свободы
t_crit <- qt(1 - alpha/2, df = N2 - 1)

se_unknown_2 <- sd_est_2 / sqrt(N2)

unknown_lower_2 <- mean_est_2 - t_crit * se_unknown_2
unknown_upper_2 <- mean_est_2 + t_crit * se_unknown_2

cat(paste("  Квантиль Стьюдента t_{", N2-1, ", 1-α/2} =", round(t_crit, 4), "\n"))
cat(paste("  Доверительный интервал: [", round(unknown_lower_2, 4), ";", round(unknown_upper_2, 4), "]\n"))

if (unknown_lower_2 <= a && a <= unknown_upper_2) {
  cat("  Результат: Истинное значение a =", a, "попадает в интервал\n\n")
} else {
  cat("  Результат: Истинное значение a =", a, "НЕ попадает в интервал\n\n")
}

#N = 1000
# Квантиль распределения Стьюдента с (N-1) степенями свободы
t_crit <- qt(1 - alpha/2, df = N3 - 1)

se_unknown_3 <- sd_est_3 / sqrt(N3)

unknown_lower_3 <- mean_est_3 - t_crit * se_unknown_1
unknown_upper_3 <- mean_est_3 + t_crit * se_unknown_3

cat(paste("  Квантиль Стьюдента t_{", N3-1, ", 1-α/2} =", round(t_crit, 4), "\n"))
cat(paste("  Доверительный интервал: [", round(unknown_lower_3, 4), ";", round(unknown_upper_3, 4), "]\n"))

if (unknown_lower_3 <= a && a <= unknown_upper_3) {
  cat("  Результат: Истинное значение a =", a, "попадает в интервал\n\n")
} else {
  cat("  Результат: Истинное значение a =", a, "НЕ попадает в интервал\n\n")
}
#Построить график зависимости точечных оценок математического ожидания, а также левых и правых границ доверительных интервалов от объема выборки (на одном рисунке).
N <- c(N1, N2, N3)
mean_vector <- c(mean_est_1, mean_est_2, mean_est_3)

known_lower <- c(lower_1, lower_2, lower_3)
known_upper <- c(upper_1, upper_2, upper_3)

unknown_lower <- c(unknown_lower_1, unknown_lower_2, unknown_lower_3)
unknown_upper <- c(unknown_upper_1, unknown_upper_2, unknown_upper_3)

y_min <- min(known_lower, unknown_lower, mean_vector, a - 0.5)
y_max <- max(known_upper, unknown_upper, mean_vector, a + 0.5)
plot(N, mean_vector, 
     type = "b",          
     pch = 19,             
     col = "black", 
     lwd = 2,
     xlab = "Объем выборки (N)", 
     ylab = "Значение",
     main = "Зависимость оценок и доверительных интервалов от объема выборки",
     ylim = c(y_min, y_max),
     xlim = c(0, max(N) + 50),
     xaxt = "n",
     cex.main = 1.2,
     cex.lab = 1.1,
     cex.axis = 1)
axis(1, at = N, labels = N)

# Добавляем горизонтальную линию для истинного значения
abline(h = a, col = "red", lwd = 2, lty = 2)

# Добавляем доверительные интервалы для известной дисперсии
arrows(x0 = N, y0 = known_lower, 
       x1 = N, y1 = known_upper,
       angle = 90, code = 3, length = 0.1, col = "blue", lwd = 2)
arrows(x0 = N + 20, y0 = unknown_lower, 
       x1 = N + 20, y1 = unknown_upper,
       angle = 90, code = 3, length = 0.1, col = "darkgreen", lwd = 2)
points(N, mean_vector, pch = 19, col = "black", cex = 1.5)
grid()

windows()

width_known <- known_upper - known_lower
width_unknown <- unknown_upper - unknown_lower

#график ширины интервалов
plot(N, width_known, 
     type = "b", 
     pch = 19, 
     col = "blue", 
     lwd = 2,
     xlab = "Объем выборки (N)", 
     ylab = "Ширина доверительного интервала",
     main = "Зависимость ширины доверительных интервалов от объема выборки",
     ylim = c(0, max(width_known, width_unknown) * 1.1),
     xaxt = "n",
     cex.main = 1.2,
     cex.lab = 1.1)
axis(1, at = N, labels = N)

lines(N, width_unknown, type = "b", pch = 19, col = "darkgreen", lwd = 2)
