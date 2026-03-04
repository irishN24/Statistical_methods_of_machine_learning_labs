rm(list = ls())
# 1. Сгенерировать выборку X1 из нормального распределения. Параметры a S, задать произвольно. Объем выборки N = 200.
N <- 200
a1 <- 8
S1 <- 6
X1 <- rnorm(N, mean = a1, sd = S1)

a2 <- 8
S2 <- 3
# 2. Задать значения двух коэффициентов корреляции, таких чтобы 1r [0.1;0.2], 2r [0.6;0.9].
r1 <- 0.13   # слабая корреляция
r2 <- 0.85   # сильная корреляция

# 3. Построить еще две выборки X2 и X3 так, чтобы одна из них имела
# коэффициент корреляции с первой выборкой r1, а вторая соответственно r2
generate_correlated_sample <- function(X, r, a, S, N) {
  r * X + sqrt(1 - r^2) * rnorm(N, mean = a, sd = S)
}
X2 <- generate_correlated_sample(X1, r1, a2, S2, N)
X3 <- generate_correlated_sample(X1, r2, a2, S2, N)

# 4. Построить диаграммы рассеяния для X1 и X2, X1 и X3.
par(mfrow = c(1, 2)) 
plot(X1, X2, main = paste("X1 и X2 (ожидаемый r =", r1, ")"),
     xlab = "X1", ylab = "X2", col = "blue", pch = 19)
abline(lm(X2 ~ X1), col = "red")
plot(X1, X3, main = paste("X1 и X3 (ожидаемый r =", r2, ")"),
     xlab = "X1", ylab = "X3", col = "green", pch = 19)
abline(lm(X3 ~ X1), col = "red")
par(mfrow = c(1, 1)) 
# 5. Построить выборочные парные коэффициенты корреляции для X1 и X2, X1 и X3.
r_hat1 <- cor(X1, X2)
r_hat2 <- cor(X1, X3)
cat("Выборочный коэффициент корреляции между X1 и X2:", round(r_hat1, 4), "\n")
cat("Выборочный коэффициент корреляции между X1 и X3:", round(r_hat2, 4), "\n")
# 6. Рассчитать статистики Стьюдента по выборочным значениям и проверить гипотезу о наличии корреляционной связи.

# Статистика Стьюдента: t = r * sqrt((n-2)/(1-r^2))
# Нулевая гипотеза H0: r = 0 (корреляции нет)
# Альтернативная гипотеза H1: r ≠ 0 (корреляция есть)
test_correlation <- function(x, y) {
  r <- cor(x, y)
  n <- length(x)
  t_stat <- r * sqrt((n - 2) / (1 - r^2))
  p_value <- 2 * pt(-abs(t_stat), df = n - 2)
  significant <- p_value < 0.05  # уровень значимости α = 0.05
  list(t_statistic = t_stat, p_value = p_value, significant = significant, r = r)
}
test1 <- test_correlation(X1, X2)
test2 <- test_correlation(X1, X3)
cat("\nТест для X1 и X2:\n")
cat("t-статистика:", round(test1$t_statistic, 4), "\n")
cat("p-значение:", format.pval(test1$p_value, digits = 4), "\n")
if (test1$significant) {
  cat("  Результат: H0 отвергается. Корреляционная связь статистически значима.\n")
} else {
  cat("  Результат: H0 не отвергается. Корреляционная связь не обнаружена.\n")
}
cat("\nТест для X1 и X3:\n")
cat("t-статистика:", round(test2$t_statistic, 4), "\n")
cat("p-значение:", format.pval(test2$p_value, digits = 4), "\n")
if (test2$significant) {
  cat("  Результат: H0 отвергается. Корреляционная связь статистически значима.\n\n")
} else {
  cat("  Результат: H0 не отвергается. Корреляционная связь не обнаружена.\n\n")
}
# 7. Построить доверительный интервал для корреляционного коэффициента с помощью Z-преобразования Фишера.
fisher_ci <- function(r, n, conf.level = 0.95) {
  # Z-преобразование Фишера
  z <- 0.5 * log((1 + r) / (1 - r))
  z_se <- 1 / sqrt(n - 3)
  # Критическое значение для нормального распределения
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha/2)
  # Доверительный интервал для z
  z_lower <- z - z_crit * z_se
  z_upper <- z + z_crit * z_se
  
  r_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
  r_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
  return(list(lower = r_lower, upper = r_upper, z_lower = z_lower, z_upper = z_upper, z = z, z_se = z_se))
}
ci1 <- fisher_ci(r_hat1, N)
ci2 <- fisher_ci(r_hat2, N)
cat("\nДоверительный интервал для корреляции между X1 и X2:\n")
cat("  Нижняя граница:", round(ci1$lower, 4), "\n")
cat("  Верхняя граница:", round(ci1$upper, 4), "\n")
cat("\nДоверительный интервал для корреляции между X1 и X3:\n")
cat("  Нижняя граница:", round(ci2$lower, 4), "\n")
cat("  Верхняя граница:", round(ci2$upper, 4), "\n")
if (ci1$lower <= r1 && r1 <= ci1$upper) {
  cat(paste("  Теоретическое значение r1 =", r1, "попадает в доверительный интервал.\n\n"))
} else {
  cat(paste("  Теоретическое значение r1 =", r1, "НЕ попадает в доверительный интервал.\n\n"))
}
if (ci2$lower <= r2 && r2 <= ci2$upper) {
  cat(paste("  Теоретическое значение r2 =", r2, "попадает в доверительный интервал.\n"))
} else {
  cat(paste("  Теоретическое значение r2 =", r2, "НЕ попадает в доверительный интервал.\n"))
}