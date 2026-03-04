# Задание:
library(ggplot2)
library(gridExtra)
rm(list = ls())
#   1. Сгенерировать две выборки Х1 и Х2 объема N = 100 из нормального
#   распределения с параметрами, удовлетворяющими следующим ситуациям:
N <- 100
#     a. Математические ожидания и дисперсии равны;
#     b. Математические ожидания существенно различны, дисперсии равны;
#     c. Математические ожидания равны, дисперсии существенно различны;
#     d. Математические ожидания и дисперсии существенно различны.
params <- list(
  a = list(m1 = 5, m2 = 5, sd1 = 2, sd2 = 2),
  b = list(m1 = 5, m2 = 10, sd1 = 2, sd2 = 2),
  c = list(m1 = 5, m2 = 5, sd1 = 2, sd2 = 10),
  d = list(m1 = 10, m2 = 5, sd1 = 2, sd2 = 10)
)
#   2. Для каждой ситуации:
analyze_samples <- function(x1, x2, situation) {
#     a. Проверить гипотезы о нормальности с помощью критерия Шапиро-Уилка, гипотезы о равенстве средних и дисперсий;
  shapiro1 <- shapiro.test(x1)
  shapiro2 <- shapiro.test(x2)
  
  #t_test <- t.test(x1, x2, var.equal = TRUE)
  f_test <- var.test(x1, x2)

  cat("Ситуация", situation, ":\n")
  cat("  Тест Шапиро-Уилка для X1: p-value =", shapiro1$p.value, "\n")
  cat("  Тест Шапиро-Уилка для X2: p-value =", shapiro2$p.value, "\n")
  if (shapiro1$p.value > 0.05) {
    cat(" Распределение X1 не отличается от нормального\n")
  } else {
    cat(" Распределение X1 отличается от нормального\n")
  }
  if (shapiro2$p.value > 0.05) {
    cat(" Распределение X2 не отличается от нормального\n")
  } else {
    cat(" Распределение X2 отличается от нормального\n")
  }
  # Проверка равенства средних (критерий Стьюдента)
  # В зависимости от равенства дисперсий выбираем соответствующий тест
  if (f_test$p.value > 0.05) {
    t_test <- t.test(x1, x2, var.equal = TRUE)
    t_test_type <- "с предположением о равенстве дисперсий"
  } else {
    #тест Уэлча
    t_test <- t.test(x1, x2, var.equal = FALSE)
    t_test_type <- "без предположения о равенстве дисперсий (тест Уэлча)"
  }
  cat("\nКритерий Стьюдента (равенство средних):\n")
  cat(paste("  Используется", t_test_type, "\n"))
  cat(paste("  t =", round(t_test$statistic, 4), 
            ", df =", round(t_test$parameter, 4),
            ", p-value =", round(t_test$p.value, 4)))
  if (t_test$p.value > 0.05) {
    cat(" → средние равны (на уровне значимости 0.05)\n")
  } else {
    cat(" → средние различны (на уровне значимости 0.05)\n")
  }
  cat("  F-тест для равенства дисперсий: p-value =", f_test$p.value, " => ")
  if (f_test$p.value > 0.05) {
    cat(" дисперсии равны (на уровне значимости 0.05)\n")
  } else {
    cat(" дисперсии различны (на уровне значимости 0.05)\n")
  }
  # Выборочные характеристики
  mean_X1 <- mean(x1)
  mean_X2 <- mean(x2)
  var_X1 <- var(x1)
  var_X2 <- var(x2)
  sd_X1 <- sd(x1)
  sd_X2 <- sd(x2)
  
  cat("\nВыборочные характеристики:\n")
  cat(paste("  X1: среднее =", round(mean_X1, 4), 
            ", дисперсия =", round(var_X1, 4), 
            ", СКО =", round(sd_X1, 4), "\n"))
  cat(paste("  X2: среднее =", round(mean_X2, 4), 
            ", дисперсия =", round(var_X2, 4), 
            ", СКО =", round(sd_X2, 4), "\n"))
  
#     b. На одном графике построить гистограммы Х1 и Х2, оценки плотностей для обеих выборок, вертикальными линиями отметить средние значения;
  df <- data.frame(
    value = c(x1, x2),
    group = rep(c("X1", "X2"), each = N)
  )
  p1 <- ggplot(df, aes(x = value, fill = group)) + geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity", bins = 30) +
    geom_density(alpha = 0.3) + geom_vline(data = data.frame(group = c("X1", "X2"), mean = c(mean(x1), mean(x2))), aes(xintercept = mean, color = group), linetype = "dashed", size = 1) + labs(title = paste("Ситуация", situation, ": Гистограммы и плотности"), x = "Значение", y = "Плотность") + theme_minimal()
#     c. На другом графике построить коробки с усами для Х1 и Х2.
  p2 <- ggplot(df, aes(x = group, y = value, fill = group)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Ситуация", situation, ": Boxplot"),
         x = "Группа", y = "Значение") +
    theme_minimal()
  grid.arrange(p1, p2, nrow = 2)
}
for (s in names(params)) {
  p <- params[[s]]
  x1 <- rnorm(N, mean = p$m1, sd = p$sd1)
  x2 <- rnorm(N, mean = p$m2, sd = p$sd2)
  analyze_samples(x1, x2, s)
}
