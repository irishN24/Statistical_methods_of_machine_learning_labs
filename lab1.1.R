#Сгенерировать выборки объема N (параметр N задать произвольно в диапазоне от 100 до 200) следующих распределений:
#      Биномиальное,
#      Геометрическое.
# Для каждого распределения параметры задать самостоятельно.
rm(list = ls());
N <- sample(100:200, 1); # объём выборки
binom_sample <- rbinom(N, 20, 0.5); # биномиальное распределение с числом испытаний 20 и вероятностью успеха 0.5
geom_sample <- rgeom(N, 0.3); # геометрическое распределение с вероятностью успеха 0.3
# Построить полигон частот и эмпирическую функцию распределения.
# Биномиальное распределение
binom_freq <- table(factor(binom_sample, levels = 0:20)); # Частоты значений
binom_mids <- as.numeric(names(binom_freq)); # Середины интервалов
binom_probs <- as.numeric(binom_freq) / N; # Относительные частоты
plot(binom_mids, binom_probs, type = "b", col = "blue", lwd = 2,
     main = "Полигон частот (биномиальное)", xlab = "Значения", ylab = "Частота");

plot(ecdf(binom_sample), main = "Эмпирическая функция распределения (Биномиальное)",
     xlab = "x", ylab = "F(x)", col = "green", verticals = TRUE, do.points = FALSE);
# Геометрическое распределение
geom_freq <- table(factor(geom_sample, levels = 0:max(geom_sample)))  # Частоты значений
geom_mids <- as.numeric(names(geom_freq))  # Середины интервалов
geom_probs <- as.numeric(geom_freq) / N  # Относительные частоты
plot(geom_mids, geom_probs, type = "b", col = "red", lwd = 2,
     main = "Полигон частот (геометрическое)", xlab = "Значения", ylab = "Частота")

plot(ecdf(geom_sample), main = "Эмпирическая функция распределения (Геометрическое)",
     xlab = "x", ylab = "F(x)", col = "pink", verticals = TRUE, do.points = FALSE)

#Найти оценки числовых характеристик (выборочные среднее, дисперсию, СКО, моду, медиану, коэффициенты асимметрии и эксцесса).
# Для биномиального распределения
mean_binom <- mean(binom_sample);  # Выборочное среднее
var_binom <- var(binom_sample)  # Дисперсия
sd_binom <- sd(binom_sample)  # Выборочное СКО
binom_mode <- as.numeric(names(sort(table(binom_sample), decreasing = TRUE)[1])) # мода
binom_median <- median(binom_sample) #медиана
# Коэффициенты асимметрии и эксцесса (используем моменты 3-го и 4-го порядка)
#library(moments)
#binom_skewness <- skewness(binom_sample) # Коэффициент асимметрии
#binom_kurtosis <- kurtosis(binom_sample)  # Коэффициент эксцесса
cat("\nОценки числовых характеристик биномиального распределения:\n")
cat(paste("  Выборочное среднее:", round(mean_binom, 4), "\n"))
cat(paste("  Выборочная дисперсия:", round(var_binom, 4), "\n"))
cat(paste("  СКО:", round(sd_binom, 4), "\n"))
cat(paste("  Мода:", binom_mode, "\n"))
cat(paste("  Медиана:", binom_median, "\n"))
#cat(paste("  Коэффициент асимметрии:", round(binom_skewness, 4), "\n"))
#cat(paste("  Коэффициент эксцесса:", round(binom_kurtosis, 4), "\n"))

# Для геометрического распределения
mean_geom <- mean(geom_sample)  # Выборочное среднее
var_geom <- var(geom_sample)  # Выборочная дисперсия
sd_geom <- sd(geom_sample)  # Выборочное СКО
mode_geom <- as.numeric(names(sort(table(geom_sample), decreasing = TRUE)[1]))  # Мода
median_geom <- median(geom_sample)  # Медиана
#skewness_geom <- moments::skewness(geom_sample)  # Коэффициент асимметрии
#kurtosis_geom <- moments::kurtosis(geom_sample)  # Коэффициент эксцесса

cat("\nОценки числовых характеристик геометрического распределения:\n")
cat(paste("  Выборочное среднее:", round(mean_geom, 4), "\n"))
cat(paste("  Выборочная дисперсия:", round(var_geom, 4), "\n"))
cat(paste("  СКО:", round(sd_geom, 4), "\n"))
cat(paste("  Мода:", mode_geom, "\n"))
cat(paste("  Медиана:", median_geom, "\n"))
#cat(paste("  Коэффициент асимметрии:", round(skewness_geom, 4), "\n"))
#cat(paste("  Коэффициент эксцесса:", round(kurtosis, 4), "\n"))

# Теоретические характеристики
theoretical_mean_binom <- 20 * 0.5
theoretical_var_binom <- 20 * 0.5 * (1 - 0.5)

cat("\nТеоретические характеристики для биномиального распределения:\n")
cat(paste("  Теоретическое мат. ожидание:", theoretical_mean_binom, "\n"))
cat(paste("  Теоретическая дисперсия:", round(theoretical_var_binom, 4), "\n"))

theoretical_mean_geom <- (1 - 0.3) / 0.3
theoretical_var_geom <- (1 - 0.3) / 0.3^2

cat("\nТеоретические характеристики для геометрического распределения:\n")
cat(paste("  Теоретическое мат. ожидание:", round(theoretical_mean_geom, 4), "\n"))
cat(paste("  Теоретическая дисперсия:", round(theoretical_var_geom, 4), "\n"))

cat("Биномиальное распределение:\n")
cat("Теоретическое среднее:", theoretical_mean_binom, "Выборочное среднее:", mean_binom, "\n")
cat("Теоретическая дисперсия:", theoretical_var_binom, "Выборочная дисперсия:", var_binom, "\n")
cat("Геометрическое распределение:\n")
cat("Теоретическое среднее:", theoretical_mean_geom, "Выборочное среднее:", mean_geom, "\n")
cat("Теоретическая дисперсия:", theoretical_var_geom, "Выборочная дисперсия:", var_geom, "\n")

# Оценки параметров распределения методлом моментов
estimated_p_binom <- mean_binom / 20  # Оценка p для биномиального распределения
estimated_p_geom <- 1 / (mean_geom + 1)  # Оценка p для геометрического распределения
cat("Биномиальное распределение:\n")
cat("Теоретическая p:", 0.5, "Оценка p:", estimated_p_binom, "\n")
cat("Геометрическое распределение:\n")
cat("Теоретическая p:", 0.3, "Оценка p:", estimated_p_geom, "\n")

# Проверить гипотезу о виде распределения с помощью критерия хи-квадрат
# Для биномиального распределения
observed_binom <- table(factor(binom_sample, levels = 0:20))
expected_binom <- dbinom(0:20, 20, 0.5) * N
chi_sq_test_binom <- chisq.test(observed_binom, p = expected_binom / N)
cat("Хи-квадрат тест для биномиального распределения:\n")
print(chi_sq_test_binom)

# Для геометрического распределения
observed_geom <- table(factor(geom_sample, levels = 0:max(geom_sample)))
expected_geom <- dgeom(0:max(geom_sample), 0.3) * N
expected_probs <- expected_geom / sum(expected_geom)
chi_sq_test_geom <- chisq.test(observed_geom, p = expected_probs)
cat("Хи-квадрат тест для геометрического распределения:\n")
print(chi_sq_test_geom)

