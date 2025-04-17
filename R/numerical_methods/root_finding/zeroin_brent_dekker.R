#' @title Метод Деккера-Брента для нахождения корней уравнения
#' @description Реализация метода нахождения корня нелинейного уравнения f(x) = 0 на отрезке [a, b].
#' Метод комбинирует подходы бисекции, секущих и касательных, обеспечивая быструю и надёжную сходимость.
#' @param f Функция одного аргумента
#' @param a Начало интервала
#' @param b Конец интервала
#' @param tol Точность (по умолчанию 1e-8)
#' @param max_iter Максимальное число итераций (по умолчанию 100)
#' @return Датафрейм с итерациями, приближениями корня и значениями функции
#' @examples
#' Классический кубический полином:
#' f1 <- function(x) x^3 - 2*x - 5
#' brent_dekker(f1, 2, 3)
#' 
#' Трансцендентное уравнение:  
#' f2 <- function(x) exp(x) - sin(x) - 3    
#' brent_dekker(f2, 0, 2)
#' 
#' Тригонометрическая функция:  
#' f3 <- function(x) cos(x) - x
#' brent_dekker(f3, 0, 1)

brent_dekker <- function(f, a, b, tol = 1e-8, max_iter = 100) {
  if (f(a) * f(b) >= 0) stop("f(a) и f(b) должны иметь разные знаки")
  
  fa <- f(a)
  fb <- f(b)
  c <- a
  fc <- fa
  d <- e <- b - a
  iter_data <- data.frame(iter = 0, x = a, fx = fa)
  
  for (iter in 1:max_iter) {
    if (fb * fc > 0) {
      c <- a; fc <- fa; d <- e <- b - a
    }
    if (abs(fc) < abs(fb)) {
      a <- b; b <- c; c <- a
      fa <- fb; fb <- fc; fc <- fa
    }
    
    tol1 <- 2 * .Machine$double.eps * abs(b) + 0.5 * tol
    m <- 0.5 * (c - b)
    
    if (abs(m) <= tol1 || fb == 0) {
      message(sprintf("Успешно завершено на итерации %d. Корень: %.10f", iter, b))
      break
    }
    
    if (abs(e) >= tol1 && abs(fa) > abs(fb)) {
      s <- fb / fa
      if (a == c) {
        p <- 2 * m * s; q <- 1 - s
      } else {
        q <- fa / fc; r <- fb / fc
        p <- s * (2 * m * q * (q - r) - (b - a) * (r - 1))
        q <- (q - 1) * (r - 1) * (s - 1)
      }
      if (p > 0) q <- -q
      p <- abs(p)
      if (2 * p < min(3 * m * q - abs(tol1 * q), abs(e * q))) {
        e <- d; d <- p / q
      } else {
        d <- m; e <- m
      }
    } else {
      d <- m; e <- m
    }
    
    a <- b; fa <- fb
    b <- if (abs(d) > tol1) b + d else b + sign(m) * tol1
    fb <- f(b)
    
    iter_data <- rbind(iter_data, data.frame(iter = iter, x = b, fx = fb))
    message(sprintf("Итерация %d: x = %.10f, f(x) = %.10f", iter, b, fb))
  }
  
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    
    # 1. График сходимости
    plot_iter <- ggplot(iter_data, aes(x = iter, y = fx)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(title = "Сходимость метода Брента", x = "Итерация", y = "f(x)") +
      theme_minimal()
    print(plot_iter)
    
    # 2. График функции с найденным корнем
    x_vals <- seq(a - 1, b + 1, length.out = 500)
    y_vals <- sapply(x_vals, f)
    df <- data.frame(x = x_vals, y = y_vals)
    y_range <- range(y_vals)
    
    plot_func <- ggplot(df, aes(x, y)) +
      geom_line(color = "blue", linewidth = 1.1) +
      geom_point(aes(x = b, y = f(b)), color = "red", size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(title = "Функция и найденный корень", x = "x", y = "f(x)") +
      theme_minimal() +
      coord_cartesian(ylim = c(y_range[1] - 0.1 * diff(y_range), y_range[2] + 0.1 * diff(y_range)))
    print(plot_func)
  }
  
  return(b)
}