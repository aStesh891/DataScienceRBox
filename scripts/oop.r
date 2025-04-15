
### Задача 1: Реализация класса S3

# Создание объекта класса S3
create_car <- function(brand, model, year_of_manufacture) {
  obj <- list(brand = brand, model = model, year_of_manufacture = year_of_manufacture)
  class(obj) <- "Car"
  return(obj)
}

# Определение метода для класса S3
print.car <- function(obj) {
  cat("Марка:", obj$brand, "\nМодель:", obj$model, "\nГод выпуска:", obj$year_of_manufacture, "\n")  
  }

# Использование класса S3
car1 <- create_car("Opel", "Opel Astra", 2016)
print.car(car1)

### Задача 2: Реализация класса S4

# Определение класса S4
setClass(
  "Book",
  slots = list(title = "character", author = "character", year_of_publication = "numeric")
)

# Создание объекта класса S4
book <- new("Book", title = "If Cats Disappeared from the World", author = "Genki Kawamura", year_of_publication = 2018)

# Определение метода для класса S4
setMethod(
  "show",
  "Book",
  function(object) {
    cat("Название:", object@title, "\nАвтор:", object@author, "\nГод издания:", object@year_of_publication, "\n")
  }
)

# Использование класса S4
show(book)





