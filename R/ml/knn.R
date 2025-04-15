library(class)

# Создаем список для хранения функций
classification_alg <- list()

# Function to perform kNN classification
# 
#' @param train_data Training data
#' @param test_data Test data
#' @param dept_variable Name of the dependent variable (target)
#' @param indep_variables Vector of names of independent variables
#' @param start_k Value for start k. If value set 0 - k=sqrt from count of data
#' @param n_step Value of step when calculate vector for k values
#
#' @return List with kNN results including best k, predictions, and metrics

classification_alg$knn_ <- function(
    train_data, 
    test_data,
    dep_variable, 
    indep_variables,
    start_k = 2,
    n_step = 3
    ) 
  {
  #Check require function params
  stopifnot(!missing(train_data), 
            !missing(test_data),
            !missing(dep_variable),
            !missing(indep_variables)
            )
  
  if(start_k == 0){
    start_k = round(sqrt(nrow(train_data)),0)
  }
  max_k = start_k * 2
  k_values = seq(start_k, max_k, by = n_step)
  best_k = classification_alg$knn_find_best_k(
    train_data = train_data, 
    test_data = test_data, 
    k_values = k_values,
    dep_variable = dep_variable, 
    indep_variables = indep_variables
  )
  
  predictions = knn(train = train_data[, c(indep_variables)],
                     test = test_data[, c(indep_variables)],
                     cl = train_data[[dep_variable]],
                     k = best_k)
  
  model_perfomance = helpers$calculate_model_performance_func(
    actual = test_data[[dep_variable]],
    predictions = predictions,
    FALSE
    )
  
  return(list(
    start_k = start_k,
    max_k = max_k,
    best_k = best_k,
    predictions = predictions,
    model_perfomance = model_perfomance
  )
  )
}

# Function to find the best k for kNN
# 
#' @param train_data Training data
#' @param test_data Test data
#' @param k_values Vector of k values to search
#' @param dep_variable Name of the dependent variable (target)
#' @param indep_variables Vector of names of independent variables
#
#' @return Best k value

classification_alg$knn_find_best_k <- function(
    train_data, 
    test_data, 
    k_values,
    dep_variable, 
    indep_variables
    ) 
  {
  #Check require function params
  stopifnot(!missing(train_data), 
            !missing(test_data),
            !missing(k_values),
            !missing(dep_variable),
            !missing(indep_variables)
  )
  
  best_k = NULL
  best_accuracy = 0
  
  for (k in k_values) {
    predictions = knn(train = train_data[, c(indep_variables)],
                       test = test_data[, c(indep_variables)],
                       cl = train_data[[dep_variable]],
                       k = k,
                      prob = TRUE)
    
    model_perfomance = helpers$calculate_model_performance_func(
      actual = test_data[[dep_variable]],
      predictions = predictions,
      FALSE
    )
    
    if (model_perfomance$accuracy > best_accuracy) {
      best_accuracy = model_perfomance$accuracy
      best_k <- k
    }
  }
  
  return(best_k)
}








###############


library(class)

# Создаем список для хранения функций
classification_alg <- list()

# Функция для KNN с выбором наилучшего k
classification_alg$knn_ <- function(
    train_data, 
    test_data,
    dep_variable, 
    indep_variables,
    start_k = 2,
    n_step = 3
) {
  # Проверяем входные параметры
  stopifnot(!missing(train_data), 
            !missing(test_data),
            !missing(dep_variable),
            !missing(indep_variables))
  
  # Если start_k не задан, вычисляем как sqrt(N)
  if(start_k == 0){
    start_k <- round(sqrt(nrow(train_data)), 0)
  }
  
  max_k <- start_k * 2
  k_values <- seq(start_k, max_k, by = n_step)
  
  # Определяем лучшее k
  best_k <- classification_alg$knn_find_best_k(
    train_data = train_data, 
    test_data = test_data, 
    k_values = k_values,
    dep_variable = dep_variable, 
    indep_variables = indep_variables
  )
  
  # Выполняем kNN с лучшим k
  predictions <- knn(train = train_data[, c(indep_variables)],
                     test = test_data[, c(indep_variables)],
                     cl = train_data[[dep_variable]],
                     k = best_k)
  
  # Оцениваем точность (accuracy)
  accuracy <- sum(predictions == test_data[[dep_variable]]) / length(predictions)
  
  return(list(
    start_k = start_k,
    max_k = max_k,
    best_k = best_k,
    predictions = predictions,
    accuracy = accuracy  # Вместо model_performance
  ))
}

# Функция для поиска наилучшего k
classification_alg$knn_find_best_k <- function(
    train_data, 
    test_data, 
    k_values,
    dep_variable, 
    indep_variables
) {
  # Проверяем входные параметры
  stopifnot(!missing(train_data), 
            !missing(test_data),
            !missing(k_values),
            !missing(dep_variable),
            !missing(indep_variables))
  
  best_k <- NULL
  best_accuracy <- 0
  
  for (k in k_values) {
    predictions <- knn(train = train_data[, c(indep_variables)],
                       test = test_data[, c(indep_variables)],
                       cl = train_data[[dep_variable]],
                       k = k,
                       prob = TRUE)
    
    # Оцениваем точность вручную
    accuracy <- sum(predictions == test_data[[dep_variable]]) / length(predictions)
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_k <- k
    }
  }
  
  return(best_k)
}

