#' @title Data splitter
#'
#' @description This function splits data frame into 3 smaller data frames without replacements.
#' @param df dataset
#' @param prob proportion size of train, test and check datasets
#' @rdname split_df
#' @return It returns a list with 3 dataframes
#' @example
#' split_df(titanic, prob = c(0.7, 0.2, 0.1))

split_df <- function(df, prob = c(0.7, 0.2, 0.1)) {
  divide = sample(3, nrow(df), replace = TRUE, prob = prob)
  train = df[divide == 1,]
  test = df[divide == 2,]
  check = df[divide == 3,]
  return(list(train, test, check))

}



#' @title Prediction model
#'
#' @description This function creates and calculates prediction model and checks its accuracy on test model.
#' @param formula Indicates dependent and explanatory variables
#' @param df data provided in data.frame format
#' @param split proportion size of train, test and check datasets
#' @param method indicates which ML method should be used to predict data. Avaiable `logistic regression`, `random forest`, `SVM` and `all`.
#' @param return_all boolean, if `TRUE` it will return all datasets with predictions column
#' @rdname classifier
#' @return Function returns a list containing accuracy results and data frames with predictions
#' @examples
#' classifier(Survived ~ ., titanic, split = c(0.7, 0.2, 0.1), method = 'svm', return_all = FALSE)
#' \dontrun{classifier(Survived ~ ., split = c(0.7, 0.2, 0.1), method = 'all', return_all = TRUE)}
#' @export




classifier <- function(formula, df, split = c(0.7, 0.2, 0.1), method = 'all', return_all = FALSE) {

  formula_words <- stats::terms(formula, data = df)
  formula_words <- attr(formula_words, 'variables')
  formula_words <- as.vector(formula_words, mode = 'list')
  formula_words <- formula_words[-1]
  columns <- colnames(df)
  columns <- as.vector(columns, mode = 'list')
  dependent <- stringr::str_split(formula, '~')[[2]]

  if (!is.data.frame(df)) stop('Data should be provided in Data Frame format')
  if (sum(formula_words %in% columns) < length(formula_words)) stop('Formula contains columns which are not in data frame.')
  if (!method %in% c('all', 'log_reg', 'random_forest', 'svm')) stop('Please specify proper method.')
  if (nrow(df) <= 250) stop('Data frame is too short to provide proper analisys. It should contain at least 250 rows.')
  if (!is.factor(df[, dependent])) stop('Dependent needs to be binomial factor type!')
  if (length(levels(df[, dependent])) > 2) stop('Dependent needs to be binomial factor type!')


  levels(df[, dependent]) <- c('0', '1')
  df1 <- split_df(df)
  train <- df1[[1]]
  test <- df1[[2]]
  check <- dplyr::select(df1[[3]], -as.name(dependent))



  if (method == 'log_reg') {

    logreg <- stats::glm(formula, family = 'binomial', data = train)
    prediction <- stats::predict(logreg, type = 'response', newdata = train)
    prediction <- factor(ifelse(prediction > 0.5, 1, 0))
    train['predicted'] <- prediction
    acc <- data.frame(accuracy_train = caret::confusionMatrix(prediction, train[, dependent])$overall[[1]])

    prediction <- stats::predict(logreg, newdata = test, type = 'response')
    prediction <- factor(ifelse(prediction > 0.5, 1, 0))
    test['predicted'] <- prediction
    acc$accuracy_test <- caret::confusionMatrix(prediction, test[, dependent])$overall[[1]]
    row.names(acc) <- method

    prediction <- stats::predict(logreg, newdata = check, type = 'response')
    prediction <- factor(ifelse(prediction > 0.5, 1, 0))
    check['predicted'] <- prediction
    if (return_all) return(list(train_df = train, test_df = test, accuracy = acc, check_df = check))
    return(list(accuracy = acc, check = check))
  }

  if (method == 'random_forest') {
    rf <- randomForest::randomForest(formula, data = train)
    prediction <- stats::predict(rf, newdata = train)
    train['predicted'] <- prediction
    acc <- data.frame(accuracy_train = caret::confusionMatrix(prediction, train[, dependent])$overall[[1]])

    prediction <- stats::predict(rf, newdata = test)
    test['predicted'] <- prediction
    acc$accuracy_test <- caret::confusionMatrix(prediction, test[, dependent])$overall[[1]]
    row.names(acc) <- method

    prediction <- stats::predict(rf, newdata = check)
    check['predicted'] <- prediction

    if (return_all) return(list(train_df = train, test_df = test, accuracy = acc, check_df = check))
    return(list(accuracy = acc, check = check))
  }

  if (method == 'svm') {
    sup_vec_mach <- e1071::svm(formula, data = train)
    prediction <- stats::predict(sup_vec_mach, newdata = train)
    train['predicted'] <- prediction
    acc <- data.frame(accuracy_train = caret::confusionMatrix(prediction, train[, dependent])$overall[[1]])

    prediction <- stats::predict(sup_vec_mach, newdata = test)
    test['predicted'] <- prediction
    acc$accuracy_test <- caret::confusionMatrix(prediction, test[, dependent])$overall[[1]]
    row.names(acc) <- method

    prediction <- stats::predict(sup_vec_mach, newdata = check)
    check['predicted'] <- prediction

    if (return_all) return(list(train_df = train, test_df = test, accuracy = acc, check_df = check))
    return(list(accuracy = acc, check = check))
  }

  if (method == 'all') {

    logreg <- stats::glm(formula, family = 'binomial', data = train)
    rf <- randomForest::randomForest(formula, data = train)
    sup_vec_mach <- e1071::svm(formula, data = train)

    prediction_log <- stats::predict(logreg, type = 'response', newdata = train)
    prediction_log <- factor(ifelse(prediction_log > 0.5, 1, 0))
    prediction_rf <- stats::predict(rf, newdata = train)
    prediction_svm <- stats::predict(sup_vec_mach, newdata = train)
    train[paste('log_predicted', sep = '')] <- prediction_log
    train[paste('rf_predicted', sep = '')] <- prediction_rf
    train[paste('svm_predicted', sep = '')] <- prediction_svm
    acc <- data.frame(accuracy_train = c(
      caret::confusionMatrix(prediction_log, train[, dependent])$overall[[1]],
      caret::confusionMatrix(prediction_rf, train[, dependent])$overall[[1]],
      caret::confusionMatrix(prediction_svm, train[, dependent])$overall[[1]]
    ))


    prediction_log <- stats::predict(logreg, type = 'response', newdata = test)
    prediction_log <- factor(ifelse(prediction_log > 0.5, 1, 0))
    prediction_rf <- stats::predict(rf, newdata = test)
    prediction_svm <- stats::predict(sup_vec_mach, newdata = test)
    test[paste('log_predicted', sep = '')] <- prediction_log
    test[paste('rf_predicted', sep = '')] <- prediction_rf
    test[paste('svm_predicted', sep = '')] <- prediction_svm
    acc$accuracy_test <- data.frame(accuracy_test = c(
      caret::confusionMatrix(prediction_log, test[, dependent])$overall[[1]],
      caret::confusionMatrix(prediction_rf, test[, dependent])$overall[[1]],
      caret::confusionMatrix(prediction_svm, test[, dependent])$overall[[1]]
    ))
    row.names(acc) <- c('log_reg', 'random_forest', 'svm')

    prediction_log <- stats::predict(logreg, type = 'response', newdata = check)
    prediction_log <- factor(ifelse(prediction_log > 0.5, 1, 0))
    prediction_rf <- stats::predict(rf, newdata = check)
    prediction_svm <- stats::predict(sup_vec_mach, newdata = check)
    check[paste('log_predicted', sep = '')] <- prediction_log
    check[paste('rf_predicted', sep = '')] <- prediction_rf
    check[paste('svm_predicted', sep = '')] <- prediction_svm

    if (return_all) return(list(train_df = train, test_df = test, accuracy = acc, check_df = check))
    return(list(accuracy = acc, check = check))
  }
}