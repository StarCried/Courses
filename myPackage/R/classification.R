#' 使用KNN进行分类
#'
#' 这个函数使用KNN算法对数据进行分类。
#'
#' @param train 训练数据集
#' @param test 测试数据集
#' @param cl 训练数据的标签
#' @param k KNN算法中的k值
#' @return 预测的分类结果和混淆矩阵
#' @export
knn_classification <- function(train, test, cl, k) {
  require(class)
  require(caret)
  predictions <- knn(train, test, cl, k)
  confusion <- confusionMatrix(predictions, test$Species)
  return(list(predictions = predictions, confusion = confusion))
}

#' 使用决策树进行分类
#'
#' 这个函数使用决策树算法对数据进行分类。
#'
#' @param train 训练数据集
#' @param test 测试数据集
#' @return 预测的分类结果和混淆矩阵
#' @export
tree_classification <- function(train, test) {
  require(rpart)
  require(caret)
  model <- rpart(Species ~ ., data = train, method = "class")
  predictions <- predict(model, test, type = "class")
  confusion <- confusionMatrix(predictions, test$Species)
  return(list(predictions = predictions, confusion = confusion))
}

#' 使用支持向量机进行分类
#'
#' 这个函数使用SVM算法对数据进行分类。
#'
#' @param train 训练数据集
#' @param test 测试数据集
#' @return 预测的分类结果和混淆矩阵
#' @export
svm_classification <- function(train, test) {
  require(e1071)
  require(caret)
  model <- svm(Species ~ ., data = train)
  predictions <- predict(model, test)
  confusion <- confusionMatrix(predictions, test$Species)
  return(list(predictions = predictions, confusion = confusion))
}
