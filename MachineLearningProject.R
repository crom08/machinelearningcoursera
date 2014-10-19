library(caret)
library(randomForest)

load.data <- function(data) {
    read.csv(data, row.names = 1)
}

remove.zero.covariates <- function(data) {
    near.zero <- nearZeroVar(data, saveMetrics = TRUE)
    data[, !near.zero$nzv]
}

remove.missing.values <- function(data) {
    nav <- sapply(colnames(data), function(x) if(sum(is.na(data[, x])) > 0.8*nrow(data)){return(T)}else{return(F)})
    data <- data[, !nav]
#     mv <- sapply(colnames(data), function(col) {
#         if (sum(is.na(data[, col]))/nrow(data) > 0.8) {
#             return(TRUE)
#         }
#         else {
#             return(FALSE)
#         }
#     })
#     data[, !mv]
}

clean.data <- function(data) {
    data <- remove.zero.covariates(data)
    data <- remove.missing.values(data)
    data
}

fit.model <- function(trainingData) {
    set.seed(123)
    train(classe ~., data = trainingData, 
          tuneGrid = data.frame(mtry=3),
          trControl = trainControl(method="none"))
    
    
#     random.forest <- train(trainingData[,-57],
#                            trainingData$classe,
#                            tuneGrid = data.frame(mtry=3),
#                            trControl = trainControl(method="none")
#     )
#     random.forest
    
#     train(classe ~ ., data = trainingData,
#           method = "rf",
#           trControl = trainControl(
#               method = "cv", number = 10))
}

predict.using.data <- function(modelFit, data) {
#     as.character(predict(modelFit, data))
    
    as.character(predict(modelFit, newdata = data))
}

write.predictions <- function(x) {
    n <- length(x)
    for (i in 1:n) {
        file <- paste0("./Predictions/problem_id_", i, ".txt")
        write.table(x[i], file = file, 
                    quote = FALSE, row.names = FALSE,
                    col.names = FALSE)
    }
}

trainingData <- "./Data/Training Data/pml-training.csv"
testingData <- "./Data/Testing Data/pml-testing.csv"

training <- load.data(trainingData)
testing  <- load.data(testingData)

training <- clean.data(training)
testing <- clean.data(testing)
modelFit <- fit.model(training)
predictions <- predict.using.data(modelFit, testing)
write.predictions(predictions)