library(caret)
library(randomForest)

load.data <- function(file) {
    read.csv(file, row.names = 1)
}

remove.zero.covariates <- function(data) {
    near.zero <- nearZeroVar(data, saveMetrics = TRUE)
    data[, !near.zero$nzv]
}

remove.missing.values <- function(data) {
    missing.values <- sapply(colnames(data), function(col) {
        if (sum(is.na(data[, col]))/nrow(data) > 0.8) {
            return(TRUE)
        }
        else {
            return(FALSE)
        }
    })
    data[, !missing.values]
}

clean.data <- function(data) {
    data <- remove.zero.covariates(data)
    remove.missing.values(data)
}

fit.model <- function(trainingData) {
    set.seed(123)
    train(classe ~ ., data = trainingData, 
          tuneGrid = data.frame(mtry=3),
          trControl = trainControl(method="oob"))
}

predict.using.data <- function(modelFit, data) {
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
testing  <- clean.data(testing)

modelFit    <- fit.model(training)
predictions <- predict.using.data(modelFit, testing)
write.predictions(predictions)