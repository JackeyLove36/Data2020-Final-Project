cat("\014")
rm(list = ls(all = TRUE))

library(haven)
library(pROC)
library(ggplot2)
library(mice)
library(randomForest)

data <- read_dta("/Users/jinyu/Desktop/ff_data_x_preprocessed_v1.dta")

data$y_binary = as.factor(data$y_binary)

data = data[,-6]
data = data[,-1]
  
summary(data$y_binary)

str(data)

factor_variables <- c("cf1edu", "cm1edu", "f2b32", "m2d2", "cf3marm", "cf3kids", 
                      "cf3md_case_lib", "cf4cohm", "t4d7", "cf1hhinc", "k5d1f", "k5f1f")

for (var in factor_variables) {
  numeric_values <- as.numeric(as.character(data[[var]]))
  numeric_values[numeric_values < 0] <- NA
  data[[var]] <- factor(numeric_values)
}

str(data)

#variable_names_fac <- c("cf1edu", "cm1edu", "f2b32", "m2d2", "cf3marm", "cf3md_case_lib", "cf4cohm", "k5d1f", "k5f1f")

#data[variable_names_fac] <- lapply(data[variable_names_fac], factor)

variable_names <- c("cf3kids", "t4d7", "cf1hhinc")

data[variable_names] <- lapply(data[variable_names], function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))
  } else {
    as.numeric(x)
  }
})








#data_x = data[, -13]

#y_binary = data$y_binary

#data_x <- mice(data_x, m = 5, method = 'pmm', seed = 123)

#data <- complete(data_x, 5)

#data = cbind(data,y_binary)



data <- mice(data, m = 5, method = 'pmm', seed = 123)

data <- complete(data, 5)

#select_train <- sample(nrow(data), nrow(data)*0.9)
#train <- data[select_train, ]
#test <- data[-select_train, ]

#results <- data.frame(seed = integer(), accuracy = numeric())

set.seed(283)
  
data_y1 <- data[data$y_binary == 1, ]
data_y0 <- data[data$y_binary == 0, ]


test_indices_y1 <- sample(nrow(data_y1), 150)
test_indices_y0 <- sample(nrow(data_y0), 150)

test <- rbind(data_y1[test_indices_y1, ], data_y0[test_indices_y0, ])

train <- data[!rownames(data) %in% rownames(test), ]





rf_model <- randomForest(x = train[, -13], y = train$y_binary, ntree = 100)


test_predict <- predict(rf_model, test)
compare_test <- table(test$y_binary, test_predict, dnn = c('Actual', 'Predicted'))
accuracy <- sum(diag(compare_test)) / sum(compare_test)
print(accuracy)
#summary(test$y_binary)[1]/(summary(test$y_binary)[1]+summary(test$y_binary)[2])


imp <- importance(rf_model)
imp <- imp[order(imp, decreasing = TRUE),]  # 按重要性排序
barplot(imp, main="Feature Importance", las=2)

imp_df <- as.data.frame(imp)
imp_df$Feature <- rownames(imp_df)
colnames(imp_df)[1] <- "Importance"

##########这是feature importance的代码
ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col() +  
  coord_flip() +  
  labs(x = "Feature", y = "Importance", title = "Feature Importance") +
  theme_minimal()  



##########这是ROC的代码
test_predict <- predict(rf_model, newdata = test, type = "prob")[, 2]  # 假设第二列是正类的概率
roc_obj <- roc(test$y_binary, test_predict)
auc_value <- auc(roc_obj)

ggplot(data = data.frame(TPR = roc_obj$sensitivities, FPR = 1 - roc_obj$specificities), aes(x = FPR, y = TPR)) +
  geom_line() + 
  geom_abline(linetype = "dashed") +
  annotate("text", x = 0.6, y = 0.2, label = paste("AUC =", round(auc_value, 2)), size = 5, color = "red") +
  labs(x = "False Positive Rate", y = "True Positive Rate", title = "ROC Curve of Random Forest") +
  theme_minimal()






