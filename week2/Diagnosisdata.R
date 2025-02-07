wbcd <- read.csv(file.choose())

str(wbcd)

wbcd <- wbcd[-1]

table(wbcd$diagnosis)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
 
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469,1 ]
wbcd_test_labels <- wbcd[470:569,1 ]

install.packages("class")
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 2)
install.packages("caret")
library(caret)

confusionMatrix(wbcd_test_labels, wbcd_test_pred)

install.packages("gmodels")
library(gmodels)

k_values <- c(1,5,11,15,21,27)
for (k_val in k_values) {
  wbcd_test_pred <- knn(train = wbcd_train,
                       test = wbcd_test,
                       cl = wbcd_train_labels,
                       k = k_val)
  
CrossTable(x = wbcd_test_labels,
           y = wbcd_test_pred,
           prop.chisq = FALSE)
}





