

unknown <- data.frame(ingredient =  c("tomato", "carrot"),
                      sweetness = c(6,4),
                      crunchiness = c(4,9))
unknown


pred <- knn(select(things, sweetness, crunchiness), 
            select(unknown,sweetness, crunchiness), things$class, k=1)
pred

pred <- knn(select(things, sweetness, crunchiness), 
            select(unknown,sweetness, crunchiness), things$class, k=4)
pred

#install.packages("stargazer")
library(dplyr)
library(ggplot2)
library(stargazer)


loan <- read.csv("C:/Users/hcazs/Downloads/LoanStats3a_securev1.csv", skip=1)
table(loan$loan_status)
loan <- filter(loan, loan_status!="")
loan$good <- ifelse(loan$loan_status == "Current" | 
                      loan$loan_status == "Fully Paid" |
                      loan$loan_status == "Does not meet the credit policy.  Status:Fully Paid",
                    "good","bad")
loan$fico <- (loan$fico_range_high+loan$fico_range_low)/2
table(loan$good)
stargazer(select(filter(loan, good == "good"),dti, fico), median = TRUE, type = "text")
stargazer(select(filter(loan, good == "good"),dti, fico), median = TRUE, type = "text")
ggplot(aes(x = dti, color = factor(good)) ,data = loan) + geom_density()
ggplot(aes(x = fico, color = factor(good)) ,data = loan) + geom_density()
loan <- loan %>% select(good, fico, dti)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

loan$fico_n <- normalize(loan$fico)
loan$dti_n <- normalize(loan$dti)
summary(loan[,c("fico", "fico_n")])

set.seed(364)

sample <- sample(nrow(loan),floor(nrow(loan)*0.8))

train <- loan[sample,]
test <- loan[-sample,]

prop.table(table(train$good))

prop.table(table(test$good))

train_knn <- select(train, fico_n, dti_n)
test_knn <- select(test, fico_n, dti_n)

library(class)
pred <- knn(train_knn, test_knn, train$good, k = 5)
head(pred)

library(gmodels) #contains CrossTable function
CrossTable(x = test$good, y = pred, prop.chisq = FALSE)

confusionMatrix(factor(test$good), factor(pred))
