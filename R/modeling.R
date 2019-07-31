# Modeling
options(scipen = 9999)
library(readr)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)


source("functions.R")

bids.data <- read_csv("bids.csv")
test.data <- read_csv("test.csv")
train.data <- read_csv("train.csv")

# extracting features from bids
feature.x <- create.x(bids.data)
dim(feature.x)
summary(feature.x)


# merging feature and train data
dim(train.data)
data.x <- inner_join(feature.x, train.data %>% select(bidder_id, outcome), by = "bidder_id")
summary(data.x)

# transforming the outcome in factors
data.x$outcome <- as.factor(data.x$outcome)
levels(data.x$outcome) <- c("human", "bot")

# data.x %>% select(everything()) %>%   summarise_all(funs(sum(is.na(.))))
# there were some NA in common country, replaced na with "xx", maybe not the most indicated, by will do the job for now

# # # joining bids and training data
# # class.bids.data <- dplyr::inner_join(bids.data,train.data,by="bidder_id")
#
#
#
# data.bids.x <- create.x(bids.data)
#
# dim(data.bids.x)
#
#
# full.data <- data.x %>% dplyr::inner_join(outcome(class.bids.data),by="bidder_id")
# dim(full.data)
# summary(full.data)
# full.data %>%  select(everything()) %>%   summarise_all(funs(sum(is.na(.))))

cor(data.x[, 2:(dim(data.x)[2] - 1)])

control.tun <- trainControl(method = "repeatedcv", number = 25, repeats = 5, savePredictions = "final", classProbs = TRUE)


model <- train(outcome ~ total.bids + total.auc + avg.bids + total.dist.dev + avg.dev.auc + total.country + avg.country.auc + average.time.to.bid + percent.first + percent.final
  + total.dist.ip + avg.ip.auc, data = data.x, method = "glm", family = binomial(), trControl = control.tun)
# print(varImp(model,scale=FALSE))

model2 <- train(outcome ~ total.bids + total.auc + avg.bids + total.dist.dev + avg.dev.auc + total.country + avg.country.auc + average.time.to.bid + percent.first + percent.final
  + total.dist.ip + avg.ip.auc, data = data.x, method = "rf", trControl = control.tun)
# print(varImp(model2,scale=FALSE))

model3 <- train(outcome ~ total.bids + total.auc + avg.bids + total.dist.dev + avg.dev.auc + total.country + avg.country.auc + average.time.to.bid + percent.first + percent.final
  + total.dist.ip + avg.ip.auc, data = data.x, method = "gbm", trControl = control.tun, verbose = F)
# print(varImp(model2,scale=FALSE))


# 4630 |= 4700, there are 70 bidders on our dataset without any bidding activity
test.data.x <- left_join(test.data %>% select(bidder_id), feature.x, by = "bidder_id")
summary(test.data.x)
# extracting all the users with no bids and attributing 0 to their prediction
no.activity.bidder <- test.data.x %>%
  filter(is.na(total.bids) == TRUE) %>%
  select(bidder_id) %>%
  mutate(prediction = 0)
no.activity.bidder

# test.data.x <- inner_join(test.data %>% select(bidder_id),feature.x,by="bidder_id")
test.data.x <- inner_join(test.data %>% select(bidder_id), feature.x, by = "bidder_id")
summary(test.data.x)
dim(test.data.x)

# removed from the function
# test.data.x <- select(test.data.x,-c("common.phone","common.country"))

# pred.bot <-predict(model2,test.data.x[,2:dim(test.data.x)[2]], type = "prob")$bot
pred.bot <- predict(model, test.data.x[, 2:dim(test.data.x)[2]])
pred <- data.frame("bidder_id" = test.data.x$bidder_id, "prediction" = pred.bot)
results <- rbind(pred, no.activity.bidder)
# head(results)
# dim(results)
# ordering the results as in test file
aux <- results %>% arrange(match(bidder_id, test.data$bidder_id))
write.csv(aux, "LOG25CV10R.csv", row.names = FALSE)

# RF
pred.bot <- predict(model2, test.data.x[, 2:dim(test.data.x)[2]], type = "prob")$bot
pred <- data.frame("bidder_id" = test.data.x$bidder_id, "prediction" = pred.bot)
results <- rbind(pred, no.activity.bidder)
# head(results)
# dim(results)
# ordering the results as in test file
aux <- results %>% arrange(match(bidder_id, test.data$bidder_id))
write.csv(aux, "RF25CV10R.csv", row.names = FALSE)

pred.bot <- predict(model3, test.data.x[, 2:dim(test.data.x)[2]], type = "prob")$bot
pred <- data.frame("bidder_id" = test.data.x$bidder_id, "prediction" = pred.bot)
results <- rbind(pred, no.activity.bidder)
# head(results)
# dim(results)
# ordering the results as in test file
aux <- results %>% arrange(match(bidder_id, test.data$bidder_id))
write.csv(aux, "GBM25CV10R.csv", row.names = FALSE)


avg.pred.bot <- apply(cbind(predict(model2, test.data.x[, 2:dim(test.data.x)[2]], type = "prob")$bot, predict(model3, test.data.x[, 2:dim(test.data.x)[2]], type = "prob")$bot), 1, mean)
res.avg.pred.bot <- data.frame("bidder_id" = test.data.x$bidder_id, "prediction" = avg.pred.bot)
results <- rbind(res.avg.pred.bot, no.activity.bidder)
aux.avg <- results %>% arrange(match(bidder_id, test.data$bidder_id))
write.csv(aux.avg, "AVG25r10submit.csv", row.names = FALSE)