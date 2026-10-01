install.packages("mice")
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
install.packages("shiny")

library(ggthemes)
?theme_few
library(ggplot2)
?facet_wrap


library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(mice)
library(Amelia)
library(randomForest)

getwd()

#Missing Values:
hepdat <- read.csv("Original data with missing values.csv", na.strings = c("?"))

#hepdata: Data with NA
hepdat_backup <- hepdat #Data with NA
hepdat_backup1 <- hepdat #Data with question marks
head(hepdat)
tail(hepdat)
str(hepdat)
summary(hepdat)

missmap(hepdat, main = "Missing Values vs Observed Values")


mod <- mice(hepdat[,!names(hepdat) %in% "medv"], method = "rf")
impoutput <- complete(mod)

impoutput
missmap(impoutput, main = "Missing Values vs Observed Values")
write.csv(impoutput, file = "Consolidated Data.csv", row.names = F)

#Loading Consolidated Data
consoldat <- read.csv("Consolidated Data.csv")
data <- data.frame(consoldat)
summary(data)

train <- data[40:155,]
test <- data[1:40,]

#Relationship b/w age and survival
p <- ggplot(data, aes(AGE, fill=factor(Class)))
p + geom_histogram() +
  facet_grid(.~SEX) +
  theme_few()

##Creating Random Forest Analysis
set.seed(75)

rf_model <- randomForest(factor(Class) ~., data = train)
plot(rf_model, ylim=c(0,1))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

#Importance of each variable
importance <- importance(rf_model)
varimp <- data.frame(Variables = row.names(importance),
                     Importance = round(importance[,'MeanDecreaseGini'],2))
#Rank Variable based on Importance
rankimp <- varimp %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
#Plotting importance of variables
q <- ggplot(rankimp, aes(x=reorder(Variables, Importance),
                         y=importance, fill=Importance))
q + geom_bar(stat='identity') +
  geom_text(aes(x=Variables,y=0.5, label=Rank),
            hjust=0, vjust=0.55, size=4, colour='red') +
  labs(x='Variables') +
  coord_flip() +
  theme_few()

#Predict across entire dataset
prediction <- predict(rf_model,data)

#Solution saved to dataframe of 2 columns: Actual and Predicted Values
solution <- data.frame(Actual = data$Class, Predicted = prediction)

#Save solution in .csv file
write.csv(solution, file='rf_model_solution.csv', row.names = F)

#Final Result
sol <- c("Actually Died and Predicted Dead = 28",
         "Actually Died but Predicted Survived = 4",
         "Actually Survived but Predicted Dead = 1",
         "Actually Survived and Predicted Survived = 122")

final <- matrix(sol, 1, 4, dimnames = NULL)
