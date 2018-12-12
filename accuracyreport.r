library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
#library(doMC)
#registerDoMC(cores=detectCores()) 
df= read.csv("C:/Users/user/Desktop/minor2/pos-neg/predict_try.csv")
#df=c("@SharmaKhemchand @rajnathsingh @hmo should close this JNU. It has become  breeding ground for anti national goons")
df$Sentiment <- as.factor(df$Sentiment)
df$Sentiment
z=df$Sentiment
corpus <- Corpus(VectorSource(df$SentimentText))
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus.clean)
df.test <- df
dtm.test <- dtm
corpus.clean.test <- corpus.clean

testdtm <- DocumentTermMatrix(corpus.clean.test)
dim(testdtm)

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("neg", "pos"))
  y
}

test<- apply(testdtm, 2, convert_count)


super_model <- readRDS("./naivebayesmodel.rds")
#print(super_model)
final_predictions <- predict(super_model, test)
final_predictions
confusionMatrix(final_predictions, z)
table("Predictions"= final_predictions, "Actual" = z)

