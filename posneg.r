library(tm)
library(RTextTools)
library(e1071)
library(dplyr)

library(NLP)
library(openNLP)
library(wordcloud)
library(text2vec)
library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(SnowballC)
library(purrr)
#library(qdap)
#library(ggplot2)
#library(rtweet)
library(NLP)
library(openNLP)
library(base64enc)
# Library for parallel processing
#library(doMC)
#registerDoMC(cores=detectCores()) 
df= read.csv("C:/Users/user/Desktop/minor2/username/timelineextract.csv")
x= df$tweets
extractChunks <- function(x) {
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  tokenizedAndTagged <- data.frame(Tokens = x[POSwords], Tags = tags)
  tokenizedAndTagged$Tags_mod = grepl("NN", tokenizedAndTagged$Tags)
  chunk = vector()
  chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
  for (i in 2:nrow(tokenizedAndTagged)) {
    if(!tokenizedAndTagged$Tags_mod[i]) {
      
      chunk[i] = 0
      
    } else if (tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]) {
      
      chunk[i] = chunk[i-1]
      
    } else {
      
      chunk[i] = max(chunk) + 1
      
    }
  }
  text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
  tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
  names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
  res = text_chunk[grepl("NN", names(text_chunk))]
  res = sapply(res, function(x) paste(x, collapse =  " "))
  
  return(res)
  gc()
}



documents = tolower(x)
clean_tweet = gsub("&amp", " ", documents)
clean_tweet = gsub('http\\S+\\s*'," ", clean_tweet)
clean_tweet = gsub("(RT)", " ", clean_tweet)
clean_tweet= gsub("[_/*-]","", clean_tweet)
clean_tweet = gsub("@[[:alnum:]]+", " ", clean_tweet)
clean_tweet = gsub("[[:punct:]]", " ", clean_tweet)
clean_tweet = gsub("[[:digit:]]", " ", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet)
documents = extractChunks(clean_tweet)
documents <- Corpus(VectorSource(documents))
documents = tm_map(documents, removeWords, stopwords('english'))

#documents <- tm_map(documents, stemDocument)
library(caret)
dtm_train1 = TermDocumentMatrix(documents)
m1 <- as.matrix(dtm_train1)
#m
v1 <- sort(rowSums(m1),decreasing=TRUE)
#dim(v)
d1 <- data.frame(word = names(v1),freq=v1)
keyword1= d1$word[1]
keyword1 = as.character(keyword1)
keyword1

keyword = keyword1
setwd("C:/xampp/htdocs")
library(ggplot2)
library(qdap)
par(bg = "white")
pal <- brewer.pal(8, "Set1")
#pal <- pal[-(1:2)]

png("keyword_cloud.png", width=800,height=800,family = "sans")
wordcloud(d1$word,d1$freq, min.freq = 1,colors = pal,scale = c(5,0.5),rot.per = 0.20)
kex<-dev.off()

png("keyword_bargraph.png",width=700,height=800)
barplot(d1[1:10,]$freq,las=2,names.arg = d1[1:10,]$word,col="pink",main="MOST FREQUENT WORD",ylab="WORD FREQUENCIES")
kexx<-dev.off()
#keyword = "padmavat"
api_key= '794TI5L9FxMMwWgstcmAGqmqm'
api_secret = 'PkvlCxUDYGrJYkrrbvkYG8VzOmU0olUfBKOAYOvnJkCKkt75Qn'
access_token = '966365642193432576-PWcHLPyFXGySppwIC41ggEvWDppvpO9'
access_token_secret = 'DXby7jAaXiVFOBsBrms7JNksStxBmElR2MJGuLKnHvscj'


setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
tweets <- searchTwitter(keyword, n=200,lang = "en",resultType = "popular")
tweets <-strip_retweets(tweets)
x =laply(tweets, function(t) t$getText())
y =laply(tweets, function(t) t$getScreenName())
x<-as.data.frame(x)
y<-as.data.frame(y)
y
setwd("C:/Users/user/Desktop/divyansh")
kexx<-if (file.exists("wordtweet.csv")) file.remove("wordtweet.csv")
write.csv(x, file="wordtweet.csv")
posnegdf= read.csv("wordtweet.csv")
doc = posnegdf$x
ass = doc
ass = as.data.frame(ass)
doc= as.character(doc)
clean_tweet = gsub("&amp", " ", doc)
clean_tweet = gsub('http\\S+\\s*'," ", clean_tweet)
clean_tweet = gsub("(RT)", " ", clean_tweet)
clean_tweet= gsub("[_/*-]","", clean_tweet)
clean_tweet = gsub("@[[:alnum:]]+", " ", clean_tweet)
clean_tweet = gsub("[[:punct:]]", " ", clean_tweet)
clean_tweet = gsub("[[:digit:]]", " ", clean_tweet)
clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet)
#df$Sentiment <- as.factor(df$Sentiment)
corpus <- Corpus(VectorSource(clean_tweet))
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus.clean)
df.test <- posnegdf
dtm.test <- dtm
corpus.clean.test <- corpus.clean
testdtm <- DocumentTermMatrix(corpus.clean.test)
#dim(testdtm)


convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("neg", "pos"))
  y
}

test<- apply(testdtm, 2, convert_count)


super_model <- readRDS("./naivebayesmodel.rds")
#print(super_model)
final_predictions <- predict(super_model, test)
final<-as.data.frame(final_predictions)
final
#class(final)
#final_predictions
#final
#class(final)

arr= c()
for(i in 1:nrow(final)){
  if(as.numeric(final$final_predictions[i]==1))
  {
    arr[i]=1
  }else{
    arr[i]=0
  }
}
arr

for(i in 1:length(arr)){
  arr[i]=0.25*arr[i]
}
#arr


good = scan('C:/Users/user/Desktop/minor2/pos-neg/opinion-lexicon-English/positive-words.txt',
           what='character', comment.char=';')
bad = scan('C:/Users/user/Desktop/minor2/pos-neg/opinion-lexicon-English/negative-words.txt',
           what='character', comment.char=';')

bad_text = c(bad)
good_text = c(good)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

final_score<- score.sentiment(doc, good_text, bad_text, .progress='text')
#final_score
final_score<-cbind(final_score,y)
final_score$
final_score$score=final_score$score*0.75
#final_score$score

final_score$score=arr+final_score$score
#final_score
abs_score<-final_score[order(final_score$score),]
g
countpos=1
countneg =1
tdy<-if (file.exists("positivetweets.csv")) file.remove("positivetweets.csv")
tdyy<-if (file.exists("negativetweets.csv")) file.remove("negativetweets.csv")
tdy<-if (file.exists("positivetweets.txt")) file.remove("positivetweets.txt")
tdyy<-if (file.exists("negativetweets.txt")) file.remove("negativetweets.txt")
#as.character(final$final_predictions[2])=="1"
#as.character(ass$ass[2])
for(j in 1:10){
  if(abs_score$score[j]<0){
  neg = as.character(abs_score$text[j])
  #neg=as.data.frame(neg)
  
  #neg_handle=as.data.frame(neg_handle)
  nega=cbind(neg_handle,neg)
  write.table(nega,file ="negativetweets.csv", sep=",",append = TRUE, quote = FALSE,row.names = FALSE,col.names = FALSE)
  write.table(nega,file ="negativetweets.txt", append = TRUE, quote = FALSE)
}
}
n=nrow(abs_score)
for(i in n:(n-9)){
  if(abs_score$score[i]>0){
    pos = as.character(abs_score$text[i])
    pos_handle=as.character(abs_score$y[i])
    posa=cbind(pos_handle,pos)
    write.table(posa,file ="positivetweets.csv", sep=",",append = TRUE, quote = FALSE,row.names = FALSE,col.names = FALSE)
    write.table(posa,file ="positivetweets.txt", append = TRUE, quote = FALSE)
  }
}

#countpos
#countneg
