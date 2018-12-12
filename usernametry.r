#libraries
library(tm)
library(text2vec)
library(twitteR)
library(ROAuth)
library(httr)
library(plyr)
library(SnowballC)
library(wordcloud)
library(igraph)
library(RColorBrewer)
library(dplyr)
library(purrr)
library(rtweet)
library(NLP)
library(openNLP)
library(base64enc)
#reading tweets from csv file
setwd("C:/Users/user/Desktop/minor2/username")
df<- read.csv(file = "timelineextract.csv",header = TRUE)
documents2 = df$likes
x = df$tweets
#class(x)
xxx = x
xxx = gsub(":", " ", xxx)
xxx= as.character(xxx)



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
clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet)

documents = extractChunks(clean_tweet)
documents <- Corpus(VectorSource(clean_tweet))
documents = tm_map(documents, removeWords, stopwords('english'))

#documents <- tm_map(documents, stemDocument)
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


users <- function(xxx){
 
  xx <- strsplit(xxx, " ")
  lapply(xx, function(xx)xx[grep("@([A-Za-z]+[A-Za-z0-9_]+)", xx)])

}

ans = users(xxx)
tablex <-table(unlist(ans))
tablex<-sort(tablex,decreasing = TRUE)
tablexdf <-as.data.frame(tablex)
username <-as.character(tablexdf$Var1[1])
username <- gsub("@", "", username)
username
setwd("C:/xampp/htdocs")
par(bg = "white")
pal <- brewer.pal(8, "Set1")
#pal <- pal[-(1:2)]
png("usename_cloud.png", width=800,height=800,family = "sans")
wordcloud(tablexdf$Var1,tablexdf$Freq,min.freq = 1,colors = pal,scale = c(5,0.5),rot.per = 0.20)
mem<-dev.off()


dat =tablexdf[1:10,]
dat$fraction = dat$Freq / sum(dat$Freq)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)

dat$ymin = c(0, head(dat$ymax, n=-1))
library(ggplot2)
#p1 = ggplot( data =dat, aes(fill=dat$Var1, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #geom_rect() +
  #coord_polar(theta="y") +
  #xlim(c(0, 4)) +
  #labs(title="Basic ring plot")

p2 = ggplot(data =dat, aes(fill=dat$Var1, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(colour="grey30") +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Customized ring plot")


library(gridExtra)
png("username_ring_plots.png", height=6, width=5, units="in", res=120)
grid.arrange(p2, nrow=1)
zem<-dev.off()









setwd("C:/Users/user/Desktop/minor2/username")

#keyword scapping
#appname ="minor_twitter"

api_key= '794TI5L9FxMMwWgstcmAGqmqm'
api_secret = 'PkvlCxUDYGrJYkrrbvkYG8VzOmU0olUfBKOAYOvnJkCKkt75Qn'
access_token = '966365642193432576-PWcHLPyFXGySppwIC41ggEvWDppvpO9'
access_token_secret = 'DXby7jAaXiVFOBsBrms7JNksStxBmElR2MJGuLKnHvscj'


setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#twitter_token <- create_token(app = appname,consumer_key = key,consumer_secret = secret)

tweets <- userTimeline(user =username, n= 500 ,includeRts = TRUE)

#strip_retweets(tweets, strip_manual=TRUE, strip_mt=TRUE)
x =laply(tweets, function(t) t$getText())
#length(x)
#x
x<-as.data.frame(x)

#rgx = "\\b[keyword]\\b"
rgx <- paste(keyword, collapse = "|")
y <- grep(rgx, x$x,ignore.case = TRUE)
y<-as.array(y)
#length(y)
awe<-if (file.exists("mentioned_user_&_keyword.csv")) file.remove("mentioned_user_&_keyword.csv")
if(length(y)==0){
  print("NO TWEETS EXISTS WITH YOUR USED WORD BY YOUR MOST MENTIONED PERSON")
}else{
  if(length(y)<20){
    for (i in 0:length(y)){
      z = as.character(x$x[y[i]])
      print(z)
      write.table(z,file ="mentioned_user_&_keyword.csv", sep = ",", append = TRUE, quote = FALSE,
                  col.names = FALSE, row.names = FALSE)
    }
    }else{
      for (i in 0:length(20)){
        z = as.character(x$x[y[i]])
        print(z)
        write.table(z,file ="mentioned_user_&_keyword.csv", sep = ",", append = TRUE, quote = FALSE,
                    col.names = FALSE, row.names = FALSE)
        
      }
    }
  }




    




