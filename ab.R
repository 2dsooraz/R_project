
data(mtcars)
str(mtcars)
install.packages("tm")
install.packages("twitteR")
install.packages("igraph")
library(tm)
library(twitteR)
library(ggplot2)

install.packages("readxl")
install.packages("foreign")
install.packages("readr")
install.packages("jsonlite")
library(dplyr)
install.packages("magrittr")
library(magrittr)
df <- data(iris)
str(iris)
ggplot(iris, aes(Petal.Length,fill=Species))+ geom_histogram()
install.packages("data.table")
#--------Q.NO. 9 dubei ani 10 ko or question yesmei parcha ---------using text mining------------------- 
#load the twitter file saved in your location 
tweets<-load(file = "./data/rdmTweets.RData")
#count the number of tweets 
(n.tweet <- length(tweets))
#View the text variable of 320 tweets
strwrap(tweets[[320]]$text, width = 55)
## convert tweets to a data frame
df <- twListToDF(tweets)
# build a corpus
myCorpus <- Corpus(VectorSource(df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuations and numbers
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs, http followed by non-space characters
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# remove r and big from stopwords
myStopwords <- setdiff(stopwords("english"), c("r", "big"))
# remove stopwords
myCorpus <- tm_map(myCorpus, removeWords,myStopwords)
# keep a copy of corpus
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary = myCorpusCopy)
# replace "miners" with "mining", because "mining" was first stemmed to "mine" and then completed to "miners"
myCorpus <- tm_map(myCorpus, gsub, pattern="miners", replacement="mining")
strwrap(myCorpus[320], width=55) #check the corpus again (iteratively)!
myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
# inspect frequent words
(freq.terms <- findFreqTerms(myTdm, lowfreq=20))
#co-occurance of the term 
findAssocs(myTdm, "r", 0.2)
install.packages("wordcloud")
library(wordcloud)
m <- as.matrix(myTdm)
freq <- sort(rowSums(m), decreasing=T)
wordcloud(words=names(freq), freq=freq, min.freq=4,random.order=F)


#-------------q.no 10 -------------SOCIAL NETWORK ANALYSIS ----------
library(igraph)
g <- graph(c(1,2))
plot(g)
g1<- graph(c("S","R","R","G","G","S","S","G","A","R"))
plot(g1,
     vertex.color="green",
     vertex.size="30",
     edge.color="red",
     edge.size=5)
degree(g1,mode="all")
closeness(g1,mode="all",weights=NA)
betweenness(g1,directed = T,weights=NA)
diameter(g1,directed=F, weights=NA)
edge_Density(g1, loops=F)
reciprocity(g1)
#------q.no.6 -------------
(i1 <- c(1:15))
(i2 <- seq(from=1,to=15,by=1))
(R <- rep(c(1:15), length.out=15,each=1))
(N <- c(1.1,2.2,3.3,4.4,5.5))
R+N
R-N
R*N
R/N
L <- list("This","is","my","first","programming","in","R")
UL <- as.character(L)
#----------q.no.7-----------
df <- read.csv("pollution.csv", header=TRUE)
str(df)
df$particular_matter <- as.factor(df$particular_matter)
df$date_time <- as.Date(df$date_time)
df$value <- as.integer(df$value)
#-----removing outlier----
summary(df)
boxplot(df)
data1 <- df$value
length(df)
#bench <- (Value in mean) + 1.5*IQR(data1)
data1 <- data1( data1 < bench)
summary(data1)
boxplot(data1)
length(data1)
#summary statistics of "value" variable by "particular_matter" categories
data %>%  data1 %>% group_by(particular_matter) %>% 
  summarize(data1$value)





#-------------q.no. 8 -------------
ggplot(df, aes(particular_matter))+geom_bar()
ggplot(df, aes(value))+geom_histogram()
ggplot(df, aes(date_time,value))+geom_line()
a <- ggplot(df, aes(value,fill=particular_matter))+geom_histogram()
summary(a)


