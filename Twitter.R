## ----author info, include=F----------------------------------------------
## Author:  Yanchang Zhao
## Email:   yanchang@RDataMining.com
## Website: http://www.RDataMining.com
## Date:    26 May 2017

#setwd('./GettingCleaning')

## Verifies that the required libraries get installed if needed
verify_deps <- function(...) {
  lapply(list(...), function(lib) {
    if (!lib %in% installed.packages()) 
      install.packages(lib)
  })
}

## Verify dependencies
verify_deps("magrittr", "twitteR",'tm','ggplot2','graph','Rgraphviz','RColorBrewer','wordcloud','topicmodels','data.table','sentiment','stringr')

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

## Load the dependencies

## ----load libraries, include=F, echo=F-----------------------------------
## load required packages
library(magrittr) ## for pipe operations
library(twitteR)
library(tm)
library(ggplot2)
library(graph)
library(Rgraphviz)
library(RColorBrewer)
library(wordcloud)
#library(fpc)
library(topicmodels)
library(data.table) # month(), asIDate()
library(sentiment)
library(stringr)

## ----term weighting, eval=F, tidy=F--------------------------------------
# library(magrittr)
# library(tm) ## package for text mining
# a <- c("I like R", "I like Python")
# ## build corpus
# b <- a %>% VectorSource() %>% Corpus()
# ## build term document matrix
# m <- b %>% TermDocumentMatrix(control=list(wordLengths=c(1, Inf)))
# m %>% inspect()
# ## various term weighting schemes
# m %>% weightBin() %>% inspect() ## binary weighting
# m %>% weightTf() %>% inspect() ## term frequency
# m %>% weightTfIdf(normalize=F) %>% inspect() ## TF-IDF
# m %>% weightTfIdf(normalize=T) %>% inspect() ## normalized TF-IDF

## ----retrieve-tweets, eval=F---------------------------------------------
## ## Option 1: retrieve tweets from Twitter
## library(twitteR)
## library(ROAuth)
## ## Twitter authentication
## setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
## ## 3200 is the maximum to retrieve
## tweets <- "RDataMining" %>% userTimeline(n=3200)

## ----download-tweets, eval=F---------------------------------------------
## ## Option 2: download @RDataMining tweets from RDataMining.com
## library(twitteR)
## url <- "http://www.rdatamining.com/data/RDataMining-Tweets-20160212.rds"
## download.file(url, destfile="./data/RDataMining-Tweets-20160212.rds")
## ## load tweets into R
## tweets <- readRDS("./data/RDataMining-Tweets-20160212.rds")

## ----load-tweets, echo=F-------------------------------------------------

## load tweets into R
library(twitteR)
library(tidyverse)

#authenticate using your login token
consumerKey <- "NTMoENQJ4RQ4rSMTvGX0k0IuU"
consumerSecret <- "7yoIpBCuM9RfxmUt2I3wgyPh5ESl307GI6V2lLbXAe39jxUeSj"
accessToken <- "735776315702288384-QaY452YBvZiZjBAqkjvzEXieVoAPTpp"
accessTokenSecret <- "zUgHvnvJul6bqvLsESPkWFhdv2Ha4joLksj0j2yugGPM1"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

##Retrieve tweets from Twitter
searchstring <- hashtags <- c("@NAB") # @ANZ_AU @Westpac @NAB @CommBank
# searchstring <- paste(hashtags, collapse = " OR ")
tweets <- searchTwitter(searchstring, n = 3200) #since="2017-03-01", until="2017-04-07" , resultType = "popular"
rm(hashtags,searchstring)

## ----print-tweets, tidy=F------------------------------------------------
(n.tweet <- tweets %>% length())
# convert tweets to a data frame
tweets.df <- tweets %>% twListToDF()
# tweet #1
tweets.df[1, c("id", "created", "screenName", "replyToSN",
               "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
# print tweet #1 and make text fit for slide width
tweets.df$text[1] %>% strwrap(60) %>% writeLines()

tweets.df
# write_rds(big4_df,'./data/tweets_big4.rds')

## ----text cleaning functions, tidy=F-------------------------------------

big4_df <- read_rds('./data/tweets_big4.rds')

library(tidytext)

data(stop_words)

text_df <- 
  big4_df %>%
  select(id,text) %>%
  as_tibble() 

tidy_df <-
  text_df %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_replace(word, "[^[:graph:]]", "")) %>% # #remove all non graphical characters
  mutate(word = str_replace(word, "http[^[:space:]]*", "")) %>% # function for removing URLs, i.e."http" followed by any non-space letters
  mutate(word = str_replace(word, "[^[:alpha:][:space:]]*", "")) %>% # function for removing anything other than English letters or space
  anti_join(stop_words)
  

library(ggplot2)

tidy_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggplot(big4_df, aes(x = created, fill = bank)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~bank, ncol = 1)

# Because we have kept text such as hashtags and usernames in the dataset, we can’t use a simple anti_join() to remove stop words. Instead, we can take the approach shown in the filter() line that uses str_detect() from the stringr package.

remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- big4_df %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

frequency <- tidy_tweets %>% 
  group_by(bank) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(bank) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

library(tidyr)

frequency <- frequency %>% 
  select(bank, word, freq) %>% 
  spread(bank, freq)

frequency

library(scales)

ggplot(frequency, aes(anz, cba)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, bank) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(bank, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(anz / cba)) %>%
  arrange(desc(logratio))

word_ratios %>% 
  arrange(abs(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (David/Julia)") +
  scale_fill_discrete(name = "", labels = c("David", "Julia"))

## ----prepare-text, tidy=F------------------------------------------------
library(tm)

# build a corpus and specify the source to be character vectors
corpus.raw <- tweets.df$text %>% VectorSource() %>% Corpus()

# text cleaning
corpus.cleaned <- corpus.raw %>% 
  # convert to lower case
  tm_map(content_transformer(tolower)) %>% 
  # remove URLs
  tm_map(content_transformer(removeURL)) %>% 
  # remove numbers and punctuations
  tm_map(content_transformer(removeNumPunct)) %>% 
  # remove stopwords
  tm_map(removeWords, myStopwords) %>% 
  # remove extra whitespace
  tm_map(stripWhitespace) 

## ----stemming and stem-completion, tidy=F--------------------------------
## stem words
corpus.stemmed <- corpus.cleaned %>% tm_map(stemDocument)

## stem completion
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  stripWhitespace(x)
}

corpus.completed <- corpus.stemmed %>% 
  lapply(stemCompletion2, dictionary=corpus.cleaned) %>% 
  VectorSource() %>% Corpus()

## ----before/after text cleaning, tidy=F----------------------------------
# original text
corpus.raw[[1]]$content %>% strwrap(60) %>% writeLines()
# after basic cleaning
corpus.cleaned[[1]]$content %>% strwrap(60) %>% writeLines()
# stemmed text
corpus.stemmed[[1]]$content %>% strwrap(60) %>% writeLines()
# after stem completion
corpus.completed[[1]]$content %>% strwrap(60) %>% writeLines()

## ----fix-mining, tidy=F--------------------------------------------------

# replace old word with new word
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}
corpus.completed <- corpus.completed %>% 
  replaceWord("pexaaustralia", "pexa")

## ----term-doc-matrix, tidy=F---------------------------------------------
tdm <- corpus.completed %>% 
  TermDocumentMatrix(control = list(wordLengths = c(1, Inf)))  %>% 
  print

## ----frequent-terms, out.truncate=70-------------------------------------
# inspect frequent words
freq.terms <- tdm %>% findFreqTerms(lowfreq=2) %>% print
term.freq <- tdm %>% as.matrix() %>% rowSums()
term.freq <- term.freq %>% subset(term.freq>=2)
df <- data.frame(term=names(term.freq), freq=term.freq)

## ----plot-frequent-terms, tidy=F, fig.align="center", fig.width=4, fig.height=3, out.height=".8\\textheight"----
library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))


## ----wordcloud1----------------------------------------------------------
m <- tdm %>% as.matrix
# calculate the frequency of words and sort it by frequency
word.freq <- m %>% rowSums() %>% sort(decreasing=T)

# colors
library(RColorBrewer)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

## ----eval=F--------------------------------------------------------------
## # plot word cloud
## library(wordcloud)
## wordcloud(words=names(word.freq), freq=word.freq, min.freq=3, random.order=F, colors=pal)

## ----wordcloud2, fig.width=8, out.width="0.9\\textwidth", crop=T, echo=F----
wordcloud(words=names(word.freq), freq=word.freq, min.freq=3, random.order=F, colors=pal)

## ----association---------------------------------------------------------
# which words are associated with "pexa"?
tdm %>% findAssocs('pexa', 0.2)


## ----network, fig.width=12, out.width="1.05\\textwidth", out.height="0.6\\textwidth", crop=T----
library(graph)
library(Rgraphviz)
plot(tdm, term=freq.terms, corThreshold=0.3, weighting=T
     ,attrs=list(node=list(label="foo", 
                          fillcolor="lightgreen",
                          fontsize=50,
                          height=1.8,
                          width=1.8),
                edge=list(color="pink",width="0.4")))

## ----clustering----------------------------------------------------------
# remove sparse terms
m2 <- tdm %>% removeSparseTerms(sparse=0.95) %>% as.matrix()

# calculate distance matrix
dist.matrix <- m2 %>% scale() %>% dist()

# hierarchical clustering
fit <- dist.matrix %>% hclust(method="ward")

## ----save-data,echo=F----------------------------------------------------
# save m2 for social network analysis later
term.doc.matrix <- m2
term.doc.matrix %>% save(file="./data/termDocMatrix.rdata")

## ----plot-cluster, fig.width=8, fig.height=6, out.height='.9\\textwidth'----
plot(fit)
fit %>% rect.hclust(k=6) # cut tree into 6 clusters
groups <- fit %>% cutree(k=6)

## ----kmeans--------------------------------------------------------------
m3 <- m2 %>% t() # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed to make the result reproducible
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits=3) # cluster centers

## ----print-clusters------------------------------------------------------
for (i in 1:k) {
  cat(paste("cluster ", i, ":  ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:5], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

## ----echo=F--------------------------------------------------------------
set.seed(523)

## ----topic modelling-----------------------------------------------------
dtm <- tdm %>% as.DocumentTermMatrix()
library(topicmodels)
lda <- LDA(dtm, k=8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic
term <- apply(term, MARGIN=2, paste, collapse=", ") %>% print

## ----density-plot, tidy=F, fig.width=10, out.width="\\textwidth", out.height="0.5\\textwidth", fig.align='center', crop=T----
rdm.topics <- topics(lda) # 1st topic identified for every document (tweet)
rdm.topics <- data.frame(date=as.IDate(tweets.df$created), 
                         topic=rdm.topics)
ggplot(rdm.topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")

## ----eval=F--------------------------------------------------------------
## # install package sentiment140
## require(devtools)
## install_github('sentiment140', 'okugami79')

## ----sentiment-----------------------------------------------------------
# sentiment analysis
library(sentiment)
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)
# sentiment plot
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data=sentiments, sum)

## ----sentiment plot------------------------------------------------------
plot(result, type="l")
