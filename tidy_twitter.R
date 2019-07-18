
## ----load-tweets, echo=F-------------------------------------------------

## load tweets into R
library('twitteR')
library('tidyverse')
library('tidytext')
library('lubridate')
library('SnowballC')
library('tm')

#authenticate using your login token
consumerKey <- "NTMoENQJ4RQ4rSMTvGX0k0IuU"
consumerSecret <- "7yoIpBCuM9RfxmUt2I3wgyPh5ESl307GI6V2lLbXAe39jxUeSj"
accessToken <- "735776315702288384-QaY452YBvZiZjBAqkjvzEXieVoAPTpp"
accessTokenSecret <- "zUgHvnvJul6bqvLsESPkWFhdv2Ha4joLksj0j2yugGPM1"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

##Retrieve tweets from Twitter
# searchstring <- hashtags <- c("@ANZ_AU") # @ANZ_AU @Westpac @NAB @CommBank
searchstring1 <- hashtags <- c("@realDonaldTrump") # @ANZ_AU @Westpac @NAB @CommBank
searchstring2 <- hashtags <- c("@BarackObama") # @ANZ_AU @Westpac @NAB @CommBank
# searchstring3 <- hashtags <- c("@NAB") # @ANZ_AU @Westpac @NAB @CommBank
# searchstring4 <- hashtags <- c("@CommBank") # @ANZ_AU @Westpac @NAB @CommBank
# searchstring <- paste(hashtags, collapse = " OR ")
tweets1 <- searchTwitter(searchstring1, n = 3200) #since="2017-03-01", until="2017-04-07" , resultType = "popular"
tweets2 <- searchTwitter(searchstring2, n = 3200) #since="2017-03-01", until="2017-04-07" , resultType = "popular"
# tweets3 <- searchTwitter(searchstring3, n = 3200) #since="2017-03-01", until="2017-04-07" , resultType = "popular"
# tweets4 <- searchTwitter(searchstring4, n = 3200) #since="2017-03-01", until="2017-04-07" , resultType = "popular"

rm(hashtags,searchstring1,searchstring2,searchstring3,searchstring4)

t1<-tweets1 %>% twListToDF() %>% as_tibble() %>% mutate(grp = 'trump')
t2<-tweets2 %>% twListToDF() %>% as_tibble() %>% mutate(grp = 'obama')
# t3<-tweets3 %>% twListToDF() %>% as_tibble() %>% mutate(grp = 'nab')
# t4<-tweets4 %>% twListToDF() %>% as_tibble() %>% mutate(grp = 'cba')

tweets.df <- bind_rows(t1
                       ,t2
                       # ,t3
                       # ,t4
                       )

# write_rds(tweets.df,'./data/tweets_big4.rds')

rm(hashtags,searchstring1,searchstring2,searchstring3,searchstring4,t1,t2,t3,t4,tweets1,tweets2,tweets3,tweets4)

## ----print-tweets, tidy=F------------------------------------------------
(n.tweet <- tweets %>% length())
# convert tweets to a data frame
tweets.df <- tweets %>% twListToDF() %>% as_tibble()
# tweet #1
tweets.df[1, c("id", "created", "screenName", "replyToSN","favoriteCount", "retweetCount", "longitude", "latitude", "text")]

# print tweet #1 and make text fit for slide width
tweets.df$text[1] %>% strwrap(60) %>% writeLines()

tweets.df
# write_rds(big4_df,'./data/tweets_big4.rds')

## ----text cleaning functions, tidy=F-------------------------------------

# tweets.df <- read_rds('./data/tweets_big4.rds')

remove_reg <- "https?://[^\\s]+|&amp;|&lt;|&gt;|\bRT\\b" #cleaning of twitter texts
# mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", "fig", "file", "cg", "cb", "cm"))

tidy_tweets <-
  tweets.df %>%
  select(grp,id, created,text) %>%
  filter(!str_detect(text, "^RT")) %>% #remove tweets from this dataset that are retweets so that we only have tweets that we wrote ourselves
  mutate(text = str_remove_all(text, remove_reg), #remove odd characters
         created = ymd_hms(created)) %>% #update date format for use late
  unnest_tokens(word, text, token = "tweets") %>% #token is a variant that retains hashtag # and @ symbols.
  filter(!str_detect(word, "[#@]")) %>% #exclude authors and hastag
  filter(!word %in% stop_words$word,
         # !word %in% mystopwords$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z']+"))

#word stemming
tidy_stem <-
  tidy_tweets %>%
  mutate(word_stem = wordStem(word), #stemming of words (i.e. win, winning, winner)
         word_stem = ifelse(stemCompletion(word_stem,dictionary=tidy_tweets$word, type = 'prevalent') !='',
                       stemCompletion(word_stem,dictionary=tidy_tweets$word, type = 'prevalent'),
                       word)) #complete the incomplete words consistently back to itself

#common words
tidy_stem %>%
  count(grp,word_stem, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word_stem = reorder(word_stem, n)) %>%
  ggplot(aes(word_stem, n)) +
  facet_grid(.~grp, scales = "free")+
  geom_col() +
  xlab(NULL) +
  coord_flip()

library(scales)

#Compare word frequency
tidy_stem %>%
  count(grp, word_stem) %>%
  group_by(grp) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(grp, proportion) %>%
  gather(grp, proportion, anz:nab) %>% #exclude one as base (wbc)

  # expect a warning about rows with missing values being removed
  ggplot(aes(x = proportion, y = wbc, color = abs(wbc - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word_stem), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~grp, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Westpac", x = NULL)


#sentiments
df_sentiment <-
  tidy_stem[,c('grp','id','created','word_stem')] %>%
  rename(word = word_stem) %>%
  mutate(response.date = as.factor(as.numeric(format(created,format ='%Y%m%d')))) %>%
  inner_join(get_sentiments("bing")) %>%
  count(grp, index = response.date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#plot sentiment timeseries
ggplot(df_sentiment, aes(index, sentiment, fill = grp)) +
  geom_col(show.legend = FALSE) +
  facet_grid(.~grp)+ #, scales = "free"
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Sentiments Aggregated into Months")

#Common Sentiments word
tidy_stem[,c('id','grp','created','word_stem')] %>%
  rename(word = word_stem) %>%
  mutate(response.date = as.factor(as.numeric(format(created,format ='%Y%m%d')))) %>%
  inner_join(get_sentiments("bing")) %>%
  count(grp,word,sentiment) %>%
  group_by(grp,sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_grid(sentiment~grp, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

library(reshape2)
library(wordcloud)

#sentiment cloud
tidy_stem[,c('grp','word_stem')] %>%
  rename(word = word_stem) %>%
  inner_join(get_sentiments("bing")) %>%
  count(grp,word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment + grp, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","brown","gold","blue","green"), #extend colours as required
                   max.words = 1500)

#important key words used frequently within each group but not across group.
plot_tf_idf <-
  tidy_stem[,c('grp','word_stem')] %>%
  count(grp, word_stem, sort = TRUE) %>%
  bind_tf_idf(word_stem, grp, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word_stem = factor(word_stem, levels = rev(unique(word_stem)))) %>%
  mutate(grp = as.factor(grp))

plot_tf_idf %>%
  group_by(grp) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word_stem = reorder(word_stem, tf_idf)) %>%
  ggplot(aes(word_stem, tf_idf, fill = grp)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~grp, ncol = 2, scales = "free") +
  coord_flip()

#Bigrams
df_bigrams <-
  tweets.df %>%
  select(grp,text) %>%
  filter(!str_detect(text, "^RT")) %>% #remove tweets from this dataset that are retweets so that we only have tweets that we wrote ourselves
  mutate(text = gsub(" ?@\\w+ ?", "", text), #remove authors from text
         text = str_remove_all(text, remove_reg)) %>% #remove odd characters
  unnest_tokens(word, text, token = "ngrams", n = 2) #token is a variant that retains hashtag # and @ symbols.


df_bigrams_fil <-
  df_bigrams %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         str_detect(word1, "[a-z]+"),
         str_detect(word2, "[a-z]+")) %>%
  mutate(word1_stem = wordStem(word1), #stemming of words (i.e. win, winning, winner)
         word1_stem = ifelse(stemCompletion(word1_stem,dictionary=tidy_tweets$word, type = 'prevalent') !='',
                            stemCompletion(word1_stem,dictionary=tidy_tweets$word, type = 'prevalent'),
                            word1),
         word2_stem = wordStem(word2), #stemming of words (i.e. win, winning, winner)
         word2_stem = ifelse(stemCompletion(word2_stem,dictionary=tidy_tweets$word, type = 'prevalent') !='',
                             stemCompletion(word2_stem,dictionary=tidy_tweets$word, type = 'prevalent'),
                             word2))

bigrams_united <-
  df_bigrams_fil %>%
  unite(bigram, word1_stem, word2_stem, sep = " ")

bigram_tf_idf <-
  bigrams_united %>%
  count(grp, bigram) %>%
  bind_tf_idf(bigram, grp, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>%
  group_by(grp) %>%
  top_n(10, wt=tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = grp)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~grp, ncol = 2, scales = "free") +
  coord_flip()

#handling of negation words
negation_words <- c("not", "no", "never", "without")

neg_words <- df_bigrams_fil %>%
  filter(word1_stem %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2_stem = "word")) %>%
  count(grp, word2_stem, value, sort = TRUE)

neg_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2_stem = reorder(word2_stem, contribution)) %>%
  ggplot(aes(word2_stem, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~grp, ncol = 2, scales = "free") +
  xlab("Words preceded by \"negation terms (not, no, never, without)\"") +
  ylab("Sentiment score * # of occurrences") +
  coord_flip()

# filter for only relatively common combinations
# new bigram counts:

library(igraph)

bigram_graph <-
  df_bigrams_fil %>%
  count(grp, word1_stem, word2_stem, sort = TRUE) %>%
  filter(n > 1) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


df_txt$comment2 <-
  tolower(df_txt$p2p.my.bank.comment) %>%
  str_replace_all("[^[:alpha:][:space:]]*", "") %>%
  str_replace_all(
    c(
      'not good' = 'not_good',
      'not like' = 'not_like',
      'not happy' = 'not_happy',
      'not bad' = 'not_bad',
      'not mandatory' = 'not_mandatory',
      'no advantage' = 'no_advantage',
      'no delays' = 'no_delays',
      "last minute" = "last_minute",
      "didnt sign" = "didnt_sign",
      'last minute' = 'last_minute',
      'not helpful' = 'not_helpful',
      'not great' = 'not_great',
      'not satisfied' = 'not_satisfied',
      'not happy' = 'not_happy',
      'not accept' = 'not_accept',
      'no issues' = 'no_issues',
      'not sign' = 'not_sign'
    )
  )

