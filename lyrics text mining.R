## load libraries
library(tidyverse)
library(tidytext)
library(sentimentr)
library(ggridges)
library(scales)
library(topicmodels)
library(tm)
library(forcats)
library(wordcloud2)
library(udpipe)
library(igraph)
#install.packages("ggraph")
library(ggraph)
library(caret)

## read data & mutate data
billboard <- readRDS("data/billboard_join_final.rds")

billboard <- readRDS("data/billboard_join_final.rds") %>% 
              mutate(decade = 
                      ifelse(billboard$year %in% 1970:1979, "1970s", 
                             ifelse(billboard$year %in% 1980:1989, "1980s", 
                                   ifelse(billboard$year %in% 1990:1999, "1990s", 
                                          ifelse(billboard$year %in% 2000:2009, "2000s", 
                                                 ifelse(billboard$year %in% 2010:2017, "2010s","1960s"))))))

## creat lytics text
lyrics_text <- data.frame(docID = c(1:nrow(billboard)),
                          song_name = billboard$song_name,
                          text = as.character(billboard$lyrics),
                          stringsAsFactors = F)

# billboard <- billboard %>% 
#   mutate(docID=c(1:nrow(billboard)),
#          decade = ifelse(year < 1978, "1968-1977", 
#                          ifelse(year >= 1978 & year < 1988, "1978-1987", 
#                                 ifelse(year >= 1988 & year < 1998, "1988-1997", 
#                                        ifelse(year >= 1998 & year < 2008, "1998-2007","2008-2017")))))

billboard$lyrics <- as.character(billboard$lyrics)

#----------------------------------- PART 1: word freqs ----------------------------------------------
# freq without stop words
freq <- billboard %>%
  unnest_tokens(word, lyrics) %>%
  count(word,sort=TRUE) %>% 
  anti_join(stop_words,by ="word") 

freq %>%
  slice(1:25) %>% 
ggplot(aes(x=fct_reorder(word,n), y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_y_continuous(labels=comma) +
  scale_fill_gradient(low = "white", high = "black")

## word cloud
freq <- billboard %>%
  select(docID, lyrics) %>%
  unnest_tokens(word, lyrics) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("yeah","la","ooh","hey","da", "uh", "yo", "na")) %>%
  arrange(desc(n)) %>%
  rename(freq=n)

wc <- wordcloud2(data = freq)

## TFIDF by song

lyrics <- billboard %>%
  unnest_tokens(word, lyrics) %>% 
  select(docID, song_name, word)

tmp <- lyrics %>%
  count(docID,word) %>%
  bind_tf_idf(word,docID,n) %>%
  group_by(docID) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>% # get top 10 words in terms of tf-idf
  inner_join(select(billboard, song_name, docID),by='docID')

docs <- c(3684, 3489, 3407, 3522, 3526, 3446)

plDF <- tmp %>% 
  filter(docID %in% docs) %>%
  ungroup() %>%
  mutate(xOrder=n():1)

TFIDF_song <-plDF %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=song_name)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ song_name,scales='free',nrow = 1) +
  scale_x_continuous(breaks = plDF$xOrder,
                     labels = plDF$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words for 7 Songs')+
  theme_bw()

# freq by decade
freq <- billboard %>%
  select(docID, decade,lyrics) %>%
  unnest_tokens(word, lyrics) %>%
  count(decade,word) %>%
  anti_join(stop_words) %>%
  group_by(decade) %>%
  arrange(desc(n)) %>%
  slice(1:25) %>%
  ungroup() %>%
  mutate(xOrder=n():1)

freq %>%
  ggplot(aes(x=xOrder,y=n)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ decade,scales='free',nrow = 1) +
  scale_x_continuous(breaks = freq$xOrder,
                     labels = freq$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='Word  Frequency',
       title = "Top Words by decade",
       subtitle = paste0('Based on ', 
                         nrow(billboard),
                         ' lyrics'))

## TFIDF by decade

decade <- billboard %>%
  unnest_tokens(word, lyrics) %>% 
  select(decade, song_name, word)

tmp <- decade %>%
  count(decade,word) %>%
  bind_tf_idf(word,decade,n) %>%
  group_by(decade) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:20)

plDF <- tmp %>% 
  ungroup() %>% 
  mutate(xOrder=n():1)

TFIDF_decade <- plDF %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=decade)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ decade,scales='free',nrow = 1) +
  scale_x_continuous(breaks = plDF$xOrder,
                     labels = plDF$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words for 5 decades')+
  theme_bw()

# freq by genre
## pop
freq <- billboard %>%
  filter(pop == 1) %>% 
  unnest_tokens(word, lyrics) %>%
  count(word,sort=TRUE) %>% 
  anti_join(stop_words,by ="word") 

freq %>%
  slice(1:25) %>% 
  ggplot(aes(x=fct_reorder(word,n),y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_y_continuous(labels=comma) +
  scale_fill_gradient(low = "white", high = "black")

## TFIDF by genre
# pop,rock,RandB,soul,hiphop,rap,country,dance,alternative_indie,blues,disco,
# funk,jazz,folk,easy_listenin,singer_songwriter,electric,adult_contemporary,punk,metal,others

billboard_genre <- billboard %>% 
  gather("pop","rock","RandB","soul","hiphop","rap","country","dance","alternative_indie",
         "blues","disco","funk","jazz","folk","easy_listenin","singer_songwriter",
         "electric","adult_contemporary","punk","metal","others", key = "genre_gather", value = "count")

genre <- billboard_genre %>%
  filter(count == 1) %>% 
  unnest_tokens(word, lyrics) %>% 
  select(genre_gather, song_name, word)

tmp <- genre %>%
  count(genre_gather,word) %>%
  bind_tf_idf(word,genre_gather,n) %>%
  group_by(genre_gather) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:20)

genres <- c("rock","RandB","soul","hiphop","rap","country","dance")

plDF <- tmp %>% 
  filter(genre_gather %in% genres) %>%
  ungroup() %>% 
  mutate(xOrder=n():1)

TFIDF_genre <- plDF %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=genre_gather)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ genre_gather,scales='free',nrow = 1) +
  scale_x_continuous(breaks = plDF$xOrder,
                     labels = plDF$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words for 7 genres')+
  theme_bw()

# freq by singer
## Taylor Swift
Taylor_Swift <-billboard %>%
  filter(singer == "Taylor Swift") %>% 
  unnest_tokens(word, lyrics) %>%
  count(word,sort=TRUE) %>% 
  anti_join(stop_words,by ="word") 

Taylor_Swift %>%
  slice(1:25) %>% 
  ggplot(aes(x=fct_reorder(word,n),y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_y_continuous(labels=comma) +
  scale_fill_gradient(low = "white", high = "black")

## the beatles
beatles <-billboard %>%
  filter(singer == "The Beatles") %>% 
  unnest_tokens(word, lyrics) %>%
  count(word,sort=TRUE) %>% 
  anti_join(stop_words,by ="word") 

beatles %>%
  slice(1:25) %>% 
  ggplot(aes(x=fct_reorder(word,n),y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_y_continuous(labels=comma) +
  scale_fill_gradient(low = "white", high = "black")

## TFIDF by singer
billboard_singer <- billboard %>% 
  group_by(singer) %>% 
  summarise(num = n()) %>% 
  arrange(desc(num)) %>% 
  slice(1:10)

singer <- billboard %>%
  unnest_tokens(word, lyrics) %>% 
  select(singer, song_name, word)

tmp <- singer %>%
  count(singer,word) %>%
  bind_tf_idf(word,singer,n) %>%
  group_by(singer) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:20)

plDF <- tmp %>% 
  filter(singer %in% billboard_singer$singer) %>%
  ungroup() %>% 
  mutate(xOrder=n():1)

TFIDF_singer <- plDF %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=singer)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ singer,scales='free',nrow = 1) +
  scale_x_continuous(breaks = plDF$xOrder,
                     labels = plDF$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words for 10 singers')+
  theme_bw()

#---------------------------------- PART 2: sentiment analysis ---------------------------------------

## read data
billboard_wrk <- readRDS("data/billboard_join_final.rds") %>% 
  mutate(decade = 
           ifelse(year %in% 1970:1979, "1970s", 
                  ifelse(year %in% 1980:1989, "1980s", 
                         ifelse(year %in% 1990:1999, "1990s", 
                                ifelse(year %in% 2000:2009, "2000s", 
                                       ifelse(year %in% 2010:2017, "2010s","1960s"))))))

## creat lytics text
lyrics_text <- data.frame(docID = billboard_wrk$lyricsID,
                          song_name = billboard_wrk$song_name,
                          text = as.character(billboard_wrk$lyrics),
                          stringsAsFactors = F)

textDFt <- lyrics_text %>%
  unnest_tokens(word, text)

sentText_bing <- textDFt %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(docID, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) # net positive

sentText_afinn <- textDFt %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(docID) %>%
  summarize(sentiment = sum(score))

## --
lyricsTidy <- billboard_wrk %>% 
  mutate(lyrics = as.character(lyrics)) %>% 
  select(lyricsID, lyrics) %>% 
  unnest_tokens(word, lyrics)

lyricsLength <- lyricsTidy %>%
  count(lyricsID) %>%
  rename(lyricsLength = n)

sentLyrics <- lyricsTidy %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(lyricsID) %>%
  summarize(sentiment = sum(score)) %>%
  left_join(lyricsLength, by = 'lyricsID') %>%
  mutate(aveSentiment = sentiment/lyricsLength)

## (a) sentiment by rank:
## NOTE: no interesting pattern found 

sentByRank <- sentLyrics %>%
  left_join(select(billboard_wrk, lyricsID, rank), by = 'lyricsID') %>% 
  mutate(rank = as.factor(rank))

nlyrics <- nrow(billboard_wrk)

sentByRank %>%
  group_by(rank) %>%
  summarize(meanSent = mean(aveSentiment)) %>%
  ggplot(aes(x = rank,y = meanSent, color = rank)) + geom_point(size = 5, show.legend = F) + 
  geom_hline(aes(yintercept = 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Average Sentiment by Rank',
       subtitle = paste0(nlyrics,' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'),
       x = 'Rank',
       y = 'Average Sentiment')

#class(sentByRank$rank)

## Distribution:

## top 20
sentByRank %>%
  filter(as.numeric(rank) <= 20) %>% 
  ggplot(aes(x = aveSentiment, y = rank, group = rank, fill = rank)) +
  geom_density_ridges(scale = 2.0, size = 0.25, alpha = 0.4, show.legend = F) +
  scale_x_continuous(limits = c(-.2, 0.8), expand = c(0.01, 0)) +
  theme_bw() + 
  geom_vline(aes(xintercept=0)) + 
  labs(x = 'Lyrics Sentiment',
       y = 'Rank',
       title = 'Distribution of Lyrics Sentiment by Rank',
       subtitle = paste0(nlyrics, ' Lyrics, Billboard Yearly Hit 100 (Ranked Top 20), 1968 - 2017'))

## bottom 20
sentByRank %>%
  filter(as.numeric(rank) > 80) %>% 
  ggplot(aes(x = aveSentiment, y = rank, group = rank, fill = rank)) +
  geom_density_ridges(scale = 2.0, size = 0.25, alpha = 0.4, show.legend = F) +
  scale_x_continuous(limits = c(-.2, 0.8), expand = c(0.01, 0)) +
  theme_bw() + 
  geom_vline(aes(xintercept=0)) + 
  labs(x = 'Lyrics Sentiment',
       y = 'Rank',
       title = 'Distribution of Lyrics Sentiment by Rank',
       subtitle = paste0(nlyrics, ' Lyrics, Billboard Yearly Hit 100 (Ranked Bottom 20), 1968 - 2017'))

## (b) sentiment by genre:
from <- which(colnames(billboard_wrk) == "pop")
to <- which(colnames(billboard_wrk) == "metal")
id_col <- which(colnames(billboard_wrk) == "lyricsID")
n_rank <- which(colnames(billboard_wrk) == "rank")
n_decade <- which(colnames(billboard_wrk) == "decade")
n_year <- which(colnames(billboard_wrk) == "year")

sentByGenre_plot <- data.frame()

for (i in from:to){
  
  temp <- sentLyrics %>%
    left_join(billboard_wrk[, c(id_col, i)], by = 'lyricsID') %>% 
    group_by(get(colnames(billboard_wrk)[i])) %>% 
    summarise(meanSent = mean(aveSentiment))
  
  colnames(temp) <- c("col1", "meanSent")
  
  sentByGenre_plot <- rbind(sentByGenre_plot, data.frame(genre = colnames(billboard_wrk)[i],
                                               meanSent = temp$meanSent[temp$col1 == 1]))
  
}

sentByGenre_plot %>%
  ggplot(aes(x = reorder(genre, -meanSent),y = meanSent, fill = genre)) + 
  geom_bar(stat = "identity") + geom_hline(aes(yintercept = 0)) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Average Sentiment by Genre',
       subtitle = paste0(nlyrics,' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'),
       x = 'Genre',
       y = 'Average Sentiment')

Genre_levels_dat <- sentByGenre_plot %>% 
  arrange(desc(meanSent))

Genre_levels <- Genre_levels_dat$genre

## Distribution:

## "clean" Version, GenreTidy (***)
GenreTidy <- sentLyrics %>% 
  left_join(billboard_wrk[, c(id_col, n_rank, n_decade, n_year, from:to)], by = 'lyricsID') %>% 
  gather(genre_tidy, flag, pop:metal) %>% 
  filter(flag == 1) %>% 
  select(-flag)

GenreTidy %>%
  ggplot(aes(x = aveSentiment, y = factor(GenreTidy$genre_tidy, levels = Genre_levels), 
             group = genre_tidy, fill = genre_tidy)) +
  geom_density_ridges(scale = 2.0, size = 0.25, alpha = 0.4, show.legend = F) +
  scale_x_continuous(limits = c(-.2, 0.8), expand = c(0.01, 0)) +
  theme_bw() + 
  geom_vline(aes(xintercept=0)) + 
  labs(x = 'Lyrics Sentiment',
       y = 'Genre',
       title = 'Distribution of Lyrics Sentiment by Genre',
       subtitle = paste0(nlyrics, ' Lyrics, Billboard Yearly Hit 100 (Ranked Bottom 20), 1968 - 2017'))
## NOTE: A small (but obvious) shift can be seen from the graph.

## (c) overtime sentiment -- updated on 5/9/2018

### c.1 by Year
sentByYear <- sentLyrics %>%
  left_join(select(billboard_wrk, lyricsID, year), by = 'lyricsID') %>% 
  mutate(year = as.factor(year))

nlyrics <- nrow(billboard_wrk)

sentByYear %>%
  group_by(year) %>%
  summarize(meanSent = mean(aveSentiment)) %>%
  ggplot(aes(x = year,y = meanSent, color = year)) + geom_point(size = 5, show.legend = F) + 
  geom_hline(aes(yintercept = 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Average Sentiment by Year',
       subtitle = paste0(nlyrics,' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'),
       x = 'Year',
       y = 'Average Sentiment')
## NOTE: => average sentiments decreased overtime (an outlier in 1976?)

## Distribution:
sentByYear %>%
  ggplot(aes(x = aveSentiment, y = year, group = year, fill = year)) +
  geom_density_ridges(scale = 2.0, size = 0.25, alpha = 0.4, show.legend = F) +
  scale_x_continuous(limits = c(-.2, 0.8), expand = c(0.01, 0)) +
  theme_bw() + 
  geom_vline(aes(xintercept=0)) + 
  labs(x = 'Lyrics Sentiment',
       y = 'Year',
       title = 'Distribution of Lyrics Sentiment by Year',
       subtitle = paste0(nlyrics, ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))

### c.2 by Decade
sentByDecade <- sentLyrics %>%
  left_join(select(billboard_wrk, lyricsID, decade), by = 'lyricsID') %>% 
  mutate(decade = as.factor(decade))

nlyrics <- nrow(billboard_wrk)

sentByDecade %>%
  group_by(decade) %>%
  summarize(meanSent = mean(aveSentiment)) %>%
  ggplot(aes(x = decade,y = meanSent, color = decade)) + geom_point(size = 5, show.legend = F) + 
  geom_hline(aes(yintercept = 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Average Sentiment by Decade',
       subtitle = paste0(nlyrics,' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'),
       x = 'Decade',
       y = 'Average Sentiment')
## NOTE: => obvious trend

## Distribution:
sentByDecade %>%
  ggplot(aes(x = aveSentiment, y = decade, group = decade, fill = decade)) +
  geom_density_ridges(scale = 2.0, size = 0.25, alpha = 0.4, show.legend = F) +
  scale_x_continuous(limits = c(-.2, 0.8), expand = c(0.01, 0)) +
  theme_bw() + 
  geom_vline(aes(xintercept=0)) + 
  labs(x = 'Lyrics Sentiment',
       y = 'Decade',
       title = 'Distribution of Lyrics Sentiment by Decade',
       subtitle = paste0(nlyrics, ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))

## genre occurences, %
GenreTidy %>% 
  group_by(decade, genre_tidy) %>% 
  summarise(n.genre_occ = n()) %>% 
  ggplot(aes(x = genre_tidy, y = n.genre_occ, fill = genre_tidy)) + geom_bar(stat = "identity") +
  facet_wrap(~decade) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  labs(x = 'Genre',
       y = 'Number of Occurences',
       title = 'Number of Occurences by Genre across Decades',
       subtitle = paste0(nlyrics, ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))

GenreTidy %>% 
  group_by(decade, genre_tidy) %>% 
  summarise(n.genre_occ = n()) %>% 
  group_by(decade) %>% 
  mutate(perc = n.genre_occ/sum(n.genre_occ)) %>% 
  ggplot(aes(x = genre_tidy, y = perc, fill = genre_tidy)) + geom_bar(stat = "identity") +
  facet_wrap(~decade) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
  labs(x = 'Genre',
       y = 'Percentage of Occurences',
       title = 'Percentage of Occurences by Genre across Decades',
       subtitle = paste0(nlyrics, ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))
# Findings: pop is an evergreen genre; less rock after 1990s; music genre becomes less diverse after 2000s --
# (no punk / jazz / electronic / easylistening) 

## look into what happened in 1970s (spike)
sentByDecade %>% 
  filter(decade == "1970s", aveSentiment > 0.35) %>% 
  ggplot(aes(x = reorder(lyricsID, -aveSentiment), y = aveSentiment)) + geom_bar(stat = "identity") +
  labs(x = 'lyricsID',
       y = 'Average Sentiment',
       title = 'Songs with High Average Sentiment (>.35) in 1970s')

top_aveSent_1970s <- select(billboard_wrk, c("lyricsID", "lyrics", "song_name", "singer", "rank", "year")) %>% 
  left_join(sentByDecade, by = "lyricsID") %>% 
  filter(decade == "1970s", aveSentiment > 0.35) %>% 
  arrange(desc(aveSentiment))
## 1960

sentByDecade %>% 
  filter(decade == "2010s", aveSentiment < -0.35) %>% 
  ggplot(aes(x = reorder(lyricsID, -aveSentiment), y = aveSentiment)) + geom_bar(stat = "identity") +
  labs(x = 'lyricsID',
       y = 'Average Sentiment',
       title = 'Songs with Low Average Sentiment (< -0.35) in 2010s')

top_aveSent_2010s <- select(billboard_wrk, c("lyricsID", "lyrics", "song_name", "singer", "rank", "year")) %>% 
  left_join(sentByDecade, by = "lyricsID") %>% 
  filter(decade == "2010s", aveSentiment < -0.35) %>% 
  arrange(desc(aveSentiment))
## what can lead to the change in sentiment in music?

## (d) wider range of sentiments

## join lyrics (***)
GenreLyricsTidy <- GenreTidy %>% 
  left_join(select(billboard_wrk, lyricsID, lyrics), by = "lyricsID")

## prepare data set for future sentiment analysis ("bing", "afinn", "nrc")
sent_prep <- GenreLyricsTidy %>% 
  mutate(lyrics = as.character(lyrics)) %>% 
  rename(genre = genre_tidy) %>% 
  unnest_tokens(word, lyrics) %>% 
  count(genre, word) ## n: word freq

## nrc
term.genre <- sent_prep %>%
  group_by(genre) %>%
  summarize(n.genre = sum(n)) ## n.genre: word freq for a particular genre 

sent_prep %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(genre, sentiment) %>%
  summarize(total = sum(n)) %>% ## total: word freq for a genre-sentiment pair
  inner_join(term.genre, by = "genre") %>%
  mutate(relative.sentiment = total/n.genre) %>% 
  ggplot(aes(x = sentiment, y = relative.sentiment, fill = sentiment)) + geom_bar(stat = 'identity') + 
  facet_wrap(~ genre, ncol = 4) + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) + 
  labs(title = "Relative Sentiment by Genre with a Wider Range of Sentiments") +
  labs(x = "Sentiment", y = "Relative Sentiment (Percentage)")

#-------------------------------- PART 3: Part-of-speech tagging  ------------------------------------

## Download the English corpus(Only run this once)
# dl <- udpipe_download_model(language = "english")

## @knitr setup
udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')

# Read data
billboard <- readRDS("data/billboard_join_final.rds") %>% 
  mutate(decade = 
           ifelse(year %in% 1970:1979, "1970s", 
                  ifelse(year %in% 1980:1989, "1980s", 
                         ifelse(year %in% 1990:1999, "1990s", 
                                ifelse(year %in% 2000:2009, "2000s", 
                                       ifelse(year %in% 2010:2017, "2010s","1960s"))))))

## Creat lytics text
lyrics_text <- data.frame(docID = c(1:nrow(billboard)),
                          song_name = billboard$song_name,
                          text = as.character(billboard$lyrics),
                          stringsAsFactors = F)

# ## Tag the speech(This takes a lot time, so don't run it again if you had)
# lyrics_pos <- udpipe_annotate(udmodel_english, x = lyrics_text$text) %>% 
#   as.data.frame(.) %>% 
# 
#   saveRDS(lyrics_pos,"data/lyrics_pos.rds")

## Join with genres,decades, ranks, etc...
lyrics_pos <- readRDS("data/lyrics_pos.rds")
lyrics_pos_wrk <- lyrics_pos %>%
  mutate(docID = as.integer(str_remove(doc_id,"doc")) ) %>% 
  filter(upos != "PUNCT") %>% 
  left_join(select(lyrics_text,-text),by ='docID') %>% 
  left_join(select(mutate(billboard,docID = c(1:nrow(billboard))),
                   -c(genre_raw,genre,lyrics)),
            by = 'docID')


nlyrics <- nrow(lyrics_text)

## @knitr TopNouns

# noun for all
topNoun_all <- lyrics_pos_wrk %>%
  filter(upos=="NOUN") %>%
  count(lemma,sort = T) %>%
  slice(1:20) %>%
  ggplot(aes(x=fct_reorder(lemma,n),y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip()+
  labs(title='Top Nouns Used in billboard-hot-100 lyrics',
       subtitle = paste0(nlyrics,' lyrics from billboard-hot-100'),
       caption = 'data sources: Wikipedia, azlyrics.com, and metrolyrics.com /n 1968 ~ 2017',
       x = 'Noun',
       y = 'Count')

# noun by decade
tmp <- lyrics_pos_wrk %>%
  filter(upos=="NOUN") %>% 
  count(decade,lemma) %>%
  group_by(decade) %>%
  arrange(desc(n)) %>%
  slice(1:30) %>%
  ungroup() %>%
  mutate(x = n():1)  # for plotting

topNoun_bydecade <- tmp %>%
  ggplot(aes(x=x,y=n,fill=decade)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~decade,scales='free',nrow = 1) +
  scale_x_continuous(breaks = tmp$x,
                     labels = tmp$lemma,
                     expand = c(0,0)) + 
  labs(title='Top Nouns by Decades',
       subtitle = paste0(nlyrics,' lyrics from billboard-hot-100'),
       caption = 'data sources: Wikipedia, azlyrics.com, and metrolyrics.com /n 1968 ~ 2017',
       x = 'Noun',
       y = 'Count')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# noun by genre

genres_list <- c("pop","rap","country","blues","dance") 
noun_genre_plots <- list() 
for (i in 1:length(genres_list) ) {

  tmp <- lyrics_pos_wrk %>%
    filter(upos == "NOUN") %>%
    filter_(paste(genres_list[i],"== 1" ) ) %>%
    count(lemma) %>%
    arrange(desc(n)) %>%
    slice(1:30) %>%
    mutate(x = n():1) 
  
  noun_genre_plots[[i]] <- tmp %>% 
    ggplot(aes(x=x,y=n)) + 
    geom_bar(stat='identity',fill = scales::hue_pal()(length(genres_list))[i],show.legend = F) + 
    coord_flip() + 
    scale_x_continuous(breaks = tmp$x,
                       labels = tmp$lemma,
                       expand = c(0,0)) + 
    labs(title=genres_list[i],
         x = 'Noun',
         y = 'Count')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

topNoun_bygenre <- gridExtra::grid.arrange(grobs = noun_genre_plots, ncol = length(genres_list))

## @knitr TopADJ

## ADJ by decades
tmp <- lyrics_pos_wrk %>%
  filter(upos=="ADJ") %>%
  count(decade,lemma) %>%
  group_by(decade) %>%
  arrange(desc(n)) %>%
  slice(1:30) %>%
  ungroup() %>%
  mutate(x = n():1)   # for plotting

topAdj_bydecade <- tmp %>%   
  ggplot(aes(x=x,y=n,fill=decade)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~decade,scales='free',nrow = 1) +
  scale_x_continuous(breaks = tmp$x,
                     labels = tmp$lemma,
                     expand = c(0,0)) + 
  labs(title='Top Adjectives by Decades',
       subtitle = paste0(nlyrics,' lyrics from billboard-hot-100'),
       caption = 'data sources: Wikipedia, azlyrics.com, and metrolyrics.com /n 1968 ~ 2017',
       x = 'Adjectives',
       y = 'Count')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ADJ by genre

ADJ_genre_plots <- list() 
for (i in 1:length(genres_list) ) {
  
  tmp <- lyrics_pos_wrk %>%
    filter(upos == "ADJ") %>%
    filter_(paste(genres_list[i],"== 1" ) ) %>%
    count(lemma) %>%
    arrange(desc(n)) %>%
    slice(1:30) %>%
    mutate(x = n():1) 
  
  ADJ_genre_plots[[i]] <- tmp %>% 
    ggplot(aes(x=x,y=n)) + 
    geom_bar(stat='identity',fill = scales::hue_pal()(length(genres_list))[i],show.legend = F) + 
    coord_flip() + 
    scale_x_continuous(breaks = tmp$x,
                       labels = tmp$lemma,
                       expand = c(0,0)) + 
    labs(title=genres_list[i],
         x = 'Noun',
         y = 'Count')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

topADJ_bygenre <- gridExtra::grid.arrange(grobs = ADJ_genre_plots, ncol = length(genres_list))

## @knitr Cooccurrences_for_all
stats <- cooccurrence(x = lyrics_pos_wrk$lemma, 
                      relevant = lyrics_pos_wrk$upos %in% c("NOUN", "ADJ"))

wordnetwork <- head(stats,150)

wordnetwork <- graph_from_data_frame(wordnetwork)

Cooc_All <- ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink",show.legend = F) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 3) +
  labs(title = "Cooccurrences of Words Next to Each Other", 
       subtitle = "Nouns & Adjective",
       caption = 'data sources: Wikipedia, azlyrics.com, and metrolyrics.com /n 1968 ~ 2017',
       x = '',y='')+
  theme_bw()


## @knitr DependencyParsing

tmpLeft <- lyrics_pos_wrk %>%
  select(doc_id,paragraph_id,sentence_id,lemma,head_token_id,dep_rel,upos)

tmpRight <- lyrics_pos_wrk %>%
  select(doc_id,paragraph_id,sentence_id,token_id,lemma,upos)


tmp2 <- tmpLeft %>%
  left_join(tmpRight,
            by=c('doc_id'='doc_id',
                 'paragraph_id'='paragraph_id',
                 'sentence_id'='sentence_id',
                 'head_token_id'='token_id')
  ) %>%
  filter(dep_rel %in% "nsubj" & upos.x %in% c("NOUN") & upos.y %in% c("ADJ")) %>%
  mutate(term = paste(lemma.y,lemma.x,sep=" ")) %>%
  count(term,sort = T)


dep_All <- tmp2 %>%
  head(40) %>%
  ggplot(aes(x=fct_reorder(term,n),y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  labs(title='Top Keywords Extracted Using Dependency Parsing',
       subtitle = paste0(nlyrics,' lyrics from billboard-hot-100'),
       caption = 'data sources: Wikipedia, azlyrics.com, and metrolyrics.com /n 1968 ~ 2017',
       x = 'Keyword',
       y = 'Frequency')


## @knitr RakeKeyWords
statsAll <- keywords_rake(x = lyrics_pos_wrk, 
                          term = "token", 
                          group = c("doc_id", "paragraph_id", "sentence_id"),
                          relevant = lyrics_pos_wrk$upos %in% c("NOUN", "ADJ"),
                          ngram_max = 4) %>%
  filter(freq > 50) %>%
  arrange(desc(freq))


tmp <- statsAll %>%
  filter(ngram %in% c(1,2)) %>%
  group_by(ngram) %>%
  arrange(desc(freq)) %>%
  slice(1:20) %>%
  ungroup() %>%
  mutate(x = n():1)

Rake_All <- tmp %>%
  mutate(ngram=factor(paste0('ngram=',ngram))) %>%
  ggplot(aes(x=x,y=freq,fill=ngram)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~ngram,scales='free',nrow = 1) +
  scale_x_continuous(breaks = tmp$x,
                     labels = tmp$keyword,
                     expand = c(0,0)) + 
  labs(title='Top Keywords',
       subtitle = 'Extracted using RAKE',
       caption = 'data sources: Wikipedia, azlyrics.com, and metrolyrics.com /n 1968 ~ 2017',
       x = 'Keyword',
       y = 'Count')


#-------------------------------- PART 4: TF_IDF & Topic modeling ------------------------------------
options( scipen = 999) # disable scientific notation in plot
billboard_wrk$decade <- as.factor(billboard_wrk$decade) # convert to factor

# We now move beyond the use of specific words used in isolation. Will use LDA to calculate the frequency 
# with which words appear together combining these measures to produce groups of words that are likely to 
# indicate a topic contained in the text

## By Decade
# Create a Corpus, remove stop, undesirable and short words
lyrics_corpus <- Corpus(VectorSource(as.vector(billboard_wrk$lyrics))) %>% 
                  tm_map(content_transformer(removePunctuation)) %>% 
                  tm_map(content_transformer(tolower)) %>% 
                  tm_map(content_transformer(stripWhitespace)) %>% 
                  tm_map(content_transformer(stemDocument), language = "english") %>% 
                  tm_map(removeWords, stopwords("english"))

# Create document term matrix
lyrics_DTM <- DocumentTermMatrix(lyrics_corpus, control = list(wordLengths = c(2, Inf)))

# Setting Control Parameters
control_LDA_Gibbs <- list(alpha = 50/20, estimate.beta = T, 
                          verbose = 0, prefix = tempfile(), 
                          save = 0, 
                          keep = 50, 
                          seed = 980,  # for reproducibility
                          nstart = 1, best = T,
                          delta = 0.1,
                          iter = 2000, 
                          burnin = 100, 
                          thin = 2000) 

# First Topic Model
my_first_topic_model <- LDA(lyrics_DTM, 20, method = "Gibbs", control = control_LDA_Gibbs)

## extract term and topic weights
lda_beta <- tidy(my_first_topic_model, matrix = "beta")
#lda_gamma <- tidy(my_first_topic_model, matrix = "gamma")

top_terms <- lda_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(beta) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  arrange(topic, -beta) 

# plot top words by topics for all decades and genres
top_terms$topic <- as.factor(top_terms$topic)

top_terms %>%
  ggplot(aes(x=beta, y=order, col = topic)) + 
  geom_point(size=1.5, alpha=0.5) + 
  geom_text(aes(label=term, x=0.0), size=3) +  
  facet_wrap(~topic, scales='free')+
  theme_bw() + 
  xlab('Weight in Topic') + ylab('Term') + xlim(-0.005, NA)+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position="none") +
  labs(title="Top Words by 20 Topic LDA Models in all Decades", 
       subtitle= paste0(nlyrics, ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))

# Decade: 1970s (with highest avg sentiment scores)
lyrics_corpus_1970 <- Corpus(VectorSource(as.vector(filter(billboard_wrk, decade == '1970s')$lyrics))) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(stripWhitespace)) %>% 
  tm_map(content_transformer(stemDocument), language = "english") %>% 
  tm_map(removeWords, stopwords("english"))

# identify no. of docs 
ndocs <- length(lyrics_corpus_1970)

# ignore overly sparse terms (appearing in less than 1% of the documents)
minDocFreq <- ndocs * 0.01

# ignore overly common terms (appearing in more than 80% of the documents)
maxDocFreq <- ndocs * 0.9

# Create document term matrix by ignoring sparse & common terms
lyrics_DTM_1970 <- DocumentTermMatrix(lyrics_corpus_1970,
                                      control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))

inspect(lyrics_DTM_1970) # 842 terms

#Find the sum of words in each Document
rowTotals <- apply(lyrics_DTM_1970, 1, sum) 

#remove all docs without words
lyrics_DTM_1970 <- lyrics_DTM_1970[rowTotals > 0, ] 

# Topic Model
topic_model_1970 <- LDA(lyrics_DTM_1970, 10, method = "Gibbs", control = control_LDA_Gibbs)

## extract term and topic weights
lda_beta_1970 <- tidy(topic_model_1970, matrix = "beta")
#lda_gamma_1970 <- tidy(topic_model_1970, matrix = "gamma")

top_terms_1970 <- lda_beta_1970 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(beta) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  arrange(topic, -beta) 

top_terms_1970$topic <- as.factor(top_terms_1970$topic)

# plot top words by topics for 1970s and genres: more positive words (sun / shine / light / man)
top_terms_1970 %>%
  ggplot(aes(x = beta, y = order, col = topic)) + 
  geom_point(size = 2.5) + 
  geom_text(aes(label = term, x = 0.0), size = 3.5, show.legend = FALSE) +  
  facet_wrap(~topic, scales='free')+
  theme_bw() + 
  xlab('Weight in Topic') + ylab('Term') + xlim(-0.005, NA)+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none") +
  labs(title = "Top Words by 20 Topic LDA Models in 1970s", 
       subtitle = paste0(nrow(filter(billboard_wrk, decade == '1970s')), ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))

# Decade: 2010s (with lowest avg sentiment scores)
lyrics_corpus_2010 <- Corpus(VectorSource(as.vector(filter(billboard_wrk, decade == '2010s')$lyrics))) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(stripWhitespace)) %>% 
  tm_map(content_transformer(stemDocument), language = "english") %>% 
  tm_map(removeWords, stopwords("english"))

# identify no. of docs 
ndocs <- length(lyrics_corpus_2010)

# ignore overly sparse terms (appearing in less than 1% of the documents)
minDocFreq <- ndocs * 0.01

# ignore overly common terms (appearing in more than 80% of the documents)
maxDocFreq <- ndocs * 0.9

# Create document term matrix
lyrics_DTM_2010 <- DocumentTermMatrix(lyrics_corpus_2010, 
                                      control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
inspect(lyrics_DTM_2010)

# Topic Model
topic_model_2010 <- LDA(lyrics_DTM_2010, 10, method = "Gibbs", control = control_LDA_Gibbs)

## extract term and topic weights
lda_beta_2010 <- tidy(topic_model_2010, matrix = "beta")

top_terms_2010 <- lda_beta_2010 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(beta) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  arrange(topic, -beta) 

top_terms_2010$topic <- as.factor(top_terms_2010$topic)

# plot top words by topics for 2010s decades and genres: more profane words coming: shit / ass / f*ck in topic 10
top_terms_2010 %>%
  ggplot(aes(x=beta, y=order, col = topic)) + 
  geom_point(size=2.5) + 
  geom_text(aes(label=term, x=0.0), size=3.5, show.legend=FALSE) +  
  facet_wrap(~topic, scales='free')+
  theme_bw() + 
  xlab('Weight in Topic') + ylab('Term') + xlim(-0.005, NA)+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = "none") +
  labs(title = "Top Words by 20 Topic LDA Models in 2010s", 
       subtitle = paste0(nrow(filter(billboard_wrk, decade == '2010s')), ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))

## By genre

# Disco: top avg sentiment score
lyrics_corpus_disco <- Corpus(VectorSource(as.vector(filter(billboard_wrk, disco == 1)$lyrics))) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(stripWhitespace)) %>% 
  tm_map(content_transformer(stemDocument), language = "english") %>% 
  tm_map(removeWords, stopwords("english"))

# identify no. of docs 
ndocs <- length(lyrics_corpus_disco)

# ignore overly sparse terms (appearing in less than 1% of the documents)
minDocFreq <- ndocs * 0.01

# ignore overly common terms (appearing in more than 80% of the documents)
maxDocFreq <- ndocs * 0.9

# Create document term matrix
lyrics_DTM_disco <- DocumentTermMatrix(lyrics_corpus_disco, 
                                       control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
inspect(lyrics_DTM_disco)

# Topic Model
topic_model_disco <- LDA(lyrics_DTM_disco, 10, method = "Gibbs", control = control_LDA_Gibbs)

## extract term and topic weights
lda_beta_disco <- tidy(topic_model_disco, matrix = "beta")

top_terms_disco <- lda_beta_disco %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(beta) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  arrange(topic, -beta) 

top_terms_disco$topic <- as.factor(top_terms_disco$topic)

# plot top words by topics for disco for all decades: no negative words appeared in any topic
top_terms_disco %>%
  ggplot(aes(x=beta, y=order, col = topic)) + 
  geom_point(size=2.5) + 
  geom_text(aes(label=term, x=0.0), size=3.5, show.legend=FALSE) +  
  facet_wrap(~topic, scales='free')+
  theme_bw() + 
  xlab('Weight in Topic') + ylab('Term') + xlim(-0.005, NA)+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = "none") +
  labs(title = "Top Words by 20 Topic LDA Models in Disco Genre", 
       subtitle = paste0(nrow(filter(billboard_wrk, disco == 1)), ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))

# Rap: lowest avg sentiment score
lyrics_corpus_rap <- Corpus(VectorSource(as.vector(filter(billboard_wrk, rap == 1)$lyrics))) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(stripWhitespace)) %>% 
  tm_map(content_transformer(stemDocument), language = "english") %>% 
  tm_map(removeWords, stopwords("english"))

# identify no. of docs 
ndocs <- length(lyrics_corpus_rap)

# ignore overly sparse terms (appearing in less than 1% of the documents)
minDocFreq <- ndocs * 0.01

# ignore overly common terms (appearing in more than 80% of the documents)
maxDocFreq <- ndocs * 0.9

# Create document term matrix
lyrics_DTM_rap <- DocumentTermMatrix(lyrics_corpus_rap, 
                                     control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
inspect(lyrics_DTM_rap) # 2,117 terms

# Topic Model
topic_model_rap <- LDA(lyrics_DTM_rap, 10, method = "Gibbs", control = control_LDA_Gibbs)

## extract term and topic weights
lda_beta_rap <- tidy(topic_model_rap, matrix = "beta")

top_terms_rap <- lda_beta_rap %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(beta) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  arrange(topic, -beta) 

top_terms_rap$topic <- as.factor(top_terms_rap$topic)

# plot top words by topics for rap for all decades: lyrics look pessimistic and violent (topic 6/13/19)
top_terms_rap %>%
  ggplot(aes(x = beta, y = order, col = topic)) + 
  geom_point(size = 2.5) + 
  geom_text(aes(label = term, x = 0.0), size = 3.5, show.legend = FALSE) +  
  facet_wrap(~topic, scales = 'free')+
  theme_bw() + 
  xlab('Weight in Topic') + ylab('Term') + xlim(-0.005, NA)+
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position = "none") +
  labs(title = "Top Words by 20 Topic LDA Models in Rap Genre", 
       subtitle = paste0(nrow(filter(billboard_wrk, rap == 1)), ' Lyrics, Billboard Yearly Hit 100, 1968 - 2017'))

# Critial thinking: the results of topic analysis have to interpreted subjectively.

## --------------------------------------- Part 5: Lyrics repetition --------------------------------------
richness_wrk <- billboard %>%
  unnest_tokens(word, lyrics) %>% 
  anti_join(stop_words,by ="word") %>% 
  group_by(lyricsID) %>% 
  summarise(richness = n_distinct(word) / n()) %>%  # the richness is defined as the % of the unique words
  ungroup() %>% 
  arrange(desc(richness)) %>% 
  inner_join(billboard, by = "lyricsID") 


rich_bydecade <- richness_wrk %>% 
  group_by(decade) %>% 
  summarize(avg_richness = mean(richness,na.rm = T) ) %>% 
  ggplot(aes(x=decade, y=avg_richness)) +
  geom_bar(aes(fill = decade),stat = "identity",show.legend = F) +
  labs(title="richness of lyrics across decades",
       x = 'Deacdes',
       y = 'Richness')

genres_list <- c("pop","rap","country","blues","dance") 
rich_genre_plots <- list() 
for (i in 1:length(genres_list) ) {
  
  tmp <- richness_wrk %>% 
    filter_(paste(genres_list[i],"== 1" ) ) %>% 
    group_by(decade) %>% 
    summarize(avg_richness = mean(richness,na.rm = T) )
  
  
  
  rich_genre_plots[[i]] <- tmp %>% 
    ggplot(aes(x=reorder(decade,avg_richness),y=avg_richness)) + 
    geom_bar(stat='identity',fill = scales::hue_pal()(length(genres_list))[i],show.legend = F) +  
    coord_flip() + 
    # scale_x_continuous(breaks = tmp$x,
    #                    labels = tmp$lemma,
    #                    expand = c(0,0)) + 
    labs(title=genres_list[i],
         x = 'Deacdes',
         y = 'Richness') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

rich_bygenre <- gridExtra::grid.arrange(grobs = rich_genre_plots, ncol = length(genres_list))
