library(textdata)
library(tidytext)
library(tidyverse)
library(dplyr)
library(janeaustenr)
library(wordcloud)
library(textdata)
library(gutenbergr)
library(reshape2)
library(textreadr)
library(scales)
library(plotly)
library(igraph)
library(ggraph)
library(tm)
library(RColorBrewer)

# Reading block_pro
pro <- read_document(file="/Users/saipruthvi/Desktop/assignment/block_pro/Blockchain_Technological_Revolution_in_B.txt")
block_pro <- c(pro)
block_pro <- data_frame(line=1, text=block_pro) 

# Tokenizing block_pro
pro_token <- block_pro %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 
pro_token 

# Reading block_con
con <- read_document(file="/Users/saipruthvi/Desktop/assignment/block_con/The_Revolution_Will_not_be_Decentralised.txt")
block_con <- c(con)
block_con <- data_frame(line=1, text=block_con)

# Tokenizing block_con
con_token <- block_con %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
con_token


# Combining into single data frame
block <- bind_rows(mutate(pro_token, author="pro"),
                   mutate(con_token, author= "con"))%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author)
block 

# Creating IDF
block_idf <- block %>%
  bind_tf_idf(word, author, n)
block_idf

# Plotting IDF using Plotly
comp_plot <- plot_ly(data = block_idf, x=~tf_idf, y=~words, color =~author, opacity = 0.001)
comp_plot
# Bing Sentiment Analysis using Plotly
# block_pro
pro_senaly <- block_pro %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
pro_bing <- pro_senaly %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
pro_bing <- ggplotly(pro_bing)
pro_bing

# block_con
con_senaly <- block_con %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
con_bing <- con_senaly %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
con_bing <- ggplotly(con_bing)
con_bing 

# NRC Sentiment Analysis - Word Cloud
# block_pro
pro_nrc <- block_pro %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
pro_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   title.colors=c("red","blue"),
                   max.words=100, fixed.asp=TRUE,
                   scale=c(0.6,0.6), title.size=1, rot.per=0.25)

# block_con
con_nrc <- block_con %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
con_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   title.colors=c("red","blue"),
                   max.words=100, fixed.asp=TRUE,
                   scale=c(0.6,0.6), title.size=1, rot.per=0.25) 

# Creating Bigrams and plotting the networks
# block_pro
pro_bigrams <- block_pro %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)
pro_bigrams 

pro_bigraph <- pro_bigrams %>%
  filter(n>2) %>%
  graph_from_data_frame()
ggraph(pro_bigraph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) 

# block_con
con_bigrams <- block_con %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) 
con_bigraph <- con_bigrams %>%
  filter(n>2) %>%
  graph_from_data_frame()
ggraph(con_bigraph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) 

# Creating single data frame with all tokens along with frequency proportions
frequency <- bind_rows(mutate(pro_token, author="pro"),
                       mutate(con_token, author= "con")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  #select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `con`)
frequency 

# Plotting a correlogram using Plotly
pc_plot <- ggplot(frequency, aes(x=proportion, y=pro,
                           color = abs(pro- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(aes(text=paste("word: ", word)), alpha=.1, size=2.5, width=0.3, height=0.3)+
  #geom_text(aes(label=word), colour="gray20", alpha=1) +
  #scale_x_log10(labels = percent_format())+
  #scale_y_log10(labels= percent_format())+ 
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "pro", x=NULL)
pc_plot <- ggplotly(pc_plot)
pc_plot 

# Creating a Document Term Matrix (DTM)
block_dtm <- block %>%
  group_by(author) %>%
  cast_dtm(author, word, n)
block_dtm

# Creating a vector for negeation tokens
negation_tokens <- c('wall', 'student', 'job', 'american')

# Negeated Token for block_pro
pro_negated <- block_pro %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments('afinn'), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()
pro_negated

# Negeated Token for block_con
con_negated <- block_pro %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments('afinn'), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()
con_negated


# Function to plot negeted tokens
negated_plot <- function(x){
  pro_negated %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
} 

# Negeation plot using the plot fuction created above
negated_plot(x="block") 

# Sentiment Analysis using AFINN

con_afinn <- con_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value)) 
con_afinn


# bitcoin
pro_afinn <- pro_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value)) 
pro_afinn
