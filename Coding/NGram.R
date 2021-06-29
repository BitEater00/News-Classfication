library(janeaustenr)

news <- read.csv("news.csv")
news$text <- as.character(news$text)
fake_news <- news%>%filter(type == 0)
true_news <- news%>%filter(type == 1)

#Unigram - Fake_News
tokeniztion_df_fake <- fake_news %>% unnest_tokens(word, text)
tokeniztion_df_fake <- tokeniztion_df_fake %>% anti_join(stop_words)
tokeniztion_df_fake %>% count(word, sort = TRUE) %>% filter(n > 10000) %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word , fill = n)) +
  geom_col() +
  labs(y = NULL) +
  theme_minimal()


#Unigram - True_News
tokeniztion_df_true <- true_news %>% unnest_tokens(word, text)
tokeniztion_df_true <- tokeniztion_df_true %>% anti_join(stop_words)
tokeniztion_df_true %>% count(word, sort = TRUE) %>% filter(n > 10000) %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word,fill = n)) +
  geom_col() +
  labs(y = NULL)+
  theme_minimal()



#Bigram - Fake_News
df_bigrams <-  fake_news %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_separated <- df_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ") %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_separated %>%  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>% filter(n > 1000) %>% graph_from_data_frame()
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


#Bigram - True_News
df_bigrams <-  true_news %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_separated <- df_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ") %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_separated %>%  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>% filter(n > 1000) %>% graph_from_data_frame()
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) 

