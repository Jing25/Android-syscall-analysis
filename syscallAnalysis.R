rm(list = ls())
#lab
setwd("/Users/jing/Desktop/class/ECS251/Android-syscall-analysis")

library(rjson)
library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(ggplot2)
library(ggraph)

##load data.frame
load("benign_sysc.Rda")
load("malicious_sysc.Rda")

## load benign data
# filePath_b <- "../data/benign"
# temp_b <- list.files(filePath_b, pattern="*.json", full.names=TRUE)
# names_b <- str_extract(temp_b, "benign_...")
# data_temp <-  fromJSON(paste(readLines(temp_b[1]), collapse = ""))
# syscalls <- unlist(data_temp, use.names = FALSE)
# df_benign <- data.frame(text = c(syscalls), book = names_b[1])
# for(i in 2:length(temp_b)) {
#   data_temp <-  fromJSON(paste(readLines(temp_b[i]), collapse = ""))
#   syscalls <- unlist(data_temp, use.names = FALSE)
#   temp_df <- data.frame(text = syscalls, book = names_b[i])
#   df_benign <- rbind(df_benign, temp_df)
# }

##save data.frame
#save(df_benign,file="benign_sysc.Rda")

## load malicious data
# filePath_m <- "../data/malicious"
# temp_m <- list.files(filePath_m, pattern="*.json", full.names=TRUE)
# names_m <- str_extract(temp_m, "malicious_...")
# data_temp <-  fromJSON(paste(readLines(temp_m[1]), collapse = ""))
# syscalls <- unlist(data_temp, use.names = FALSE)
# df_malicious <- data.frame(text = c(syscalls), book = names_m[1])
# for(i in 2:length(temp_m)) {
#   data_temp <-  fromJSON(paste(readLines(temp_m[i]), collapse = ""))
#   syscalls <- unlist(data_temp, use.names = FALSE)
#   temp_df <- data.frame(text = syscalls, book = names_m[i])
#   df_malicious <- rbind(df_malicious, temp_df)
# }
# save(df_malicious,file="malicious_sysc.Rda")

filePath <- "data"
temp <- list.files(filePath, pattern="*.json", full.names=TRUE)
data_temp <- fromJSON(paste(readLines(temp[1]), collapse = ""))
syscalls <- unlist(data_temp, use.names = FALSE)
names <- c("messenger", "momentWeChat", "MsgWeChat", "whatsapp")
df_apps <- data.frame(text = c(syscalls), book = names[1])
for(i in 2:length(names)) {
    data_temp <-  fromJSON(paste(readLines(temp[i]), collapse = ""))
    syscalls <- unlist(data_temp, use.names = FALSE)
    temp_df <- data.frame(text = syscalls, book = names[i])
    df_apps <- rbind(df_apps, temp_df)
}

# bigram_benign <- df_benign[df_benign$book == names_b[10],] %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigram_benign <- df_benign %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep <- bigram_benign %>% separate(bigram, c("word1", "word2", "word3"), sep = " ")

bigram_malicious <- df_malicious %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_sep <- bigram_malicious %>% separate(bigram, c("word1", "word2"), sep = " ")

bigram_filter <- bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_count <-bigram_filter  %>% 
  count(word1, word2, sort = TRUE)
bigrams_united <- bigram_filter %>%
  unite(bigram, word1, word2, sep = " ")
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

bigram_graph <- bigram_count %>%
  filter(n > 500) %>%
  graph_from_data_frame()

set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)




x <- subset(bigram_tf_idf, select = c(bigram, tf_idf))
# for(name in names_b) {
#   bigram_temp <- df_benign[df_benign$book == name,] %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
#   bigram_benign <- unite()
# }
# 
# 
# json_file <- "/Users/jingli/Desktop/classes/ECS251/project/benign/benign_11.json"
# json_file_2 <- "/Users/jingli/Desktop/classes/ECS251/project/benign/benign_15.json"
# json_data_1 <- fromJSON(paste(readLines(json_file), collapse = ""))
# json_data_2 <- fromJSON(paste(readLines(json_file_2), collapse = ""))
# 
# data1 <- unlist(json_data_1, use.names = FALSE)
# data2 <- unlist(json_data_2, use.names = FALSE)
# df <- data_frame(text = data, book = "benign_11")
# df_book <- data_frame(book = data1)
# 
# mySyscalls <- df %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
# 
# mySyscalls_f <-  mySyscalls %>% count(bigram, sort = TRUE)
# mySyscalls_united <- data.frame(book = "a", bigram = mySyscalls_f$bigram)
# # mySyscalls_sep <- mySyscalls_f %>% separate(bigram, c("word1", "word2"), sep = " ")
# # mySyscalls_united <- mySyscalls_sep %>% unite(bigram, word1, word2, sep = " ")
# 
# mySyscalls_tf_idf <- mySyscalls_united %>%
#   count(book, bigram) %>%
#   bind_tf_idf(bigram, book, n) %>%
#   arrange(desc(tf_idf))
# 
# 
# 
# austen_bigrams <- austen_books() %>%
#   unnest_tokens(bigram, text, token = "ngrams", n = 2)
# 
# austen_bigrams
# 
# bigrams_separated <- austen_bigrams %>%
#   separate(bigram, c("word1", "word2"), sep = " ")
# 
# bigrams_filtered <- bigrams_separated %>%
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word)
# 
# # new bigram counts:
# bigram_counts <- bigrams_filtered %>% 
#   count(word1, word2, sort = TRUE)
# 
# bigram_counts
# 
# 
# bigrams_united <- bigrams_filtered %>%
#   unite(bigram, word1, word2, sep = " ")
# 
# bigrams_united
# 
# bigram_tf_idf <- bigrams_united %>%
#   count(book, bigram) %>%
#   bind_tf_idf(bigram, book, n) %>%
#   arrange(desc(tf_idf))
# 
# bigram_tf_idf
# 
# 
