setwd("C:/Users/benjr/Desktop/Projects/StoryCorps_Scraping/")
options(scipen = 7, stringsAsFactors = FALSE)
library(readr)
library(dplyr)
#library(ggplot2)
library(tidytext)
library(tm)
library(topicmodels)
library(cld2)
#library(cld3)

read_csv("data/sc_interview.csv") -> sc

# ##SUMMARY STATS
# sc %>%
#   summarize(n(),
#             n_distinct(questions),
#             sum(is.na(questions)))
# 
# ##AVERAGE NUMBER OF QUESTIONS
# sc %>%
#   filter(!is.na(questions)) %>%
#   group_by(id) %>%
#   summarize(num_q = n_distinct(questions)) %>%
#   ungroup %>%
#   summarize(mean(num_q), median(num_q))
# 
# sc %>%
#   filter(!is.na(questions)) %>%
#   group_by(id) %>%
#   summarize(num_q = n_distinct(questions)) %>%
#   ungroup %>%
#   ggplot(aes(num_q)) +
#   geom_histogram(binwidth = 7) +
#   scale
#   
# 
# ##AVERAGE LENGTH OF INTERVIEW
# sc %>%
#   filter(length != 23590) %>%
#   distinct(id, length) %>%
#   summarize(mean(length/60),
#             median(length/60))
  

##TOP 10 QUESTIONS
sc %>%
  filter(!is.na(questions)) %>%
  count(questions, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  as.data.frame

data("stop_words")

sc %>%
  #filter(!is.na(questions) & cld2::detect_language(questions) == 'en') %>%
  filter(!is.na(questions)) %>%
  distinct(questions) %>%
  mutate(qid = 1:nrow(.)) -> q

q %>%
  group_by(qid) %>%
  unnest_tokens(word, questions) %>%
  ungroup %>%
  mutate(word = trimws(word) %>% removePunctuation) %>%
  anti_join(
    stop_words
  ) %>%
  filter(!grepl("_", word)) %>%
  filter(!grepl("^[[:digit:]]+$", word)) %>%
  filter(word != "") %>%
  #filter(cld2::detect_language(word) == 'en') %>%
  distinct -> sc_q

sc_q %>%
  count(qid, word, sort =  TRUE) %>%
  cast_dtm(qid, word, n) %>%
  LDA(k = 4, control = list(seed = 1234)) %>%
  tidy -> q_lda

q_lda %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
