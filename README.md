# Looking-for-content

Sentiment analysis - tweet attribution

# Second, lowercasing using package quanteda to avoid creating corpus.
temp %>% mutate(text = char_tolower(text, )) %>% .$text

stopword_composite <- 

sort(unique(tidytext::stop_words$word), decreasing = TRUE)
tidytext::stop_words %>% as.data.frame() %>% arrange(word)
str(tidytext::stop_words)
