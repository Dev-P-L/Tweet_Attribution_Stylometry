# Downloading packages. 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(utf8)) install.packages("utf8", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(tm)) install.packages("tm", repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(quanteda)) install.packages("quanteda", repos = "http://cran.us.r-project.org")
if(!require(wordcloud2)) install.packages("wordcloud2", repos = "http://cran.us.r-project.org")


# Requiring libraries.
library(tidyverse)
library(dslabs)
library(utf8)
library(lubridate)
library(scales)
library(tm)
library(wordcloud)
library(kableExtra)
library(gridExtra)
library(caret)
library(pROC)
library(ggthemes)
library(tidytext)
library(stringr)
library(quanteda)
library(wordcloud2)

Sys.setlocale("LC_ALL", "C")

cbbPalette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9", 
                "#009E73", "#F0E442",  "#D55E00", "#CC79A7")

data("trump_tweets")
tweets <- trump_tweets

# Let's extract relevant data.
buffer <- tweets %>% 
  mutate(device = str_replace_all(str_replace_all(source, "Twitter for Android",
                    "Android"), "Twitter for iPhone", "iPhone")) %>%
  filter(device %in% c("Android", "iPhone") &
         created_at >= ymd("2015-06-17") & 
         created_at < ymd("2016-11-08"))

# Let's split into training set and validation set
set.seed(1)
ind_val <- createDataPartition(y = buffer$device, 
                               times = 1, p = 1/3, list = FALSE)
ind_train <- as.integer(setdiff(1:nrow(buffer), ind_val))
train_tweets <- buffer[ind_train, ]
val_tweets <- buffer[ind_val, ]

train_tweets <- train_tweets %>%
  mutate(month = floor_date(with_tz(created_at, "EST"), unit = "month")) %>%
  mutate(week = floor_date(with_tz(created_at, "EST"), unit = "week")) %>%
  mutate(day = floor_date(with_tz(created_at, "EST"), unit = "day")) %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  mutate(am_pm = gsub('[0-9: ]+','\\1',format(created_at, '%r'))) %>%
  arrange(created_at) %>%
  select(- source) %>%
  select(device, everything()) 

str(train_tweets, vec.len = 1)

# Analysing tweets text.
# Same result in each case
nrow(trump_tweets %>% filter(!is_retweet))
nrow(trump_tweets %>% filter(is_retweet == FALSE))
nrow(trump_tweets)

# Which means all is_retweet values are FALSE
sum(trump_tweets$is_retweet)

# Tidying 
v <- train_tweets[1008, ] %>% 
  mutate(text = str_replace_all(text, "[\n]" , "")) %>%
  mutate(text = str_replace_all(text, "&amp", "")) %>%
  mutate(text = str_replace_all(text, "http.*" , ""))  

buffer <- data.frame(text =
  "\n@joebiden @joebiden #TBT &amp https://stackoverflow.com/questions/33995830/") 
buffer %>% 
  mutate(text = str_replace_all(text, "[\n]" , "")) %>%
  mutate(text = str_replace_all(text, "&amp", "")) %>%
  mutate(text = str_replace_all(text, "http.*" , ""))

# Finding occurrence of pattern
grep("\\$", train_tweets$text)
train_tweets$text[39]

###################################################
###################################################

## TIDYING IN EARNEST

# Adding leading and trailing white space character near punctuation marks.
# https://stackoverflow.com/questions/34874089/regex-r-add-space-between-punctuation-marks-and-words-but-also-between-punctua
# Great reference, great reference! 

# With str_replace_all
# str_squish() also reduces repeated whitespace inside a string.
temp <- train_tweets %>%
  mutate(text = str_replace_all(text, "\n" , " ")) %>%
  mutate(text = str_replace_all(text, "&amp", " ")) %>%
  mutate(text = str_replace_all(text, "http.*" , " ")) %>%
  mutate(text = str_replace_all(text, ".@" , " @")) %>%
  mutate(text = str_replace_all(text, '\"' , " ")) %>%
  mutate(text = str_replace_all(text, "[/(),:;!?–•…-]" , " ")) %>% 
  mutate(text = removeNumbers(text)) %>%
  mutate(text = str_squish(text)) %>%
  as.data.frame()

# Extracting hashtags
# https://stackoverflow.com/questions/13762868/how-do-i-extract-hashtags-from-tweets-in-r
v <- str_replace_all(temp$text, "[.']", "")
v <- str_extract_all(v, "#\\S+")

v[2135]

buffer <- ""
for (i in 1:length(v)) {
  w <- v[[i]]
  w <- ifelse(length(w) == 0, "", paste(w, collapse = " "))
  buffer <- append(buffer, w)
}

buffer <- buffer[2:length(buffer)] 

# SPLITTING BETWEEN LOWER CASE AND UPPER CASE
# https://stackoverflow.com/questions/43706474/splitting-string-between-capital-and-lowercase-character-in-r
# We can use regex lookaround to match lower case letters 
# (positive lookbehind - (?<=[a-z])) followed by upper case letters 
# (positive lookahead -(?=[A-Z]))
v <- 1:length(buffer)
for (i in 1:length(v)) {
  w <- str_replace_all(buffer[i], "#", "")
  w <- unlist(strsplit(w, "(?<=[a-z])(?=[A-Z])", perl = TRUE))
  v[i] <- paste(w, collapse = " ")
}
length(buffer)
length(v)
v

temp$text[914]
v[914]
v[2135]

# Il faut ajouter aux tweets privés de leurs hastags.

# NON je crois

# Ici if faut construire une table de correspondance.
check_up <- data.frame(text = buffer, expansion = v) %>%
  arrange(buffer)
check_up[10, ]

expanded <- 1:nrow(temp)

for (i in 1:nrow(temp)) {
  w <- paste(" ", temp$text[i], " ")
  for (j in 1:nrow(check_up)) {
    w <- gsub(paste(" ", check_up$text[j], " ", sep = ""),
              paste(" ", check_up$expansion[j], " ", sep = ""), w) 
  }
  expanded[i] <- w
}

grep("#", expanded)
expanded[2135]
grep("#1", train_tweets$text)
train_tweets$text[2135]

##############################################
##############################################
# WORDCLOUD

df <- data.frame(mot = c("toto", "%", "$"), freq = c(250, 100, 50))
set.seed(1)
wordcloud2(df, size = 1, minSize = 0, gridSize =  0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "blue",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65)

train_tweets$text[1096:1098]

buffer <- data.frame(text =
    "\n@joebiden ,, ; !? ---––– : \n\n @joebiden .@t \\ \"test\" t\t #TBT &amp \nhttps://www.kaggle.com/erikbruin/text-mining-the-clinton-and-trump-election-tweets http://topepo.github.io/caret/available-models.html") 

buffer <- buffer %>%
  mutate(text = str_replace_all(text, "\n" , " ")) %>%
  mutate(text = str_replace_all(text, "&amp", " ")) %>%
  mutate(text = str_replace_all(text, "http.*" , " ")) %>%
  mutate(text = str_replace_all(text, ".@" , " @")) %>%
  mutate(text = str_replace_all(text, '\"' , " ")) %>%
  mutate(text = str_replace_all(text, '[/(),:;!?–-]' , " ")) 
# Finding occurrence of pattern
buffer$text

grep("http://", train_tweets$text)
train_tweets$text[grep("http://", train_tweets$text)]

# First, cleaning up dataset.
# Keeping $ because lots of them and it might separate the 2 devices. 
temp <- temp %>% 
  mutate(text = str_replace_all(text, "\\$", " dollar "))
temp$text[39]

# Second, lowercasing using package quanteda to avoid creating corpus.
temp <- temp %>% mutate(text = char_tolower(text)) %>% .$text



stopword_composite <- 
  
sort(unique(tidytext::stop_words$word), decreasing = TRUE)
tidytext::stop_words %>% as.data.frame() %>% arrange(word)
str(tidytext::stop_words)




