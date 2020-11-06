
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
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(quanteda)) install.packages("quanteda", repos = "http://cran.us.r-project.org")
if(!require(wordcloud2)) install.packages("wordcloud2", repos = "http://cran.us.r-project.org")
if(!require(textreg)) install.packages("textreg", repos = "http://cran.us.r-project.org")
if(!require(remotes)) install.packages("remotes", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(textdata)) install.packages("textdata", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")

# install ???? shiny httpuv xtable sourcetools fastmap

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
library(textreg)
library(remotes)
library(tidytext)
library(textdata)
library(devtools)

devtools::install_github("gaospecial/wordcloud2")

# If getting into trouble with downloading ncr lexicon. 
# install_github("EmilHvitfeldt/textdata")
# install_github("juliasilge/tidytext")
# textdata::lexicon_nrc(delete = TRUE)
# textdata::lexicon_nrc()
# https://github.com/juliasilge/tidytext/issues/146

# If getting into trouble with knitting several wordcould2.
# https://github.com/Lchiffon/wordcloud2/issues/65
# Thank you to gaospecial
# devtools::install_github("gaospecial/wordcloud2")

# To fix up the problem of the bird
# https://github.com/Lchiffon/wordcloud2/issues/12
# devtools::install_github("lchiffon/wordcloud2")

Sys.setlocale("LC_ALL", "C")

# The palette with black:
cbf_b_Palette <- c("#E69F00", "#0072B2", "#000000", #56b4e9,  
                "#009E73", "#D55E00", "#CC79A7")

cbf_y_Palette <- c("#E69F00", "#0072B2", "#000000",  
                "#009E73", "#F0E442", "#D55E00", "#CC79A7")

data("trump_tweets")
tweets <- trump_tweets %>% 
  mutate(text = sapply(text, utf8_normalize, map_quote = TRUE))

# Otherwise curly apostrophes remain.

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

# The Stack Overflow is about removing links. Why presented in that way? 
  
buffer <- data.frame(text =
  ".@l \n@joebiden .@he . ... .l l. l.l essai ok non@joe jest@joebiden jtt#TBT &amp https://stackoverflow.com/questions/33995830/") 

buffer %>% 
  mutate(text = str_replace_all(text, "[\n]" , "")) %>%
  mutate(text = str_replace_all(text, "&amp", "")) %>%
  mutate(text = str_replace_all(text, "[?@]", " @")) %>%
  mutate(text = str_replace_all(text, "[?#]", " #")) %>%
  mutate(text = str_replace_all(text, "[\\.]", ""))

# Finding occurrence of pattern
grep("\\$", train_tweets$text)
train_tweets$text[39]

###################################################
###################################################

## TIDYING IN EARNEST

# Adding leading and trailing white space character near punctuation marks.
# https://stackoverflow.com/questions/34874089/regex-r-add-space-between-punctuation-marks-and-words-but-also-between-punctua
# Great reference, great reference! 
# But not "." in order to preserve abreviations and not numbers 
# in order to preserve #1. 

# With str_replace_all().
# We don't remove dots but bullets ...
# str_squish() also reduces repeated whitespace inside a string.
temp <- train_tweets %>%
  mutate(text = str_replace_all(text, "\n" , " ")) %>%
  mutate(text = str_replace_all(text, "&amp", " ")) %>%
  mutate(text = str_replace_all(text, "http.*" , " ")) %>%
  mutate(text = str_replace_all(text, ".@" , " @")) %>%
  mutate(text = str_replace_all(text, '\"' , " ")) %>%
  mutate(text = str_replace_all(text, "[/(),:;!?–•…-]" , " ")) %>% 
  mutate(text = str_squish(text)) %>%
  as.data.frame()

# Before extracting hashtags
# CAUTION! The # symbol is mostly the hashtag symbol but sometimes 
# the number symbol. In this case, it is in the combination #1. To preserve 
# these cases, #1 is going to be transformed into NUMBERONE and later on will
# become #1 again. 
# Digits are replaced with white space characters instead of just being
# removed to avoid collapsing the $ symbol with words in transforming 
# "$13million" into "$million". 
length(grep("#1", temp$text))

temp <- temp %>%
  mutate(text = str_replace_all(text, "#1", " NUMBERONE ")) %>%
  mutate(text = str_replace_all(text, "[0-9]", " "))
length(grep("NUMBERONE", temp$text))

#########################################

# Otherwise with compact hashtags

# temp$text is readily available. 

# Removing trailing dots (periods or full stops) 
# at the end of a word ending in a lowercase letter, 
# so preserving abreviations usually in uppercase.
compact <- temp
comp <- compact$text
seq_comp <- seq_along(comp)
for (i in seq_comp) {
  w <- comp[i]
  g <- unlist(gregexpr(pattern = "[a-z]\\.", w))
  
  # for loop to adjust all cases of dots following a lowercase letter
  seq_g <- seq_along(g)
  for (j in seq_g) {
    pos <- g[j] + 1
    substr(w, pos, pos) <- " "
  }
  comp[i] <- w
}

compact$text <- comp
compact$text

train_tweets$text[991]
temp$text[991]
compact$text[991]

# To further clean, let's also remove dots with leading and trailing 
# white space character.
compact$text <- str_replace_all(compact$text, " \\. ", " ")
compact$text[998]

# Lowercasing. 
compact$text <- char_tolower(compact$text)

# Removing stopwords from the package tidytext (many more than in tm). 
# Adding leading and trailing white space character to stopwords
# in order to prevent letters from abreviations such as "U.S." 
# being removed.
stop <- paste(" ", stopwords("english"), " ", sep = "")
seq_stop <- seq_along(stop)
# Adding leading and trailing white space chatacters to the tweets. 
comp <- paste(" ", compact$text, " ", sep = "")
seq_comp <- seq_along(comp)
for (i in seq_comp) {
  w <- comp[i] 
  
  # Inner for loop to care for each stopword
  for (j in seq_stop) {
    w <- gsub(stop[j], " ", w) 
  }
  comp[i] <- w
}

compact$text <- comp

# Replacing remaining apostrophes with one white space character
# in order to isolate the "s" in "'s" in (some) genitive cases. 
# https://community.rstudio.com/t/tm-package-removing-unwanted-characters-works-in-r-but-not-knitr/26734
compact$text <- str_replace_all(compact$text, "'s", " ")
compact$text

# Removing period punctuation marks without inserting any whitespace 
# in order to preserve abreviations, such as "usa", and acronyms. 
# How many?
compact$text <- str_remove_all(compact$text, "\\.")

# Reconverting "NUMBERONE" to "#1" back.
compact$text <- str_replace_all(compact$text, "numberone", " #1 ")

# Removing extra white space characters.
compact$text <- str_squish(compact$text)

train_tweets$text[889]
compact$text[889]

length(grep("#1", compact$text))

##############################################
##############################################

# EXPANDING TWEETS
# FIRST EXPANDING HASHTAGS

# https://stackoverflow.com/questions/13762868/how-do-i-extract-hashtags-from-tweets-in-r
v <- str_extract_all(temp$text, "#\\S+")
v[2135]
str(v)
class(v)
class(v[96])
class(v[2135])

# v is a list. Each element contains a character vector, which is most of the 
# time null because there is no hashtag. 
# In each tweet with one or more hastags, let's amalgamate all hashtags 
# into one string with white space between the different hashtags. 
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
v

temp$text[914]
v[914]
temp$text[2135]
v[2135]

# Now we've got to add the expanded hastags to the tweets 
# after removing the compact hashtags from the tweets.
# So, let's first retrieve the compact hashtags from the tweets.
expanded <- temp %>% 
  mutate(text = str_remove_all(temp$text, "#\\S+")) %>%
  mutate(text = paste(text, v, sep = " ")) 

expanded$text

# Let's NOT switch to vcorpus() and cleaning functions from the 
# package tm.  Cleaning will be tailor-made to get more flexibility.
# Combining stopwords from the package tidytext with the function
# removeWords leads to removing separate letters from abreviations 
# such as "u.s.". 
removeWords("it is a test isheheram u.s.a. U.S. @test #test will I i isn't it's",
  unique(tidytext::stop_words$word))

# Removing trailing dots (periods or full stops) at the end of a word ending
# in a lowercase letter, so preserving abreviations usually in uppercase.
exp <- expanded$text
seq_exp <- seq_along(exp)
for (i in seq_exp) {
  w <- exp[i]
  g <- unlist(gregexpr(pattern = "[a-z]\\.", w))
  
  # Other for loop to adjust all cases of dots following a lowercase letter.
  seq_g <- seq_along(g)
  for (j in seq_g) {
    pos <- g[j] + 1
    substr(w, pos, pos) <- " "
  }
  exp[i] <- w
}

expanded$text <- exp
expanded$text

train_tweets$text[991]
temp$text[991]
expanded$text[991]

# To further clean, let's also remove dots with leading and trailing 
# white space character.
expanded$text <- str_replace_all(expanded$text, " \\. ", " ")
expanded$text[998]

# Lowercasing. 
expanded <- expanded %>% mutate(text = char_tolower(text))

# Removing stopwords from the package tidytext (many more than in tm). 
# Adding leading and trailing white space character to stopwords
# in order to prevent letters from abreviations such as "U.S." 
# being removed.
stop <- paste(" ", stopwords("english"), " ", sep = "")
seq_stop <- seq_along(stop)
# Adding leading and trailing white space chatacters to the tweets. 
exp <- paste(" ", expanded$text, " ", sep = "")
seq_exp <- seq_along(exp)
for (i in seq_exp) {
  w <- exp[i] 
  
  # Inner for loop to care for each stopword
  for (j in seq_stop) {
    w <- gsub(stop[j], " ", w) 
  }
  exp[i] <- w
}

expanded$text <- exp

# Replacing remaining apostrophes with one white space character
# in order to isolate the "s" in "'s" in (some) genitive cases. 
# https://community.rstudio.com/t/tm-package-removing-unwanted-characters-works-in-r-but-not-knitr/26734
expanded$text <- str_replace_all(expanded$text, "'s", " ")
expanded$text

# Removing period punctuation mark without inserting any whitespace 
# in order to preserve abreviations, such as "usa", and acronyms. 
# How many?
expanded$text <- str_remove_all(expanded$text, "\\.")

# Reconverting "NUMBERONE" to "#1" back.
expanded$text <- str_replace_all(expanded$text, "numberone", " #1 ")

# Removing extra white space characters.
expanded$text <- str_squish(expanded$text)

train_tweets$text[889]
expanded$text[889]

length(grep("#1", expanded$text))


# 3 versions of the training set:

# - a compact version: the training set cleaned but including
# hashtags and mentions (here mentions are "Twitter mentions", 
# i.e. @usernames included in tweets;
# - a fully expanded version: the training set cleaned and including
# hashtags and mentions expanded;
# - a light version: the training set cleaned with hastags and 
# mentions expanded but without most proper names 
# (proper names from text, expanded hastags and mentions).

# In sentiment analysis, the 3 versions can be used. Indeed, sentiment analysis 
# will use only words from preexisting dictionaries (Bing, etc.) and common names
# can be found in them, in principle no proper name. Consequently, the version 
# shouldn't make a difference since all matched common names are already
# in principle in the light version, but from a computational point of view,
# the light version seems recommended.

# This reasoning remains valid whether applied to evaluating hyperbolism
# or tweet attribution.

# In machine learning using word frequency for tweet attribution, proper names
# can make a difference since proper name frequency can vary from 
# one device to the other. In principle, the compact version or even
# the fully expanded version should be used: both will be tried
# and choice will be made based on performance metric. 

# Compact version already exists. It is the data frame *compact*.

# Let's build up the fully expanded version.

#####################################
#####################################

# FULLY EXPANDED VERSION

# Let's start with extracting mentions. 

mentions <- unlist(str_extract_all(train_tweets$text, "@\\S+"))
mentions <- str_remove_all(mentions, "'s")
mentions <- str_replace_all(mentions, "[[:punct:]]", "")
mentions <- str_replace_all(mentions, "[~=+^", "")
mentions <- str_remove_all(mentions, "[0-9]")
mentions

unique <- sort(unique(mentions))
ind_upper <- grep("[A-Z]", unique, perl=TRUE)
ind_lower <- setdiff(seq_along(unique), ind_upper)

mentions_upper <- unique[ind_upper]
mentions_lower <- unique[ind_lower]

# Normalizing uppercase/lowercase: if a mention exists with lowercase only
# and with some uppercase letters, then the form with uppercase letters 
# is preferred.
mentions_lower_mod <- mentions_lower
for (i in seq_along(mentions_lower)) {
  for (j in seq_along(mentions_upper)) {
    if(mentions_lower[i] == tolower(mentions_upper[j])) 
      {mentions_lower_mod[i] <- mentions_upper[j]}
  }
}

# Now, let's simply replace in the vector *unique* the mentions_lower values 
# with *the mentions_lower_mod* values. 
for (i in seq_along(mentions)) {
  for (j in seq_along(mentions_lower))
    if(mentions[i] == mentions_lower[j]) 
      {mentions[i] <- mentions_lower_mod[j]}    
}
sort(mentions)

# Let's split mentions based on uppercase. 
# SPLITTING BETWEEN LOWER CASE AND UPPER CASE
# https://stackoverflow.com/questions/43706474/splitting-string-between-capital-and-lowercase-character-in-r
# We can use regex lookaround to match lower case letters 
# (positive lookbehind - (?<=[a-z])) followed by upper case letters 
# (positive lookahead -(?=[A-Z]))

for (i in seq_along(mentions)) {
  w <- mentions[i]
  w <- unlist(strsplit(w, "(?<=[a-z])(?=[A-Z])", perl = TRUE))
  w <- paste(w, collapse = " ")
  mentions[i] <- w
}

sort(mentions)

# Removing stopwords, very unfrequent, from mentions.
stop <- paste(" ", stopwords("english"), " ", sep = "")
seq_stop <- seq_along(stop)
# Adding leading and trailing white space characters to the tweets. 
# Lowercasing otherwise we might miss the whole intention. 
ment <- paste(" ", tolower(mentions), " ", sep = "")
seq_ment <- seq_along(ment)
for (i in seq_ment) {
  w <- ment[i] 
  
  # Inner for loop to care for each stopword
  for (j in seq_stop) {
    w <- gsub(stop[j], " ", w) 
  }
  ment[i] <- w
}
mentions <- ment

# Now, to get the fully expanded version of the training set,
# it suffices to add the vector *mentions* to the feature 
# *expanded$text* after expurgating this feature from mentions, of course.
# In the original tweets, some @ symbols were isolated and need removing.
# Other symbols as well such as ~ (63) or = (64) or + (67).
# Except of course # since "numberone" had been replaced with "#1".
# $ is sometimes $m. It was so, no longer because since digits 
# have been replaced with white space. 
 
fully_expanded <- expanded %>% 
  mutate(text = str_remove_all(text, "@\\S+")) %>%
  mutate(text = str_replace_all(text, "([#-])|[[:punct:]]", "\\1")) %>%
  mutate(text = str_replace_all(text, "[~=+^]", " ")) %>%
  mutate(text = paste(text, mentions, sep = " ")) %>%
  mutate(text = str_squish(text))
fully_expanded$text

g <- grep("#1", fully_expanded$text)
str(g)
fully_expanded$text[g]
fully_expanded$text[128]
train_tweets$text[g]
s <- str_replace_all("=(^{})*/²³^¨ # @ + ~ ` ´", "[[:punct:]]", " ")
grep("[[:punct:]]", s)
length(grep("great", fully_expanded$text))
length(grep(" great ", paste(" ", fully_expanded$text, " ", sep = "")))
g <- grep("great", fully_expanded$text)
g1 <- grep(" great ", paste(" ", fully_expanded$text, " ", sep = ""))
g2 <- setdiff(g, g1)
fully_expanded$text[g2]

########################################
########################################

# And now, let's end it up with building up a light version 
# of the training set in order to remove proper names. 
# The objective is about facilitating the EDA of sentiment-related tokens 
# by (provisionally) discarding proper nouns.  

# Anyway, sentiment analysis and machine will be conducted on the 
# fully expanded data frame. 

# To buil up the light version, we do need a list of proper names 
# in order to remove them from the fully expanded version. As a proxy, the 
# tokens originating from the mentions will be used and they will be removed
# from the fully expanded version. 

# Let's split the mentions at white space.
names <- sort(unique(unlist(strsplit(str_squish(mentions), split = " "))))
names <- paste(" ", names, " ", sep = "")

# Let's add leading and trailing white space to fully expanded tweets.
exp <- paste(" ", fully_expanded$text, " ", sep = "")

# Let's remove split names from fully expanded tweets.
el <- as.character(seq_along(exp))

for (i in seq_along(exp)) {
  w <- exp[i]
  
  for (j in seq_along(names)) {
    w <- gsub(names[j], " ", w)
  }
  
  el[i] <- w
  
}

expanded_light <- fully_expanded %>% 
  mutate(text = str_squish(el))
expanded_light$text

length(grep("great", expanded_light$text))
length(grep("great", fully_expanded$text))
length(grep("great", expanded$text))
length(grep("great", train_tweets$text))
grep("great", names)

# COMPACT LIGHT

comp <- paste(" ", compact$text, " ", sep = "")
el <- seq_along(comp) 

for (i in seq_along(comp)) {
  w <- comp[i]
  
  for (j in seq_along(names)) {
    w <- gsub(names[j], " ", w)
  }
  
  el[i] <- w
}

compact_light <- compact %>% mutate(text = str_squish(el))
expanded_light$text

length(grep(" great ", compact_light$text))
length(grep(" greatly ", compact$text))
length(grep(" great ", train_tweets$text))
grep(" great ", names)

# WORDCLOUD

list_crude <- 
  temp %>% 
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_crude

list_compact <- 
  compact %>% 
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_compact 
str(list_compact)

length(grep(" great ", paste(" ", compact$text, " ", sep = "")))
grepl(" g ", "g g g g t")
sum(str_count("g g g g t", " g "))
str_count("g g g g t", " g ")
s <- str_count(paste(" ", compact$text, " ", sep = ""), "great ")
sum(s)

list_compact_Android <- 
  compact %>% 
  filter(device == "Android") %>%
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_compact_Android 

list_compact_iPhone <- 
  compact %>% 
  filter(device == "iPhone") %>%
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_compact_iPhone

list_compact_light_Android <- 
  compact_light %>% 
  filter(device == "Android") %>%
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_compact_Android 

list_compact_light_iPhone <- 
  compact_light %>% 
  filter(device == "iPhone") %>%
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_compact_iPhone

list_expanded_Android <- 
  expanded %>% 
  filter(device == "Android") %>%
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_expanded_Android

list_expanded_iPhone <- 
  expanded %>% 
  filter(device == "iPhone") %>%
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_expanded_iPhone

list_fully_expanded <- 
  fully_expanded %>% 
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_fully_expanded

list_fully_expanded_Android <- 
  fully_expanded %>% 
  filter(device == "Android") %>%
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_fully_expanded_Android

list_fully_expanded_iPhone <- 
  fully_expanded %>% 
  filter(device == "iPhone") %>%
  select(text) %>%
  unnest_tokens(words, text) %>% count(words, sort = TRUE)
list_fully_expanded_iPhone

# Wordcloud compact Android
# https://stackoverflow.com/questions/27981651/text-wordcloud-plotting-error
# You may also try plotting with a larger device, for example
words <- list_compact_Android$words
freq <- list_compact_Android$n
dev.new(width = 1000, height = 1000, unit = "px")
set.seed(1)
wordcloud(words, freq, min.freq = 10, 
          max.words = 10, random.order = FALSE, random.color = FALSE,
          colors = rev(cbf_b_Palette), 
          rot.per = 1/3, scale = c(8,.5))
wordcloud2(list_compact_Android, color = cbf_y_Palette, 
           backgroundColor = "powderblue", 
           shuffle = FALSE)

# Wordcloud compact iPhone
words <- list_compact_iPhone$words
freq <- list_compact_iPhone$n
dev.new(width = 1000, height = 1000, unit = "px")
set.seed(1)
wordcloud(words, freq, min.freq = 10, 
          max.words = 10, random.order = FALSE, random.color = FALSE,
          rot.per = 1/4, colors = rev(cbf_b_Palette), 
          scale = c(6,.5))

# Wordcloud CRUDE
words <- list_crude$words
freq <- list_crude$n
dev.new(width = 1000, height = 1000, unit = "px")
set.seed(1)
wordcloud(words, freq, min.freq = 10, 
          max.words = 10, random.order = FALSE, random.color = FALSE,
          rot.per = 1/4, colors = rev(cbf_b_Palette), 
          scale = c(8,.5))

df <- list_crude %>% 
  arrange(desc(n)) %>% head(., 12) %>%
  `colnames<-`(c("word", "freq"))
set.seed(1)
wordcloud2(df, color = cbf_y_Palette, backgroundColor = "blue", 
           shuffle = FALSE)

# Wordcloud2 compact Android
df <- list_compact_Android %>% 
  arrange(desc(n)) %>% head(., 12) %>%
  `colnames<-`(c("word", "freq"))
set.seed(1)
wordcloud2(df, color = cbf_b_Palette, backgroundColor = "white", 
           shuffle = FALSE)

# Wordcloud2 compact iPhone
buffer <- list_compact_iPhone %>% 
  arrange(desc(n)) %>% head(., 12) %>%
  `colnames<-`(c("words", "freqs"))
set.seed(1)
wordcloud2(buffer, color = cbf_b_Palette, backgroundColor = "grey", 
           shuffle=FALSE)

##########################################
# Negation is not in.

s <- get_sentiments("bing") %>% as.data.frame() %>%
  mutate(word = paste(" ", word, " ", sep = ""))
grep(" adulate ", s$word)

s <- get_sentiments("nrc") %>%
  as.data.frame() %>%
  mutate(word = paste(" ", word, " ", sep = ""))
grep(" love ", s$word)

s <- get_sentiments("afinn") %>%
  as.data.frame() %>%
  mutate(word = paste(" ", word, " ", sep = ""))
grep(" no ", s$word)
s$word[762]
s$word[1612]
s$word[1613]
s$word[1605]
s$word[1606]

s <- get_sentiments("loughran") %>%
  as.data.frame() %>%
  mutate(word = paste(" ", word, " ", sep = ""))
grep(" not ", s$word)

test_Android <- temp %>% 
  select(text, device) %>%
  mutate(text = paste(" ", text, " ", sep = "")) %>%
  filter(device == "Android") 
grep(" aren't ", test_Android$text)

test_iPhone <- temp %>% 
  select(text, device) %>%
  mutate(text = paste(" ", text, " ", sep = "")) %>%
  filter(device == "iPhone") 
grep(" aren't ", test_iPhone$text)








