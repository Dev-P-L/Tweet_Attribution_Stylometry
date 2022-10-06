# Acces to HTML Output Document

https://github.com/Dev-P-L/Tweet_Attribution_Stylometry

<br>
<br>

# Presentation of Projet on
# Tweet Attribution through Stylometry

<br>

## Which are the files included in this repository?

* **Tweet_Attribution_Stylometry.html** is the output in HTML. 

  * For readers' convenience, Tweet_Attribution_Stylometry.html is an HTML document with interactive layout: the table of contents, the wordclouds, the graphs, and many tables are interactive; moreover, code can be visualized by pushing tag buttons on the right-hand-side of the HTML document.

  * Furthermore, for everyone’s convenience, I have tried using, when possible, colors that are clearly distinguishable to take into account alternative color perception, following pieces of advice given in [Cookbook R](http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/). As far as code visualization is concerned — when pushing tag buttons on the right-hand-side of the HTML document —, I hope the theme espresso, which I have chosen, is satisfactory.

  * If you wish to visualize Tweet_Attribution_Stylometry.html, you can activate the hyperlink https://dev-p-l.github.io/Tweet_Attribution_Stylometry/Tweet_Attribution_Stylometry.html or use GitHub Desktop or knit Tweet_Attribution_Stylometry.rmd or ask me by mail for a copy. 

* **Tweet_Attribution_Stylometry.rmd** contains all code in R Markdown. 

  * You are most welcome to knit the file Tweet_Attribution_Stylometry.Rmd to produce the document Tweet_Attribution_Stylometry.html. If you wish to run or knit the file Tweet_Attribution_Stylometry.Rmd on your computer, I suggest placing the files Tweet_Attribution_Stylometry.Rmd and styles.css in the same folder.

* **styles.css** contains most layout specificities of the file Tweet_Attribution_Stylometry.html.

* **train_tweets.csv** contains all data from the training set.

* **val_tweets.csv** contains all data from the validation set.

<br>

## What is this project?

* **Executive Summary**

  * An accuracy level of 92 % has been reached in attributing tweets on the validation set with 212 stylometric predictors, while a baseline model would predict with an accuracy level of 53 % (percentage of the main class). This is deemed to be of valuable predictive quality on very short texts.

  * This has required long preparation through Data Profiling, Data Wrangling, Exploratory Data Analysis, and Predictor Building.

  * Tweets come from the account of Candidate Donald Trump during the 2016 US presidential election campaign. Two devices have been used to issue these tweets: an Android device and an iPhone. The challenge is to predict the sending device on the validation set by using stylometric predictors.

  * Other predictors are available, such as timing, reactions (likes and retweets), content words or sentiments. They might be used later on in other Data Science projects based on the same dataset to further increase accuracy.

  * Tweet attribution has been operated through Machine Learning with one algorithm: eXtreme Gradient Boosting Tree.

  * This project is merely technical; it expresses absolutely no political vision or standpoint; it is in no way person-related; the author’s methods, insights, results, and conclusions are only the ones explicitly expressed in this project itself, which only encompasses files lodged with the GitHub repository: https://github.com/Dev-P-L/Tweet_Attribution_Stylometry .

* **TAGS**

  * stylometry, tweet attribution, Natural Language Processing, Text Mining, Regex, interactive wordcloud, interactive graph, interactive table, Machine Learning, eXtreme Gradient Boosting Tree

* **GitHub**

  * https://github.com/Dev-P-L/Tweet_Attribution_Stylometry

* **Requirements**

  * This project is based on the dataset trump_tweets from the R package dslabs. This means that usage of this project must strictly comply with all requirements imposed by dslabs and by all dslabs sources.
