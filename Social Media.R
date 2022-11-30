library(tm) # load tm for text mining
library(ggplot2) # load ggplot2 for plotting
library(wordcloud) # load wordcloud for word-cloud generator
library(SnowballC) # load SnowballC for text stemming
library(RColorBrewer) # load RcolorBrewer for color palettes
library(syuzhet) # load syuzhet for sentiment analysis and emotion classification

text <- readLines(file.choose()) # read the text file 
TextDoc <- Corpus(VectorSource(text)) # Load the data as a corpus
View(text) # view the dataset

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x)) 
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))

# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)

# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("the, is, at, on"))

# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)

# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("and", "the", "use","with","that","studies","social","https")) 

# Remove extra white space
TextDoc <- tm_map(TextDoc, stripWhitespace)

# Reducing the words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

# Build a document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descending value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq,las = 2, names.arg = dtm_d[1:5,]$word,
        col ="blue", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

# generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_dtm, terms = c("media","health","mental"), corlimit = 0.25)

# Find associations for words that occur at least 50 times
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)

# counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(text)

# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))

# Computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:41]))

#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

# plot count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Emotion Sentiments")


