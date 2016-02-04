## Part 1. Cleaning data

# input the tweets from chapters and d1 followers csv files
# but all commas within tweet contents removed
# and all corrupted recordsare removed.
hash.twt <- read.csv("hashtag_tweets - nocomma - checked_remap.csv")
chapter.twt <- read.csv("chapters_tweets - nocomma - checked.csv")
unique.twt <- read.csv("UniqueHashtagTweets.csv")

hash.twt.en <- hash.twt[hash.twt$lang == "en", ] 
chapter.twt.en <- chapter.twt[chapter.twt$lang == "en", ] 
unique.twt.en <- unique.twt[unique.twt$lang == "en", ] 

# removed all non-alphabetic characters in tweets content
WordExtraction <- function (some_txt) {
  # remove retweet entities
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  # remove <br>
  some_txt = gsub("<br>", "", some_txt)
  # remove at people
  some_txt = gsub("@\\w+", "", some_txt)
  # remove html links
  some_txt = gsub("http(s?)(://)(.*)[.|/|_](.*)+", "", some_txt)
  some_txt = gsub("htt(.*)", "", some_txt)
  # replace smile
  some_txt = gsub(": ", " ", some_txt)
  # some_txt = gsub("[:D|:)|;)|^^|^.^]", " smile ", some_txt)
  
  # remove Tweet key we used to query tweets 
  tweet.key <- c("#EarthHour", "#climatechange", "#yourpower", "#useyourpower")
  for (key in tweet.key) {
    some_txt = gsub(key, "", some_txt)  
  }
  
  # remove punctuation
  some_txt = gsub("[^A-Za-z]+", " ", some_txt)
  # some_txt3 = gsub("[^A-Za-z0-9]+", " ", some_txt)
  
  # remove unnecessary spaces
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  
  # define "tolower error handling" function 
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply 
  some_txt = sapply(some_txt, try.error)
}

some_txt <- hash.twt.en[, "text"]
some_txt <- WordExtraction(some_txt)
write.csv(some_txt, "./Wordcloud_Input/hashWords.csv", row.names=F)

some_txt <- chapter.twt.en[, "text"]
some_txt <- WordExtraction(some_txt)
write.csv(some_txt, "chapterWords.csv", row.names=F)

some_txt <- unique.twt.en[, "text"]
some_txt <- WordExtraction(some_txt)
write.csv(some_txt, "uniqueWords.csv", row.names=F)

## Part 2. Running the package, see the below link for step-by-step details
## https://georeferenced.wordpress.com/2013/01/15/rwordcloud/


#import the librabry
library(wordcloud)
library(tm)

# import the data
lords <- Corpus(DirSource("./Wordcloud_Input/"))

# check the data
inspect(lords)

# transform and prepare the data for the word cloud
lords <- tm_map(lords, stripWhitespace)
lords <- tm_map(lords, content_transformer(tolower))
lords <- tm_map(lords, removeWords, stopwords("english"))
lords <- tm_map(lords, removeWords, c("amp"))
lords <- tm_map(lords, stemDocument)

# Word cloud graph
wordcloud(lords, scale=c(3,0.3), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

# Part 3: combined word cloud across sentiment
rank.twt <- read.csv("Tweets_rank.csv")
rank.twt <- subset(rank.twt, !duplicated(rank.twt$text))
rank.twt.en <- rank.twt[rank.twt$lang == "en", ] 

some_txt <- rank.twt.en[, "text"]
some_txt <- WordExtraction(some_txt)
write.csv(some_txt, "rankWords.csv", row.names=F)

# draw it man!
emos = levels(factor(rank.twt.en$rank))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[rank.twt.en$rank == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.3), random.order = FALSE, title.size = 1.5)
