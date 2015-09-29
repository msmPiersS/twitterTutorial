####################################################################################
## R tutorial for pulling twitter data and manipulating
## June 2014
## ps
##
##
####################################################################################


####################################################################################
## setup

  # check operating system
  #localFlag = Sys.info()["sysname"] == "Windows"  

  # define directories
    work_dir = getwd()
    data_dir = work_dir
    load_dir = work_dir
 

  #setwd(work_dir)
  #getwd()

  #libs
  library(plyr)
  library(Matrix)
  library(slam)

  # input parameters
  #usFlag = 0

  #if (usFlag) {
  #  proxy = "208.87.234.180" #us  
  #}  else {
  #  proxy = "85.115.54.180" #uk  
  #}
  #proxy_port = 8081
  #perDay = 5000;

  options("scipen"=100, "digits"=4)

  consumerKey = "lVbSHTMLK9xIPUmEJzLaMg"
  consumerSecret = "BLq7XWnMZhqZKgZT4ZKcaLao1MIblZxySStzGm5hH6M"
  tokenKey = "18312009-IUK9zOjvSCox5gWCP9TUHKzUGuiELviIc7MCcMYZL"
  tokenSecret = "K48BJcIg6z3u8joVNCUoGZGjRbE5dMoGr8WnsOKXY"

## end setup
####################################################################################


####################################################################################
## get twitter data

  # In order to access the twitter API you need to sign up for a developer key
  # https://dev.twitter.com/discussions/631
  # This is pretty straightforward although you will need a twitter account
  # This provides you with the key authentication required to access the API
  # your consumerKey, consumerSecret, tokenKey and tokenSecret 

  library(httr) # useful package from Hadley Wikham to neogiate the web
  library(rjson) #useful package for dealing with data in json format

  # First thing we need to do is get an authenticated connection
  # myapp defines the 'application' I have set up along with the keys associated with it


## set up connection and query

  myapp = oauth_app("dataPullPS", key = consumerKey, secret = consumerSecret)
  
  # sig then defines a connection using the app with the correct authentication
  
  sig = sign_oauth1.0(myapp,
                    token = tokenKey,
                    token_secret = tokenSecret)

  # check to make sure we have a valid connection
  statusQuery = "https://api.twitter.com/1.1/application/rate_limit_status.json?resources=search"
  #statusRaw = GET(statusQuery, c(sig, use_proxy(proxy, port = proxy_port)))
  statusRaw = GET(statusQuery, c(sig))
  statusData = fromJSON(as.character(statusRaw))
  statusData

  # looks ok- lets try and get some data
  # what you pull is defined by the query you submit- 
  # we are using the search api which searches through a sample of tweets for 
  # ones that match the search string

  # set up base query
  # info here https://dev.twitter.com/docs/using-search
  baseQuery = "https://api.twitter.com/1.1/search/tweets.json?"
  searchTermMSM = "q=moneysupermarket"
  searchTermDataScience = "q=%22data%20science%22OR%22datascience%22"
  
  #searchTerm = searchTermDH
  searchTerm = searchTermMSM
  #searchTerm = searchTermCustom
  lang = "&lang=en"
  count = "&count=100"

  # create full query
  tweetQuery = paste(baseQuery,searchTerm,lang,count ,sep='')
  tweetQuery



## submit query

  # submit querty to API using connection
  #tweetsRaw = GET(tweetQuery, c(sig, use_proxy(proxy, port = proxy_port)))
  tweetsRaw = GET(tweetQuery, c(sig))
  
  #save raw pull incase we struggle with internet
  #save(searchTerm, tweetQuery, statusData, tweetsRaw, file=paste(data_dir, "twitterRawData.Rdata", sep=""))
  #load(paste(data_dir, "twitterRawData.Rdata", sep=""))

  # use json package to turn json data into something we can use
  # each tweet is stored in a list, with a wide array of information associated with it
  tweetsDataList = fromJSON(as.character(tweetsRaw))

  # what did we get back?
  names(tweetsDataList$search_metadata)
  tweetsDataList$search_metadata$query
  tweetsDataList$search_metadata$count

  names(tweetsDataList$statuses[[1]])
  tweetsDataList$statuses[[1]]$text

  names(tweetsDataList$statuses[[1]]$user)
  tweetsDataList$statuses[[1]]$user$name
  tweetsDataList$statuses[[1]]$entities


## turn results into something useful

  #still not that usable - lets turn it into a more useful format
  tweetsData = data.frame(id = sapply(tweetsDataList$statuses , function(x) x$id))
  
  tweetsData$tweets = sapply(tweetsDataList$statuses , function(x) as.character(x$text))
  tweetsData$created_at= sapply(tweetsDataList$statuses , function(x) x$created_at)
  tweetsData$userid =sapply(tweetsDataList$statuses , function(x) x$user$id)
  tweetsData$usercreated =sapply(tweetsDataList$statuses , function(x) x$user$created_at)
  tweetsData$lang =sapply(tweetsDataList$statuses , function(x) x$metadata$iso_language_code)
  tweetsData$time_zone =sapply(tweetsDataList$statuses , function(x) x$user$time_zone)
  tweetsData$name =sapply(tweetsDataList$statuses , function(x) as.character(x$user$name))
  tweetsData$screen_name =sapply(tweetsDataList$statuses , function(x) as.character(x$user$screen_name))
  tweetsData$location =sapply(tweetsDataList$statuses , function(x) as.character(x$user$location))
  tweetsData$followers_count =sapply(tweetsDataList$statuses , function(x) x$user$followers_count)
  tweetsData$friends_count =sapply(tweetsDataList$statuses , function(x) x$user$friends_count)
  tweetsData$retweet_count = sapply(tweetsDataList$statuses , function(x) x$retweet_count)
  tweetsData$favorite_count = sapply(tweetsDataList$statuses , function(x) x$favorite_count)
  tweetsData$hashtags= sapply(tweetsDataList$statuses , function(x) as.character(x$entities$hashtags))

  
## end get twitter data
####################################################################################


####################################################################################
## Explore twitter data

  #useful package for processing and analysing text: tm (for topic models)

  library(tm)
  
## first need to clean up text

  #pull the text of the tweets from our data frame
  #load(file =  paste(data_dir,'dsTweets.rData', sep=""))
  #tescoTweets = dsTweets
  tgtTweets = tweetsData

  tweets = tgtTweets$tweet

  # first we need to clean up the text and standardise as best we can
  tmpCorpus = Corpus(VectorSource(tweets))  
  
  # remove weird encoding
  tmpCorpus = tm_map(tmpCorpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")), lazy=TRUE)
  tmpCorpus = tm_map(tmpCorpus, content_transformer(function(x) gsub('ã', "", x)), lazy=TRUE)
  tmpCorpus = tm_map(tmpCorpus, content_transformer(function(x) gsub('â', "", x)), lazy=TRUE)
  tmpCorpus = tm_map(tmpCorpus, content_transformer(function(x) gsub('€', "", x)), lazy=TRUE)
  tmpCorpus = tm_map(tmpCorpus, content_transformer(function(x) gsub('™', "", x)), lazy=TRUE)

  # strip whitespace
  tmpCorpus = tm_map(tmpCorpus, content_transformer(stripWhitespace), lazy=TRUE)
  # lowercase
  tmpCorpus = tm_map(tmpCorpus, content_transformer(tolower), lazy=TRUE)
  # remove punctuation
  tmpCorpus = tm_map(tmpCorpus, content_transformer(removePunctuation), lazy=TRUE)
  # remove numbers
  tmpCorpus = tm_map(tmpCorpus, content_transformer(removeNumbers), lazy=TRUE)
  # remove stopwords - remove comman words that you don't care about
  # in this case we want to remove tesco etc
  #stopWordsAdd = c("tesco", "tescos", "uktesco", "tescouk", "amp", "httptc", "dunnhumby")
  stopWordsAdd = c("amp", "httptcotpnqpjaudy", "httptcoiuinjtl", "moneysupermarket", "moneysupermkt")
  stopWordsFinal = c(stopwords('english'), stopWordsAdd)
  
  tmpCorpus = tm_map(tmpCorpus, content_transformer(removeWords), stopWordsFinal, lazy=TRUE)

  #tmpCorpus[[1]]

  # next we creat a term-document matrix: limit to count of 2 going in
  tmpTdm = TermDocumentMatrix(tmpCorpus, 
                              control = list(bounds = list(global = c(2,Inf)), minWordLength = 1))
  
  ## what terms are most frequent
  # create count matrix
  #spAll = sparseMatrix(i=tmpTdm$i, j=tmpTdm$j, x=tmpTdm$v)
  #sp2All = sparseMatrix(i=myDtm2All$i, j=myDtm2All$j, x=myDtm2All$v)
  counts = sort(row_sums((tmpTdm)), decreasing=TRUE)
  freqMat = data.frame(word=names(counts), freq=counts)
  freqMatSort = freqMat[with(freqMat, order(-counts)), ]
  freqMatSort

  # lets visualise

  library(wordcloud)
   fCut = 3
  wordcloud(freqMat$word, freqMat$freq, min.freq=fCut, scale=c(4, 0.2), random.order=FALSE, 
          rot.per=0.15, colors=brewer.pal(8,"Dark2"))

## quick look at simple sentiment scoring

  # run sentiment - very simple sentiment scoring
  # load in positive words and negative words - and count occurances
  negWords = scan(paste(data_dir,"/negative-words.txt", sep=""), what="character", comment.char=";")
  posWords = scan(paste(data_dir,"/positive-words.txt", sep=""), what="character", comment.char=";")

  # calculate which words in each document are positive or negative
  tmpPosIdx = which(!is.na(match(tmpTdm$dimnames$Terms, posWords)))  
  tmpNegIdx = which(!is.na(match(tmpTdm$dimnames$Terms, negWords)))  
  
  # calulcate simple sentiment by adding up the positive words and subtracting the negative ones
  tgtTweets$sentScore = laply(1:tmpTdm$ncol, function(x) {sum(tmpTdm[tmpPosIdx, x]) - sum(tmpTdm[tmpNegIdx, x])})


  # add in date and timedocdate
  tgtTweets$dateTimes = as.POSIXlt(tgtTweets$created_at,"%a %b %d %H:%M:%S %z %Y", tz="GMT")
  tgtTweets$dates = as.Date(tgtTweets$dateTimes)
  tgtTweets$times = strftime(tgtTweets$dateTimes, format="%H:%M:%S")

## visualise with ggplot

  # quick ggplot of tweets over time
  library(ggplot2)

  names(tgtTweets)
  # tweets over time by retweets
  ggplot(tgtTweets, aes(dateTimes, retweet_count)) + 
    geom_point()

  # add in size = followers
  ggplot(tgtTweets, aes(dateTimes, retweet_count)) + 
    geom_point(size = log(tgtTweets$followers_count)/4)


  # add in colour of sentiment
  ggplot(tgtTweets, aes(dateTimes, retweet_count, colour = sentScore)) + 
    geom_point(size = log(tgtTweets$followers_count)/4)


# adjust colour scale and clean up titles
  ggplot(tgtTweets, aes(dateTimes, log(retweet_count), colour = sentScore)) + 
    geom_point(size = log(tgtTweets$followers_count)/2) +
    scale_colour_gradient(low = "red",
                        high = "green", space = "Lab", na.value = "grey50",
                        guide = "colourbar") +
    scale_y_continuous(name = "log retweet count") 
  


## end
####################################################

