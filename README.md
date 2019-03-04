# Graph-Analysis-on-youtube-scaring-data
# Jennifer Jenikins video's analises
#
#  pZvf2DsYGzE, BZUtM-McyJk, WUZehy-2eHc, ky_1LIZWPDI, XCvqwFInXes&t=942s
setwd( "/Users/irinamahmudjanova/Documents/STUDY/DATA_SCIENCE/DataCamp_data")
### Getting YouTube data
# install.packages("vosonSML") https://github.com/vosonlab/vosonSML
library(vosonSML)
# gogl developer API key

# Use this key in your application by passing it with the key=API_KEY parameter.
#   https://console.cloud.google.com/apis/credentials?project=marine-pillar-192807

apikey <- "AIzaSyByeVdhmwfgwQ3kVfGqDdziydQuF4TMbf0"

key <- AuthenticateWithYoutubeAPI(apiKeyYoutube=apikey)


## Collecting data using youtube  hartbr S_CYdTmj7lA

# ledzep heart https://www.youtube.com/watch?v=EUFGJvYlPFk,  living loving https://www.youtube.com/watch?v=fSM9zc3SsuM, when the lavee break https://www.youtube.com/watch?v=FFDYuO53BUk, black dog https://www.youtube.com/watch?v=yBuub4Xe1mw, lemonsong https://www.youtube.com/watch?v=Zyhu2ysqKGk,
videojj <- c('pZvf2DsYGzE', 'BZUtM-McyJk', 'WUZehy-2eHc', 'ky_1LIZWPDI', 'XCvqwFInXes&t=942s')
ytjj <- CollectDataYoutube(videojj, key, writeToFile = FALSE) # depricated look at the end of the file for update
ytjj %>% str
#write.csv(ytjj, '/Users/irinamahmudjanova/Documents/STUDY/DATA_SCIENCE/DataCamp_data/ytjj.csv', row.names = F)



videoBach <- c('LL3qLut37vukafIcURjd3PaA', 'MhGnJL4DDhg')
yt_Bach <- CollectDataYoutube(videoBach, key, writeToFile = FALSE)
write.csv(yt_Bach, '/Users/irinamahmudjanova/Documents/STUDY/DATA_SCIENCE/DataCamp_data/yt_Bach.csv', row.names = F)

yt2 %>% str
write.csv(yt2, '/Users/irinamahmudjanova/Documents/STUDY/DATA_SCIENCE/DataCamp_data/ytSchitts.csv', row.names = F)


data <- read.csv("ytjj.csv", header = T)
#data <- read.csv("yt_Bach.csv", header = T)
data <- youtubeData
data %>% str

data1 <- data[data$ReplyToAnotherUser !=FALSE, ]
data1 %>% str

y <- data.frame(data1$User, data1$ReplyToAnotherUser)
y
# create user network
library(igraph)
net <-  graph.data.frame(y, directed = T)
net <- simplify(net)
net
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree<- degree(net)

# Histogram of node degree
hist(V(net)$degree,
     col = 'green',
     main = "Histogram of the node degree",
     ylab = "Frequency",
     xlab = 'Degree of Vertices'
)

plot(net,
     vertex.size = 0.2*V(net)$degree,
     edge.arrow.size = 0.3,
     vertex.label.cex = 0.01*V(net)$degree)

# install.packages("syuzhet")
library(syuzhet)

comments <- iconv(data$Comment, to = 'utf-8-mac')
comments %>% str
# Obtain sentiment scores
s <- get_nrc_sentiment(comments)
s %>% head
s %>% str
#Create the neutral
s$neautral <- ifelse(s$negative + s$positive ==0, 1, 0)
s %>% head(10)
s$anger


barplot(100*colSums(s))
barplot(100*colSums(s)/sum(s),
        las = 2,
        col = rainbow(10),
        ylab = "Percentage",
        main = "Sentiment Scores For YouTube Jenifer's video Comments")


jj_youtube <- CreateActorNetwork(ytjj, writeToFile = FALSE)
jj_youtube

s$comments <- comments
s %>% str
print( strtrim(comments, 20))

# only n chars from Right side    последние слова
# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }

# get substring only 30 first chars from text только первые слова
substrLeft <- function(x, n){
  substr(x, nchar(x)-n, n)
}
print(substrLeft(comments, 30))



###############  Update working

# create a list of youtube video ids to collect on
apikey <- "AIzaSyByeVdhmwfgwQ3kVfGqDdziydQuF4TMbf0"
videojj <- c('pZvf2DsYGzE', 'BZUtM-McyJk', 'WUZehy-2eHc', 'ky_1LIZWPDI', 'XCvqwFInXes&t=942s')
myYoutubeVideoIds <- GetYoutubeVideoIDs(c(# all http from youtube
                                          "https://youtu.be/xxxxxxxx"))
#next scrip does the same as CollectDataYoutube
youtubeData <- Authenticate("youtube", apiKey = apikey) %>% 
      Collect(videoIDs = videojj, writeToFile = TRUE, verbose = FALSE, maxComments = 200)
  
youtubeData %>% str
write.csv(youtubeData, '/Users/irinamahmudjanova/Documents/STUDY/DATA_SCIENCE/DataCamp_data/ytjj_2019.csv', row.names = F)


# next script creating list of all ID as actor network
actorNetwork <- Authenticate("youtube", apiKey = apikey) %>%
  Collect(videoIDs = videojj) %>%
  Create("actor", writeToFile = TRUE)
actorNetwork %>% str
