### Graph and Sentiment Analysis of youtube actor
!https://github.com/IrinaMax/Graph-and-Sentiment-Analysis-on-youtube-scraping-data/issues/4#issue-420336748

## Jennifer Jenkins video's analises
this script was created for my frind on You tube to implement her network and Sentiment Analysis of the comments of some of her video    
     
     #the links id is   pZvf2DsYGzE, BZUtM-McyJk, WUZehy-2eHc, ky_1LIZWPDI, XCvqwFInXes&t=942s
     setwd( "/Users/irinamahmudjanova/Documents/STUDY/DATA_SCIENCE")
    ### Getting YouTube data

     library(vosonSML)
     # gogl developer API key

     # Use this key in your application by passing it with the key=API_KEY parameter.

    apikey <- "xxx this must be my key"

    key <- AuthenticateWithYoutubeAPI(apiKeyYoutube=apikey)


    ## Collecting data using youtube  
    videojj <- c('pZvf2DsYGzE', 'BZUtM-McyJk', 'WUZehy-2eHc', 'ky_1LIZWPDI', 'XCvqwFInXes&t=942s')
    ytjj <- CollectDataYoutube(videojj, key, writeToFile = FALSE) # depricated look at the end of the file for update
    ytjj %>% str
    write.csv(ytjj, '/Users/irinamahmudjanova/Documents/STUDY/DATA_SCIENCE/DataCamp_data/ytjj.csv', row.names = F)

After we collect data we save it and it can be used later as well

    
    data <- read.csv("ytjj.csv", header = T)
   
    data <- youtubeData   ## the new version
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
![rplot](https://user-images.githubusercontent.com/16123495/53720427-9960bd00-3e15-11e9-8437-e890f3dd64ab.png)
   
Let's do Sentiment analysis for this comments
     
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

We can create a siple barplot or colerful
  
     barplot(100*colSums(s))
     barplot(100*colSums(s)/sum(s),
        las = 2,
        col = rainbow(10),
        ylab = "Percentage",
        main = "Sentiment Scores For YouTube Jenifer's video Comments")
        
![jenifer_barplot_sentiment](https://user-images.githubusercontent.com/16123495/53720513-ce6d0f80-3e15-11e9-9d9a-93f350e8a788.png)

    jj_youtube <- CreateActorNetwork(ytjj, writeToFile = FALSE)
    jj_youtube

    s$comments <- comments
    s %>% str
    print( strtrim(comments, 20))




 after update in the vosonSML library I revise my code 

    # create a list of youtube video ids to collect on
    apikey <- "xxxthis is my key"
     videojj <- c('pZvf2DsYGzE', 'BZUtM-McyJk', 'WUZehy-2eHc', 'ky_1LIZWPDI', 'XCvqwFInXes&t=942s')
   
    #next scrip does the same as CollectDataYoutube
    # we use the same apikey as used previously 
     youtubeData <- Authenticate("youtube", apiKey = apikey) %>% 
           Collect(videoIDs = videojj, writeToFile = TRUE, verbose = FALSE, maxComments = 200)
  
     youtubeData %>% str
     write.csv(youtubeData, '/Users/irinamahmudjanova/Documents/STUDY/DATA_SCIENCE/ytjj_2019.csv', row.names = F)


     
     actorNetwork <- Authenticate("youtube", apiKey = apikey) %>%
         Collect(videoIDs = videojj) %>%
         Create("actor", writeToFile = TRUE)
         actorNetwork %>% str
         
Colorful d3heatmap plot of sentiment, based on clustering of interacted people   
         
     #install.packages("d3heatmap")
     library(d3heatmap)
     d3heatmap(s[1:100,], scale = "column")
     
  ![JJ_youtube_hitmap](https://user-images.githubusercontent.com/16123495/54256330-e250f980-4518-11e9-8e84-dc7b4e7254af.png)

You also can present top best of more interested sentiment         

     d3heatmap(s[1:100,], scale = "column")
