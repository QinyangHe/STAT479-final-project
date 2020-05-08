if (!require("DT")) {
  install.packages("DT") # do this once per lifetime
  stopifnot(require("DT")) # do this once per session
}
if (!require("wordcloud")) {
  install.packages("wordcloud") # do this once per lifetime
  stopifnot(require("wordcloud")) # do this once per session
}
if (!require("rtweet")) {
  install.packages("rtweet") # do this once per lifetime
  stopifnot(require("rtweet")) # do this once per session
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets") # do this once per lifetime
  stopifnot(require("shinyWidgets")) # do this once per session
}
if (!require("tidytext")) {
  install.packages("tidytext") # do this once per lifetime
  stopifnot(require("tidytext")) # do this once per session
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard") # do this once per lifetime
  stopifnot(require("shinydashboard")) # do this once per session
}
if (!require("dplyr")) {
  install.packages("dplyr") # do this once per lifetime
  stopifnot(require("dplyr")) # do this once per session
}
if (!require("shinyjs")) {
  install.packages("shinyjs") # do this once per lifetime
  stopifnot(require("shinyjs")) # do this once per session
}
if (!require("forestmangr")) {
  install.packages("forestmangr") # do this once per lifetime
  stopifnot(require("forestmangr")) # do this once per session
}
if (!require("syuzhet")) {
  install.packages("syuzhet") # do this once per lifetime
  stopifnot(require("syuzhet")) # do this once per session
}
library(shiny)
library(rtweet)
library(ggplot2)
ui <- dashboardPage(
  # App title ----
  dashboardHeader(
    title = "Tweets Visualizer"),
  
  # Sidebar layout with input and output definitions ----
  dashboardSidebar(
    useShinyjs(),
      div(
        id = "resettable",
        textInput(inputId = "username",
                  label = "Analyse a user account",
                  placeholder = "enter a user account"
        ),
        actionButton(inputId = "addAccount",
                     label = "Analyze"
        ),
        textInput(inputId = "hashtag",
                  label = "Analyse a hashtag",
                  placeholder = "enter a hashtag"
        ),
        actionButton(inputId = "addHashtag",
                     label = "Analyze"
        )
    ),
    
    sidebarMenu(id = "tabs",
      menuItem("Original Text", tabName = "text", icon = icon("th")),
      menuItem("Wordcloud", tabName = "wordcloud", icon = icon("dashboard")),
      menuItem("Word Frequency Chart", tabName = "frequency", icon = icon("th")),
      menuItem("Frequency of tweets", tabName = "frequency_tweets", icon = icon("th")),
      menuItem("Tweeting Platform", tabName = "tweetPlatform", icon = icon("th")),
      menuItem("Sentiment analysis", tabName = "sentiment", icon = icon("th"))
      
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      # First tab content
      tabItem(tabName = "wordcloud",
              plotOutput("wordcloud")
              
      ),
      
      # Second tab content
      tabItem(tabName = "frequency",
            plotOutput("frequency_plot")
      ),
      
      tabItem(tabName = "text",
        dataTableOutput("tweetFrame")  
      ),
      
      tabItem(tabName = "frequency_tweets",
              plotOutput("tweets_frequency")  
      ),
      
      tabItem(tabName = "tweetPlatform",
              plotOutput("platformChart")),
      
      tabItem(tabName = "sentiment",
              plotOutput("sentiment_plot"))
    )
  )
)

server <- function(input, output) {
  
  get_hashtage = eventReactive({input$addHashtag
                             },{
      search_tweets(q = input$hashtag, n = 1000)
                             
  })
  
  get_account= eventReactive({input$addAccount
  },{
    tweetFrame = get_timeline(user = input$username, n = 1000)
  })

  
  observeEvent({input$addHashtag}, {
      tweetFrame = get_hashtage()
      #construct output for original text
      output$tweetFrame = renderDataTable({
        datatable(data.frame(user_id = tweetFrame$user_id,
                             tweet_time = tweetFrame$created_at,
                             screen_name = tweetFrame$screen_name,
                             source = tweetFrame$source,
                             text = tweetFrame$text))
      })
      
      #construct wordcloud
      tweets_text = as.character(tweetFrame$text)
      tweets_text <- gsub("c\\(", "", tweets_text)
      set.seed(1234)
      output$wordcloud = renderPlot({
        wordcloud(tweets_text, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
      })
      
      #construct word frequency chart
      tweets_words = get_hashtage()%>%
        select(text) %>%
        unnest_tokens(word, text)
      print("word1")
      tweets_words <- tweets_words %>%
        anti_join(stop_words)
      output$frequency_plot = renderPlot({
        print("word2")
        tweets_words %>% 
          count(word, sort = TRUE) %>%
          top_n(15) %>%
          mutate(word = reorder(word, n)) %>%
          ggplot(aes(x = word, y = n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          labs(y = "Count",
               x = "Unique words",
               title = "Most frequent words found in the tweets",
               subtitle = "Stop words removed from the list")
      })
      
      #construct time frequency
      tweets_freq = tweetFrame
      output$tweets_frequency = renderPlot({
        ts_plot(tweets_freq, "minute") +
          ggplot2::theme_minimal() +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
          ggplot2::labs(
            x = NULL, y = NULL,
            title = paste("Time frequency of tweets regarding hastage #", input$hashtag),
            subtitle = "Twitter status (tweet) counts aggregated using one minute intervals",
            caption = "\nSource: Data collected from Twitter's REST API via rtweet")
      })
      
      #construct tweetPlatform
      platform <- tweetFrame %>% 
        select(source) %>% 
        group_by(source) %>%
        summarize(count=n())
      platform <- subset(platform, count > 10)
      
      data <- data.frame(
        category=platform$source,
        count=platform$count
      )
      data$fraction = data$count / sum(data$count)
      data$percentage = data$count / sum(data$count) * 100
      data$ymax = cumsum(data$fraction)
      data$ymin = c(0, head(data$ymax, n=-1))
      data <- round_df(data, 2)
      Source <- paste(data$category, data$percentage, "%")
      output$platformChart = renderPlot({
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
          geom_rect() +
          coord_polar(theta="y") + 
          xlim(c(2, 4)) +
          theme_void() +
          theme(legend.position = "right"
        )
      })
      
      #show sentiment analysis
      # Converting tweets to ASCII to trackle strange characters
      tweets <- iconv(tweetFrame$text, from="UTF-8", to="ASCII", sub="")
      print("here1")
      # removing retweets, in case needed 
      tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
      # removing mentions, in case needed
      tweets <-gsub("@\\w+","",tweets)
      ew_sentiment<-get_nrc_sentiment((tweets))
      sentimentscores<-tibble(colSums(ew_sentiment[,]))
      names(sentimentscores) <- "Score"
      sentimentscores <- cbind("sentiment"=colnames(ew_sentiment),sentimentscores)
      rownames(sentimentscores) <- NULL
      print("here2")
      output$sentiment_plot = renderPlot({
        ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
          geom_bar(aes(fill=sentiment),stat = "identity")+
          theme(legend.position="none")+
          xlab("Sentiments")+ylab("Scores")+
          ggtitle("Total sentiment based on scores")+
          theme_minimal()
      })
      
    })
  
  
  observeEvent({input$addAccount}, {
    tweetFrame = get_account()
    output$tweetFrame = renderDataTable({
      datatable(data.frame(user_id = tweetFrame$user_id,
                           tweet_time = tweetFrame$created_at,
                           screen_name = tweetFrame$screen_name,
                           source = tweetFrame$source,
                           text = tweetFrame$text))
    })
    
    #construct wordcloud
    tweets_text = as.character(get_account()$text)
    tweets_text <- gsub("c\\(", "", tweets_text)
    print("here5")
    set.seed(1234)
    output$wordcloud = renderPlot({
      wordcloud(tweets_text, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))
    })
    
    #construct word frequency chart
    tweets_words = get_account()%>%
      select(text) %>%
      unnest_tokens(word, text)
    print("word1")
    tweets_words <- tweets_words %>%
      anti_join(stop_words)
    output$frequency_plot = renderPlot({
      print("word2")
      tweets_words %>% 
        count(word, sort = TRUE) %>%
        top_n(15) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(x = word, y = n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip() +
        labs(y = "Count",
             x = "Unique words",
             title = "Most frequent words found in the tweets",
             subtitle = "Stop words removed from the list")
    })
    
    #construct time frequency
    tweets_freq = get_account()
    colnames(tweets_freq)[colnames(tweets_freq)=="screen_name"] <- "Twitter_Account"
    output$tweets_frequency = renderPlot({
      ts_plot(dplyr::group_by(tweets_freq, Twitter_Account), "month", group = 1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = paste("Time frequency of", input$username, "'s tweets"),
          subtitle = "Tweet counts aggregated by year",
          caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
    })
    
    #construct tweetPlatform
    platform <- tweetFrame %>% 
      select(source) %>% 
      group_by(source) %>%
      summarize(count=n())
    platform <- subset(platform, count > 10)
    
    data <- data.frame(
      category=platform$source,
      count=platform$count
    )
    data$fraction = data$count / sum(data$count)
    data$percentage = data$count / sum(data$count) * 100
    data$ymax = cumsum(data$fraction)
    data$ymin = c(0, head(data$ymax, n=-1))
    data <- round_df(data, 2)
    Source <- paste(data$category, data$percentage, "%")
    output$platformChart = renderPlot({
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
        geom_rect() +
        coord_polar(theta="y") + 
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "right")
    })
    
    #show sentiment analysis
    # Converting tweets to ASCII to trackle strange characters
    tweets <- iconv(tweetFrame$text, from="UTF-8", to="ASCII", sub="")
    # removing retweets, in case needed 
    tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
    # removing mentions, in case needed
    tweets <-gsub("@\\w+","",tweets)
    ew_sentiment<-get_nrc_sentiment((tweets))
    sentimentscores<-tibble(colSums(ew_sentiment[,]))
    names(sentimentscores) <- "Score"
    sentimentscores <- cbind("sentiment"=colnames(ew_sentiment),sentimentscores)
    rownames(sentimentscores) <- NULL
    output$sentiment_plot = renderPlot({
      ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
        geom_bar(aes(fill=sentiment),stat = "identity")+
        theme(legend.position="none")+
        xlab("Sentiments")+ylab("Scores")+
        ggtitle("Total sentiment based on scores")+
        theme_minimal()
    })
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

#some code references
#https://rstudio.github.io/shinydashboard/
#https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16