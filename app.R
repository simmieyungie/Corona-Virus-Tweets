#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Load in the required packages
library(scales)
library(reshape2)
library(dplyr)
library(tidyr)
library(wordcloud)
library(tidytext)
library(RColorBrewer)
library(shinydashboard)
library(twitteR)
library(ROAuth)
library(stringr)
library(rsconnect)
library(ggplot2)
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Corona Virus Tweets") 


#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Visit-us", icon = icon("send",lib='glyphicon'))))

#value boxes
frow1 <- fluidRow(
    shinydashboard::valueBoxOutput("value1"),
    shinydashboard::valueBoxOutput("value2"),
    shinydashboard::valueBoxOutput("value3"))

frow3 <- fluidRow(  
    box(
        title = "WordCloud of Words"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("wordcloud", height = "300px")
    ),
    box(
        title = "Top 10 words"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("top10", height = "300px")
    )
)


frow2 <- fluidRow(
    box(
        title = "Top Positive and Negative Words",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("bing", height = "300px")
        
    )
)

#body of dashboard
body <- dashboardBody(frow1, frow2, frow3)




#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body,skin='blue')



# Define server logic required to draw a histogram
server <- function(input, output){
    
    #Save app credentials for access to tweets
    consumerKey <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    
    consumerSecret <- "XXXXXXXXXXXXXXXXXXXXXXXXX"
    
    accessToken <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    
    accessTokenSecret <-  "XXXXXXXXXXXXXXXXXXXXXXXXXXX"
    
    #set up 
    setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
    
    dataInput <- reactive({
        #search twitter
        corona_tweets_1 <- searchTwitter("#coronoavirus", n = 1000, 
                         since = "2020-01-01", resultType = "recent", lang = "en")
        
        #Convert corona_tweets to dataframe 
        corona_tweets_1 <- twListToDF(corona_tweets_1)
        
    })
    
    
    #Create reactive word cloud
    output$wordcloud <- renderPlot({
        ##Word clouds for all tweets
        table_1 <- dataInput()[,1:16] %>% 
            mutate(rowmumber = row_number()) %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word, sort = T) 
        wordcloud(table_1$word,freq = table_1$n, max.words = 100,
                                                   min.freq=1,scale=c(3,.5), 
                                                   random.order = FALSE,rot.per=.5,
                                                   colors = brewer.pal(8, "Dark2"))
    })

    
    #Build value box
    output$value1 <- shinydashboard::renderValueBox({
        n <- dataInput()[,1:16] %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            group_by(word, sentiment) %>% 
            count(word, sentiment, sort = T) %>% 
            ungroup() %>% 
            group_by(sentiment) %>% 
            summarise(n = sum(n)) %>% 
            mutate(n = round(n/sum(n), 2)) %>% 
            filter(sentiment == "positive")
        
        n <- n[,2]
            
        
        shinydashboard::valueBox(paste(n, "%"), subtitle = "Positive Tweets", 
                                 icon = icon("stats", lib ="glyphicon" ), color = "light-blue")
    })
 
    output$value2 <- shinydashboard::renderValueBox({
        n <- dataInput()[,1:16] %>% 
            mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
            mutate(text = tolower(text)) %>% 
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            inner_join(get_sentiments("bing")) %>% 
            group_by(word, sentiment) %>% 
            count(word, sentiment, sort = T) %>% 
            ungroup() %>% 
            group_by(sentiment) %>% 
            summarise(n = sum(n)) %>% 
            mutate(n = round(n/sum(n), 2)) %>% 
            filter(sentiment == "negative")
        
        n <- n[,2]
        
        
        shinydashboard::valueBox(paste(n, "%"), subtitle = "Negative Tweets", 
                                 icon = icon("stats", lib ="glyphicon" ), color = "green")
    })
    
    output$top10 <- renderPlot({
       topwords <-  dataInput()[,1:16] %>% 
            mutate(text = tolower(text)) %>% 
            mutate(text = gsub("rt", "", text)) %>% 
            mutate(text = gsub("https","", text)) %>% 
            mutate(text = gsub("t.co", "", text)) %>% 
            mutate(text = gsub("ppl", "people", text)) %>% 
            mutate(text = gsub("coronoavirus", "coronavirus", text)) %>% 
            mutate(text = gsub("en", "", text)) %>% 
            mutate(rowmumber = row_number()) %>%#mutate row numbers
            mutate(text = str_remove(text, "rt")) %>% 
            unnest_tokens(word, text) %>%  #unnest words
            anti_join(stop_words) %>% #removes stop words
            count(word, sort = T) %>%#count most occuring words
            top_n(10) #select top 10
       
        ggplot(topwords, aes(reorder(word, n), n, fill = word)) + #piped into ggplot
            geom_bar(stat = "identity", show.legend = F) + coord_flip() +
            labs(x = "Word", y = "count", title = "Most words used (Top 10)")
    })
    
    
    output$value3 <- shinydashboard::renderValueBox({
        
        tweets_count <- dataInput()[,1:16] %>% 
            nrow()
        
        
        shinydashboard::valueBox(tweets_count, subtitle = "Total Tweets", 
                                 icon = icon("stats", lib ="glyphicon" ), color = "orange")
    })
    
    output$bing <- renderPlot({
       pos_vs_neg <- req(dataInput())[,1:16] %>% 
           mutate(text = iconv(text, from = "latin1", to = "ASCII")) %>% 
           mutate(text = tolower(text)) %>% 
           mutate(text = gsub("fidelity", " ", text)) %>% 
           unnest_tokens(word, text) %>% 
           anti_join(stop_words) %>% 
           inner_join(get_sentiments("bing")) %>% 
           group_by(word, sentiment) %>% 
           count(word, sentiment, sort = T) %>% 
           ungroup() %>% 
           group_by(sentiment) %>% 
           top_n(10)
    ggplot(pos_vs_neg, aes(reorder(word, n), n, fill = word)) +
           geom_col(show.legend = F) +
           facet_wrap(~sentiment, scales = "free_y") +
           coord_flip() + 
           labs(y = "Count", x = "Words", main = "Positive vs Negative words")
       
            
    })
}

shinyApp(ui = ui, server = server)


