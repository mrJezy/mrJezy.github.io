---
title: "Wordcloud"
date: 2018-04-07
tags: [wordcloud, text mining, NLP, news]
header:
  image: "/images/bckg.png"
excerpt: "Wordcloud, Text Mining, NLP, news"
---

I wrote a shiny app to extract the words from leading hungarian online news sites and plot a wordcloud from them. You can find the application here: [Wordcloud](https://datafitness.shinyapps.io/wordcloud/)

Note: the application's language is **Hungarian**!

## The code for the application:

### ui.R

{% highlight r %}
# Define the UI
#libraries
library(shiny)
library(rvest)
library(dplyr)
library(tm)
library(jsonlite)
library(corpus)
library(hunspell)
library(wordcloud2)
library(extrafont)
loadfonts()

fluidPage(
        pageWithSidebar(
                # Application title
                headerPanel("Magyar hírportálok címlapjainak szófelhői"),
                # Sidebar with inputs and buttons
                sidebarPanel(
#                        htmlOutput("valami", inline = TRUE),
#                        tags$hr(),
                        selectInput('data', "Hírportál:",
                                    c("24.hu" = "24.hu",
                                      "444" = "444",
                                      "888" = "888",
                                      "index" = "index",
                                      "Magyar Nemzet Online" = "Magyar Nemzet Online",
                                      "Origo" = "Origo")),
                        sliderInput('size', 'Betűméret', min = 0.2, max = 1,
                                    value = 0.4,step = 0.1),
                        actionButton("update_h4", "24.hu frissítése"),
                        actionButton("update_n", "444 frissítése"),
                        actionButton("update_ny", "888 frissítése"),
                        actionButton("update_i", "index frissítése"),
                        actionButton("update_mno", "Magyar Nemzet Online frissítése"),
                        actionButton("update_o", "Origo frissítése")),
                # Main Panel
                mainPanel(
                        #tags$head(
                        #        tags$style(HTML('div#wcLabel {display: none;}'))
                        #),
                        wordcloud2Output('wordcloud2',height = "500px"))
        )
)
{% endhighlight %}

### server.R

{% highlight r %}
# Hungarian stopword dictionary
hu_stopwords <- fromJSON('https://raw.githubusercontent.com/6/stopwords-json/master/dist/hu.json')

# Set the dictionary path:
# setwd('/Users/mac/Desktop/Data Science/Pet Projects/Online Media Frontpage Wordcloud/')
hun <- dictionary("hu_HU.dic")
#print(hun)

# Custom functions

# Stemming
myStem <- function(x) {
        res <- hunspell_stem(x, dict = hun)
        idx <- which(lengths(res)==0)
        if (length(idx)>0){
                res[idx] <- x[idx]
        }
        sapply(res, tail, 1)
}
# Content transformer for tm_map
hunstemmer <- content_transformer(function(txt){paste(myStem(strsplit(txt, " ")[[1]]))})

# Text processing
text_proc <- function(x) {
        # Create corpus
        temp <- VCorpus(VectorSource(x))
        # Remove Punctation, Numbers
        temp <- tm_map(temp, removePunctuation)
        temp <- tm_map(temp, removeNumbers)
        # Remove Whitespace
        temp <- tm_map(temp, stripWhitespace)
        # Stem words
        temp <- tm_map(temp, hunstemmer)
        # Remove Stopwords
        temp <- tm_map(temp, removeWords, hu_stopwords)
        # Create Term Document Matrix
        TDM <- TermDocumentMatrix(temp, control = list(wordLengths=c(2,Inf)))
        TDM_mat <- as.matrix(TDM)
        # Create frequencies of words
        TDM_freq <- sort(rowSums(TDM_mat), decreasing=TRUE)
        result <- data.frame(word = names(TDM_freq), freq=TDM_freq)
        rm(temp, TDM, TDM_freq)
        return(result)
}

# Web Scraping ORIGO, index.hu, 444.hu, 24.hu, mno.hu, 888.hu
get_o <- function(){
        # ORIGO
        o_url <- read_html('http://www.origo.hu/index.html')
        o_cikk <- o_url %>% html_nodes(".news-text-block h2, p") %>% html_text()
        o_words <- text_proc(o_cikk)
        return(o_words)
}
get_i <- function(){
        # index.hu
        i_url <- read_html('http://www.index.hu')
        i_cikk <- i_url %>% html_nodes(".cikkcim a, .ajanlo span") %>% html_text()
        i_words <- text_proc(i_cikk)
        return(i_words)
}
get_n <- function(){
        # 444.hu
        n_url <- read_html('http://www.444.hu')
        n_cikk <- n_url %>% html_nodes("h1.title, h4 a, p") %>% html_text()
        n_words <- text_proc(n_cikk)
        return(n_words)
}
get_h4 <- function(){
        # 24.hu
        h4_url <- read_html('http://www.24.hu')
        h4_cikk <- h4_url %>% html_nodes(".post-title, .lead-after-wrap p") %>% html_text()
        h4_words <- text_proc(h4_cikk)
        return(h4_words)
}
get_mno <- function(){
        # mno.hu
        mno_url <- read_html('http://www.mno.hu')
        mno_cikk <- mno_url %>% html_nodes(".content h3, .lead p") %>% html_text()
        mno_words <- text_proc(mno_cikk)
        return(mno_words)
}
get_ny <- function(){
        # 888.hu
        ny_url <- read_html('http://www.888.hu')
        ny_cikk <- ny_url %>% html_nodes(".fig-wrap h2, .col-holder h2,
                                         .text-box p, .text-wrap h2, .text-wrap p") %>% html_text()
        ny_words <- text_proc(ny_cikk)
        return(ny_words)
}

# Define mapping table
media_fonts <- data.frame(t(c('Titillium-Bold','HelveticaNeue','Roboto-Bold','OpenSans-Regular','Publico-Medium','SourceSansPro-Bold')),
                          stringsAsFactors = F)
#media_fonts <- data.frame(t(test), stringsAsFactors = F)
dfs <- data.frame(t(c('h4_words','n_words', 'ny_words', 'i_words', 'mno_words', 'o_words')),
                  stringsAsFactors = F)
color <- data.frame(t(c('#fff','#222','#fff','#323232','#000','#fff')),
                    stringsAsFactors = F)
bcolor <- data.frame(t(c('#64667a','#ffff73','#e01365','#fff','#DCEBF0','#155092')),
                     stringsAsFactors = F)
last_update <- data.frame(t(c('24.hu','444.hu','888.hu','index.hu','MNO','Origo')),
                          stringsAsFactors = F)
keys <- rbind(dfs,media_fonts,color,bcolor,last_update)
colnames(keys) <- c("24.hu", "444", "888", "index", "Magyar Nemzet Online", "Origo")

# Define the server code
function(input, output, session) {
        # Progress bar
        progress <- Progress$new(session, min=1, max=7)
        progress$set(message = 'Adatok begyűjtése',
                     detail = 'Ez eltarthat egy ideig...')
        progress$set(value = 1)
        o_words <- get_o()
        progress$set(value = 2)
        i_words <- get_i()
        progress$set(value = 3)
        n_words <- get_n()
        progress$set(value = 4)
        h4_words <- get_h4()
        progress$set(value = 5)
        mno_words <- get_mno()
        progress$set(value = 6)
        ny_words <- get_ny()
        progress$set(value = 7)
        progress$close()
        #output$valami <- renderUI({HTML("text()")})
        output$wordcloud2 <- renderWordcloud2({
                wordcloud2(data=get(keys[1,input$data]), size=input$size,
                           color = keys[3,input$data],
                           backgroundColor = keys[4,input$data],
                           fontFamily = keys[2,input$data])
        })
        observeEvent(input$update_h4, {
                progress <- Progress$new(session, min=1, max=2)
                progress$set(message = 'Adatok begyűjtése',
                             detail = 'Ez eltarthat egy ideig...')
                progress$set(value = 1)
                get_h4()
                keys[5,1] <- format(Sys.time(), "%Y. %m. %d. %H:%M:%S")
                progress$set(value = 2)
                progress$close()
        })
        observeEvent(input$update_n, {
                progress <- Progress$new(session, min=1, max=2)
                progress$set(message = 'Adatok begyűjtése',
                             detail = 'Ez eltarthat egy ideig...')
                progress$set(value = 1)
                get_n()
                keys[5,2] <- format(Sys.time(), "%Y. %m. %d. %H:%M:%S")
                progress$set(value = 2)
                progress$close()
        })
        observeEvent(input$update_ny, {
                progress <- Progress$new(session, min=1, max=2)
                progress$set(message = 'Adatok begyűjtése',
                             detail = 'Ez eltarthat egy ideig...')
                progress$set(value = 1)
                get_ny()
                keys[5,3] <- format(Sys.time(), "%Y. %m. %d. %H:%M:%S")
                progress$set(value = 2)
                progress$close()
        })
        observeEvent(input$update_i, {
                progress <- Progress$new(session, min=1, max=2)
                progress$set(message = 'Adatok begyűjtése',
                             detail = 'Ez eltarthat egy ideig...')
                progress$set(value = 1)
                get_i()
                keys[5,4] <- format(Sys.time(), "%Y. %m. %d. %H:%M:%S")
                progress$set(value = 2)
                progress$close()
        })
        observeEvent(input$update_mno, {
                progress <- Progress$new(session, min=1, max=2)
                progress$set(message = 'Adatok begyűjtése',
                             detail = 'Ez eltarthat egy ideig...')
                progress$set(value = 1)
                get_mno()
                keys[5,5] <- format(Sys.time(), "%Y. %m. %d. %H:%M:%S")
                progress$set(value = 2)
                progress$close()
        })
        observeEvent(input$update_o, {
                progress <- Progress$new(session, min=1, max=2)
                progress$set(message = 'Adatok begyűjtése',
                             detail = 'Ez eltarthat egy ideig...')
                progress$set(value = 1)
                get_o()
                keys[5,6] <- format(Sys.time(), "%Y. %m. %d. %H:%M:%S")
                progress$set(value = 2)
                progress$close()
        })
}
{% endhighlight %}
