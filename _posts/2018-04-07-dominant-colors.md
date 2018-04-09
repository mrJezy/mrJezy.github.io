---
title: "Dominant Colors extraction from Movie Posters"
date: 2018-04-07
tags: [movies, dominant colors, k-means]
header:
  image: "/images/bckg.png"
excerpt: "Movies, Dominant Colors, k-means"
classes: wide
---

I wrote a shiny app to extract dominant colors from movie posters, using k-means clustering. You can find the application here: [Dominant Colors](https://datafitness.shinyapps.io/dominant_colors/)

## The code for the application:

### ui.R

{% highlight r %}
# Define the UI
# Loading the required libraries
library(jpeg)
library(reshape2)
library(ggplot2)
library(colorspace)
library(treemapify)
library(rvest)
library(shinyWidgets)

fluidPage(
        # Application title
        fluidRow(
                h1('Dominant Colors of Movie Posters using k-means clustering')
        ),
        fluidRow(
                # Sidebar with inputs and buttons
                column(4,
                        h4(searchInput(
                                inputId = "search",
                                label = "Search movie by title:",
                                placeholder = "Type in title",
                                btnSearch = icon("search"),
                                btnReset = icon("remove"),
                                width = "100%")
                        ),
                       h4(sliderInput('n_colors', 'Number of colors:', min = 4, max = 12,
                                   value = 8,step = 1)),
                       actionButton("get_dc", "Get dominant colors")),
                # Main Panel
                column(4,
                        h4(textOutput("selected_movie")),
                        imageOutput("display_poster")
                ),
                column(4,
                       h4(textOutput("dominant_colors")),
                       plotOutput("dc_treemap"))
        )
)
{% endhighlight %}


### server.R

{% highlight r %}
# Functions

# Image loader helper function
imageLoader <- function(url){  # This function takes a URL, and generates a data.frame with pixel locations and colors
        # Download to disk, load
        download.file(url, "tempPicture.jpg", mode = "wb")  # Stash image locally
        readImage <- readJPEG("tempPicture.jpg")

        longImage <- melt(readImage)
        rgbImage <- reshape(longImage, timevar = "Var3",
                            idvar = c("Var1", "Var2"), direction = "wide")
        rgbImage$Var1 <- -rgbImage$Var1
        return(rgbImage)
}

# Colors to HEX function
col2hex <- function(data){
        if (class(data)=='numeric') {
                return(hex(sRGB(data[1],data[2],data[3])))
        } else if (class(data)==('character')) {
                return('Only numeric vector or data.frame input allowed!')
        } else {
                return(apply(data,1,function(x) hex(sRGB(x[1],x[2],x[3]))))
        }
}

# Colors to HEX color then plot a color palette function
plt.col <- function(colpal,nk){
        i_palette <- col2hex(colpal)
        palplot <- ggplot(as.data.frame(matrix(c(1:nk,rep(1,each=nk)),nrow=nk,ncol=2))) +
                theme_bw() +
                theme(axis.line = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank()) +
                xlim(c(0, 1)) +
                ylim(c(0, nk))
        for(i in 1:length(i_palette)){
                palplot <- palplot + geom_rect(xmin = 0, xmax = 0.2,   ymin = 0+i-1, ymax = i,   fill = i_palette[i])
        }
        return(palplot)
}

# Image source function
get_img <- function(title){
        title_search <- gsub(" ","+",title)
        search_url <- read_html(paste0('https://www.themoviedb.org/search?query=',title_search))
        movie_img <- search_url %>% html_nodes("img.poster") %>% html_attr('data-src')
        movieposter <- paste0('https://image.tmdb.org/t/p/w300_and_h450_bestv2/',gsub('.*/','',movie_img[1]))
        return(movieposter)
}

# Define the server code
function(input, output, session) {
        output$selected_movie <- renderText({
                if(input$search==""){
                        paste("The poster of the movie: ")
                } else{
                        paste("The poster of the movie: ")
                }
        })
        output$display_poster <- renderImage({
                if(input$search==""){
                        list(src = "",
                             height = 450,
                             width = 300)
                } else{
                        download.file(get_img(input$search),
                                      destfile = "poster.jpg")
                        list(src = "poster.jpg",
                             height = 450,
                             width = 300)
                }
        })
        output$dominant_colors <- renderText({
                paste("Dominant colors: ")
        })
        observeEvent(input$get_dc, {
                movieposter <- get_img(input$search)
                n_colors <- input$n_colors
                output$dc_treemap <- renderPlot({
                        if(movieposter==""){}
                        else{
                                # K-Means most dominant colors
                                rgbImage <- imageLoader(movieposter)  # Pick one, or use your own URL.
                                kColors <- n_colors  # Number of palette colors
                                set.seed(42)
                                kMeans <- kmeans(rgbImage[, 3:5], nstart = 25, centers = kColors, iter.max=1000, algorithm="Lloyd")
                                domColors <- kMeans$centers
                                i_palette <- col2hex(domColors)
                                i_palette
                                treemap <- as.data.frame(cbind(i_palette,table(kMeans$cluster)))
                                treemap$i_palette <- as.character(treemap$i_palette)
                                treemap$V2 <- as.numeric(levels(treemap$V2))
                                ggplot(treemap, aes(area = V2, label = i_palette)) +
                                        geom_treemap(fill = i_palette) +
                                        geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                                                          grow = TRUE)
                        }
                })
        })
}
{% endhighlight %}
