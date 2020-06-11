library(shiny)
library(tidyverse)
library(rvest)
library(jsonlite)
library(tidytext)
library(wordcloud)
cleanFun <- function(htmlString) {
    c1 = gsub("<.*?>", "", htmlString)
    c2 = gsub("[\r\n]", "", c1)
    return(c2)
}
ui <- pageWithSidebar(
    
    headerPanel("GitHub Jobs"),
    sidebarPanel(
        selectInput('descr','Description:',
                    c('Python','R','C++','Java','Php')),
        textInput('location','Destination:',
                  'ca'),
        numericInput("lat", "Latitude:", 37.3229978),
        numericInput("long", "Longitude:", -122.0321823),
        checkboxInput("fulltime", "Full time only?", TRUE),
        checkboxInput("usell", "Use longitude and latitude?", FALSE),
        actionButton("search", "Search!")
    ),
    mainPanel(
        plotOutput('wc'),
        tableOutput('loc'),
        verbatimTextOutput("caption")
    )
)

server <- function(input, output) {
    #des <- str_c('description=',input$descr,'&')
    ori = 'https://jobs.github.com/positions.json?'
    output$wc = renderPlot({
        input$search
        tss = isolate({
            des <- str_c('description=',input$descr,'&')
            loc = str_c('location=',input$location,'&')
            lats = str_c('lat=',input$lat,'&')
            longs = str_c('long=',input$long,'&')
            if(input$usell){
                ts = str_c(ori,lats,longs,des)
            }else{ts = str_c(ori,des,loc)}
            if(input$fulltime){
                ts=str_c(ts,'full_time=true&')
            }
            b = fromJSON(ts)
            descr = cleanFun(b$description)
            df = tibble(line=1:length(descr),descr = descr)
            df  %>% 
                unnest_tokens(word, descr) %>% 
                anti_join(stop_words) %>%
                count(word, sort = TRUE)%>% 
                with(wordcloud(
                    word, n, min.freq = 2, max.words = 100, random.order = FALSE,
                    colors = brewer.pal(8, "Dark2")))
            
        })
        tss
    })
    output$loc <- renderTable({
        input$search
        tb = isolate({
            des <- str_c('description=',input$descr,'&')
            loc = str_c('location=',input$location,'&')
            lats = str_c('lat=',input$lat,'&')
            longs = str_c('long=',input$long,'&')
            if(input$usell){
                ts = str_c(ori,lats,longs,des)
            }else{ts = str_c(ori,des,loc)}
            if(input$fulltime){
                ts=str_c(ts,'full_time=true&')
            }
            b = fromJSON(ts)
            tibble('Job type'=b$type,'company'=b$company,'location'=b$location,
                   'Title'=b$title,'Apply'=cleanFun(b$how_to_apply))
        })
        tb
        
    })
    output$caption = renderText({
        des <- str_c('description=',input$descr,'&')
        loc = str_c('location=',input$location,'&')
        lats = str_c('lat=',input$lat,'&')
        longs = str_c('long=',input$long,'&')
        if(input$usell){
            ts = str_c(ori,lats,longs,des)
        }else{ts = str_c(ori,des,loc)}
        if(input$fulltime){
            ts=str_c(ts,'full_time=true&')
        }
        ts
    })
    
    
}

shinyApp(ui = ui, server = server)
