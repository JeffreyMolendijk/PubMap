#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list = ls())

library(shiny)
library(shinythemes)
library(leaflet)
library(easyPubMed)
library(XML)
library(dplyr)
library(maps)
library(shinydashboard)

#Load world cities to match against author affiliation > this dataset already contains lattitude and longitude
data("world.cities")

pmquery = function(key, year, tool, email){
  pmsearch = paste(key," AND ", year, "[PDAT]&tool=", tool, "&email=", email ,sep = "")
  return(pmsearch)
}

chopchop_to_df = function (pubmed_data, included_authors = "all", max_chars = 500, 
          autofill = TRUE, dest_file = NULL, getKeywords = TRUE, encoding = "UTF8") 
{
  if (!included_authors %in% c("all", "first", 
                               "last")) 
    stop("Method is not supported!")
  message("Processing PubMed data ", appendLF = FALSE)
  paper.data <- pubmed_data
  expFields <- c("pmid", "doi", "title", 
                 "abstract", "year", "month", "day", 
                 "jabbrv", "journal", "keywords", "lastname", 
                 "firstname", "address", "email")
  papers.authors.list <- lapply(1:length(paper.data), (function(i) {
    if (length(paper.data) > 50) {
      rep.dot <- as.integer(seq(1, length(paper.data), 
                                length.out = 50))
      if (i %in% rep.dot) 
        message(".", appendLF = FALSE)
    }
    else {
      message(".", appendLF = FALSE)
    }
    art <- paper.data[[i]]
    out <- tryCatch({
      article_to_df(pubmedArticle = art, autofill = autofill, 
                    max_chars = max_chars, getKeywords = getKeywords, 
                    getAuthors = TRUE)
    }, error = function(e) {
      NULL
    })
    if (is.null(out)) {
      out <- data.frame(pmid = NA, doi = NA, title = NA, 
                        abstract = NA, year = NA, month = NA, day = NA, 
                        jabbrv = NA, journal = NA, keywords = NA, lastname = NA, 
                        firstname = NA, address = NA, email = NA)
    }
    if (included_authors == "first") {
      out <- out[1, ]
    }
    else if (included_authors == "last") {
      out <- out[nrow(out), ]
    }
    out2 <- data.frame(rebuild = (1:nrow(out)))
    for (jj in 1:length(expFields)) {
      if (expFields[jj] %in% names(out)) {
        out2[, expFields[jj]] <- out[, expFields[jj]]
      }
      else {
        out2[, expFields[jj]] <- NA
      }
    }
    out2[, -1]
  }))
  message(" done!")
  papers.authors.df <- do.call(rbind, papers.authors.list)
  keep.rw <- apply(papers.authors.df, 1, (function(rw) {
    sum(is.na(rw)) < length(rw)
  }))
  papers.authors.df <- papers.authors.df[keep.rw, ]
  if (!is.null(dest_file)) {
    if (class(dest_file) == "character" & length(dest_file) == 
        1) {
      tryCatch(utils::write.table(papers.authors.df, dest_file, 
                                  fileEncoding = encoding), error = function(e) {
                                    NULL
                                  })
    }
  }
  return(papers.authors.df)
}


#collect pubmed ids using easypubmeds get_pubmed_ids
#function does not allow a maximum list over 100, but the search string can be modified using &retmax=100 to increase to 100.
#the following search finds the top 
#my_entrez_id <- get_pubmed_ids('lipidomics AND "2019"[PDAT]&retmax=10&tool=pubmap&email=testuser1@live.com')
#my_entrez_id <- get_pubmed_ids('lipidomics microbiome AND "2010"[PDAT]:"2019"[PDAT]&retmax=10&tool=pubmap&email=testuser1@live.com')
#my_entrez_id <- get_pubmed_ids('"learning analytics"&tool=pubmap&email=testuser1@live.com')

pmsearch.key = '"learning analytics"'
pmsearch.year = '"2019"'
pmsearch.tool = 'pubmap'
pmsearch.email = 'testuser1@live.com'

search = pmquery(pmsearch.key, pmsearch.year, pmsearch.tool, pmsearch.email)


my_entrez_id <- get_pubmed_ids(search)

my_entrez_id$Count
my_entrez_id$QueryTranslation %>% print(quote = FALSE)

my_entrez_data <- fetch_pubmed_data(my_entrez_id, retmax = 5)

#table_articles_byauth uses articles_to_list as one of the first steps. this would convert a massive list into a single list.
#Instead we can manually convert large my_entrez_data into smaller chunks, e.g.  1:100.
#Next we can alter table_articles_byauth to run on testlists (previously converted data)

testlist = articles_to_list(my_entrez_data, encoding = "UTF-8")
testsplit = split(1:length(testlist), ceiling(seq_along(1:length(testlist))/300) )

resultlist = list()

for(i in 1:length(testsplit)){
  print(i)
  

  resultlist[[i]] <- chopchop_to_df(pubmed_data = testlist[testsplit[[i]]], 
                                included_authors = "first", 
                                max_chars = 0, 
                                encoding = "ASCII")

  
}

new_PM_df = do.call(bind_rows, resultlist)




new_PM_df$newaddress <- new_PM_df$address %>% gsub("[[:punct:]]","", .) %>% strsplit(split = " ")

new_PM_df$countrymatch = "NULL"
new_PM_df$citymatch = "NULL"


for(i in 1:nrow(new_PM_df)){

  country = (world.cities$country.etc %in% (new_PM_df$newaddress[i] %>% unlist)) %>% world.cities$country.etc[.] %>% 
    table %>% sort(., decreasing = TRUE) %>% head(1) %>% names()
  
  if(is.null(country)){country = "NULL"}
  
  new_PM_df$countrymatch[i] = country
  
  
  city = (world.cities$name %in% (new_PM_df$newaddress[i] %>% unlist)) %>% world.cities$name[.] %>% 
    table %>% sort(., decreasing = TRUE) %>% head(1) %>% names()
  
  if(is.null(city)){city = "NULL"}
  
  new_PM_df$citymatch[i] = city  
}



new_PM_df = world.cities %>% group_by(country.etc) %>% summarise(meanlat = mean(lat), meanlong = mean(long)) %>% 
            left_join(x = new_PM_df, y =  ., by = c("countrymatch" = "country.etc"))

new_PM_df = world.cities %>% group_by(name) %>% summarise(citymeanlat = mean(lat), citymeanlong = mean(long)) %>% 
  left_join(x = new_PM_df, y =  ., by = c("citymatch" = "name"))

#To do >
leaflet(data = new_PM_df) %>% addTiles() %>% addMarkers(~citymeanlong, ~citymeanlat, popup = ~as.character(citymatch), label = ~as.character(citymatch), clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))
















# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # You can access the value of the widget with input$text, e.g.
  observeEvent(eventExpr = input$start, handlerExpr =  {
  
    progress <- Progress$new(session, min=1, max=1)

    progress$set(value = 1, 
                 message = 'Contacting PubMed',
                 detail = 'This may take a while...')
    
    
  search = pmquery(input$pubmedsearch, input$year, 'pubmap', input$email)
  my_entrez_id <<- get_pubmed_ids(search)
  
  my_entrez_data <- fetch_pubmed_data(my_entrez_id, retmax = 900)
  
  progress$close()
  

  #This part creates the results data frame, but splits the task into chunks of 100.
  testlist = articles_to_list(my_entrez_data, encoding = "UTF-8")
  testsplit = split(1:length(testlist), ceiling(seq_along(1:length(testlist))/100) )
  
  resultlist = list()
  
  progress <- Progress$new(session, min=1, max=length(testsplit))
  on.exit(progress$close())
  
  for(i in 1:length(testsplit)){

    
    progress$set(value = i, 
                 message = 'Analyzing PubMed results',
                 detail = paste('This may take a while... I found ', my_entrez_id$Count, 'results!'))
    
    
    resultlist[[i]] <- chopchop_to_df(pubmed_data = testlist[testsplit[[i]]], 
                                      included_authors = "first", 
                                      max_chars = 0, 
                                      encoding = "ASCII")
    
    
  }
  
  new_PM_df = do.call(bind_rows, resultlist)
  
  
  
  new_PM_df$newaddress <- new_PM_df$address %>% gsub("[[:punct:]]","", .) %>% strsplit(split = " ")
  
  new_PM_df$countrymatch = "NULL"
  new_PM_df$citymatch = "NULL"
  
  
  for(i in 1:nrow(new_PM_df)){
    
    country = (world.cities$country.etc %in% (new_PM_df$newaddress[i] %>% unlist)) %>% world.cities$country.etc[.] %>% 
      table %>% sort(., decreasing = TRUE) %>% head(1) %>% names()
    
    if(is.null(country)){country = "NULL"}
    
    new_PM_df$countrymatch[i] = country
    
    
    city = (world.cities$name %in% (new_PM_df$newaddress[i] %>% unlist)) %>% world.cities$name[.] %>% 
      table %>% sort(., decreasing = TRUE) %>% head(1) %>% names()
    
    if(is.null(city)){city = "NULL"}
    
    new_PM_df$citymatch[i] = city  
    
  }
  
  new_PM_df = world.cities %>% group_by(country.etc) %>% summarise(meanlat = mean(lat), meanlong = mean(long)) %>% 
    left_join(x = new_PM_df, y =  ., by = c("countrymatch" = "country.etc"))
  
  new_PM_df = world.cities %>% group_by(name) %>% summarise(citymeanlat = mean(lat), citymeanlong = mean(long)) %>% 
    left_join(x = new_PM_df, y =  ., by = c("citymatch" = "name"))
  
  print("Done")
  
  new_PM_df <<- new_PM_df
  
  }
               
)
  
  observeEvent(eventExpr = input$start, handlerExpr = output$table <- DT::renderDataTable( {new_PM_df %>% select(jabbrv ,journal) %>% group_by(jabbrv ,journal) %>% tally(sort = TRUE) %>% purrr::set_names(., c("Journal","Journal abbreviation", "Count")) %>% DT::datatable(., options = list(scrollY = "300px", paging = FALSE, searching = FALSE, lengthChange = FALSE), rownames = FALSE)} ) )
  
  observeEvent(eventExpr = input$start, handlerExpr = output$tablekeyword <- DT::renderDataTable( {new_PM_df$keywords %>% strsplit(., split = ";") %>% unlist %>% tolower() %>% as.data.frame() %>% rlang::set_names("keyword") %>% group_by(keyword) %>% summarise(counts = n()) %>% filter(!is.na(counts)) %>% arrange(desc(counts))%>% purrr::set_names(., c("Keyword", "Count")) %>% DT::datatable(., options = list(scrollY = "300px", paging = FALSE, searching = FALSE, lengthChange = FALSE), rownames = FALSE)} )) 
      
  observeEvent(eventExpr = input$start, handlerExpr = output$mymap <- renderLeaflet({
    leaflet(data = new_PM_df) %>% addTiles() %>% addMarkers(~meanlong, ~meanlat, popup = ~as.character(countrymatch), label = ~as.character(countrymatch), clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))
  }) )
  
  observeEvent(eventExpr = input$start, handlerExpr = output$mymapcity <- renderLeaflet({
    leaflet(data = new_PM_df) %>% addTiles() %>% addMarkers(~citymeanlong, ~citymeanlat, popup = ~as.character(citymatch), label = ~as.character(citymatch), clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))
  }) )
  
  observeEvent(eventExpr = input$start, handlerExpr = output$query <- renderValueBox({valueBox(value = tags$p("Pubmed query", style = "font-size: 50%;"), subtitle = my_entrez_id$QueryTranslation %>% print(quote = FALSE), icon = tags$i(icon("search"), style = "font-size: 75%;"),color = "light-blue")}) )
  observeEvent(eventExpr = input$start, handlerExpr = output$resultcount <- renderValueBox({valueBox(value = tags$p("Results found", style = "font-size: 50%;"), subtitle = my_entrez_id$Count, icon = tags$i(icon("list"), style = "font-size: 75%;"),color = "light-blue")}) )
  
}


ui <- dashboardPage(title = "PubMap, for confused researchers!",
  dashboardHeader(title = tags$a(href='http://github.com/jeffreymolendijk',
                                 tags$img(src='logowhite.svg', style="padding-top:0px; padding-bottom:5px", height = 40, width = "auto", align = "center"))),
  dashboardSidebar(sidebarMenu(menuItem("Read me", tabName = "README", icon = icon("readme")),
                               menuItem("Analysis", tabName = "Analysis", icon = icon("chart-bar")), hr()),
                   textInput(inputId = "pubmedsearch", label = "Pubmed search", value = '"learning analytics"'), 
                   textInput(inputId = "year", label = "Publication year", value = '2019'),
                   textInput(inputId = "email", label = "Your email", value = 'testuser1@live.com'),
                   actionButton("start", "Submit")),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Analysis", verticalLayout(fluidRow( column(8, offset = 0, valueBoxOutput("query", width = 12)), column(4, offset = 0, valueBoxOutput("resultcount", width = 12)) ),
                                                   tabBox(width = 12, title = "Table results", tabPanel("Journals", DT::dataTableOutput("table")), tabPanel("Keywords", DT::dataTableOutput("tablekeyword"))),
                                                   tabBox(width = 12, title = "Map results", tabPanel("Per country", leafletOutput("mymap")), tabPanel("Per city", leafletOutput("mymapcity"))), fluid = FALSE)),
      tabItem(tabName = "README", includeMarkdown("README.md"))
    ))
  
)

# Run the application 
shinyApp(ui = ui, server = server)

