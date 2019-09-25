#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(easyPubMed)
library(XML)
library(dplyr)
library(maps)

#Load world cities to match against author affiliation > this dataset already contains lattitude and longitude
data("world.cities")
## For keywords make table of most commonly published journals. 



#collect pubmed ids using easypubmeds get_pubmed_ids
#function does not allow a maximum list over 100, but the search string can be modified using &retmax=100 to increase to 100.
#the following search finds the top 
#my_entrez_id <- get_pubmed_ids('lipidomics AND "2019"[PDAT]&retmax=10&tool=pubmap&email=testuser1@live.com')
#my_entrez_id <- get_pubmed_ids('lipidomics microbiome AND "2010"[PDAT]:"2019"[PDAT]&retmax=10&tool=pubmap&email=testuser1@live.com')
my_entrez_id <- get_pubmed_ids('"learning analytics"&retmax=10&tool=pubmap&email=testuser1@live.com')

my_entrez_data <- fetch_pubmed_data(my_entrez_id)

new_PM_df <- table_articles_byAuth(pubmed_data = my_entrez_data, 
                                   included_authors = "first", 
                                   max_chars = 0, 
                                   encoding = "ASCII")

new_PM_df$newaddress <- new_PM_df$address %>% gsub("[[:punct:]]","", .) %>% strsplit(split = " ")

new_PM_df$countrymatch = "NULL"

for(i in 1:nrow(new_PM_df)){

  country = (world.cities$country.etc %in% (new_PM_df$newaddress[i] %>% unlist)) %>% world.cities$country.etc[.] %>% 
    table %>% sort(., decreasing = TRUE) %>% head(1) %>% names()
  
  if(is.null(country)){country = "NULL"}
  
  new_PM_df$countrymatch[i] = country
    
}

new_PM_df$citymatch = "NULL"

for(i in 1:nrow(new_PM_df)){
  
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



ui <- fixedPage(
  
  titlePanel("PubMap: Visualize your PubMed search on the map"),
  
  fixedRow(
    column(3,
                    textInput(inputId = "pubmedsearch", label = "Pubmed search", value = 'lipidomics'), hr(),
                    textInput(inputId = "year", label = "Publication year", value = '2019'), hr(),
                    actionButton("start", "Submit")
    ),
    column(9, verticalLayout(DT::dataTableOutput("table"), hr() , leafletOutput("mymap"), fluid = FALSE)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # You can access the value of the widget with input$text, e.g.
  output$value <- renderPrint({ input$text })
  
  output$table <- DT::renderDataTable( {new_PM_df %>% select(jabbrv ,journal) %>% group_by(jabbrv ,journal) %>% tally(sort = TRUE) %>% DT::datatable(., options = list(scrollY = "300px", paging = FALSE, searching = FALSE, lengthChange = FALSE))} )
  
  output$mymap <- renderLeaflet({
    leaflet(data = new_PM_df) %>% addTiles() %>% addMarkers(~meanlong, ~meanlat, popup = ~as.character(countrymatch), label = ~as.character(countrymatch), clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

