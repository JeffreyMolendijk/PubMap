# PubMap
Shiny app to analyze PubMed affiliations, highlighting journals and institutes related to a research topic.
(Find out in which journals your peers publish their results!)

## Installation
Please use one of the following options to run PubMap:
1. Install the shiny package and run shiny::runGitHub('PubMap', 'JeffreyMolendijk')
2. PubMap will be hosted on shinyapps.io in the future, to use within a browser.

## User guide
1. Define the search topic as you would do in a PubMed search
2. Define the publishing date (I would recommend recent years to speed up the analysis)
3. Click the submit button
4. Once PubMap is finished analyzing the data it will create a table and results panel


## Result interpretation
PubMap performs a PubMed search using the search parameters provided and analyzes the author affiliations of the results.
PubMap displays a table of scientific journals which have published research articles related to your topic of interest. 
PubMap displays a table of keywords related to your PubMed search.
PubMap generates a Leaflet map, indicating the countries, cities or institutes which frequently publish articles related to the search topic.

## Current limitations
The data retrieval process and analysis of data can be very time consuming, depending on the search parameters. 
PubMap is currently set to analyze the first 2000 results. This limitation may be removed in the future. 

## Comments
Running PubMap sends a search query to NCBI (PubMed). 
NCBI request users to include an email address in PubMed queries, and to limit the number of searches to three per second.
https://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.Usage_Guidelines_and_Requiremen

PubMap can be slow depending on the number of results found on PubMed, we recommend using a single year to test your query first. 
Quotation marks are placed around the PubMed Search input panel. "Protein biomarker" would search only for articles containing this combination of words.
Searching for Protein biomarker (without quotes) would give you all results containing either protein or biomarker.