rm(list=ls())

directorio <- "/Users/IDECOR/Documents/Code/Test Scripts/Scrapy/"
setwd(directorio)
getwd()

################################ extraer texto ################################

library(rvest)

web = read_html("https://www.geeksforgeeks.org/data-structures-in-r-programming")

heading = html_node(web, 'h1') 
######### h1 es el CSS selector del TÍTULO de la web ##########
# childNodes: NodeList [h1]
# children: HTMLCollection [h1]
# firstChild: h1
# firstElementChild: h1
# innerHTML:  "<h1 class=\"\">Data Structures in R Programming</h1>"

text = html_text(heading)
print(text)

# párrafos - usa html_nodes() para extraer todos

paragraph = html_nodes(web, 'p')
pText = html_text(paragraph) # convierte la data a texto
print(head(pText))


######################### Scraping a Table #########################

rm(list=ls())

# Static web
library(rvest)

# GDP per capita (current international dollar) by country or  territory or non IMF members 
web <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita")
node <- html_nodes(web, "table")
tableGDP <- html_table(node)[[2]]
head(tableGDP)

# Presidentes Argentinos 
web2 <- read_html("https://es.wikipedia.org/wiki/Anexo:Presidentes_de_la_Naci%C3%B3n_Argentina")
node2 <- html_nodes(web2, "table")
table.presidentesARG <- html_table(node2)[[1]]
head(table.presidentesARG)


# Dynamic web
library(rvest)
library(tidyverse)

url <- "https://www.worldometers.info/world-population/population-by-country/"

html <- read_html(url)
table_html <- html %>% html_nodes("table") %>% .[[1]] # usa el nodo html para extraer la tabla
table_df <- table_html %>% html_table() # lo convierte en data frame
head(table_df)


################################ extraer json data ################################

library(jsonlite)

# COVID cases in India
rawdata <- fromJSON("https://data.covid19india.org/v4/min/timeseries.min.json")
names(rawdata)

# getting the information for DL (Delhi)
data <- rawdata['DL']

# data for specific date 2020-03-16
data[[1]][[1]][[15]]



######################### Scraping precios #########################
rm(list=ls())

library(rvest)
shop <- read_html("https://scrapeme.live/shop")

# cada producto contiene
# a: that stores the product URL.
# img: that contains the product image.
# h2: that keeps the product name.
# span: that stores the product price.

html_products <- shop %>% html_elements("li.product")
#a_element <- html_products %>% html_elements("a") 
#img_element <- html_products %>% html_elements("img") 
#h2_element <- html_products %>% html_elements("h2") 
#span_element <- html_products %>% html_elements("span")

# 4 lists 
urls <- html_products %>% 
  html_element("a") %>% 
  html_attr("href") 
images <- html_products %>% 
  html_element("img") %>% 
  html_attr("src") 
names <- html_products %>% 
  html_element("h2") %>% 
  html_text2() 
prices <- html_products %>% 
  html_element("span") %>% 
  html_text2()

products <- data.frame(urls, images, names, prices)
products

names(products) <- c("url", "image", "name", "price")
names(products)
table(products)

#write.csv(products, file = "./products.csv", fileEncoding = "UTF-8")
#save(products, file="products.rda")
openxlsx::write.xlsx(products, "products.xlsx")


######################### Web Crawling #########################
# website consists of several web pages
# to scrape the entire website, extract the list of all pagination links
# Inspect pagination number HTML

page_links <- shop %>% 
  html_elements("a.page-numbers") %>% 
  html_attr("href")
# initializing the lists that will store the scraped data 
product_urls <- list() 
product_images <- list() 
product_names <- list() 
product_prices <- list() 

# initializing the list of pages to scrape with the first pagination links 
pages_to_scrape <- list("https://scrapeme.live/shop/page/1/") 

# initializing the list of pages discovered 
pages_discovered <- pages_to_scrape 

# current iteration 
i <- 1 
# max pages to scrape 
limit <- 5 

# until there is still a page to scrape 
while (length(pages_to_scrape) != 0) { 
  # getting the current page to scrape 
  page_to_scrape <- pages_to_scrape[[1]] 
  
  # removing the page to scrape from the list 
  pages_to_scrape <- pages_to_scrape[-1] 
  
  # retrieving the current page to scrape 
  document <- read_html(page_to_scrape) 
  
  # extracting the list of pagination links 
  new_pagination_links <- document %>% 
    html_elements("a.page-numbers") %>% 
    html_attr("href") 
  
  # iterating over the list of pagination links 
  for (new_pagination_link in new_pagination_links) { 
    # if the web page discovered is new and should be scraped 
    if (!(new_pagination_link %in% pages_discovered) && !(new_pagination_link %in% page_to_scrape)) { 
      pages_to_scrape <- append(new_pagination_link, pages_to_scrape) 
    } 
    
    # discovering new pages 
    pages_discovered <- append(new_pagination_link, pages_discovered) 
  } 
  
  # removing duplicates from pages_discovered 
  pages_discovered <- pages_discovered[!duplicated(pages_discovered)] 
  
  # scraping logic... 
  
  # incrementing the iteration counter 
  i <- i + 1 
} 

length(pages_discovered)


######################### Parallel web scraping #########################
# scrapea simultaneamente varias páginas

library(rvest)
library(parallel)
pages_to_scrape <- list( 
  "https://scrapeme.live/shop/page/1/", 
  "https://scrapeme.live/shop/page/2/", 
  "https://scrapeme.live/shop/page/3/", 
  "https://scrapeme.live/shop/page/4/", 
  "https://scrapeme.live/shop/page/5/",
  "https://scrapeme.live/shop/page/6/",
  "https://scrapeme.live/shop/page/7/",
  "https://scrapeme.live/shop/page/8/",
  "https://scrapeme.live/shop/page/9/",
  "https://scrapeme.live/shop/page/10/",
  "https://scrapeme.live/shop/page/11/",
  "https://scrapeme.live/shop/page/12/",
  "https://scrapeme.live/shop/page/13/",
  "https://scrapeme.live/shop/page/14/",
  "https://scrapeme.live/shop/page/15/",
  "https://scrapeme.live/shop/page/16/",
  "https://scrapeme.live/shop/page/17/",
  "https://scrapeme.live/shop/page/48/" 
)
length(pages_to_scrape)

scrape_page <- function(pages_to_scrape) {

  library(rvest) # va si o si adentro para que no tire error el read_html()
  
  shop <- read_html(pages_to_scrape) 
  
  html_products <- shop %>% html_elements("li.product") 
  
  product_urls <- html_products %>% 
    html_element("a") %>% 
    html_attr("href") 
  product_images <- html_products %>% 
    html_element("img") %>% 
    html_attr("src") 
  product_names <- 
    html_products %>% 
    html_element("h2") %>% 
    html_text2() 
  product_prices <- 
    html_products %>% 
    html_element("span") %>% 
    html_text2() 
  
  products <- data.frame( 
    unlist(product_urls), 
    unlist(product_images), 
    unlist(product_names), 
    unlist(product_prices) 
  ) 
  
  names(products) <- c("url", "image", "name", "price") 
  
  return(products) }

num_cores <- detectCores() 
cluster <- makeCluster(num_cores) 

scraped_data_list <- parLapply(cluster, pages_to_scrape, scrape_page) 

products <- do.call("rbind", scraped_data_list)


######################### Scraping Dynamic Content #########################

# library(RSelenium)
# { 
# driver <- rsDriver( 
# browser = c("chrome"), 
# chromever = "114.0.5735.90", 
# verbose = F, 
# extraCapabilities = list("chromeOptions" = list(args = list("--headless"))) 
# )
# web_driver <- driver[["client"]]
# web_driver$navigate("https://scrapeme.live/shop/")
# }




################################ extraer vía automating web browsers ################################
# https://www.geeksforgeeks.org/web-scraping-using-rselenium/

#library(tidyverse)
#library(RSelenium)
#library(httr)
#library(rvest)

#rD <- rsDriver(browser = "chrome", version = "latest",
#               chromever = "latest")

#remDr <- rD$client

#remDr$navigate("https://www.worldometers.info/coronavirus/")

# Extract the total number of cases
#total_cases <- remDr$findElement(using = "xpath",
#                                 value = '//*[@id="maincounter-wrap"]/div/span')
#total_cases <- total_cases$getElementText()[[1]]

# Extract the total number of deaths
#total_deaths <- remDr$findElement(using = "xpath",
#                                  value = '/html/body/div[3]/div[2]/div[1]/div/div[6]/div/span')
#total_deaths <- total_deaths$getElementText()[[1]]

# Extract the total number of recoveries
#total_recoveries <- remDr$findElement(using = "xpath",
#                                      value = '/html/body/div[3]/div[2]/div[1]/div/div[7]/div/span')
#total_recoveries <- total_recoveries$getElementText()[[1]]

# Print the extracted data
#cat("Total Cases: ", total_cases, "\n")
#cat("Total Deaths: ", total_deaths, "\n")
#cat("Total Recoveries: ", total_recoveries, "\n")

# Close the server
#remDr$close()
#selServ$stop()
