library(pacman)
p_load(tidyverse, httr2, rvest, RSelenium, xml2)

# 172.17.0.2
# 127.0.0.1
# 192.168.99.100
remDr <- remoteDriver(remoteServerAddr = "127.0.0.1", port = 4445)

remDr$open()
remDr$navigate("https://www.uptodate.com/login")
remDr$getTitle()

#Log in manually

remDr$navigate("https://www.uptodate.com/contents/table-of-contents/patient-education")

table_of_contents <- xml2::read_html(remDr$getPageSource()[[1]])

# table_of_contents %>%  html_elements("#toc-list > ul > li:nth-child(1) > a")

contents_links <-
  table_of_contents %>%  html_elements(".toc-result-item > a")

contents_links <- html_attrs(contents_links)

contents_links <-
  unlist(contents_links)[c(T,F,F,F)]

for (i in 1:length(contents_links)) {
  contents_links[i] <- paste0("https://www.uptodate.com",contents_links[i])
}


master_navigation <- list()
for (i in 1:length(contents_links)) {
  
  remDr$navigate(contents_links[i])
  topic <- xml2::read_html(remDr$getPageSource()[[1]])
  articles <- 
    topic %>%  html_elements(".toc-result-item")
  article_language_links <- list()
  for (j in 1:length(articles)) {
    
    this_article_links <- list()
    
    if(
      articles[[j]] %>% 
      html_element("div") %>% 
      is.na()
    )
    {
      this_article_links[[1]] <-
        paste0(
          "https://www.uptodate.com",
          articles[[j]] %>% 
            html_elements("a") %>% 
            html_attr("href")
        )
      names(this_article_links)[1] <- "English"

    } else {
      options_list <-
        articles[[j]] %>% 
        html_elements("option")
      
      #assuming default blank is first in list
      for(k in 2:length(options_list))
      {
        this_article_links[[k-1]] <- 
          options_list[[k]] %>% 
          html_attr("value")
        names(this_article_links)[[k-1]] <-
          options_list[[k]] %>% 
          html_text()
      }
      
      
      
    }
    
    
    article_name <- 
      articles[[j]] %>% 
      html_elements("a") %>% 
      html_text()
    
    article_language_links[[j]] <- this_article_links
    names(article_language_links)[j] <- article_name
    
    print("article")
    print(j)
  }
  
  master_navigation[[i]] <- article_language_links
  
  names(master_navigation)[i] <- contents_links[i]
  
  print("contents")
  print(i)
  
}

save(master_navigation, file = "./data/master_navigation_file")

article_name <- 
  articles[[j]] %>% 
  html_elements("a") %>% 
  html_text()

articles <- 
  topic %>%  html_elements(".toc-result-item")


article_names <-
  articles %>% 
  html_elements("a") %>% 
  html_text()
  # html_attr("href")

articles %>% 
  html_elements(".wk-field-select-container") %>% 
  html_text()

articles %>% 
  html_elements("option")
