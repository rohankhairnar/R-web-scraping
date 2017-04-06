#by rohan khairnar
#Extracts details of products based on 'search_term'.
#The 'search_term' is hard-coded and has to be entered in the R file before running.
#Script runs in while loop for each 'next' occurence of a page, thus multiple iterations.
#Final file contains data with product titles, selling price, ratings, review links, product links and product codes.

#to do
#mine for positive and negative reviews
#code optimization, check
#quality checks, NA replacements/alternate css selectors


library(xml2)
library(rvest)
library(utils)

start_time <- proc.time()
local_file <- data.frame()

search_term <- "logitech"
search_term_coded <- URLencode(search_term)

url1 <- "https://www.amazon.com"
url2 <- "/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords="
amazon_pd <- paste0(url1, url2, search_term_coded)
page <- 1
deco0 <-"............................................................................"
deco <- "***********************"

while(!is.null(amazon_pd))
{
  while(amazon_pd != "https://www.amazon.com")
  {
    writeLines(paste0(("\n"),deco,(" Web Scrapping: Page "),page,(" for "),search_term,deco,("\n")))
    writeLines(paste0("Iteration ",page," initiated ! \n"))
    amazon_html<- read_html(amazon_pd)
    
    writeLines("\nFetching attributes....\n.\n.\n.")
    attributes<- html_attrs(html_nodes(amazon_html,"#resultsCol .s-access-detail-page"))
    writeLines("Attributes fetched\n\n")
    
    writeLines(paste0(("Iteration "),page,(" begins:"),deco0,("\n")))
    prod_titles <- sapply(attributes,'[[','title')
    writeLines(paste0(length(prod_titles)," products fetched"))
    prod_links <- sapply(attributes,'[[','href')
    
    #building valid links for missing domains. Some of the offer listed products have
    #encoded urls, which need to be decoded and replaced
    for(i in 1:length(prod_links))
    {
      if(length(prod_links[!grepl(url1,prod_links)]) != 0)
      {
        prod_links[i] <- gsub(".*url=",'',URLdecode(prod_links[i]))
      }
    }
    
    new_links <- prod_links
    writeLines(paste0(length(new_links)," links generated"))

    all_reviews_links <- gsub('/dp/','/product-reviews/',new_links)
    writeLines(paste0(length(all_reviews_links), " links for reviews generated"))
    
    #fetching all product codes
    prod_codes <- gsub('.*reviews/','',gsub('/ref.*','',all_reviews_links))
    writeLines(paste0(length(prod_codes), " product codes extracted"))
    
    #parting the CSS Selectors in order to make it dynamic as per the product codes
    part1 <- 'a.a-link-normal[href*='
    part2 <- '] .sx-zero-spacing'
    attributes_cost <- NULL
    trial0 <- paste0(part1,prod_codes,part2)
    prod_cost <- NULL
    
    for (i in 1:length(prod_codes))
    {
      if(length(html_nodes(amazon_html,trial0[i])) > 0)
      {
        attributes_cost[i] <-html_attrs(html_nodes(amazon_html,trial0[i]))
        prod_cost[i] = sapply(attributes_cost[i],'[[','aria-label')
      }
      else
      {
        prod_cost[i] <- "NA"
      }
    }
    
    drop(part1)
    drop(part2)
    drop(trial0)
    prod_cost = unlist(prod_cost)
    writeLines(paste0(length(prod_cost), " values for cost fetched"))
    
    
    part1 <- "span[name*='"
    part2 <- "'] span.a-icon-alt"
    trial0 <- paste0(part1,prod_codes,part2)
    prod_rating <- NULL
    
    for (i in 1:length(prod_codes))
    {
      if(length(html_nodes(amazon_html,trial0[i])) > 0)
      {
        prod_rating[i] <-html_text(html_nodes(amazon_html,trial0[i]))
        prod_rating[i] <- trimws(gsub('out.*','',prod_rating[i]))
      }
      else
      {
        prod_rating[i] <- "NA"
      }
    }
    drop(part1)
    drop(part2)
    drop(trial0)
    writeLines(paste0(length(prod_rating)," product ratings fecthed"))
    
    #for number of reviewers, to be added
    #html_text(html_nodes(amazon_html,".a-row a-spacing-top-mini span[name='B00YVVE7YO'] .a-size-small"))
    
    #generate a data frame of new data and bind with local_file for all iterations
    prod_data = data.frame(prod_titles,prod_cost,prod_rating,prod_codes,new_links,all_reviews_links)
    
    writeLines("\nBinding the collected data...\n.\n.")
    local_file <- rbind(local_file,prod_data)
    writeLines("Data sccessfully binded in local_file\n")
    writeLines(paste0("Iteration ",page," ends",deco0,"\n\n"))
    
    #for next iteration
    next_pg <- html_attrs(html_nodes(amazon_html,".pagnRA a"))
    next_pg_link <- paste0(url1,sapply(next_pg,'[[','href'))
    
    #used in next iteration
    amazon_pd <-next_pg_link
    page = page +1
  }
  write(local_file, file ="amazonsearch.csv")
  writeLines("\nFiles written successfully !")
  amazon_pd <- NULL
  writeLines(paste0("\nProcess completed !!\n"))
  writeLines(paste0("Total products lined: ", length(local_file$prod_titles)))
  writeLines(paste0("Total pages traversed: ", page))
  writeLines(paste0("\nResults for ",search_term," saved at ",getwd(),"/amazonsearch.csv"))
  writeLines(paste0("\nProcess time: "))
  et <- proc.time()-start_time
  print(et)
}
