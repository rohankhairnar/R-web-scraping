library(xml2)
library(rvest)
library(utils)

local_file <- data.frame()
search_term <- "dell 8gb 500"
search_term_coded <- URLencode(search_term)

url1 <- "https://www.amazon.com"
url2 <- "/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords="
amazon_pd <- paste0(url1, url2, search_term_coded)

while(!is.null(amazon_pd))
{
  while(amazon_pd != "https://www.amazon.com")
  {
    amazon_html<- read_html(amazon_pd)
    #write_html(amazon_html, file = "/Users/temp/Desktop/R practise/amazhtml.txt")
    
    attributes<- html_attrs(html_nodes(amazon_html,"#resultsCol .s-access-detail-page"))
    attributes
    
    prod_titles <- sapply(attributes,'[[','title')
    length(prod_titles)
    prod_links <- sapply(attributes,'[[','href')
    length(prod_links)
    
    #building valid links for links missing domain. Some of the offer listed products have
    #encoded urls, which need to be decoded and replaced
    for(i in 1:length(prod_links))
    {
      if(length(prod_links[!grepl(url1,prod_links)]) != 0)
      {
        prod_links[i] <- gsub(".*url=",'',URLdecode(prod_links[i]))
      }
    }
    
    new_links <- prod_links
    length(new_links)
    #attributes_links <- html_attrs(html_nodes(amazon_html, "#resultsCol .a-spacing-top-mini .a-link-normal"))
    #links <- sapply(attributes_links,'[[','href')
    #tryd <- links[!]
    
    #length(links)
    #new_links<-gsub('ref.*','',links)
    all_reviews_links <- gsub('/dp/','/product-reviews/',new_links)
    length(all_reviews_links)
    
    #fetching all product codes
    prod_codes <- gsub('.*reviews/','',gsub('/ref.*','',all_reviews_links))
    length(prod_codes)
    
    #parting the CSS Selectors in order to make it dynamic as per the product codes
    part1 <- 'a.a-link-normal[href*='
    part2 <- '] .sx-zero-spacing'
    #trial0 <- 1
    #prod_cost <- 1
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
    length(prod_cost)
    
    
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
    
    #for number of reviewers
    #html_text(html_nodes(amazon_html,".a-row a-spacing-top-mini span[name='B00YVVE7YO'] .a-size-small"))
    
    prod_data = data.frame(prod_titles,prod_cost,prod_rating,prod_codes,new_links,all_reviews_links)
    
    local_file <- rbind(local_file,prod_data)
    
    #for next iteration
    next_pg <- html_attrs(html_nodes(amazon_html,".pagnRA a"))
    next_pg_link <- paste0(url1,sapply(next_pg,'[[','href'))
    #used in next iteration
    amazon_pd <-next_pg_link
  }
  write.csv(local_file, file ="amazonsearch.csv")
  amazon_pd <- NULL
  print(paste0(("Results for "),search_term,(" saved at "),getwd(),"/amazonsearch.csv"))
}