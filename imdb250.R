library(rvest)
library(utils)
library(xml2)

base_url<- "http://www.imdb.com"
url250 <- "http://www.imdb.com/chart/top?ref_=nv_mv_250_6"

imdb_250 <- read_html(url250)

imdb_250 %>%
  html_nodes(".secondaryInfo , strong , #main a") %>%
  html_text()


#write_html(imdb_250, file = "/Users/temp/Desktop/R practise/imdbhtml.txt")

movie_list <- imdb_250 %>%
  html_nodes(".titleColumn a") %>%
  html_text()

movie_year<- gsub(")","", gsub("\\(","", 
                               imdb_250 %>%
                                 html_nodes(".secondaryInfo") %>%
                                 html_text()))

movie_rating<-imdb_250 %>%
  html_nodes("strong") %>%
  html_text()

all_nodes<-html_nodes(imdb_250, ".titleColumn a")

#write_html(all_nodes,file = "/Users/temp/Desktop/R practise/nodes.html")

#html_attrs(all_nodes)

key_cast<- sapply(html_attrs(all_nodes),'[[','title')
movie_link <- paste0(base_url,sapply(html_attrs(all_nodes),'[[','href'))

#html_attrs((all_nodes),'[[','href')

rank <- c(1:length(movie_link))

x<- html_nodes(imdb_250, ".ratingColumn strong")
#gsub("//D","",x)%>% 
voters_count<- gsub(".*?based on ","",gsub(" user ratings.*","",x))

db_250 = data.frame(rank, movie_list, key_cast, movie_year, movie_rating, voters_count, movie_link)

write.csv(db_250, file = "/Users/temp/Desktop/R practise/imdb250.csv")

