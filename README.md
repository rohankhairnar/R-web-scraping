# R-Projects-Rep

Packages required:
xml2
rvest
utils

Install Packages:

install.packages(xml2)
install.packages(rvest)
install.packages(utils)

amazonsearch.R

Extracts details of products based on 'search term'. The 'search term' is hard-coded and has to be entered in the R file before running.
Script runs in while loop for each 'next' occurence of a page. Final file contains data with product titles, selling price, ratings, review links, product links and product codes.

imdb250.R

Contains basic scripts to being with web scrapping in R, good when executed line-by-line to understand changes at each level.