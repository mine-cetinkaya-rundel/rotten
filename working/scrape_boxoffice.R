scrape_boxoffice <- function(clean = TRUE, write = FALSE){
  url <- "http://boxofficemojo.com/alltime/adjusted.htm"
  boxoffice <- url %>%
    html() %>%
    html_nodes(xpath='//table') %>%
    html_table(fill = TRUE, header = TRUE)
  boxoffice <- boxoffice[[4]]
  if(clean == TRUE){
    names(boxoffice) <- names(boxoffice) %>%
      str_replace(" \\(click to view\\)", "") %>%
      str_replace(" ", "_") %>%
      str_replace("\\^", "") %>%
      tolower()
    if(write == TRUE){
      write.csv(boxoffice, "boxoffice.csv")
    }
  }
  if(clean == FALSE & write == TRUE){
    write.csv(boxoffice, "boxoffice_raw.csv")
  }
  return(boxoffice_raw = boxoffice)
}


letters %>%
  str_pad(5, "right") %>%
  str_c(letters)



