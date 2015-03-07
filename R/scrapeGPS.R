getNikeplus <- function(url){
  require("rvest")
  require("V8")
  js <- html(url) %>% html_nodes("body script") %>% html_text()
  trg <- gsub("\n\t\twindow.np.", "var ", js[[12]])
  ct <- new_context()
  ct$eval(trg)
  res <- ct$get("baked_data")
  res <- as.data.frame(res$activity$geo$waypoints)
  return(res)
}

getRunmeter <- function(url){
  require("rvest")
  require("V8")
  js <- html(url) %>% html_nodes("head script") %>% html_text()
  ct <- new_context()
  ct$eval(js[[4]])
  res <- ct$get("jsonData")
  res <- as.data.frame(res)
  return(res)
}