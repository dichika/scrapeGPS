#' @export
getNikeplus <- function(url, gps=TRUE){
  require("rvest")
  require("V8")
  doc <- html(url)
  js <- doc %>% html_nodes("body script") %>% html_text()
  if(length(js)!=23){
    return("No Data")
  }
#   num <- doc %>% html_nodes("meta") %>% html_attr("property")
#   content <- doc %>% html_nodes("meta") %>% html_attr("content")
#   if(content[!is.na(num)&num=="og:type"] != "nikeapp:run"){
#     return("No Data")
#   }
  trg <- gsub("\n\t\twindow.np.", "var ", js[[12]])
  trg2 <- gsub("^\n\t.+window.np.share_data", "var share_data", js[[14]])
  trg2 <- gsub("\n\n\t\t\t\twindow.np.shared_page = true;\n\t\t", "", trg2)
  ct <- new_context()
  ct$eval(trg)
  ct$eval(trg2)
  res <- ct$get("baked_data")
  res2 <- ct$get("share_data")
  fuel <- res$activity$fuel
  calories <- res$activity$calories
  shoes <- res$activity$tags$SHOES$name
  distance_km <- res$activity$distance
  time <- as.character(as.POSIXct(gsub("T", " ", res$activity$startTimeUtc), "%Y-%m-%d %H:%M:%S", tz="UTC"))
  name <- res2$username
  gender <- res2$gender
  attr <- data.frame(name=ifelse(is.null(name), NA, name),
                     gender=ifelse(is.null(gender), NA, gender),
                     time_UTC=ifelse(is.null(time),  NA, time),
                     fuel=ifelse(is.null(fuel),  NA, fuel),
                     calories=ifelse(is.null(calories),  NA, calories),
                     distance_km=ifelse(is.null(distance_km),  NA, distance_km),
                     shoes=ifelse(is.null(shoes),  NA, shoes),
                     stringsAsFactors=FALSE)
  if(gps){
    gps_data <- res$activity$geo$waypoints
    gps_data <- ifelse(is.null(gps_data), NA, gps_data)
    gps_data <- as.data.frame(gps_data)
    return(list(attr=attr, gps=gps_data))    
  } else{
    return(attr)    
  }
}

#' @export
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