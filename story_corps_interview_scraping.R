options(scipen = 7, stringsAsFactors = FALSE)
library(httr)
library(dplyr)
`%notin%` <- function(x,y) !(x %in% y)

url = "https://archive.storycorps.org/wp-json/interviews?page="
id = 1:17287

getStoryCorpsStats <- function() {
  GET("https://archive.storycorps.org/wp-json/stats") %>%
    content %>%
    bind_rows %>%
    as.data.frame
}

begin <- Sys.time()

sc_interview <- data.frame()
for (i in id) {
  
    print(paste0("On page: ", i))
  
    GET(paste0(url, i)) %>% content -> tmp
    
    if (length(tmp) == 0) {
      stop("No data returned from API")
    }
    
    dat <- data.frame()
    for (a in 1:length(tmp)) {
      
      if(is.null(tmp[[a]]$questions %>% unlist) | all(tmp[[a]]$questions %>%
                                                      unlist %>% attributes %>% unlist %>% unname %notin% c('text', 'text.text'))) {
        
        data.frame(
          id = tmp[[a]]$`_id`,
          title = tmp[[a]]$title,
          url = tmp[[a]]$url,
          description = tmp[[a]]$description,
          length = ifelse(is.null(tmp[[a]]$interview_audio$length), NA, tmp[[a]]$interview_audio$length),
          locality = tmp[[a]]$location %>%
            .[attributes(.)$names == 'locality'] %>%
            unlist %>%
            unname,
          region = tmp[[a]]$location %>%
            .[attributes(.)$names == 'region'] %>%
            unlist %>%
            unname,
          country = tmp[[a]]$location %>%
            .[attributes(.)$names == 'country'] %>%
            unlist %>%
            unname,
          keywords = paste(tmp[[a]]$keywords, collapse = ", "),
          created_date = tmp[[a]]$created_date,
          plays = tmp[[a]]$plays,
          permit = tmp[[a]]$permissions %>%
            .[attributes(.)$names == 'permit'] %>%
            unlist %>%
            unname,
          questions = NA
        ) -> api
        
        dat <- bind_rows(dat, api)
        
      }else{
        
        data.frame(
          id = tmp[[a]]$`_id`,
          title = tmp[[a]]$title,
          url = tmp[[a]]$url,
          description = tmp[[a]]$description,
          length = ifelse(is.null(tmp[[a]]$interview_audio$length), NA, tmp[[a]]$interview_audio$length),
          locality = tmp[[a]]$location %>%
            .[attributes(.)$names == 'locality'] %>%
            unlist %>%
            unname,
          region = tmp[[a]]$location %>%
            .[attributes(.)$names == 'region'] %>%
            unlist %>%
            unname,
          country = tmp[[a]]$location %>%
            .[attributes(.)$names == 'country'] %>%
            unlist %>%
            unname,
          keywords = paste(tmp[[a]]$keywords, collapse = ", "),
          created_date = tmp[[a]]$created_date,
          plays = tmp[[a]]$plays,
          permit = tmp[[a]]$permissions %>%
            .[attributes(.)$names == 'permit'] %>%
            unlist %>%
            unname,
          questions = tmp[[a]]$questions %>%
            unlist %>%
            .[attributes(.)$names %in% c('text.text', 'text')] %>%
            unname
        ) -> api
        
        dat <- bind_rows(dat, api)
        
      }
    
    }

  bind_rows(dat, sc_interview) -> sc_interview
  
}

print(Sys.time() - begin)
