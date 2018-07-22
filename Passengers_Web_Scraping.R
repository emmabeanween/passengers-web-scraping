library(rvest)
library(stringr)
library(lubridate)
#get the movie page from IMDB
passengers_movie <- read_html("https://www.imdb.com/title/tt1355644/")
description_path <- passengers_movie %>%  html_node(xpath = '//meta[@property= "og:description"]') 
#a short description
description_content <- description_path %>% html_attr("content") 
#rating, eg. PG-13
adult_rating <- passengers_movie %>% html_node(xpath = '//meta[@itemprop = "contentRating"]') %>% html_attr("content")
#retrieve the title
full_imdb_title <- passengers_movie %>% html_node(xpath = '//meta[@name="title"]') %>% html_attr("content")
shortened_title <- full_imdb_title %>% substring(-10, 10)
cast_path <- '//span[@itemprop = "actors"]' 
lead_cast_text <- passengers_movie %>% html_node(xpath = cast_path) %>% html_text()
#three lead cast actors
lead_cast <- lead_cast_text %>% str_trim()
lead_cast_final <- lead_cast %>% str_remove(",")
full_description <- passengers_movie %>% html_node(xpath = '//span[@itemprop = "description"]') %>% html_text()
full_description_final <- full_description %>% str_trim(side = c("left"))
average_user_rating <- passengers_movie %>% html_node(xpath = '//span[@itemprop = "ratingValue"]') %>% html_text() %>% as.numeric()
user_rating_base<- passengers_movie %>% html_node(xpath = '//strong[@title]') %>% html_attr("title") %>% as.character()
total_user_ratings <- str_sub(user_rating_base, start = 13, end = 20) %>% str_remove(",") %>% as.numeric()
movie_length <- passengers_movie %>% html_node(xpath = '//time[@itemprop = "duration"]') 
#returns 2 nodes, one with 1hr 56 min and one with 116 min, get both
two_durations <- sapply(movie_length, function(node){
  text <- node %>% html_text()
  return (text)
})
two_total_durations <- two_durations %>% str_trim()
#includes info such as budget, total gross, opening day weekend, movie company etc.
more_info <- passengers_movie %>% html_nodes(".txt-block")
random_facts <- sapply(more_info, function(node){
  text_all <- node %>% html_text()
  return(text_all)
})
#random_facts is a list
#get gross for both US + Worldwide, and budget
trivia <- random_facts[[20]] %>% str_trim()
all_gross <- random_facts[c(12, 13)] %>% str_trim() %>% strsplit("                ")
gross_matrix <- matrix(all_gross, nrow = 1, ncol = 2)
total_budget <- total_budget <- random_facts[[10]] %>% str_trim() %>% str_sub(start = 8, end = 19)
opening_weekend <- random_facts[[11]] %>% str_sub(start = 48, end = 64) %>% str_remove(",") 
#convert with lubridate
opening_weekend <- dmy(opening_weekend)
class(opening_weekend)
#see how much time has passed between when the movie came out and now
today <- as.Date((lubridate::now()))
diff <- as.period(today - opening_weekend, unit = "years")
diff_in_years <-  as.numeric(substring(diff, -10, 3))/(365) %>% round(2)
genres <- passengers_movie %>% html_nodes(xpath = '//span[@itemprop="genre"]') %>% html_text()
total_reviews <- passengers_movie %>% html_nodes(xpath = '//span[@itemprop = "reviewCount"]') %>% html_text()
numbers_from_total_reviews <- gsub("([400-1000]+).*$", "\\1", total_reviews)
user_reviews <- str_split(numbers_from_total_reviews, "               ")[[1]] %>% as.numeric()
critic_reviews <- str_split(numbers_from_total_reviews, "               ")[[2]] %>% as.numeric()
small_reviews <- data.frame(User = user_reviews, Critic = critic_reviews )
small_reviews
passengers_movie_list <- list(title = full_imdb_title, lead_cast = lead_cast_final, description = full_description_final, user_rating = 
                                average_user_rating )
passengers_movie_list

passengers_characters <- passengers_movie %>% html_nodes(".character") %>% html_text() 
main_passengers_characters <- passengers_characters[1:3] %>% str_trim()
passengers_keywords <- passengers_movie %>% html_nodes(xpath = '//span[@itemprop="keywords"]') %>% html_text()
filming_location <- random_facts[[9]] %>% str_trim() %>% str_sub(start = 22, end = 44)
tagline <- random_facts[[1]] %>% str_trim() %>% str_remove("\n") %>% str_sub(start = 1, end = 30)
full_supporting_cast <- passengers_movie %>% html_nodes(".cast_list") %>% html_table(trim=TRUE)



  
  








