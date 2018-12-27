
library(rvest)
library(stringr)
library(tidyr)
library(gsubfn)
colleges_page <- read_html("https://www.usnews.com/best-colleges/rankings/national-universities")
titles <- colleges_page %>% html_nodes("h3") %>% html_text() %>% str_trim()
final_titles <- titles[1:10] 
rankings <- colleges_page %>% html_nodes(xpath = '//*[@class="display-block block-flush text-strong"]') %>% html_text() %>% str_trim()
rankings <- rankings[1:10]
rankings <- parse_number(rankings)
table_rank <- table(rankings)
ties <- rankings[table_rank > 1]
final_rankings <- sapply(rankings, function(rank){ if(rank %in% ties){return(paste(rank, "(tie)"))}else{return(rank)}   })
locations <- colleges_page %>% html_nodes(xpath = '//*[@class="display-block block-normal text-small"]') %>% html_text() 
final_locations <- locations %>% str_trim()
tuition <- colleges_page %>% html_nodes(xpath='//*[@class="display-inline-for-medium-up inline-right-tight-for-medium-up border-right-for-medium-up"]') %>% html_text() %>% str_trim()
tuition <- tuition[1:20]
tuitions <- str_extract(tuition[1:20], '^\\$[0-9,]*')
final_tuitions <- tuitions[!is.na(tuitions)]
inds <- seq(2, 20, by=2)
undergrad_enrollment <- tuition[inds]
undergrad_enrollment <- as.numeric(gsub("[^\\d]+", "", undergrad_enrollment, perl=TRUE))
first_bit <- sapply(undergrad_enrollment, function(string){ if(nchar(string) > 4){paste0(substring(string, 1, 2), ",")}else{paste0(substring(string, 1, 1), ",")}   })
second_bit <- sapply(undergrad_enrollment, function(string){if(nchar(string)>4){substring(string, 3)}else{substring(string, 2)}})
final_undergrad_enrollment <- paste0(first_bit, second_bit)
descriptions <- colleges_page %>% html_nodes(xpath='//*[@class="block-normal text-small show-for-medium-up ellipsis"]') %>% html_text() %>% str_trim()
descriptions <- str_remove(descriptions, "[\\n]")
descriptions <- str_remove(descriptions, "more")
descriptions[10] <- str_remove(descriptions[10], "more")
final_descriptions <- descriptions
top_ten_colleges <- data.frame(name=final_titles,location=final_locations, ranking=final_rankings, undergrad_enrollment  = final_undergrad_enrollment, tuition  = final_tuitions, description = final_descriptions)
knitr::kable(top_ten_colleges)




