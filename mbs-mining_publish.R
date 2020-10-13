# 17 Apr 2019
# This was a project I did in collaboration with a hotelier consultant. The objective of the study was to understand what customers look for when staying at a hotel. An efficient way is to understand this is to collect customer reviews from websites such as tripadvisor to get insights. 
# This script scrapes tripadvisor website and collects the text of reviews and breaks down the text into individual words to count their occurence. This particular example looks at the Marina Bay Sands hotel in Singapore.

# update Oct 2020: The website structure of Tripadvisor has changed and the script no longer works as it is very specific to the structure of the site.

library(tidytext)
library(dplyr)
library(rvest) #text scraping
library(ggplot2)
library(ggwordcloud)

#create empty object to store review text
summary_review <- data.frame("word" = "hotel","n" = 0) 

#link to first page of reviews
pg0_url <- "https://www.tripadvisor.com.sg/Hotel_Review-g294265-d1770798-Reviews-Marina_Bay_Sands-Singapore.html#REVIEWS"

#find the the lnumber of the last page listed in the bottom of the main page
 npages<- pg0_url %>%
   read_html() %>%
   html_nodes(" .pageNum ") %>%
   html_attr(name="data-page-number") %>%
   tail(.,1) %>%
   as.numeric()

for(i in 1:npages) {
  
  x <- 5*i-5
  pg0_url <- paste("https://www.tripadvisor.com.sg/Hotel_Review-g294265-d1770798-Reviews-or",x,"-Marina_Bay_Sands-Singapore.html#REVIEWS", sep = "")
  
  review_text <- pg0_url %>%
    read_html() %>%
    html_nodes('.common-text-ReadMore__content--2X4LR') %>%
    html_node('.hotels-review-list-parts-ExpandableReview__reviewText--3oMkH span') %>%
    html_text()
  
  review_text <- na.omit(review_text)
  text_df <- tibble(text = review_text) %>% unnest_tokens(word, text) %>% count(word, sort = TRUE)
  summary_review <- rbind(summary_review,text_df) %>% group_by(word) %>% summarize(n = sum(n)) %>% ungroup()
  
}

summary_review <- summary_review[order(-summary_review$n),]
write.csv(summary_review, file = "MBS.csv")

top_20 <- summary_review[1:20,1:2]

ggplot(top_20, aes(label = word, size = n*10)) +
  geom_text_wordcloud() +
  theme_minimal()
