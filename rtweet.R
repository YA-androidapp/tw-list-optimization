# ------------------------------------------------------------------------------------------- #
#                                                                                             #
#  tw-list-optimization                                                                       #
#                                                                                             #
# ------------------------------------------------------------------------------------------- #
#     Copyright (c) 2018 YA-androidapp(https://github.com/YA-androidapp) All rights reserved. #
# ------------------------------------------------------------------------------------------- #

setwd("~/R/tw-list-optimization")

# lib
pkgTest <- function(x)
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}
pkgTest("devtools")
pkgTest("tidyverse")

# devtools::install_github("mkearney/rtweet")
library(rtweet)

library(RMeCab)
Sys.setlocale("LC_CTYPE","ja_JP.UTF-8")

source("const.R")


# 定数


## path of home directory
home_directory <- path.expand("~/")

## combine with name for token
file_name <- file.path(home_directory, "twitter_token.rds")

## save token to home directory

twitter_token = create_token(
  app = appname,
  consumer_key = consumerKey,
  consumer_secret = consumerSecret)
saveRDS(twitter_token, file = file_name)
twitter_token <- readRDS(file = file_name)



## replace (because of rate limit)

lists_statuses2 <- function(list_id = NULL, slug = listName, owner_user = userName,
                            npm = 4000, include_rts = TRUE,
                            parse = TRUE, token = twitter_token){
  members <- lists_members(slug = listName, owner_user = userName)
  print(get_timelines(t(members[,"screen_name"]), n = npm, include_rts = TRUE,
                      parse = TRUE, token = twitter_token))
  
}

##



user_tweet <- get_timeline(userName,
                           n = 4000, include_rts = TRUE,
                           parse = TRUE,  token = twitter_token)

list_tweet <- lists_statuses2(list_id = NULL, slug = listName, owner_user = userName,
                              n = 4000, include_rts = TRUE,
                              parse = TRUE, token = twitter_token)

user_tweets <- user_tweet[order(user_tweet$created_at, decreasing=T), ]
list_tweets <- list_tweet[order(list_tweet$created_at, decreasing=T), ]

# 新しい方
# 厳密にはリストに追加されているユーザー同士でも比較する必要があるが割愛
if(user_tweets[nrow(user_tweets),"created_at"] > list_tweets[nrow(list_tweets),"created_at"]){
  maxi <- user_tweets[nrow(user_tweets),]
}else{
  maxi <- list_tweets[nrow(list_tweets),]
}
user_tweetss <- user_tweets[user_tweets$created_at >= maxi$created_at, ]
list_tweetss <- list_tweets[list_tweets$created_at >= maxi$created_at, ]
nrow(user_tweetss)
nrow(list_tweetss)
user_tweetss[nrow(user_tweetss),]
list_tweetss[nrow(list_tweetss),]

write_as_csv(user_tweetss, "user_tweetss.csv", prepend_ids = TRUE, na = "",
             fileEncoding = "UTF-8")
write_as_csv(list_tweetss, "list_tweetss.csv", prepend_ids = TRUE, na = "",
             fileEncoding = "UTF-8")

user_tweetss <- read.csv("user_tweetss.csv", encoding = "UTF-8")
list_tweetss <- read.csv("list_tweetss.csv", encoding = "UTF-8")

user_df <- user_tweetss$text
list_df <- list_tweetss$text



## Preprocessing

preprocessing <- function(text) {
  result = text
  
  result <- gsub("^@(home)|(mention)|(logout)[[:space:]]*","",result)
  result <- gsub("RT[[:space:]]*@[0-9a-zA-Z_-]+[[:space:]]*:?[[:space:]]*","",result)
  result <- gsub("\\r","",result)
  result <- gsub("\\n","",result)
  result <- gsub("<[^>]*>","",result)
  result <- gsub("><","",result)
  result <- gsub("[[:space:]][[:space:]]+"," ",result)
  result <- gsub("https?://[^ $]+","",result)
  result <- gsub("\\s+[[:punct:]]+\\s+"," ",result)
  
  result <- trimws(result)
  
  return( result )
}
user_df_p <- preprocessing(user_df)
list_df_p <- preprocessing(list_df)



## Mecab

runMecab <- function(df){
  #text <- df$text
  text <- df
  # MeCabに投げて解析
  texts <- NA
  for (i in 1:length(text)){
    tryCatch({
      if(length(text[i])>0){
        # cat(i, " ")
        ulist <- unlist(RMeCabC(as.character(text[i]), 1)) # ",1"で表層形ではなく基本形で出力
        # 特定の品詞のみ抽出
        items <- ulist[names(ulist) %in% c("名詞")] # , "形容詞", "動詞")]
        items <- items[items!= "0"& items!= "1"& items!= "2"& items!= "3"& items!= "4"&
                         items!= "5"& items!= "6"& items!= "7"& items!= "8"& items!= "9"&
                         items!= "０"& items!= "１"& items!= "２"& items!= "３"& items!= "４"&
                         items!= "５"& items!= "６"& items!= "７"& items!= "８"& items!= "９"]
        texts <- append(texts, paste(items, collapse = " "))
      }
    }, 
    error = function(e) {
      texts <- append(texts,"")
    },
    warning = function(e) {
      texts <- append(texts,"")
    },
    finally = {},
    silent = TRUE
    )
  }
  texts <- texts[-1]
  return(texts)
}
user_df_m <- runMecab(user_df_p)
list_df_m <- runMecab(list_df_p)

user_df_m1 <- user_df_m[user_df_m!=""]
list_df_m1 <- list_df_m[list_df_m!=""]

# user_df_m[user_df_m %in% list_df_m]

answerDf <- user_tweetss

i <- 0
answerDf$point <- rep(0, nrow(answerDf))
for (text1 in user_df_m1) {
  cat(".")
  for (text2 in list_df_m1) {
    cat("_")
    point <- adist(text1, text2)/length(text1)
    if(point < 0.5){
      answerDf[i,"point"] <- 1
      break
    }
  }
  i <- i + 1
  
  #if(i>100)break
}

# ...
