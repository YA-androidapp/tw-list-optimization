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
pkgTest("caret")
pkgTest("RMeCab")

# devtools::install_github("mkearney/rtweet")
library(rtweet)

library(RMeCab)
Sys.setlocale("LC_CTYPE","ja_JP.UTF-8")

source("const.R")


# 定数


## path of home directory
home_directory <- path.expand("~")

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

nrow(user_tweets)
nrow(list_tweets)

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

saveRDS(user_tweetss, file = "user_tweetss.rds")
saveRDS(list_tweetss, file = "list_tweetss.rds")

# user_tweetss <- read.csv("user_tweetss.csv", encoding = "UTF-8", stringsAsFactors=F)
# list_tweetss <- read.csv("list_tweetss.csv", encoding = "UTF-8", stringsAsFactors=F)
# user_tweetss <- readRDS("user_tweetss.rds")
# list_tweetss <- readRDS("list_tweetss.rds")
nrow(user_tweetss)
nrow(list_tweetss)

user_df <- user_tweetss$text
list_df <- list_tweetss$text
length(user_df)
length(list_df)



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
  result <- gsub("^ ","",result)
  result <- gsub(" $","",result)
  
  result <- trimws(result)
  
  return( result )
}
user_df_p <- preprocessing(user_df)
list_df_p <- preprocessing(list_df)
length(user_df_p)
length(list_df_p)

saveRDS(user_df_p, file = "user_df_p.rds")
saveRDS(list_df_p, file = "list_df_p.rds")
# user_df_p <- readRDS("user_df_p.rds")
# list_df_p <- readRDS("list_df_p.rds")



## Mecab

runMecab <- function(df){
  #text <- df$text
  text <- df
  lent <- length(text)
  # MeCabに投げて解析
  texts <- rep("", lent)
  for (i in 1:lent){
    tryCatch({
      if(nchar(text[i])>0){
        # cat(i, " ")
        ulist <- unlist(RMeCabC(as.character(text[i]), 1)) # ",1"で表層形ではなく基本形で出力
        # 特定の品詞のみ抽出
        items <- ulist[names(ulist) %in% c("名詞")] # , "形容詞", "動詞")]
        items <- items[items!= "%"& items!= "0"& items!= "1"& items!= "2"& items!= "3"& items!= "4"&
                         items!= "5"& items!= "6"& items!= "7"& items!= "8"& items!= "9"&
                         items!= "％"& items!= "０"& items!= "１"& items!= "２"& items!= "３"& items!= "４"&
                         items!= "５"& items!= "６"& items!= "７"& items!= "８"& items!= "９"]
        texts[i] <- paste(items, collapse = " ")
      }else{
        texts[i] <- ""
      }
    }, 
    error = function(e) {
      texts[i] <- ""
    },
    finally = {},
    silent = TRUE
    )
  }
  return(texts)
}
user_df_m <- runMecab(user_df_p)
list_df_m <- runMecab(list_df_p)
length(user_df_m)
length(list_df_m)

saveRDS(user_df_m, file = "user_df_m.rds")
saveRDS(list_df_m, file = "list_df_m.rds")
# user_df_m <- readRDS("user_df_m.rds")
# list_df_m <- readRDS("list_df_m.rds")

# user_df_m[user_df_m %in% list_df_m]

df.mecab <- list_tweetss

i <- 1
rowc <- nrow(df.mecab)
df.mecab$mecabed <- rep("", rowc)
df.mecab$point <- rep(1, rowc)
for (i in 1:rowc) {
  point <- 1
  tryCatch({
    text1 <- list_df_m[i]
    df.mecab[i,"mecabed"] <- text1
    if(!is.null(text1) && nchar(text1)>0){
      for (text2 in user_df_m) {
        tryCatch({
          if(!is.null(text2) && nchar(text2)>0){
            point <- as.numeric(adist(text1, text2)/max(nchar(text1), nchar(text2)))
            if(point<df.mecab[i,"point"]){
              df.mecab[i,"point"] <- point
            }
          }
        }, 
        error = function(e) {},
        warning = function(e) {},
        finally = {},
        silent = TRUE
        )
      }
    }
  }, 
  error = function(e) {},
  warning = function(e) {},
  finally = {},
  silent = TRUE
  )
  
  cat(sprintf(paste("%",as.character(nchar(rowc)+1),"d",sep=""), i), ":\t", sprintf("%03.3f", round((100*i)/rowc, 1)), "%\t", sprintf("%01.3f", point), "\n")
  i <- i + 1
  
  # if(i>1000)break
}

write.csv(df.mecab, "df.mecab.csv", quote=TRUE, row.names=FALSE, fileEncoding = "UTF-8")
write.csv(df.mecab[,c("point", "mecabed")], "df.mecab_lite.csv", quote=TRUE, row.names=FALSE, fileEncoding = "UTF-8")
saveRDS(df.mecab, file = "df.mecab.rds")
# df.mecab <- readRDS("df.mecab.rds")

nrow(df.mecab)

pos <- 1:100
df.mecab <- df.mecab_[pos,]

df.mecab.text <- df.mecab[,"mecabed"]
df.mecab.dm <- as.data.frame(t(docMatrixDF(df.mecab.text, weight = "tf*idf*norm")))
df.mecab.dm$point <- df.mecab[,"point"]
rownames(df.mecab.dm) = paste(list_tweetss[pos, "text"], list_tweetss[pos, "created_at"], sep=" ")

train <- df.mecab.dm

train_sample = sample(nrow(train), nrow(train)*0.5)
train_tmp_data_frame = train[train_sample, ]
test_tmp_data_frame = train[-train_sample, ]

test_tmp_data_frame$point = as.factor(ifelse(test_tmp_data_frame$point<=0.5,1,0))
train_tmp_data_frame$point = as.factor(ifelse(train_tmp_data_frame$point<=0.5,1,0))

# ----------

library(stats)

a <- df.mecab.dm

# 主成分分析を実行
a.pc <- prcomp(a)
summary(a.pc) #princomp()は(行数)<(列数)で実行不可だが、prcomp()では可能
a.pc$rotation #固有ベクトル表示

# 結果のプロット
png("test.png", width = 2000, height = 2000)
plot(a.pc$x[,1], a.pc$x[,2], type="n")
text(a.pc$x[,1], a.pc$x[,2], list_tweetss[pos,"text"], col=ifelse(df.mecab.dm$point<=0.5,2,1))
dev.off()

# ----------

library(randomForest)
tmp_colnames <- colnames(test_tmp_data_frame)
colnames(test_tmp_data_frame) <- c(paste("X", 1:(ncol(test_tmp_data_frame)-1), sep=""), "point")
# colnames(test_tmp_data_frame) <- c(paste("X", colnames(test_tmp_data_frame)[1:(ncol(test_tmp_data_frame)-1)], sep=""), "point")
colnames(train_tmp_data_frame) <- c(paste("X", 1:(ncol(train_tmp_data_frame)-1), sep=""), "point")
# colnames(train_tmp_data_frame) <- c(paste("X", colnames(train_tmp_data_frame)[1:(ncol(train_tmp_data_frame)-1)], sep=""), "point")
rf_model = randomForest(point ~ . , data = train_tmp_data_frame, ntree = 100, proximity = TRUE)

rf_tune = tuneRF(train_tmp_data_frame[,1:(ncol(train_tmp_data_frame)-1)],train_tmp_data_frame[,ncol(train_tmp_data_frame)],doBest=T)
m <- rf_tune$mtry
(rf_model = randomForest(point ~ . , data = train_tmp_data_frame, ntree = 100, proximity = TRUE, mtry = m ))

png("test3.png", width = 2000, height = 2000)
varImpPlot(rf_model)
dev.off()

names(rf_model)
rf_model.imp <- importance(rf_model)
rownames(rf_model.imp) <- tmp_colnames[1:(length(tmp_colnames)-1)]

rf_model$predicted

rf_model_predict = predict(rf_model, test_tmp_data_frame, type="class")

table(rf_model_predict, test_tmp_data_frame$point)

# 評価
n <- length(rf_model_predict)
er <- as.numeric(as.character(rf_model_predict)) - as.numeric(as.character(test_tmp_data_frame$point))
rss <- sum(er*er) / n
rms <- sqrt(rss)
