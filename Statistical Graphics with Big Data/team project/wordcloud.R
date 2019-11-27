# Load library 
library(tidyverse)
library(data.table)

# Load Data
setwd("C:/Users/Chanmi Yoo/Desktop/빅데/팀프로젝트/data")
spotify <- fread("SpotifyFeatures.csv", encoding = "UTF-8")

# Preprocessing
# 1) NA 값 확인 
table(is.na(spotify))
## [설명] 결측치가 전혀 없다. 

# 2) 중복 음원 제거 
spotify.uniq <- spotify[-which(duplicated(spotify$track_id)),]
dim(spotify.uniq)
## [설명] 총 176774개 음원이 있다. 


# Creating Word Cloud - Track Title
# Load Library 
library(tm)
library(wordcloud)
library(RColorBrewer)

# Expand memory limit
memory.limit(9999999999999)

# Create a text1 vector 
# 데이터가 너무 커서 처리하는 시간이 오래 걸린다.
# 따라서 text11(60000개), text12(60000개), text13(56774개), 총 3개로 분할하여 Corpus를 처리하기로 하였다. 
text1 <- spotify.uniq %>% 
  mutate(rank=row_number(desc(popularity))) %>% 
  filter(rank %in% 1:60000) %>% 
  select(track_name)

text2 <- spotify.uniq %>% 
  mutate(rank=row_number(desc(popularity))) %>% 
  filter(rank %in% 60001:120000) %>% 
  select(track_name) 

text3 <- spotify.uniq %>% 
  mutate(rank=row_number(desc(popularity))) %>% 
  filter(rank %in% 120001:176774) %>% 
  select(track_name) 

######################################################
# 1) featuring 정보 
#tail(str_subset(get(paste("text", i, sep=""))$track_name, regex('feat', ignore_case = T)), 10)
## [설명] featuring 정보는 대부분 "(feat. [가수])", "(Feat. [가수])", "[FEAT. [가수]]"의 형태로 들어간다. 

# featuring 정보 제거 
text1 <-  text1 %>% 
  str_replace("[[:blank:]]{1}[fF]{1}(eat)[[:print:]]+", "") %>% 
  str_replace_all("[[:blank:]]{1}(FEAT.)[[:print:]]+", "")


# 2) remaster 정보
#tail(str_subset(text1, regex('remaster', ignore_case = T)), 10)
## [설명] remastered 정보는 대부분 "- Remastered" " - Remasterizado [연도]" 의 형태로 들어간다. 

# remaster 정보 제거 
text1 <- text1 %>% 
  str_replace_all("(Remastered) +", "") %>% 
  str_replace_all("[0-9]{4}[[:space:]]{1}(Remaster)", "") %>% 
  str_replace_all("(-)[[:space:]]{1}(Remasterizado)[[:space:]]{1}[0-9]{4}", "") %>% 
  str_replace_all("(- Remastered)", "") %>% 
  str_replace_all("(Remastered)", "") %>% 
  str_replace_all("(- Remaster)", "") %>% 
  str_replace_all("[0-9]{4}[[:print:]](Remaster)", "") %>% 
  str_replace_all("[0-9]{4}( Digital Remaster)+", "") %>% 
  str_replace_all("[-(]{1}[[:print:]]+", "")

# 3) version 정보 
#tail(str_subset(text1$track_name, regex('version', ignore_case = T)), 10)
## [설명] version 정보는 " - [문자] Version " 혹은 " ( [문자] version ) 의 형태로 들어간다. 

text1 <- text1 %>% 
  str_replace_all("(-)[[:space:]][[:print:]]+[Vv]{1}(ersion)", "") %>% 
  str_replace_all("(- Version)", "") %>% 
  str_replace_all("(()[[:print:]][Vv]{1}(ersion)())", "")

# create a corpus
#docs <- Corpus(VectorSource(get(paste("text", i, sep=""))))
docs1 <- Corpus(VectorSource(text1))

# clean text1 data 
docs1 <- docs1 %>%
  #tm_map(removeNumbers) %>%  # 숫자 제거 
  tm_map(removePunctuation) %>%  # 구두점 제거 
  tm_map(stripWhitespace) %>%  # 빈 띄어쓰기 제거 
  tm_map(content_transformer(tolower)) %>%  # 소문자화 
  tm_map(removeWords, c(stopwords("english"), 'dont', 'cant', 'major', 'minor')) # 불용어 제거(english 패키지, 무의미한 단어)

#  tm_map(removeWords, stopwords(c('it','its','itself','this','that','these','those',
#                                  'am','is','are,','been','being','would','did','doing','would','should',
#                                  'a','an','and','but','if','or','because','as','by')))  # 불용어 제거 

# create a document-term-matrix
dtm1 <- TermDocumentMatrix(docs1)
matrix1 <- as.matrix(dtm1) 

words1 <- sort(rowSums(matrix1), decreasing = T) 
df1 <- data.frame(word = names(words1), freq1 = words1)


# Text2 처리 
# 1) featuring 정보 
#tail(str_subset(get(paste("text", i, sep=""))$track_name, regex('feat', ignore_case = T)), 10)
## [설명] featuring 정보는 대부분 "(feat. [가수])", "(Feat. [가수])", "[FEAT. [가수]]"의 형태로 들어간다. 

# featuring 정보 제거 
text2$track_name <-  text2$track_name %>% 
  str_replace("[[:blank:]]{1}[fF]{1}(eat)[[:print:]]+", "") %>% 
  str_replace_all("[[:blank:]]{1}(FEAT.)[[:print:]]+", "")


# 2) remaster 정보
#tail(str_subset(text2$track_name, regex('remaster', ignore_case = T)), 10)
## [설명] remastered 정보는 대부분 "- Remastered" " - Remasterizado [연도]" 의 형태로 들어간다. 

# remaster 정보 제거 
text2$track_name <- text2$track_name %>% 
  str_replace_all("(Remastered) +", "") %>% 
  str_replace_all("[0-9]{4}[[:space:]]{1}(Remaster)", "") %>% 
  str_replace_all("(-)[[:space:]]{1}(Remasterizado)[[:space:]]{1}[0-9]{4}", "") %>% 
  str_replace_all("(- Remastered)", "") %>% 
  str_replace_all("(Remastered)", "") %>% 
  str_replace_all("(- Remaster)", "") %>% 
  str_replace_all("[0-9]{4}[[:print:]](Remaster)", "") %>% 
  str_replace_all("[0-9]{4}( Digital Remaster)+", "") %>% 
  str_replace_all("[-(]{1}[[:print:]]+", "")

# 3) version 정보 
#tail(str_subset(text2$track_name, regex('version', ignore_case = T)), 10)
## [설명] version 정보는 " - [문자] Version " 혹은 " ( [문자] version ) 의 형태로 들어간다. 

text2$track_name <- text2$track_name %>% 
  str_replace_all("(-)[[:space:]][[:print:]]+[Vv]{1}(ersion)", "") %>% 
  str_replace_all("(- Version)", "") %>% 
  str_replace_all("(()[[:print:]][Vv]{1}(ersion)())", "")

# Create a corpus
docs2 <- Corpus(VectorSource(text2))

# Clean text2 data 
docs2 <- docs2 %>%
  #tm_map(removeNumbers) %>%  # 숫자 제거 
  tm_map(removePunctuation) %>%  # 구두점 제거 
  tm_map(stripWhitespace) %>%  # 빈 띄어쓰기 제거 
  tm_map(content_transformer(tolower)) %>%  # 소문자화 
  tm_map(removeWords, c(stopwords("english"), 'dont', 'cant', 'major', 'minor')) # 불용어 제거(english 패키지, 무의미한 단어)

# Create a document-term-matrix
dtm2 <- TermDocumentMatrix(docs2)
matrix2 <- as.matrix(dtm2) 

words2 <- sort(rowSums(matrix2), decreasing = T) 
df2 <- data.frame(word = names(words2), freq2 = words2)


# Text3 처리
# 1) featuring 정보 
#tail(str_subset(get(paste("text", i, sep=""))$track_name, regex('feat', ignore_case = T)), 10)
## [설명] featuring 정보는 대부분 "(feat. [가수])", "(Feat. [가수])", "[FEAT. [가수]]"의 형태로 들어간다. 

# featuring 정보 제거 
text3 <-  text3 %>% 
  str_replace("[[:blank:]]{1}[fF]{1}(eat)[[:print:]]+", "") %>% 
  str_replace_all("[[:blank:]]{1}(FEAT.)[[:print:]]+", "")


# 2) remaster 정보
#tail(str_subset(text3$track_name, regex('remaster', ignore_case = T)), 10)
## [설명] remastered 정보는 대부분 "- Remastered" " - Remasterizado [연도]" 의 형태로 들어간다. 

# remaster 정보 제거 
text3 <- text3 %>% 
  str_replace_all("(Remastered) +", "") %>% 
  str_replace_all("[0-9]{4}[[:space:]]{1}(Remaster)", "") %>% 
  str_replace_all("(-)[[:space:]]{1}(Remasterizado)[[:space:]]{1}[0-9]{4}", "") %>% 
  str_replace_all("(- Remastered)", "") %>% 
  str_replace_all("(Remastered)", "") %>% 
  str_replace_all("(- Remaster)", "") %>% 
  str_replace_all("[0-9]{4}[[:print:]](Remaster)", "") %>% 
  str_replace_all("[0-9]{4}( Digital Remaster)+", "") %>% 
  str_replace_all("[-(]{1}[[:print:]]+", "")

# 3) version 정보 
#tail(str_subset(text3$track_name, regex('version', ignore_case = T)), 10)
## [설명] version 정보는 " - [문자] Version " 혹은 " ( [문자] version ) 의 형태로 들어간다. 

text3 <- text3 %>% 
  str_replace_all("(-)[[:space:]][[:print:]]+[Vv]{1}(ersion)", "") %>% 
  str_replace_all("(- Version)", "") %>% 
  str_replace_all("(()[[:print:]][Vv]{1}(ersion)())", "")

# Create a corpus
docs3 <- Corpus(VectorSource(text3))

# Clean text3 data 
docs3 <- docs3 %>%
  #tm_map(removeNumbers) %>%  # 숫자 제거 
  tm_map(removePunctuation) %>%  # 구두점 제거 
  tm_map(stripWhitespace) %>%  # 빈 띄어쓰기 제거 
  tm_map(content_transformer(tolower)) %>%  # 소문자화 
  tm_map(removeWords, c(stopwords("english"), 'dont', 'cant', 'major', 'minor')) # 불용어 제거(english 패키지, 무의미한 단어)

# Create a document-term-matrix
dtm3 <- TermDocumentMatrix(docs3)
matrix3 <- as.matrix(dtm3) 

words3 <- sort(rowSums(matrix3), decreasing = T) 
df3 <- data.frame(word = names(words3), freq3 = words3)


######################################################
# 3개의 데이터프레임 합치기 
df1$word <- as.character(df1$word)
df2$word <- as.character(df2$word)
df3$word <- as.character(df3$word)

df <- full_join(df1, df2, by="word") %>%
  full_join(df3, by="word")

df <- df %>% 
  mutate(freq = rowSums(subset(df, select=c(freq1,freq2,freq3)), na.rm = T)) %>% 
  select(word, freq) %>% 
  arrange(desc(freq))

# Top 10 frequently appear word in track_name  
head(df,10)

# Draw wordcloud 
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, 
          min.freq = 300,  max.freq = Inf, # 최소 300번 이상 나온 단어 추출  
          random.order=FALSE, rot.per=0.35,         
          colors=brewer.pal(8, "Dark2"))

######################################################

# [결과] love가 2299회로 가장 많았고, act(1318회)가 그 뒤를 이었으며, act, one, time, song, like, man, little, good, life 순으로 많았다. 

######################################################

