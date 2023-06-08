#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("lubridate")
#install.packages("quanteda")
#install.packages("readtext")
#install.packages("seededlda")
#install.packages("writexl")
#install.packages("reticulate")
library(tidyverse)
library(tidytext)
library(lubridate)
library(quanteda)
library(readtext)
library(seededlda)
library(writexl)
library(reticulate)

#setwd("./카카오톡 받은 파일")

pkg_v <- c("tidyverse", "tidytext", "lubridate", "quanteda", "readtext", "seededlda")
purrr::map(pkg_v, require, ch = T)

file_path <- "./everytime_keywords2.xlsx"
readxl::read_excel(file_path) %>%
  glimpse()

readxl::read_excel(file_path) %>%
  select(text, keyword) -> vac_df
vac_df %>% head(3)

# nan값 제거
#na.omit(vac_df) ->vac_df

# 발화별 ID 부여
vac_df %>%
  mutate(ID = factor(row_number())) -> vac2_df

vac2_df %>% glimpse()

vac2_df %>%
  unnest_tokens(word, keyword, token = "regex", pattern = ",") -> vac_tk
vac_tk %>%
  select(text, word) %>%
  head(n = 5)

vac_tk %>%
  count(word, sort = T) -> count_df
count_df %>% head(n = 10)

#텍스트 전처리
combined_df <-
  vac_tk %>%
  group_by(ID) %>%
  summarise(text2 = str_flatten(word, " ")) %>%
  ungroup() %>%
  inner_join(vac2_df, by = "ID")

combined_df %>% glimpse()

combined_df %>%
  corpus(text_field = "text2") -> c_corp
c_corp %>% glimpse()

c_corp <- quanteda::corpus(combined_df, text_field = "text2")
docvars_df <- quanteda::docvars(c_corp)
docvars_df %>% glimpse()


c_corp %>%
  tokens(remove_punct = T) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 0.8,
           termfreq_type = "quantile",
           max_docfreq = 0.1,
           docfreq_type = "prop") -> c_dfm

c_dfm %>% topfeatures(n = 20)

set.seed(37)
c_dfm %>% textmodel_lda(k = 13) -> c_lda
c_lda %>% glimpse()

# lda 성능 평가
c_lda %>% divergence(weighted = TRUE, min_size = 0.01, select = NULL)->r
print(r)

c_lda %>% terms(n = 10) %>% as.data.frame() %>%
  select(topic1:topic13)

# phi threshold 지정
phi_threshold <- 0.015

# meaningful keyword 추출
c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic1 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic1>threshold) -> meaningful1
dimnames(meaningful1)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic2 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic2>threshold) -> meaningful2
dimnames(meaningful2)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic3 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic3>threshold) -> meaningful3
dimnames(meaningful3)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic4 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic4>threshold) -> meaningful4
dimnames(meaningful4)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic5 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic5>threshold) -> meaningful5
dimnames(meaningful5)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic6 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic6>threshold) -> meaningful6
dimnames(meaningful6)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic7 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic7>threshold) -> meaningful7
dimnames(meaningful7)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic8 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic8>threshold) -> meaningful8
dimnames(meaningful8)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic9 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic9>threshold) -> meaningful9
dimnames(meaningful9)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic10 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic10>threshold) -> meaningful10
dimnames(meaningful10)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic11 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic11>threshold) -> meaningful11
dimnames(meaningful11)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic12 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic12>threshold) -> meaningful12
dimnames(meaningful12)[[1]]

c_lda$phi %>% t() %>%
  as.data.frame() %>%
  arrange(topic13 %>% desc) %>%
  round(digits = 3) %>%
  subset(topic13>threshold) -> meaningful13
dimnames(meaningful13)[[1]]

keywords_vec <- c(dimnames(meaningful1)[[1]],
                  dimnames(meaningful2)[[1]],
                  dimnames(meaningful3)[[1]],
                  dimnames(meaningful4)[[1]],
                  dimnames(meaningful5)[[1]],
                  dimnames(meaningful6)[[1]],
                  dimnames(meaningful7)[[1]],
                  dimnames(meaningful8)[[1]],
                  dimnames(meaningful9)[[1]],
                  dimnames(meaningful10)[[1]],
                  dimnames(meaningful11)[[1]],
                  dimnames(meaningful12)[[1]],
                  dimnames(meaningful13)[[1]])
keywords_vec


c_lda$theta %>% as.data.frame() -> clda_theta_df
clda_theta_df %>%
  select(topic1:topic13) %>%
  round(digits = 3) %>% head(3)

quanteda::docvars(c_corp) -> docvars_df
docvars_df  %>% glimpse()

bind_cols(docvars_df, clda_theta_df) %>%
  mutate(textID = factor(row_number()), .before = ID) -> theta_df
theta_df %>% glimpse()

theta_df %>%
  writexl::write_xlsx("theta_df.xlsx")


theta_df %>%
  select(topic1:topic10) %>%
  pmap(sum) %>%
  head(3)

theta_df %>%
  arrange(topic9 %>% desc) %>%
  pull(text) %>% head(5)

c_lda %>% terms(n = 5) %>% as.data.frame() -> c_ldaterm5_df
c_ldaterm5_df %>%
  writexl::write_xlsx("ldaterms.xlsx")

# 우울 발화 사전 불러오기
depressed_dic <- read_csv(file="cleaned_similarity80.csv")
depressed_dic1 <- subset(depressed_dic, tag == 1, select=c(cleaned_word))$cleaned_word
depressed_dic2 <- subset(depressed_dic, tag == 2, select=c(cleaned_word))$cleaned_word
depressed_dic3 <- subset(depressed_dic, tag == 3, select=c(cleaned_word))$cleaned_word
depressed_dic4 <- subset(depressed_dic, tag == 4, select=c(cleaned_word))$cleaned_word
depressed_dic5 <- subset(depressed_dic, tag == 5, select=c(cleaned_word))$cleaned_word
depressed_dic6 <- subset(depressed_dic, tag == 6, select=c(cleaned_word))$cleaned_word
depressed_dic7 <- subset(depressed_dic, tag == 7, select=c(cleaned_word))$cleaned_word
depressed_dic8 <- subset(depressed_dic, tag == 8, select=c(cleaned_word))$cleaned_word
depressed_dic9 <- subset(depressed_dic, tag == 9, select=c(cleaned_word))$cleaned_word

depressed_dic1 %>% strsplit(',') %>% unlist -> dep_dic1
depressed_dic2 %>% strsplit(',') %>% unlist -> dep_dic2
depressed_dic3 %>% strsplit(',') %>% unlist -> dep_dic3
depressed_dic4 %>% strsplit(',') %>% unlist -> dep_dic4
depressed_dic5 %>% strsplit(',') %>% unlist -> dep_dic5
depressed_dic6 %>% strsplit(',') %>% unlist -> dep_dic6
depressed_dic7 %>% strsplit(',') %>% unlist -> dep_dic7
depressed_dic8 %>% strsplit(',') %>% unlist -> dep_dic8
depressed_dic9 %>% strsplit(',') %>% unlist -> dep_dic9

# list(
#   s1우울기분 = c("혼자", "기분", "싫다","아프다", "마음", "행복하다", "우울증",
#              "우울하다"),
#   s2흥미감소 = c("공부", "싫다"),
#   s3체중변화 = c("버리다", "먹다", "약", "안", "많이", "살"),
#   s4수면장애 = c("자다", "많이", "잠", "안"),
#   s5초조지연 = c("시간", "공부", "시험", "무섭디"),
#   s6활력상실 = c("공부", "만나다"),
#   s7무가치감 = c("혼자", "친구", "동생", "부모님"),
#   s8집중력감소 = c("공부"),
#   s9자살사고 = c("자살", "살다", "살", "죽다")
# ) -> ldaterm_dsm5_l

# seed word 구성
list() -> ldaterm_dsm5_l


#s1
seed_vec <- c()
for (dep in dep_dic1){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s1=seed_vec))

#s2
seed_vec <- c()
for (dep in dep_dic2){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s2=seed_vec))

#s3
seed_vec <- c()
for (dep in dep_dic3){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s3=seed_vec))

#s4
seed_vec <- c()
for (dep in dep_dic4){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s4=seed_vec))

#s5
seed_vec <- c()
for (dep in dep_dic5){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s5=seed_vec))

#s6
seed_vec <- c()
for (dep in dep_dic6){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s6=seed_vec))

#s7
seed_vec <- c()
for (dep in dep_dic7){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s7=seed_vec))

#s8
seed_vec <- c()
for (dep in dep_dic8){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s8=seed_vec))

#s9
seed_vec <- c()
for (dep in dep_dic9){
  if (dep %in% keywords_vec){
    seed_vec <- c(seed_vec, dep)
  }
}
seed_vec<- unique(seed_vec)
ldaterm_dsm5_l <- c(ldaterm_dsm5_l, list(s9=seed_vec))

ldaterm_dsm5_l

c(ldaterm_dsm5_l) -> ldaterm_l
dictionary(ldaterm_l) -> dict_topic

set.seed(37)
Sys.time() -> t1
c_dfm %>%
  textmodel_seededlda(dictionary = dict_topic) -> c_slda
Sys.time() -> t2
t2 - t1

terms(c_slda, 9)

c_slda$theta %>% as.data.frame() -> cslda_theta_df

quanteda::docvars(c_corp) -> docvars_df

bind_cols(docvars_df, cslda_theta_df) %>%
  mutate(textID = row_number(), .before = ID) -> slda_theta_df

slda_theta_df %>% glimpse()

slda_theta_df %>%
  writexl::write_xlsx("slda_theta_df.xlsx")

slda_theta_df %>%
  arrange(s1 %>% desc) %>%
  select(text) %>%
  head(10)

slda_theta_df %>%
  arrange(s1 %>% desc) %>% head(5) %>% .$textID -> id_v

slda_theta_df %>%
  filter(textID %in% id_v) %>%
  pull(text)

topics(c_slda) -> r
c(r) -> result
y.matrix = matrix(data = result, ncol = 1, byrow = TRUE)
y.df = as.data.frame(y.matrix)

# seeded LDA 결과 저장
cbind(slda_theta_df, y.df) -> result_df
names(result_df)
names(result_df)[14] <- c("label")
result_df %>%
  writexl::write_xlsx("slda_result_0603.xlsx")

# 일반 LDA 성능과 seeded LDA 성능 비교(어느 쪽이 cluster가 더 잘 형성 되었는지)
c_lda %>% divergence(weighted = TRUE, min_size = 0.01, select = NULL)->r
print(r)
c_slda %>% divergence(weighted = TRUE, min_size = 0.01, select = NULL)->sr
print(sr)


