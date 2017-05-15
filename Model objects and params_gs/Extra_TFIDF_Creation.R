# TF-IDF CREATION : Not used as of now!(https://cran.r-project.org/web/packages/tidytext/vignettes/tf_idf.html)


load("train_data_ff.RData")
	

# Question_1_form
tfidf_q1 <- train_data %>% unnest_tokens(word, question1_form, drop=F, token="words") %>% count(id, word) 
colnames(tfidf_q1) <- c("id","words_que1","que1_words_cnt")
tfidf_q1 <- merge(x= tfidf_q1, y=train_data, by = "id", all.x=TRUE)
tf_idf_que1 <- tfidf_q1 %>% bind_tf_idf(words_que1, question1_form, que1_words_cnt)
		#save(x=tf_idf_que1, file="tf_idf_que1.RData")
		#load("tf_idf_que1.RData")

# TF-IDF scores for Question_2_form
tfidf_q2 <- train_data %>% unnest_tokens(word, question2_form, drop=F, token="words") %>% count(id, word) 
colnames(tfidf_q2) <- c("id","words_que2","que2_words_cnt")
tfidf_q2 <- merge(x= tfidf_q2, y=train_data, by = "id", all.x=TRUE)
tf_idf_que2 <- tfidf_q2 %>% bind_tf_idf(words_que2, question2_form, que2_words_cnt)
		#save(x=tf_idf_que2, file="tf_idf_que2.RData")
		#load("tf_idf_que2.RData")
		
		
