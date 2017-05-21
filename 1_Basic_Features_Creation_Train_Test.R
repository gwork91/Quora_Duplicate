# Basic features set creation
# Few commands have been left optional for the user and are commmented

reqPackages <- c("tm","qdap","quanteda","lda","LDAvis","stringr","qdapDictionaries","stringdist","Matrix","stringi","magrittr",
				   "dplyr","tidytext","h2o")				   
lapply(reqPackages, require, character.only=TRUE)

#setwd("C:\\Users\\Palash Goyal\\Downloads\\Quora_Duplicate")

# Load Train and Test datasets
train_data  <- read.csv("train.csv")
train_data <- train_data[,c("id","qid1","qid2","question1","question2","is_duplicate")]
		#save(x=train_data,file="train_data.RData")
		#load("train_data.RData")

test_data  <- read.csv("test.csv")
test_data <- test_data[,c("test_id","question1","question2")]
		#save(x=test_data,file="test_data.RData")
		#load("test_data.RData")

# Punc-lower-stopword-stem-stripSpace
col_func <- function(input_col){
	input_col <- input_col	%>% 
		{gsub("[[:digit:]]"," ", .)}  %>% 
		{gsub("[[:punct:]]"," ", .)} %>% 
		{gsub("http\\w+"," ", .)} %>% 
		{gsub("[ \t]{2,}"," ", .)} %>% 
		{gsub("^\\s+|\\s+$"," ", .)} 
		
	corp <- Corpus(VectorSource(input_col))
	corp <- tm_map(corp,removePunctuation)
	corp <- tm_map(corp, content_transformer(removeNumbers))
	corp <- tm_map(corp,tolower)

	stop_words <- c("ax","dir","na","i","you","edu","s","t","m","can","lines","re","what","there","all","we","one","the","of","or","in","for","by","on","but","is","in","-","ã","a","not","with","as","was","if","they","are","this","and","it","have","from","at","my","be","by","not","that","to","div","may","please","ok","ltr","n","don","na","\u0081","a","an",",","from","com","org","like","likes","so")				   
	corp <- tm_map(corp, removeWords, c(stopwords("english"),stop_words)) 
	corp <- tm_map(corp, content_transformer(stemDocument))
	corp <- tm_map(corp, content_transformer(stripWhitespace))
	final_col <- data.frame(content = sapply(corp, function(i) i), stringsAsFactors = FALSE)
	#rm(corp, input_col)
	
	return(final_col$content)
	
}

# Feature creation : word count, unique word & char count in original & reformed cols, common word count , etc.
data_process <- function(input_df){
	input_df[,"question1"] <- as.character(input_df[,"question1"])
	input_df[,"question2"] <- as.character(input_df[,"question2"])
	input_df$question1_form <- col_func(input_df[,"question1"])
	input_df$question2_form <- col_func(input_df[,"question2"])
print("done with col_func")
	
	# Count of words in the reformed q1 and q2 cols
	input_df$q1_form_len <- sapply(str_match_all(input_df[,"question1_form"], "\\S+"), length)
	input_df$q2_form_len <- sapply(str_match_all(input_df[,"question2_form"], "\\S+"), length)
print("done with word count")
	
	# Count of words in the original q1 and q2 cols
	input_df$q1_length <- sapply(str_match_all(input_df[,"question1"], "\\S+"), length)
	input_df$q2_length <- sapply(str_match_all(input_df[,"question2"], "\\S+"), length)
print("done with word count in original")
	
	# Count of unique words in the original q1 and q2 cols
	input_df$q1_unique <- as.numeric(sapply(input_df$question1, function(x) length(as.vector(table(unlist(strsplit(x," ")))) )))
	input_df$q2_unique <- as.numeric(sapply(input_df$question2, function(x) length(as.vector(table(unlist(strsplit(x," ")))) )))
print("done with unique word count")
	
	# Count of unique words in the reformed q1 and q2 cols
	input_df$q1_form_uni <- as.numeric(sapply(input_df$question1_form, function(x) length(as.vector(table(unlist(strsplit(x," ")))) )))
	input_df$q2_form_uni <- as.numeric(sapply(input_df$question2_form, function(x) length(as.vector(table(unlist(strsplit(x," ")))) )))
print("done with unique word count in reformed")
	
	# Count of number of characters in reformed q1 and q2 cols without spaces
	char_cnt <- function(x) stri_length(x) - stri_count_fixed(x, " ")
	input_df$q1_form_char <- as.numeric(sapply(input_df[,"question1_form"], char_cnt))
	input_df$q2_form_char <- as.numeric(sapply(input_df[,"question2_form"], char_cnt))
	input_df$q1_q2_char_diff <- abs(input_df$q1_form_char - input_df$q2_form_char)		# difference in characters for question1_form and question2_form
print("done with char count in reformed")
	
	# Count of unique common words in both q1 and q2 
	common_word <- function(a,b) { 
	 return(length(intersect(unlist(strsplit(a," ") ) , unlist(strsplit(b," "))))) }
	input_df$common_cnt <- as.numeric(mapply(common_word, input_df$question1_form, input_df$question2_form ))
print("done with common count")
	
	# Percent of common words (with unique)
	input_df$prcnt_common <- round(input_df$common_cnt / (input_df$q1_form_uni + input_df$q2_form_uni) ,2)
print("done with percent")	
	
	return(input_df)
}


train_data <- data_process(train_data)
save(x=train_data, file="train_data_ff.RData")
		# load("train_data_ff.RData")  			# File split in 2, issue of jupyter hub 25mb
		# train_data1 <- train_data[1:202145,]
		# train_data2 <- train_data[202146:404290,]
		# write.csv(x=train_data1, file="train_data1.csv", row.names = FALSE)
		# write.csv(x=train_data2, file="train_data2.csv", row.names = FALSE)

test_data <- data_process(test_data)
save(x=test_data, file="test_data_ff.RData")
#load("test_data_ff.RData")

# save only the text reformed columns
test_text_data <- test_data[,c("test_id","question1_form","question2_form")]
save(x=test_text_data, file="test_text_data.RData")
write.csv(x=test_text_data, file="test_text_data.csv", row.names = FALSE)

# select only the relevant cols for the model :
sel_cols <- c("test_id","q1_form_len","q2_form_len","q1_length","q2_length","q1_unique","q2_unique","q1_form_uni","q2_form_uni","q1_form_char","q2_form_char","q1_q2_char_diff","common_cnt","prcnt_common")
test_data_cols <- test_data[,sel_cols]
save(x=test_data_cols, file="test_data_cols.RData")
		# test_data_cols1 <- test_data_cols[c(1:(dim(test_data_cols)[1]/2)),]
		# test_data_cols2 <- test_data_cols[c(((dim(test_data_cols)[1]/2)+1):(dim(test_data_cols)[1])),]
		# write.csv(x=test_data_cols1, file="test_data_cols1.csv", row.names = FALSE)
		# write.csv(x=test_data_cols2, file="test_data_cols2.csv", row.names = FALSE)	
		
		
		
# TF-IDF CREATION : Not used as of now!(https://cran.r-project.org/web/packages/tidytext/vignettes/tf_idf.html)
# Question_1_form
#tfidf_q1 <- train_data %>% unnest_tokens(word, question1_form, drop=F, token="words") %>% count(id, word) 
#colnames(tfidf_q1) <- c("id","words_que1","que1_words_cnt")
#tfidf_q1 <- merge(x= tfidf_q1, y=train_data, by = "id", all.x=TRUE)
#tf_idf_que1 <- tfidf_q1 %>% bind_tf_idf(words_que1, question1_form, que1_words_cnt)
		#save(x=tf_idf_que1, file="tf_idf_que1.RData")
		#load("tf_idf_que1.RData")

# TF-IDF scores for Question_2_form
#tfidf_q2 <- train_data %>% unnest_tokens(word, question2_form, drop=F, token="words") %>% count(id, word) 
#colnames(tfidf_q2) <- c("id","words_que2","que2_words_cnt")
#tfidf_q2 <- merge(x= tfidf_q2, y=train_data, by = "id", all.x=TRUE)
#tf_idf_que2 <- tfidf_q2 %>% bind_tf_idf(words_que2, question2_form, que2_words_cnt)
		#save(x=tf_idf_que2, file="tf_idf_que2.RData")
		#load("tf_idf_que2.RData")
