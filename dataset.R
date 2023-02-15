library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)

# Create train data set
# load data
df1.train = read_excel("Training Set Data.xls", sheet = "Legend",col_names = FALSE)


concentration.train <- df1.train$...1[!is.na(df1.train$...1)][-1]
concentration.train <- c(as.numeric(substring(concentration.train,1,nchar(concentration.train)-1)),100,0)


sheet_names = df1.train$...2[-1]
full_df1.train = data.frame(wave_len = c(450:5500))

j = 1

for (i in sheet_names) {
  df.train = read_excel("Training Set Data.xls", sheet = i,
                  col_names = FALSE)
  
    df.train = df.train[-c(1,2,3,4,5,6), ]
    colnames(df.train) <- c("wave_len",sheet_names[j])
    full_df1.train = left_join(full_df1.train, df.train %>% mutate_if(is.character,as.numeric),by="wave_len")
    
  j = j + 1
}

melamine_con.train <- read_excel("melamine.con.xlsx", 
                           sheet = "100%")

av_con.train <- read_excel("melamine.con.xlsx", 
                     sheet = "0%")

# merge pure AV n pure melamine

full_df2.train <- full_df1.train %>% merge(melamine_con.train,by.x = "wave_len",by.y="cm-1")%>% 
  merge(av_con.train,by.x = "wave_len",by.y="cm-1")  

full_df3.train <- full_df1.train$wave_len

for(i in 2:81){
  min.val.train <- min(full_df2.train[,i,drop=TRUE])
  max.val.train <- max(full_df2.train[,i,drop=TRUE])
  trans.val.train <- (full_df2.train[,i,drop=TRUE]-min.val.train)/(max.val.train-min.val.train)
  full_df3.train <- data.frame(full_df3.train,trans.val.train)
}

colnames(full_df3.train)[-1] <- c(paste0(rep("con",times=20),rep(c(1:20),each=4),rep("rep",times=80),rep(c(1:4),times=20)))

full_df3.train <- as_tibble(full_df3.train)
colnames(full_df3.train)[1] <- "wave_len"


# spectral data set

full_df_rep.train <- tibble(mel.concentration=rep(concentration.train,each=4),rep=paste0(rep("rep",times=80),rep(1:4,times=20)))
full_df_rep1.train <- c()

for(i in 2:81){
  rep.data.train <- full_df3.train[,i,drop=TRUE] 
  full_df_rep1.train <- rbind(full_df_rep1.train,rep.data.train)
}

colnames(full_df_rep1.train) <- full_df3.train$wave_len
full_df_rep.train <- full_df_rep.train %>% cbind(as_tibble(full_df_rep1.train)) 

write.csv(full_df_rep.train,"train.set.csv")


#------------------------------------------------------------------------------------------
# Create test data set
# load data
df1.test = read_excel("Test Set Data.xls", sheet = "Legend", col_names = FALSE)

concentration.test <- df1.test$...1[!is.na(df1.test$...1)][-1]
concentration.test <- c(as.numeric(substring(concentration.test,1,nchar(concentration.test)-1)))

sheet_names = df1.test$...2[-1]
full_df.test = data.frame(wave_len = c(450:5500))

j = 1

for (i in sheet_names) {
  df.test = read_excel("Test Set Data.xls", sheet = i, col_names = FALSE)
  
  df.test = df.test[-c(1,2,3,4,5,6), ]
  colnames(df.test) <- c("wave_len",sheet_names[j])
  full_df.test = left_join(full_df.test, df.test %>% mutate_if(is.character,as.numeric),by="wave_len")
  
  j = j + 1
}

full_df2.test <- full_df.test$wave_len

for(i in 2:41){
  min.val.test <- min(full_df.test[,i,drop=TRUE])
  max.val.test <- max(full_df.test[,i,drop=TRUE])
  trans.val.test <- (full_df.test[,i,drop=TRUE]-min.val.test)/(max.val.test-min.val.test)
  full_df2.test <- data.frame(full_df2.test,trans.val.test)
}

colnames(full_df2.test)[-1] <- c(paste0(rep("con",times=10),rep(c(1:10),each=4),rep("rep",times=40),rep(c(1:4),times=10)))

full_df2.test <- as_tibble(full_df2.test)
colnames(full_df2.test)[1] <- "wave_len"

# spectral data set

full_df_rep.test <- tibble(mel.concentration=rep(concentration.test,each=4),rep=paste0(rep("rep",times=40),rep(1:4,times=10)))
full_df_rep1.test <- c()

for(i in 2:41){
  rep.data.test <- full_df2.test[,i,drop=TRUE] 
  full_df_rep1.test <- rbind(full_df_rep1.test,rep.data.test)
}

colnames(full_df_rep1.test) <- full_df2.test$wave_len
full_df_rep.test <- full_df_rep.test %>% cbind(as_tibble(full_df_rep1.test)) 

write.csv(full_df_rep.test,"test.set.csv")
