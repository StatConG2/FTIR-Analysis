library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)


#------------------------------------------------------------------------------------------
# Create test data set
# load data

df1.test = read_excel("C://Users//Anjana Senadeera//Downloads//Test Set Data - Final.xls", sheet = "Legend ", 
                      col_names = FALSE)

concentration.test <- df1.test$...1[!is.na(df1.test$...1)][-1]
concentration.test <- c(as.numeric(substring(concentration.test,1,nchar(concentration.test)-1)))

sheet_names = df1.test$...2[-1]
full_df.test = data.frame(wave_len = c(450:5500))

j = 1

for (i in sheet_names) {
  df.test = read_excel("C://Users//Anjana Senadeera//Downloads//Test Set Data - Final.xls",
                       sheet = i, col_names = FALSE)
  
  df.test = df.test[-c(1,2,3,4,5,6), ]
  colnames(df.test) <- c("wave_len",sheet_names[j])
  full_df.test = left_join(full_df.test, df.test %>% mutate_if(is.character,as.numeric),by="wave_len")
  
  j = j + 1
}

full_df2.test <- full_df.test$wave_len

for(i in 2:69){
  min.val.test <- min(full_df.test[,i,drop=TRUE])
  max.val.test <- max(full_df.test[,i,drop=TRUE])
  trans.val.test <- (full_df.test[,i,drop=TRUE]-min.val.test)/(max.val.test-min.val.test)
  full_df2.test <- data.frame(full_df2.test,trans.val.test)
}

colnames(full_df2.test)[-1] <- c(paste0(rep("con",times=17),rep(c(1:17),each=4),rep("rep",times=68),rep(c(1:4),times=17)))

full_df2.test <- as_tibble(full_df2.test)
colnames(full_df2.test)[1] <- "wave_len"

# spectral data set

full_df_rep.test <- tibble(mel.concentration=rep(concentration.test,each=4),rep=paste0(rep("rep",times=68),rep(1:4,times=17)))
full_df_rep1.test <- c()
 
for(i in 2:69){
  rep.data.test <- full_df2.test[,i,drop=TRUE] 
  full_df_rep1.test <- rbind(full_df_rep1.test,rep.data.test)
}

colnames(full_df_rep1.test) <- full_df2.test$wave_len
full_df_rep.test <- full_df_rep.test %>% cbind(as_tibble(full_df_rep1.test)) 

write.csv(full_df_rep.test,"C://Users//Anjana Senadeera//Downloads//test.set.csv")
