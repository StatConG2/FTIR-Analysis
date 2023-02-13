library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)

# load data
df = read_excel("Training Set Data.xls", sheet = "Legend",col_names = FALSE)


sheet_names = df$...2[-1]
full_df1 = data.frame(id = c(1:5051))

j = 1

for (i in sheet_names) {
  df = read_excel("Training Set Data.xls", sheet = i,
                  col_names = FALSE)
  if(j == 1){
    df = df[-c(1,2,3,4,5,6), ]
    colnames(df) <- c("wave_len",sheet_names[j])
    full_df1 = cbind(full_df1, df %>% mutate_if(is.character,as.numeric))
  }else{
    df = df[-c(1,2,3,4,5,6), 2]
    colnames(df) <- sheet_names[j]
    full_df1 = cbind(full_df1, df%>% mutate_if(is.character,as.numeric))
  }
  j = j + 1
}

melamine_con <- read_excel("melamine.con.xlsx", 
                           sheet = "100%")

av_con <- read_excel("melamine.con.xlsx", 
                     sheet = "0%")

# merge pure AV n pure melamine

full_df2 <- full_df1 %>% merge(melamine_con,by.x = "wave_len",by.y="cm-1")%>% 
  merge(av_con,by.x = "wave_len",by.y="cm-1")  

full_df3 <- full_df1$wave_len

for(i in 3:82){
  min.val <- min(full_df2[,i,drop=TRUE])
  max.val <- max(full_df2[,i,drop=TRUE])
  trans.val <- (full_df2[,i,drop=TRUE]-min.val)/(max.val-min.val)
  full_df3 <- data.frame(full_df3,trans.val)
}

colnames(full_df3)[-1] <- c(paste0(rep("con",times=20),rep(c(1:20),each=4),rep("rep",times=80),rep(c(1:4),times=20)))

full_df3 <- as_tibble(full_df3)
colnames(full_df3)[1] <- "wave_len"

full_df4 <- tibble(full_df3$wave_len)

x = seq(2,81,4)

# replicate 1

z=1
for (i in x) {
  sub_df = full_df3[,c(1,i)]
  colnames(sub_df) <- c("Wave Length", "Absorbance")
  csv_name = sprintf("%srep1.csv",LETTERS[seq(from=1, to=20)][z])
  write.csv(sub_df,csv_name, row.names = F)
  z = z +1 
}

# replicate 2

z=1
for (i in x) {
  sub_df = full_df3[,c(1,(i+1))]
  colnames(sub_df) <- c("Wave Length", "Absorbance")
  csv_name = sprintf("%srep2.csv",LETTERS[seq(from=1, to=20)][z])
  write.csv(sub_df,csv_name, row.names = F)
  z = z +1 
}

# replicate 3

z=1
for (i in x) {
  sub_df = full_df3[,c(1,(i+2))]
  colnames(sub_df) <- c("Wave Length", "Absorbance")
  csv_name = sprintf("%srep3.csv",LETTERS[seq(from=1, to=20)][z])
  write.csv(sub_df,csv_name, row.names = F)
  z = z +1 
}

# replicate 4

z=1
for (i in x) {
  sub_df = full_df3[,c(1,(i+3))]
  colnames(sub_df) <- c("Wave Length", "Absorbance")
  csv_name = sprintf("%srep4.csv",LETTERS[seq(from=1, to=20)][z])
  write.csv(sub_df,csv_name, row.names = F)
  z = z +1 
}


