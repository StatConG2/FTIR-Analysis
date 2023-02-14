# Load packages
library(pls)
library(readxl)
library(magrittr)
library(dplyr)
library(caret)
library(ggplot2)

# Load data
abs_values <- read.csv("abs.values.csv")

# Apply plsr with 100% melamine concentration data using LOOCV

df = read_xls("Training Set Data.xls", 
              sheet = "Legend",col_names = FALSE)

concentration <- df$...1[!is.na(df$...1)][-1]
concentration <- rep(c(as.numeric(substring(concentration,1,nchar(concentration)-1)),100,0),each=4)

# Model covered signature region (450-1500 cm-1)

full_df <- matrix(1:80)

for(i in 4:1050){
  full_df <- cbind(full_df,abs_values[,i,drop=TRUE])
}

# Fit a plsr model
set.seed(1)
full_df <- full_df[,-1]
df.pls <- plsr(concentration ~ full_df, 12, validation = "LOO")
summary(df.pls)

# Scree plot
validationplot(df.pls)

# Predict values
plsr_pred <- predict(df.pls, full_df, ncomp=12)

# RMSE
RMSE(plsr_pred,concentration)

# Plot actual vs. fitted data (without 100% melamine)
df1 <- tibble(actual=concentration,fitted=plsr_pred) %>% filter(actual != 100)

ggplot(data=df1,aes(x=actual,y=fitted)) +
  geom_point()



# -------------------------------------------------------------------------------------
# Model covered peak region (3400-3500 cm-1)

full_df1 <- matrix(1:80)

for(i in 2950:3050){
  full_df1 <- cbind(full_df1,abs_values[,i,drop=TRUE])
}

# Fit a plsr model
set.seed(1)
full_df1 <- full_df1[,-1]
df.pls <- plsr(concentration ~ full_df1, 12, validation = "LOO")
summary(df.pls)

# Scree plot
validationplot(df.pls)

# Predict values
plsr_pred1 <- predict(df.pls, full_df1, ncomp=12)

# RMSE
RMSE(plsr_pred1,concentration)

# Plot actual vs. fitted data (without 100% melamine)
df2 <- tibble(actual=concentration,fitted=plsr_pred1) %>% filter(actual != 100)

ggplot(data=df2,aes(x=actual,y=fitted)) +
  geom_point()
