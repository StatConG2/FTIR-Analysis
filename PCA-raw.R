library(devtools)
install_github("vqv/ggbiplot") # You may need to install Rtools 4.2

######################################################################
# Without 100% Melamine sample
newpca <- as.data.frame(abs_values[-c(19),])
#View(newpca)
mix_status2 <- c(rep("adultrated", 18), "pure AV")
newpca <- cbind(newpca, mix_status2)

spectrum.pca <- prcomp(newpca, center = TRUE,scale. = TRUE)
summary(spectrum.pca)
spectrum.pca$x

ggbiplot(spectrum.pca,choices = c(1,3), var.axes = FALSE, labels= row.names(newpca), 
         ellipse = TRUE, groups = mixstatus_2 )

#########################################################################
# For full wave range 450:5051

newpca2 <- as.data.frame(abs_values)
#View(newpca)
mix_status <- c(rep("adultrated", 18), "pure Melamine", "pure AV")
newpca2 <- cbind(newpca2, mix_status)

spectrum.pca2 <- prcomp(newpca2[,-c(5052)], center = TRUE, scale. = TRUE)
summary(spectrum.pca2)
spectrum.pca2$x

ggbiplot(spectrum.pca2, choices = c(1,2), var.axes = FALSE, labels= row.names(newpca2), 
         ellipse = TRUE, groups = mix_status)



# For 3300:3700 region

newpca3 <- as.data.frame(abs_values[c(1, 2851:3250)])
View(newpca3)
mix_status <- c(rep("adultrated", 18), "pure Melamine", "pure AV")
newpca3 <- cbind(newpca3, mix_status)


spectrum.pca3 <- prcomp(newpca3[,-c(402)], center = FALSE, scale. = TRUE)
summary(spectrum.pca3)
#spectrum.pca3$x

ggbiplot(spectrum.pca3, choices = c(1,3), var.axes = TRUE, labels= newpca3$concentration, 
         ellipse = TRUE, groups = mix_status)




# For 450:1500 region

newpca4 <- as.data.frame(abs_values[,c(3301:3700)])
#View(newpca)
mix_status <- c(rep("adultrated", 18), "pure Melamine", "pure AV")
newpca3 <- cbind(newpca3, mix_status)
mix_status

spectrum.pca3 <- prcomp(newpca3[,-c(5053)], center = TRUE, scale. = TRUE)
summary(spectrum.pca3)
spectrum.pca3$x

ggbiplot(spectrum.pca3, choices = c(1,3), var.axes = TRUE, labels= row.names(newpca3), 
         ellipse = TRUE, groups = mix_status)







