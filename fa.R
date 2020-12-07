library(ggplot2)
library(dplyr)
require(tidyverse)
require(randomForest)
##Cleaning the data
df = read.csv('fa.csv')
head(df)
names(df)
names(df) <- c('name','gender','age','importance','fitness_level','exercise_f','excuse',
               'type','size','time','length','diet','diet_excuse','healthy','friends','equipment',
               'motivation')
df$gender <- as.factor(df$gender)
df$age <- as.factor(df$age)
df$importance <- as.factor(df$importance)
df$fitness_level <- as.factor(df$fitness_level)
df$exercise_f <- as.factor(df$exercise_f)
df$size <- as.factor(df$size)
df$time <- as.factor(df$time)
df$length <- as.factor(df$length)
df$diet <- as.factor(df$diet)
df$healthy <- as.factor(df$healthy)
df$friends <- as.factor(df$friends)
df$equipment <- as.factor(df$equipment)

#transform data structure to turn multiple choices answers into individual variable.

#Transforming variable:What barriers, if any, prevent you from exercising more regularly? (Select all that applies)
#resulting in new dataset called 'excu' containing 6 variables, each variable represent one unique answer
#to the original survey question.The value '0' from each row means that particular answers was not selected
#at the original survey question, and vice versa.
lev <- levels(factor(df$excuse))
lev <- unique(unlist(strsplit(lev, ";")))
mnames <- gsub(" ", "_", paste("exc", lev, sep = "."))
excu <- matrix(data = "0", nrow = length(df$excuse), ncol = length(lev))
char.var <- as.character(df$excuse)
for (i in 1:length(lev)) {
  excu[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
}
excu <- data.frame(excu, stringsAsFactors = TRUE)
colnames(excu) <- mnames
summary(excu)

excu <- excu[,c("exc.I'll_become_too_tired","exc.I_don't_have_enough_time"
                ,"exc.I_can't_stay_motivated","exc.I_have_an_injury"
                ,"exc.I_don't_really_enjoy_exercising","exc.I_exercise_regularly_with_no_barriers")]

#Transforming variable:What forms of exercise do you currently participate in? (Select all that applies)
#resulting in new dataset called 'ty' containing 8 variables, each variable represent one unique answer
#to the original survey question.The value '0' from each row means that particular answers was not selected
#at the original survey question, and vice versa.
lev1 <- levels(factor(df$type))
lev1 <- unique(unlist(strsplit(lev1, ";")))
mnames1 <- gsub(" ", "_", paste("type", lev1, sep = "."))
ty <- matrix(data = "0", nrow = length(df$type), ncol = length(lev1))
char.var1 <- as.character(df$type)
for (i in 1:length(lev1)) {
  ty[grep(lev1[i], char.var1, fixed = TRUE), i] <- "1"
}
ty <- data.frame(ty, stringsAsFactors = TRUE)
colnames(ty) <- mnames1
summary(ty)

#Transforming variable:What prevents you from eating a healthy balanced diet, if any? (Select all that applies)
#resulting in new dataset called 'dit' containing 5 variables, each variable represent one unique answer
#to the original survey question.The value '0' from each row means that particular answers was not selected
#at the original survey question, and vice versa.
lev2 <- levels(factor(df$diet_excuse))
lev2 <- unique(unlist(strsplit(lev2, ";")))
mnames2 <- gsub(" ", "_", paste("diet", lev2, sep = "."))
dit <- matrix(data = "0", nrow = length(df$diet_excuse), ncol = length(lev2))
char.var2 <- as.character(df$diet_excuse)
for (i in 1:length(lev2)) {
  dit[grep(lev2[i], char.var2, fixed = TRUE), i] <- "1"
}
dit <- data.frame(dit, stringsAsFactors = TRUE)
colnames(dit) <- mnames2
summary(dit)
dit <- dit[,c("diet.Lack_of_time","diet.Ease_of_access_to_fast_food","diet.Cost","diet.Temptation_and_cravings"
              ,"diet.I_have_a_balanced_diet")]

#Transforming variable:What motivates you to exercise?
#resulting in new dataset called 'motiv' containing 5 variables, each variable represent one unique answer
#to the original survey question.The value '0' from each row means that particular answers was not selected
#at the original survey question, and vice versa.
lev3 <- levels(factor(df$motivation))
lev3 <- unique(unlist(strsplit(lev3, ";")))
mnames3 <- gsub(" ", "_", paste("moti", lev3, sep = "."))
motiv <- matrix(data = "0", nrow = length(df$motivation), ncol = length(lev3))
char.var3 <- as.character(df$motivation)
for (i in 1:length(lev3)) {
  motiv[grep(lev3[i], char.var3, fixed = TRUE), i] <- "1"
}
motiv <- data.frame(motiv, stringsAsFactors = TRUE)
colnames(motiv) <- mnames3
summary(motiv)
motiv <- motiv[,c("moti.I_want_to_be_fit","moti.I_want_to_increase_muscle_mass_and_strength"
                  ,"moti.I_want_to_lose_weight","moti.I_want_to_be_flexible"
                  ,"moti.I_want_to_relieve_stress"
                  ,"moti.I_want_to_achieve_a_sporting_goal"
                  ,"moti.I'm_sorry_..._I'm_not_really_interested_in_exercising")]
summary(motiv)

#combining transformed dataset with original dataset with original survey questions
#as variables columns deleted.

df <- cbind(df[,-c(7,8,13,17)],excu,ty,dit,motiv)

head(df)
