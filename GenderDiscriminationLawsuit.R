
# 1. data cleaning
# (set your own directory!)
setwd("/Users/eleanorli/Desktop/AN6003 Course Materials/Graded Team Assignment - Gender Discrimination Lawsuit")

library(data.table)
library(caTools)

df <- fread("Lawsuit.csv")

sum(is.na(df))

df$Dept <- factor(df$Dept,levels = c(1,2,3,4,5,6),
                  labels = c('Biochemistry/Molecular Biology',
                             'Physiology','Genetics','Pediatrics',
                             'Medicine','Surgery'))
df$Gender <- factor(df$Gender,levels = c(0,1),labels = c('Female','Male'))
df$Clin <- factor(df$Clin,levels = c(0,1),labels = c('clinical','research'))
df$Cert <- factor(df$Cert,levels = c(0,1),labels = c('not certified','Board certified'))
df$Rank1 <- factor(df$Rank,levels = c(1,2,3),
                  labels = c('Assistant','Associate','Full professor'))
df$Rank2 <- ifelse(df$Rank == '3','Full professor','Other')
df$Rank2 <- factor(df$Rank2)
df$SalIncrement <- df$Sal95 - df$Sal94
df$Rank2 <- relevel(df$Rank2,ref = 'Other')

#----------------------------------------------------------------------------------------

# 2. descriptive(rank)
# 2.1 Gender
library(ggplot2)
ggplot(data = df,aes(x = Gender,fill = Rank2)) + geom_bar(position = "fill")+
  scale_fill_manual(values = c("Full professor" = "#ACD5EB", "Other" = "#F2F2F2"))+
  labs(title = 'Number of Full Professors/Other by Gender') + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),  
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5)   
  )
# Result: In the male group, the full professor accounts for nearly half while this occupation only accounts for less than 25% in the female group. Overall, the proportion of men becoming full professors is higher than that of women.

# 2.2 Reasons why female are underrepresented in full professor positions compared to male
# 2.2.1 Exper
ggplot(data=df, aes(x=Exper,fill=Rank2)) +
  scale_fill_manual(values = c("Full professor" = "#ACD5EB", "Other" = "#F2F2F2"))+
  geom_histogram(binwidth = 3,colour = '#8C8C8C') + 
  labs(title = "Frequency Distribution of Exper by rank", 
       x = "Exper", 
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5) 
  )

ggplot(data=df,aes(x=Gender,y=Exper,fill=Gender)) + geom_boxplot()+
  scale_fill_manual(values = c("Female" = "#ACD5EB", "Male" = "#F2F2F2"))+
  theme_minimal() +
  labs(title = "Exper Distribution Compared by Gender") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5) 
  )
# Result: The full professor level tends to represent a range in 10 to 20 experience years, compared to others which are more concentrated in the range of 5 to 10 years. If focusing on gender, it is found that the male group shows a boarder range of experience years, specifically, its higher median value indicates that men generally have more years of experience compared to women. In this regard, the less presence of females in the full professor position could be explained by their relatively lower experience levels.


# 2.2.2 Cert
ggplot(data = df,aes(x = Cert,fill = Rank2)) + geom_bar(position = "fill")+
  scale_fill_manual(values = c("Full professor" = "#ACD5EB", "Other" = "#F2F2F2"))+
  labs(title = 'Number of Full Professors/Other by Cert') + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
    axis.title.x = element_text(size = 12.5), 
    axis.title.y = element_text(size = 12.5) 
  )

ggplot(data = df,aes(x = Gender,fill = Cert)) + geom_bar(position = "fill")+
  scale_fill_manual(values = c("Board certified" = "#ACD5EB", "not certified" = "#F2F2F2"))+
  labs(title = 'Proportion of Cert by Gender') + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5) 
  )
# Result: The proportion of doctors with board certified who are full professors is higher than those who are not certified, which implies that doctors who are certified are more likely to become full professors. When focusing on the gender factor, it is found that the overall certification rate of men is higher than that of women. In this regard, the proportion of women with board certified is lower, which would explain why they are underrepresented in full professor positions.

# 2.3 The threshold for women to be promoted to full professor is lower
# 2.3.1 Exper
ggplot(data = df,aes(x = Rank1, y = Exper, fill = Gender)) + geom_boxplot()+
  scale_fill_manual(values = c("Female" = "#ACD5EB", "Male" = "#F2F2F2"))+
  labs(title = 'Boxplot of Exper by Rank Compared in Gender') + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
    axis.title.x = element_text(size = 12.5),
    axis.title.y = element_text(size = 12.5)   
  )
# Result: Regardless of the position, women tends to have less experience than men, which could imply that women are promoted with relatively less experience in the same position, hence, the promotion threshold for women is relatively low.

# 2.3.2 Cert
ggplot(data = df,aes(x = Rank1,fill = Cert)) + geom_bar(position = "fill")+
  scale_fill_manual(values = c("Board certified" = "#ACD5EB", "not certified" = "#F2F2F2"))+
  labs(title = 'Barchart of Cert by Rank Compared in Gender') + facet_grid(Gender~.)+ 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),  
    axis.title.x = element_text(size = 12.5), 
    axis.title.y = element_text(size = 12.5)   
  )
# Result: In the full professor position, the rate of women without board certified is significantly higher than that of men. It means women presents higher likelihood to be promoted to the full professor position without certificates, which could indicate that the promotion system is more stringent for men.

# 2.4 Prate Issue
ggplot(data = df, aes(x = Rank1, y = Prate, fill = Gender)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("Female" = "#ACD5EB", "Male" = "#F2F2F2")) +
  labs(title = 'Boxplot of Prate by Rank Compared in Gender', x = 'Rank') +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),  
    axis.title.x = element_text(size = 12.5), 
    axis.title.y = element_text(size = 12.5),   
    axis.text.x = element_text(size = 10)  
  )
# Result: Women present a relatively higher publication rate compared to men in both assistant and associate levels. However, it drops in the full professor level while the prate of men generally has increased with promotion in positions. Assuming similar quality across gender, the publication efficiency of men increases with promotion, while female’s publication efficiency shows a narrowing trend. The discrepancy might explain the preference of promoting male as a prior.



#----------------------------------------------------------------------------------

# 3. predictive(rank)
# 3.1 Colinearity
m1 = glm(Rank2~Gender+Dept+Clin+Cert+Exper+Prate,family = binomial, data = df)
summary(m1)
library(car)
vif_values <- vif(m1)
print(vif_values)
# Result: In the regression model, the GVIF^(1/(2*Df))s of Prate and Clin were 4.202533 and 2.471135 respectively, both showing high multicollinearity. This indicates that there is a significant correlation between these two variables and other independent variables. Other variables have lower VIF values, showing that they have less this problem. Further processing of Prate and Clin is recommended to improve model stability and accuracy.

# 3.2 Grouped regression
df_male <- df[Gender == 'Male']
df_female <- df[Gender == 'Female']
# Comment: Since the two variables Prate and Clin have multicollinearity problem and they are not significant after processing it, that's why these two variables are excluded.

mg_male <- glm(Rank2~Dept+Cert+Exper,family = binomial,data = df_male)
summary(mg_male)
  #Confusion Matrix
  threshold <- 0.5
  predicted <- predict(mg_male, type = 'response')
  predicted_rank <- ifelse(predicted > threshold, "Full professor", "Other")
  actual_rank <- df_male$Rank2
  actual_rank <- relevel(df_male$Rank2, ref = "Full professor")
  confusion_matrix_male <- table(Actual = actual_rank,Predicted = predicted_rank, deparse.level = 2)
  confusion_matrix_male
  # In the male group, the model correctly predicted 51 full professors (74%) and 73 non-full professors (85%), and misclassified 18 full professors as non-full professors and 13 non-full professors as full professors.

mg_female <- glm(Rank2~Dept+Cert+Exper,family = binomial,data = df_female)
summary(mg_female)
  #Confusion Matrix
  threshold <- 0.5
  predicted <- predict(mg_female, type = 'response')
  predicted_rank <- ifelse(predicted > threshold, "Full professor", "Other")
  actual_rank <- df_female$Rank2
  actual_rank <- relevel(df_female$Rank2, ref = "Full professor")
  confusion_matrix_female <- table(Actual = actual_rank,Predicted = predicted_rank, deparse.level = 2)
  confusion_matrix_female
  # In the female group, the model correctly predicted 9 full professors (56%) and 86 non-full professors (96%), and misclassified 7 full professors as non-full professors and 4 non-full professors as full professors.

# Result: Based on our analysis, men generally tend to have more years of experience. And the coefficient for the 'experience' factor is higher for females, suggesting that females tend to achieve promotions to higher ranks more quickly compared to males.

# 3.3 CART
library(rpart)
library(rpart.plot)
set.seed(2020)
# balance the trainset
# Random sample from majority class nofullprofessor and combine into new trainset -----
dt.nonprof <- df[df$Rank2 == 'Other', ]
dt.prof <- df[df$Rank2 == 'Full professor', ]
chosen <- sample(seq(1:nrow(dt.nonprof)), size = nrow(dt.prof))
dt.nonprof.chosen <- dt.nonprof[chosen, ] 

# Combine two data tables by appending the rows
data1 <- rbind(dt.prof, dt.nonprof.chosen)
summary(data1)

# Cart model
m2 <- rpart(Rank2 ~ Dept + Gender + Clin + Cert + Prate + Exper, 
            data = data1, method = 'class', 
            control = rpart.control(minsplit = 5, cp = 0))

rpart.plot(m2, nn=T, main='Maximal tree')
printcp(m2, digits = 3)
plotcp(m2)

# Compute min CVerror + 1SE in maximal tree m2
CVerror.cap <- m2$cptable[which.min(m2$cptable[,"xerror"]), "xerror"] + m2$cptable[which.min(m2$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree m2.
i <- 1; j<- 4
while (m2$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(m2$cptable[i,1] * m2$cptable[i-1,1]), 1)

# Get best tree based on 10 fold CV with 1 SE
m2.best <- prune(m2, cp = cp.opt)
printcp(m2.best, digits = 3)

  # Misclassification error
  pred <- predict(m2.best, newdata = data1, type = "class")
  conf_matrix <- table(Predicted = pred, Actual = data1$Rank2)
  print(conf_matrix)
  misclass_error <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
  print(paste("Misclassification Error: ", round(misclass_error, 4)))
  # The model has a misclassification error rate of 21.76%.

# Variable importance
m2.best$variable.importance
m2.best.scaledVarImpt <- round(100*m2.best$variable.importance/sum(m2.best$variable.importance))
m2.best.scaledVarImpt

# Result: Experience (Exper) is the most important predictor of promotion, accounting for 98% of the model's predictive power. Prate contributes only 2%. Gender is not a significant variable in predicting promotion (not used in the final tree).

#----------------------------------------------------------------------------------------

# 4. descriptive(salary)
# 4.1 Intro
# 4.1.1 Relationship between Salary and Exper
ggplot(data = df, aes(x = Exper, y = Sal95)) + 
  geom_point(color = "#8C8C8C") + 
  geom_smooth(method = 'lm', color = "#011140", fill = "#ACD5EB") +  
  labs(title = "Salary1995 by Experience") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),  
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5)   
  )
# Result: In this chart and the subsequent linear regression analysis, it can be observed that Experience is strongly positively correlated with Salary.

# 4.1.2 Gender Discrepency in Exper
ggplot(data = df,aes(x=Gender,y=Exper,fill = Gender)) + geom_boxplot() +
  scale_fill_manual(values = c("Female" = "#ACD5EB", "Male" = "#F2F2F2")) + 
  theme_minimal() + 
  labs(title = "Exper by Gender")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),  
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5)
  )
# Result: The boxplot, however, shows that females generally have less experience than males. This suggests that the reason for females earning less than males could be due to insufficient experience rather than gender discrimination.

# 4.2 Remove Effect of Exper
# Construct Tool Variables: To eliminate the impact of experience differences between genders on salary, we constructed three tool variables, dividing salary by experience and comparing the resulting ratios.
df$sal_to_exper_94 <- df$Sal94/df$Exper
df$sal_to_exper_95 <- df$Sal95/df$Exper
df$salIncre_to_exper <- df$SalIncrement/df$Exper

ggplot(data=df,aes(x=Gender,y=sal_to_exper_94, fill = Gender))+geom_boxplot()+
  scale_fill_manual(values = c("Female" = "#ACD5EB", "Male" = "#F2F2F2"))+
  theme_minimal() + ylim(0, 50000) + 
  labs(title = "Gender Salary Comparison after Removing the Exper Effect(94)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),  
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5)
  )

ggplot(data=df,aes(x=Gender,y=sal_to_exper_95, fill = Gender))+geom_boxplot()+
  scale_fill_manual(values = c("Female" = "#ACD5EB", "Male" = "#F2F2F2"))+
  theme_minimal() + ylim(0, 50000)+ 
  labs(title = "Gender Salary Comparison after Removing the Exper Effect(95)")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),  
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5)
  )

ggplot(data=df,aes(x=Gender,y=salIncre_to_exper, fill = Gender))+geom_boxplot()+
  scale_fill_manual(values = c("Female" = "#ACD5EB", "Male" = "#F2F2F2"))+
  theme_minimal() + ylim(0, 4000)+ 
  labs(title = "Gender Salary Increment Comparison after Removing the Exper Effect")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),  
    axis.title.x = element_text(size = 12.5),  
    axis.title.y = element_text(size = 12.5)
  )
# Result: After removing the effect of experience, the salaries in 1995, 1994, and the incremental changes between these two years all indicate that the salary levels between males and females are nearly the same, with females even surpassing males slightly.

#----------------------------------------------------------------------------------------

# 5. predictive(salary)
ms_95 = lm(Sal95~Gender+Exper+Cert+Dept+Rank1+Clin,data = df)
summary(ms_95)
# Adjusted R-squared:  0.8998
  # RMSE
  predictions <- predict(ms_95)
  residuals <- df$Sal95 - predictions
  rmse95 <- sqrt(mean(residuals^2))
  rmse95
  # RMSE is 27450.74
  
ms_94 = lm(Sal94~Gender+Exper+Cert+Dept+Rank1+Clin,data = df)
summary(ms_94)
# Adjusted R-squared:  0.8994
  # RMSE
  predictions <- predict(ms_94)
  residuals <- df$Sal94 - predictions
  rmse94 <- sqrt(mean(residuals^2))
  rmse94
  # RMSE is 24927.45

ms_Incre = lm(SalIncrement~Gender+Exper+Cert+Dept+Rank1+Clin,data = df)
summary(ms_Incre)
# Adjusted R-squared:  0.781 
  # RMSE
  predictions <- predict(ms_Incre)
  residuals <- df$SalIncrement - predictions
  rmse_SalIncrement <- sqrt(mean(residuals^2))
  rmse_SalIncrement
  # RMSE is 4080.478

# Result: After processing model, it is found that the prate variable is not significant, hence, it is excluded to improve model stability and accuracy. Based on the results of the three models, the coefficient for Gender is not statistically significant in any of the models (p-values: 0.5478 for 1995, 0.6206 for 1994, and 0.3083 for salary increment). This indicates that gender does not have a significant effect on salary or salary increments, after accounting for other variables such as experience, certification, department, rank, and clinical research involvement.

