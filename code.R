library(tidyverse)
library(ggcorrplot)
library(scales)
library(grid)
library(gridExtra)
library(caTools)
library(caret)
library(class)
library(performance)
library(smotefamily)



df <- read_csv("D:/Projects/Self Project/2/WineQuality.csv",
               show_col_types = F)

View(df)
df

# looking at the data:
glimpse(df)
summary(df)



# Changing the column names:
cname <- gsub(" ", "_", colnames(df))
colnames(df) <- cname
vars_fac <- c('type','quality')
vars_cont <- cname[!(cname %in% vars_fac)]


# Handling NA values:
{
  na_vars <- c('fixed_acidity','volatile_acidity','citric_acid',
               'residual_sugar','chlorides','pH','sulphates')
  
  df_1 <- df %>% select(-all_of(na_vars), -quality, -type)
  
  
  for(i in na_vars){
    df_temp <- cbind(df_1, df[i]) %>% as_tibble()
    reformulate('free_sulfur_dioxide + total_sulfur_dioxide + 
                       density + alcohol', response = i) %>% 
      lm(data = df_temp) %>% predict() -> pred_var
    
    for(j in 1:nrow(df_temp)){
      if((df[i])[j,] %>% is.na() == TRUE) ((df[i])[j,] = pred_var[j]) 
    }
  }
}

df %>% summarise_all(~ sum(is.na(.))) %>% 
  t() 


df <- df %>% mutate(across(where(is.numeric), 
                           ~replace_na(., mean(., na.rm = TRUE))))


# Changing column types:
df <- df %>% mutate(across(all_of(vars_fac), as.factor))


# Function for categorical plot:
plot1 <- function(var){
  df %>% count({{var}}) %>% ggplot(aes(x = {{var}}, y = n)) +
    geom_bar(stat = 'identity', position = position_dodge2(),
             width = 0.3, fill = 'yellow', colour = 'black') +
    theme_minimal() + labs(y = '')
}

attach(df)
plot1(quality)
plot1(type)
detach(df)

    # Type ~ Quality
df %>% group_by(quality, type) %>% count() %>% 
  ggplot(aes(x = quality, y = n, fill = type)) +
  geom_col(position = 'fill', width = 0.3) + labs(y = '') +
  scale_y_continuous(labels = percent, n.breaks = 10) +
  theme_minimal()
# Comment: 'Red' wines are fewer in number in good quality


#================================================================
# Selecting only the white wines:
df_w <- df %>% filter(type == 'white') %>% 
  select(-type)

# re-level the quality column:
df_w %>% mutate('Quality' = case_when(
  quality %in% 3:5 ~ 0,
  quality %in% 6:9 ~ 1
)) %>% select(-quality) -> df_w
df_w$Quality <- as.factor(df_w$Quality)


View(df_w)
glimpse(df_w)

#==================================================================
# Checking for Multicollinearity:

# Method 1
df_w %>% select(-Quality) %>% 
  cor() %>% ggcorrplot(lab = T)

# Some combination is showing moderate correlation.


# Method 2
{
  df1 <- df %>% select(all_of(vars_cont))
  
  M <- matrix(ncol = 3, nrow = length(vars_cont),
              dimnames = list(paste(1:length(vars_cont)),
                              c('Variable','VIF','Tolerance'))) %>% 
    as.data.frame()
  for(i in 1:length(vars_cont)){
    f <- reformulate(".", response = vars_cont[i])
    s <- lm(f, data = df1) %>% summary()
    v <- round(1/(1-s$r.squared),5)
    M[i,] <- c(vars_cont[i], v, round(1/v, 5))
  }
  
  print(M)
}

glm(Quality ~ ., data = df_w, 
    family = binomial(link = 'logit')) -> g1
check_collinearity(g1)


    # Scaling data to remove multicollinearity:
df_w3 <- df_w
for(i in vars_cont){
  df_w3[i] <- scale(df_w3[i])
}

glm(Quality ~ ., data = df_w3, 
    family = binomial(link = 'logit')) -> g3
check_collinearity(g3)
View(df_w3)


df_w2 <- df_w %>% select(-density)
# vars_cont2 <- colnames(df_w2)

    # re-checking
glm(Quality ~ ., data = df_w2, 
    family = binomial(link = 'logit')) -> g2
check_collinearity(g2)
# No multicollinearity is present there.

#================================================================
# Significance check:
plot2 <- function(var, fill_var){
  cols <- c('yellow','blue')
  
  df_w %>% ggplot(aes({{var}}, fill = {{fill_var}})) + 
    geom_density(colour = NA, alpha = 0.4) +
    scale_x_continuous(n.breaks = 10) + 
    labs(x = '') + theme_minimal() +
    scale_fill_manual(values = cols) -> p1
  
  df_w %>% ggplot(aes({{var}}, fill = {{fill_var}})) + 
    geom_boxplot(outlier.colour = 'red',
                 outlier.size = 2) + 
    labs(x = '') +theme_minimal() +
    scale_fill_manual(values = cols) -> p2
  grid.arrange(p1,p2, ncol = 2)
}

attach(df_w)
plot2(fixed_acidity, Quality)
plot2(volatile_acidity, Quality)
plot2(citric_acid, Quality)
plot2(residual_sugar, Quality)
plot2(chlorides, Quality)
plot2(free_sulfur_dioxide, Quality)
plot2(total_sulfur_dioxide, Quality)
plot2(density, Quality)
plot2(pH, Quality)
plot2(sulphates, Quality)
plot2(alcohol, Quality)
detach(df)


    # Significance of predictors (Statistically, ANOVA)
M <- matrix(ncol = 2, nrow = length(vars_cont2),
            dimnames = list(paste(1:length(vars_cont2)),
         c('Variables','p-value'))) %>% as.data.frame()

for(i in 1:length(vars_cont2)){
  f <- reformulate('Quality', response = vars_cont2[i])
  u <- aov(f, data = df_w) %>% 
    summary() %>% unlist()
  
  M[i,] <- c(vars_cont2[i], round(u['Pr(>F)1'],5))
}
print(M)
#==================================================================
#==================================================================
# LOGISTIC
#==================================================================
#==================================================================
########################
# Function for PRECISION & RECALL:
stats2 <- function(C, model){      # C := Confusion Matrix and Statistics
  t <- C$table
  
  acc <- C$overall[[1]]
  pre <- t[2,2]/(t[2,2]+t[2,1])
  rec <- t[2,2]/(t[2,2]+t[1,2])
  f1 <- 2*(rec*pre)/(rec+pre)
  
  matrix(c(acc,pre,rec,f1), byrow = T,
         dimnames = list(c('Accuracy','Precision',
                           'Recall','F1-Score'),
                         paste(model))) -> M
  return(list('Confusion Matrix' = t,
              'Metrics' = M))
}
###################################################################
set.seed(42)
s <- sample.split(df_w2$Quality, SplitRatio = 0.75)
train_data <- subset(df_w2, s == TRUE)
test_data <- subset(df_w2, s == FALSE)
# it will remain fixed throughout the process

    # Using 'Logit' ::
gL <- glm(Quality ~ ., data = train_data,
          family = binomial(link = 'logit'))
summary(gL)

# Some variables are not significant, so we will delete them
# and re-fit the model again.
gL2 <- glm(Quality ~ .-citric_acid-chlorides-
             total_sulfur_dioxide-pH, data = train_data,
           family = binomial(link = 'logit'))
summary(gL2)

p_hat_train <- predict.glm(gL2, type = 'response')

## Optimum cut-off selection:
metric_func <- function(data, phat){  # function to store the 
  cut_points <- seq(0.01,0.99,0.001)    # necessary metrics
  
  d <- data.frame(matrix(nrow = length(cut_points),
                         ncol = 4, dimnames = list(
                           paste(1:length(cut_points)),
                           c('p_opt','Accuracy',
                             'Sensitivity','Specificity')
                         )))
  
  for(i in 1:length(cut_points)){
    C <- confusionMatrix(
      if_else(phat >= cut_points[i], 1, 0) %>% as.factor(),
      data$Quality)
    
    d[i,] <- c(cut_points[i], C$overall[[1]],
               C$byClass[[1]],C$byClass[[2]])
  }
  
  d$sens_spec <- d[,3]*d[,4]
  return(d)
}



m_train <- metric_func(train_data,p_hat_train) 


# plot of all the metrics:
p1_opt <- m_train[which.max((m_train$Accuracy)),]$p_opt
p2_opt <- m_train[which.max((m_train$sens_spec)),]$p_opt

m_train %>% 
  pivot_longer(Accuracy:sens_spec,
               names_to = 'Metrics', values_to = 'value') %>% 
  ggplot(aes(x = p_opt, y = value, colour = Metrics)) +
  geom_line() + labs(x = 'Cutoff (p)') +
  geom_vline(xintercept = c(p1_opt, p2_opt), lty = 5) +
  theme_minimal()



glue::glue('Optimum cut point (Accuracy): {c1}',
     'Optimum cut point (Sens*Spec): {c2}',
     .sep = '\n', c1 = p1_opt, c2 = p2_opt)


   ### On train data:
confusionMatrix(ifelse(p_hat_train >= p1_opt, 1, 0) %>% 
                  as.factor(), train_data$Quality) -> C1

stats2(C1,'Logistic')


   ### On test data:
p_hat_test <- predict.glm(gL2, newdata = test_data,
                         type = 'response')

confusionMatrix(ifelse(p_hat_test >= p1_opt, 1, 0) %>% 
                  as.factor(), test_data$Quality) -> C2

stats2(C2,'Logistic')


   ### ROC curves:
m_test <- metric_func(test_data, p_hat_test)

ROC_func <- function(m, type){
  plot(1 - m$Specificity, m$Sensitivity, type = 'l',
       main = paste('ROC curve ||',type,'data'), 
       ylab = 'Specificity (TPR)',
       xlab = '1-Sensitivity (FPR)', lwd = 2, las = 1)
  abline(a = 0, b = 1, h = 0:1, v = 0:1, lty = 2)
}

par(mfrow = c(1,2))
ROC_func(m_train, 'Train')
ROC_func(m_test, 'Test')

###################################################################
# knn 
###################################################################
# Scaling data: (using min-max scaler)
scale2 <- function(x) ((x-min(x))/(max(x)-min(x)))
train_data_scaled <- train_data %>% 
  mutate_if(is.numeric, list(~ scale2(.)))
test_data_scaled <- test_data %>% 
  mutate_if(is.numeric, list(~ scale2(.)))


# Choosing optimal value of "k" :
error <- array(0)
k <- seq(1, 79, by = 2)

for(i in 1:length(k)){
  knn_model <- knn(train_data_scaled[,-11], 
                   test_data_scaled[,-11], 
                   cl = train_data$Quality, k = k[i])
  error[i] <- mean(test_data$Quality != knn_model)
}



# Error plot:
data.frame('k' = k, 'ErrorRate' = error) %>% 
  ggplot(aes(x = k, y = ErrorRate)) + 
  geom_line(colour = 'red') + geom_point() +
  scale_y_continuous(labels = percent, n.breaks = 10) +
  scale_x_continuous(n.breaks = length(error)) +
  theme_minimal() +
  geom_vline(xintercept = 41, linetype = 2, 
             colour = 'blue', lwd = 1)

k_opt <- 41

# Final model:
knn_model <- knn(train_data_scaled[,-11], test_data_scaled[,-11], 
                 cl = train_data_scaled$Quality, k = k_opt)

confusionMatrix(test_data_scaled$Quality, knn_model) -> C
stats2(C,'knn')

















#==================================================================
#==================================================================
## Balancing the data::
#==================================================================
df_w2_bal <- SMOTE(df_w2[,-11], target = df_w2$Quality,
                   K = 5, dup_size = 1)$data %>% 
  mutate(Quality = as.factor(class)) %>% 
  select(-class)


# glimpse(df_w2)
# glimpse(df_w2_bal)


# Unbalanced
df_w2 %>% count(Quality) %>% 
  mutate('%' = percent(n/sum(n)))

# Balanced
df_w2_bal %>% count(Quality) %>% 
  mutate('%' = percent(n/sum(n)))


  ## LOGISTIC 2 (on balanced data)
set.seed(42)
s <- sample.split(df_w2_bal$Quality, SplitRatio = 0.75)
train_data_bal <- subset(df_w2_bal, s == TRUE)
test_data_bal <- subset(df_w2_bal, s == FALSE)

gL_F <- glm(Quality ~ .-citric_acid-chlorides-
                 total_sulfur_dioxide-pH, data = train_data_bal,
               family = binomial(link = 'logit'))
summary(gL_F)

p_hat_train_F <- predict.glm(gL_F, type = 'response')


m_train_F <- metric_func(train_data_bal, p_hat_train_F) 
p1_opt_F <- m_train_F[which.max((m_train_F$Accuracy)),]$p_opt
p2_opt_F <- m_train_F[which.max((m_train_F$sens_spec)),]$p_opt


glue::glue('Optimum cut point (Accuracy): {c1}',
           'Optimum cut point (Sens*Spec): {c2}',
           .sep = '\n', c1 = p1_opt_F, c2 = p2_opt_F)


### On train data:
confusionMatrix(ifelse(p_hat_train_F >= p1_opt_F, 1, 0) %>% 
                  as.factor(), train_data_bal$Quality) -> C1_F

stats2(C1_F,'Logistic')


### On test data:
p_hat_test_F <- predict.glm(gL_F, newdata = test_data_bal,
                          type = 'response')

confusionMatrix(ifelse(p_hat_test_F >= p1_opt_F, 1, 0) %>% 
                  as.factor(), test_data_bal$Quality) -> C2_F

stats2(C2_F,'Logistic')


