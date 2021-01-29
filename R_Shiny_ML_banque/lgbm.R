library(readr)

setwd("C:/Users/abenes/Documents/YNOV M2/Modele_de_banque_et_scoring/R_Shiny_ML_banque")

#Lecture des fichiers
application_train <- read_csv("application_train.csv")
head(application_train)
application_test <- read_csv("application_test.csv")
head(application_test)

#summary de la table train et répartition de notre variable cible
summary(application_train)
table(application_train$TARGET)
round(prop.table(table(application_train$TARGET)),2)



# Proportions de valeurs manquantes dans chaques variables 
pctmiss <- colSums(is.na(application_train))/nrow(application_train)
round(pctmiss, 2)




#EDA VISUALISATION GRAPHIQUE


#installation 
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages('scales')
library(scales)

## Diagramme en barre de la variable cible
ggplot(application_train, aes(x = TARGET)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "TARGET", 
       y = "Frequency", 
       title = "Proportions by TARGET")

#Prépation de la table avec le pourcentage de la variable cible
#Cette table va nous permettre d'avoir un graphique avec des labels

plotdata <- application_train %>%
  count(TARGET) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

ggplot(plotdata, 
       aes(x = factor(TARGET),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25)  +
  labs(x = "TARGET", 
       y = "Percent", 
       title  = "Participants by target")

#EDA DE L'AGE

summary(application_train$DAYS_BIRTH / -365)

#Ces âges semblent raisonnables.
#Il n'y a pas de valeurs aberrantes pour l'âge sur le haut ou le bas. 
#En moyenne 44 ans


#Et les jours de travail?
summary(application_train$DAYS_EMPLOYED)


#Cela ne semble pas correct! La valeur maximale (en plus d'être positive) est d'environ 1000 ans!


application_train[application_train$DAYS_EMPLOYED == 365243, NULL]

#Graphiques sur l'age 



ggplot(application_train, aes(x = DAYS_BIRTH/-365)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 bins = 25) + 
  labs(title="Participants by age", 
       subtitle = "number of bins = 25",
       x = "Age")

ggplot(application_train, aes(x = DAYS_BIRTH/-365)) +
  geom_density(fill = "deepskyblue", 
               bw = 1) + 
  labs(title = "Participants by age",
       subtitle = "bandwidth = 1")


ggplot(application_train, aes(DAYS_BIRTH/-365, fill = factor(TARGET))) + 
  geom_bar(position = "fill") + 
  xlab("DAYS_BIRTH") + ylab("") + labs(fill = "TARGET")

ggplot(application_train, aes(DAYS_BIRTH/-365, col = factor(TARGET))) +
  geom_density()

#Mise en forme de l'age en année et postive

application_train$YEARS_BIRTH<- application_train$DAYS_BIRTH/-365
application_train$YEARS_ID_PUBLISH<- application_train$DAYS_ID_PUBLISH/-365



application_test$YEARS_BIRTH<- application_test$DAYS_BIRTH/-365
application_test$YEARS_ID_PUBLISH<- application_test$DAYS_ID_PUBLISH/-365


application_train= application_train %>% select(-DAYS_BIRTH,-DAYS_ID_PUBLISH)
application_test= application_test %>% select(-DAYS_BIRTH,-DAYS_ID_PUBLISH)

#installation 
install.packages("questionr")
library(questionr)

#Découpage de la classe age pour faire de l'EDA nus avons vu précédément que l'age avait un lien dans le fait de rembourser son crédit ou pas
application_train$YEARS_BIRTH_class <- cut(application_train$YEARS_BIRTH,c(20,25,30,35,40,45,50,55,60,65,70), include.lowest = TRUE, labels = c("20-25","25-30","30-35","35-40","40-45","45-50","50-55","55-60","60-65","65-70"))
table(application_train$YEARS_BIRTH_class)


#Création d'une base qui nous permettre de faire un graphique 
base <- application_train %>%
  group_by(YEARS_BIRTH_class) %>%
  summarise(TARGET=mean(TARGET,na.rm = TRUE),
            TARGET2=mean(TARGET,na.rm = TRUE)*100,
            pctlabel=paste0(round(mean(TARGET,na.rm = TRUE)*100),"%"),
            DAYS_BIRTH=mean(YEARS_BIRTH,na.rm = TRUE),
            YEARS_BIRTH=mean(YEARS_BIRTH,na.rm = TRUE))


ggplot(base, 
       aes(x = YEARS_BIRTH_class,
           y = TARGET2)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25)  +
  labs(x = "age group (years)",
       y = "Failure to Repay(%)",
       title  = "Failure to Repay by Age group")

#Plus on est jeune moin on a de chance de rembourser notre crédit

ggplot(application_train, aes(EXT_SOURCE_3, col = factor(TARGET))) +
  geom_density()
# Nous pouvons clairement voir que cette caractéristique
#a un certain rapport avec la probabilité d'un demandeur de rembourser un prêt.


ggplot(application_train, aes(AMT_CREDIT , col = factor(TARGET))) +
  geom_density()


ggplot(application_train, aes(factor(TARGET), AMT_CREDIT, fill = factor(TARGET))) +
  geom_boxplot()
#pas trop de différence juste que l'écart interquartille est plus petit pour ceux qui n'ont pas remboursé leur crédit


library(labelled)
look_for(application_train)

#???CONCATENATION DES DEUX TABLES
full <- bind_rows(application_train,application_test)

#Récupération de la target
Target <- application_train$TARGET
#Suppresion de L'ID dans l'application test 
Id <- application_test$SK_ID_CURR
#Suppresion des deux dans la table ful
full[,c('SK_ID_CURR','TARGET')] <- NULL
#Séparation des variables quantitative et char
chr <- full[,sapply(full, is.character)]
num <- full[,sapply(full, is.numeric)]

chr[is.na(chr)] <- "Not Available"

fac <- chr %>% 
  lapply(as.factor) %>% 
  as_data_frame()

#Dans la table full on garde que les variables fac et num 
full <- bind_cols(fac, num)
rm(chr, fac, num)

full[is.na(full)] <- 0

#num <- application_train[, sapply(application_train,is.numeric)]

#rm(application_train, application_test)

#On crée la base de train qui va jusqu'a la longeur des target
train <- full[1:length(Target),]
test <- full[(length(Target)+1):nrow(full),]

install.packages('caret')
library(caret)

set.seed(123)
inTrain <- createDataPartition(Target, p=.9, list = F)


tr <- train[inTrain,]
va <- train[-inTrain,]





#dimension
print(dim(tr))
print(dim(va))


tr_ta <- Target[inTrain]
va_ta <- Target[-inTrain]

install.packages('lightgbm')
library(lightgbm)
#mise en forme des tables pour le Label
lgb.train = lgb.Dataset(data.matrix(tr), label = tr_ta)
lgb.valid = lgb.Dataset(data.matrix(va), label = va_ta)


params.lgb = list(
  objective = "binary"
  , metric = "auc"
  , min_data_in_leaf = 1
  , min_sum_hessian_in_leaf = 100
  , feature_fraction = 1
  , bagging_fraction = 1
  , bagging_freq = 0
)

# Get the time to train the lightGBM model

lgb.model <- lgb.train(
  params = params.lgb
  , data = lgb.train
  , valids = list(val = lgb.valid)
  , learning_rate = 0.05
  , num_leaves = 7
  , num_threads = 2
  , nrounds = 3000
  , early_stopping_rounds = 200
  , eval_freq = 50
)

tree_imp <- lgb.importance(lgb.model, percentage = TRUE) %>% head(20)
lgb.plot.importance(tree_imp, measure = "Gain")



#on garde que ces 20 varibles 
#???CONCATENATION DES DEUX TABLES
full2 <- bind_rows(application_train,application_test)

#Récupération de la target
Target <- application_train$TARGET
#Suppresion de L'ID dans l'application test 
Id <- application_test$SK_ID_CURR
Id_train <- application_train$SK_ID_CURR
#Suppresion des deux dans la table full2
full2[,c('SK_ID_CURR','TARGET')] <- NULL


#Séparation des variables quantitative et char
chr <- full2[,sapply(full, is.character)]
num <- full2[,sapply(full, is.numeric)]

chr[is.na(chr)] <- "Not Available"

fac <- chr %>% 
  lapply(as.factor) %>% 
  as_data_frame()

#Dans la table full on garde que les variables fac et num 
full2 <- bind_cols(fac, num)


full2[is.na(full)] <- 0

full2_format <- full2[,c(tree_imp$Feature)]

#On crée la base de train qui va jusqu'a la longeur des target
train <- full2_format[1:length(Target),]
test <- full2_format[(length(Target)+1):nrow(full),]

#On crée la base de train qui va jusqu'a la longeur des target
ID_VF=rbind(Id,Id_train)
train_target =cbind(Id_train,Target,full2_format[1:length(Target),])


inapp <- createDataPartition(Target, p=.01, list = F)
#on enrengistre le fichier pour notre appli shinny
base_model <- train_target[inapp,]
#Save the test data. We will upload this to the shiny app to get predictions
write.csv(base_model, 'test_data.csv', row.names = FALSE)

base_model2= base_model %>% select(-Target,-Id_train)
#Save the test data. We will upload this to the shiny app to get predictions
write.csv(base_model2, 'test_data2.csv', row.names = FALSE)



set.seed(123)
inTrain <- createDataPartition(Target, p=.9, list = F)


tr <- train[inTrain,]
va <- train[-inTrain,]





#dimension
print(dim(tr))
print(dim(va))


tr_ta <- Target[inTrain]
va_ta <- Target[-inTrain]




#mise en forme des tables pour le Label
lgb.train = lgb.Dataset(data.matrix(tr), label = tr_ta)
lgb.valid = lgb.Dataset(data.matrix(va), label = va_ta)


params.lgb = list(
  objective = "binary"
  , metric = "auc"
  , min_data_in_leaf = 1
  , min_sum_hessian_in_leaf = 100
  , feature_fraction = 1
  , bagging_fraction = 1
  , bagging_freq = 0
)

# Get the time to train the lightGBM model

lgb.model2 <- lgb.train(
  params = params.lgb
  , data = lgb.train
  , valids = list(val = lgb.valid)
  , learning_rate = 0.05
  , num_leaves = 7
  , num_threads = 2
  , nrounds = 3000
  , early_stopping_rounds = 200
  , eval_freq = 50
)

tree_imp_mod2 <- lgb.importance(lgb.model2, percentage = TRUE) %>% head(5)
lgb.plot.importance(tree_imp_mod2, measure = "Gain")



# make test predictions sur notre model qu'on va tester en server
lgb_pred <- predict(lgb.model2, data = data.matrix(base_model2), n = lgb.model$best_iter)
#Prediction "response" 
pred.target = factor(ifelse(lgb_pred > 
                           0.5, 1, 0))
va_base <- cbind(base_model,lgb_pred, pred.target)
# Matrice de confusion
(m.confusion <- as.matrix(table(va_base$pred.target, va_base$Target)))
m.confusion 
matrice2=data.frame(m.confusion)
colnames(matrice2) <- c("Predition","Realite","Nb")
m.confusion <- unclass(m.confusion)
# Taux d'erreur
Tx_err <- function(y, ypred) {
  mc <- table(y, ypred)
  error <- (mc[1, 2] + mc[2, 1])/sum(mc)
  print(error)
}


# make test predictions
lgb_pred_test <- predict(lgb.model2, data = data.matrix(test), n = lgb.model$best_iter)

result <- data.frame(SK_ID_CURR = Id, TARGET = lgb_pred_test)

write.csv(result,"lgb_pred.csv", row.names = F)


#Save the model. We will use it in the shiny app

save(lgb.model2 , file = 'lgb.rda')


