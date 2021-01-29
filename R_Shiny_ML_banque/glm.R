library(readr)

setwd("C:/Users/abenes/Documents/YNOV M2/Modele_de_banque_et_scoring")

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
       aes(x = TARGET,
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
application_test$YEARS_BIRTH<- application_test$DAYS_BIRTH/-365


application_train$YEARS_ID_PUBLISH<- application_train$DAYS_ID_PUBLISH/-365
application_test$YEARS_ID_PUBLISH<- application_test$DAYS_ID_PUBLISH/-365


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
            DAYS_BIRTH=mean(DAYS_BIRTH/-365,na.rm = TRUE),
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


#Recodage des variables quantitative que je trouves pertinente en classe pour la regression logistique


## Recodage de application_train$AMT_CREDIT en application_train$AMT_CREDIT_rec
application_train$AMT_CREDIT_rec <- cut(application_train$AMT_CREDIT,
                                        include.lowest = TRUE,
                                        right = FALSE,
                                        dig.lab = 4,
                                        breaks = c(45000, 712500, 1380000, 2047500, 2715000, 3382500, 4050000)
)




#EXT_SOURCE_1
## Recodage de application_train$EXT_SOURCE_1 en application_train$EXT_SOURCE_1_rec
application_train$EXT_SOURCE_1_rec <- cut(application_train$EXT_SOURCE_1,
                                          include.lowest = TRUE,
                                          right = TRUE,
                                          dig.lab = 2,
                                          breaks = 5
)
#EXT_SOURCE_2 
## Recodage de application_train$EXT_SOURCE_2 en application_train$EXT_SOURCE_2_rec
application_train$EXT_SOURCE_2_rec <- cut(application_train$EXT_SOURCE_2,
                                          include.lowest = TRUE,
                                          right = TRUE,
                                          dig.lab = 2,
                                          breaks = 5
)

#EXT_SOURCE_3

## Recodage de application_train$EXT_SOURCE_3 en application_train$EXT_SOURCE_3_rec
application_train$EXT_SOURCE_3_rec <- cut(application_train$EXT_SOURCE_3,
                                          include.lowest = FALSE,
                                          right = FALSE,
                                          dig.lab = 4,
                                          breaks = 6
)

#AMT_CREDIT 
## Recodage de application_train$AMT_CREDIT en application_train$AMT_CREDIT_rec
application_train$AMT_CREDIT_rec <- cut(application_train$AMT_CREDIT,
                                        include.lowest = FALSE,
                                        right = FALSE,
                                        dig.lab = 0,
                                        breaks = c(45000, 219559.5, 403589.25, 655987.5, 981454.5, 1463143.5, 4050000)
)
#AMT_ANNUITY 
## Recodage de application_train$AMT_ANNUITY en application_train$AMT_ANNUITY_rec
application_train$AMT_ANNUITY_rec <- cut(application_train$AMT_ANNUITY,
                                         include.lowest = FALSE,
                                         right = FALSE,
                                         dig.lab = 0,
                                         breaks = c(1615.5, 16524, 24903, 34596,258025.5)
)
#AMT_GOODS_PRICE
## Recodage de application_train$AMT_GOODS_PRICE en application_train$AMT_GOODS_PRICE_rec
application_train$AMT_GOODS_PRICE_rec <- cut(application_train$AMT_GOODS_PRICE,
                                             include.lowest = FALSE,
                                             right = FALSE,
                                             dig.lab = 4,
                                             breaks = c(40500, 238500, 450000, 538396, 679500, 4050000)
)
#DAYS_ID_PUBLISH  
## Recodage de application_train$YEARS_ID_PUBLISH en application_train$YEARS_ID_PUBLISH_rec
application_train$YEARS_ID_PUBLISH_rec <- cut(application_train$YEARS_ID_PUBLISH,
                                              include.lowest = FALSE,
                                              right = FALSE,
                                              dig.lab = 4,
                                              breaks = c(0, 4,8,11, 20)
)

#OWN_CAR_AGE
## Recodage de application_train$OWN_CAR_AGE en application_train$OWN_CAR_AGE_rec
application_train$OWN_CAR_AGE_rec <- cut(application_train$OWN_CAR_AGE,
                                         include.lowest = FALSE,
                                         right = FALSE,
                                         dig.lab = 4,
                                         breaks = c(0, 5, 9,12, 15, 91)
)
#REGION_POPULATION_RELATIVE
## Recodage de application_train$REGION_POPULATION_RELATIVE en application_train$REGION_POPULATION_RELATIVE_rec
application_train$REGION_POPULATION_RELATIVE_rec <- cut(application_train$REGION_POPULATION_RELATIVE,
                                                        include.lowest = FALSE,
                                                        right = FALSE,
                                                        dig.lab = 0,
                                                        breaks = c(0.00029, 0.0123263333333333, 0.0243626666666667, 0.036399, 0.072508)
) 
#AMT_INCOME_TOTAL 
## Recodage de application_train$AMT_INCOME_TOTAL en application_train$AMT_INCOME_TOTAL_rec
application_train$AMT_INCOME_TOTAL_rec <- cut(application_train$AMT_INCOME_TOTAL,
                                              include.lowest = FALSE,
                                              right = FALSE,
                                              dig.lab = 0,
                                              breaks = c(25650, 112500, 147150,202500, 1.17e+08)
)
#AMT_REQ_CREDIT_BUREAU_YEAR
## Recodage de application_train$AMT_REQ_CREDIT_BUREAU_YEAR en application_train$AMT_REQ_CREDIT_BUREAU_YEAR_rec
application_train$AMT_REQ_CREDIT_BUREAU_YEAR_rec <- cut(application_train$AMT_REQ_CREDIT_BUREAU_YEAR,
                                                        include.lowest = FALSE,
                                                        right = FALSE,
                                                        dig.lab = 0,
                                                        breaks = c(0, 2, 3, 7, 25)
)



#Selection des variables dans mon application train



application_train_model<- select(application_train, SK_ID_CURR,TARGET,NAME_CONTRACT_TYPE,CODE_GENDER,YEARS_BIRTH_class,FLAG_OWN_CAR,FLAG_OWN_REALTY,
                                 CNT_CHILDREN,NAME_TYPE_SUITE,NAME_INCOME_TYPE,NAME_EDUCATION_TYPE,NAME_FAMILY_STATUS,NAME_HOUSING_TYPE,
                                 OCCUPATION_TYPE,REGION_RATING_CLIENT,REGION_RATING_CLIENT_W_CITY,WEEKDAY_APPR_PROCESS_START,
                                 ORGANIZATION_TYPE,WALLSMATERIAL_MODE,EMERGENCYSTATE_MODE,EXT_SOURCE_1_rec,EXT_SOURCE_2_rec,EXT_SOURCE_3_rec,
                                 AMT_CREDIT_rec,AMT_ANNUITY_rec)
str(application_train_model)
application_train_model$TARGET <- factor(application_train_model$TARGET)
application_train_model$CNT_CHILDREN<- factor(application_train_model$CNT_CHILDREN)
application_train_model$REGION_RATING_CLIENT<- factor(application_train_model$REGION_RATING_CLIENT)
application_train_model$REGION_RATING_CLIENT_W_CITY<- factor(application_train_model$REGION_RATING_CLIENT_W_CITY)
#for ( i in c(2,8,15,16))
#{
#  application_train_model[,i]= as.factor(application_train_model[,i])
#}

#drop id
application_train_model$SK_ID_CURR = NULL

# what is the proportion of missing data for each variable?
pctmiss <- colSums(is.na(application_train_model))/nrow(application_train_model)
round(pctmiss, 2)

install.packages("funModeling")
library(funModeling)
df_status(application_train_model)


test1=application_train_model %>% select(-SK_ID_CURR ,-TARGET)

test1[] <- lapply(test1, as.character)
test1[is.na(test1)  ] <- "N/A"

test2=application_train_model %>% select(SK_ID_CURR ,TARGET)

application_train_model=cbind(test2,test1)


df_status(application_train_model)
str(application_train_model)

#Install
install.packages('caret')
library(caret)

Target <- application_train_model$TARGET
set.seed(123)
inTrain <- createDataPartition(Target, p=.9, list = F)

#data frame pour les individus en apprentissage
tr1 <- application_train_model[inTrain,]

inapp <- createDataPartition(Target, p=.01, list = F)
#on enrengistre le fichier pour notre appli shinny
base_model <- tr1[inapp,]
#Save the test data. We will upload this to the shiny app to get predictions
write.csv(base_model, 'test_data.csv', row.names = FALSE)

tr = tr1 %>% setdiff(base_model)


application_train_model
#data frame pour les individus en test
# l'indiçage négatif qui indique les individus à exclure.
va <- application_train_model[-inTrain,]

#dimension
print(dim(tr))
print(dim(va))





#vérifions la distribution des classes
#fréquences absolues des classes - éch. d'apprentissage
print(table(tr$Target))

#fréquences relatives des classes dans l'éch. d'apprentissage
print(prop.table(table(tr$TARGET)))

#distribution des classes dans l'éch. test
print(prop.table(table(va$TARGET)))

#########Modélisation



####Méthode GLM 


#paramètre du processu d'apprentissage
fitControl <- trainControl(method="none")
reg <- glm(TARGET ~., data = tr, family = binomial(logit))

summary(reg)

#Prediction "response" 
solution_log <- predict(reg, type = "response", newdata = va)
pred.chd = factor(ifelse(solution_log > 
                           0.5, 1, 0))

va_base <- cbind(va,solution_log, pred.chd)
# Matrice de confusion
(m.confusion <- as.matrix(table(va_base$pred.chd, va_base$TARGET)))

m.confusion <- unclass(m.confusion)
# Taux d'erreur
Tx_err <- function(y, ypred) {
  mc <- table(y, ypred)
  error <- (mc[1, 2] + mc[2, 1])/sum(mc)
  print(error)
}
Tx_err(va_base$pred.chd, va_base$TARGET)


va_base %>% group_by(TARGET) %>% summarise(Mean = mean(solution_log))
# Save the solution to a dataframe
solution <- data.frame('SK_ID_CURR' = va$SK_ID_CURR, solution_log)



save(reg , file = 'GLM.rda')













##AVEC INTEGRATION DE SELECTION stepAIC()
m_lrs <- train(TARGET ~ ., data = tr, method="glmStepAIC",
               trControl=trainControl("none"))
#ML sans la selection de variable
m_lr <- train(TARGET ~ ., data =tr ,method="glm",trControl=fitControl)
#importance des variables - intrinsèque au modèle
print(varImp(m_lr))


### Affichage du modèle final
print(m_lrs$finalModel)

#On voit le nombre de variables retenues
#Le nombre de variables est égal au nombre de coefficients - 1.
#nombre de variables sélectionnées
print(length(m_lrs$finalModel$coefficients)-1)


#Performances sur l'échantillon test.

#application sur le test set - mesure des performances
mat_conf<- confusionMatrix(data=predict(m_lrs,newdata =
                                     va),reference=va$Target,positive="1")
print(mat_conf)

#L'objet "matrice de confusion" possède une série de propriétés. Pour accéder aux indicateurs
#globaux, nous utilisons $overall qui est un vecteur aux valeurs nommées.
#accès aux indicateurs globaux
print(mat_conf$overall)

#Pour accéder à la cellule "Accuracy", nous ferons :
  #accuracy
  print(mat_conf$overall["Accuracy"])
  
#La courbe LIFT ou courbe de gain est utilisée pour mesurer l'efficacité d'un ciblage (scoring) 
  
#score des individus positifs
score <- predict(m_lr,va,type="prob")[,"1"]
print(quantile(score))
  
#Avec l'option "type = prob", predict() produit pour chaque individu les probabilités
#d'appartenance aux classes. Nous avons une matrice pour l'ensemble des individus de
#l'échantillon test. Ciblant les crédit pas remboursés, nous récupérons la colonne "1" dans un vecteur
#nommé "score". Nous affichons ses quartiles. Le score est une probabilité dans notre étude, il
#varie entre 0 et 1.

#tableau de données pour le scoring
liftdata <- data.frame(classe=va$Target)
liftdata$score <- score
#objet lift
lift_obj <- lift(classe ~ score, data=liftdata, class="1")
print(lift_obj)


#affichage de la courbe lift
plot(lift_obj)

#La courbe est proche de la limite théorique (atteinte lorsque tous les spams = yes se voient
#attribuer un score plus élevé que les spam = no). Notre ciblage est d'excellente qualité.


#Courbe ROC
#Bien que d'aspect similaire à la courbe LIFT, la courbe ROC répond à une problématique
#différente. Elle vise à mesurer la qualité d'un modèle en s'affranchissant des coûts de mauvaise
#affectation et de la représentativité de l'échantillon utilisé (les proportions des classes dans
#l'échantillon peut être différent de celui de la population).
#Curieusement, il n'y a pas d'outil simple pour construire la courbe ROC avec "caret". Nous
#passons par le package "pROC" qu'il faudra au préalable installer. Nous disposons déjà de tous
#les outils nécessaires avec les classes observées et les scores calculés sur l'échantillon test.
#La fonction roc() de "pROC" prend en entrée une indicatrice des classes (1 quand spam = yes, 0


#install
install.packages("pROC")
#library
library(pROC)


#objet roc
roc_obj <- roc(va$Target=="1",score)
#plot de l'objet roc
plot(1-roc_obj$specificities,roc_obj$sensitivities,type="l")
abline(0,1)

#Nous pouvons accéder à la valeur de l'AUC (aire sous la courbe).
#aire sous la courbe
print(roc_obj$auc)                                                                       sinon) et le score. Nous affichons la courbe avec plot().