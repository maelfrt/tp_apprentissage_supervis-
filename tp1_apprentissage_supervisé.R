### EXO 1

U <- runif(1000, min = 0, max = 1)
X <- rnorm(1000,0,1)
Y <- ifelse(X <= 0, ifelse(U <= 0.2, 1, 0),
            ifelse(U > 0.4, 1, 0))



U_200 <- runif(200, min = 0, max = 1)
X_200 <- rnorm(200,0,1)
Y_200 <- ifelse(X_200 <= 0, ifelse(U_200 <= 0.2, 1, 0),
            ifelse(U_200 > 0.4, 1, 0))

install.packages("class") # à faire une seule fois
library(class)


#ALGORITHME KNN 
Y_pred <- knn(
  train = cbind(X), #variable de base ( ici X)
  test = cbind(X_200),  #variable de test (ici X_test)
  cl = as.factor(Y),  #modalité de decission (Y)
  k = 5     #nombre de voisin choisis 
)

mean(Y_200 == Y_pred)



###EXO 2 

set.seed(1123)

install.packages("mlbench")  # à faire une seule fois
library(mlbench)

data(Vehicle)

"18 predictor potentiel cxar 19 données"

predictors <- Vehicle[, !grepl("^(Skew|Kurt)", names(Vehicle))]
X <-  predictors[, !grepl("^(Class)", names(predictors))]

X <- scale(X)   ### centrer reduire (normaliser) les donner pour eviter les problemes des poids des distances 

Y <- Vehicle$Class

n <- nrow(Vehicle)

train_index <- sample(1:n, size = 0.8*n) ##### Decouper la base en gardant 80%

X_train <- X[train_index, ]  #creer nos 2 bases 
X_test <- X[-train_index, ]

Y_train <- Y[train_index]
Y_test <- Y[-train_index]


Y_pred_2 <- knn(
  train = cbind(X_train), 
  test = cbind(X_test),  
  cl = as.factor(Y_train),  
  k = 5   
)

mean(Y_test == Y_pred_2) #76% 



