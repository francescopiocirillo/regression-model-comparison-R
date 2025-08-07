####################################################
#############      Set-up       ####################
####################################################
setwd("C:/Users/franc/OneDrive/Desktop/Corsi UNISA/Magistrale/1. Data Science/ESAME")  # usato per cambiare la directory di lavoro
dataset <- read.csv("RegressionDSDA250130.csv") # nella variabile dataset ora è contenuto il file .csv specificato come parametro
dim(dataset) # dimensioni del dataset, righe e colonne
head(dataset)  # Mostra le prime righe
names(dataset) # Restituisce i nomi delle voci contenute nel dataset
png("Scatterplot_Matrix.png", width=2400, height=2400); pairs(dataset); dev.off(); # Scatterplot Matrix

set.seed(30012025)
n <- nrow(dataset) # data size
p <- ncol(dataset)-1 # numero di regressori, -1 perché c'è la Y da non considerare

train_size <- floor(0.7 * n) # 70% rappresenta la percentuale di training del dataset
# Scegliamo un numero train_size casuale di numeri da 1 a n: questi rappresentano
# gli indici dei campioni del dataset che faranno parte del training-set
train_indices <- sample(seq_len(n), size = train_size)

train_set <- dataset[train_indices, ]
# Gli elementi i cui indici non sono parte del training-set verranno inseriti nel
# test-set per effettuare la predizione
test_set <- dataset[-train_indices, ] 

x_test = model.matrix(Y~., test_set)[,-1]
y_test = test_set$Y

###############################################
########### Valutazione correlazione ##########
###############################################
corDataset <- round(cor(dataset), digits =2)
View(corDataset)

# Plot della matrice di correlazione
library(corrplot)
png("corr_matrix.png", width=720, height=720)
corrplot(corDataset, method = 'ellipse')
dev.off()

# Si calcola la correlazione media
average_correlation <- mean(corDataset[lower.tri(corDataset, diag = FALSE)])
print("La correlazione media è:")
average_correlation

# Si valutano la correlazione massima e le correlazioni > 0.5 in valore assoluto
library(reshape2)
# si effettua una copia della matrice così da non modificare l'originale
mat_change_diag <- corDataset 
# si calcola il valore assoluto dei valori nella matrice copiata
mat_change_diag <- abs(mat_change_diag) 
# sulla matrice triangolare inferiore e sulla diagonale si settano come valori NA
mat_change_diag[lower.tri(mat_change_diag, diag = TRUE)] <- NA 
# converte la matrice in un data frame con tre colonne: Var1, Var2, e value
cor_melted <- melt(mat_change_diag, na.rm = TRUE) 
cor_vector <- setNames(cor_melted$value, paste(cor_melted$Var1, cor_melted$Var2, sep = "_"))

# stampa della massima correlazione
cor_vector[which.max(cor_vector)]
# la coppia a massima correlazione in valore assoluto è Y-X18
# con correlazione -0.54
# L'unica correlazione > 0.5 in valore assoluto è anche quella massima
cor_vector[cor_vector > 0.5] 

# scatterplot delle due variabili con correlazione maggiore
max_cor_vars <- strsplit(names(which.max(cor_vector)), "_")[[1]] # divido i nomi delle variabili
var1 <- max_cor_vars[1]
var2 <- max_cor_vars[2]
png("corrMax_scatterplot.png", width=720, height=720)
plot(dataset[[var1]], dataset[[var2]], 
     xlab = var1, 
     ylab = var2, 
     main = paste(var1, "vs", var2),
     col = "blue")
dev.off()

#####################################################
##############  Multicollinearità  ##################
#####################################################
library(car)
# Modello OLS per calcolare VIF
fit = lm(Y~., data=dataset) 
# misura la multicoll, non valuta variabili dipendenti nel calcolo
vif = vif(fit)

# stampa valori significativi
x_max_vif <- which.max(vif) 
vif_max   <- vif[x_max_vif]
x_min_vif <- which.min(vif)
vif_min   <- vif[x_min_vif]
vif_mean  <- mean(vif) 
vif_maggiori_5 <- vif[vif > 5]
print("Il numero di regressori con collinearità maggiore di 5 è:")
length(vif_maggiori_5) # nessuno
print("Massima collinearità:")
vif_max
print("Minima collinearità:")
vif_min
print("Collinearità media:")
vif_mean


###########################################################################
###############        Best Subset Selection (BSS)       ##################
###########################################################################
# importiamo le librerie utili per effettuare subsets selection
library(leaps)
library(boot)

print("BSS")
# Si effettua il fit con BSS (infatti non è stato specificato il parametro method) 
# con tutti i regressori da 1 a p
regfit.bss = regsubsets(Y~., data=train_set, nvmax=p)
bss_summary = summary(regfit.bss)
bss_summary

# i1)
###############
####  BIC  ####
###############
print("Modello a minimo BIC per Best Subset Selection:")
bss_min_bic_index <- which.min(bss_summary$bic); 
bss_min_bic_index # dimensione 9

# Il valore del BIC minimo è
min(bss_summary$bic)

# Coefficienti stimati con BSS basata su BIC
bss_bic_coefs <- coef(regfit.bss, id = bss_min_bic_index)
print("I ceofficienti stimati con BSS basata su BIC sono: ")
bss_bic_coefs

# i2) calcolo MSE di test con BSS basata su BIC
selected_vars.bss_bic <- names(bss_bic_coefs)[-1]  # variabili selezionate da BSS a minimo BIC
x_test_selected <- x_test[, selected_vars.bss_bic]
print("L'MSE di test stimato con BSS a minimo BIC è:")
mse.bss_bic <- mean((y_test - (cbind(1, x_test_selected) %*% bss_bic_coefs))^2);
mse.bss_bic

# Plot del BIC
png("BSS_BIC.png", width=720, height=720)
plot(regfit.bss,scale = "bic")
dev.off()

# ii1)
##########################################
##############   Backward   ##############
##########################################

print("Backward")
# si effettua il fit con backward stepwise
regfit.bwd = regsubsets(Y~., data=train_set, nvmax=p, method="backward")
bwd_summary = summary(regfit.bwd);
bwd_summary

########################################
####  k-fold Cross Validation (k=5) ####
########################################
# Non c'è un metodo "predict()" in regsubsets dunque lo creiamo noi 
predict.regsubsets = function(object,newdata,id,...){ # ... <-> ellipsis
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id) # estrae i coefficienti del modello specificato da id.
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

# cross-validation fatta con regsubsets
k = 5
folds = sample(1:k,nrow(train_set),replace=TRUE)
# creiamo una matrice nella quale si memorizzeranno tutti gli errori di cross validation
cv.errors.bwd = matrix(NA,k,p, dimnames=list(NULL, paste(1:p))) 
# Ciclo per la cross-validation
for(j in 1:k){ # itera sui fold
  # si considera come training set il j-esimo fold con metodo backward
  best.fit = regsubsets(Y~., data=train_set[folds!=j,], nvmax=p, method="backward")
  for(i in 1:p){
    pred = predict(best.fit, train_set[folds==j,], id=i)
    cv.errors.bwd[j,i] = mean((train_set$Y[folds==j]-pred)^2) # MSE
  }
}
# Calcolo la media degli MSE per ogni modello
mean.cv.errors.bwd=colMeans(cv.errors.bwd) 
# Plot dimensione-mse
png("bwd_cverr.png", width=720, height=720)
par(mfrow=c(1,1))
plot(mean.cv.errors.bwd, type="b")
dev.off()

# Modello che minimizza l'MSE di test
optimal_model <- which.min(mean.cv.errors.bwd)
print("Modello scelto per 5-fold cross-validation con backward:")
optimal_model # dimensione 13
# Minimo MSE stimato con CV
min(mean.cv.errors.bwd)

# Fit con tutto il training-set per stimare i coefficienti ottimo
reg.best.bwd = regsubsets(Y~., data=train_set, nvmax=p, method="backward")

# Calcolo coefficienti del modello ottimo con 5-fold CV 
bwd_cv_coefs <- coef(reg.best.bwd, id = optimal_model) 
print("Coefficienti stimati del modello ottimo con 5-fold CV:");
bwd_cv_coefs

# ii2)
# calcolo mse di test con backward che utilizza 5-fold cross validation 
selected_vars.bwd_cv <- names(bwd_cv_coefs)[-1]  # variabili selezionate da CV
x_test_selected <- x_test[, selected_vars.bwd_cv]
mse.bwd_cv <- mean((y_test - (cbind(1, x_test_selected) %*% bwd_cv_coefs))^2);
print("MSE di test stimato con backward che utilizza 5-fold cross validation:")
mse.bwd_cv

# iii1)
###################################################################
###############      Ridge regression         #####################
###################################################################
print("Ridge regression")
library(glmnet)

x = model.matrix(Y~., train_set)[,-1] # x di training tranne intercetta
y = train_set$Y

# ridge regression model
ridge.mod = glmnet(x, y, alpha=0)

# Cross-validation 
cv.out.ridge = cv.glmnet(x, y, alpha=0)

# fit con lambda che minimizza MSE per Ridge
bestlam.ridge <- cv.out.ridge$lambda.min

print("Valore migliore di lambda per Ridge regression:")
bestlam.ridge
ridge.final = glmnet(x, y, alpha=0, lambda=bestlam.ridge)
print("Coefficienti stimati con Ridge regression con lambda migliore:")
ridge.coef = predict(ridge.final, type="coefficients"); 
ridge.coef

# Plot che mostra i coefficienti al variare di lambda con Ridge regression
library(plotmo)
png("Ridge_coeff.png", width=720, height=720)
plot_glmnet(ridge.mod)
mtext("Ridge", font=2, line=-1.5)
# è stata aggiunta sul grafico una linea tratteggiata per la miglior lambda
abline(v=log(bestlam.ridge), col="blue", lty=2) 
dev.off()

# iii2)
# calcolo MSE di test per Ridge
ridge.pred = predict(ridge.final, s=bestlam.ridge, newx=x_test)
mse.ridge <- mean((ridge.pred-y_test)^2);
print("MSE di test stimato con Ridge regression:")
mse.ridge

# iv1)
###################################################################
###############      LASSO regression         #####################
###################################################################
print("Lasso")

# LASSO regression model
lasso.mod = glmnet(x, y, alpha=1)

# Cross-validation 
cv.out.lasso = cv.glmnet(x, y, alpha=1)

# fit con lambda che minimizza MSE per Lasso
bestlam.lasso <- cv.out.lasso$lambda.min
print("Valore migliore di lambda per LASSO regression:")
bestlam.lasso

lasso.final = glmnet(x, y, alpha=1, lambda=bestlam.lasso)
lasso.coef = predict(lasso.final, type="coefficients"); 
# Viene effettuata variable selection, 6 predittori hanno coefficiente nullo
print("Coefficienti stimati con LASSO regression con lambda migliore:")
lasso.coef
# Regressori scartati da LASSO
print("Regressori scartati da LASSO regression:")
names(lasso.coef[which(lasso.coef == 0),])

# Plot che mostra i coefficienti al variare di lambda con Lasso regression
library(plotmo)
png("Lasso_coeff.png", width=720, height=720)
plot_glmnet(lasso.mod)
mtext("Lasso", font=2, line=-1.5)
# è stata aggiunta sul grafico una linea tratteggiata per la miglior lambda
abline(v=log(bestlam.lasso), col="blue", lty=2)
dev.off()

# iv2) calcolo MSE di test con Lasso
lasso.pred = predict(lasso.final, s=bestlam.lasso, newx=x_test)
print("MSE di test stimato con LASSO regression:")
mse.lasso <- mean((lasso.pred-y_test)^2); 
mse.lasso

##############################################################
###########    Confronto tra le strategie         ############
##############################################################
# Si crea un vettore con i vari MSE di test calcolati in precedenza
mse_values <- setNames(
  c(mse.bss_bic, mse.bwd_cv, mse.lasso, mse.ridge),
  c("bss_bic", "bwd_cv", "lasso", "ridge")
); mse_values
model_min_mse <- mse_values[which.min(mse_values)]; # si seleziona il min
print("Il modello che minimizza l'MSE di test è")
model_min_mse
model_min_mse <- names(model_min_mse)

###
if (model_min_mse == "lasso") {
  print("Lasso è la strategia migliore")
  # si crea la matrice di design su tutto il dataset
  x = model.matrix(Y~., dataset)[,-1]
  y = dataset$Y
  
  # si effettua la cross validation per la valutazione della migliore lambda
  cv.out = cv.glmnet(x, y, alpha=1)
  bestlam <- cv.out$lambda.min
  # fit con Lasso regression con la migliore lambda
  lasso.final = glmnet(x, y, alpha=1, lambda=bestlam)
  
  lasso.coef = predict(lasso.final, type="coefficients")
  print("Ecco i regressori scelti e i rispettivi coefficienti:")
  print(lasso.coef)
} else if (model_min_mse == "ridge") {
  print("Ridge regression è la strategia migliore")
  # si crea la matrice di design su tutto il dataset
  x = model.matrix(Y~., dataset)[,-1]
  y = dataset$Y
  
  # si effettua la cross validation per la valutazione della migliore lambda
  cv.out = cv.glmnet(x, y, alpha=0)
  bestlam <- cv.out$lambda.min
  # fit con Ridge regression con la migliore lambda
  ridge.final = glmnet(x, y, alpha=0, lambda=bestlam)
  
  ridge.coef = predict(ridge.final, type="coefficients")
  print("Ecco i regressori scelti e i rispettivi coefficienti:")
  print(ridge.coef)
} else {
  # plotting di Cp, AIC, BIC ed adjusted R^2 per lo stepwise selezionato
  if (grepl("bwd", model_min_mse)) {
    method_selected <- bwd_summary
    print("Backward è la strategia migliore. Ecco i regressori scelti e i relativi coefficienti:")
    bwd_cv_coefs
  } else if (grepl("bss", model_min_mse)) {
    method_selected <- bss_summary
    print("Best Subsets Selection è la strategia migliore. Ecco i regressori scelti e i relativi coefficienti:")
    bss_bic_coefs
  }
  
  png("bestmethod_CpBICAdjr2.png", width=720, height=720)
  par(mfrow=c(2,2))
  
  plot(method_selected$rss, xlab="Number of Variables ", ylab="RSS", type="l")
  points(which.min(method_selected$rss),min(method_selected$rss),col="red",cex=2,pch=20)
  
  plot(method_selected$adjr2, xlab="Number of Variables ", ylab="Adjusted RSq", type="l")
  points(which.max(method_selected$adjr2),max(method_selected$adjr2),col="red",cex=2,pch=20)
  
  plot(method_selected$cp, xlab="Number of Variables ", ylab="Cp", type="l")
  points(which.min(method_selected$cp ),min(method_selected$cp),col="red",cex=2,pch=20)
  
  plot(method_selected$bic, xlab="Number of Variables ",ylab="BIC",type="l")
  points(which.min(method_selected$bic),min(method_selected$bic), col = "red", cex = 2, pch = 20)
  dev.off()
}

