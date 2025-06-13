#Aquest scrip serveix per crear un tibble amb les dades i els resultats dels tres meta-learners
#Esta pensat per carregar varaible a varaible per poder veure i personalitzar els models d'entrenament per obtenir la millor performance possible

#------------------------------------------------------------------

#He d'escollir resposta:
#numerics: PesoRN , percentil_birth
#factors: SGA_birth_hard_imput, Complic_Extra, PE, PEearly, PElate, severeSGA, SGA_prenatal
resposta<-"SGA_prenatal"

#triar 2="reduccio_Stress" ; 3="Dieta_mediterrania"
tract<-3

num_arbres<-500

####=====--     Carregar dades(només farà falta un cop)     --=====####
if(!exists("dades")){ #perque només ho faci un cop
  dades1<-read_spss("dades_experiment")
  dades<-dades1 #gurado separat les dades originals
  
  #Passo les vaiables categoriques a factor
  numeriques<- c("Cod", "Edad", "Talla", "BMIpre", "PesoRN", "percentil_birth")
  fac<-setdiff(names(dades1), numeriques)
  
  dades<- dades %>%
    mutate(across(all_of(fac), as.factor)) %>%
    mutate(across(setdiff(fac, c("Group_COD", "Parto_tipo", "SexoRN")),
                  ~fct_recode(.x, "No" = "0", "Si" = "1"))) %>%
    mutate(Edad=as.numeric(Edad)) #si no queda com a nom una etiqueta
  
  
  #creo un vector amb els outcomes i un amb les covariables
  covariates <- names(dades) %>%
    head(18) %>%
    setdiff(c("Cod","Group_COD"))
  
  outcomes<- names(dades) %>% tail(9)
}



####=====--   Arreglo dades i objectes per aplicar meta-learners (dat0)   --=====####

#arreglo les dades
dat0<- dades %>%
  filter(Group_COD==1 | Group_COD==tract) %>%
  rename(Y=resposta) %>%
  rename(T=Group_COD) %>%
  select(all_of(c("T", covariates, "Y")))

dat0<- dat0[complete.cases(dat0),]


#creo els objectes que necessitare
if (is.factor(dat0$Y)==T & length(levels(dat0$Y))==2){
  tipus_resp<-"binaria"
  summary_model<-twoClassSummary
  metrica<-"ROC"
  norma_split<-"gini" # "hellinger" millor per desbalancejades
  
} else if (is.factor(dat0$Y)==T & length(levels(dat0$Y)) > 2){ 
  tipus_resp<-"multiclasse"
  summary_model<-multiClassSummary
  metrica<-"Accuracy"
  norma_split<-"gini" 
  
} else {
  tipus_resp<-"numerica"
  summary_model<-defaultSummary
  metrica<-"RMSE"
  norma_split<-"variance"
}

print(tipus_resp) #per comprobar que ha anat bé

oj_trControl<-trainControl(method = "repeatedcv",
                           number = 10,
                           savePredictions = "final",
                           summaryFunction = summary_model,
                           classProbs = TRUE==is.factor(dat0$Y))


#### =========       S-learner        ========= ####

#creo una copia de les dades per poder afegir coses
datS<-dat0
datS$Tdoble<-datS$T

mu_fit <- train(
  Y~.,
  data = datS, 
  method = "ranger",
  metric = metrica,
  tuneGrid = expand.grid(mtry = 3:7,
                         splitrule = norma_split, 
                         min.node.size = c(10, 15, 20)),
  importance="impurity",
  trControl = oj_trControl,
  num.trees = num_arbres*2,
  keep.inbag = TRUE)

mu_fit #model que utilitzarem

#Copia temporal de datS per poder aplicar el contrafactuals
datS_TMP <- datS

#Amb el model calculo els potencials (amb i sense tractament pels dos grups)
datS_TMP$T <- factor(1, levels = levels(datS$T))
mu0_hat_s <- predict(mu_fit, newdata = datS_TMP, type=ifelse(tipus_resp=="binaria","prob", "raw"))

datS_TMP$T <- factor(tract, levels = levels(datS$T))
mu1_hat_s <- predict(mu_fit, newdata = datS_TMP, type=ifelse(tipus_resp=="binaria","prob", "raw"))

#en el cas de que sigui binaria, agafo la probabilitat de "Si"
if (tipus_resp=="binaria"){
  mu0_hat_s<- mu0_hat_s$Si
  mu1_hat_s<- mu1_hat_s$Si}

#calculo els ITE (els efectes del tractament per cada individu)
cate_s <- as.numeric(mu1_hat_s) - as.numeric(mu0_hat_s)

datS$CATE<-cate_s



#### =========      T-learner       ======== ####

# faig una copia de les dades per poder-hi canviar el que sigui necessari 
dat1<-dat0

# creo dades amb el subsample de control i tractament
dat1_0 <- dat1[dat1$T == 1,]
dat1_1 <- dat1[dat1$T == tract, ]

#Model mu_0
mu0_fit <- train(
  Y~. -T,
  data = dat1_0, 
  method = "ranger",
  metric = metrica,
  trControl = oj_trControl,
  tuneGrid = expand.grid(mtry = 4:10,
                         splitrule = norma_split,
                         min.node.size = c(10,15,20)),
  num.trees = num_arbres,
  importance = "impurity")
mu0_fit


#Model mu_1
mu1_fit <- train(
  Y~. -T,
  data = dat1_1, 
  method = "ranger",
  metric = metrica,
  trControl = oj_trControl,
  tuneGrid = expand.grid(mtry = 4:10,
                         splitrule = norma_split,
                         min.node.size = c(10,15,20)),
  num.trees = num_arbres,
  importance = "impurity")
mu1_fit

#creo objectes on guardaré les dades predites amb els models
mu0_hat <- rep(0, nrow(dat1)) #pels pontencials amb control
mu1_hat <- rep(0, nrow(dat1)) #pels pontencials amb tractament

#guardo les prediccions als objectes que em creat abans
if (tipus_resp=="binaria"){
  #tractats sense tractament (contrafactual)
  mu0_hat[dat1$T == tract] <- predict(mu0_fit, dat1_1, type="prob")$Si
  #tractats amb tractament
  mu1_hat[dat1$T == tract] <- predict(mu1_fit, dat1_1, type="prob")$Si
  
  #controls sense tractament
  mu0_hat[dat1$T == 1] <- predict(mu0_fit, dat1_0, type="prob")$Si
  #controls smb tractament (contrafactual)
  mu1_hat[dat1$T == 1] <- predict(mu1_fit, dat1_0, type="prob")$Si
} else{
  #tractats
  mu0_hat[dat1$T == tract] <- predict(mu0_fit, dat1_1)
  mu1_hat[dat1$T == tract] <- predict(mu1_fit, dat1_1)
  
  #controls
  mu0_hat[dat1$T == 1] <- predict(mu0_fit, dat1_0)
  mu1_hat[dat1$T == 1] <- predict(mu1_fit, dat1_0)
}


#computo CATE
cate_t <- mu1_hat - mu0_hat
dat1$CATE<-cate_t

datT<-dat1



#### ========      X-learner      ========= ####

# igual que abans faig una copia per no tocar l'original
dat1<-dat0

# creo dades separades segons si són control o tractament
dat1_0 <- dat1[dat1$T == 1, ]
dat1_1 <- dat1[dat1$T == tract, ]

#Model mu_0 
mu0_fit <- train(
  Y~. -T,
  data = dat1_0, 
  method = "ranger",
  metric = metrica,
  trControl = oj_trControl,
  tuneGrid = expand.grid(mtry = 4:10,
                         splitrule = norma_split,
                         min.node.size = c(10,15,20)),
  num.trees = num_arbres,
  importance = "impurity")
mu0_fit


#Model mu_1
mu1_fit <- train(
  Y~. -T,
  data = dat1_1, 
  method = "ranger",
  metric = metrica,
  trControl = oj_trControl,
  tuneGrid = expand.grid(mtry = 4:10,
                         splitrule = norma_split,
                         min.node.size = c(10,15,20)),
  num.trees = num_arbres,
  importance = "impurity")

mu1_fit


# calculo els pseudo-valors amb les prediccions del model i les observacions
if (tipus_resp=="binaria"){
  #controls:  pseudo D = predicció contrafactual - realitat 
  psi_x_0 <- predict(mu1_fit, dat1_0, type="prob")$Si - as.numeric(dat1_0$Y)
  
  #tractats:  pseudo D = realitat - predicció contrafactual
  psi_x_1 <- as.numeric(dat1_1$Y) - predict(mu0_fit, dat1_1, type="prob")$Si
  
} else{
  #controls
  psi_x_0 <- predict(mu1_fit, dat1_0) - as.numeric(dat1_0$Y)
  
  #tractats
  psi_x_1 <- as.numeric(dat1_1$Y) - predict(mu0_fit, dat1_1)
}

#preparo dades per entrenar el model amb els pseudo-valors
dades_psi0<- dat1_0[covariates]%>%
  mutate(psi_0=psi_x_0)

dades_psi1<- dat1_1[covariates]%>%
  mutate(psi_1=psi_x_1)

# Amb els pseudo-valors faig els models separats de control i tractament

tau_x_0_fit <- train(
  psi_0~.,
  data = dades_psi0, 
  method = "ranger",
  metric="RMSE",
  trControl = trainControl(method = "cv",
                           number = 10,
                           savePredictions = "final",
                           summaryFunction = defaultSummary),
  tuneGrid = expand.grid(mtry = 4:10,
                         splitrule = "variance",
                         min.node.size = c(10,15,20)),
  num.trees = num_arbres)

tau_x_1_fit <- train(
  psi_1~.,
  data = dades_psi1, 
  method = "ranger",
  metric="RMSE",
  trControl = trainControl(method = "cv",
                           number = 10,
                           savePredictions = "final",
                           summaryFunction = defaultSummary),
  tuneGrid = expand.grid(mtry = 4:10,
                         splitrule = "variance",
                         min.node.size = c(10,15,20)),
  num.trees = num_arbres)


#Predic el efecte per cada grup

# Potencials al control
tau_x_0_hat <- rep(0, nrow(dat1))
tau_x_0_hat[dat1$T == 1] <- predict(tau_x_0_fit, dat1_0)
tau_x_0_hat[dat1$T == tract] <- predict(tau_x_0_fit, dat1_1)

# Potencials al tractament
tau_x_1_hat <- rep(0, nrow(dat1))
tau_x_1_hat[dat1$T == tract] <- predict(tau_x_1_fit, dat1_1)
tau_x_1_hat[dat1$T == 1] <- predict(tau_x_1_fit, dat1_0)

#Estimo el propensity score EN AQUEST CAS (EXPERIMENT ALEATORITZAT NO TÈ SENIT FER-HO)
#ho faig per poder comparar amb el que donaria amb PS
ps_fit <- ranger(y = dat1$T, x = dat1[, covariates], probability = TRUE)
ps_hat <- ps_fit$predictions[,2] 

#M'hasseguro de la possitivitat (tots amb almenys 1% d'estar als dos grups)
epsilon <- .01
ps_hat <- ifelse(ps_hat < epsilon, epsilon, ifelse(ps_hat > 1 - epsilon, 1 - epsilon, ps_hat))

#Compute the CATE as propensity score-weighted combination of the group-specific estimates
cate_x2 <- ps_hat * tau_x_0_hat + (1 - ps_hat) * tau_x_1_hat

cate_x<- 0.5 * tau_x_0_hat + (1 - 0.5) * tau_x_1_hat
dat1$CATE<-cate_x


datX<-dat1


####  ------    Guardar dades   --------   ####
dat_final<-datX %>% mutate(
  CATE_s=cate_s,
  CATE_t=cate_t,
  CATE_x2=cate_x2 #X learner utilitzant el propensity score
)

# NOMÉS EL PRIMER COP, crear les dades
if (!exists("resultats")) {
  resultats<- expand.grid(variable=outcomes,
                          tractament=c(2,3)) %>% as_tibble() %>%
    mutate(dades = vector("list", n()))
}


idx <- which(resultats$variable == resposta & resultats$tractament == tract)
resultats$dades[[idx]] <- dat_final