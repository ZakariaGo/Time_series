#Ce projet a pour objectif de classifier des series temporelles en tutilisatn l'algorithms de programation dynamique de Fisher.

#Importation des données "simu" depuis la source.


data_simu = read.table("http://allousame.free.fr/mlds/donnees/simu.txt", header = FALSE)


# Importation des données "Aiguillage" depuis la source.

real_data = read.table("http://allousame.free.fr/mlds/donnees/aiguillage.txt", header = FALSE,sep = ",")

head(data_simu)

head(real_data)

target= as.matrix(real_data[,553]) 

real_data_features = as.matrix(real_data[,-553])

dim(data_simu)

dim(real_data_features)
# Creation de la fonction : "diam" , pour la calcule de la matrice des diametres des classes selon la formule 2.
diam <- function(data, n) {
  #init matrix D
  D <- matrix(data = 0, nrow = n, ncol = n)
  for (a in 1:(n-1)){
    j <- a+1 
    for (b in j:n){
      mu = colMeans(as.matrix(data[a:b,])) 
      vect <- vector("numeric", length = b)
      ind <- 1
      for (i in a:b){
        temp <- (data[i,] - mu)
        tp <- sum(temp^2)
        vect[ind] <- tp
        ind <- ind +1
      }
      D[a,b] <- sum(vect) 
    }
  }
  D
}



D = diam(data_simu, dim(data_simu)[1])




# Implementation de l'algorithme de programation dynamique de Fisher.


clustficher <- function(data, number_clusters)  {   
  
  # Etape_Initiale : Initialisation de la Matrice "D", "M1" et "M2", et "t" ; Vecteur des instants de changements ainsi que "cluster" , vecteur de classes.
  
  n <-  dim(data)[1]
  
  
  D <- matrix(data = 0, nrow = n, ncol = n)
  
  # Initialisation de la matrice M1  
  
  M1 <- matrix(data = 0, nrow = n, ncol = number_clusters)
  
  
  # Initialisation de la matrice M2 
  
  
  M2 <- matrix(data = 0, nrow = n, ncol = number_clusters)
  
  # temp : une variable temporaire , pour ajuster le nombre des instants de changement en dimension
  
  
  temp <- number_clusters -1 
  
  
  # Vecteur des instants de changement "t" 
  
  t <- array(data =0, dim = temp) 
  
  
  # cluster : Vecteur de Classes
  
  
  cluster <- array(dim=n)
  
  # Etape 1 : Calcul de la matrice des diametres des classes D.   
  
  D <- diam(data,n)
  
  # Etape 2 : Caclul recursif des criteres Optimaux : M1 et M2.
  
  
  M1[,1] <- D[1,]
  
  for (k in c(2:number_clusters)) {
    for (i in c(k:n)) {
      v <- vector(mode="numeric",length= (i-k) +1 )
      j <- 0 
      for(t in c(k:i)) {
        j <- j+1 
        v[j] <- M1[t-1,k-1] + D[t,i]
      }
      M1[i,k] <- min(v)
      M2[i,k] <- which.min(v) + k-1
    } 
  }
  
  # Etape 3 : Calcul recursif des instants de changements optimaux 
  
  k <- temp
  
  
  m <- n
  
  
  while (k >= 1) {
    
    t[k] <- M2[m,(k+1)]
    m <- t[k] -1
    k <- k-1
  }
  
  # Etape 4 : Labels des classes formes à partir des instants de changements 
  
  tp <- t[1] -1
  
  
  cluster[1:tp] <- 1
  
  for (k in c(2:temp)) {
    x <- t[k-1]
    y <- t[k]-1
    for (i in x:y){
      cluster[i] <- k 
    }
  }
  
  x <- t[temp]
  
  cluster[x:n] <- number_clusters
  
  # La méthode du coude 
  
  plot(M1[n,],type="b",lty=1:number_clusters,xlab = "Le nombre de clusters",ylab="% de l'inértie", main = "La méthode du coude")
  
  return(list(val1=cluster, val2=t))
}



res_sim <- clustficher(data_simu, 5) 


res_aiguillage <- clustficher(real_data_features, 5)


# * # * # * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *


# # lancer le K-means :  K-Means 

clustKmeans <- function(data, number_clusters){
  n <- dim(data)[1] 
  #d <- cbind(data,c(1:n)) 
  km <- kmeans(data, number_clusters, nstart = 20)
  #La méthode du coude
  inertie.expl <- rep(0,times=20)
  for (k in c(2:20)){
    clus <- kmeans(data,centers=k,nstart=20)
    inertie.expl[k] <- clus$betweenss/clus$totss
  }
  plot(1:5,inertie.expl,type="b",xlab="Nombre de clusters",ylab="% d'inertie",main = "La méthode du coude (KMeans)")
  km 
  
} 
# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *


# CAH



clustCah <- function(data, number_clusters){ 
  
  
  
  # la matrice de distance 
  
  
  
  n <- dim(data)[1]
  
  
  distance <- dist(data, method = "euclidean")
  
  # Application de l'algorithme 
  
  
  
  H <- hclust(distance, method="ward.D2")
  
  
  
  # Affichage du dendograme 
  
  
  
  plot(as.hclust(H),hang=-1)
  
  
  rect.hclust(as.hclust(H),k=number_clusters,border="blue")
  
  groupes.cah <- cutree(H,k=number_clusters)
  
  
  F <- cbind(c(1:n),data)
  
  plot(F, col=as.numeric(groupes.cah),xlab = "index",ylab = "data",main = "Algorithme CAH") 
  
  
  plot(cbind(c(1:n),groupes.cah), col=as.numeric(groupes.cah),xlab = "index",ylab = "clusters",main = "Algorithme CAH") 
  
  
  #la méthode du coude
  
  
  
  inertie <- sort(H$height, decreasing = TRUE)
  
  
  plot(inertie[1:20], type = "b", xlab = "Number of clusters",ylab = " % Inertie",main = "la méthode du coude (CAH)")
  
  
  groupes.cah
  
}


# Graphe de programmation dynamique de Fisher sur les donnees simulees 



plot(cbind(c(1:210),data_simu),col=as.numeric(res_sim$val1),xlab="index",ylab="Les donnees",main = "L'algorithme de programmation dynamique de Fisher") 
abline(v=res_sim$val2, col="blue") 


## Graphe de programmation dynamique de Fisher sur les donnees reelles 

plot(cbind(c(1:140),res_aiguillage$val1),col=as.numeric(res_aiguillage$val1),xlab="index",ylab="Les classes",main = "L'lagorithme de programmation dynamique de Fisher") 
abline(v=res_aiguillage$val2,col="blue") 


# Comparaison des donnees Aiguillage avec les vraies classes 

plot(cbind(c(1:140),target), col=as.numeric(target),xlab = "index",ylab = "les classes",main = "La partition selon les vraies classes") 
abline(v=res_aiguillage$val2,col="blue")

# Table de confusion 
tab <- table(res_aiguillage$val1, target)
print(tab)
prop.table(tab)


# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *


# Algorithme K_means sur les donn�es simulees 

# Affichage des r�sultats 

# Sans tenir de la variable temps 

F <- cbind(c(1:210),data_simu)

km_sim <- clustKmeans(data_simu,5) 




plot(F, col=as.numeric(km_sim$cluster),xlab = "index",ylab = "Data",main = "Algorithme de classification Kmeans") 


## En prennant en consideration le temps


plot(F, col=as.numeric(km_sim_temps$cluster),xlab = "index",ylab = "Les donnees ",main = "Kmeans sur les donnees simulees en considerant le temps") 


# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *# * # *


#Algorithme de la CAH 


cah_sim <- clustCah(data_simu,5)



# Affichage des r�sultats 


# Sans tenir de la variable temps 


plot(F, col=as.numeric(cah_sim),xlab = "index",ylab = "Data",main = "Algorithme de classification CAH") 


#Kmeans et CAH sur les donnees reelles : AIGUILLAGE 


cah_aiguillage <- clustCah(real_data_features,4)



kmeans_aiguillage <- clustKmeans(real_data_features,4) 


# Affichage des resultats 



plot(cbind(c(1:140),kmeans_aiguillage$cluster), col=as.numeric(kmeans_aiguillage$cluster),xlab = "index",ylab = "Les classes ",main = "Algorithme de classification Kmeans")


plot(cbind(c(1:140),cah_aiguillage), col=as.numeric(cah_aiguillage),xlab = "index",ylab = "Les classes ",main = "Algorithme de classification CAH") 





