setwd("C:\\Users\\K.KHALDI\\Documents\\INSA\\projet\\data")
train=read.csv("train.txt", sep="|", dec = ",", stringsAsFactors = FALSE)
names(train)=c("clicked", "depth", "position", "userid", "gender", "age", "text")
closeAllConnections()

nfile <- file("train.txt", open = "r")


n=0
data=NULL

# Lecture de la premi`ere ligne de donn´ees
oneLine = readLines(nfile, n = 1, warn = FALSE)
x = scan(text=oneLine, sep="|", quiet=T, what="character")




#x[[5]]


## REGRESSION LINEAIRE SUR Depth, Position, Age

p = length(x)-4 # Nombre de regresseurs
# Initialisation
lambda = 1
Q = diag(p+1) * lambda
theta = rep(0, (p+1))
nval = 1

#x[2] correspond au nombre de d'annonces affichées ur la session (Depth)
#x[3] correspond à la position 
#x[6] correspond à l'âge

while (length(oneLine <- readLines(nfile, n =1, warn = FALSE)) > 0)

{
  #data = rbind(data, x)
  
  X=as.matrix(as.numeric(t(cbind(c(1,x[2:3], x[6])))))
  Y=as.matrix(as.numeric(x[1]))
  #on transpose pas X
  fi <-X
  
  l=as.numeric((1+t(fi)%*%Q%*%fi)^(-1))
  Q=Q-l*(Q %*% fi %*% t(fi) %*% Q)
  theta=theta+Q%*%fi %*% (Y-t(fi)%*%theta)
  
  x = scan(text=oneLine, sep="|", quiet=T, what="character")
  
  nval=nval+1
  
  if(nval%%10000 == 0)
  {
    print(paste("iteration=",nval))
  }
  
}



#erreur

closeAllConnections()

nfile <- file("train.txt", open = "r")


n=1

# Lecture de la premi`ere ligne de donn´ees
oneLine = readLines(nfile, n = 1, warn = FALSE)
x = scan(text=oneLine, sep="|", quiet=T, what="character")



eqm <- 0
n <-1
thet <- theta[1:4]
var <- 0
YBar <- 0

while (length(oneLine <- readLines(nfile, n = 1, warn = FALSE)) > 0) {
  # Mise `a jour de theta
  
  data = rbind(data, x)
  
  X=as.matrix(as.numeric(t(cbind(c(1,x[2:3], x[6])))))
  Y=as.matrix(as.numeric(x[1]))
  
  
  fi <- t(X)
  
  var <- ((n-1)/n)*var + ((n-1)/n**2)*(YBar - t(Y))**2
  YBar <- YBar + (1/n)*(t(Y)-YBar)
  
  Ychap <- t(theta)%*%X
  
  eqm <- (n/(n+1))*eqm + (1/n)*(t(Y) - Ychap)**2
  
  
  print(paste("iteration", n, "EQM", eqm , "Var", var))
  
  n = n + 1
}


# Variance expliquée

R2 = 1 - (eqm/var)







### REG LOG
#pred du click en fonction du nombre de pub affichées, la position de l'affiche cliquée, identification de l'utilisateuret le sexe


reg <- glm(clicked ~ depth + position + userid + gender , data = train, family = binomial(logit))
reg


#
summary(reg)
