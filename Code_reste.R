#Telechargement de la base de donnée#
tab=read.csv("~/Desktop/final.csv",header=TRUE,sep=";")
head(tab)
attach(tab)


#Regression logistique#
rep=glm(Reponse~Treso+Stocks.trav.en.cours+Rent.fi+Provision.risques.charges.non.courant+Prod.acti.ordi+Immo.incorp+Immo.corp+Ecart.daquisition+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+Resultat.net+Capitaux.propres+Autres.actifs,family=binomial())
summary(rep)

#Test de Hosmer Lemeshow #
library(ResourceSelection)
hoslem.test(Reponse, fitted(rep))

#Test résidus studentisés#
plot(rstudent(rep), type = "p", cex = 0.5, ylab = "Studentized residuals", col = "springgreen2", ylim = c(-3, 3))
abline(h = c(-2, 2), col = "red")

#Test chi deux#
(chi2 <- with(rep, null.deviance - deviance))
(ddl <- with(rep, df.null - df.residual))
(pvalue <- pchisq(chi2, ddl, lower.tail = F))

#Probabilités prédites#
fit=predict(rep, newdata = tab[,6:28], type = "link", se = TRUE)
PredictedProb <- plogis(fit$fit)

#Courbe ROC#
library(ROCR)
library(gplots)
library(gtools)
library(gdata)
library(gdtools)
Pred = prediction(PredictedProb, Reponse)
Perf = performance(Pred, "tpr", "fpr")
plot(Perf, colorize = TRUE, main = "ROC apprentissage")
perf <- performance(Pred, "auc")
perf@y.values[[1]]

#Représentation graphique des probabilités prédites#
plot(sort(PredictedProb),type="l",col = "blue",ylab="Sorted predicted probabilities")

#Scoring par classification#
library(rpart)
fit1=rpart(Reponse~Treso+Stocks.trav.en.cours+Rent.fi+Provision.risques.charges.non.courant+Prod.acti.ordi+Immo.incorp+Immo.corp+Ecart.daquisition+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+Resultat.net+Capitaux.propres+Autres.actifs)
plot(fit1);text(fit1);








EF<-read.table('C:/Users/Sofiane/Desktop/ter/eiffage.txt')
LE<-read.table('C:/Users/Sofiane/Desktop/ter/legrand.txt')
SA<-read.table('C:/Users/Sofiane/Desktop/ter/sanofi.txt')
SO<-read.table('C:/Users/Sofiane/Desktop/ter/sodexo.txt')
SE<-read.table('C:/Users/Sofiane/Desktop/ter/ses.txt')


r<-function(a){
    p= c()
    for (i in 0:58){
        p[i+1]=(a[60-(i+1),2]-a[60-i,2])/a[60-i,2]
    }
    p
}
r(EF);r(LE);r(SA);r(SO);r(SE)

M<-cbind(r(EF),r(LE),r(SA),r(SO),r(SE))
C<-cov(M)

## Portfolio optimization attempt ##

V<-matrix(nrow=7, ncol=10000)
for (i in 1:10000){
    c=c()
    c[1]=runif(1,0,1)
    c[2]=runif(1,0,1-c[1])
    c[3]=runif(1,0,1-c[1]-c[2])
    c[4]=runif(1,0,1-c[1]-c[2]-c[3])
    c[5]=1-c[1]-c[2]-c[3]-c[4]
    
    V[1,i]=c[1]*mean(r(EF))+c[2]*mean(r(LE))+c[3]*mean(r(SA))+c[4]*mean(r(SO))+c[5]*mean(r(SE))
    
    Q=0
    for (j in 1:5){
        Q=Q+sum(c[j]*c[1:5]*C[j,1:5])
    }
    
    V[2,i]=Q
    
    V[3,i]=c[1]
    V[4,i]=c[2]
    V[5,i]=c[3]
    V[6,i]=c[4]
    V[7,i]=c[5]
    
}
V

##Efficient frontier ##

plot(V[2,],V[1,], xlab="Risk", ylab="Return", main="Efficient frontier")

## Best return ##
B=1
for (i in 2:10000){
    if (V[1,i]>V[1,B]){
        B=i
    }
}
V[,B];B

## Lowest risk ##
W=1
for (i in 2:10000){
    if (V[2,i]<V[2,W]){
        W=i
    }
}
V[,W];W

## Best couple ##
J=1
y=V[,1]
for (i in 2:10000){
    if (V[2,i]<0.004){
        if (V[1,i]>0.012){
            y<-cbind(y,V[,i])
            J=i
        }
    }
}
y


## INVESTMENT ##

x<-y[,5]
x
sqrt(x[2])

t=c()
for (i in 3:7){
    t[i-2]=1000000*x[i]
}
t

## Stock volume ##

vol<-function(j,p){
    S=t[j]/p
    S
}

## Results ##

For alpha =0.507273, beta = 0.005545 , gamma = 0.010806, delta = 0.452410, epsilon = 0.025966


