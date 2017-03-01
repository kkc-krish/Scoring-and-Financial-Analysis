
#Telechargement de la base de donnée#

tab=read.csv("~/Desktop/final.csv",header=TRUE,sep=";")
head(tab)
attach(tab)

Reponse=as.factor(Reponse)

tab2=tab[,4]
tab3=tab[,6:28]
tab1=cbind(tab2,tab3)

#regression logistique classique#

rep<-glm(Reponse~.,data=tab,family=binomial())
summary(rep)

#score test data set#

library(ROCR)

tab1=cbind(Entreprise,Reponse,Rent.fi,Ratio.endet,Provision.risques.charges.non.courant,Prod.acti.ordi,Marge.ope,Immo.corp,Dettes.fi.non.courantes,Creances.clients.cmptes.rat,Cout.endettement.financier.net,Effectif,Capitaux.propres)

test<-predict(Reponse,type='response',tab[,6:28])

pred<-prediction(test,Reponse)

perf <- performance(pred,"tpr","fpr")

plot(perf)


#get results of terms in regression#
g<-predict(rep,type='terms',tab1)

#function to pick top 3 reasons#
#works by sorting coefficient terms in equation#
# and selecting top 3 in sort for each loan scored#
ftopk<- function(x,top=3){
    res=names(x)[order(x, decreasing = TRUE)][1:top]
    paste(res,collapse=";",sep="")
}

# Application of the function using the top 3 rows#
topk=apply(g,1,ftopk,top=3)

#add reason list to scored tets sample#
# test<-cbind(test, topk)

#load tree package
library(rpart)
fit1<-rpart(Reponse~.,data=tab)

fit1=rpart(Reponse~Rent.fi+Ratio.endet+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.corp+Dettes.fi.non.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+Effectif+Capitaux.propres)

plot(fit1);text(fit1);

fit1=rpart(Reponse~Total.actifs+Treso+Stocks.trav.en.cours+Rent.fi+Ratio.endet+Resultat.ope+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Immo.corp+Fournisseurs.cmptes.rat+Ecart.daquisition+Dettes.fi.non.courantes+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+CA.annuel+Actifs.fi.non.courant+Benefice.net+Effectif+Capitaux.propres+Autres.actifs)
plot(fit1);text(fit1);


rep<-glm(Reponse~Total.actifs,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Autres.actifs,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Capitaux.propres,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Effectif,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Benefice.net,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Actifs.fi.non.courant,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~CA.annuel,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Cout.endettement.financier.net,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Creances.clients.cmptes.rat,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Dettes.fi.courantes,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Dettes.fi.non.courantes,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Ecart.daquisition,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Fournisseurs.cmptes.rat,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Immo.corp,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Immo.incorp,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Marge.ope,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Prod.acti.ordi,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Provision.risques.charges.non.courant,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Prod.acti.ordi,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Resultat.ope,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Ratio.endet,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Rent.fi,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Stocks.trav.en.cours,data=tab,family=binomial())
summary(rep)
rep<-glm(Reponse~Treso,data=tab,family=binomial())
summary(rep)

rep<-glm(Reponse~.,data=tab,family=binomial())

rep=lm(Cours~Total.actifs)
summary(rep)

test=Total.actifs
test=(test-mean(test))/var(test)

rep=lm(Cours~test)
summary(rep)
rep=lm(Cours~Total.actifs)
summary(rep)

plot(Cours~Total.actifs)

rep=lm(Cours~Autres.actifs)
summary(rep)


rep=lm(Cours~Capitaux.propres)
summary(rep)

rep=lm(Cours~Effectif)
summary(rep)

rep=lm(Cours~Benefice.net)
summary(rep)

rep=lm(Cours~Actifs.fi.non.courant)
summary(rep)

rep=lm(Cours~CA.annuel)
summary(rep)

rep=lm(Cours~Cout.endettement.financier.net)
summary(rep)

rep=lm(Cours~Creances.clients.cmptes.rat)
summary(rep)

rep=lm(Cours~Dettes.fi.courantes)
summary(rep)

rep=lm(Cours~Dettes.fi.non.courantes)
summary(rep)

rep=lm(Cours~Ecart.daquisition)
summary(rep)

rep=lm(Cours~Fournisseurs.cmptes.rat)
summary(rep)

rep=lm(Cours~Immo.corp)
summary(rep)


rep=lm(Cours~Immo.incorp)
summary(rep)


rep=lm(Cours~Marge.ope)
summary(rep)



rep=lm(Cours~Prod.acti.ordi)
summary(rep)


rep=lm(Cours~Provision.risques.charges.non.courant)
summary(rep)


rep=lm(Cours~Resultat.ope)
summary(rep)


rep=lm(Cours~Ratio.endet)
summary(rep)

rep=lm(Cours~Rent.fi)
summary(rep)

rep=lm(Cours~Stocks.trav.en.cours)
summary(rep)

rep=lm(Cours~Treso)
summary(rep)


rep=lm(Cours~Total.actifs+Treso+Stocks.trav.en.cours+Rent.fi+Ratio.endet+Resultat.ope+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Immo.corp+Fournisseurs.cmptes.rat+Ecart.daquisition+Dettes.fi.non.courantes+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+CA.annuel+Actifs.fi.non.courant+Benefice.net+Effectif+Capitaux.propres+Autres.actifs)
summary(rep)


1 -

moins immo corp , resultat ope , eccart dacqui, actif non courant

rep=lm(Cours~Total.actifs+Treso+Stocks.trav.en.cours+Rent.fi+Ratio.endet+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Fournisseurs.cmptes.rat+Dettes.fi.non.courantes+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+CA.annuel+Benefice.net+Effectif+Capitaux.propres+Autres.actifs)
summary(rep)

encore un peu mieux

1bis -

on enleve

rep=lm(Cours~Total.actifs+Rent.fi+Ratio.endet+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+CA.annuel+Benefice.net+Effectif)
summary(rep)

1bisbis -

on enleve

rep=lm(Cours~Rent.fi+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Creances.clients.cmptes.rat+CA.annuel+Benefice.net)
summary(rep)

2 -

que les bons

rep=lm(Cours~Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Creances.clients.cmptes.rat+CA.annuel)
summary(rep)

3 -

sans marge ope

rep=lm(Cours~Provision.risques.charges.non.courant+Prod.acti.ordi+Immo.incorp+Creances.clients.cmptes.rat+CA.annuel)
summary(rep)

tout est significatif

4-

test avec la regre logistique et que les bons

rep=glm(Reponse~Provision.risques.charges.non.courant+Prod.acti.ordi+Immo.incorp+Creances.clients.cmptes.rat+CA.annuel)
summary(rep)

5-

test regre logistique avec tous


rep=glm(Reponse~Total.actifs+Treso+Stocks.trav.en.cours+Rent.fi+Ratio.endet+Resultat.ope+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Immo.corp+Fournisseurs.cmptes.rat+Ecart.daquisition+Dettes.fi.non.courantes+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+CA.annuel+Actifs.fi.non.courant+Benefice.net+Effectif+Capitaux.propres+Autres.actifs)
summary(rep)

rep=glm(Reponse~Total.actifs+Treso+Stocks.trav.en.cours+Rent.fi+Ratio.endet+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Immo.corp+Fournisseurs.cmptes.rat+Ecart.daquisition+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+CA.annuel+Actifs.fi.non.courant+Benefice.net+Effectif+Capitaux.propres+Autres.actifs,family=binomial())
summary(rep)

que les bons

rep=glm(Reponse~Rent.fi+Provision.risques.charges.non.courant+Benefice.net)
summary(rep)

rep=glm(Reponse~Rent.fi+Provision.risques.charges.non.courant+Benefice.net+Creances.clients.cmptes.rat)
summary(rep)

un peu plus

rep=glm(Reponse~Rent.fi+Provision.risques.charges.non.courant+Benefice.net+Creances.clients.cmptes.rat+CA.annuel)
summary(rep)


Reponse=as.factor(Reponse)




str_constant <- "~ 1"

str_all <- "~Total.actifs+Treso+Stocks.trav.en.cours+Rent.fi+Ratio.endet+Resultat.ope+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.incorp+Immo.corp+Fournisseurs.cmptes.rat+Ecart.daquisition+Dettes.fi.non.courantes+Dettes.fi.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+CA.annuel+Actifs.fi.non.courant+Benefice.net+Effectif+Capitaux.propres+Autres.actifs"



modele <- glm(chd ~ 1, data = tab[,4:28], family = binomial)

modele.forward <- stepAIC(modele, scope = list(lower = str_constant, upper = str_all),
trace = TRUE, data = tab[,4:28], direction = "forward")

Breaktotact = c(-0.3659, 0.0236, 0.1906, 0.4196 , max(Total.actifs))
Total.actifs.d = cut(Total.actifs, breaks = Breaktotact, include.lowest = TRUE)
summary(Total.actifs.d)

rep=glm(Reponse~Rent.fi+Ratio.endet+Provision.risques.charges.non.courant+Prod.acti.ordi+Marge.ope+Immo.corp+Dettes.fi.non.courantes+Creances.clients.cmptes.rat+Cout.endettement.financier.net+Effectif+Capitaux.propres,family=binomial())
summary(rep)
hoslem.test(Reponse, fitted(rep), g=12)

