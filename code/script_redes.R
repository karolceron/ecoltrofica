#########################################################
##### Introducao ao estudo de redes ecol?gicas #######
######################################################
# Karoline Ceron



#instalando os pacotes
install.packages("bipartite")
install.packages("igraph")

#carregando os pacotes
library("bipartite")
library("igraph")

#carregando os dados
rede<-bezerra2009 #rede ponderada
#Individuals observed in a flower-visitation network of oil-collecting
#bees in a Brazilian steppe

rede
redeb=rede
redeb[redeb>0] <-1 #binarizando a rede

#gerando graficos de rede
plotweb(redeb) #rede binaria
plotweb(rede) #ponderada

visweb(redeb)#binaria
visweb(rede) #ponderada


#grau
degb<-specieslevel(redeb, index="degree") #binario
degb
deg<-specieslevel(rede, index="degree") #ponderado
deg

colSums(redeb) #grau
colSums(rede) #força do no? - apenas para ponderado

#centralidade
cen<-specieslevel(rede, index="closeness") #binario e ponderado
cen

#tamanho da rede
size<-networklevel(rede, index="number of species") #binario e ponderado
size

#conectancia
conb <-networklevel(redeb, index="connectance") #binario e ponderado
conb

con <-networklevel(rede, index="connectance")#ponderado
con

#aninhamento
nodf.redeb <-networklevel(redeb, index="NODF")
nodf.redeb
suarede.random <- nullmodel(redeb,N=1000, method="vaznull")
nodf.ram.suarede <- unlist(sapply(suarede.random, networklevel, index="NODF"))
nodf.ram.suarede.mean <- mean(nodf.ram.suarede) 
nodf.ram.suarede.sd <- sd(nodf.ram.suarede) 
delta.nodf.suarede <- nodf.redeb-nodf.ram.suarede.mean
zscore.nodf<-delta.nodf.suarede/nodf.ram.suarede.sd
zscore.nodf # > 1.93 ? significativo


#weighted NODF
nodf.rede <-networklevel(rede, index="weighted NODF")
nodf.rede
suarede.random <- nullmodel(rede,N=1000, method="vaznull")
nodf.ram.suarede <- unlist(sapply(suarede.random, networklevel, index="weighted NODF"))
nodf.ram.suarede.mean <- mean(nodf.ram.suarede) 
nodf.ram.suarede.sd <- sd(nodf.ram.suarede) 
delta.nodf.suarede <- nodf.rede-nodf.ram.suarede.mean
zscore.nodf<-delta.nodf.suarede/nodf.ram.suarede.sd
zscore.nodf # > 1.93 ? significativo



#modularidade bin?ria
modsuarede<-computeModules(redeb, method="DormannStrauss")
modsuarede@likelihood #modularidade
printoutModuleInformation(modsuarede) #ver agrupamentos
suarede.random <- nullmodel(redeb,N=100, method="vaznull")
mod.ram.suarede<- sapply(suarede.random, computeModules, method="DormannStrauss")
mod.ram <- sapply(mod.ram.suarede, function(x) x@likelihood)
mod.ram.suarede.mean <- mean(mod.ram) 
mod.ram.suarede.sd <- sd(mod.ram) 
delta.mod.suarede <- modsuarede@likelihood-mod.ram.suarede.mean
zscore.mod<-delta.mod.suarede/mod.ram.suarede.sd
zscore.mod # > 1.93 ? significativo


#modularidade ponderada

modsuarede<-computeModules(rede, method="Beckett", deep = FALSE, deleteOriginalFiles = TRUE, 
                           steps = 1E7, tolerance = 1e-10, experimental = FALSE, forceLPA=FALSE)
modsuarede@likelihood #modularidade
printoutModuleInformation(modsuarede) #ver agrupamentos
suarede.random <- nullmodel(suarede,N=100, method="vaznull")
mod.ram.suarede<- sapply(suarede.random, computeModules, method="Beckett")
mod.ram <- sapply(mod.ram.suarede, function(x) x@likelihood)
mod.ram.suarede.mean <- mean(mod.ram) 
mod.ram.suarede.sd <- sd(mod.ram) 
delta.mod.suarede <- modsuarede@likelihood-mod.ram.suarede.mean
zscore.mod<-delta.mod.suarede/mod.ram.suarede.sd
zscore.mod # > 1.93 ? significativo


#H2 especializa??o - ponderado
h2.suarede <-networklevel(rede, index="H2")
h2.suarede #especializa??o
suarede.random <- nullmodel(rede,N=1000, method="vaznull")
h2.ram.suarede <- unlist(sapply(suarede.random, networklevel, index="H2"))
h2.ram.suarede.mean <- mean(h2.ram.suarede) 
h2.ram.suarede.sd <- sd(h2.ram.suarede) 
delta.h2.suarede <- (h2.suarede-h2.ram.suarede.mean)
zscore.h2<-delta.h2.suarede/h2.ram.suarede.sd
zscore.h2 # > 1.93 ? significativo


###########################################
########### pacotao do bipartite ##########
###########################################

#resumo para dados binarios
networklevel(redeb, index="binary")
#topologia da rede - binaria
networklevel(rede, index="topology")

#resumo para dados ponderados
networklevel(rede, index="quantitative")



