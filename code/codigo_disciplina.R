##################################
### DISCIPLINA DE ECOLOGIA TROFICA
#################################
### KAROLINE CERON
###############################


install.packages("dietr")
library(dietr)


#carregando os dados
# primeira coluna é o predador analisado
# segunda coluna a categoria de presa
# as demais os valores de frequencia, numero e volume em porcentagem

data <- read.table("boana.txt", header=T) 
data


#Calcula o IRI para cada presa da espécie
# temos que indicar qual coluna contém cada dado

iri <- CompositeIndices(DietData = data, Indices = c("IRI"), PercentNumber = 4,
                 PercentOccurrence = 3, PercentVolWeight = 5, ReturnRaw = TRUE, PercentOnly = FALSE)
iri

source("psiri_function.R")

psi <- psiri(iri)
names(psi) <- iri$Boana_caiapo$Prey
psi


## Calular vários indices de eletividade

disp <- read.table("disp.txt", header=T)
pred <- read.table("pred.txt", header=T)

#computa diversos indices de eletividade
my.indices <- Electivity(Diet = pred, Available = disp, Indices =
                           c("ForageRatio","Ivlev","Strauss","JacobsQ","JacobsD","Chesson","VanderploegScavia"),LogQ = TRUE,
                         CalcAbundance = TRUE, Depleting = FALSE)

my.indices$Ivlev #indice de Ivlev

my.indices$VanderploegScavia #indice de Vanderploeg & Scavia


# plotando indice de eletividade

pdf("graph.pdf", width = 10, height = 10)

PlotElectivity(Electivity.Calcs = my.indices, Indices = "Ivlev",
               BarColor = c("Red","Purple"))

dev.off()




