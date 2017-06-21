setwd("~/Desktop")
lista<- read.csv("Listafinal3.csv")
str(lista)
colnames(lista)
require(vegan)
library(vegan)

#PCA

install.packages("ggfortify")
require(ggfortify)
library(ggfortify)
lista$Nombre <- factor(lista$Nombre)
colnames(lista)[4] <- "Especie"

g1 <- prcomp(lista[,7:15], center = T)

autoplot(g1 ,data = lista, colour="Habitat",shape= T, loadings=T, cex.names= 60,xlim=c(-0.4, 0.4), ylim=c(-0.1,0.1),loadings.colour="black", loadings.label=T, loadings.label.size=15,  loadings.label.vjust= 3, loadings.label.colour = "brown4", label.size=40,  frame= T, frame.colour = "Habitat", frame.type= "t", frame.fill= T)  + 
  scale_shape_manual(values= c(1:18, 21:25)) + scale_fill_manual(values = c("white","white")) + geom_point(aes(colour = Habitat, shape= Especie), size=25, fill= "white") + 
  theme(legend.text = element_text(face= "italic", size = 50),axis.text.y = element_text(size = 60), axis.text.x = element_text(size = 60), axis.title.x = element_text(size=60), axis.title.y = element_text(size=60),  legend.title = element_text(size= 80),  legend.background = element_blank()), axis.text.x = element_text(size = 40)

png("PCA7.png", width=50, height = 40, units = "in", res = 300)
print(g1)
summary(g)
dev.off()


#BOXPLOT HABITATS

install.packages("extrafont")
library(extrafont)
font_import()
plot.new()
setwd("~/Desktop")
listaparamo5<-read.csv("listaparamo5.csv")
str(listaparamo5)
#dc<-decostand(listaparamo5[,7:15], "norm", na.rm=T)
#lit1<-data.frame(listaparamo5[,1:6])



setwd("~/Desktop")
lt2<-read.csv("listabosque3.csv")
str(lt2)
#dc2<-decostand(lt2[,7:15], "norm", na.rm=T)
#lt22<-data.frame(lt2[,1:6])



#plot.new()
aab<-par(mfrow=c(1,2))


boxplot(listaparamo5[,7:15], col=rainbow(length(unique(listaparamo5))), outline = F, las="2", cex.lab=20, main = "Paramo", ylim=c(0,300)) 
boxplot(lt2[,7:15], col=rainbow(length(unique(lt2))), outline = F, las="2", cex.lab=20, main = "Bosque Alto andino", ylim=c(0,300))

png("b6.png", width=20, height = 20, units = "in", res = 300)

print(aab)

dev.off()

#Barplot species per habitat

setwd("~/Desktop")
lista<- read.csv("Listafinal3.csv")
td<-table(lista$Nombre, lista$Habitat)
View(td)

#SPECNUMBER
td2<-as.data.frame.matrix(table(lista$Nombre, lista$Habitat))

#SPECIES DIVERSITY

jenj<-par(mfrow=c(1,2))

barplot(td2$`Bosque Alto Andino`, beside=T, names.arg = rownames(td2), las=2, cex.names = 0.8, main = "Bosque Altoandino", col = rainbow(23), cex.label = 1, ylim = c(0,25))
barplot(td2$`Paramo`, beside=T, names.arg = rownames(td2), las=2, cex.names =0.8, main = "Paramo", col = rainbow(23), cex.label = 5, ylim = c(0,25))

png("dd1.png", width=100, height = 150, units = "in", res = 300)

print(jenj)

dev.off()

library(vegan)
div<-specnumber(t(td2))


#INDICES

Shannon<-diversity(td2, index = "shannon", MARGIN = 2)
Simpson<-diversity(td2, index = "simpson", MARGIN = 2)


barras<-par(mfrow=c(1,2))


barplot(Shannon, axisnames = T , horiz = F, col= c("indianred3", "deepskyblue4"),main = "Indice Shannon", ylim = c(0,5), las=1, space = 0.5,  axes = T, cex.names= 0.8)
barplot(Simpson, axisnames = T , horiz = F, col= c("indianred3", "deepskyblue4"), main = "Indice Simpson", ylim = c(0,5), las=1, space = 0.5, axes = T,  cex.names = 0.8)

png("Indices.png", width=20, height = 30, units = "in", res = 300) #para guardar imagen

print(barras)

dev.off()

#INDICE SIMPSON
Simpson<-diversity(td2, index = "simpson", MARGIN = 2)

barplot(Simpson, axisnames = T , horiz = F, col= c("indianred3", "deepskyblue4"), main = "Indice Simpson", ylim = c(0,5), las=1, space = 0.5, axes = T,  cex.names = 2)

dev.off() #para eliminar la funcion de par

#Barplot Food Guild

list2<-read.csv("listafinal3.csv")
grem3<-as.data.frame.matrix(table(list2$Gremio, list2$Habitat))
gr<-par(mfrow=c(1,2))

barplot(grem3$`Bosque Alto Andino`, names.arg = rownames(grem3), las=2, cex.names = 5, main = "Bosque Altoandino", col=rainbow(7), ylim = c(0,45))

barplot(grem3$Paramo, names.arg = rownames(grem3), las=2, cex.names =5, main = "Paramo",col=rainbow(7), ylim= c(0,45))

png("gremios1.png", width=20, height = 30, units = "in", res = 300)

print(gr)


#Barplot Families
setwd("~/Desktop")
flias<-read.csv("listafinal3.csv")
flias1<-as.data.frame.matrix(table(flias$Familia, flias$Habitat))
ff<-par(mfrow=c(1,2))
#plot.new()
barplot(flias1$`Bosque Alto Andino`, names.arg = rownames(flias1),las=2, cex.names = 0.8, main = "Bosque Altoandino", col=rainbow(11), ylim = c(0,30))

barplot(flias1$Paramo, names.arg = rownames(flias1),las=2, cex.names = 0.8, main = "Paramo", col=rainbow(11), ylim = c(0,30))

png("familias.png", width=20, height = 30, units = "in", res = 300)
print(ff)
dev.off()


