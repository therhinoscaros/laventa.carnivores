install.packages("boral","plyr","dplyr","PBSmapping","pracma","ggplot2")
library(boral)
library(plyr)
library(dplyr)
library(PBSmapping) 
library(pracma) 
library(ggplot2)


la.venta.gllvm <- read.csv("~/La Venta GLLVM codings.csv")
#Select guilds to include (in this case, >1kg)
y <- as.matrix(cbind(la.venta.gllvm[51:52],la.venta.gllvm[54:59],la.venta.gllvm[61:62],la.venta.gllvm[64:67]))

#Set model parameters
guild_priors <- list(type = c("normal", "normal", "normal", "halfcauchy"), hyparrams = c(10, 10, 10, 5))
boral_mcmccontrol <- list(n.burnin = 8000, n.interation = 50000, n.thin = 10, seed = 177)
#Run GLLVM code
lvm.la.venta <- boral(y = y, family = "negative.binomial", lv.control = list(num.lv = 2), prior.control = guild_priors, row.eff = "random", mcmc.control = boral_mcmccontrol, save.model = FALSE)
#Diagnostic plots
plot(lvm.la.venta)

#Ordination to scale LV scores
lvs.la.venta <- lvsplot(lvm.la.venta, alpha = 0.55, main = "Unconstrained biplot", return.vals = TRUE)
lvs.la.venta.sites <- as.data.frame(lvs.la.venta$scaled.lvs)
lvs.la.venta.coefs <- as.data.frame(lvs.la.venta$scaled.lv.coefs)
la.venta.combined <- cbind(la.venta.gllvm,lvs.la.venta.sites)

#Write csv including guild codings, environment and LV scores
write.csv(la.venta.combined,"~/La Venta LV Scores.csv")

#Write csv including GLLVM coefficients
guild_animals_Pure <- colnames(y)
df_guild_animals_Pure_la.venta<- cbind(lvs.la.venta.coefs, guild_animals_Pure)
write.csv(df_guild_animals_Pure_la.venta,"~/Documents/La Venta/Carnivore paper/GLLVMs/La Venta LV Coefficients (Large animals).csv")

#Function to find distance from a point
LV.dist <- function(X1, X2, Y1, Y2){
  distance <- sqrt(abs(X1-X2)^2+abs(Y1-Y2)^2)
  return(distance)
}

#Find distance from the point defining each scenario in LV space
la.venta.combined$Mammal.dist <- LV.dist(la.venta.combined$V1,-0.183832565, la.venta.combined$V2, 0.214404633)
la.venta.combined$Mammal.Langstonia.dist <- LV.dist(la.venta.combined$V1,-0.180389235, la.venta.combined$V2, 0.203996107)
la.venta.combined$Mammal.Langstoniacorr.dist <- LV.dist(la.venta.combined$V1,-0.187498267, la.venta.combined$V2, 0.214080489)
la.venta.combined$Mammal.Pseudosuchia.dist <- LV.dist(la.venta.combined$V1,-0.215643948, la.venta.combined$V2, 0.199546871)
la.venta.combined$Mammal.Pseudosuchiacorr.dist <- LV.dist(la.venta.combined$V1,-0.212643365, la.venta.combined$V2, 0.204464245)
la.venta.combined$Mammal.Birds.dist <- LV.dist(la.venta.combined$V1,-0.198861129, la.venta.combined$V2, 0.204233488)
la.venta.combined$Mammal.Terrestrial.reptiles.dist <- LV.dist(la.venta.combined$V1,-0.203034068, la.venta.combined$V2, 0.175333034)
la.venta.combined$Mammal.Terrestrial.reptilescorr.dist <- LV.dist(la.venta.combined$V1,-0.22783072, la.venta.combined$V2, 0.199481486)
la.venta.combined$Mammal.Terrestrial.reptiles.Eunectes.dist <- LV.dist(la.venta.combined$V1,-0.210209576, la.venta.combined$V2, 0.17512097)
la.venta.combined$Mammal.Terrestrial.reptiles.Eunectescorr.dist <- LV.dist(la.venta.combined$V1,-0.229533077, la.venta.combined$V2, 0.199624602)
la.venta.combined$Mammal.GiantCrocs.dist <- LV.dist(la.venta.combined$V1,-0.201762515, la.venta.combined$V2, 0.204056586)
la.venta.combined$Mammal.GiantCrocscorr.dist <- LV.dist(la.venta.combined$V1,-0.199539057, la.venta.combined$V2, 0.197669493)
la.venta.combined$Mammal.Crocs.Terrestrial.reptiles.dist <- LV.dist(la.venta.combined$V1,-0.22876855, la.venta.combined$V2, 0.162171131)
la.venta.combined$Mammal.Crocs.Terrestrial.reptilescorr.dist <- LV.dist(la.venta.combined$V1,-0.245866225, la.venta.combined$V2, 0.189582379)
la.venta.combined$Mammal.GiantCrocs.Terrestrial.reptiles.dist <- LV.dist(la.venta.combined$V1,-0.218529757, la.venta.combined$V2, 0.168101866)
la.venta.combined$Mammal.GiantCrocs.Terrestrial.reptilescorr.dist <- LV.dist(la.venta.combined$V1,-0.241516752, la.venta.combined$V2, 0.195991743)
la.venta.combined$Mammal.Crocs.Turtles.dist <- LV.dist(la.venta.combined$V1,-0.360610071, la.venta.combined$V2, 0.270005577)
la.venta.combined$Mammal.Crocs.Turtlescorr.dist <- LV.dist(la.venta.combined$V1,-0.385235329, la.venta.combined$V2, 0.29112839)
la.venta.combined$Mammal.Crocs.Turtles.terrestrialreptiles.dist <- LV.dist(la.venta.combined$V1,-0.37129942, la.venta.combined$V2, 0.258595693)
la.venta.combined$Mammal.Crocs.Turtles.terrestrialreptilescorr.dist <- LV.dist(la.venta.combined$V1,-0.39448278, la.venta.combined$V2, 0.294223033)
la.venta.combined$Mammal.Crocs.Turtles.terrestrialreptiles.Birds.dist <- LV.dist(la.venta.combined$V1,-0.381726261, la.venta.combined$V2, 0.252721131)
la.venta.combined$Mammal.Crocs.Turtles.terrestrialreptiles.Birdscorr.dist <- LV.dist(la.venta.combined$V1,-0.411573481, la.venta.combined$V2, 0.292165092)
la.venta.combined$Spradley <- LV.dist(la.venta.combined$V1,-0.2098234, la.venta.combined$V2,0.22391378)


#Find total hull area
find_hull_Realm_pure <- function(la.venta.combined) la.venta.combined[chull(la.venta.combined$V1, la.venta.combined$V2),]
hulls_Realm_Pure <- ddply(la.venta.combined, "Realm_Mode", find_hull_Realm_pure)
hulls_Realm_Pure$PID <- as.numeric(as.integer(as.factor(hulls_Realm_Pure$Realm_Mode)))
hulls_Realm_Pure <- hulls_Realm_Pure %>% group_by(Realm_Mode) %>% dplyr::mutate(POS = row_number()) %>% ungroup() 
hulls_Realm_Pure$X <- hulls_Realm_Pure$V1
hulls_Realm_Pure$Y <- hulls_Realm_Pure$V2
hulls_Realm_Pure_Area <- hulls_Realm_Pure %>% group_by(Realm_Mode) %>% dplyr::mutate(Area = polyarea(X, Y)) %>% slice(1) %>% dplyr::select(Realm_Mode, Area) 
hulls_Realm_Pure_Area$Area <- abs(hulls_Realm_Pure_Area$Area) 

#Using the hull area, we calculated the radius as 0.046908647. Then we subset according to this radius and export these files to find the climate variables from
one.percent.mammals <- subset(la.venta.combined, la.venta.combined$Mammal.dist < 0.046908647)
one.percent.mammals.Langstonia <- subset(la.venta.combined, la.venta.combined$Mammal.Langstonia.dist < 0.046908647)
one.percent.mammals.Langstoniacorr <- subset(la.venta.combined, la.venta.combined$Mammal.Langstoniacorr.dist < 0.046908647)
one.percent.mammals.croc <- subset(la.venta.combined, la.venta.combined$Mammal.Pseudosuchia.dist < 0.046908647)
one.percent.mammals.croccorr <- subset(la.venta.combined, la.venta.combined$Mammal.Pseudosuchiacorr.dist < 0.046908647)
one.percent.mammals.birds <- subset(la.venta.combined, la.venta.combined$Mammal.Birds.dist < 0.046908647)
one.percent.mammals.terrestrialreptiles <- subset(la.venta.combined, la.venta.combined$Mammal.Terrestrial.reptiles.dist < 0.046908647)
one.percent.mammals.terrestrialreptilescorr <- subset(la.venta.combined, la.venta.combined$Mammal.Terrestrial.reptilescorr.dist < 0.046908647)
one.percent.mammals.terrestrialreptilesEunectes <- subset(la.venta.combined, la.venta.combined$Mammal.Terrestrial.reptiles.Eunectes.dist < 0.046908647)
one.percent.mammals.terrestrialreptilesEunectescorr <- subset(la.venta.combined, la.venta.combined$Mammal.Terrestrial.reptiles.Eunectescorr.dist < 0.046908647)
one.percent.mammals.giantcroc <- subset(la.venta.combined, la.venta.combined$Mammal.GiantCrocs.dist < 0.046908647)
one.percent.mammals.giantcroccorr <- subset(la.venta.combined, la.venta.combined$Mammal.GiantCrocscorr.dist < 0.046908647)
one.percent.mammals.terrestrialreptilesCrocs <- subset(la.venta.combined, la.venta.combined$Mammal.Crocs.Terrestrial.reptiles.dist < 0.046908647)
one.percent.mammals.terrestrialreptilesCrocscorr <- subset(la.venta.combined, la.venta.combined$Mammal.Crocs.Terrestrial.reptilescorr.dist < 0.046908647)
one.percent.mammals.terrestrialreptilesGiantCrocs <- subset(la.venta.combined, la.venta.combined$Mammal.GiantCrocs.Terrestrial.reptiles.dist < 0.046908647)
one.percent.mammals.terrestrialreptilesGiantCrocscorr <- subset(la.venta.combined, la.venta.combined$Mammal.GiantCrocs.Terrestrial.reptilescorr.dist < 0.046908647)
one.percent.mammals.crocturtle <- subset(la.venta.combined, la.venta.combined$Mammal.Crocs.Turtles.dist < 0.046908647)
one.percent.mammals.crocturtlecorr <- subset(la.venta.combined, la.venta.combined$Mammal.Crocs.Turtlescorr.dist < 0.046908647)
one.percent.mammals.crocturtleterrestrial <- subset(la.venta.combined, la.venta.combined$Mammal.Crocs.Turtles.terrestrialreptiles.dist < 0.046908647)
one.percent.mammals.crocturtleterrestrialcorr <- subset(la.venta.combined, la.venta.combined$Mammal.Crocs.Turtles.terrestrialreptilescorr.dist < 0.046908647)
one.percent.mammals.crocturtleterrestrialbirds <- subset(la.venta.combined, la.venta.combined$Mammal.Crocs.Turtles.terrestrialreptiles.Birds.dist < 0.046908647)
one.percent.mammals.crocturtleterrestrialbirdscorr <- subset(la.venta.combined, la.venta.combined$Mammal.Crocs.Turtles.terrestrialreptiles.Birdscorr.dist < 0.046908647)

write.csv(one.percent.mammals,"One Percent La Venta Mammals.csv")
write.csv(one.percent.mammals.Langstonia,"One Percent La Venta Mammals + Langstonia.csv")
write.csv(one.percent.mammals.Langstoniacorr,"One Percent La Venta Mammals + Langstonia (corrected).csv")
write.csv(one.percent.mammals.croc,"One Percent La Venta Mammals + Crocodiles.csv")
write.csv(one.percent.mammals.croccorr,"One Percent La Venta Mammals + Crocodiles (Corrected).csv")
write.csv(one.percent.mammals.croccorr,"One Percent La Venta Mammals + Crocodiles (Corrected).csv")
write.csv(one.percent.mammals.birds,"One Percent La Venta Mammals + Birds.csv")
write.csv(one.percent.mammals.terrestrialreptiles,"One Percent La Venta Mammals + Terrestrial reptiles.csv")
write.csv(one.percent.mammals.terrestrialreptilescorr,"One Percent La Venta Mammals + Terrestrial reptiles (corrected).csv")
write.csv(one.percent.mammals.terrestrialreptilesEunectes,"One Percent La Venta Mammals + Terrestrial reptiles (inc. Eunectes).csv")
write.csv(one.percent.mammals.terrestrialreptilesEunectescorr,"One Percent La Venta Mammals + Terrestrial reptiles (inc. Eunectes)(corrected).csv")
write.csv(one.percent.mammals.giantcroc,"One Percent La Venta Mammals + Giant Crocodiles.csv")
write.csv(one.percent.mammals.giantcroccorr,"One Percent La Venta Mammals + Giant Crocodiles (corrected).csv")
write.csv(one.percent.mammals.terrestrialreptilesCrocs,"One Percent La Venta Mammals + terrestrial reptiles + crocs.csv")
write.csv(one.percent.mammals.terrestrialreptilesCrocscorr,"One Percent La Venta Mammals + terrestrial reptiles + crocs (corrected).csv")
write.csv(one.percent.mammals.terrestrialreptilesGiantCrocs,"One Percent La Venta Mammals + terrestrial reptiles + giant crocs.csv")
write.csv(one.percent.mammals.terrestrialreptilesGiantCrocscorr,"One Percent La Venta Mammals + terrestrial reptiles + giant crocs (corrected).csv")
write.csv(one.percent.mammals.crocturtle,"One Percent La Venta Mammals + Crocodiles + Turtles.csv")
write.csv(one.percent.mammals.crocturtlecorr,"One Percent La Venta Mammals + Crocodiles + Turtles (corrected).csv")
write.csv(one.percent.mammals.crocturtleterrestrial,"One Percent La Venta Mammals + Crocodiles + Turtles + Terrestrial reptiles.csv")
write.csv(one.percent.mammals.crocturtleterrestrialcorr,"One Percent La Venta Mammals + Crocodiles + Turtles + Terrestrial reptiles (corrected).csv")
write.csv(one.percent.mammals.crocturtleterrestrialbirds,"One Percent La Venta Mammals + Crocodiles + Turtles + Terrestrial reptiles + Birds.csv")
write.csv(one.percent.mammals.crocturtleterrestrialbirdscorr,"One Percent La Venta Mammals + Crocodiles + Turtles + Terrestrial reptiles + Birds (corrected).csv")


#Plot ordination with ggplot to show the position of each point in LV space
la.venta.plot <- ggplot(la.venta.combined) + geom_point(aes(x=V1,y=V2,colour=Title),size=5) + theme_bw() + scale_colour_manual(values = c("#696969","#556b2f","#8b4513","#483d8b","#b8860b","black",'#008b8b','#4682b4', '#00008b', '#7f007f', '#b03060', '#ff4500', '#ffff00', '#00ff00', '#8a2be2', '#dc143c', '#00ffff', '#0000ff', '#adff2f', '#ff00ff','#1e90ff','#fa8072','#eee8aa','#add8e6','#ff1493',"#228b22",'#ee82ee','#98fb98','#ffc0cb')) + 
  theme(text=element_text(size=5) ,legend.position = "bottom") + labs(x="Latent Variable 1", y="Latent Variable 2")
la.venta.plot

ggsave(filename = "La venta GLLVM Ordination.png", plot = la.venta.plot, width = 20, height = 20, units = 'cm', scale = 2, dpi = 800)

