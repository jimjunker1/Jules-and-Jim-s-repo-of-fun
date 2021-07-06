
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(reshape2)
library(vegan)
library(ggrepel)
library(labdsv)

setwd("")

######   
ms_full = read.csv(file = "F:/Energy & Fuels Alan Marshall Manuscript/NMDS All Samples/NMDS_MRB.csv", T, stringsAsFactors = F)
samps = list("TOP", "Y01", "Y06", "FS1", "FS2", "FS3", "FS6","SC", "LG", "LL", "LP", "Miss", "ESFA", "GG1", "GG2", "GG3", "GG4", "POXC2", "POXC3", "POXC4","POXC5","POXC6", "POXC7", "AGC15", "BGC15", "AGC16", "BGC16", "D2G", "D2L", "D2P", "LSW", "PLFA", "SRFA", "SRNOM", "PG", "PO", "PP", "MO", "HF")
colnames(MRB) = c("sample", "C", "H","N","O","S","OC","HC","Molform", "chemsupp")
#ms_full = rbind(ms_full, ms4)

ms_full$type = NA
ms_full$hetero = NA
ms_full$character = NA
ms_full$samp = ms_full$sample
ms_full$pres <- 1

FW_fix = which(ms_full$sample == "SRFA" | ms_full$sample == "SRNOM" | ms_full$sample == "AGC15" | 
                 ms_full$sample == "BGC15" | ms_full$sample == "TOP" | ms_full$sample == "Y01" |
                 ms_full$sample == "FS1" | ms_full$sample == "FS2" | ms_full$sample == "FS3" | 
                 ms_full$sample == "FS6" | ms_full$sample == "SC" | ms_full$sample == "LG" |
                 ms_full$sample == "LL" | ms_full$sample == "LP" | ms_full$sample == "Miss" | 
                 ms_full$sample == "GG1" | ms_full$sample == "GG2" | ms_full$sample == "GG3" |
                 ms_full$sample == "GG4" | ms_full$sample == "D2G" | ms_full$sample == "D2L" | 
                 ms_full$sample == "D2P" | ms_full$sample == "PLFA")
ms_full[FW_fix, "type"] = "FW"

T_fix = which(ms_full$sample == "ESFA" | ms_full$sample == "PG" | ms_full$sample == "PO" |
                ms_full$sample == "PP" | ms_full$sample == "POXC2" | ms_full$sample == "POXC3" | 
                ms_full$sample == "POXC4" | ms_full$sample == "POXC5" | 
                ms_full$sample == "POXC6" | ms_full$sample == "POXC7")
ms_full[T_fix, "type"] = "T"

M_fix = which(ms_full$sample == "LSW" | ms_full$sample == "MO" | ms_full$sample == "HF")
ms_full[M_fix, "type"] = "M"


lipid_fix = which(ms_full$OC > 0 & ms_full$OC <= 0.29 & ms_full$HC >= 1.6 & ms_full$HC <= 2.5)
ms_full[lipid_fix, "character"] = "lipid";rm(lipid_fix)
protein_fix = which(ms_full$OC > 0.29 & ms_full$OC <= 0.6 & ms_full$HC >= 1.5 & ms_full$HC <= 2.5)
ms_full[protein_fix, "character"] = "protein";rm(protein_fix)
ascarb_fix = which(ms_full$OC > 0.6 & ms_full$OC <= 1.2 & ms_full$HC >= 1.5 & ms_full$HC <= 2.5)
ms_full[ascarb_fix, "character"] = "ascarb";rm(ascarb_fix)
uhc_fix = which(ms_full$OC > 0 & ms_full$OC <= 0.29 & ms_full$HC >= 1 & ms_full$HC <= 1.6)
ms_full[uhc_fix, "character"] = "uhc";rm(uhc_fix)
ca_fix = which(ms_full$OC > 0 & ms_full$OC <= 0.4 & ms_full$HC >= 0 & ms_full$HC <= 0.7)
ms_full[ca_fix, "character"] = "ca";rm(ca_fix)
lignin_fix = which(ms_full$OC > 0.29 & ms_full$OC <= 0.65 & ms_full$HC >= 07 & ms_full$HC <= 1.5)
ms_full[lignin_fix, "character"] = "lignin";rm(lignin_fix)
tannin_fix = which(ms_full$OC > 0.65 & ms_full$OC <= 1.2 & ms_full$HC >= 05 & ms_full$HC <= 1.5)
ms_full[tannin_fix, "character"] = "tannin";rm(tannin_fix)
unclass_fix = which(is.na(ms_full$character))
ms_full[unclass_fix, "character"] = "unclass";rm(unclass_fix)

CH.fix = which(ms_full$O == 0 & ms_full$N == 0 & ms_full$S == 0)
ms_full[CH.fix, "grp"] = "CH"
rm(CH.fix)
CHO.fix = which(ms_full$N == 0 & ms_full$S == 0)
ms_full[CHO.fix, "grp"] = "CHO"
rm(CHO.fix)
CHN.fix = which(ms_full$O == 0 & ms_full$N == 1 & ms_full$S == 0)
ms_full[CHN.fix, "grp"] = "CHN"
rm(CHN.fix)
CHNO.fix = which(ms_full$O == 1 & ms_full$N == 1 & ms_full$S == 0)
ms_full[CHNO.fix, "grp"] = "CHNO"
rm(CHNO.fix)
CHO.fix = which(ms_full$N == 0 & ms_full$S == 0)
ms_full[CHO.fix, "hetero"] = "CHO"
rm(CHO.fix)
CHON.fix = which(ms_full$N == 1 & ms_full$S == 0)
ms_full[CHON.fix, "hetero"] = "CHON1"
rm(CHON.fix)
CHONS.fix =which(ms_full$N == 1 & ms_full$S == 1)
ms_full[CHONS.fix, "hetero"] = "CHON1S1"
rm(CHONS.fix)
CHOS.fix = which(ms_full$N == 0 & ms_full$S == 1)
ms_full[CHOS.fix, "hetero"] = "CHOS1"
rm(CHOS.fix)
CHON2.fix = which(ms_full$N == 2 & ms_full$S == 0)
ms_full[CHON2.fix, "hetero"] = "CHON2"
rm(CHON2.fix)
CHON2S.fix = which(ms_full$N == 2 & ms_full$S == 1)
ms_full[CHON2S.fix, "hetero"] = "CHON2S1"
rm(CHON2S.fix)
CHON3.fix = which(ms_full$N == 3 & ms_full$S == 0)
ms_full[CHON3.fix, "hetero"] = "CHON3"
rm(CHON3.fix)

ms_full$Dup = duplicated(ms_full)
length(which(ms_full$Dup == T))
ms_full = subset(ms_full, ms_full$Dup == F)
which(ms_full$chem.spp == "C24H33N1O8S0")
######
NMDSvectors <- plyr::ddply(ms_full, c("sample"), summarize, OCmean = mean(OC), HCmean = mean(HC), lignin = length(which(character == "lignin")), 
                     uhc = length(which(character == "uhc")), protein = length(which(character == "protein")), ca = length(which(character == "ca")),
                     tannin = length(which(character == "tannin")), ascarb = length(which(character == "ascarb")), lipid = length(which(character == "lipid")), 
                     unclass = length(which(character == "unclass")), CHO = length(which(hetero == "CHO")), CHON1 = length(which(hetero == "CHON1")), CHON1S1 = length(which(hetero == "CHON1S1")), 
                     CHON2 = length(which(hetero == "CHON2")), CHON2S1 = length(which(hetero == "CHON2S1")), CHOS1 = length(which(hetero == "CHOS1")), CH = length(which(hetero == "CH")), 
                     CHN = length(which(hetero == "CHN")), CHON3 = length(which(hetero == "CHON3"))) 
NMDSvectors$type = NA
FW_fix = which(ms_full$sample == "SRFA" | ms_full$sample == "SRNOM" | ms_full$sample == "AGC15" | 
                 ms_full$sample == "BGC15" | ms_full$sample == "TOP" | ms_full$sample == "Y01" |
                 ms_full$sample == "FS1" | ms_full$sample == "FS2" | ms_full$sample == "FS3" | 
                 ms_full$sample == "FS6" | ms_full$sample == "SC" | ms_full$sample == "LG" |
                 ms_full$sample == "LL" | ms_full$sample == "LP" | ms_full$sample == "Miss" | 
                 ms_full$sample == "GG1" | ms_full$sample == "GG2" | ms_full$sample == "GG3" |
                 ms_full$sample == "GG4" | ms_full$sample == "D2G" | ms_full$sample == "D2L" | 
                 ms_full$sample == "D2P" | ms_full$sample == "PLFA")
ms_full[FW_fix, "type"] = "FW"

T_fix = which(ms_full$sample == "ESFA" | ms_full$sample == "PG" | ms_full$sample == "PO" |
                ms_full$sample == "PP" | ms_full$sample == "POXC2" | ms_full$sample == "POXC3" | 
                ms_full$sample == "POXC4" | ms_full$sample == "POXC5" | 
                ms_full$sample == "POXC6" | ms_full$sample == "POXC7")
ms_full[T_fix, "type"] = "T"

M_fix = which(ms_full$sample == "LSW" | ms_full$sample == "MO" | ms_full$sample == "HF")
ms_full[M_fix, "type"] = "M"

hist(NMDSvectors$lignin);shapiro.test(NMDSvectors$lignin);qqnorm(NMDSvectors$lignin)
hist(NMDSvectors$uhc);shapiro.test(NMDSvectors$uhc);qqnorm(NMDSvectors$uhc)
hist(NMDSvectors$protein);shapiro.test(NMDSvectors$protein)
hist(NMDSvectors$ca);shapiro.test(NMDSvectors$ca)
hist(NMDSvectors$tannin);shapiro.test(NMDSvectors$tannin)

#######run to format the data frame for Primer
ms.spp.long <- reshape2::dcast(ms_full, chemsupp ~ samp, value.var = "pres")

ms.spp.long[is.na(ms.spp.long)] = 0
ms.spp.long2 = ms.spp.long[,-1]
rownames(ms.spp.long2 )= ms.spp.long[,1]

set.seed(101)
ms.nmds <- vegan::metaMDS(ms.spp.long2,  trymax = 5000, autotransform = T, k = 2)
ms.nmds
plot(ms.nmds)

NMDS.scrs <- as.data.frame(scores(ms.nmds, display = "sites"))

NMDS.scrs <- cbind(NMDS.scrs, names(ms.spp.long2))
NMDS.scrs
NMDS.scrs$type = c("FW", "M", "FW", "FW", "FW", "P",
                   "M", "M", "M", "M", "M", "M",
                   "P", "P","P", "P","M","FW", "FW")
colnames(NMDS.scrs) <- c("NMDS1", "NMDS2", "Sample", "Type")

NMDS.spp <- as.data.frame(scores(ms.nmds, display = "species"))
NMDS.spp

set.seed(301)
vec.env <- envfit(ms.nmds, NMDSvectors, permutations = 5000, na.rm = T)
vec.env
env.scrs <- as.data.frame(scores(vec.env, display = "vectors"))
env.scrs <- cbind(env.scrs, Species = rownames(env.scrs))
env.scrs
stream.scrs = as.data.frame(scores(vec.env, display = "factors"))
stream.scrs = cbind(stream.scrs, sample = rownames(stream.scrs))
stream.scrs
env.vec = stream.scrs[c(20:22),]
env.vec$sample = c("FW","M", "P")
cha.vec = env.scrs[c(2,3,7),]
cha.vec$Species = c("H saturation", "Lignin", "Tannin")

Hsurf = ordisurf(ms.nmds ~ NMDSvectors$HCmean, plot = F)
head(Hsurf)

extract.xyz = function(obj){
  xy = expand.grid(x = obj$grid$x, y = obj$grid$y)
  xyz = cbind(xy, c(obj$grid$z))
  names(xyz) = c("x", "y", "z")
  return(xyz)
}
contour.vals = extract.xyz(obj = Hsurf)
head(contour.vals)

Hsat = ggplot(data = contour.vals, aes(x, y, z=z)) +stat_contour(aes(colour = ..level..));Hsat
#####
theme_set(theme_bw(20))
treat <- NMDSvectors$type

find_hull <- function(df) df[chull(df$NMDS1, df$NMDS2),]
hulls <- ddply(NMDS.scrs, "Type", find_hull)
cols = c("dark green", "orange", "purple")
vec.plot <- ggplot(NMDS.scrs) +
  geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, fill = Type, color = Type), alpha = 0.3) +	
  geom_point(data = hulls,aes(x = NMDS1, y = NMDS2, fill = Type), shape = 21, size = 5, colour = "black") +
  coord_fixed() +
  #geom_segment(data = env.vec, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), size = 1, 
  #             arrow = arrow(length(unit(0.5, "cm")))) +
  geom_segment(data = cha.vec, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), size = 1, lty = "dashed",
               arrow = arrow(length(unit(0.5, "cm")))) +
  #geom_label_repel(data = env.vec, aes(x = NMDS1, y = NMDS2, label = sample), size = 5, box.padding= 0.35, segment.color = "grey", segment.size = 1.2, force = 1.2) +
  geom_label_repel(data = cha.vec, aes(x = NMDS1, y = NMDS2, label = Species), size = 5, fontface = "italic", box.padding= 0.35, segment.color = "grey", segment.size = 1.2, force = 1.2) +
  scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
  annotate("text", x = -0.97, y = -1.55, size = 5, parse = T, label=  "stress == 0.09") +
  theme(legend.position = c(0.08,0.2), legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank());vec.plot 


png("ENV_FTICR.png", res = 300, height = 10, width = 10, units = "in")
vec.plot
dev.off()
hulls$z = NA
colnames(hulls) = c("x", "y", "Sample", "Type", "z")
Hsat = ggplot(data = contour.vals, aes(x, y, z=z)) + stat_contour(aes(colour = ..level..)) + 
  geom_polygon(data = hulls, aes(x = x, y = y, fill = Type), alpha = 0.3) +
  geom_point(data = hulls, aes(x = x, y = y, fill = Type), shape = 21, size =5, color = "black") +
  theme(legend.position = c(0.08,0.2), legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank());Hsat


vec.plot = Hsat + geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, fill = Type, color = Type), alpha = 0.3) +	
  geom_point(data = hulls,aes(x = NMDS1, y = NMDS2, fill = Type), shape = 21, size = 5, colour = "black") +
  coord_fixed() +
  #geom_segment(data = env.vec, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), size = 1, 
  #             arrow = arrow(length(unit(0.5, "cm")))) +
  geom_segment(data = cha.vec, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), size = 1, lty = "dashed",
               arrow = arrow(length(unit(0.5, "cm")))) +
  #geom_label_repel(data = env.vec, aes(x = NMDS1, y = NMDS2, label = sample), size = 5, box.padding= 0.35, segment.color = "grey", segment.size = 1.2, force = 1.2) +
  geom_label_repel(data = cha.vec, aes(x = NMDS1, y = NMDS2, label = Species), size = 5, fontface = "italic", box.padding= 0.35, segment.color = "grey", segment.size = 1.2, force = 1.2) +
  scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
  annotate("text", x = -0.97, y = -1.55, size = 5, parse = T, label=  "stress == 0.09") +
  theme(legend.position = c(0.08,0.2), legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank());vec.plot 


FT_dis.bray = dsvdis(t(ms.spp.long2[,c(1:13)]), index = "bray")
isoMDS(FT_dis.bray)

ft.nmds = nmds(FT_dis.bray)
plot(ft.nmds)