###############################################################################
#       VARIOGRAMS AND WOODY PLANT COUNTS FOR ARTEMIS ANTBED NATURE REFUGE    #
#                Author: Gabriel Crowley 16 March 2023                        #
#     R code for Crowley and Murphy. Are increases in projective foliage      #
# cover and woody stem density related, and do they have the same underlying  #
#          drivers? Insights from an Australian tropical savanna              #
###############################################################################

###############################################################################
#                 LOAD PACKAGES and SET UP DIRECTORY                          #
###############################################################################
# setwd("~/R")
# download the files "Veg.RData" and "WP.RData" from 
# https://github.com/GM-Crowley/CYP to your working directory 

# create a folder called "output" within your working directory to store figures
# and tables

# clear list and increase memory
rm(list = ls())
options(java.parameters =  c('-Xss2560k', '-Xmx1g') )

suppressPackageStartupMessages({
  library(raster)
  library(data.table)
  library(sp)
  library(ggplot2)
  library(rstatix)
  library(dplyr)
  library(tidyr)
  options("rgdal_show_exportToProj4_warnings"="none")
  library(rgdal)
})
# other required packages are  "gstat", PairedData" and "DescTools"

###############################################################################
#    Import and set up rasters
###############################################################################

# load vegetation maps of ARTEMIS ANTBED NATURE REFUGE (AANR)
load("Veg.RData")

# import rasters from TERN repository 
# (see https://portal.tern.org.au/metadata/23885)  
# and clip to extent of AANR
ext <- as(extent(1251195, 1256205, -1639205, -1632875), 'SpatialPolygons')
R2001 <- raster::raster("/vsicurl/https://dap.tern.org.au/thredds/fileServer/landscapes/remote_sensing/landsat/seasonal_fractional_cover_v3/persistent_green/qld/lztmre_qld_m200103200105_dp7a2.tif")
R2001 <- crop(R2001, ext)
R2018 <- raster::raster("/vsicurl/https://dap.tern.org.au/thredds/fileServer/landscapes/remote_sensing/landsat/seasonal_fractional_cover_v3//persistent_green/qld/lztmre_qld_m201803201805_dp7a2.tif")
R2018 <- crop(R2018, ext)
R2020 <- raster::raster("/vsicurl/https://dap.tern.org.au/thredds/fileServer/landscapes/remote_sensing/landsat/seasonal_fractional_cover_v3//persistent_green/qld/lztmre_qld_m202003202005_dp7a2.tif")
R2020 <- crop(R2020, ext)

# create Autumn Persistent Green rasters for each vegetation type and year 
GR.2001 <- (GR*R2001)
GR.2018 <- (GR*R2018)
GR.2020 <- (GR*R2020)
TT.2001 <- (TT*R2001)
TT.2018 <- (TT*R2018)
TT.2020 <- (TT*R2020)
EW.2001 <- (EW*R2001)
EW.2018 <- (EW*R2018)
EW.2020 <- (EW*R2020)

# plot Autumn Persistent Green maps for each vegetation type and year
raster::plot(GR.2001, main = "2001", legend = F)
raster::plot(TT.2001, legend = F, add = T)
raster::plot(EW.2001, legend = F, add = T)
raster::plot(GR.2018, main = "2018", legend = F)
raster::plot(TT.2018, legend = F, add = T)
raster::plot(EW.2001, legend = F, add = T)
raster::plot(GR.2018, main = "2020")
raster::plot(TT.2020, legend = F, add = T)
raster::plot(EW.2020, legend = F, add = T)

dev.off()

# convert rasters to data tables
Grassland2001dt <- data.table::as.data.table(raster::rasterToPoints(GR.2001))
Grassland2018dt <- data.table::as.data.table(raster::rasterToPoints(GR.2018))
Grassland2020dt <- data.table::as.data.table(raster::rasterToPoints(GR.2020))
Teatree2001dt <- data.table::as.data.table(raster::rasterToPoints(TT.2001))
Teatree2018dt <- data.table::as.data.table(raster::rasterToPoints(TT.2018))
Teatree2020dt <- data.table::as.data.table(raster::rasterToPoints(TT.2020))
EWoodland2001dt <- data.table::as.data.table(raster::rasterToPoints(EW.2001))
EWoodland2018dt <- data.table::as.data.table(raster::rasterToPoints(EW.2018))
EWoodland2020dt <- data.table::as.data.table(raster::rasterToPoints(EW.2020))

# clean up
rm(R2001, R2020, GR, TT, EW, ext)
rm(GR.2001, GR.2020, TT.2001, TT.2020, EW.2001, EW.2020)
invisible(gc())

# combine data tables into a long-format file
colnames(Grassland2001dt)[3] <- "PG"
colnames(Grassland2020dt)[3] <- "PG"
colnames(Grassland2018dt)[3] <- "PG"
colnames(Teatree2001dt)[3] <- "PG"
colnames(Teatree2018dt)[3] <- "PG"
colnames(Teatree2020dt)[3] <- "PG"
colnames(EWoodland2001dt)[3] <- "PG"
colnames(EWoodland2018dt)[3] <- "PG"
colnames(EWoodland2020dt)[3] <- "PG"

Grassland2001dt$Year <- 2001
Grassland2001dt$Veg <- "Grassland"
Grassland2018dt$Year <- 2018
Grassland2018dt$Veg <- "Grassland"
Grassland2020dt$Year <- 2020
Grassland2020dt$Veg <- "Grassland"
Teatree2001dt$Year <- 2001
Teatree2001dt$Veg <- "Tea tree woodland"
Teatree2018dt$Year <- 2018
Teatree2018dt$Veg <- "Tea tree woodland"
Teatree2020dt$Year <- 2020
Teatree2020dt$Veg <- "Tea tree woodland"
EWoodland2001dt$Year <- 2001
EWoodland2001dt$Veg <- "Eucalypt woodland"
EWoodland2018dt$Year <- 2018
EWoodland2018dt$Veg <- "Eucalypt woodland"
EWoodland2020dt$Year <- 2020
EWoodland2020dt$Veg <- "Eucalypt woodland"

AANR <- rbind(Grassland2001dt, Grassland2018dt, Grassland2020dt, Teatree2001dt, Teatree2018dt, Teatree2020dt, EWoodland2001dt, EWoodland2018dt, EWoodland2020dt)
AANR <- na.omit(AANR)
AANR$int <- interaction(AANR$Year, AANR$Veg, sep = " ")

AANR <- within(AANR, {
  Year <- factor(Year, levels=c("2001", "2018", "2020"))
  Veg <- factor(Veg, levels=c("Grassland", "Tea tree woodland",  "Eucalypt woodland"))
  int <- factor(int, levels = c("2020 Grassland", "2018 Grassland", "2001 Grassland", "2020 Tea tree woodland", "2018 Tea tree woodland", "2001 Tea tree woodland", "2020 Eucalypt woodland", "2018 Eucalypt woodland", "2001 Eucalypt woodland"))
})

# clean up
rm(Grassland2001dt, Grassland2018dt, Grassland2020dt, Teatree2001dt, Teatree2018dt, Teatree2020dt, EWoodland2001dt, EWoodland2018dt, EWoodland2020dt)
invisible(gc())

# remove aberrant values
AANR$id <- paste(AANR$x,'.',AANR$y)
Dels <- AANR[AANR$PG > 100, "id"]
AANR <- AANR[ ! AANR$id %in% Dels$id, ]
rm(Dels)
AANR <- subset(AANR, select = -c(id)) 

# regenerate veg-year files without aberrant values
Grassland2001dt <- subset(AANR, int == "2001 Grassland", select = x:Year)
Grassland2018dt <- subset(AANR, int == "2018 Grassland", select = x:Year)
Grassland2020dt <- subset(AANR, int == "2020 Grassland", select = x:Year)
Teatree2001dt <- subset(AANR, int == "2001 Tea tree woodland", select = x:Year)
Teatree2018dt <- subset(AANR, int == "2018 Tea tree woodland", select = x:Year)
Teatree2020dt <- subset(AANR, int == "2020 Tea tree woodland", select = x:Year)
EWoodland2001dt <- subset(AANR, int == "2001 Eucalypt woodland", select = x:Year)
EWoodland2018dt <- subset(AANR, int == "2018 Eucalypt woodland", select = x:Year)
EWoodland2020dt <- subset(AANR, int == "2020 Eucalypt woodland", select = x:Year)

# compare PG values for 2001 and 2018

GR <- merge(Grassland2001dt, Grassland2018dt, by = c("x", "y"))
GR$PG.change <- GR$PG.y - GR$PG.x 
  GR <- subset(GR, select = c("PG.change"))
GR$Veg <- "GR"

TT <- merge(Teatree2001dt, Teatree2018dt, by = c("x", "y"))
TT$PG.change <- TT$PG.y - TT$PG.x 
TT <- subset(TT, select = c("PG.change"))
TT$Veg <- "TT"

EW <- merge(EWoodland2001dt, EWoodland2018dt, by = c("x", "y"))
EW$PG.change <- EW$PG.y - EW$PG.x 
EW <- subset(EW, select = c("PG.change"))
EW$Veg <- "EW"

Change <- rbind(GR, TT, EW)

AANR.PG.change.2001.2018 <- Change %>%
  group_by(Veg) %>%
  summarise(
    mean = mean(PG.change),
    sd = sd(PG.change),
    n = n(),
    sem = sd / sqrt(n)
  )

write.csv(AANR.PG.change.2001.2018,
          file = "./output/AANR_PG_change_2001_2018.csv",
          row.names = FALSE)

# remove 2018 values from AANR
AANR<-AANR[!(AANR$Year==2018),]
AANR <- within(AANR, {
  Year <- factor(Year, levels=c("2001", "2020"))
  int <- factor(int, levels = c("2020 Grassland", "2001 Grassland", "2020 Tea tree woodland", "2001 Tea tree woodland", "2020 Eucalypt woodland", "2001 Eucalypt woodland"))
})

################################################################################
# Analyse PG changes between 2001 and 2020  
################################################################################

################################################################################
# Generate histogram for PG for each vegetation type and year 
################################################################################

# check data characteristics
# histogram  
ggplot(AANR, aes(x = PG)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(int ~ .)+
  theme_bw()+
  theme(legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)) +
  xlab("") +
  ylab("Frequency")+theme(strip.text.y = element_text(angle = 0))

# Grassland, Tea tree woodland and Eucalypt woodland have very different distributions, 
#    so probably need to be analyzed separately
# This conclusion is confirmed by the following comparisons

# outlier test
PG.outliers <- AANR %>%
  group_by(Year, Veg) %>%
  rstatix::identify_outliers(PG)
table(PG.outliers$is.outlier)
table(PG.outliers$is.extreme)
# identified 740 outliers, including 17 extreme outliers

# Shapiro test on Random sample of AANR
set.seed(25)
AANR.Random <- AANR %>% dplyr::slice_sample(n = 5000)

AANR.Random %>%
  group_by(Year, Veg) %>%
  rstatix::shapiro_test(PG)
# p < 0.05 identified non-normality of PG

# Comparison of standard deviations  
AANR.sum <- group_by(AANR, Veg, Year) %>%
  dplyr::summarise(
    Feature = "PG",
    count = n(),
    mean = mean(PG),
    sem = sd(PG)/sqrt(n()),
    median = median(PG)
  )
AANR.sum

# PG in grassland and tea tree woodland has higher SEMs but lower means 
# than in eucalypt woodland has, so heteroscedasticity unlikely to be resolved 
# through log transformation

# Therefore run separate analyses for each vegetation type

# clean up
rm(AANR.Random, PG.outliers)
invisible(gc())

###############################################################################
## generate stats for Grassland
###############################################################################

Grassland <- rbind(Grassland2001dt, Grassland2020dt)

# outlier test
Grassland.PG.outliers <- Grassland %>%
  group_by(Year) %>%
  rstatix::identify_outliers(PG)
table(Grassland.PG.outliers$is.outlier)
table(Grassland.PG.outliers$is.extreme)

# identified 87 outliers, including 4 extreme outliers

# Shapiro test
Grassland %>%
  group_by(Year) %>%
  rstatix::shapiro_test(PG)
# p < 0.05 identified non-normality of PG
# therefore compare grassland PG across years using a either a Wilcoxon t-test or a Two-sample sign test

# convert grassland file to wide format
Grassland <- spread(Grassland, Year, PG)
colnames(Grassland)[3] <- "yr2001"
colnames(Grassland)[4] <- "yr2020"
Grassland <- na.omit(Grassland)

# Test whether paired samples are distributed symmetrically around the median
# using a random sample so that pairs can be seen

set.seed(701)
Grassland.Random <- Grassland %>% dplyr::slice_sample(n = 100)
before <- Grassland.Random$yr2001
after <- Grassland.Random$yr2020
Grassland.paired <- PairedData::paired(before, after)
PairedData::plot(Grassland.paired, type = "profile") + theme_bw() + ggtitle("Grassland")

# As distribution is asymmetrical, perform Two-sample sign test

Sign.Gr <- DescTools::SignTest(x = Grassland$yr2001,y = Grassland$yr2020)
Gr.Sign <- cbind("Grassland", nrow(Grassland), median(Grassland$yr2001), median(Grassland$yr2020), Sign.Gr$statistic, Sign.Gr$parameter, Sign.Gr$p.value)
colnames(Gr.Sign)<- c("Veg", "n", "medPG2001", "medPG2020", "S", "Differences", "p.value")

median(Grassland$yr2001)
median(Grassland$yr2020)
Sign.Gr$statistic
Sign.Gr$parameter
Sign.Gr$p.value

# Increase in median PG in grassland from 14 to 21 is significant
# S = 121, number of differences = 2,073, p = 0

# assess PG change
Grassland$PG.change <- Grassland$yr2020 - Grassland$yr2001

G.change.sum <- Grassland %>%
  summarise(Veg = "Grassland",
            Year = NA,
            Feature = "PG change",
            count = n(),
            mean = mean(PG.change),
            median = median(PG.change),
            sem = sd(PG.change)/sqrt(n())
  )

# clean up
rm(Grassland, Sign.Gr, Grassland.PG.outliers, Grassland.Random, Grassland.paired)
invisible(gc())

################################################################################
## generate stats for Tea tree woodland
################################################################################

Teatree <- rbind(Teatree2001dt, Teatree2020dt)

# outlier test
Teatree.PG.outliers <- Teatree %>%
  group_by(Year) %>%
  rstatix::identify_outliers(PG)
table(Teatree.PG.outliers$is.outlier)
table(Teatree.PG.outliers$is.extreme)

# identified 12 outliers and 0 extreme outliers

# Shapiro test on Tea tree woodland
set.seed(313)
Teatree.Random <- Teatree %>% dplyr::slice_sample(n = 5000)

Teatree.Random %>%
  group_by(Year) %>%
  rstatix::shapiro_test(PG)
# p < 0.05 identified non-normality of PG
# therefore PG of tea tree woodland is compared using a Wilcoxon t-test or a Two-sample sign test

# convert tea tree woodland file to wide format
Teatree <- spread(Teatree, Year, PG)
colnames(Teatree)[3] <- "yr2001"
colnames(Teatree)[4] <- "yr2020"
Teatree <- na.omit(Teatree)

# Test whether paired samples are distributed symmetrically around the median
# using a random sample so that pairs can be seen

set.seed(15)
Teatree.Random <- Teatree %>% dplyr::slice_sample(n = 100)
before <- Teatree.Random$yr2001
after <- Teatree.Random$yr2020
Teatree.paired <- PairedData::paired(before, after)
PairedData::plot(Teatree.paired, type = "profile") + theme_bw() + ggtitle("Tea tree woodland")

# As distribution is symmetrical, perform Wilcoxon test  
T.Wilcoxon.test <- wilcox.test(Teatree$yr2001, Teatree$yr2020, paired = TRUE, alternative = "two.sided")
median(Teatree$yr2001)
median(Teatree$yr2020)
T.Wilcoxon.test$statistic
T.Wilcoxon.test$p.value

# Increase in median PG in tea tree woodland from 26 to 38 is significant
# V = 19,680.5, p = 0

# for comparison with other vegetation types, undertake Two-sample paired sign test
Sign.TT <- DescTools::SignTest(x = Teatree$yr2001,y = Teatree$yr2020)
TT.Sign <- cbind("Tea tree woodland", nrow(Teatree), median(Teatree$yr2001), median(Teatree$yr2020), Sign.TT$statistic, Sign.TT$parameter, Sign.TT$p.value)
colnames(TT.Sign)<- c("Veg", "n", "medPG2001", "medPG2020", "S", "Differences", "p.value")

median(Teatree$yr2001)
median(Teatree$yr2020)
Sign.TT$statistic
Sign.TT$parameter
Sign.TT$p.value
# Increase in median PG in grassland from 26 to 38 is significant
# S = 78, number of differences = 6,136, p = 0

# assess PG change
Teatree$PG.change <- Teatree$yr2020 - Teatree$yr2001

T.change.sum <- Teatree %>%
  summarise(Veg = "Tea tree woodland",
            Year = NA,
            Feature = "PG change",
            count = n(),
            mean = mean(PG.change, na.rm = TRUE),
            median = median(PG.change, na.rm = TRUE),
            sem = sd(PG.change, na.rm = TRUE)/sqrt(n())
  )

# clean up
rm(Teatree, Sign.TT, Teatree.PG.outliers, Teatree.Random, Teatree.paired, T.Wilcoxon.test)
invisible(gc())

################################################################################
## generate stats for Eucalypt Woodland
################################################################################

EWoodland <- rbind(EWoodland2001dt, EWoodland2020dt)

# outlier test
EWoodland.PG.outliers <- EWoodland %>%
  group_by(Year) %>%
  rstatix::identify_outliers(PG)
table(EWoodland.PG.outliers$is.outlier)
table(EWoodland.PG.outliers$is.extreme)

# identified 641 outliers, including 13 extreme outliers

# Shapiro test on Eucalypt woodland
set.seed(802)
EWoodland.Random <- EWoodland %>% dplyr::slice_sample(n = 5000)
EWoodland.Random %>%
  group_by(Year) %>%
  rstatix::shapiro_test(PG)
# p < 0.05 identified non-normality of PG
# therefore PG of eucalypt woodland is compared using a Wilcoxon t-test or a Two-sample sign test

# Create eucalypt woodland file in wide format
EWoodland <- spread(EWoodland, Year, PG)
colnames(EWoodland)[3] <- "yr2001"
colnames(EWoodland)[4] <- "yr2020"
EWoodland <- na.omit(EWoodland)

# Test whether paired samples are distributed symmetrically around the median
# using a random sample so that pairs can be seen

set.seed(1256)
EWoodland.Random <- EWoodland %>% dplyr::slice_sample(n = 100)
before <- EWoodland.Random$yr2001
after <- EWoodland.Random$yr2020
EWoodland.paired <- PairedData::paired(before, after)
PairedData::plot(EWoodland.paired, type = "profile") + theme_bw()  + ggtitle("Eucalypt woodland")

# As distribution is asymmetrical, perform Two-sample sign test  
Sign.EW <- DescTools::SignTest(x = EWoodland$yr2001,y = EWoodland$yr2020)
EW.Sign <- cbind("Eucalypt woodland", nrow(EWoodland), median(EWoodland$yr2001), median(EWoodland$yr2020), Sign.EW$statistic, Sign.EW$parameter, Sign.EW$p.value)
colnames(EW.Sign)<- c("Veg", "n", "medPG2001", "medPG2020", "S", "Differences", "p.value")

median(EWoodland$yr2001)
median(EWoodland$yr2020)
Sign.EW$statistic
Sign.EW$parameter
Sign.EW$p.value

# Increase in median PG in eucalypt woodland from 35 to 42 is significant
# S = 567, number of differences = 12,984, p = 0

# assess PG change
EWoodland$PG.change <- EWoodland$yr2020 - EWoodland$yr2001

E.change.sum <- EWoodland %>%
  summarise(Veg = "Eucalypt woodland",
            Year = NA,
            Feature = "PG change",
            count = n(),
            mean = mean(PG.change, na.rm = TRUE),
            median = median(PG.change, na.rm = TRUE),
            sem = sd(PG.change, na.rm = TRUE)/sqrt(n())
  )

# clean up
rm(EWoodland, Sign.EW, EWoodland.PG.outliers, EWoodland.Random, EWoodland.paired, before, after)
invisible(gc())

# combine stats for the three veg types
PG.change.sum <- rbind(G.change.sum, T.change.sum, E.change.sum)
SignTest <- rbind(Gr.Sign, TT.Sign, EW.Sign)
SignTest
write.csv(SignTest,
          file = "./output/AANR_SignTest.csv",
          row.names = FALSE)
AANR.sum <- rbind(AANR.sum, PG.change.sum)
AANR.sum
write.csv(AANR.sum,
          file = "./output/AANR_summary.csv",
          row.names = FALSE)

# clean up
rm(Gr.Sign, TT.Sign, EW.Sign, SignTest)
rm(G.change.sum, T.change.sum, E.change.sum, PG.change.sum, AANR.sum)
invisible(gc())
dev.off()

################################################################################
# Produce Autumn Persistent Green box plots for each year and habitat
################################################################################

PG.box.plot <- ggplot(AANR, aes(x=int, y=PG, fill = int)) + 
  geom_boxplot(lwd=0.75)+
  coord_flip() +
  theme_bw()+
  scale_x_discrete(limits = rev(levels(AANR$int)), labels = NULL, breaks = NULL)+
  scale_fill_manual(name = "", 
                    values=c("2020 Grassland" = "#0072BC", 
                             "2001 Grassland" = "#00B9F1", 
                             "2020 Tea tree woodland" = "#707065", 
                             "2001 Tea tree woodland" = "#F8F8F8",
                             "2020 Eucalypt woodland" = "#F15A22", 
                             "2001 Eucalypt woodland" = "#F7931D"), 
                    labels = c("2020 Grassland" = "Grassland\n   2020\n",
                               "2001 Grassland" = "\n   2001\n", 
                               "2020 Tea tree woodland" = "Tea tree woodland\n   2020\n",
                               "2001 Tea tree woodland" = "\n   2001\n", 
                               "2020 Eucalypt woodland" = "Eucalypt woodland\n   2020\n", 
                               "2001 Eucalypt woodland" = "\n   2001\n"))+
  labs(title="", x="", y = "Autumn Persistent Green")+
  theme(axis.text = element_text(color = "black", size = 14),  
        axis.title = element_text(color = "black", size = 14),  
        legend.text = element_text(color = "black", size = 14))+
  theme(legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm"))+
  theme(panel.grid = element_blank())+
  theme(legend.key.size = unit(1.5, "lines"))+
  theme(axis.line = element_line(size = 0), axis.ticks = element_line(size = 0.8), panel.border = element_rect(size=1.5))

PG.box.plot
ggsave("./output/PG.box.plot.tiff", device = "tiff", dpi = 499, units = c("cm"), width = 14.9, height = 10.6)

# clean up
rm(PG.box.plot, AANR)
invisible(gc())
dev.off()

################################################################
#       Estimate variograms of best fit for
# Grasslands/Tea tree woodlands and Eucalypt woodlands
################################################################

#  combine grasslands and tea tree woodlands into one file
Grassland2001dt <-rbind(Grassland2001dt, Teatree2001dt)
Grassland2020dt <-rbind(Grassland2020dt, Teatree2020dt)
rm(Teatree2001dt, Teatree2020dt)

#  create spatial objects
sp::coordinates(Grassland2001dt) <- ~x + y
sp::coordinates(EWoodland2001dt) <- ~x + y
sp::coordinates(Grassland2020dt) <- ~x + y
sp::coordinates(EWoodland2020dt) <- ~x + y

# Generate directional variograms to check for anisotropy
set.seed(712)

G1.dir.var <- gstat::variogram(PG~1, as(Grassland2001dt, "SpatialPixelsDataFrame"), cutoff = 1000,
                               alpha = c(0, 45, 90, 135))

G2.dir.var <- gstat::variogram(PG~1, as(Grassland2020dt, "SpatialPixelsDataFrame"), cutoff = 1000,
                               alpha = c(0, 45, 90, 135))

EW1.dir.var <- gstat::variogram(PG~1, as(EWoodland2001dt, "SpatialPixelsDataFrame"), cutoff = 1000,
                                alpha = c(0, 45, 90, 135))

EW2.dir.var <- gstat::variogram(PG~1, as(EWoodland2020dt, "SpatialPixelsDataFrame"), cutoff = 1000,
                                alpha = c(0, 45, 90, 135))

# plot directional variograms
plot(G1.dir.var, main = "Grassland 2001")
plot(G2.dir.var, main = "Grassland 2020")
plot(EW1.dir.var, main = "Eucalypt woodland 2001")
plot(EW2.dir.var, main = "Eucalypt woodland 2020")  
rm(G1.dir.var, G2.dir.var, EW1.dir.var, EW2.dir.var)

#  generate data table for two-dimensional variogram using gstat
G1.var <- gstat::variogram(PG~1, as(Grassland2001dt, "SpatialPixelsDataFrame"), cutoff = 1000)
G2.var <- gstat::variogram(PG~1, as(Grassland2020dt, "SpatialPixelsDataFrame"), cutoff = 1000)
EW1.var <- gstat::variogram(PG~1, as(EWoodland2001dt, "SpatialPixelsDataFrame"), cutoff = 1000)
EW2.var <- gstat::variogram(PG~1, as(EWoodland2020dt, "SpatialPixelsDataFrame"), cutoff = 1000)

G1.var$label <- "Grassland/Tea tree woodland 2001"
G2.var$label <- "Grassland/Tea tree woodland 2020"
EW1.var$label <- "Eucalypt woodland 2001"
EW2.var$label <- "Eucalypt woodland 2020"

var <- rbind(G1.var, G2.var, EW1.var, EW2.var)
var$label <- factor(var$label, levels = c("Grassland/Tea tree woodland 2020", "Grassland/Tea tree woodland 2001", "Eucalypt woodland 2020", "Eucalypt woodland 2001"))

#  as variograms are isotopic, fit exponential variogram using gstat
G1.var.exp <- gstat::fit.variogram(G1.var, gstat::vgm("Exp"))
G2.var.exp <- gstat::fit.variogram(G2.var, gstat::vgm("Exp"))
EW1.var.exp <- gstat::fit.variogram(EW1.var, gstat::vgm("Exp"))
EW2.var.exp <- gstat::fit.variogram(EW2.var, gstat::vgm("Exp"))

G1.var.exp$label <- "Grassland/Tea tree woodland 2001"
G2.var.exp$label <- "Grassland/Tea tree woodland 2020"
EW1.var.exp$label <- "Eucalypt woodland 2001"
EW2.var.exp$label <- "Eucalypt woodland 2020"

var.exp <- rbind(G1.var.exp, G2.var.exp, EW1.var.exp, EW2.var.exp)
var.exp$label <- factor(var.exp$label, levels = c("Grassland/Tea tree woodland 2020", "Grassland/Tea tree woodland 2001", "Eucalypt woodland 2020", "Eucalypt woodland 2001"))

# extract and save  variogram parameters
G1.var.exp$Veg <-"Grassland/Tea Tree woodland"
G2.var.exp$Veg <-"Grassland/Tea Tree woodland"
EW1.var.exp$Veg <-"Eucalypt woodland"
EW2.var.exp$Veg<-"Eucalypt woodland"
G1.var.exp$Year <-"2001"
G2.var.exp$Year <-"2020"
EW1.var.exp$Year <-"2001"
EW2.var.exp$Year<-"2020"
Variogram.parameters <- rbind(G1.var.exp, G2.var.exp, EW1.var.exp, EW2.var.exp)
Variogram.parameters <- Variogram.parameters[, c("Veg", "Year", "model", "psill", "range", "kappa")]
Variogram.parameters <- as.data.frame(Variogram.parameters)
write.csv(Variogram.parameters,
          file = "./output/Variogram_parameters.csv",
          row.names = FALSE)
Variogram.parameters
rm(Variogram.parameters)

# extract variogram curves for Kolmogorov-Smirnov test
G1.df <- as.data.frame(G1.var[,])
colnames(G1.df)[3] <- "G2001"
G2.df <- as.data.frame(G2.var[,])
colnames(G2.df)[3] <- "G2020"
EW1.df <- as.data.frame(EW1.var[,])
colnames(EW1.df)[3] <- "EW2001"
EW2.df <- as.data.frame(EW2.var[,])
colnames(EW2.df)[3] <- "EW2020"

PG.Variograms <- cbind(G1.df$dist, G1.df$G2001, G2.df$G2020, EW1.df$EW2001, EW2.df$EW2020)
colnames(PG.Variograms) <- c("Distance","G2001", "G2020", "EW2001", "EW2020")
PG.Variograms <- as.data.frame(PG.Variograms)

# compare 2001 and 2020 curves using Kolmogorov-Smirnov test
Kolmogorov.Smirnov_GRTT <- ks.test(PG.Variograms$G2001, PG.Variograms$G2020)
GRTT.ks <- cbind("Grassland/Tea tree woodland", nrow(PG.Variograms), Kolmogorov.Smirnov_GRTT$statistic, Kolmogorov.Smirnov_GRTT$p.value)
colnames(GRTT.ks)<- c("Veg", "n" , "D", "p.value")

Kolmogorov.Smirnov_EW <- ks.test(PG.Variograms$EW2001, PG.Variograms$EW2020)
EW.ks <- cbind("Eucalypt woodland", nrow(PG.Variograms), Kolmogorov.Smirnov_EW$statistic, Kolmogorov.Smirnov_EW$p.value)
colnames(EW.ks)<- c("Veg", "n" , "D", "p.value")

Variogram.ks <- rbind(GRTT.ks, EW.ks)
write.csv(Variogram.ks,
          file = "./output/Variogram_ks.csv",
          row.names = FALSE)

Variogram.ks
# p < 0.05 indicates 2020 curves are significantly different from 2001 curves in
# both vegetation types

# clean up
rm(G1.df, G2.df, EW1.df, EW2.df)
rm(PG.Variograms, Variogram.ks)
invisible(gc())

#  plot variograms and best-fit curves for each vegetation-year combination

G1.preds = gstat::variogramLine(G1.var.exp, maxdist = max(G1.var$dist))
G2.preds = gstat::variogramLine(G2.var.exp, maxdist = max(G2.var$dist))
EW1.preds = gstat::variogramLine(EW1.var.exp, maxdist = max(EW1.var$dist))
EW2.preds = gstat::variogramLine(EW2.var.exp, maxdist = max(EW2.var$dist))

G1.preds$label <- "Grassland/Tea tree woodland 2001"
G2.preds$label <- "Grassland/Tea tree woodland 2020"
EW1.preds$label <- "Eucalypt woodland 2001"
EW2.preds$label <- "Eucalypt woodland 2020"

preds <- rbind(G1.preds, G2.preds, EW1.preds, EW2.preds)
preds$label <- factor(preds$label, levels = c("Grassland/Tea tree woodland 2020", "Grassland/Tea tree woodland 2001", "Eucalypt woodland 2020", "Eucalypt woodland 2001"))

# clean up
rm(Grassland2001dt, Grassland2020dt, EWoodland2001dt, EWoodland2020dt)
rm(G1.var, G2.var, EW1.var, EW2.var)
rm(G1.var.exp, G2.var.exp, EW1.var.exp, EW2.var.exp)
rm(G1.preds, G2.preds, EW1.preds, EW2.preds)
invisible(gc())

Variogram.plot <- ggplot(var, aes(x = dist, y = gamma, colour = label)) +
  geom_point(aes(shape=label, colour = label, fill = label), size = 3) +
  geom_line(data = preds, linetype=1, lwd = 0.75)+
  theme_bw()+
  scale_x_continuous(name ="Distance (m)")+
  scale_y_continuous(name ="Semivariance")+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_line(size = 0), axis.ticks = element_line(size = 0.8), panel.border = element_rect(size=0.8))+
  theme(text = element_text(size = 14, color = "black"), legend.text = element_text(size = 14, color = "black"), axis.text = element_text(size = 14, color = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  scale_colour_manual(name = "", 
                     values=c("#0072BC", "#00B9F1", "#F15A22", "#F7931D"), 
                     labels = c("Grassland/\nTeatree woodland\n   2020\n\n", "   2001", 
                                "Eucalypt\nwoodland\n   2020\n\n", "   2001")) +
  scale_shape_manual(name = "", 
                     values = c(24,25,22,23), 
                     labels = c("Grassland/\nTeatree woodland\n   2020\n\n", "   2001", 
                                "Eucalypt\nwoodland\n   2020\n\n", "   2001")) +
  scale_fill_manual(name = "", 
                    values=c("#0072BC", "#00B9F1", "#F15A22", "#F7931D"), 
                      labels = c("Grassland/\nTeatree woodland\n   2020\n\n", "   2001", 
                                 "Eucalypt\nwoodland\n   2020\n\n", "   2001"))

Variogram.plot 

ggsave("./output/Variogram_plot.tiff", plot = Variogram.plot, width = 19.04, height = 10.55, units = "cm", dpi = 499)

# clean up
dev.off()
rm(list = ls())
invisible(gc())

###############################################################################
#  Assess woody plant counts and draw box plots
###############################################################################

# load plant count data
load("WP.RData")

WP$Year <- as.factor(WP$Year)
WP$Class <- factor(WP$Class, levels = c("1-3", "> 3"))

WP.plot <- ggplot(WP, aes(x=Class, y=Count, fill=Year)) +
  geom_boxplot(lwd=0.75)+
  theme_bw()+
  scale_fill_manual(values = c("white", "gray"))+
  scale_x_discrete(name ="Size class (m)")+
  scale_y_continuous(name ="Number of woody plants")+
  theme(panel.grid = element_blank())+
  theme(legend.position = c(0.2, 0.8))+
  theme(axis.line = element_line(size = 0), axis.ticks = element_line(size = 0.8), panel.border = element_rect(size=0.8))+
  theme(text = element_text(size = 14, color = "black"), legend.text = element_text(size = 14, color = "black"), axis.text = element_text(size = 14, color = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5))

WP.plot
ggsave("./output/WoodyPlant.tiff", plot = WP.plot, width = 17.6, height = 12, units = "cm", dpi = 499)

# assess significance of differences in woody plant number in each year
WP_wide <- tidyr::spread(WP, Year, Count)
colnames(WP_wide)[3] <- "yr2001"
colnames(WP_wide)[4] <- "yr2021"
WP_wide[is.na(WP_wide)] <- 0

# assess differences in small woody plants
WP_small <- WP_wide[ which(WP_wide$Class == "1-3"), ]
before <- as.numeric(WP_small$yr2001)
after <- as.numeric(WP_small$yr2021)

small.paired <- PairedData::paired(before, after)
PairedData::plot(small.paired, type = "profile") + theme_bw() + ggtitle(" ")

# as distribution is asymmetrical, perform Two-sample sign test
Sign.WP_small <- DescTools::SignTest(x = WP_small$yr2001,y = WP_small$yr2021)
WP_small.Sign <- cbind("1-3 m", nrow(WP_small), median(WP_small$yr2001), median(WP_small$yr2021), Sign.WP_small$statistic, Sign.WP_small$parameter, Sign.WP_small$p.value)
colnames(WP_small.Sign)<- c("Size", "n", "medNo2001", "medNo2021", "S", "Differences", "p.value")

# p < 0.05 indicates significantly more small plants in 2021 than in 2001 

# assess differences in large woody plants
WP_large <- WP_wide[ which(WP_wide$Class == "> 3"), ]
before <- as.numeric(WP_large$yr2001)
after <- as.numeric(WP_large$yr2021)
large.paired <- PairedData::paired(before, after)
PairedData::plot(large.paired, type = "profile") + theme_bw()

# as distribution is asymmetrical, perform Two-sample sign test
Sign.WP_large <- DescTools::SignTest(x = WP_large$yr2001,y = WP_large$yr2021)
WP_large.Sign <- cbind("> 3 m", nrow(WP_large), median(WP_large$yr2001), median(WP_large$yr2021), Sign.WP_large$statistic, Sign.WP_large$parameter, Sign.WP_large$p.value)
colnames(WP_large.Sign)<- c("Size", "n", "medNo2001", "medNo2021", "S", "Differences", "p.value")

WP.Sign.test <- rbind(WP_small.Sign, WP_large.Sign)
WP.Sign.test

write.csv(WP.Sign.test,
          file = "./output/WoodyPlant_SignTest.csv",
          row.names = FALSE)

# p < 0.05 indicates significantly more large plants in 2021 than in 2001 

# clean up
dev.off()
rm(list = ls())
invisible(gc())
