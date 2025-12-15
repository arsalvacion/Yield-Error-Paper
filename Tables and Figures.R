####################################################
# Yield Error Paper V_06 Figures and Tables        #
# ARSalvacion                                      #
####################################################

library(ggplot2)
library(tidyverse)
library(patchwork)
library(sf)
library(ggspatial)
library(gt)
library(gtExtras)
library(gtsummary)
library(ggpubr)
library(RColorBrewer)
library(raster)
library(terra)
library(tmaptools)
library(OpenStreetMap)
library(tidyterra)
library(colorspace)
library(ggpubr)
library(rcompanion)
library(FSA)
library(moments)
library(classInt)
library(gstat)
library(ggalluvial)
library(ggsci)
library(automap)

setwd("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/R Code/Tables and Figures/")

# Figure 1 ---- 


## Fig 1a

WA<-st_read("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/DroughtHub/Aus_WGS.gpkg", quiet=TRUE)|>
  filter(STE_NAME21=="Western Australia")

AU<-st_read("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/DroughtHub/Aus_WGS.gpkg", quiet=TRUE)


AUSP<-st_read("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/C4AP team - Documents/General/Data/ABS Boundary Maps/Aus_WGS_001DS.gpkg", quiet=TRUE)

warfz<-st_read("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/C4AP team - Documents/General/Data/WA CVT Zone/WARainfallZones.gpkg", quiet=TRUE)|>
  st_transform(crs=4326)|>
  mutate(RFZ = factor(rainfall_r_3, levels=c("Low","Medium","High","VeryHigh")))

warfzl<-st_read("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/C4AP team - Documents/General/Data/WA CVT Zone/WARainfallZones_Lines2.gpkg", quiet=TRUE)|>
  st_transform(crs=4326)|>
  mutate(RFZ = factor(rainfall_r_3, levels=c("Low","Medium","High","Very High")))

farm1<-st_read("/Volumes/dmp/0-9/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Young Hill Farms/Data/boundaries/boundaries.gpkg",quiet=TRUE)

#farm2<-st_read("/Volumes/dmp/0-9/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Marshall/Data/boundaries_2024/boundaries.gpkg",quiet=TRUE)
farm2<-st_read("/Volumes/dmp/0-9/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Koranga/Data/boundaries/boundaries.gpkg",quiet=TRUE)

farm3<-st_read("/Volumes/dmp/0-9/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Kirnan/Data/boundaries/boundaries.gpkg", quiet=TRUE)

farm4<-st_read("/Volumes/dmp/0-9/Drought_hub-EASTOJ-SE21104/Project Participants/Neenwest/Data/boundaries/boundaries1.gpkg", quiet=TRUE)

farm1a<-farm1|>
  dplyr::filter(Farm %in% c("Minchies","Jucasta","Tarkyne"))|>
  st_centroid()|>
  st_transform(crs=4326)|>
  st_coordinates() |>
  data.frame()|>
  dplyr::summarise(across(everything(), mean))|>
  mutate(Farm = "Farm 1a")

farm1b<-farm1|>
  dplyr::filter(!Farm %in% c("Minchies","Jucasta","Tarkyne"))|>
  st_centroid()|>
  st_transform(crs=4326)|>
  st_coordinates() |>
  data.frame()|>
  dplyr::summarise(across(everything(), mean))|>
  mutate(Farm = "Farm 1b")

farm2<-farm2|>
  st_centroid()|>
  st_transform(crs=4326)|>
  st_coordinates() |>
  data.frame()|>
  dplyr::summarise(across(everything(), mean))|>
  mutate(Farm = "Farm 2")

farm3<-farm3|>
  st_centroid()|>
  st_transform(crs=4326)|>
  st_coordinates() |>
  data.frame()|>
  dplyr::summarise(across(everything(), mean))|>
  mutate(Farm = "Farm 3")

farm4<-farm4|>
  st_centroid()|>
  st_transform(crs=4326)|>
  st_coordinates() |>
  data.frame()|>
  dplyr::summarise(across(everything(), mean))|>
  mutate(Farm = "Farm 4")

farms<-rbind(farm1a,farm1b, farm2, farm3,farm4)


lbls<-c("Low", "Medium", "High", "Very High")

fig1a_m<-warfz|>ggplot()+
  geom_sf(data=AU, fill="grey80")+
  geom_sf(data=WA, fill="#FAF9F6")+
  geom_sf(aes(fill=RFZ))+
  scale_fill_manual(values=c( '#b5eeff', '#6eabf5','#2373b7','#003a77'), labels=lbls)+
  xlim(114.097916275, 123.309239077)+
  ylim(-35.135511236, -27.488643924)+
  theme_bw()+
  geom_point(data=farms, aes(x=X,y=Y, shape=Farm), size=3, fill="black")+
  scale_shape_manual(values=c(21, 22, 23, 24,25))+
  labs(x="Longitude", y="Latitude", fill="Rainfall Zones", shape="Farms")+
  theme(legend.background = element_rect(fill="grey90"),
        legend.box = "horizontal")+
  #theme(legend.position = "inside", legend.position.inside = c(0.65,0.831))+
  theme(legend.position = "inside", legend.position.inside = c(0.705,0.831))+
  theme(panel.background =  element_rect(fill ="#cbeaff") ,
        panel.grid.major = element_line(colour="grey80"),
        panel.border = element_rect(fill=NA, colour = "black", linewidth = rel(1)))+
  annotation_scale()+
  annotation_north_arrow(
    which_north = "true",
    style=north_arrow_minimal(),
    pad_x = unit(-0.15, "in"), pad_y = unit(0.25, "in"))

bbx1<-st_as_sfc(st_bbox(warfz))


fig1a_i<-AUSP|>
  # filter( STE_NAME21=="Western Australia")|>
  ggplot()+
  geom_sf(fill="grey90")+
  geom_sf(data=WA, fill="#FAF9F6")+
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  geom_sf(data=bbx1, fill=NA, colour="red",lwd=0.55)+
  theme(panel.background =  element_rect(fill ="#cbeaff") ,
        panel.grid.major = element_line(colour="grey90"),
        panel.border = element_rect(fill=NA, colour = "black", linewidth = rel(1)))


#fig1a<-fig1+inset_element(fig1i, left = 0.6, bottom = 0.6, right = 0.95, top = 1)
fig1a<-fig1a_m+inset_element(fig1a_i, left = 0.65, bottom = 0.2, right = 0.95, top =0.8)


## Fig 1b

ausb<-st_read("/Volumes/dmp/A-J/Agri_Analytics_Hub-EASTOJ-SE22510/Staff/Arnold/Data/Aus_WGS.gpkg", quiet=T)
espe<-st_read("/Volumes/dmp/A-J/Agri_Analytics_Hub-EASTOJ-SE22510/Staff/Arnold/Data/Esperance.gpkg", quiet=T)
yhfb<-st_read("/Volumes/dmp/A-J/Digital_Edge_RES6520-EASTOJ-SE21023/Project Participants/Young Hill Farms/Data/boundaries/boundaries.gpkg", quiet=T)


yhfbwgs<-st_transform(yhfb, crs=4326)

fn<-c("Gnarda","Spurlings","MtNey","Maroomba")


stwgs<-yhfbwgs|>
  filter(Farm == "Gnarda")

stwgsp<-yhfb|>
  filter(Farm == "Gnarda")

bm <- rast(read_osm(stwgs, ext = 1.2, type = "esri-imagery"))


fig1b_m<-stwgs|>
  mutate(FieldN = str_split(Field, " ",simplify = T)[, 2])|>
  ggplot()+
  geom_spatraster_rgb(data = bm)+
  geom_sf(fill=NA, color="red")+
  theme_bw()+
  ggrepel::geom_text_repel(
    aes(label = FieldN, geometry = geom),
    stat = "sf_coordinates",
    force = 0,
    color = "white",     # text color
    bg.color = "grey30", # shadow color
    bg.r = 0.15)+
  theme(legend.position = c(0.1,0.50))+
  coord_sf(datum=32750)+
  labs(x="Easting (m)", y="Northing (m)")+
  annotation_scale()+
  annotation_north_arrow(
    which_north = "true",
    style=north_arrow_minimal(),
    pad_x = unit(-0.15, "in"), pad_y = unit(0.25, "in"))

bbx<-st_as_sfc(st_bbox(stwgs))

fig1b_i<-ausb|>
  filter( STE_NAME21=="Western Australia")|>
  ggplot()+
  geom_sf(fill="grey90")+
  geom_sf(data=espe, fill="beige")+
  theme_bw()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  geom_sf(data=bbx, fill=NA, colour="red")


fig1b<-fig1b_m+inset_element(fig1b_i, left = 0.75, bottom = 0.01, right = 0.99, top = 0.3)

fig1<-fig1a+fig1b

ggsave("Fig1.png", 
       plot= fig1,
       width = 7200,
       height = 3600,
       unit="px",
       dpi=600)


# Figure 2 ----
weatherdat<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/MonthlyRain_2020.csv")


fig2<-weatherdat|>
  mutate(Month=factor(Month, levels=month.abb))|>
  ggplot(aes(x=Month, y=Rain))+
  geom_col(fill="#009392")+
  geom_line(aes(y = Tmax* 1.73, group=1), color = "#cf597e", linewidth=2.5)+
  geom_point(aes(y = Tmax* 1.73, group=1), size=3.5, shape=16)+
  geom_line(aes(y = Tmin* 1.73, group=1), linetype=2,color = "#004488", linewidth=3)+
  geom_point(aes(y = Tmin* 1.73, group=1), size=3.5, shape=16)+
  theme_bw()+
  scale_y_continuous(
    name = "Rainfall (mm)",
    sec.axis = sec_axis(~ . /1.73 , name = expression("Temperature ("~degree~"C)")))

ggsave("Fig2.png", 
       plot= fig2,
       width = 3800,
       height = 2800,
       unit="px",
       dpi=600)    


# Table 1 - Manually edited to include annual rainfall from 2018-2022 ----

kir_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/kirnan_yldsum_raw.csv")
kir_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/kirnan_yldsum_clean.csv")

yh_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/yhill_yldsum_raw.csv")
yh_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/yhill_yldsum_clean.csv")

# mar_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/marshall_yldsum_raw.csv")
# mar_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/marshall_yldsum_clean.csv")

kor_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/koranga_yldsum_raw.csv")
kor_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/koranga_yldsum_clean.csv")

nen_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/neenwest_yldsum_raw.csv")
nen_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/neenwest_yldsum_clean.csv")

kir_r1<-kir_raw|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  mutate(Status="Raw",
         FB = "FB3")

kir_c1<-kir_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  mutate(Status="Clean",
         FB = "FB3")


kir_diff<-kir_r1|>
  dplyr::select(Field, Farm, Area,Year,RFZ)|>
  mutate(MeanD = kir_r1$MeanYld-kir_c1$MeanYld,
         RngD = kir_r1$RangeYld-kir_c1$RangeYld,
         MnD = kir_r1$MinYld-kir_c1$MinYld,
         MxD = kir_r1$MaxYld-kir_c1$MaxYld,
         SdD = kir_r1$SDYld-kir_c1$SDYld,
         CVD = kir_r1$CV-kir_c1$CV
  )|>
  mutate(FB = "FB3")



yh_r1<-yh_raw|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(Field != "Gnarda" & Year !=2020)|>
  filter(!Field %in% c("J2","J3", "M3","T2"))|>
  mutate(Status="Raw",
         FB = "FB1")

yh_c1<-yh_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(Field != "Gnarda" & Year !=2020)|>
  filter(!Field %in% c("J2","J3", "M3","T2"))|>
  mutate(Status="Clean",
         FB = "FB1")


yhm<-merge(yh_r1,yh_c1, by=c("Year","Field"))

yh_diff<-yhm|>
  mutate(MeanD = MeanYld.x-MeanYld.y,
         RngD = RangeYld.x-RangeYld.y,
         MnD = MinYld.x-MinYld.y,
         MxD = MaxYld.x-MaxYld.y,
         SdD = SDYld.x-SDYld.y,
         CVD = CV.x-CV.y
  )|>
  dplyr::select(Field,Farm.x,Area.x, Year,RFZ.x,MeanD,RngD,MnD,MxD,SdD,CVD)|>
  rename(Farm=Farm.x, Area =Area.x, RFZ=RFZ.x)|>
  mutate(FB = "FB1")


kor_r1<-kor_raw|>
  filter(Product=="wheat", 
        Year>2017)|>
  filter(Year<2023)|>
  filter(Area>20)|>
  mutate(Status="Raw",
         FB = "FB2")

kor_c1<-kor_cln|>
  filter(Product=="wheat")|>
  mutate(Status="Clean",
         FB = "FB2")

kor_r1|>
  summarise(n_distinct(Field))

mar_diff<-mar_r1|>
  dplyr::select(Field, Farm, Area,Year,RFZ)|>
  mutate(MeanD = mar_r1$MeanYld-mar_c1$MeanYld,
         RngD = mar_r1$RangeYld-mar_c1$RangeYld,
         MnD = mar_r1$MinYld-mar_c1$MinYld,
         MxD = mar_r1$MaxYld-mar_c1$MaxYld,
         SdD = mar_r1$SDYld-mar_c1$SDYld,
         CVD = mar_r1$CV-mar_c1$CV
  )|>
  mutate(FB = "FB2")


nen_r1<-nen_raw|>
  filter(Product=="wheat",
         Year>2017,
         Kur<1000)|>
  filter(Year<2023)|>
  mutate(Status="Raw",
         FB = "FB4")

nen_c1<-nen_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  mutate(Status="Clean",
         FB = "FB4")

nenm<-merge(nen_r1,nen_c1, by=c("Year","Field"))

nen_diff<-nenm|>
  mutate(MeanD = MeanYld.x-MeanYld.y,
         RngD = RangeYld.x-RangeYld.y,
         MnD = MinYld.x-MinYld.y,
         MxD = MaxYld.x-MaxYld.y,
         SdD = SDYld.x-SDYld.y,
         CVD = CV.x-CV.y
  )|>
  dplyr::select(Field,Farm.x,Area.x, Year,RFZ.x,MeanD,RngD,MnD,MxD,SdD,CVD)|>
  rename(Farm=Farm.x, Area =Area.x, RFZ=RFZ.x)|>
  mutate(FB = "FB4")



ysmdiff<-rbind(kir_diff,yh_diff,mar_diff,nen_diff)

ysmdiff|>
  filter(Area>20)|>
  group_by(FB,RFZ)|>
  summarise(Field = n_distinct(unique(Field)),
            NYears = n_distinct(unique(Year)),
            NSample = n(),
            mnArea = min(Area),
            mxArea = max(Area),
            mMeanD = mean(MeanD),
            mnMeanD = min(MeanD),
            mxMeanD = max(MeanD),
            mMaxD = mean(MxD),
            mnMaxD = min(MxD),
            mxMaxD = max(MxD),
            mSdD = mean(SdD),
            mnSdD = min(SdD),
            mxSdD = max(SdD),
            mCVD = mean(CVD),
            mnCVD = min(CVD),
            mxCVD = max(CVD))|>
  gt()


ysm_df|>
  filter(Area>20)|>
  group_by(FB, RFZ, Status)|>
  summarise(NYears = n_distinct(unique(Year)),
            NSample = n(),
            mArea = mean (Area),
            mnArea = min(Area),
            mxArea = max(Area),
            mKur = mean(Kur),
            mnKur = min(Kur),
            mxKur = max(Kur),
            mSkew = mean(Skew),
            mnSkew = min(Skew),
            mxSkew = max(Skew)
  )




# Table 2 ---- Manually formatted in MS Word ----
kirnan_meta<-read.csv("/Volumes/dmp/A-J/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Kirnan/Data/DellaBosca_Yld_Mass_D_metadata.csv")
marsh_meta<-read.csv("/Volumes/dmp/A-J/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Marshall/Data/Marshall_Yld_Mass_D_metadata.csv")
kor_meta<-read.csv("/Volumes/dmp/A-J/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Koranga/Data/Koranga_Yld_Mass_D_metadata.csv")
yhill_meta<-read.csv("/Volumes/dmp/A-J/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Young Hill Farms/Data/Young Hill Farms (Restructure)_Yld_Mass_D_metadata.csv")
neen_meta<-read.csv("/Volumes/dmp/A-J/Drought_hub-EASTOJ-SE21104/Project Participants/Neenwest/Data/Neenwest_Yld_Mass_D_metadata.csv")

yh_err<-yhill_meta|>
  filter(Farm != "Gnarda" & Year !=2020)|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(!Field %in% c("J2","J3", "M3","T2"))|>
  mutate (FarmC = ifelse(Farm %in% c("Minchies","Jucasta","Tarkyne"), "Farm 1a", "Farm 1b"))|>
  dplyr::select(FarmC,edge.,gps.,vel.,swth.,tlag., yld.,spat_hl.,spat_lh., Year, Farm, Field)|>
  mutate(Measurement =rowSums(across(c(edge., yld.)),na.rm=T), 
         Positional = gps.,
         Harvesting = rowSums(across(c(swth.,tlag.,vel.)),na.rm=T),
         Spatial = rowSums(across(c(spat_lh., spat_lh.)),na.rm=T),
         Total = rowSums(across(c(Measurement, Positional, Harvesting, Spatial)),na.rm=T))|>
  dplyr::select(FarmC,Measurement,Positional,Harvesting, Spatial, Total)

kor_err<-kor_meta|>
  filter(Product=="wheat",
         Year>2017)|>
  filter(Year<2023)|>
  mutate (FarmC = "Farm 2")|>
  dplyr::select(FarmC, edge.,gps.,vel.,swth.,tlag., yld.,spat_hl.,spat_lh., Year, Farm, Field)|>
  mutate(Measurement =rowSums(across(c(edge., yld.)),na.rm=T),
         Positional = gps.,
         Harvesting = rowSums(across(c(swth.,tlag.,vel.)),na.rm=T),
         Spatial = rowSums(across(c(spat_lh., spat_lh.)),na.rm=T),
         Total = rowSums(across(c(Measurement, Positional, Harvesting, Spatial)),na.rm=T))|>
  dplyr::select(FarmC,Measurement,Positional,Harvesting, Spatial, Total)

# mar_err<-marsh_meta|>
#   filter(Product=="wheat", 
#          Year>2017)|>
#   filter(Year<2023)|>
#   mutate (FarmC = "Farm 2")|>
#   dplyr::select(FarmC, edge.,gps.,vel.,swth.,tlag., yld.,spat_hl.,spat_lh., Year, Farm, Field)|>
#   mutate(Measurement =rowSums(across(c(edge., yld.)),na.rm=T), 
#          Positional = gps.,
#          Harvesting = rowSums(across(c(swth.,tlag.,vel.)),na.rm=T),
#          Spatial = rowSums(across(c(spat_lh., spat_lh.)),na.rm=T),
#          Total = rowSums(across(c(Measurement, Positional, Harvesting, Spatial)),na.rm=T))|>
#   dplyr::select(FarmC,Measurement,Positional,Harvesting, Spatial, Total)

kir_err<-kirnan_meta|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  mutate (FarmC = "Farm 3")|>
  dplyr::select(FarmC, edge.,gps.,vel.,swth.,tlag., yld.,spat_hl.,spat_lh., Year, Farm, Field)|>
  mutate(Measurement =rowSums(across(c(edge., yld.)),na.rm=T), 
         Positional = gps.,
         Harvesting = rowSums(across(c(swth.,tlag.,vel.)),na.rm=T),
         Spatial = rowSums(across(c(spat_lh., spat_lh.)),na.rm=T),
         Total = rowSums(across(c(Measurement, Positional, Harvesting, Spatial)),na.rm=T))|>
  dplyr::select(FarmC,Measurement,Positional,Harvesting, Spatial, Total)

neen_err<-neen_meta|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  mutate (FarmC = "Farm 4")|>
  dplyr::select(FarmC, edge.,gps.,swth.,tlag., yld.,spat_hl.,spat_lh., Year, Farm, Field)|>
  mutate(Measurement =rowSums(across(c(edge., yld.)),na.rm=T), 
         Positional = gps.,
         Harvesting = rowSums(across(c(swth.,tlag.)),na.rm=T),
         Spatial = rowSums(across(c(spat_lh., spat_lh.)),na.rm=T),
         Total = rowSums(across(c(Measurement, Positional, Harvesting, Spatial)),na.rm=T))|>
  dplyr::select(FarmC,Measurement,Positional,Harvesting, Spatial, Total)
  

err_df<-rbind(yh_err,kor_err, kir_err,neen_err)

err_df|>
  tidyr::pivot_longer(!c(FarmC), names_to = "Errors", values_to = "Prop")|>
  dplyr::group_by(FarmC, Errors)|>
  dplyr::summarise(Mean = round(mean(Prop),2),
                   Min = round(min(Prop),2),
                   Max = round(max(Prop),2))|>
  gt()


err_df|>
  tidyr::pivot_longer(!c(FarmC), names_to = "Errors", values_to = "Prop")|>
  dplyr::group_by(Errors)|>
  dplyr::summarise(Mean = round(mean(Prop),2),
                   Min = round(min(Prop),2),
                   Max = round(max(Prop),2))|>
  gt()

# Table 3 ----

kir_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/kirnan_yldsum_clean.csv")
yh_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/yhill_yldsum_clean.csv")
#mar_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/marshall_yldsum_clean.csv")
kor_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/koranga_yldsum_clean.csv")
nen_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/neenwest_yldsum_clean.csv")



yh_c1<-yh_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(Field != "Gnarda" & Year !=2020)|>
  filter(Area>20)|>
  mutate(Status= "Clean")|>
  # filter(!Field %in% c("J2","J3", "M3","T2"))|>
  mutate (FB = ifelse(Farm %in% c("Minchies","Jucasta","Tarkyne"), "Farm 1a", "Farm 1b"))


kir_c1<-kir_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(Area>20)|>
  mutate(Status="Clean",
         FB = "Farm 3")

kor_c1<-kor_cln|>
  filter(Product=="wheat")|>
  filter(Area>20)|>
  mutate(RFZ="Medium",
         Status="Clean",
         FB = "Farm 2")

nen_c1<-nen_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(Area>20)|>
  mutate(Status="Clean",
         FB = "Farm 4")

cln_df<-rbind(yh_c1,kir_c1, nen_c1,kor_c1)


cln_df|>
  group_by(FB)|>
  summarise(mMeanYld = mean(MeanYld),
            mnMeanYld = min(MeanYld),
            mxMeanYld = max(MeanYld),
            mSD = mean(SDYld),
            mnSD = min(SDYld),
            mxSD = max(SDYld),
            mCV= mean(CV),
            mnCV = min(CV),
            mxCV = max(CV)
            # mKur= mean(Kur),
            # mnKur = min(Kur),
            # mxKur = max(Kur),
            # mSkew= mean(Skew),
            # mnSkew = min(Skew),
            # mxSkew = max(Skew),
  )|>
  gt::gt()

### Table 4 ----

g_meta<-yhill_meta|>
  filter(Product=="wheat")|>
  filter(Farm == "Gnarda",
         Year ==2020)|>
  dplyr::select(Field,edge.,gps.,vel.,swth.,tlag., yld.,spat_hl.,spat_lh., Year, Farm, Field)|>
  mutate(Measurement =rowSums(across(c(edge., yld.)),na.rm=T), 
         Positional = gps.,
         Harvesting = rowSums(across(c(swth.,tlag.,vel.)),na.rm=T),
         Spatial = rowSums(across(c(spat_lh., spat_lh.)),na.rm=T),
         Total = rowSums(across(c(Measurement, Positional, Harvesting, Spatial)),na.rm=T))|>
  dplyr::select(Field,Measurement,Positional,Harvesting, Spatial, Total)

  
g_meta|>
  tidyr::pivot_longer(!c(Field), names_to = "Errors", values_to = "Prop")|>
  dplyr::group_by(Field, Errors)|>
  dplyr::summarise(Mean = round(mean(Prop),2),
                   Min = round(min(Prop),2),
                   Max = round(max(Prop),2))|>
  gt()


g_meta|>
  tidyr::pivot_longer(!c(Field), names_to = "Errors", values_to = "Prop")|>
  dplyr::group_by(Errors)|>
  dplyr::summarise(Mean = round(mean(Prop),2),
                   Min = round(min(Prop),2),
                   Max = round(max(Prop),2))|>
  gt()

# Table 5 ----

kir_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/kirnan_yldsum_raw.csv")
kir_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/kirnan_yldsum_clean.csv")

yh_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/yhill_yldsum_raw.csv")
yh_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/yhill_yldsum_clean.csv")

mar_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/marshall_yldsum_raw.csv")
mar_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/marshall_yldsum_clean.csv")

kor_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/koranga_yldsum_raw.csv")
kor_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/marshall_yldsum_clean.csv")

nen_raw<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/neenwest_yldsum_raw.csv")
nen_cln<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-Curtin/Research/Yield Errors Effect/neenwest_yldsum_clean.csv")


kir_r1<-kir_raw|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(!Field %in% c("Spargos Backs", "T6"))|>
  mutate(Status="Raw",
         FB = "FB3")

kir_c1<-kir_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(!Field %in% c("Spargos Backs", "T6"))|>
  mutate(Status="Clean",
         FB = "FB3")


kir_diff<-kir_r1|>
  dplyr::select(Field, Farm, Area,Year,RFZ)|>
  mutate(MeanD = ((kir_c1$MeanYld-kir_r1$MeanYld)/(kir_r1$MeanYld))*100,
         MxD = ((kir_c1$MaxYld-kir_r1$MaxYld)/(kir_r1$MaxYld))*100,
         SdD = ((kir_c1$SDYld- kir_r1$SDYld)/( kir_r1$SDYld))*100,
         CVD = ((kir_c1$CV-kir_r1$CV)/(kir_r1$CV))*100,
         KurD =((kir_c1$Kur-kir_r1$Kur)/(kir_r1$Kur))*100 
  )|>
  mutate(FB = "FB3")



yh_r1<-yh_raw|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(Farm != "Gnarda" & Year !=2020)|>
  mutate(Status="Raw",
         FB = "FB1")

yh_c1<-yh_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  filter(Farm != "Gnarda" & Year !=2020)|>
  mutate(Status="Clean",
         FB = "FB1")


yhm<-merge(yh_r1,yh_c1, by=c("Year","Field"))

yh_diff<-yhm|>
  mutate(MeanD = ((MeanYld.y- MeanYld.x)/(MeanYld.x))*100,
         MxD = ((MaxYld.y-MaxYld.x)/(MaxYld.x))*100,
         SdD = ((SDYld.y-SDYld.x)/(SDYld.x))*100,
         CVD = ((CV.y-CV.x)/(CV.x))*100,
         KurD = ((Kur.y-Kur.x)/(Kur.x))*100
  )|>
  dplyr::select(Field,Farm.x,Area.x, Year,RFZ.x,MeanD,MxD,SdD,CVD,KurD )|>
  rename(Farm=Farm.x, Area =Area.x, RFZ=RFZ.x)|>
  mutate(FB = "FB1")


mar_r1<-mar_raw|>
  filter(Product=="wheat")|>
  mutate(Status="Raw",
         FB = "FB2")

mar_c1<-mar_cln|>
  filter(Product=="wheat")|>
  mutate(Status="Clean",
         FB = "FB2")

mar_diff<-mar_r1|>
  dplyr::select(Field, Farm, Area,Year,RFZ)|>
  mutate(MeanD = ((mar_c1$MeanYld-mar_r1$MeanYld)/(mar_c1$MeanYld))*100,
         MxD = ((mar_c1$MaxYld-mar_r1$MaxYld)/(mar_r1$MaxYld))*100,
         SdD = ((mar_c1$SDYld-mar_r1$SDYld)/(mar_r1$SDYld))*100,
         CVD =((mar_c1$CV- mar_r1$CV)/ (mar_r1$CV))*100,
         KurD =((mar_c1$Kur- mar_r1$Kur)/ (mar_r1$Kur))*100
  )|>
  mutate(FB = "FB2")


nen_r1<-nen_raw|>
  filter(Product=="wheat",
         Year>2017,
         Kur<1000)|>
  filter(Year<2023)|>
  mutate(Status="Raw",
         FB = "FB4")

nen_c1<-nen_cln|>
  filter(Product=="wheat", 
         Year>2017)|>
  filter(Year<2023)|>
  mutate(Status="Clean",
         FB = "FB4")

nenm<-merge(nen_r1,nen_c1, by=c("Year","Field"))

nen_diff<-nenm|>
  mutate(MeanD = ((MeanYld.y-MeanYld.x)/(MeanYld.x))*100,
         MxD = ((MaxYld.y-MaxYld.x)/(MaxYld.x))*100,
         SdD = ((SDYld.y-SDYld.x)/(SDYld.x))*100,
         CVD = ((CV.y- CV.x)/( CV.x))*100,
         KurD = ((Kur.y- Kur.x)/( Kur.x))*100
  )|>
  dplyr::select(Field,Farm.x,Area.x, Year,RFZ.x,MeanD,MxD,SdD,CVD,KurD)|>
  rename(Farm=Farm.x, Area =Area.x, RFZ=RFZ.x)|>
  mutate(FB = "FB4")



ysmdiff<-rbind(kir_diff,yh_diff,mar_diff,nen_diff)

ysmdiff|>
  filter(Area>20)|>
  group_by(FB,RFZ)|>
  summarise(MPctDm = mean(MeanD),
            MPctDmn = min(MeanD),
            MPctDmx = max(MeanD),
            MxPctDm = mean(MxD),
            MxPctDmn = min(MxD),
            MxPctDmx = max(MxD),
            SdPctDm = mean(SdD),
            SdPctDmn = min(SdD),
            SdPctDmx = max(SdD),
            CVPctDm = mean(CVD),
            CVPctDmn = min(CVD),
            CVPctDmx = max(CVD),
            KurPctDm = mean(KurD),
            KurPctDmn = min(KurD),
            KurPctDmx = max(KurD))|>
  gt()|>
  fmt_number(
    columns = 3:14,
    decimals =2
  )
  


ysmdiff|>
  filter(Area>20)|>
  #group_by(FB,RFZ)|>
  summarise(MPctDm = mean(MeanD),
            MPctDmn = min(MeanD),
            MPctDmx = max(MeanD),
            MxPctDm = mean(MxD),
            MxPctDmn = min(MxD),
            MxPctDmx = max(MxD),
            SdPctDm = mean(SdD),
            SdPctDmn = min(SdD),
            SdPctDmx = max(SdD),
            CVPctDm = mean(CVD),
            CVPctDmn = min(CVD),
            CVPctDmx = max(CVD))|>
  gt()|>
  fmt_number(
    columns = 3:14,
    decimals =2
  )


# Table 6 ----

gnarda_r1<-yh_raw|>
  filter(Product=="wheat", 
         Year==2020, 
         Farm == "Gnarda")|>
  mutate(Status="Raw",
         FB = "FB1")

gnarda_c1<-yh_cln|>
  filter(Product=="wheat", 
         Year==2020, 
         Farm == "Gnarda")|>
  mutate(Status="Raw",
         FB = "FB1")


gnarda_diff<-gnarda_c1[,c(3,7,5,10,11,12)]-gnarda_r1[,c(3,7,5,10,11,12)]

gnarda_diff_p<-(gnarda_c1[,c(3,7,5,10,11,12)]-gnarda_r1[,c(3,7,5,10,11,12)])/(gnarda_r1[,c(3,7,5,10,11,12)])*100

gnarda_m<-merge(gnarda_r1,gnarda_c1, by=c("Field"))

gnarda_diff$Field

# Fig 4 ----

bm1 <- rast(read_osm(stwgs, ext = 1.2, type = "esri-imagery"))

yme<-st_read("/Volumes/dmp/A-J/Digital_Edge_RES6520-EASTOJ-SE21023/Project Participants/Young Hill Farms/Data/cleaned/2020/Marked-Yld_Mass_D/Gnarda.gpkg", quiet=TRUE)

# fig3a<-yme|>
#   st_transform(yhfb, crs=4326)|>
#   filter(!is.na(cleaned))|>
#   mutate(
#     Error = case_when( cleaned == "spat_hl" | cleaned == "spat_lh"~ "Spatial",
#                        cleaned == "tlag" | cleaned== "vel" | cleaned== "swth"~ "Harvesting",
#                        cleaned == "gps"~ "Positional",
#                        .default = "Measurement"))|>
#   ggplot()+
#   geom_spatraster_rgb(data = bm1)+
#   geom_sf(aes(colour=Error), size=0.15)+
#   #scale_colour_brewer(palette = "Paired")+
#   scale_colour_viridis_d()+
#   guides(color = guide_legend(override.aes = list(size = 1.9))) +
#   theme_bw()+
#   labs(x="Longitude", y="Latitude", colour="Error Type")+
#   scale_x_continuous(limits=c(122.527,122.61))+
#   theme(legend.position="inside",legend.position.inside = c(0.2,0.80))+
#   theme(legend.background = element_rect(fill="grey90"))+
#  #  theme(legend.key = element_rect(fill="grey90"))+
#   annotation_scale()+
#   annotation_north_arrow(
#     which_north = "true",
#     style=north_arrow_minimal(),
#     pad_x = unit(-0.15, "in"), pad_y = unit(0.25, "in"))


g1b<-stwgs|>
  filter(Field=="Gnarda 1")

bm1 <- rast(read_osm(g1b, ext = 1.2, type = "esri-imagery"))



fig5<-yme|>
  st_transform(yhfb, crs=4326)|>
  filter(!is.na(cleaned))|>
  filter(Field=="Gnarda 1")|>
  mutate(
    Error = case_when( cleaned == "spat_hl" | cleaned == "spat_lh"~ "Spatial",
                       cleaned == "tlag" | cleaned== "vel" | cleaned== "swth"~ "Harvesting",
                       cleaned == "gps"~ "Positional",
                       .default = "Measurement"))|>
  ggplot()+
  geom_spatraster_rgb(data = bm1)+
  geom_sf(aes(colour=Error), size=0.4)+
  scale_colour_brewer(palette = "Paired")+
  #scale_colour_viridis_d()+
  # scale_fill_brewer(palette = "Set1")+
  guides(color = guide_legend(override.aes = list(size = 1.9))) +
  #xlim(122.5299,122.5507 )+
  # ylim(-33.48137 , -33.46456 )+
  theme_bw()+
  coord_sf(datum=32750)+
  labs(x="Easting (m)", y="Northing (m)", colour="Error Type")+
  theme(legend.position="inside",legend.position.inside = c(0.17,0.80))+
  theme(legend.background = element_rect(fill="grey90"))+
  #  theme(legend.key = element_rect(fill="grey90"))+
  annotation_scale()+
  annotation_north_arrow(
    which_north = "true",
    style=north_arrow_minimal(),
    pad_x = unit(-0.15, "in"), pad_y = unit(0.25, "in"))




ggsave("Fig4.png", 
       plot= fig5,
       width = 3700,
       height = 3700,
       unit="px",
       dpi=500)


# Figure 4 ----
rawy<-st_read("/Volumes/dmp/A-J/Digital_Edge_RES6520-EASTOJ-SE21023/Project Participants/Young Hill Farms/Data/cleaned/2020/Marked-Yld_Mass_D/Gnarda.gpkg", quiet=TRUE)

clny<-st_read("/Volumes/dmp/A-J/Digital_Edge_RES6520-EASTOJ-SE21023/Project Participants/Young Hill Farms/Data/cleaned/2020/Cleaned-Yld_Mass_D/Gnarda.gpkg", quiet=TRUE)

ycol<-brewer.pal(9,"YlGn")

g1b<-stwgs|>
  filter(Field=="Gnarda 1")

bm <- rast(read_osm(g1b, ext = 1.2, type = "esri-imagery"))

rawdat<-rawy|>
  st_transform(crs=4326)|>
  filter(Field=="Gnarda 1")

brk<-classIntervals(rawdat$Yld_Mass_D, n=5, style="equal")$brks

ymn<-round(min(brk),1)
ymx<-round(max(brk),1)

fig4a<-rawdat|>
  ggplot()+
  geom_spatraster_rgb(data = bm)+
  geom_sf(aes(colour=Yld_Mass_D),size=0.15)+
  geom_sf(data=g1b, fill=NA, colour="red", size=3)+
  scale_colour_stepsn(colours = ycol, breaks=round(brk,1), 
                      limits=c(ymn,ymx), 
                      guide=guide_coloursteps(show.limits = TRUE,
                                              even.steps = FALSE))+  
  theme_bw()+
  theme(legend.position="bottom")+
  coord_sf(datum=32750)+
  guides(colour=guide_colourbar(title.position="top", barwidth=18))+
  labs(colour=' Yield (tons' ~ha^-1*")")+
  annotation_scale()+
  annotation_north_arrow(
    which_north = "true",
    style=north_arrow_minimal(),
    pad_x = unit(-0.15, "in"), pad_y = unit(0.25, "in"))+
  labs(x="Easting (m)", y="Northing (m)")
 # annotate("text", x=122.53,y=-33.465, label="(a)", size=8, colour="white")



clndat<-clny|>
  st_transform(crs=4326)|>
  mutate(cleaned = replace_na(cleaned, "cleaned"))|>
  filter(cleaned == "cleaned")|>
  filter(Field=="Gnarda 1")

brk<-classIntervals(clndat$Yld_Mass_D, n=5, style="equal")$brks

ymn<-round(min(brk),1)
ymx<-round(max(brk),1)


fig4b<-clndat|>
  ggplot()+
  geom_spatraster_rgb(data = bm)+
  geom_sf(aes(colour=Yld_Mass_D),size=0.15)+
  geom_sf(data=g1b, fill=NA, colour="red", size=3)+
  scale_colour_stepsn(colours = ycol, breaks=round(brk,1), 
                      limits=c(ymn,ymx), 
                      guide=guide_coloursteps(show.limits = TRUE,
                                              even.steps = FALSE))+  
  theme_bw()+
  coord_sf(datum=32750)+
  theme(legend.position="bottom")+
  guides(colour=guide_colourbar(title.position="top", barwidth=18))+
  labs(colour=' Yield (tons' ~ha^-1*")")+
  annotation_scale()+
  annotation_north_arrow(
    which_north = "true",
    style=north_arrow_minimal(),
    pad_x = unit(-0.15, "in"), pad_y = unit(0.25, "in"))+
  labs(x="Easting (m)", y="Northing (m)")
  #annotate("text", x=122.53,y=-33.465, label="(b)", size=8, colour="white")


ggsave("Fig4_5.png", 
       plot= (fig4a|fig4b),
       width = 7400,
       height = 4200,
       unit="px",
       dpi=600)


# Figure 5 ----
kir_vgm<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/kirnan_variograms.csv")
mar_vgm<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/marshall_variograms.csv")
nen_vgm<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/neen_variograms.csv")
yhl_vgm<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/yhills_variograms.csv")
kor_vgm<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/kor_variograms.csv")



vgm_df<-rbind(kir_vgm,kor_vgm,nen_vgm, yhl_vgm)

vgm_df|>
  filter(PRODUCT=="wheat")|>
  filter(ERROR=="Clean")|>
  mutate(CI=(NUGGET/SILL)*100)|>
  mutate(CIC = case_when(CI>75 ~"Weak",
                         CI<25 ~ "Strong",
                         .default="Moderate"))|>
  group_by(FarmB)|>
  summarise(Nfld = n_distinct(FIELD),
            NSamp = n())




raw_vgm<-vgm_df|>
  filter(PRODUCT=="wheat")|>
  filter(ERROR=="Raw")|>
  mutate(CI=(NUGGET/SILL)*100)|>
  mutate(CIC = case_when(CI>75 ~"Weak",
                         CI<25 ~ "Strong",
                         .default="Moderate"))

cln_vgm<-vgm_df|>
  filter(PRODUCT=="wheat")|>
  filter(ERROR=="Clean")|>
  mutate(CI=(NUGGET/SILL)*100)|>
  mutate(CIC = case_when(CI>75 ~"Weak",
                         CI<25 ~ "Strong",
                         .default="Moderate"))


trans_df<-data.frame(cln_vgm[,8])
trans_df$Trans <- paste(raw_vgm$CIC,cln_vgm$CIC, sep="-")
colnames(trans_df)[1]<-"FarmB"

trans_tab<-trans_df|>
  table()|>
  data.frame()|>
  separate(Trans, c("Raw","Clean"))


fig5<-trans_tab|>
  mutate(Raw= factor(Raw, levels=c("Weak","Moderate","Strong")))|>
  ggplot(aes(axis1 = Raw, axis2 = Clean, y = Freq)) +
  geom_alluvium(aes(fill = FarmB), width = 1/12) +
  geom_stratum(width = 1/6, color = "grey") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Raw", "Clean"), expand = c(.05, .05))+
  theme_bw()+labs(x= "Yield Data", y="Frequency", fill="Farms")+
  theme()+
  #ylim(c(0,500))+
  scale_fill_nejm()+
  theme(axis.text = element_text(size = 10))





ggsave("Fig5.png", 
       plot= fig5,
       width = 4800,
       height = 4000,
       unit="px",
       dpi=600)


a<-ci_df3|>
  mutate(Trans= paste(Raw,Clean, sep="-"))|>
  mutate(Perc = Freq/251)|>
  group_by(FarmB,Trans)|>
  mutate(SPerc = sum(Perc))

b<-ci_df3|>
  mutate(Trans= paste(Raw,Clean, sep="-"))|>
  mutate(Perc = Freq/251)|>
  group_by(Trans)|>
  mutate(SPerc = sum(Perc))


write.csv(ci_df2, "Error_trans.csv", row.names=F)



# Table 7 ----

g20r<-st_read("/Volumes/dmp/A-J/Digital_Edge_RES6520-EASTOJ-SE21023/Project Participants/Young Hill Farms/Data/cleaned/2020/Marked-Yld_Mass_D/Gnarda.gpkg")
g20c<-st_read("/Volumes/dmp/A-J/Digital_Edge_RES6520-EASTOJ-SE21023/Project Participants/Young Hill Farms/Data/cleaned/2020/Cleaned-Yld_Mass_D/Gnarda.gpkg")


fldr<-  unique(g20r$Field)
fldc<-  unique(g20c$Field)

x<-g20r
y<-fldr

raw_df<-data.frame(NULL)

for (i in 1:length(y)){
  ydat<-x|>
    filter(Field ==y[i])
  
  
  ak<-autoKrige(Yld_Mass_D~1, ydat, nmax=25)
  
  vmod<-as.character(ak$var_model[2,1])
  nugg<-ak$var_model[1,2]
  sill<-ak$var_model[2,2]
  
  res<-data.frame(y[i],vmod, nugg, sill)
  
  res<-res|>
    mutate(CI = (nugg/sill)*100)|>
    mutate(CIC = case_when(CI>75 ~"Weak",
                           CI<25 ~ "Strong",
                           .default="Moderate"))
  

  
  
  
  # vario <- variogram(Yld_Mass_D ~ 1, ydat)
  # ## Empirical
  # var_fit <- fit.variogram(
  #   vario,
  #   vgm(c("Exp", "Mat", "Sph", "Gau", "Ste Mat","Exc")),
  #   fit.kappa = TRUE
  # )
  # 
  # nugget = var_fit$psill[1]
  # sill = var_fit$psill[2]
  # vmod = as.character(var_fit$model[2])
  
  #res<-data.frame(y[i],vmod, nugget, sill)
  
  # res<-res|>
  #   mutate(CI = (nugget/sill)*100)|>
  #   mutate(CIC = case_when(CI>75 ~"Weak",
  #                          CI<25 ~ "Strong",
  #                          .default="Moderate"))
    
raw_df<-rbind(raw_df,res)  

}

cln_df<-data.frame(NULL)


x<-g20c
y<-fldc


for (i in 1:length(y)){
  ydat<-x|>
    filter(Field ==y[i])
  
  ak<-autoKrige(Yld_Mass_D~1, ydat, nmax=25)
  
  vmod<-as.character(ak$var_model[2,1])
  nugg<-ak$var_model[1,2]
  sill<-ak$var_model[2,2]
  
  res<-data.frame(y[i],vmod, nugg, sill)
  
  res<-res|>
    mutate(CI = (nugg/sill)*100)|>
    mutate(CIC = case_when(CI>75 ~"Weak",
                           CI<25 ~ "Strong",
                           .default="Moderate"))
  
  
  
  
  
  # vario <- variogram(Yld_Mass_D ~ 1, ydat)
  # ## Empirical
  # var_fit <- fit.variogram(
  #   vario,
  #   vgm(c("Exp", "Mat", "Sph", "Gau", "Ste Mat","Exc")),
  #   fit.kappa = TRUE
  # )
  # 
  # nugget = var_fit$psill[1]
  # sill = var_fit$psill[2]
  # vmod = as.character(var_fit$model[2])
  
  #res<-data.frame(y[i],vmod, nugget, sill)
  
  # res<-res|>
  #   mutate(CI = (nugget/sill)*100)|>
  #   mutate(CIC = case_when(CI>75 ~"Weak",
  #                          CI<25 ~ "Strong",
  #                          .default="Moderate"))
  
  cln_df<-rbind(cln_df,res)  
  
  # vario <- variogram(Yld_Mass_D ~ 1, ydat)
  # ## Empirical
  # var_fit <- fit.variogram(
  #   vario,
  #   vgm(c("Exp", "Mat", "Sph", "Gau","Ste Mat")),
  #   fit.kappa = TRUE
  # )
  # 
  # nugget = var_fit$psill[1]
  # sill = var_fit$psill[2]
  # vmod = as.character(var_fit$model[2])
  # 
  # res<-data.frame(y[i],vmod, nugget, sill)
  # 
  # res<-res|>
  #   mutate(CI = (nugget/sill)*100)|>
  #   mutate(CIC = case_when(CI>75 ~"Weak",
  #                          CI<25 ~ "Strong",
  #                          .default="Moderate"))
  
  
  a<-cbind(raw_df,cln_df)
  
  # Reduced nuggest
  nd<-cln_df$nugg-raw_df$nugg
  
  trans<-paste(raw_df$CIC,cln_df$CIC, sep="-")
  
}



# Table 8 (Part 1)----


gn_error<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/gnarda2020_v6.csv")

erf<-c("Raw","Measurement (M)","Harvesting (H)","Spatial (S)","Positional (P)", "PH","HS","PS","PHS","MP","MS","MPS","MH","MPH","MHS","Clean")


gn_error<-gn_error|>
  mutate(ARS= 1-(RMSE/SD))|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))


rmse_diff<-gn_error|>
  dplyr::select(FOLD, FIELD, RMSE, ERROR)|>
  pivot_wider(id_cols =c(FOLD, FIELD), names_from =ERROR, values_from = RMSE )|>
  group_by(FIELD)|>
  summarise(Rawm = mean(Uncleaned),
            Meas = mean (Measurement),
            Has = mean(`Harvesting Dynamics`),
            Spat = mean (`Spatial Outliers`),
            Pos = mean(Positional),
            PHm = mean(PH),
            HSm = mean(HS),
            PSm = mean(PS),
            PHSm = mean(PHS),
            MHm = mean(MH),
            MPm = mean(MP),
            MSm = mean(MS),
            MPHm = mean(MPH),
            MPSm = mean(MPS),
            MHSm = mean(MHS),
            Cleanm = mean(Cleaned))|>
  mutate(MeasD = ((Meas - Rawm)/Rawm)*100,
         PosD =((Pos - Rawm)/Rawm)*100,
         HasD = ((Has- Rawm)/Rawm)*100,
         SpD =  ((Spat- Rawm)/Rawm)*100,
         HS_D =  ((HSm - Rawm)/Rawm)*100,
         PH_D =  ((PHm - Rawm )/Rawm)*100,
         PS_D =  ((PSm - Rawm )/Rawm)*100,
         PHS_D =  ((PHSm - Rawm )/Rawm)*100,
         MH_D =  ((MHm - Rawm)/Rawm)*100,
         MP_D =  ((MPm - Rawm)/Rawm)*100,
         MS_D =  ((MSm - Rawm)/Rawm)*100,
         MPS_D =  ((MPSm - Rawm )/Rawm)*100,
         MPH_D =  ((MPHm - Rawm )/Rawm)*100,
         MHS_D =  ((MHSm - Rawm)/Rawm)*100,
         Cln_D = ((Cleanm - Rawm)/Rawm)*100)|>
  dplyr::select(FIELD,MeasD, HasD,SpD,PosD, PH_D, HS_D, PS_D, PHS_D, MH_D, MP_D, MS_D, MPH_D, MHS_D, MPS_D, Cln_D)|>
  psych::describe()|>
  data.frame()|>
  dplyr::select(mean, min, max)


lccc_diff<-gn_error|>
  dplyr::select(FOLD, FIELD, LCCC, ERROR)|>
  pivot_wider(id_cols =c(FOLD, FIELD), names_from =ERROR, values_from = LCCC )|>
  group_by(FIELD)|>
  summarise(Rawm = mean(Uncleaned),
            Meas = mean (Measurement),
            Has = mean(`Harvesting Dynamics`),
            Spat = mean (`Spatial Outliers`),
            Pos = mean(Positional),
            PHm = mean(PH),
            HSm = mean(HS),
            PSm = mean(PS),
            PHSm = mean(PHS),
            MHm = mean(MH),
            MPm = mean(MP),
            MSm = mean(MS),
            MPHm = mean(MPH),
            MPSm = mean(MPS),
            MHSm = mean(MHS),
            Cleanm = mean(Cleaned))|>
  mutate(MeasD = ((Meas - Rawm)/Rawm)*100,
         PosD =((Pos - Rawm)/Rawm)*100,
         HasD = ((Has- Rawm)/Rawm)*100,
         SpD =  ((Spat- Rawm)/Rawm)*100,
         HS_D =  ((HSm - Rawm)/Rawm)*100,
         PH_D =  ((PHm - Rawm )/Rawm)*100,
         PS_D =  ((PSm - Rawm )/Rawm)*100,
         PHS_D =  ((PHSm - Rawm )/Rawm)*100,
         MH_D =  ((MHm - Rawm)/Rawm)*100,
         MP_D =  ((MPm - Rawm)/Rawm)*100,
         MS_D =  ((MSm - Rawm)/Rawm)*100,
         MPS_D =  ((MPSm - Rawm )/Rawm)*100,
         MPH_D =  ((MPHm - Rawm )/Rawm)*100,
         MHS_D =  ((MHSm - Rawm)/Rawm)*100,
         Cln_D = ((Cleanm - Rawm)/Rawm)*100)|>
  dplyr::select(FIELD,MeasD, HasD,SpD,PosD, PH_D, HS_D, PS_D, PHS_D, MH_D, MP_D, MS_D, MPH_D, MHS_D, MPS_D, Cln_D)|>
  psych::describe()|>
  data.frame()|>
  dplyr::select(mean, min, max)


ars_diff<-gn_error|>
  dplyr::select(FOLD, FIELD, ARS, ERROR)|>
  pivot_wider(id_cols =c(FOLD, FIELD), names_from =ERROR, values_from = ARS )|>
  group_by(FIELD)|>
  summarise(Rawm = mean(Uncleaned),
            Meas = mean (Measurement),
            Has = mean(`Harvesting Dynamics`),
            Spat = mean (`Spatial Outliers`),
            Pos = mean(Positional),
            PHm = mean(PH),
            HSm = mean(HS),
            PSm = mean(PS),
            PHSm = mean(PHS),
            MHm = mean(MH),
            MPm = mean(MP),
            MSm = mean(MS),
            MPHm = mean(MPH),
            MPSm = mean(MPS),
            MHSm = mean(MHS),
            Cleanm = mean(Cleaned))|>
  mutate(MeasD = ((Meas - Rawm)/Rawm)*100,
         PosD =((Pos - Rawm)/Rawm)*100,
         HasD = ((Has- Rawm)/Rawm)*100,
         SpD =  ((Spat- Rawm)/Rawm)*100,
         HS_D =  ((HSm - Rawm)/Rawm)*100,
         PH_D =  ((PHm - Rawm )/Rawm)*100,
         PS_D =  ((PSm - Rawm )/Rawm)*100,
         PHS_D =  ((PHSm - Rawm )/Rawm)*100,
         MH_D =  ((MHm - Rawm)/Rawm)*100,
         MP_D =  ((MPm - Rawm)/Rawm)*100,
         MS_D =  ((MSm - Rawm)/Rawm)*100,
         MPS_D =  ((MPSm - Rawm )/Rawm)*100,
         MPH_D =  ((MPHm - Rawm )/Rawm)*100,
         MHS_D =  ((MHSm - Rawm)/Rawm)*100,
         Cln_D = ((Cleanm - Rawm)/Rawm)*100)|>
  dplyr::select(FIELD,MeasD, HasD,SpD,PosD, PH_D, HS_D, PS_D, PHS_D, MH_D, MP_D, MS_D, MPS_D,MPH_D, MHS_D, Cln_D)|>
  psych::describe()|>
  data.frame()|>
  dplyr::select(mean, min, max)

erf<-c("Raw","Measurement (M)","Harvesting (H)","Spatial (S)","Positional (P)", "PH","HS","PS","PHS","MP","MS","MPS","MH","MPH","MHS","Clean")






# Figure 7 ----

gn_error2<-gn_error|>
  group_by(FIELD, ERROR2)|>
  mutate (mRMSE = mean(RMSE),
           mLCCC = mean(LCCC),
           mARS = mean(ARS))|>
  mutate(EG=recode(ERROR2,
                   "Raw"="A",
                   "Measurement (M)"="B",
                   "Harvesting (H)"="C",
                   "Spatial (S)"="D",
                   "Positional (P)"="E",
                   "PH" ="F",
                   "HS"="G",
                   "PS"="H",
                   "PHS"="I",
                   "MP"="J",
                   "MS"="K",
                   "MPS" ="L",
                   "MH"="M",
                   "MPH"= "N",
                   "MHS"="O",
                   "Clean" ="P"))

mrc<-dunnTest(mRMSE~EG, method="bonferroni",data=gn_error2)

mrcres<-mrc$res

cld <- cldList(comparison = mrcres$Comparison,
               p.value    = mrcres$P.adj,
               threshold  = 0.05)
cld$Err<-erf

gn_error|>
 # mutate(ARS= 1-(RMSE/SD))|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  group_by(FIELD, ERROR2)|>
  summarise(mRMSE = mean(RMSE))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))|>
  ggplot()+
  geom_boxplot(aes(x=ERROR2,y=mRMSE, fill=ERROR2))+
  geom_text(data=cld, aes(x=Err, y=0.1, label=Letter))+
  coord_flip()+
  theme_bw()+labs(x="Error Type",y=bquote('Root Mean Square Error (t' ~ha^-1*")"))+
  scale_x_discrete(limits=rev)+
  #ylim(c(0,0.402))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_fill_manual(values=lcol)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18))


mrc_lccc<-dunnTest(mLCCC~EG, method="bonferroni",data=gn_error2)

mrcres_lccc<-mrc_lccc$res

cld_lccc <- cldList(comparison = mrcres_lccc$Comparison,
               p.value    = mrcres_lccc$P.adj,
               threshold  = 0.05)
cld_lccc$Err<-erf

gn_error|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  group_by(FIELD, ERROR2)|>
  summarise(mLCCC = mean(LCCC))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))|>
  ggplot()+
  geom_boxplot(aes(x=ERROR2,y=mLCCC, fill=ERROR2))+
  geom_text(data=cld_lccc, aes(x=Err, y=0.57, label=Letter))+
  coord_flip()+
  theme_bw()+labs(x="Error Type",y="Lin's Concordance Correlation Coefficient")+
  scale_x_discrete(limits=rev)+
  # ylim(c(0,0.402))+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_fill_manual(values=lcol)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18))


mrc_ars<-dunnTest(mARS~EG, method="bonferroni",data=gn_error2)

mrcres_ars<-mrc_ars$res

cld_ars <- cldList(comparison = mrcres_ars$Comparison,
                    p.value    = mrcres_ars$P.adj,
                    threshold  = 0.05)
cld_ars$Err<-erf


gn_error|>
  mutate(ARS= 1-(RMSE/SD))|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  group_by(FIELD, ERROR2)|>
  summarise(mARS = mean(ARS))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))|>
  ggplot()+
  geom_boxplot(aes(x=ERROR2,y=mARS, fill=ERROR2))+
  geom_text(data=cld_ars, aes(x=Err, y=0.876, label=Letter))+
  coord_flip()+
  theme_bw()+labs(x="Error Type",y="Accuracy Ratio Score")+
  scale_x_discrete(limits=rev)+
  #ylim(c(0,0.402))+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_fill_manual(values=lcol)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18))


# Figure X ---- Yield Cluster Before and After Cleaning




# Figure 7 (All data) ---- 

kir_int<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/kirnan_bk_error_test3.csv")
nen_int<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/neenwest_bk_error_test3.csv")
mar_int<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/marshall_bk_error_test3.csv")
yhs_int<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/yh_bk_error_test3.csv")
kor_int<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/koranga_bk_error_test3.csv")


erf<-c("Raw","Measurement (M)","Harvesting (H)","Spatial (S)","Positional (P)", "PH","HS","PS","PHS","MP","MS","MPS","MH","MPH","MHS","Clean")

lcol<-c("#c27c34", "white","white","white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "#249e89")

yhs_int2<-yhs_int|>
  filter(PRODUCT=="wheat")|>
  filter(Farm != "Gnarda" & Year !=2020)|>
  filter(!FIELD %in% c("Spargos Backs","T6","Causeway","Super Shed"))|>
  mutate (FarmB = ifelse(Farm %in% c("Minchies","Jucasta","Tarkyne"), "Farm 1a", "Farm 1b"))|>
  group_by(FIELD, ERROR, FarmB)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  dplyr:: select(FIELD,ERROR,mRMSE,mLCCC,mCV,mSD,mARS,FarmB)

yh1<-yhs_int2|>
  filter(FarmB == "Farm 1a")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))


yh2<-yhs_int2|>
  filter(FarmB == "Farm 1b")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))



kir_int2<-kir_int|>
  filter(PRODUCT=="wheat")|>
  filter(FIELD != "T2SW")|>
  filter(ThPProp<100)|>
  group_by(FIELD, ERROR)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  mutate(FarmB ="Farm 3")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))

nen_int2<-nen_int|>
  filter(PRODUCT=="wheat")|>
  filter(!FIELD %in% c("W1","W12","W4","W10","W14","W9EAST","W9SOUTH","W2EAST","10BDAM","7B","W20","W4ANORTH", "3+4B") )|>
  filter(ThPProp<90)|>
  group_by(FIELD, ERROR)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  mutate(FarmB ="Farm 4")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))


mar_int2<-mar_int|>
  filter(PRODUCT=="wheat")|>
  filter(FIELD != "C36" & Year !=2023)|>
  filter(FIELD != "C31" & Year !=2022)|>
  #filter(!FIELD %in% c("C31","C23","C24") )|>
  filter(ThPProp<100)|>
  group_by(FIELD, ERROR)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  mutate(FarmB ="Farm 2")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))


kor_int2<-kor_int|>
  filter(PRODUCT=="wheat")|>
  filter(ThPProp<90)|>
  filter(!FIELD %in% c("Tripod","Bush", "House","Mingenew","Mullewa","Autumn Gate","Brians","eradu","Walkaway") )|>
  group_by(FIELD, ERROR)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  mutate(FarmB ="Farm 5")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))


erf<-c("Raw","Measurement (M)","Harvesting (H)","Spatial (S)","Positional (P)", "PH","HS","PS","PHS","MP","MS","MPS","MH","MPH","MHS","Clean")


## Load the DunnBoxplot file which contain the dunnsbp function for dunns boxplot rendering

f1a_rmse<-dunnsbp(yh1,"mRMSE","ERROR2")
f1b_rmse<-dunnsbp(yh2,"mRMSE","ERROR2")
f2_rmse<-dunnsbp(mar_int2,"mRMSE","ERROR2")
f3_rmse<-dunnsbp(kir_int2,"mRMSE","ERROR2")
f4_rmse<-dunnsbp(nen_int2,"mRMSE","ERROR2")
f5_rmse<-dunnsbp(kor_int2,"mRMSE","ERROR2")

fig7a<-f1a_rmse+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y=bquote('RMSE (t' ~ha^-1*")"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 1a")

fig7b<-f1b_rmse+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y=bquote('RMSE (t' ~ha^-1*")"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 1b")

fig7c<-f5_rmse+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y=bquote('RMSE (t' ~ha^-1*")"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 2")

fig7d<-f3_rmse+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y=bquote('RMSE (t' ~ha^-1*")"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 3")

fig7e<-f4_rmse+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y=bquote('RMSE (t' ~ha^-1*")"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 4")

fig7<-fig7a+fig7b+fig7c+fig7d+fig7e

ggsave("Fig7.png", 
       plot= fig7,
       width = 9000,
       height = 8000,
       unit="px",
       dpi=600)


f1a_lccc<-dunnsbp(yh1,"mLCCC","ERROR2")
f1b_lccc<-dunnsbp(yh2,"mLCCC","ERROR2")
f2_lccc<-dunnsbp(mar_int2,"mLCCC","ERROR2")
f3_lccc<-dunnsbp(kir_int2,"mLCCC","ERROR2")
f4_lccc<-dunnsbp(nen_int2,"mLCCC","ERROR2")
f5_lccc<-dunnsbp(kor_int2,"mLCCC","ERROR2")

fig8a<-f1a_lccc+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="LCCC")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 1a")

fig8b<-f1b_lccc+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="LCCC")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 1b")

fig8c<-f5_lccc+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="LCCC")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 2")

fig8d<-f3_lccc+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="LCCC")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  scale_y_continuous(limits=c(0.5,1.01))+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 3")

fig8e<-f4_lccc+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="LCCC")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 4")

fig8<-fig8a+fig8b+fig8c+fig8d+fig8e

ggsave("Fig8.png", 
       plot= fig8,
       width = 9000,
       height = 8000,
       unit="px",
       dpi=600)


f1a_ars<-dunnsbp(yh1,"mARS","ERROR2")
f1b_ars<-dunnsbp(yh2,"mARS","ERROR2")
f2_ars<-dunnsbp(mar_int2,"mARS","ERROR2")
f3_ars<-dunnsbp(kir_int2,"mARS","ERROR2")
f4_ars<-dunnsbp(nen_int2,"mARS","ERROR2")
f5_ars<-dunnsbp(kor_int2,"mARS","ERROR2")

fig9a<-f1a_ars+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="ARS")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 1a")

fig9b<-f1b_ars+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="ARS")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 1b")

fig9c<-f5_ars+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="ARS")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 2")

fig9d<-f3_ars+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="ARS")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 3")

fig9e<-f4_ars+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="ARS")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=15),
        axis.title.x=element_text(size=18),
        plot.title = element_text(size=20, face="italic"))+
  ggtitle("Farm 4")

fig9<-fig9a+fig9b+fig9c+fig9d+fig9e

ggsave("Fig9.png", 
       plot= fig9,
       width = 9000,
       height = 8000,
       unit="px",
       dpi=600)

                        

# Table 8 Part 2 ----

df1<-rbind(yh1,yh2,kir_int2,nen_int2, kor_int2)

kor_int3<-kor_int|>
  filter(PRODUCT=="wheat")|>
  filter(ThPProp<90)|>
  filter(!FIELD %in% c("Tripod","Bush", "House","Mingenew","Mullewa","Autumn Gate","Brians","eradu","Walkaway") )|>
  group_by(FIELD, Year,ERROR)|>
  summarise(mRMSE = mean(RMSE,na.rm=T),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  mutate(FarmB ="Farm 5")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))

kir_int3<-kir_int|>
  filter(PRODUCT=="wheat")|>
  filter(!FIELD %in% c("T2SW","T6","W9NORTH","W16","C6N","N3N","W19","C16","S3","T1N","T7WELL") )|>
  filter(ThPProp<90)|>
  group_by(FIELD, Year,ERROR)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  mutate(FarmB ="Farm 3")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))


nen_int3<-nen_int|>
  filter(PRODUCT=="wheat")|>
  filter(!FIELD %in% c("W1","W12","W4","W10","W14","W16","W19","W9EAST","W9SOUTH","W9NORTH","W2EAST","10BDAM","7B","W20","W4ANORTH", "3+4B",  "W3CREEK","9B") )|>
  filter(ThPProp<90)|>
  group_by(FIELD, Year,ERROR)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  mutate(FarmB ="Farm 4")|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))

yhs_int3<-yhs_int|>
  filter(PRODUCT=="wheat")|>
  filter(Farm != "Gnarda" & Year !=2020)|>
  filter(!FIELD %in% c("Spargos Backs","T6","Causeway","Super Shed"))|>
  mutate (FarmB = ifelse(Farm %in% c("Minchies","Jucasta","Tarkyne"), "Farm 1a", "Farm 1b"))|>
  group_by(FIELD, Year,ERROR,FarmB)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mCV = mean(CV),
            mSD = mean(SDYld),
            mARS = mean(ARS))|>
  data.frame()|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, level=erf))|>
  dplyr:: select(FIELD,Year,ERROR,mRMSE,mLCCC,mCV,mSD,mARS,FarmB, ERROR2)

df_int3<-rbind(yhs_int3,kir_int3, nen_int3, kor_int3)





rmse_pc<-df_int3|>
  dplyr::select(FIELD,Year, FarmB, mRMSE, ERROR)|>
  pivot_wider(id_cols =c(FIELD, FarmB,Year), names_from =ERROR, values_from = mRMSE)|>
  mutate(MeasD = ((Measurement -Uncleaned)/Uncleaned)*100,
         HarvD = ((`Harvesting Dynamics` -Uncleaned)/Uncleaned)*100,
         SpatD =  ((`Spatial Outliers` -Uncleaned)/Uncleaned)*100,
         PosD =  ((Positional -Uncleaned)/Uncleaned)*100,
         PHD = ((PH -Uncleaned)/Uncleaned)*100,
         HSD = ((HS -Uncleaned)/Uncleaned)*100,
         PSD = ((PS -Uncleaned)/Uncleaned)*100,
         PHSD = ((PHS -Uncleaned)/Uncleaned)*100,
         MPD = ((MP -Uncleaned)/Uncleaned)*100,
         MSD = ((MS -Uncleaned)/Uncleaned)*100,
         MPSD = ((MPS -Uncleaned)/Uncleaned)*100,
         MHD = ((MH -Uncleaned)/Uncleaned)*100,
         MPHD = ((MPH -Uncleaned)/Uncleaned)*100,
         MSHD = ((MHS -Uncleaned)/Uncleaned)*100,
         ClnD = ((Cleaned -Uncleaned)/Uncleaned)*100
         )|>
  dplyr::select(FarmB, MeasD, HarvD,SpatD , PosD,PHD ,HSD, PSD,PHSD, MPD, MSD, MPSD,MHD,MPHD,MSHD, ClnD)|>
  summarise(mpMeasD = mean(MeasD, na.rm=T),
            mnpMeasD = min(MeasD, na.rm=T),
            mxpMeasD = max(MeasD, na.rm=T),
            mpHarvD = mean(HarvD, na.rm=T),
            mnpHarvD = min(HarvD, na.rm=T),
            mxpHarvD = max(HarvD, na.rm=T),
            mpSpatD = mean(SpatD, na.rm=T),
            mnpSpatD = min(SpatD, na.rm=T),
            mxpSpatD = max(SpatD, na.rm=T),
            mpPosD = mean(PosD, na.rm=T),
            mnpPosD = min(PosD, na.rm=T),
            mxpPosD = max(PosD, na.rm=T),
            mpPHD = mean(PHD, na.rm=T),
            mnpPHD = min(PHD, na.rm=T),
            mxpPHD = max(PHD, na.rm=T),
            mpHSD = mean(HSD, na.rm=T),
            mnpHSD = min(HSD, na.rm=T),
            mxpHSD = max(HSD, na.rm=T),
            mpPSD = mean(PSD, na.rm=T),
            mnpPSD = min(PSD, na.rm=T),
            mxpPSD = max(PSD, na.rm=T),
            mpPHSD = mean(PHSD, na.rm=T),
            mnpPHSD = min(PHSD, na.rm=T),
            mxpPHSD = max(PHSD, na.rm=T),
            mpMPD = mean(MPD, na.rm=T),
            mnpMPD = min(MPD, na.rm=T),
            mxpMPD = max(MPD, na.rm=T),
            mpMSD = mean(MSD, na.rm=T),
            mnpMSD = min(MSD, na.rm=T),
            mxpMSD = max(MSD, na.rm=T),
            mpMPSD = mean(MPSD, na.rm=T),
            mnpMPSD = min(MPSD, na.rm=T),
            mxpMPSD = max(MPSD, na.rm=T),
            mpMHD = mean(MHD, na.rm=T),
            mnpMHD = min(MHD, na.rm=T),
            mxpMHD = max(MHD, na.rm=T),
            mpMPHD = mean(MPHD, na.rm=T),
            mnpMPHD = min(MPHD, na.rm=T),
            mxpMPHD = max(MPHD, na.rm=T),
            mpMSHD = mean(MSHD, na.rm=T),
            mnpMSHD = min(MSHD, na.rm=T),
            mxpMSHD = max(MSHD, na.rm=T),
            mpClnD = mean(ClnD, na.rm=T),
            mnpClnD = min(ClnD, na.rm=T),
            mxpClnD = max(ClnD, na.rm=T),
            
            )


lccc_pc<-df_int3|>
  dplyr::select(FIELD,Year, FarmB, mLCCC, ERROR)|>
  pivot_wider(id_cols =c(FIELD, FarmB,Year), names_from =ERROR, values_from = mLCCC)|>
  mutate(MeasD = ((Measurement -Uncleaned)/Uncleaned)*100,
         HarvD = ((`Harvesting Dynamics` -Uncleaned)/Uncleaned)*100,
         SpatD =  ((`Spatial Outliers` -Uncleaned)/Uncleaned)*100,
         PosD =  ((Positional -Uncleaned)/Uncleaned)*100,
         PHD = ((PH -Uncleaned)/Uncleaned)*100,
         HSD = ((HS -Uncleaned)/Uncleaned)*100,
         PSD = ((PS -Uncleaned)/Uncleaned)*100,
         PHSD = ((PHS -Uncleaned)/Uncleaned)*100,
         MPD = ((MP -Uncleaned)/Uncleaned)*100,
         MSD = ((MS -Uncleaned)/Uncleaned)*100,
         MPSD = ((MPS -Uncleaned)/Uncleaned)*100,
         MHD = ((MH -Uncleaned)/Uncleaned)*100,
         MPHD = ((MPH -Uncleaned)/Uncleaned)*100,
         MSHD = ((MHS -Uncleaned)/Uncleaned)*100,
         ClnD = ((Cleaned -Uncleaned)/Uncleaned)*100
  )|>
  dplyr::select(FarmB, MeasD, HarvD,SpatD , PosD,PHD ,HSD, PSD,PHSD, MPD, MSD, MPSD,MHD,MPHD,MSHD, ClnD)|>
  summarise(mpMeasD = mean(MeasD, na.rm=T),
            mnpMeasD = min(MeasD, na.rm=T),
            mxpMeasD = max(MeasD, na.rm=T),
            mpHarvD = mean(HarvD, na.rm=T),
            mnpHarvD = min(HarvD, na.rm=T),
            mxpHarvD = max(HarvD, na.rm=T),
            mpSpatD = mean(SpatD, na.rm=T),
            mnpSpatD = min(SpatD, na.rm=T),
            mxpSpatD = max(SpatD, na.rm=T),
            mpPosD = mean(PosD, na.rm=T),
            mnpPosD = min(PosD, na.rm=T),
            mxpPosD = max(PosD, na.rm=T),
            mpPHD = mean(PHD, na.rm=T),
            mnpPHD = min(PHD, na.rm=T),
            mxpPHD = max(PHD, na.rm=T),
            mpHSD = mean(HSD, na.rm=T),
            mnpHSD = min(HSD, na.rm=T),
            mxpHSD = max(HSD, na.rm=T),
            mpPSD = mean(PSD, na.rm=T),
            mnpPSD = min(PSD, na.rm=T),
            mxpPSD = max(PSD, na.rm=T),
            mpPHSD = mean(PHSD, na.rm=T),
            mnpPHSD = min(PHSD, na.rm=T),
            mxpPHSD = max(PHSD, na.rm=T),
            mpMPD = mean(MPD, na.rm=T),
            mnpMPD = min(MPD, na.rm=T),
            mxpMPD = max(MPD, na.rm=T),
            mpMSD = mean(MSD, na.rm=T),
            mnpMSD = min(MSD, na.rm=T),
            mxpMSD = max(MSD, na.rm=T),
            mpMPSD = mean(MPSD, na.rm=T),
            mnpMPSD = min(MPSD, na.rm=T),
            mxpMPSD = max(MPSD, na.rm=T),
            mpMHD = mean(MHD, na.rm=T),
            mnpMHD = min(MHD, na.rm=T),
            mxpMHD = max(MHD, na.rm=T),
            mpMPHD = mean(MPHD, na.rm=T),
            mnpMPHD = min(MPHD, na.rm=T),
            mxpMPHD = max(MPHD, na.rm=T),
            mpMSHD = mean(MSHD, na.rm=T),
            mnpMSHD = min(MSHD, na.rm=T),
            mxpMSHD = max(MSHD, na.rm=T),
            mpClnD = mean(ClnD, na.rm=T),
            mnpClnD = min(ClnD, na.rm=T),
            mxpClnD = max(ClnD, na.rm=T),
            
  )


lccc_pc  
  
lccc_pc[,16:30]  

lccc_pc[,31:45]  


ars_pc<-df_int3|>
  dplyr::select(FIELD,Year, FarmB, mARS, ERROR)|>
  pivot_wider(id_cols =c(FIELD, FarmB,Year), names_from =ERROR, values_from = mARS)|>
  mutate(MeasD = ((Measurement -Uncleaned)/Uncleaned)*100,
         HarvD = ((`Harvesting Dynamics` -Uncleaned)/Uncleaned)*100,
         SpatD =  ((`Spatial Outliers` -Uncleaned)/Uncleaned)*100,
         PosD =  ((Positional -Uncleaned)/Uncleaned)*100,
         PHD = ((PH -Uncleaned)/Uncleaned)*100,
         HSD = ((HS -Uncleaned)/Uncleaned)*100,
         PSD = ((PS -Uncleaned)/Uncleaned)*100,
         PHSD = ((PHS -Uncleaned)/Uncleaned)*100,
         MPD = ((MP -Uncleaned)/Uncleaned)*100,
         MSD = ((MS -Uncleaned)/Uncleaned)*100,
         MPSD = ((MPS -Uncleaned)/Uncleaned)*100,
         MHD = ((MH -Uncleaned)/Uncleaned)*100,
         MPHD = ((MPH -Uncleaned)/Uncleaned)*100,
         MSHD = ((MHS -Uncleaned)/Uncleaned)*100,
         ClnD = ((Cleaned -Uncleaned)/Uncleaned)*100
  )|>
  dplyr::select(FarmB, MeasD, HarvD,SpatD , PosD,PHD ,HSD, PSD,PHSD, MPD, MSD, MPSD,MHD,MPHD,MSHD, ClnD)|>
  summarise(mpMeasD = mean(MeasD, na.rm=T),
            mnpMeasD = min(MeasD, na.rm=T),
            mxpMeasD = max(MeasD, na.rm=T),
            mpHarvD = mean(HarvD, na.rm=T),
            mnpHarvD = min(HarvD, na.rm=T),
            mxpHarvD = max(HarvD, na.rm=T),
            mpSpatD = mean(SpatD, na.rm=T),
            mnpSpatD = min(SpatD, na.rm=T),
            mxpSpatD = max(SpatD, na.rm=T),
            mpPosD = mean(PosD, na.rm=T),
            mnpPosD = min(PosD, na.rm=T),
            mxpPosD = max(PosD, na.rm=T),
            mpPHD = mean(PHD, na.rm=T),
            mnpPHD = min(PHD, na.rm=T),
            mxpPHD = max(PHD, na.rm=T),
            mpHSD = mean(HSD, na.rm=T),
            mnpHSD = min(HSD, na.rm=T),
            mxpHSD = max(HSD, na.rm=T),
            mpPSD = mean(PSD, na.rm=T),
            mnpPSD = min(PSD, na.rm=T),
            mxpPSD = max(PSD, na.rm=T),
            mpPHSD = mean(PHSD, na.rm=T),
            mnpPHSD = min(PHSD, na.rm=T),
            mxpPHSD = max(PHSD, na.rm=T),
            mpMPD = mean(MPD, na.rm=T),
            mnpMPD = min(MPD, na.rm=T),
            mxpMPD = max(MPD, na.rm=T),
            mpMSD = mean(MSD, na.rm=T),
            mnpMSD = min(MSD, na.rm=T),
            mxpMSD = max(MSD, na.rm=T),
            mpMPSD = mean(MPSD, na.rm=T),
            mnpMPSD = min(MPSD, na.rm=T),
            mxpMPSD = max(MPSD, na.rm=T),
            mpMHD = mean(MHD, na.rm=T),
            mnpMHD = min(MHD, na.rm=T),
            mxpMHD = max(MHD, na.rm=T),
            mpMPHD = mean(MPHD, na.rm=T),
            mnpMPHD = min(MPHD, na.rm=T),
            mxpMPHD = max(MPHD, na.rm=T),
            mpMSHD = mean(MSHD, na.rm=T),
            mnpMSHD = min(MSHD, na.rm=T),
            mxpMSHD = max(MSHD, na.rm=T),
            mpClnD = mean(ClnD, na.rm=T),
            mnpClnD = min(ClnD, na.rm=T),
            mxpClnD = max(ClnD, na.rm=T),
            
  )


ars_pc|>
  dplyr::select(FarmB, FIELD, MeasD, HarvD,SpatD , PosD,PHD ,HSD, PSD,PHSD, MPD, MSD, MPSD,MHD,MPHD,MSHD, ClnD)|>
  arrange(MeasD)

ars_pc[,16:30]
ars_pc[,31:45]


df_int3|>
  dplyr::select(FIELD,Year, FarmB, mRMSE,mLCCC,mARS, ERROR)|>
  filter(ERROR %in% c("Uncleaned","Cleaned"))|>
  group_by(ERROR)|>
  summarise(RMSE = mean(mRMSE),
          LCCC = mean(mLCCC),
          ARS = mean(mARS)
          )
 



# Figure 10 ---

g20.df<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/gnarda2020_v6.csv")

lcol<-c("#c27c34", "white","white","white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "#249e89")

g20.df2<-g20.df|>
  mutate(ARS = 1 -(RMSE/SD))|>
  mutate(ERROR2 = recode(ERROR,
                         "Uncleaned" = "Raw",
                         "Cleaned"= "Clean",
                         "Measurement"="Measurement (M)",
                         "Harvesting Dynamics" = "Harvesting (H)",
                         "Spatial Outliers" = "Spatial (S)",
                         "Positional" = "Positional (P)",
                         .deafult = ERROR))|>
  mutate(ERROR2 = factor(ERROR2, levels=erf))|>
  group_by(FIELD, ERROR2)|>
  summarise(mRMSE = mean(RMSE),
            mLCCC = mean(LCCC),
            mARS =mean(ARS))

fig10<-dunnsbp(g20.df2,"mRMSE","ERROR2")

fig10<-fig10+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y=bquote('RMSE (t' ~ha^-1*")"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18))


ggsave("Fig10.png", 
       plot= fig10,
       width = 3600,
       height = 3600,
       unit="px",
       dpi=500)


fig11<-dunnsbp(g20.df2,"mLCCC","ERROR2")

fig11<-fig11+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="LCCC")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18))


ggsave("Fig11.png", 
       plot= fig11,
       width = 3600,
       height = 3600,
       unit="px",
       dpi=500)


fig12<-dunnsbp(g20.df2,"mARS","ERROR2")

fig12<-fig12+
  scale_fill_manual(values=lcol)+
  labs(x="Error Type",y="ARS")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  scale_x_discrete(limits=rev)+
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title = element_text(size=18))


ggsave("Fig12.png", 
       plot= fig12,
       width = 3600,
       height = 3600,
       unit="px",
       dpi=500)





# Figure 13 ----

grascdir<-dir("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/Yield Map Cluster", pattern = ".tif", full.names=T)

f13.df<-read.csv("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/Data/Gnarda_Clustering_Data.csv")

yhfb<-st_read("/Volumes/dmp/A-J/Digital_Edge_RES6520-EASTOJ-SE21023/Project Participants/Young Hill Farms/Data/boundaries/boundaries.gpkg", quiet=T)


pbnd<-yhfb|>
  filter(Field=="Gnarda 1")

bm <- rast(read_osm(pbnd, ext = 1.2, type = "esri-imagery"))

g1r<-rast(grascdir[2])
g1rc<-g1r[[2]]

g1c<-rast(grascdir[1])
g1cc<-g1c[[2]]

fig13a1<-ggplot()+
  geom_spatraster_rgb(data = bm)+
  geom_spatraster(data = as.factor(g1rc))+
  scale_fill_manual(values=c("#EDEF5C","#17A77E" ,"#255668" ), 
                    labels=c("Low","Medium","High"),
                    na.value="transparent",
                    na.translate=FALSE)+
  geom_sf(data=pbnd, fill=NA, colour="red")+
  theme_bw()+
  coord_sf(datum=32750)+
  theme(legend.position="none")+
  annotation_scale()+
  annotation_north_arrow(
    which_north = "true",
    style=north_arrow_minimal(),
    pad_x = unit(-0.15, "in"), pad_y = unit(0.25, "in"))+
  labs(x="Easting (m)", y="Northing (m)", fill="Yield\nZone")

fig13a2<-ggplot()+
  geom_spatraster_rgb(data = bm)+
  geom_spatraster(data = as.factor(g1cc))+
  scale_fill_manual(values=c("#EDEF5C","#17A77E" ,"#255668" ), 
                    labels=c("Low","Medium","High"),
                    na.value="transparent",
                    na.translate=FALSE)+
  geom_sf(data=pbnd, fill=NA, colour="red")+
  theme_bw()+
  coord_sf(datum=32750)+
  annotation_scale()+
  annotation_north_arrow(
    which_north = "true",
    style=north_arrow_minimal(),
    pad_x = unit(-0.15, "in"), pad_y = unit(0.25, "in"))+
  labs(x="Easting (m)", y="Northing (m)", fill="Yield\nZone")
 
 # scale_x_continuous(breaks=seq(122.530,122.60, 0.025), limits=c(122.53,122.6))+
 # scale_x_continuous(breaks=seq(1014500,1015500,500))+
 

fig13b1<-f13.df|>
  filter(Field=="Gnarda 1",
         Status=="Raw")|>
  #mutate(Status=factor(Status, levels=c("Raw", "Clean")))|>
  ggplot(aes(x=as.factor(Cluster),y=Yld, fill=as.factor(Cluster)))+
  scale_fill_manual(values=c("#EDEF5C","#17A77E" ,"#255668" ))+
  #facet_wrap(~Status)+
  geom_boxplot()+
  labs(x="Yield Zones", y=' Yield (tons' ~ha^-1*")")+
  theme_bw()+
  scale_x_discrete(labels=c("Low","Medium","High"))+
  theme(legend.position="none")

fig13b2<-f13.df|>
  filter(Field=="Gnarda 1",
         Status=="Clean")|>
  #mutate(Status=factor(Status, levels=c("Raw", "Clean")))|>
  ggplot(aes(x=as.factor(Cluster),y=Yld, fill=as.factor(Cluster)))+
  scale_fill_manual(values=c("#EDEF5C","#17A77E" ,"#255668" ))+
 # facet_wrap(~Status)+
  geom_boxplot()+
  labs(x="Yield Zones", y=' Yield (tons' ~ha^-1*")")+
  theme_bw()+
  scale_x_discrete(labels=c("Low","Medium","High"))+
  theme(legend.position="none")

fig13a<-fig13a1|fig13a2
fig13b<-fig13b1|fig13b2

fig13<-fig13a/fig13b+
  plot_layout(heights=c(2,1))


ggsave("Fig13.png", 
       plot=fig13,
       width = 7500,
       height = 5250,
       unit="px",
       dpi=600)
