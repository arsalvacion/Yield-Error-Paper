###################################################
# Fitting Variogram Model to Raw and Clean Data  #
##################################################

library(sf)
library(automap)
library(tidyverse)
library(parallel)

setwd("/Users/293869b/Library/CloudStorage/OneDrive-SharedLibraries-Curtin/RES 67114 Agri Analytics Hub - Documents/General/Journal paper development/Yield map error/Version 6/R Code/Tables and Figures/")


to_test <- list(
  "Raw" = "raw",
  "Clean" = c(
    "edge","zyld", "gps", c("spat_hl", "spat_lh"), "swth", "tlag","yld", "vel"
  )
)

### Farm 1 Young Hill ----

yhillsd<-list.files("/Volumes/dmp/A-J/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Young Hill Farms/Data/cleaned/", 
                    pattern=".gpkg", recursive=TRUE, full.names = TRUE)

yhills<-yhillsd|>
  data.frame()|>
  filter(str_detect(yhillsd, pattern = "Marked-Yld"))


x<-yhills

rdf<-data.frame(NULL)


for (i in 56:length(x[,1])){ 
  ydat<-st_read(x[i,])
  Year<-str_match(x[i,],"cleaned//\\s*(.*?)\\s*/Marked-")[,2]
  Farm<-str_match(x[i,],"Yld_Mass_D/\\s*(.*?)\\s*.gpkg")[,2]
  fld<-unique(ydat$Field)
  
  
  
  for (j in 1:length(to_test)){
    ydat2<-ydat|>
      filter(!cleaned %in% to_test[[j]])|>
      mutate(ERROR =names(to_test[j]))|>
      mutate(FARM = Farm )|>
      mutate (YEAR = Year)
    
    
    
    flds<- split(ydat2, ydat2$Field)
    
    
    cl <- makeCluster(1+ length(flds))
    clusterEvalQ(cl, library("automap"))
    clusterEvalQ(cl, library("sf"))
    
    cvr<-list()
    
    cvr <- clusterApply(
      cl,
      1:length(flds),
      flds,
      fun = \(i, flds) {
        dat<-flds[[i]]
        
        ## Start timer
        start <- Sys.time()
        
        ak<-autoKrige(Yld_Mass_D~1, dat, nmax=25)
        
        finish <- Sys.time()  
        
        list(
          FIELD = dat$Field[1],
          FARM = dat$FARM[1],
          YEAR = dat$YEAR[1],
          prdct = dat$Product[1],
          ERROR = dat$ERROR[1],
          vmod = as.character(ak$var_model$model[2]),
          nugget = ak$var_model$psill[1],
          sill = ak$var_model$psill[2],
          range = ak$var_model$range[2]
          
        )
        
      })
    stopCluster(cl)
    prdf <- data.frame(matrix(unlist(cvr), nrow=length(cvr), byrow=TRUE))
    colnames(prdf)<-c("FIELD","FARM","YEAR","PRODUCT","ERROR","VMOD","NUGGET","SILL","VRANGE")
    rdf<-rbind(rdf,prdf)
  }
}

write.csv(rdf, "yhills_variograms.csv", row.names=F)

### Farm 2 Marshall ----

marshalld<-list.files("/Volumes/dmp/0-9/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Marshall/Data/cleaned/", 
                      pattern=".gpkg", recursive=TRUE, full.names = TRUE)

marshall<-marshalld|>
  data.frame()|>
  filter(str_detect(marshalld, pattern = "Marked-Yld"))


x<-marshall

rdf<-data.frame(NULL)


for (i in 1:length(x[,1])){ 
  ydat<-st_read(x[i,])
  Year<-str_match(x[i,],"cleaned//\\s*(.*?)\\s*/Marked-")[,2]
  Farm<-str_match(x[i,],"Yld_Mass_D/\\s*(.*?)\\s*.gpkg")[,2]
  fld<-unique(ydat$Field)
  
  
  
  for (j in 1:length(to_test)){
    ydat2<-ydat|>
      filter(!cleaned %in% to_test[[j]])|>
      mutate(ERROR =names(to_test[j]))
    
    
    
    flds<- split(ydat2, ydat2$Field)
    
    
    
    cl <- makeCluster(1+ length(flds))
    clusterEvalQ(cl, library("automap"))
    clusterEvalQ(cl, library("sf"))
    
    cvr<-list()
    
    cvr <- clusterApply(
      cl,
      1:length(flds),
      flds,
      fun = \(i, flds) {
        dat<-flds[[i]]
        
        ## Start timer
        start <- Sys.time()
        
        ak<-autoKrige(Yld_Mass_D~1, dat, nmax=25)
        
        finish <- Sys.time()  
        
        list(
          FIELD = dat$Field[1],
          prdct = dat$Product[1],
          ERROR = dat$ERROR[1],
          vmod = as.character(ak$var_model$model[2]),
          nugget = ak$var_model$psill[1],
          sill = ak$var_model$psill[2],
          range = ak$var_model$range[2]
          
        )
        
      })
    stopCluster(cl)
    prdf <- data.frame(matrix(unlist(cvr), nrow=length(cvr), byrow=TRUE))
    colnames(prdf)<-c("FIELD","PRODUCT","ERROR","VMOD","NUGGET","SILL","VRANGE")
    rdf<-rbind(rdf,prdf)
  }
}

write.csv(rdf, "marshall_variograms.csv", row.names=F)


### Farm 3 Kirnan ----

kirnand<-dir("/Volumes/dmp/0-9/Agri_Analytics_Hub-EASTOJ-SE22510/Project Participants/Kirnan/Data/cleaned/",
           pattern=".gpkg", recursive=TRUE, full.names = TRUE)

kirnan<-kirnand|>
  data.frame()|>
  filter(str_detect(kirnand, pattern = "Marked-Yld"))


x<-kirnan

rdf<-data.frame(NULL)

##  7-27 for cropping period 2018-2022

for (i in 7:27){ 
  ydat<-st_read(x[i,])
  Year<-str_match(x[i,],"cleaned//\\s*(.*?)\\s*/Marked-")[,2]
  Farm<-str_match(x[i,],"Yld_Mass_D/\\s*(.*?)\\s*.gpkg")[,2]
  fld<-unique(ydat$Field)
  
  
  
  for (j in 1:length(to_test)){
    ydat2<-ydat|>
      filter(!cleaned %in% to_test[[j]])|>
      mutate(ERROR =names(to_test[j]))
    
    
    
    flds<- split(ydat2, ydat2$Field)
    
    
    
    cl <- makeCluster(1+ length(flds))
    clusterEvalQ(cl, library("automap"))
    clusterEvalQ(cl, library("sf"))
    
    cvr<-list()
    
    cvr <- clusterApply(
      cl,
      1:length(flds),
      flds,
      fun = \(i, flds) {
        dat<-flds[[i]]
        
        ## Start timer
        start <- Sys.time()
        
        ak<-autoKrige(Yld_Mass_D~1, dat, nmax=25)
        
        finish <- Sys.time()  
        
        list(
          FIELD = dat$Field[1],
          prdct = dat$Product[1],
          ERROR = dat$ERROR[1],
          vmod = as.character(ak$var_model$model[2]),
          nugget = ak$var_model$psill[1],
          sill = ak$var_model$psill[2],
          range = ak$var_model$range[2]
          
        )
        
      })
    stopCluster(cl)
    prdf <- data.frame(matrix(unlist(cvr), nrow=length(cvr), byrow=TRUE))
    colnames(prdf)<-c("FIELD","PRODUCT","ERROR","VMOD","NUGGET","SILL","VRANGE")
    rdf<-rbind(rdf,prdf)
  }
}

write.csv(rdf, "kirnan_variograms.csv", row.names=F)


### Farm 4 Neenwest ----

neend<-dir("/Volumes/dmp/0-9/Drought_hub-EASTOJ-SE21104/Project Participants/Neenwest/Data/cleaned/",
             pattern=".gpkg", recursive=TRUE, full.names = TRUE)

neen<-neend|>
  data.frame()|>
  filter(stringr::str_detect(neend, pattern = "Marked-Yld"))

x<-neen

rdf<-data.frame(NULL)

for (i in 10:22)){
  ydat<-st_read(x[i,])
  Year<-str_match(x[i,],"cleaned//\\s*(.*?)\\s*/Marked-")[,2]
  Farm<-str_match(x[i,],"Yld_Mass_D/\\s*(.*?)\\s*.gpkg")[,2]
  fld<-unique(ydat$Field)
  
  
  
  for (j in 1:length(to_test)){
    ydat2<-ydat|>
      filter(!cleaned %in% to_test[[j]])|>
      mutate(ERROR =names(to_test[j]))
    
    
  
  flds<- split(ydat2, ydat2$Field)
  
  
  
  cl <- makeCluster(1+ length(flds))
  clusterEvalQ(cl, library("automap"))
  clusterEvalQ(cl, library("sf"))
  
  cvr<-list()
  
  cvr <- clusterApply(
    cl,
    1:length(flds),
    flds,
    fun = \(i, flds) {
      dat<-flds[[i]]

      ## Start timer
      start <- Sys.time()
      
      ak<-autoKrige(Yld_Mass_D~1, dat, nmax=25)
      
      finish <- Sys.time()  
      
      list(
        FIELD = dat$Field[1],
        prdct = dat$Product[1],
        ERROR = dat$ERROR[1],
        vmod = as.character(ak$var_model$model[2]),
        nugget = ak$var_model$psill[1],
        sill = ak$var_model$psill[2],
        range = ak$var_model$range[2]
        
      )
      
    })
  stopCluster(cl)
  prdf <- data.frame(matrix(unlist(cvr), nrow=length(cvr), byrow=TRUE))
  colnames(prdf)<-c("FIELD","PRODUCT","ERROR","VMOD","NUGGET","SILL","VRANGE")
  rdf<-rbind(rdf,prdf)
}
}

write.csv(rdf, "neen_variograms.csv", row.names=F)
  
  
  
  

