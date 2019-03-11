library(gganimate)
library(tidyverse)
library(sf)
library(brmap)
library(brazilmaps)
library(abjData)
library(abjutils)
library(scales)
library(animation)
library(viridis)
library(sf)

interpolateYears <- function(mypnud, years){
  nvar <- ncol(mypnud)
  varnames <- names(mypnud)
  factorvars <- c(1,3,4,5,nvar);
  for(i in 1:(length(years)-1)){
    #      cat(paste("i = ",i),sep="\n")
    yi <- years[i];
    yj <- years[i+1];
    timespan <- yj - yi;
    for(y in 1:(timespan-1)){
      #        cat(paste("y = ",y),sep="\n")
      interpolatepnud <- mypnud[which(mypnud$ano==yi),];
      for(v in 1:nvar){
        varname <- varnames[v];
        if(v == 2){
          interpolatepnud[,v] <- interpolatepnud[,v]+y          
        }else if(!v %in% factorvars){
          vari <- as.numeric(unlist(mypnud[which(mypnud$ano==yi),v]));
          varj <- as.numeric(unlist(mypnud[which(mypnud$ano==yj),v]));
          varstep <- as.numeric((varj - vari)/(timespan))
          interpolatepnud[,v] <- interpolatepnud[,v]+(y*varstep)
        }
      }
      mypnud <- rbind(mypnud,interpolatepnud)
      rm(interpolatepnud);
      cat(yi+y,sep="\n")
    }
  }    
  return(mypnud)
}

geodata <- read_sf("/home/charles/GitRepos/armagedon/Regionais/2018/geodata/municipios_2010.shp")
#geodata <- brmap::brmap_municipio %>% mutate(municipio = toupper(municipio))
geodata$nome <- toupper(geodata$nome)
geodata$nome[which(geodata$nome == "BELÉM DE SÃO FRANCISCO")] <- "BELÉM DO SÃO FRANCISCO"
geodata$nome[which(geodata$nome == "CAMPO DE SANTANA")] <- "TACIMA"
geodata$nome[which(geodata$nome == "LAGOA DO ITAENGA")] <- "LAGOA DE ITAENGA"
geodata$nome[which(geodata$nome == "SANT' ANA DO LIVRAMENTO")] <- "SANT'ANA DO LIVRAMENTO"
geodata$nome[which(geodata$nome == "SÃO VALÉRIO DA NATIVIDADE")] <- "SÃO VALÉRIO"

mypnud <- pnud_muni
mypnud2 <- interpolateYears(mypnud, c(1991,2000,2010))
mypnud <- mypnud2
save(mypnud2,file="/home/charles/LinkedIn/pnud/interpolatedpnud.RDat")
years <- unique(sort(mypnud$ano))


####
saveGIF({for(year in years){
  demodata <- mypnud %>% filter(ano == year) %>% select(municipio,espvida,ano) %>% mutate(municipio = toupper(municipio))
  
  newmapdf <- geodata %>% 
    left_join(demodata, by = c("nome" = "municipio"))
  map <- ggplot() +
    geom_sf(data = newmapdf, mapping = aes(fill = espvida), lwd = 0) +
    scale_fill_viridis(option="magma",limits = c(50,100)) +
    labs(title = paste("Life xpectancy:",year),
         caption = "Source: PNUD, made available by Abj") +
    theme(
      title = element_text(face = "bold", color = "black"),
      axis.title = element_text(face = "bold", color = "black"),
      panel.grid.major.x = element_blank()
    )
  print(map)
}},movie.name="animation_lifeexpectancy.gif", interval = 0.75)
