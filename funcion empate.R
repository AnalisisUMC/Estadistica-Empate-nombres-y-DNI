rm(list=ls())

library(tidyverse)
library(rio)

setwd('D:/pedido Luis descubrir como funciona RIO en diferentes BD/cambiar script empate')

################################################################################
######### PRIMERA ETAPA ########################################################
################################################################################

empate1=function(bd1, bd2){
  ## Eliminar vacios y duplicados en las BD
  bd1=data.frame(bd1)
  colnames(bd1)=c('id_1','pat_1','mat_1','nom1_1','nom2_1','dni','DRE_1')
  bd1=bd1%>%mutate(id_1   = if_else(id_1   %in% c('','\n'), NA, id_1),
                   pat_1  = if_else(pat_1  %in% c('','\n'), NA, pat_1),
                   mat_1  = if_else(mat_1  %in% c('','\n'), NA, mat_1),
                   nom1_1 = if_else(nom1_1 %in% c('','\n'), NA, nom1_1),
                   nom2_1 = if_else(nom2_1 %in% c('','\n'), NA, nom2_1),
                   dni    = if_else(dni    %in% c('','\n'), NA, dni),
                   DRE_1  = if_else(DRE_1  %in% c('','\n'), NA, DRE_1))
  bd1=bd1[!is.na(bd1$dni),]
  bd1=bd1[!duplicated(bd1$dni),]
  
  bd2=data.frame(bd2)
  colnames(bd2)=c('id_2','pat_2','mat_2','nom1_2','nom2_2','dni','DRE_2')
  bd2=bd2%>%mutate(id_2   = if_else(id_2   %in% c('','\n'), NA, id_2),
                   pat_2  = if_else(pat_2  %in% c('','\n'), NA, pat_2),
                   mat_2  = if_else(mat_2  %in% c('','\n'), NA, mat_2),
                   nom1_2 = if_else(nom1_2 %in% c('','\n'), NA, nom1_2),
                   nom2_2 = if_else(nom2_2 %in% c('','\n'), NA, nom2_2),
                   dni    = if_else(dni    %in% c('','\n'), NA, dni),
                   DRE_2  = if_else(DRE_2  %in% c('','\n'), NA, DRE_2))
  bd2=bd2[!is.na(bd2$dni),]
  bd2=bd2[!duplicated(bd2$dni),]
  
  ## Eliminar NA de los nombres
  bd1$pat_1  = ifelse(is.na(bd1$pat_1),  "", bd1$pat_1)
  bd2$pat_2  = ifelse(is.na(bd2$pat_2),  "", bd2$pat_2)
  bd1$mat_1  = ifelse(is.na(bd1$mat_1),  "", bd1$mat_1)
  bd2$mat_2  = ifelse(is.na(bd2$mat_2),  "", bd2$mat_2)
  bd1$nom1_1 = ifelse(is.na(bd1$nom1_1), "", bd1$nom1_1)
  bd2$nom1_2 = ifelse(is.na(bd2$nom1_2), "", bd2$nom1_2)
  bd1$nom2_1 = ifelse(is.na(bd1$nom2_1), "", bd1$nom2_1)
  bd2$nom2_2 = ifelse(is.na(bd2$nom2_2), "", bd2$nom2_2)
  
  ## Unir las BD
  bd=merge(bd1, bd2, by='dni')
  
  ## QUitar tildes de las variables a empatar
  bd$pat_1  = chartr("ÁÉÍÓÚ", "AEIOU", toupper(bd$pat_1))
  bd$mat_1  = chartr("ÁÉÍÓÚ", "AEIOU", toupper(bd$mat_1))
  bd$nom1_1 = chartr("ÁÉÍÓÚ", "AEIOU", toupper(bd$nom1_1))
  bd$nom2_1 = chartr("ÁÉÍÓÚ", "AEIOU", toupper(bd$nom2_1))
  bd$pat_2  = chartr("ÁÉÍÓÚ", "AEIOU", toupper(bd$pat_2))
  bd$mat_2  = chartr("ÁÉÍÓÚ", "AEIOU", toupper(bd$mat_2))
  bd$nom1_2 = chartr("ÁÉÍÓÚ", "AEIOU", toupper(bd$nom1_2))
  bd$nom2_2 = chartr("ÁÉÍÓÚ", "AEIOU", toupper(bd$nom2_2))
  
  ## QUitar tildes volteadas de las variables a empatar
  bd$pat_1  = chartr("ÀÈÌÒÙ", "AEIOU", toupper(bd$pat_1))
  bd$mat_1  = chartr("ÀÈÌÒÙ", "AEIOU", toupper(bd$mat_1))
  bd$nom1_1 = chartr("ÀÈÌÒÙ", "AEIOU", toupper(bd$nom1_1))
  bd$nom2_1 = chartr("ÀÈÌÒÙ", "AEIOU", toupper(bd$nom2_1))
  bd$pat_2  = chartr("ÀÈÌÒÙ", "AEIOU", toupper(bd$pat_2))
  bd$mat_2  = chartr("ÀÈÌÒÙ", "AEIOU", toupper(bd$mat_2))
  bd$nom1_2 = chartr("ÀÈÌÒÙ", "AEIOU", toupper(bd$nom1_2))
  bd$nom2_2 = chartr("ÀÈÌÒÙ", "AEIOU", toupper(bd$nom2_2))
  
  ## Eliminar espacios en las variables a empatar
  bd$pat_1  = str_trim(bd$pat_1,  side=c('both','left','right'))
  bd$mat_1  = str_trim(bd$mat_1,  side=c('both','left','right'))
  bd$nom1_1 = str_trim(bd$nom1_1, side=c('both','left','right'))
  bd$nom2_1 = str_trim(bd$nom2_1, side=c('both','left','right'))
  bd$pat_2  = str_trim(bd$pat_2,  side=c('both','left','right'))
  bd$mat_2  = str_trim(bd$mat_2,  side=c('both','left','right'))
  bd$nom1_2 = str_trim(bd$nom1_2, side=c('both','left','right'))
  bd$nom2_2 = str_trim(bd$nom2_2, side=c('both','left','right'))
  
  ## Concatenar los nombres
  bd$nom_comp1 = paste(bd$pat_1, bd$mat_1, bd$nom1_1, bd$nom2_1)
  bd$nom_comp2 = paste(bd$pat_2, bd$mat_2, bd$nom1_2, bd$nom2_2)
  
  bd$nom_comp1 = str_trim(bd$nom_comp1, side=c('both','left','right'))
  bd$nom_comp2 = str_trim(bd$nom_comp2, side=c('both','left','right'))
  
  bd$Filtro = ifelse((bd$nom_comp1==bd$nom_comp2) | (bd$pat_1==bd$pat_2) |
                       (bd$mat_1==bd$mat_2) | (bd$pat_1==bd$mat_2) |
                       (bd$mat_1==bd$pat_2) |
                       str_trim(paste(bd$nom1_1,bd$nom2_1),side=c('both','left','right'))==str_trim(paste(bd$nom1_2,bd$nom2_2),side=c('both','left','right')) |
                      (paste(bd$pat_1,bd$mat_1)==paste(bd$pat_2,bd$mat_2)),1,0)
  
  ## Exportar la BD para revisión manual y su actualización
  export(bd,'empate1.xlsx')
  export(bd,'empate1.sav')
}

## Las bases deben tener, en el siguiente orden, los siguientes campos: ID, apellido paterno, apellido materno, primer nombre, segundo nombre,
#dni, cod_DRE. (si los nombres están en una sola variable, crear una columna en blanco que reemplace el lugar del segundo nombre)
