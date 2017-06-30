
#### Initial variables ##################################
wd <- "~/paperAlgoritmoAsignaciones"
data_path <- "data"
#### Dependencies #######################################
setwd(wd)
library(readxl)
source("lib/utils.r")
#### Load data ##########################################
bruta_general_carreras <- read_excel(paste(wd,data_path,"ProjectMiFuturo.xlsx",sep="/"))
bruta_ficha_carreras <- read_excel(paste(wd,data_path,"Mifuturopeluo.xlsx",sep="/"), col_names = FALSE)
#### Normalize general_carreras #########################
general_carreras <- bruta_general_carreras
## Quality data verification by column
Ues = data.frame(nombre_largo=unique(bruta_general_carreras$Universidad)) #Correcto todas las Ues #Pendiente hacer match a codigos DEMRE y SIES
## NAs
general_carreras$Duracion[general_carreras$Duracion=="No"]=NA
general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho","Ingreso 4to anho","Arancel anual 2016")] = lapply(general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho","Ingreso 4to anho","Arancel anual 2016")],function(x){gsub("s/i",NA,x)})
## Text to data (multidimensional,call and units)
general_carreras$Duracion=as.numeric(gsub(" años","",general_carreras$Duracion))
general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho","Ingreso 4to anho","Arancel anual 2016")] = lapply(general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho","Ingreso 4to anho","Arancel anual 2016")],function(x){gsub("%","",x)})
general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho")] = lapply(general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho")],function(x){gsub(",",".",x)})
general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho")] = lapply(general_carreras[,c("% de alum colegios Subv","Retencion 1er anho","Duracion real","empleabilidad primer anho")],as.numeric)

a = strsplit(gsub("\n","",gsub("\"","",as.character(general_carreras[,c("Ingreso 4to anho")]))),",")[[1]]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%% TOFIX: Ver porque aranceles esta fallando
#general_carreras[,c("Arancel anual 2016")] = lapply(general_carreras[,c("Arancel anual 2016")],as.character)
#general_carreras[,c("Arancel anual 2016")] = lapply(general_carreras[,c("Arancel anual 2016")],function(x){gsub("$","",x)})
#general_carreras[,c("Arancel anual 2016")] = lapply(general_carreras[,c("Arancel anual 2016")],function(x){gsub(".","",x)})
#general_carreras[,c("Arancel anual 2016")] = lapply(general_carreras[,c("Arancel anual 2016")],as.numeric)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Delete unnecessary colmn and rename variables
general_carreras = general_carreras[,!(names(general_carreras) %in% c("New Content 1"))]
names(general_carreras) = c("nombre_universidad","duracion[anhos]","nombre_carrera","alum_colegios_subv[%]","retencion_1er_anho[%]","duracion_real[anhos]","empleabilidad_primer_anho[%]","ingreso_4to_anho[$]","arancel_anual_2016[$]")
#### Normalize ficha_carreras ###########################


