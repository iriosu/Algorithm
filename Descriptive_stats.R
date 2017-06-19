# Libraries ---------------------------------------------------------------
library(stargazer, quietly = TRUE)
library(lmtest, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr)
library(reshape2)
library(ggfortify)
library(data.table)
library(RMySQL)
library(plotly)
library(latex2exp)
library(truncnorm)

# Directory ---------------------------------------------------------------

#root_dir = 'C:/'
root_dir = '/home/tlarroucau/'
drop_dir = 'Dropbox/PaperDEMRE/'
setwd(paste(c(root_dir,drop_dir), collapse = ""))


# Reading files -----------------------------------------------------------
#carreras_requisitos
car_req_13 = read.csv("Datos/PAUC 2013/Seleccion/carreras_requisitos.csv", sep = ";", header=TRUE)
car_req_14 = read.csv("Datos/PAUC 2014/Seleccion/carreras_requisitos.csv", sep = ";", header=TRUE)
car_req_15 = read.csv("Datos/PAUC 2015/Seleccion/carreras_requisitos.csv", sep = ";", header=TRUE)
car_req_16 = read.csv("Datos/PAUC 2016/Seleccion/carreras.csv", sep = ";", header=TRUE)

#Rename and create some variables
car_req_list = list(car_req_13,car_req_14,car_req_15,car_req_16)

for(i in 1:4){
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'CODIGO'] = 'codigo_carrera' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'CUPOS_SUPERNUM_BEA'] = 'vacantes_bea' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'CDP_VACANTES_ESPECIALES'] = 'vacantes_bea' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'VACANTES_1SEM'] = 'vacantes_1sem' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'VACANTES_2SEM'] = 'vacantes_2sem' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'SC_1S'] = 'sobrecupo_1sem' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'SC_2S'] = 'sobrecupo_2sem' 
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'UNIVERSIDAD'] = 'nombre_universidad'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'CODIGO'] = 'codigo_carrera'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'X._NOTAS'] = 'pct_nem'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'X._RANKING'] = 'pct_ranking'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'X._RANK'] = 'pct_ranking'
  names(car_req_list[[i]])[names(car_req_list[[i]]) == 'X._Ranking'] = 'pct_ranking'
}

#Other program variables to create
for(i in 1:4){
  car_req_list[[i]]$vacantes_reg = car_req_list[[i]]$vacantes_1sem + car_req_list[[i]]$vacantes_2sem +
    car_req_list[[i]]$sobrecupo_1sem + car_req_list[[i]]$sobrecupo_2sem 
  car_req_list[[i]]$vacantes_tot =  car_req_list[[i]]$vacantes_reg + car_req_list[[i]]$vacantes_bea
  car_req_list[[i]]$code_uni = floor(car_req_list[[i]]$codigo_carrera/1000)
} 

#Edit names of universities
#Read unis abbrev
uni_abrev = read.csv("Datos/PAUC 2015/Otros datos/abbr_universidades.txt",sep = ";", header=FALSE)
names(uni_abrev)[names(uni_abrev) == 'V1'] = 'code_uni' 
names(uni_abrev)[names(uni_abrev) == 'V2'] = 'uni_abrev' 

#Merge uni names
for(i in 1:4){
  car_req_list[[i]] = merge(x = uni_abrev, y = car_req_list[[i]], by = "code_uni", all = TRUE)  
}


#asignacion_obtenida
reg_sec_uni_13 = read.csv("Datos/PAUC 2013/Output/asignacion_obtenida_reg_secuencial_university_2013.csv", sep = ";", header=TRUE)
bea_sec_uni_13 = read.csv("Datos/PAUC 2013/Output/asignacion_obtenida_bea_secuencial_university_2013.csv", sep = ";", header=TRUE)
reg_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_secuencial_university_2014.csv", sep = ";", header=TRUE)
bea_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_secuencial_university_2014.csv", sep = ";", header=TRUE)
reg_au_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_unica_student_2015.csv", sep = ";", header=TRUE)
bea_au_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_unica_student_2015.csv", sep = ";", header=TRUE)
reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_unica_student_2016.csv", sep = ";", header=TRUE)
bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_unica_student_2016.csv", sep = ";", header=TRUE)

reg_list = list(reg_sec_uni_13,reg_sec_uni_14,reg_au_stu_15,reg_au_stu_16)
bea_list = list(bea_sec_uni_13,bea_sec_uni_14,bea_au_stu_15,bea_au_stu_16)

#puntajes
puntajes_13 = read.csv("Datos/PAUC 2013/Seleccion/puntajes.csv", sep = ";", header=TRUE)
#These ones do not work because file is a txt with fixed size
#puntajes_14 = read.csv("Datos/PAUC 2014/Seleccion/puntajes.csv", sep = ";", header=TRUE)
#puntajes_15 = read.csv("Datos/PAUC 2015/Seleccion/puntajes.csv", sep = ";", header=TRUE)
#puntajes_16 = read.csv("Datos/PAUC 2016/Seleccion/puntajes.csv", sep = ";", header=TRUE)
puntajes_14 = data.frame("id_alumno" = c(1,2,3,4))
puntajes_15 = data.frame("id_alumno" = c(1,2,3,4))
puntajes_16 = data.frame("id_alumno" = c(1,2,3,4))

puntajes_list = list(puntajes_13, puntajes_14, puntajes_15, puntajes_16)

# Aggregate stats ---------------------------------------------------------
agg_stats = list()
for(i in 1:4){
  n_candidates = nrow(unique(puntajes_list[[i]]['id_alumno']))
  n_programs = nrow(unique(car_req_list[[i]]['codigo_carrera']))
  n_unis = nrow(unique(car_req_list[[i]]['code_uni']))
  vac_reg = sum(car_req_list[[i]][,'vacantes_reg'])
  vac_bea = sum(car_req_list[[i]][,'vacantes_bea'])
  agg_stats[[i]] = data.frame("Candidates" = n_candidates, "Programs" = n_programs,
                              "Universities" = n_unis, "Regular_Vacancies" = vac_reg,
                              "BEA_Vacancies" = vac_bea) 
}
#Getting the column names of the data frame, with spaces
agg_stats_edit_colnames = c("Candidates","Programs","Universities",
                      "Regular Vacancies","BEA Vacancies")

# Latex tables ------------------------------------------------------------
#Aggregate statistics from the Admission Process
table_latex = file("Paper/in-prep/tables/agg_stats_13_16.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Aggregate Statistics 2013-2016}", 
                 "\\label{tab: agg_stats_13_16}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccc}",
                 "\\toprule", 
                 "\\toprule")
table_body = c("   & 2013 & 2014 & 2015 & 2016 \\\\" ," \\midrule")
space = '\\\\[0pt]'
for(i in 1:length(names(agg_stats[[1]]))){
  row = paste(agg_stats_edit_colnames[i],"&", agg_stats[[1]][i],"&",agg_stats[[2]][i],'&',agg_stats[[3]][i],'&',agg_stats[[4]][i],space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)


