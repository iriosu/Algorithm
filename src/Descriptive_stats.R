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
root_dir = '/Users/irios/'
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

#asignacion_obtenida (process run in that year)
reg_sec_uni_13 = read.csv("Datos/PAUC 2013/Output/asignacion_obtenida_reg_secuencial_university_2013.csv", sep = ";", header=TRUE)
bea_sec_uni_13 = read.csv("Datos/PAUC 2013/Output/asignacion_obtenida_bea_secuencial_university_2013.csv", sep = ";", header=TRUE)
reg_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_secuencial_university_2014.csv", sep = ";", header=TRUE)
bea_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_secuencial_university_2014.csv", sep = ";", header=TRUE)
reg_sec_uni_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_secuencial_university_2015.csv", sep = ";", header=TRUE)
bea_sec_uni_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_secuencial_university_2015.csv", sep = ";", header=TRUE)
reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_unica_student_2016.csv", sep = ";", header=TRUE)
bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_unica_student_2016.csv", sep = ";", header=TRUE)


##############################
#### MAIN CHANGE - iriosu ####
##############################
reg_list = list(reg_sec_uni_13,reg_sec_uni_14,reg_sec_uni_15,reg_au_stu_16)
bea_list = list(bea_sec_uni_13,bea_sec_uni_14,bea_sec_uni_15,bea_au_stu_16)

#postulaciones_procesadas
# NOT Following the same assignment than for asignacion_obtenida
#Here we are reading the Unique assignment because applications shouldn't be affected by this
#Becareful here, the file of applications has BEA people in both, reg and bea posts even in the unique assignment 
#So we shouldn't use this for assignement purposes!
post_reg_au_stu_13 = read.csv("Datos/PAUC 2013/Output/postulaciones_procesadas_reg_student_unica_2013.csv", sep = ";", header=TRUE)
post_bea_au_stu_13 = read.csv("Datos/PAUC 2013/Output/postulaciones_procesadas_bea_student_unica_2013.csv", sep = ";", header=TRUE)
post_reg_au_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_reg_student_unica_2014.csv", sep = ";", header=TRUE)
post_bea_au_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_bea_student_unica_2014.csv", sep = ";", header=TRUE)
post_reg_au_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_reg_student_unica_2015.csv", sep = ";", header=TRUE)
post_bea_au_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_bea_student_unica_2015.csv", sep = ";", header=TRUE)
post_reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_reg_student_unica_2016.csv", sep = ";", header=TRUE)
post_bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_bea_student_unica_2016.csv", sep = ";", header=TRUE)

#As we have BEA students in both tracks, we need to keep track of this later
post_reg_list = list(post_reg_au_stu_13,post_reg_au_stu_14,post_reg_au_stu_15,post_reg_au_stu_16)
post_bea_list = list(post_bea_au_stu_13,post_bea_au_stu_14,post_bea_au_stu_15,post_bea_au_stu_16)

#puntajes 
puntajes_13 = read.csv("Datos/PAUC 2013/Seleccion/puntajes_2013.csv", sep = ";", header=TRUE)
puntajes_14 = read.csv("Datos/PAUC 2014/Seleccion/puntajes_2014.csv", sep = ";", header=TRUE)
puntajes_15 = read.csv("Datos/PAUC 2015/Seleccion/puntajes_2015.csv", sep = ";", header=TRUE)
puntajes_16 = read.csv("Datos/PAUC 2016/Seleccion/puntajes_2016.csv", sep = ";", header=TRUE)

puntajes_list = list(puntajes_13, puntajes_14, puntajes_15, puntajes_16)

#BEA
bea_13 = read.csv("Datos/PAUC 2013/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_14 = read.csv("Datos/PAUC 2014/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_15 = read.csv("Datos/PAUC 2015/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_16 = read.csv("Datos/PAUC 2016/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_candidates = list(bea_13, bea_14, bea_15, bea_16)
#Relabelling
for(i in 1:4){
  names(bea_candidates[[i]])[names(bea_candidates[[i]]) %in% c('Ident','INS_SECUENCIA','ID')] = 'id_alumno' 
}

# Aggregate stats ---------------------------------------------------------
agg_stats = list()
for(i in 1:4){
  d = puntajes_list[[i]]
  n_candidates_bea = length(d[d[,'id_alumno'] %in% bea_candidates[[i]][,'id_alumno'],'id_alumno'])
  n_candidates_reg = length(d[!d[,'id_alumno'] %in% bea_candidates[[i]][,'id_alumno'],'id_alumno'])
  n_programs = nrow(unique(car_req_list[[i]]['codigo_carrera']))
  n_unis = nrow(unique(car_req_list[[i]]['code_uni']))
  vac_reg = sum(car_req_list[[i]][,'vacantes_reg'])
  vac_bea = sum(car_req_list[[i]][,'vacantes_bea'])
  vac_assigned_reg = length(reg_list[[i]][reg_list[[i]]$marca == 24,'id_alumno'])
  vac_assigned_bea = length(bea_list[[i]][bea_list[[i]]$marca == 24,'id_alumno'])
  vac_assigned = vac_assigned_reg + vac_assigned_bea
  #The next lines have to count BEA applicants not aplications under the BEA track
  d_reg = post_reg_list[[i]]
  d_bea = post_bea_list[[i]]
  #TODO: Review this with Ignacio
  n_applications_reg = length(d_reg[!d_reg[,'id_alumno'] %in% bea_candidates[[i]][,'id_alumno'],'id_alumno']) #unique is not neccesary for regulars
  n_applications_bea = length(unique(rbind(d_reg[d_reg[,'id_alumno'] %in% bea_candidates[[i]][,'id_alumno'],'id_alumno'],
                                           d_bea[d_bea[,'id_alumno'] %in% bea_candidates[[i]][,'id_alumno'],'id_alumno'])))
  n_applications = n_applications_reg + n_applications_bea
  agg_stats[[i]] = data.frame("Candidates_Reg" = n_candidates_reg, 
                              "Candidates_BEA" = n_candidates_bea,"Programs" = n_programs,
                              "Universities" = n_unis, "Regular_Vacancies" = vac_reg,
                              "BEA_Vacancies" = vac_bea, "Vacancies_assigned" = vac_assigned, 
                              "Regular_Vacancies_assigned" = vac_assigned_reg, 
                              "BEA_Vacancies_assigned" = vac_assigned_bea, 
                              "Regular_applications" = n_applications_reg, 
                              "BEA_applications" = n_applications_bea,
                              "Applications" = n_applications) 
}
#Getting the column names of the data table with spaces. Need to be in the same order as variables in agg_stats
agg_stats_edit_rownames = c("Regular candidates","BEA candidates","Programs","Universities",
                            "Regular vacancies","BEA vacancies", "Vacancies assigned", 
                            "Reg Vacancies assigned", "BEA Vacancies assigned", 
                            "Regular applications", "BEA applications","Applications")

# Latex tables ------------------------------------------------------------
#Source short function for formatting numbers
source("Codigos/R/Algorithm/lib/Functions_PaperDEMRE.R")

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
  row = paste(agg_stats_edit_rownames[i],"&", f(agg_stats[[1]][i]),"&",
              f(agg_stats[[2]][i]),'&',f(agg_stats[[3]][i]),
              '&',f(agg_stats[[4]][i]),space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)
