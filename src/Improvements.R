#Improvements from unification
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
# Functions ---------------------------------------------------------------
#Source short function for formatting numbers
source("Codigos/R/Algorithm/lib/Functions_PaperDEMRE.R")
# Reading files -----------------------------------------------------------
#asignacion_obtenida (all possibilities)
reg_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_secuencial_university_2014.csv", sep = ";", header=TRUE)
bea_sec_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_secuencial_university_2014.csv", sep = ";", header=TRUE)
reg_au_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_reg_unica_university_2014.csv", sep = ";", header=TRUE)
bea_au_uni_14 = read.csv("Datos/PAUC 2014/Output/asignacion_obtenida_bea_unica_university_2014.csv", sep = ";", header=TRUE)

reg_au_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_unica_student_2015.csv", sep = ";", header=TRUE)
bea_au_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_unica_student_2015.csv", sep = ";", header=TRUE)
reg_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_reg_secuencial_student_2015.csv", sep = ";", header=TRUE)
bea_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/asignacion_obtenida_bea_secuencial_student_2015.csv", sep = ";", header=TRUE)

reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_unica_student_2016.csv", sep = ";", header=TRUE)
bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_unica_student_2016.csv", sep = ";", header=TRUE)
reg_sec_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_reg_secuencial_student_2016.csv", sep = ";", header=TRUE)
bea_sec_stu_16 = read.csv("Datos/PAUC 2016/Output/asignacion_obtenida_bea_secuencial_student_2016.csv", sep = ";", header=TRUE)

reg_sec = list(reg_sec_uni_14,reg_sec_stu_15,reg_sec_stu_16)
bea_sec = list(bea_sec_uni_14,bea_sec_stu_15,bea_sec_stu_16)
reg_au = list(reg_au_uni_14,reg_au_stu_15,reg_au_stu_16)
bea_au = list(bea_au_uni_14,bea_au_stu_15,bea_au_stu_16)

#puntajes 
puntajes_14 = read.csv("Datos/PAUC 2014/Seleccion/puntajes_2014.csv", sep = ";", header=TRUE)
puntajes_15 = read.csv("Datos/PAUC 2015/Seleccion/puntajes_2015.csv", sep = ";", header=TRUE)
puntajes_16 = read.csv("Datos/PAUC 2016/Seleccion/puntajes_2016.csv", sep = ";", header=TRUE)
puntajes_list = list(puntajes_14, puntajes_15, puntajes_16)

#BEA
bea_14 = read.csv("Datos/PAUC 2014/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_15 = read.csv("Datos/PAUC 2015/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_16 = read.csv("Datos/PAUC 2016/Seleccion/alumnos_BEA.csv", sep = ";", header=TRUE)
bea_candidates = list(bea_14, bea_15, bea_16)
#Relabelling
for(i in 1:3){
  names(bea_candidates[[i]])[names(bea_candidates[[i]]) %in% c('Ident','INS_SECUENCIA','ID')] = 'id_alumno' 
  bea_candidates[[i]]$bea = 1
}

#postulaciones_procesadas
# NOT Following the same assignment than for asignacion_obtenida
#Here we are reading the Unique assignment because applications shouldn't be affected by this
post_reg_au_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_reg_student_unica_2014.csv", sep = ";", header=TRUE)
post_bea_au_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_bea_student_unica_2014.csv", sep = ";", header=TRUE)
post_reg_au_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_reg_student_unica_2015.csv", sep = ";", header=TRUE)
post_bea_au_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_bea_student_unica_2015.csv", sep = ";", header=TRUE)
post_reg_au_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_reg_student_unica_2016.csv", sep = ";", header=TRUE)
post_bea_au_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_bea_student_unica_2016.csv", sep = ";", header=TRUE)

post_reg_list = list(post_reg_au_stu_14,post_reg_au_stu_15,post_reg_au_stu_16)
post_bea_list = list(post_bea_au_stu_14,post_bea_au_stu_15,post_bea_au_stu_16)
#Binding to have by year, this will duplicate some BEA ids!
post_list = list(rbind(post_reg_au_stu_14, post_bea_au_stu_14),
                 rbind(post_reg_au_stu_15, post_bea_au_stu_15),
                 rbind(post_reg_au_stu_16, post_bea_au_stu_16))
#Getting the number of applications for every student
num_post_list = list()
for(i in 1:3){
  d = post_list[[i]]
  #Keep only one row for id_alumno
  d = d[!duplicated(d$id_alumno),] # duplicated() Returns a vector of boolean in the correct order
  num_post_list[[i]] = d #creating num_post_list dataframe
  num_post_list[[i]]$num_post = Get_total_post(d) #creating variable (vector) for Total number of applications
}

# Merging and comparing datasets ------------------------------------------
#Double assignment by year
double_assigned = list()
#Pooling data from BEA and Reg assignments 
pooled_assignment = list()
unified_assignment = list()
#To compare assingments
compared_assignment = list()
for(i in 1:3){
  double_assigned[[i]] = merge(x = reg_sec[[i]][reg_sec[[i]]$marca==24,], 
                             y = bea_sec[[i]][bea_sec[[i]]$marca==24,], 
                             by = "id_alumno", all = FALSE)
  #Merging with BEA
  double_assigned[[i]] = merge(x = double_assigned[[i]],  #Every double assigned should be BEA
                               y = bea_candidates[[i]], 
                               by = "id_alumno", all.x = TRUE)
  #Merging with scores
  double_assigned[[i]] = merge(x = double_assigned[[i]],
                               y = puntajes_list[[i]], 
                               by = "id_alumno", all.x = TRUE)
  #Merging with number of applications applications
  double_assigned[[i]] = merge(x = double_assigned[[i]],
                               y = num_post_list[[i]], 
                               by = "id_alumno", all.x = TRUE)
  #Pooling secuencial data 
  pooled_assignment[[i]] = merge(x = reg_sec[[i]][reg_sec[[i]]$marca==24,], 
                               y = bea_sec[[i]][bea_sec[[i]]$marca==24,], 
                               by = "id_alumno", all = TRUE)[,c("id_alumno","pref.x","pref.y")]
  pooled_assignment[[i]][is.na(pooled_assignment[[i]]$pref.x),"pref.x"] = 11
  pooled_assignment[[i]][is.na(pooled_assignment[[i]]$pref.y),"pref.y"] = 11
  pooled_assignment[[i]]$pref_sec = pmin(pooled_assignment[[i]]$pref.x,pooled_assignment[[i]]$pref.y) 
  pooled_assignment[[i]] = pooled_assignment[[i]][, c("id_alumno","pref_sec")]
  #appending unified data
  unified_assignment[[i]] = rbind(reg_au[[i]], bea_au[[i]])
  unified_assignment[[i]] = unified_assignment[[i]][unified_assignment[[i]]$marca == 24,]
  names(unified_assignment[[i]])[names(unified_assignment[[i]]) == 'pref'] = 'pref_au'
  #Comparing data between secuencial and unified 
  compared_assignment[[i]] = merge(x =pooled_assignment[[i]],
                                 y = unified_assignment[[i]],
                                 by = "id_alumno", all = TRUE)
  #Improvement in preference of assignment from secuencial to unified (be careful with the sign here)
  compared_assignment[[i]]$improve = as.factor(compared_assignment[[i]]$pref_sec - compared_assignment[[i]]$pref_au)   
  compared_assignment[[i]]$process = as.character(2013 + i)
  #Merge with BEA
  compared_assignment[[i]] = merge(x = compared_assignment[[i]],
                                 y = bea_candidates[[i]], 
                                 by = "id_alumno", all.x = TRUE)
  #Merge with scores
  compared_assignment[[i]] = merge(x = compared_assignment[[i]],
                               y = puntajes_list[[i]], 
                               by = "id_alumno", all.x = TRUE)
  #Merge with number of applications applications
  compared_assignment[[i]] = merge(x = compared_assignment[[i]],
                                   y = num_post_list[[i]], 
                                   by = "id_alumno", all.x = TRUE)
}

#Subsetting in data with improvements (pooling the years)
d = rbind(compared_assignment[[1]],compared_assignment[[2]],compared_assignment[[3]])
d[is.na(d[,'bea']),'bea'] = 0
#Next line is neccesary because there are new assigned students and for those pref_sec is NA in the merge
#Improvements
d_improve = d[d$improve != '0' & !is.na(d$pref_sec),]
#New assignments
d_new_a = d[is.na(d$pref_sec),]
#as.factor() is just to parse the number to a character and see in the histogram every nunmber
d_new_a$improve = as.factor(d_new_a$pref_au)
#Double assignments
d_double = rbind(double_assigned[[1]],double_assigned[[2]],double_assigned[[3]])


# Aggregate stats of Improvements---------------------------------------------------------
agg_stats = list()
for(i in 1:3){
  n_double_assign = nrow(double_assigned[[i]])
  d = compared_assignment[[i]]
  n_improve = nrow(d[d$improve != '0' & !is.na(d$improve),])
  n_new_assigned = nrow(d[is.na(d$pref_sec),]) 
  agg_stats[[i]] = data.frame("Double_Assignments" = n_double_assign, 
                              "Improvements" = n_improve,
                              "New_assignments" = n_new_assigned) 
}
#Getting the row names of the data table with spaces. Need to be in the same order as variables in agg_stats
agg_stats_edit_rownames = c("Double Assignments","Improvements","New assignments")

# Latex table Agg stats of improvements ------------------------------------------------------------
#Latex table for statistics of improvements by year
table_latex = file("Paper/in-prep/tables/improvements.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Impact of Unified Assignment 2014-2016}", 
                 "\\label{tab: improvements}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lccccc}",
                 "\\toprule", 
                 "\\toprule")
table_body = c("   & 2014 & 2015 & 2016 & Total \\\\" ," \\midrule")
space = '\\\\[0pt]'
for(i in 1:length(names(agg_stats[[1]]))){
  #Compute total by row
  total = agg_stats[[1]][i] + agg_stats[[2]][i] + agg_stats[[3]][i]
  row = paste(agg_stats_edit_rownames[i],"&", f(agg_stats[[1]][i]),"&",
              f(agg_stats[[2]][i]),'&',f(agg_stats[[3]][i]),'&',f(total),space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)



# Chracteryzing improvement stats -----------------------------------------
#Stats for characterizing students in the groups
#These lists go over the groups now and not the years!
charac_stats = list()
groups_list = list(d_double, d_improve, d_new_a)#list of objects to iterate over(Double Assigned, Improvements, New Assigned)
for(i in 1:3){#Iterate over group list, not over years this time
  d = groups_list[[i]]
  n_bea = nrow(d[d$bea==1,])
  n_reg = nrow(d[d$bea==0,])
  mean_score_math_lan = mean(pmax((d$matematica_actual + d$lenguaje_actual)/2,(d$matematica_anterior + d$lenguaje_anterior)/2))
  #Average number of preferences they apply to
  mean_num_post = mean(d$num_post)
  charac_stats[[i]] = data.frame("Regular_students" = n_reg, 
                                 "BEA_students" = n_bea,
                                 "Mean_Math_Lan" = round(mean_score_math_lan, digits=0),
                                 "Mean_num_post" = round(mean_num_post, digits=1))
}
#Getting the row names of the data table with spaces. Need to be in the same order as variables in agg_stats
charac_stats_edit_rownames = c("Regular students","BEA students", "Mean Math-Lan", "Mean Total Applications")

# Latex table Agg stats of improvements ------------------------------------------------------------
#Characterization of of groups
table_latex = file("Paper/in-prep/tables/improvements_charac.tex")
table_header = c("\\begin{table}[H]", 
                 "\\centering", 
                 "\\caption{Characterization of students under Unified Assignment 2014-2016}", 
                 "\\label{tab: improvements}", 
                 "\\scalebox{1}{", 
                 "\\begin{tabular}{lcccc}",
                 "\\toprule", 
                 "\\toprule")
table_body = c("   & Double Assignments & Improvements & New assignments  \\\\" ," \\midrule")
space = '\\\\[0pt]'
for(i in 1:length(names(charac_stats[[1]]))){
  #Compute total by row
  row = paste(charac_stats_edit_rownames[i],"&", f(charac_stats[[1]][i]),"&",
              f(charac_stats[[2]][i]),'&',f(charac_stats[[3]][i]),space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)

# Plotting results --------------------------------------------------------

#Plot improvements assigned preference
ggplot(d_improve , aes(x = improve)) + 
  geom_bar(stat = 'count') +  
  xlab("Improvement in Preference") +
  ylab("Number of students") +
  ggsave(file="Paper/in-prep/figures/improvement_pref.pdf", width=8, height=5)
#Plot new assigned
ggplot(d_new_a , aes(x = improve)) + 
  geom_bar(stat = 'count') +  
  xlab("Asignment Preference") +
  ylab("Number of new students assigned") +
  ggsave(file="Paper/in-prep/figures/new_asignment_pref.pdf", width=8, height=5)
