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
bea_list = list(bea_14, bea_15, bea_16)
#Relabelling
for(i in 1:3){
  names(bea_list[[i]])[names(bea_list[[i]]) == 'Ident' | 
                         names(bea_list[[i]]) =='INS_SECUENCIA'] = 'id_alumno' 
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
  pooled_assignment[[i]] = merge(x = reg_sec[[i]][reg_sec[[i]]$marca==24,], 
                               y = bea_sec[[i]][bea_sec[[i]]$marca==24,], 
                               by = "id_alumno", all = TRUE)[,c("id_alumno","pref.x","pref.y")]
  pooled_assignment[[i]][is.na(pooled_assignment[[i]]$pref.x),"pref.x"] = 0
  pooled_assignment[[i]][is.na(pooled_assignment[[i]]$pref.y),"pref.y"] = 0
  pooled_assignment[[i]]$pref_sec = pmax(pooled_assignment[[i]]$pref.x,pooled_assignment[[i]]$pref.y) 
  pooled_assignment[[i]] = pooled_assignment[[i]][, c("id_alumno","pref_sec")]
  unified_assignment[[i]] = rbind(reg_au[[i]], bea_au[[i]])
  unified_assignment[[i]] = unified_assignment[[i]][unified_assignment[[i]]$marca == 24,]
  names(unified_assignment[[i]])[names(unified_assignment[[i]]) == 'pref'] = 'pref_au'
  #Merging data
  compared_assignment[[i]] = merge(x =pooled_assignment[[i]],
                                 y = unified_assignment[[i]],
                                 by = "id_alumno", all = TRUE)
  #Improvement in preference of assignment from secuencial to unified (be careful with the sign here)
  compared_assignment[[i]]$improve = as.factor(compared_assignment[[i]]$pref_sec - compared_assignment[[i]]$pref_au)   
  compared_assignment[[i]]$process = as.character(2013 + i)
}

#Subsetting in data with improvements and data with new assigned
#If you want to view specific elements of the list as data frames, do View(compared_assignment[[i]])
d = rbind(compared_assignment[[1]],compared_assignment[[2]],compared_assignment[[3]])
#Next line is neccesary because there are new assigned students and for those pref_sec is NA in the merge
d_not_na = d[d$improve != '0' & !is.na(d$pref_sec),]

#Merging with BEA and scores
#TODO: merge with scores and BEA list to characterize

# Plotting results --------------------------------------------------------

#Plot improvements pooling years without NA
ggplot(d_not_na , aes(x = improve)) + 
  geom_bar(stat = 'count') +  
  xlab("Improvement in Preference") +
  ylab("Number of students") +
  ggsave(file="Paper/in-prep/figures/improvement_pref.pdf", width=8, height=5)
#New assignments
d_just_na = d[is.na(d$pref_sec),]
#as.factor() is just to parse the number to a character and see in the histogram every nunmber
d_just_na$improve = as.factor(d_just_na$pref_au)
#Plot new assigned
ggplot(d_just_na , aes(x = improve)) + 
  geom_bar(stat = 'count') +  
  xlab("Asignment Preference") +
  ylab("Number of new students assigned") +
  ggsave(file="Paper/in-prep/figures/new_asignment_pref.pdf", width=8, height=5)

# Aggregate stats ---------------------------------------------------------
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
#Getting the column names of the data table with spaces. Need to be in the same order as variables in agg_stats
agg_stats_edit_colnames = c("Double Assignments","Improvements","New assignments")

# Latex tables ------------------------------------------------------------
#Source short function for formatting numbers
source("Codigos/R/Algorithm/lib/Functions_PaperDEMRE.R")

#Aggregate statistics from the Admission Process
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
  row = paste(agg_stats_edit_colnames[i],"&", f(agg_stats[[1]][i]),"&",
              f(agg_stats[[2]][i]),'&',f(agg_stats[[3]][i]),'&',f(total),space)
  table_body = c(table_body, row)  
}
table_footer = c("\\bottomrule",
                 "\\end{tabular}",
                 "}",
                 "\\end{table}")
writeLines(c(table_header, table_body, table_footer), table_latex)
close(table_latex)




