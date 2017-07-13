# Libraries ---------------------------------------------------------------
#library(stargazer, quietly = TRUE)
#library(lmtest, quietly = TRUE)
library(ggplot2, quietly = TRUE)
#library(dplyr)
#library(reshape2)
#library(ggfortify)
#library(data.table)
#library(RMySQL)
#library(plotly)
#library(latex2exp)
#library(truncnorm)

# Directory ---------------------------------------------------------------
root_dir = '/Users/irios/'
drop_dir = 'Dropbox/PaperDEMRE/'
setwd(paste(c(root_dir,drop_dir), collapse = ""))
# Reading files -----------------------------------------------------------
#carreras_requisitos
post_reg_sec_stu_13 = read.csv("Datos/PAUC 2013/Output/postulaciones_procesadas_reg_student_secuencial_2013.csv", sep = ";", header=TRUE)
post_reg_sec_stu_14 = read.csv("Datos/PAUC 2014/Output/postulaciones_procesadas_reg_student_secuencial_2014.csv", sep = ";", header=TRUE)
post_reg_sec_stu_15 = read.csv("Datos/PAUC 2015/Output/postulaciones_procesadas_reg_student_secuencial_2015.csv", sep = ";", header=TRUE)
post_reg_sec_stu_16 = read.csv("Datos/PAUC 2016/Output/postulaciones_procesadas_reg_student_secuencial_2016.csv", sep = ";", header=TRUE)

codigos = c("codigo_carrera1", "codigo_carrera2", "codigo_carrera3", "codigo_carrera4", "codigo_carrera5",
    "codigo_carrera6", "codigo_carrera7", "codigo_carrera8", "codigo_carrera9", "codigo_carrera10")

post_req_13 = post_reg_sec_stu_13[codigos]
post_req_13$app_count = apply(post_req_13 > 0, 1, sum)
post_req_13$year = rep(2013,nrow(post_req_13))

post_req_14 = post_reg_sec_stu_14[codigos]
post_req_14$app_count = apply(post_req_14 > 0, 1, sum)
post_req_14$year = rep(2014,nrow(post_req_14))

post_req_15 = post_reg_sec_stu_15[codigos]
post_req_15$app_count = apply(post_req_15 > 0, 1, sum)
post_req_15$year = rep(2015,nrow(post_req_15))

post_req_16 = post_reg_sec_stu_16[codigos]
post_req_16$app_count = apply(post_req_16 > 0, 1, sum)
post_req_16$year = rep(2016,nrow(post_req_16))

data = rbind(post_req_13,post_req_14,post_req_15,post_req_16)

ggplot(data , aes(x = app_count, group=year,fill=year)) +
  geom_bar(position="dodge",alpha=0.5, aes(y = (..prop..), group=year)) +
  xlab("Number of applications") +
  ylab("Percentage") +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1)) +
  ggsave(file="Paper/in-prep/figures/number_apps_per_student_2013.pdf", width=10, height=5)
