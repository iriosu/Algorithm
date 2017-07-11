#Functions of the proyect to be called in the scripts

#Formatting pretty numbers
f = function(x){
  aux = prettyNum(as.numeric(x),big.mark=",",scientific=FALSE)
  return(aux)
}
#Get the total number of applications
Get_total_post = function(d){ #Recieves a data frame with the following column names at least
  aux = vector(length = nrow(d))
  for(i in 1:nrow(d)){#Iterate over the number of rows of the data frame
    if(d[i,'codigo_carrera10'] != 0){aux[i] = 10}
    else if(d[i,'codigo_carrera9']!= 0){aux[i] = 9}
    else if(d[i,'codigo_carrera8'] != 0){aux[i] = 8}
    else if(d[i,'codigo_carrera7'] != 0){aux[i] = 7}
    else if(d[i,'codigo_carrera6'] != 0){aux[i] = 6}
    else if(d[i,'codigo_carrera5'] != 0){aux[i] = 5}
    else if(d[i,'codigo_carrera4'] != 0){aux[i] = 4}
    else if(d[i,'codigo_carrera3'] != 0){aux[i] = 3}
    else if(d[i,'codigo_carrera2'] != 0){aux[i] = 2}
    else if(d[i,'codigo_carrera1'] != 0){aux[i] = 1}
    else{aux[i] = 0} #This case shouldn't exist because the file shouldn't include these people
  }
  return(aux)  #returns a columns vector with the number of total applications
}