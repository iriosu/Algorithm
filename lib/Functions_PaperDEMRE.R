#Functions of the proyect to be called in the scripts
f = function (x){
  aux = prettyNum(as.numeric(x),big.mark=",",scientific=FALSE)
  return(aux)
}