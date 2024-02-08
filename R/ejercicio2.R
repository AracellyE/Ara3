#Creación de funciones
#Por: Aracelly Escudero
#Paso1: Obtener una base de datos y llamar la base de datos con el file.choose()

#Estadística descriptiva

#Cuartiles
q1 <-  function(data){
  orden <- sort(data)
  n <- length(orden)
  q1 <- orden[ceiling(0.25*n)]
  return(q1)
}
q2 <-  function(data){
  orden <- sort(data)
  n <- length(orden)
  q2 <- orden[ceiling(0.5*n)]
  return(q2)
}
q3 <-  function(data){
  orden <- sort(data)
  n <- length(orden)
  q3 <- orden[ceiling(0.75*n)]
  return(q3)
}
# Crear la función
Estadistica_descriptiva <- function(base){
  datos <- read_excel(base)
  trescolumnas <- datos[,1:4] #Seleccionamos solamente las 4 columnas
  Varianza <- apply(datos,2,var)
  Mediana <- apply(datos,2,median)
  Media <- apply(datos,2,mean)

  Desviacion_Estandar <- apply(datos,2,sd)
  Primer_cuartil <- apply(datos, 2, q1)
  Segundo_cuartil <- apply(datos, 2, q2)
  Tercer_cuartil <- apply(datos, 2, q3)
  Máximo <- apply(datos,2,max)
  Mínimo <- apply(datos,2,min)

  a <- data.frame(Varianza =Varianza,Desviacion_Estandar=Desviacion_Estandar
                  ,Media=Media,Mediana=Mediana,Primer_cuartil=Primer_cuartil,
                  Segundo_cuartil=Segundo_cuartil,Tercer_cuartil=Tercer_cuartil
                  , Máximo=Máximo, Mínimo=Mínimo)
  d <- t(a)
  e <- data.frame(d)

  return(e)
}

