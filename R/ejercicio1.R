potencia_rescursiva <-function(b,e){
  if (e==0){
    return (1)
  }else{
    sum<-0
    for (i in 1:b){
      sum<-sum+potencia_rescursiva(b,e-1)

    }
    return (sum)
  }

}

potencia_doble <- function(base,expo,expo2){
  j <- potencia_rescursiva(base,expo)
  if (expo2==0){
    return (1)
  }else{
    sum<-0
    for (i in 1:j){
      sum<-sum+potencia_rescursiva(j,expo2-1)

    }
    return (sum)
  }
}
