######################################################################
### CLASE DE R
### OBJETIVO: LA IDEA DE ESTE CÓDIGO ES INTRODUCIR ALGO DE FUNCIONES 
### E IDEAS BÁSICAS DE R

# PARA ESTO CONSIDERAMOS EL SIGUIENTE PROBLEMA:
# 
# PROBLEMA 1:
# Dado un número natural n consideremos el proceso de sumar
# el cuadrado de los dígitos de n. Repetir este proceso con el
# número obtenido. Ejemplo:
# 
# 53-34-25-29-85-89
# 13-10-1
#
# Se puede demostrar que cualquier número converge a 89 ó 1.
#
#
# Desarrolle un código que dado un número 'n' natural sea 
# capaz de determinar si 'n' converge a 89 ó 1.
#
# ¿Cuántos número del 1 al 1000 convergen a 89?
# 

# INICIEMOS CON ALGUNOS CÁLCULOS MANUELES ANTES DE HACER UNA FUNCIÓN

# DEFINAMOS A K COMO 13 (YA SABEMOS QUE 13 CONVERGE A 1)
k=13

# CÓMO SACAR EL RESIDUO DE UN NÚMERO AL DIVIDIRLO
# ENTRE OTRO
33 %% 11
(k %% 10 - k %% 1)/1
(k %% 100 - k %% 10)/10
(k %% 1000 - k %% 100)/100

#####################################################################
# CONSTRUCCIÓN DE LA FUNCIÓN:
# Nombre: numero.final
# Input: k (número natural)
# Output: 1 ó 89

numero.final<-function(k){
  i=0
  d=0
  s=0
  
  if ( k==1 | k==89 ){
      return(k)
  } else{
    # CÍCLO 
    while ( k!=1 & k!=89 ){
        s=0
        for ( i in 1:nchar(k) ) {
            # GUARDEMOS EL DÍGITO EN UNA VARIABLE
            d=(k %% 10^i - k %% 10^(i-1))/(10^(i-1))
            d=d*d
            s=s+d
        }
        k=s
    }
    # TERMINA CÍCLO
  }
  return(k)
}

# PARA CÁLCULAR CUÁNTOS NÚMERO DE 1 AL 1000 CONVERGEN A 89 BASTA CON
# HACER UN CÍCLO

count89=0
for (i in 1:1000){
  if (numero.final(i)==89){
    count89=count89+1
  }
}

print(count89)

# POR LO TANTO EL NÚMERO DE 89s DEL 1 AL 1000 ESTÁ DADO POR count89

# REFLEXIÓN: EL CÓDIGO SEGURAMENTE NO ES EL MÁS ÓPTIMO. POR EJEMPLO
# SI NOS PIDEN CALCULAR EL NÚMERO DE 89s PERO DE 1 A 10,000,000 
# QUIZÁ PODAMOS IR GUARDANDO CADA UNO DE LOS NÚMEROS QUE SE VAN
# GENERANDO EN EL PROCESO E IR LLENDANDO UN VECTOR/MATRIZ Y HACER UN
# CÍCLO MÁS EFICIENTE. EJEMPLO:
#
# SUPONGA QUE PROGRAMÓ EL CICLO ANTERIOR
# CUANDO i=53 EL ALGORITMO CALCULA LOS SIGUIENTES NÚMEROS:
# 53-34-25-29-85-89 Y DEVUELVE 89
# 
# NO OBSTANTE EN LA PRIMER ITERACIÓN (34) EL CICLO YA HABÍA PASADO
# POR i=34 Y CONCLUYÓ QUE numero.final(34)=89 POR LO TANTO UN CÍCLO
# MÁS EFICIENTE SÓLO HARIA ESA ITERACIÓN, ES DECIR 53-34:89
#
# DE HECHO UNA FORMA ÓPTIMA SERÍA:
#
# i=25 GENERA SERIE 25-29-85-89
# EN ESA SERIE SE CONCLUYE QUE:
# numero.final(25)=numero.final(29)=numero.final(85)=89
# Y HACEMOS QUE EL CÍCLO YA NO HAGA i=29 ó 85
#
# ¿CÓMO QUEDARÍA EL CÓDIGO DE ES ALGORITMO?
# ¿EN CUÁNTO SE REDUCE EL TIEMPO DE CÁLCULO?