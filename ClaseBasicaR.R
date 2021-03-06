######################################################################
### CLASE DE R
### OBJETIVO: LA IDEA DE ESTE C�DIGO ES INTRODUCIR ALGO DE FUNCIONES 
### E IDEAS B�SICAS DE R

# PARA ESTO CONSIDERAMOS EL SIGUIENTE PROBLEMA:
# 
# PROBLEMA 1:
# Dado un n�mero natural n consideremos el proceso de sumar
# el cuadrado de los d�gitos de n. Repetir este proceso con el
# n�mero obtenido. Ejemplo:
# 
# 53-34-25-29-85-89
# 13-10-1
#
# Se puede demostrar que cualquier n�mero converge a 89 � 1.
#
#
# Desarrolle un c�digo que dado un n�mero 'n' natural sea 
# capaz de determinar si 'n' converge a 89 � 1.
#
# �Cu�ntos n�mero del 1 al 1000 convergen a 89?
# 

# INICIEMOS CON ALGUNOS C�LCULOS MANUELES ANTES DE HACER UNA FUNCI�N

# DEFINAMOS A K COMO 13 (YA SABEMOS QUE 13 CONVERGE A 1)
k=13

# C�MO SACAR EL RESIDUO DE UN N�MERO AL DIVIDIRLO
# ENTRE OTRO
33 %% 11
(k %% 10 - k %% 1)/1
(k %% 100 - k %% 10)/10
(k %% 1000 - k %% 100)/100

#####################################################################
# CONSTRUCCI�N DE LA FUNCI�N:
# Nombre: numero.final
# Input: k (n�mero natural)
# Output: 1 � 89

numero.final<-function(k){
  i=0
  d=0
  s=0
  
  if ( k==1 | k==89 ){
      return(k)
  } else{
    # C�CLO 
    while ( k!=1 & k!=89 ){
        s=0
        for ( i in 1:nchar(k) ) {
            # GUARDEMOS EL D�GITO EN UNA VARIABLE
            d=(k %% 10^i - k %% 10^(i-1))/(10^(i-1))
            d=d*d
            s=s+d
        }
        k=s
    }
    # TERMINA C�CLO
  }
  return(k)
}

# PARA C�LCULAR CU�NTOS N�MERO DE 1 AL 1000 CONVERGEN A 89 BASTA CON
# HACER UN C�CLO

count89=0
for (i in 1:1000){
  if (numero.final(i)==89){
    count89=count89+1
  }
}

print(count89)

# POR LO TANTO EL N�MERO DE 89s DEL 1 AL 1000 EST� DADO POR count89

# REFLEXI�N: EL C�DIGO SEGURAMENTE NO ES EL M�S �PTIMO. POR EJEMPLO
# SI NOS PIDEN CALCULAR EL N�MERO DE 89s PERO DE 1 A 10,000,000 
# QUIZ� PODAMOS IR GUARDANDO CADA UNO DE LOS N�MEROS QUE SE VAN
# GENERANDO EN EL PROCESO E IR LLENDANDO UN VECTOR/MATRIZ Y HACER UN
# C�CLO M�S EFICIENTE. EJEMPLO:
#
# SUPONGA QUE PROGRAM� EL CICLO ANTERIOR
# CUANDO i=53 EL ALGORITMO CALCULA LOS SIGUIENTES N�MEROS:
# 53-34-25-29-85-89 Y DEVUELVE 89
# 
# NO OBSTANTE EN LA PRIMER ITERACI�N (34) EL CICLO YA HAB�A PASADO
# POR i=34 Y CONCLUY� QUE numero.final(34)=89 POR LO TANTO UN C�CLO
# M�S EFICIENTE S�LO HARIA ESA ITERACI�N, ES DECIR 53-34:89
#
# DE HECHO UNA FORMA �PTIMA SER�A:
#
# i=25 GENERA SERIE 25-29-85-89
# EN ESA SERIE SE CONCLUYE QUE:
# numero.final(25)=numero.final(29)=numero.final(85)=89
# Y HACEMOS QUE EL C�CLO YA NO HAGA i=29 � 85
#
# �C�MO QUEDAR�A EL C�DIGO DE ES ALGORITMO?
# �EN CU�NTO SE REDUCE EL TIEMPO DE C�LCULO?