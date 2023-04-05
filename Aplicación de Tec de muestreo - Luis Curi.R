###       APLICACION DE LAS PRINCIPALES TECNICAS DE MUESTREO               #######
###                                              BY: LUIS CURI
################################################################################
############################## PREGUNTA 1 ######################################
################################################################################

####PREPARACION DEL ENTORNO#########
library(foreign)
library(survey)
library(sampling)
library(dplyr)
library(readxl)
library(foreign)
base_P <- read.spss("Habilidades_al_trabajo.SAV", to.data.frame = TRUE)


###ANALISIS EXPLORATORIO####
dim(base_P) ##se cuenta con 3146 observaciones y 39 variables
colnames(base_P)
summary(base_P)

#Eiminacion de espacios
table(base_P$NOMBREDD) #hay presencia de espacios
base_P$NOMBREDD <- trimws(base_P$NOMBREDD)
base_P$NOMBREPV<- trimws(base_P$NOMBREPV)
base_P$NOMBREDI<- trimws(base_P$NOMBREDI)
base_P$T_EMPRESA_CAMPO <- trimws(base_P$T_EMPRESA_CAMPO)
base_P$SEC_CAMPO <- trimws(base_P$SEC_CAMPO)

#AÑADIR ID
ID <- c(1:length(base_P$ID)) #se añade ID para tener un mejor control de los datos
base_P <- cbind(ID, base_P)

#Tamanio de la Poblacion
length(base_P$ID)
N <- length(base_P$ID) 
N #el tamaño de la poblacion es de 3146 empresas

######ESTRATIFICACION POR TAMANIO DE EMPRESA
#Estrato: Tamanio de empresa
table(base_P$T_EMPRESA_CAMPO) #contamos con 3 categorias
prop.table(table(base_P$T_EMPRESA_CAMPO))*100
################################################################.
###########nuestras proporciones segun al t campo empresa son(%):
###Gran Empresa    Mediana Empresa        Pequeña Empresa 
####25.842339           8.455181             65.702479 
################################################################.

#Ordenar datos segun estrato Tamanio empresa
library(dplyr)
base_P <- base_P %>% arrange(T_EMPRESA_CAMPO) 


#Tamanios por estrato

table(base_P$T_EMPRESA_CAMPO)
as.vector(table(base_P$T_EMPRESA_CAMPO))
Nh <- as.vector(table(base_P$T_EMPRESA_CAMPO))
Nh
#############MUESTRA PILOTO##########################
#piloto de tamanio 10% por cada estrato

nh <- Nh * 0.1 
nh
ceiling(nh)
nh <- ceiling(nh)
nh

####vizualizacion y categorizacion de Estratos 

h1 <- base_P[base_P$T_EMPRESA_CAMPO=="Pequeña Empresa",]
dim(h1)

h2 <-base_P[base_P$T_EMPRESA_CAMPO=="Mediana Empresa",]
dim(h2)

h3 <- base_P[base_P$T_EMPRESA_CAMPO=="Gran Empresa",]
dim(h3)

set.seed(123)
# Muestra seleccionada por cada estrato
piloto_h1 <- h1[sample(Nh[1], nh[1], replace = F), ]  ;  dim(piloto_h1)[1] 
piloto_h2 <- h2[sample(Nh[2], nh[2], replace = F), ]  ;  dim(piloto_h2)[1] 
piloto_h3 <- h3[sample(Nh[3], nh[3], replace = F), ]  ;  dim(piloto_h3)[1]
#tomando el 10% extraemos las observaciones para el piloto


#########TAMANIO DE MUESTRA CON VARIABLE CUALITATIVA C3_P5" ##########

# Vector de varianzas ( p*q = p*(1-p) ) para estimar porporción en MAE
piloto_pq_1 <- c(max(prop.table(table(piloto_h1$C3_P5))*(1-prop.table(table(piloto_h1$C3_P5)))), 
               max(prop.table(table(piloto_h2$C3_P5))*(1-prop.table(table(piloto_h2$C3_P5)))),
               max(prop.table(table(piloto_h3$C3_P5))*(1-prop.table(table(piloto_h3$C3_P5)))))
piloto_pq_1

# Asignacion Neyman: varianzas diferentes y costos iguales
ah_1 <- (Nh*sqrt(piloto_pq_1)) / sum(Nh*sqrt(piloto_pq_1)) # Ingresa vector de varianzas
ah_1

#Asignacion proporcional
n = 0.1 * N
n * ah_1
nh_1 <- ceiling(ah_1*n)
nh_1
#se usa de referencia la variable C3_P5, para realizar el calculo respecivo en base a sus proporciones
#obteniendo un nh_1 de 80, 28 y 208


#########TAMANIO DE MUESTRA CON VARIABLE CUALITATIVA C3 P7"##########
# Vector de varianzas ( p*q = p*(1-p) ) para estimar porporción en MAE
piloto_pq_2 <- c(max(prop.table(table(piloto_h1$C3_P7))*(1-prop.table(table(piloto_h1$C3_P7)))), 
               max(prop.table(table(piloto_h2$C3_P7))*(1-prop.table(table(piloto_h2$C3_P7)))),
               max(prop.table(table(piloto_h3$C3_P7))*(1-prop.table(table(piloto_h3$C3_P7)))))
piloto_pq_2

# Asignacion Neyman: varianzas diferentes y costos iguales
ah_2 <- (Nh*sqrt(piloto_pq_2)) / sum(Nh*sqrt(piloto_pq_2)) # Ingresa vector de varianzas
ah_2

#Asignacion proporcional
n = 0.1 * N
n * ah_2
nh_2 <- ceiling(ah_2*n)
nh_2

#la segunda referencia a usar es la variable C3_P7, para realizar el calculo respecivo en base a sus proporciones
#obteniendo un nh_1 de 80, 28 y 208

####comparamos resultados en base a la semilla 123
nh_1
nh_2

sum(nh_1)
sum(nh_2)
##################################################################################.
#la suma para el tamaño de la muestra final, sale la misma cantidad en ambos casos
              #> nh_1                     nh_2
           #####79  28 209          ### 79  27 210
             ###sum(nh_1)             ####sum(nh_2)
              ####316                     ####316  ####cantidad de la muestra
##################################################################################.


######ESTRATIFICAR MEDIANTE FUN STRATA'#################

set.seed(123)
m <- strata(base_P, c("T_EMPRESA_CAMPO"), size=nh_1, method="srswor") # srswor: simple random sampling without replacement (srswor)
head(m)
summary(m)
table(m$T_EMPRESA_CAMPO, m$Prob)

muestra_MAE <- getdata(base_P, m)
head(muestra_MAE)
dim(muestra_MAE)

as.vector(table(muestra_MAE$T_EMPRESA_CAMPO))
nh

muestra_MAE$fpc = rep(Nh,nh_1)
muestra_MAE$pp  <- rep(nh_1/Nh, nh_1) 

dim(muestra_MAE)
#####################################################################################.
###de esta forma realizamos la estratificacion en base a la informacion bridada por 
###el piloto. Nuestra muestra final por estrato con la semilla 123 sale:

###                       0.0971709717097171      0.101112723754233              0.105263157894737
####Gran Empresa                    79 #                0                                   0
####Mediana Empresa                  0                 0                                   28#
####Pequeña Empresa                  0               209 #                                 0



#####DISEÑO DE MUESTRA PARA MAE#############################

designe_MAE <- svydesign(id = ~ 1, 
                         strata = ~ T_EMPRESA_CAMPO , 
                         fpc = ~ fpc,  # Finite population correction (sé cuál es N)
                         data = muestra_MAE)
designe_MAE
###En base a la informacion desarrollada se realiza el diseño que nos sirve
###para realizar las estimaciones, para ello decidimos la variable a estratificar es
#### T_EMPRESA_CAMPO.


#%%%%%%%%%%%%%%%%%%%%%%%%%#
#### MAE: ESTIMACIONES ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#
                                    #####pregunta B
#Estimar la proporcion de empresas que indicaron que sus ventas del anio 2016 Aumentaron (C3 P5)
svymean(~ C3_P5, designe_MAE, na.rm=T)
prop.table(table(na.omit(base_P$C3_P5)))
#RPTA###########################################################################.
#la proporcion de empresas que indicador que sus ventas del anio 2016 aumentarian fue de :
#0.42752 o 42.752% con un error standar de 0.0262 o un 2.62% (con la semilla 123)
#################################################################################.

#Estimar la proporcion de empresas cuyo mayor porcentaje de clientes se encuentra a nivel Nacional - Todo el Peru (C3 P7)
svymean(~ C3_P7, designe_MAE, na.rm=T)
prop.table(table(base_P$C3_P7))
#RPTA############################################################################:
#La proporcion de empresas cuyo mayor porcentaje de clientes se encuentra a nivel Nacional - Todo el Peru fue de 
#0.282088 o el 28.21% con un error starndar de 0.0241 o 2.41%
#################################################################################.


                                  ######PREGUNTA C
#Estimar la media del monto de ventas netas anuales que obtuvo en el 2016 (C3 P2)
svymean(~ C3_P2, designe_MAE, na.rm=T)
mean(na.omit(base_P$C3_P2))
#RPTA############################################################################.
#la media del monto de ventas netas anuales que obtuvo en el 2016, fue de:
#26 672 351 de soles con un error standar de 10 390 833 de soles
#################################################################################.

#Estimar la media del porcentaje de sus ventas netas anuales que fueron por exportacion (C3 P4)
svymean(~ C3_P4, designe_MAE, na.rm=T)
mean(base_P$C3_P4, na.rm = T)
#RPTA############################################################################.
#La media del porcentaje de sus ventas netas anuales que fueron por exportacion
#el 40.987% con un error standar de 7.699%
#################################################################################.



################################################################################
################################PREGUNTA 2 #####################################
################################################################################
###ANALISIS EXPLORATORIO####
dim(base_P)
colnames(base_P)
summary(base_P)

### TRABAJAMOS SOLO CON INFORMACION DE solo empresas del Departamento de Lima y Provincia de Lima:

base_Dep<-base_P[base_P$NOMBREDD == "LIMA",] #DEPARTAMENTO LIMA
base<- base_Dep[base_Dep$NOMBREPV == "LIMA",] #PROVINCIA DE LIMA

#verificando informacion a trabajar
table(base$NOMBREDD)
table(base$NOMBREPV)
table(base$NOMBREDI)


base <- base %>% arrange(NOMBREDI) #ORDENAR EN BASE A DISTRITOS


##DETERMINAR EL NUMERO DE DISTRITOS
table(base[,10]) #distritos y su cantidad de colegios
as.vector(table(base[,10])) #numero de colegios por distrito
N =dim(as.data.frame(table(base[,10])))[1] #numero de distritos

#Proporcion de colegios por conglomerado (distritos)
prop.table(table(base[,10]))*100
as.vector(prop.table(table(base[,10])))*100

#Cantidad de empresas por distritos
table(base[,10]) #distritos y su cantidad de empresas
Mh<-as.vector(table(base[,10]))

##Cantidad de conglomarados a seleccionar
n_Uprimarias = 16


#########MUESTREO POR CONGLOMERADO POR 2 ETAPAS#####
#####PRIMERA ETAPA#############
set.seed(321)
Uprimarias<-sample(N,n_Uprimarias)
Uprimarias
muestra_etapa_1 <- base[base$NOMBREDI %in%
                          unique(base$NOMBREDI)[Uprimarias],]
#se realiza un muestreo por conglomerado de primera etapa en base al departamento y provincia de Lima
#con una semilla de 321

##ver los resultados del primer muestreo
table(muestra_etapa_1$NOMBREDI) #los 16 Distritos considerados
prop.table(table(muestra_etapa_1$NOMBREDI))
as.vector(table(muestra_etapa_1$NOMBREDI)) #proporciones en base a los distritos elegidos
round(as.vector(prop.table(table(muestra_etapa_1$NOMBREDI))),3)


######## SEGUNDA ETAPA ##################

####DETERMINANDO NUESTRA MUESTRA SECUNDARIA
#se indicói que el numero de obseraciones debe ser del 50%
n_Usecundarias= sum(as.vector(table(muestra_etapa_1$NOMBREDI)))*0.5
n_Usecundarias

n_Usecundarias * round(as.vector(prop.table(table(muestra_etapa_1$NOMBREDI))),3)
n_Usecundarias * as.vector(prop.table(table(muestra_etapa_1$NOMBREDI)))
sum(n_Usecundarias * as.vector(prop.table(table(muestra_etapa_1$NOMBREDI))))
#se redondea y se llega a una cantidad total a muestrear de 420
ni = ceiling(n_Usecundarias * as.vector(prop.table(table(muestra_etapa_1$NOMBREDI))))

###muestreo
muestra_etapa_2<-NULL
for(i in 1:length(ni)){
  hi <- muestra_etapa_1[muestra_etapa_1$NOMBREDI == unique(muestra_etapa_1$NOMBREDI)[i],]
  set.seed(123)
  muestra_hi <- hi[sample(length(hi$NOMBREDI), ni[i], replace = F), ]
  
  muestra_etapa_2 <- rbind(muestra_etapa_2,muestra_hi)
}
#### esta forma se genera el muestreo estratificado con 2 etapas considerando un total de 423 empresas
###verificando resultados
dim(muestra_etapa_2)
n_Usecundarias

table(muestra_etapa_2$NOMBREDI)
#la composicion final de la muestra es la siguiente:
##############################################################################################
#BARRANCO          INDEPENDENCIA              LA MOLINA                   LIMA 
#8                     12                     25                          78 
#LOS OLIVOS        LURIGANCHO                  LURIN              MAGDALENA DEL MAR 
#28                     12                      9                         21 
#PUEBLO LIBRE        PUNTA HERMOSA            PUNTA NEGRA             SAN ISIDRO 
#10                      1                      1                         125 
#SAN JUAN DE LURIGANCHO   SAN LUIS   SAN MARTIN DE PORRES             SAN MIGUEL 
#23                        13                     37                     20
#############################################################################################.
####GENERACION DEL DISEÑO################################
##creando factores de conversion
Mi <- as.vector(table(muestra_etapa_1$NOMBREDI)) #cuantas empresas hay
ni <- as.vector(table(muestra_etapa_2$NOMBREDI)) #cuantos se escogió de mi muestra

muestra_etapa_2$fpc_1 <- rep(N, length(muestra_etapa_2$ID))
muestra_etapa_2$fpc_2 <- c(rep(Mi, ni))

##DISEÑO DE MUESTRAS (METADATA DE DISEÑO)
Designe_Mapc_2 <- svydesign(ids = ~ NOMBREDI + ID,
                            fpc = ~ fpc_1+fpc_2,
                            data = muestra_etapa_2)
##de esta forma se realiza el diseño que nos serviá para realizar en las estimaciones

#####ESTIMACIONES#######################################
                                          ##pregunta B
###estimar la proporcion de empresas que indicaron que sus ventas del anio 2016 Aumentaron (C3 P5)
svymean(~ C3_P5, Designe_Mapc_2, na.rm=T)
prop.table(table(base_P$C3_P5))
#RPTA###########################################################################.
#la proporcion de empresas que indicador que sus ventas del anio 2016 aumentarian fue de :
#0.40618 40.61% con un error standar de 0.0210 o un 2.10% (con la semilla 321)
#################################################################################.

####proporcion de empresas cuyo mayor porcentaje de clientes se encuentra a nivel Nacional - Todo el Per´u (C3 P7).
svymean(~ C3_P7, Designe_Mapc_2, na.rm=T)
prop.table(table(base_P$C3_P7))
#RPTA############################################################################:
#La proporcion de empresas cuyo mayor porcentaje de clientes se encuentra a nivel Nacional - Todo el Peru fue de 
#0.3377 o el 33.77% con un error starndar de 0.00440 o 0.440%
#################################################################################.

                               ###pregunta C
####media del monto de ventas netas anuales que obtuvo en el 2016 (C3 P2)
svymean(~ C3_P2, Designe_Mapc_2, na.rm=T)
mean(na.omit(base_P$C3_P2))
#RPTA############################################################################.
#la media del monto de ventas netas anuales que obtuvo en el 2016, fue de:
#53 541 440 de soles con un error standar de 22 622 655 de soles
#################################################################################

####media del porcentaje de sus ventas netas anuales que fueron por exportaci´on (C3 P4)
svymean(~ C3_P4, Designe_Mapc_2, na.rm=T)
mean(na.omit(base_P$C3_P4))
#RPTA############################################################################.
#La media del porcentaje de sus ventas netas anuales que fueron por exportacion
#el 27.484% con un error standar de 73.325%
#################################################################################.

####Pregunta D
####COMPARAR RESULTADOS DE AMBOS MUESTREOS Y VER CUAL ES EL MAS EXACTO
###estimar la proporcion de empresas que indicaron que sus ventas del anio 2016 Aumentaron (C3 P5)
svymean(~ C3_P5, designe_MAE, na.rm=T) #ESTRATIFICADO
svymean(~ C3_P5, Designe_Mapc_2, na.rm=T) #CONGLOMERADO
prop.table(table(base_P$C3_P5)) #POBLACION

#####RPTA:
#Eatimacion mas precisa a la poblacion : muestreo estratificado, 

####proporcion de empresas cuyo mayor porcentaje de clientes se encuentra a nivel Nacional - Todo el Per´u (C3 P7)
svymean(~ C3_P7, designe_MAE, na.rm=T) #ESTRATIFICADO
svymean(~ C3_P7, Designe_Mapc_2, na.rm=T) #CONGLOMERADO
prop.table(table(base_P$C3_P7)) #POBLACION
#Eatimacion mas precisa a la poblacion : muestreo estratificado, 
#RPTA:
####media del monto de ventas netas anuales que obtuvo en el 2016 (C3 P2)
svymean(~ C3_P2, designe_MAE, na.rm=T)  #ESTRATIFICADO
svymean(~ C3_P2, Designe_Mapc_2, na.rm=T) #CONGLOMERADO
mean(na.omit(base_P$C3_P2)) #POBLACION
#Eatimacion mas precisa a la poblacion : muestreo estratificado, 
#RPTA: 
#Estimacion mas precisa a la poblacion : muestreo estratificado, 

####media del porcentaje de sus ventas netas anuales que fueron por exportaci´on (C3 P4)
svymean(~ C3_P4, designe_MAE, na.rm=T)  #ESTRATIFICADO
svymean(~ C3_P4, Designe_Mapc_2, na.rm=T) #CONGLOMERADO
mean(na.omit(base_P$C3_P4)) #POBLACION
#EStimacion mas precisa a la poblacion : muestreo estratificado, 

###################################################################################.
#Se puede concluir entonces que las estimaciones mas precisas corresponden al muestreo 
##estratificado.Basado en las cercania del resultado poblacional a al error standar que se escuentra en cadda s¿estimacion


                                                                            #  ....fin
