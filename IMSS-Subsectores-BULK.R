#******************************************************************************#
#Nombre: Instituto Mexicano del Seguro Social (Asegurados) - BULK     
#Fecha de última actualización: 25/7/2022                              
#Proyecto: Reporte trimestral (INFONAVIT)   
#Creador: Sebastián Ocampo Palacios 
#******************************************************************************#

# 1. Librerías  -----------------------------------------------------------------------

rm (list=ls())
start=Sys.time()
paquetes=c("dplyr","tidyr", "beepr")
sapply(paquetes, 
       function (x){
         if(! x %in% rownames(installed.packages()))
           install.packages(x)
         require(x, character.only=T)
       })

# Encripción y directorio (bases de datos)
Sys.setlocale("LC_ALL", "ES_ES.UTF-8")
setwd("~/Chambas/INFONAVIT/RESPONSABILIDADES/IMSS")

# 1. Librerías y bases de datos -------------------------------------------
## Intentar mejorar con webscrapping

#Tiempo
start=Sys.time() # Toma alrededor de cinco minutos.

# 1.1 Bases de datos
# Esta sección nos permite abrir los archivos, pero es **NECESARIO** especificar
## los años y meses que nos interesa analizar. Marcados con (*)
## Bajo la siguiente estructura  "asg-aaaa-mm-dd"

## (*) Número de meses a consultar para cada uno de los años (especificar todos)
periodos=c(3,12,6)

## (*) Desde qué mes consultar cada año (por ejemplo: julio es 7) 
meses_iniciales=c(10,1,1)

## (*) Años
años=c(2020,2021,2022)

# Acá obtenemos una repetición de cada año según el total de periodos *AUTOMÁTICO*
vector_años=vector()
for (i in 1:length(años)){
  vector_años=append(vector_años, rep(años[i], periodos[i]))
}
##Meses
# Vector de meses:
## a cada mes inicial hay sumarle 0+1+2+...+(total_periodos-1).... *AUTOMÁTICO*
vector_meses=c()
for (i in 1:length(periodos)){
  vector_meses=append(vector_meses,
                      meses_iniciales[i]+(1:periodos[i]-1)
  )
}

##Días
fines_mes=c(31,28,31,30,31,30,31,31,30,31,30,31) #Días al final del mes (no bisiesto)
fines_mesbisesto=c(31,29,31,30,31,30,31,31,30,31,30,31)

# Nos ofrece el vector de días de acuerdo a periodos y meses iniciales AUTOMÁTICO
vector_dias=c()
for (i in 1:length(años)) {
  if ((años[i]/4==round(años[i]/4))==TRUE){
    #Si es bisiesto entonces....
    tijera=meses_iniciales[i]+(1:periodos[i]-1);
    vector_dias=append(vector_dias, fines_mesbisesto[tijera])
    }
    #No bisiesto entonces...
  else{
    tijera=meses_iniciales[i]+(1:periodos[i]-1);
    vector_dias=append(vector_dias, fines_mes[tijera])
    }
}

# Damos formato a los meses....
vector_meses=sprintf("%02d", vector_meses)

# Nos arroja cada combinación año-mes-día en el formato
file_names= c(paste0("C:/Datasets/IMSS-ASG/asg-",
                     vector_años,"-",vector_meses,"-", vector_dias,".csv") )
  
# Código para abrir cada base de datos... Alrededor de 30 minutos para 21 archivos.
lista_archivos=list()
start=Sys.time()
for(i in 1:1){
  df= read.csv(file_names[i],
              colClasses=c('character'),
              na.strings=c(""," ","NA"),
              sep = "|");
  # Almacenamos en la lista de archivos...
  lista_archivos[[i]]=df
}
  
Sys.time()-start 
# 2. Procesamiento de datos -----------------------------------------------

#Vector de claves para nuestro interés
claves_subsector4=c("4101","4102", "4201", "4202", "4203", "4204")
descriptores_subsector4=c("Edificación (excepto obra pública)",
  "Infraestructura y obra pública",
  "Instalaciones sanitarias",
  "Ascensores y transporte",
  "Herrería",
  "Otros")
llave=data.frame(sector_economico_4=claves_subsector4,descriptores_subsector4)

# Sample (ejemplo: procesar *1* archivo) para entender el funcionamiento.
s=as.data.frame(lista_archivos[1])
s=s %>% filter(sector_economico_4 %in% Claves_subsector4) %>% group_by(sector_economico_4) %>% summarise(n())

s=left_join(s, llave, by="sector_economico_4")
s$periodo=paste0(vector_años[1], "-", vector_meses[1], "-", vector_dias[1])

# Proceso final.

lista_archivos=list()
start=Sys.time()
for(i in 1:length(file_names)){
  df= read.csv(file_names[i],
               colClasses=c('character'),
               na.strings=c(""," ","NA"),
               sep = "|");
  # Almacenamos en la lista de archivos..
  df=df %>% 
    filter(sector_economico_4 %in% claves_subsector4) %>% 
    group_by(sector_economico_4) %>% summarise(n(), sum(as.numeric(asegurados)),
                                               sum(as.numeric(no_trabajadores)))
  
  df=left_join(df, llave, by="sector_economico_4")
  df$periodo=paste0(vector_años[i], "-", vector_meses[i], "-", vector_dias[i]);
  
  # Almacena los datos procesados en una lista...
  lista_archivos[[i]]=df
}
Sys.time()-start

IMSS_SUBSECTORES=do.call("rbind", lista_archivos); beep()
# (*) write.csv(IMSS_SUBSECTORES, "IMSS_SUBSECTORES220722.csv")
