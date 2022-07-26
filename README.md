Este código automatiza el procesamiento de las bases de datos de asegurados que libera el Instituto Mexicano del Seguro Social.  

# Instructivo
Con este código es posible procesar múltiples bases de datos tan solo sustituyendo los siguientes valores: 

###### (*) Número de meses a consultar para cada uno de los años. [Líneas 34-36]
`periodos=c(3,12,6)`

En este caso estamos analizando tres años diferentes: ara el primer año (2020) analizamos solamente el último trimestre, el siguiente año en su totalidad y finalmente el primer semestre del 2022. 

###### (*) Desde qué mes consultar cada año (por ejemplo: julio es 7) 
`meses_iniciales=c(10,1,1)`

Colocamos el número del mes desde el que iniciaremos el análisis por año (por ejemplo: octubre-10).

###### (*) Años
`años=c(2020,2021,2022)`

Finalmente incluimos los años sin  abreviaciones.

###### Combinación año-mes-día. Incluir el directorio 
`file_names= c(paste0("C:/Datasets/IMSS-ASG/asg-",
                     vector_años,"-",vector_meses,"-", vector_dias,".csv"))` 
					 
Solamente es necesario ajustar el directorio para indicar la carpeta de origen de las bases de datos, por ejemplo: **C:/Datasets/IMSS-ASG/**

## Requisitos
Almacenar las bases de asegurados en una misma carpeta. Bases de datos disponibles en http://datos.imss.gob.mx/dataset.

## Opcional

Para poder modificarla operación hay que modificar el código de las líneas [126-133].
```r
  # Almacenamos en la lista de archivos..
  df=df %>% 
    filter(sector_economico_4 %in% claves_subsector4) %>% 
    group_by(sector_economico_4) %>% summarise(n(), sum(as.numeric(asegurados)),
                                               sum(as.numeric(no_trabajadores)))
  
  df=left_join(df, llave, by="sector_economico_4")
  df$periodo=paste0(vector_años[i], "-", vector_meses[i], "-", vector_dias[i]);
```

## Contacto
Sebastián Ocampo Palacios, Enlace en la Gerencia de Estudios Económicos del INFONAVIT. Documentación para los Indicadores del Reporte Trimestral Económico. 
Contacto: socapal@outlook.com con el tema "Asegurados (ASG) IMSS".
