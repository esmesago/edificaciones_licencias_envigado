### censo de edificaciones ####

### # Limpieza area de trabajo 
rm(list=ls())
cat('\014')

# cargar base de datos 
library(readxl)
Historico_licenciamiento_Envigado <- read_excel("Documents/Ejercicios de Rstudio/TendenciaLinceciamientoyedificacionesEnvigado/Historico de licencias /Historico licenciamiento Envigado.xlsx")
View(Historico_licenciamiento_Envigado)

# instalar paquetes
## install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)
## require/install packages on this session
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       ggplot2, # plotting
       skimr) # summary data 

# limpieza de datos
# inspeccionar base de datos 

glimpse(Historico_licenciamiento_Envigado) # reporte especifico de la base de datos 
skim(Historico_licenciamiento_Envigado) %>% head()

# seleccionamos los datos que se refieren a destinaciones diferentes a la vivienda,eliminamos variabel unidades
# seleccionamos las variables que nos interesan y cambias nombres de variables para 
# mejor manejo 

df_no_viv <- Historico_licenciamiento_Envigado %>%
  filter(destino > 1) %>%
  select(!cod_depto:cod_muni) %>%
  select(-unidades, -estrato, -tipo_vivi, -VIS_NVIS, -cobertura) %>%
  rename(tramite = obj_tra)

# creamos variable de fecha para resumir por mes 

df_no_viv <-  df_no_viv %>% 
  mutate(fecha = as.Date(paste(año, mes, "01", sep = "-")))

# tratar las variables categoricas

df_no_viv <- df_no_viv %>% 
  mutate(tramite = as.factor(tramite),
         clase_suelo = as.factor(clase_suelo),
         modalidad = as.factor(modalidad),
         destino = as.factor(destino),
         area = as.numeric(area))

# asignar nombres a las categorias de las variables 
# tramite 
c_tramite <- c("nuevo", "ampliación")
c_clase_suelo <- c("urbano", "suburbano","rural")
c_modalidad <- c("nueva", "ampliación")
c_destino <- c("industria","oficina","bodega","comercio","alojamiento","educación",
               "salud","admón pública","religioso","social-recreo","otros")

df_no_viv <- df_no_viv%>%
  mutate(tramite = factor(tramite, levels = unique(tramite), labels = c_tramite),
         clase_suelo = factor(clase_suelo, levels = unique(clase_suelo), labels = c_clase_suelo),
         modalidad = factor(modalidad, levels = unique(modalidad), labels = c_modalidad),
         destino = factor(destino, levels = unique(destino), labels = c_destino))
    
# identificar el promedio de área licenciada 

# clase del suelo y modalidad 

 df_no_viv %>% 
  group_by(clase_suelo, modalidad) %>%
  summarize(
    mean_area = mean(area),
    n = n(),
    )

 #  clase del suelo y destino 
 df_no_viv %>% 
   group_by(clase_suelo, destino) %>%
   summarize(
     mean_area = mean(area),
     n = n(),
   )

# identificamos los cuartiles del areá por clase del suelos para definir tipos de proyectos 
 
 df_no_viv <- df_no_viv %>%
   group_by(clase_suelo) %>%
   mutate(cuartil_area = cut(area,
                             breaks = quantile(area, probs= c(0,0.25,0.5,0.75,1)),
                             labels = c("Q1","Q2","Q3","Q4"),
                             include.lowest = T)) 
  
 df_no_viv %>%
  group_by(clase_suelo,cuartil_area) %>%
   summarize(
     Min = min(area),
     Max = max(area),
     n= n()
   )

 hist(df_no_viv$area, 
      main = "Histograma de area licenciada",  # Título del histograma
      xlab = "area",              # Etiqueta del eje x
      ylab = "Frecuencia",           # Etiqueta del eje y
      col = "lightblue",            # Color de las barras
      border = "black",             # Color del borde de las barras
      breaks = 50                    # Número de intervalos (barras)
 )   
   
 # Crear un gráfico de dispersión de valor vs. fecha
 

 library(ggplot2)
 
 # Crear el gráfico de líneas
 ggplot(data = df_no_viv, aes(x = fecha, y = area , fill= clase_suelo)) +
   geom_area() +
   labs(title = "area no residencial licenciada por clase de suelo 2005-2023",
        x = "Año",
        y = "Aréa") +
   scale_color_discrete(name = "clase_suelo") +
   theme_minimal()
   
   
