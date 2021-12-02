##############################
#Procesamiento de datos 
#Universidad Alberto Hurtado 
#Carrera de Sociología 
#Opt. Análisis de datos estadísticos en R
#Profesora: Valentina Andrade
#Ayudantes: Dafne Jaime y Nicolás Godoy 
#Alumna: Bárbara Sepúlveda 
#22 de noviembre, 2021
#############################

#1. Carga de librerías
pacman::p_load(tidyverse, sjPlot, haven, 
               srvyr,
               magrittr,
               dyplr)
#2. Carga de base de datos Termómetro Social
TS4 <- read_dta("input/data/TS4.dta")

#3. Selección de variables para nuevo DF

datos_proc <- TS4 %>% 
  select(sexo, edad, tipo_vivienda = b2, est_animo = c2, trat_salud = c3, problemas = c10, factor_TS4)

#Variables seleccionadas
#sexo     #edad
#b2: tipo de vivienda ¿Usted vive en casa o departamento?
#c2: (Dependiende)estado de animo luego de la pandemia COVID-19 ¿cómo evalúa su estado de ánimo actual?
#c3: tratamiento medico durante pandemia ¿usted ha creído que necesita un tratamiento en salud mental con un profesional del área (con psicólogo, médico general o psiquiatra)?
#c10: Por favor, indique con qué frecuencia usted puede hablar de sus problemas con su familia o amigos

#4. Recodificacion variables 

#EDAD en tramos

datos_proc <- datos_proc %>% 
  mutate(edad = as_factor(car::recode(edad, c("18:39='Jovenes';40:59='Adulto'; 60:96='Adulto mayor'"), as.factor = T, levels = c("Jovenes", "Adulto", "Adulto mayor"))))

#Sexo, Tipo vivienda y Tratamiento salud a factor

datos_proc <- datos_proc %>% 
  mutate(sexo = as_factor(sexo),
         tipo_vivienda = as_factor(tipo_vivienda),
         trat_salud = as_factor(trat_salud))

#Tratamiento salud 2 dummy 

datos_proc <- datos_proc %>% 
  mutate(trat_salud2 = if_else(trat_salud == "1. Sí", 1, 0))

#Problemas

datos_proc <- datos_proc %>% 
  mutate(problemas = as.numeric(problemas))

datos_proc<-datos_proc %>%
  mutate(problemas2 = case_when(problemas == 1 ~ "Nunca",
                                problemas == 2 ~ "Nunca",
                                problemas == 3 ~ "A veces",
                                problemas == 4 ~ "Siempre",
                                problemas == 5 ~ "Siempre",
                                 FALSE ~ NA_character_)) # aquí es TRUE no FALSE


##Le indicamos a R que estas variables son de factor para utilizarlas en la regresion lineal
datos_proc <- datos_proc %>% 
  mutate_at(vars(edad, sexo, tipo_vivienda, problemas2, trat_salud), 
            funs(forcats::as_factor(.)))
datos_proc$tipo_vivienda <-  forcats::as_factor(datos_proc$tipo_vivienda)

#5. Guardamos el df procesado 
saveRDS(datos_proc, file = "output/data/datos_proc.rds")
