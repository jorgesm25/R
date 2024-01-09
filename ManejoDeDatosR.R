
head(df) #6 primeras columnas del df
glimpse(df) #resumen del tipo de variables 
dim(df) #filas  x columnas del df
colnames(df) #columnas del df
summary(df) #resumen estadístico de las variables numéricas
rm(Universidades) #rm se usa para borrar una variable o df
class(df$columna) #tipo de dato 
df %>% group_by(universidad) %>% count() #contar por universidad
df %>% count(universidad) #contar por universidad

#--------------------------SELECT--------------------------
df %>% select(nombre, Genero) #selecciona colunmas
df %>% select(-universidad) #borra un columna
df %>% select(-c(universidad, salario)) #borrar 2 o + columnas
df %>% select(where(is.numeric)) #selecciona numéricas (is.character, is.factor)
df %>% select(ends_with("s")) #Selecciona las columnas que inicien con S (ends_with)
df %>% select(contains("dad")) #Selecciona las columnas que contienen "dad"

#--------------------------FILTER--------------------------
df %>% filter(salario > 1000) 
df %>% filter(universidad == "UNAM") 
df %>% filter((universidad == "UNAM" & salario >= 5533)) #con 2 condiciones se usa & las condiciones van entre ()
df %>% filter(universidad == "UNAM", salario >= 5533) # lo mismo de arriba
df %>% filter((universidad == "UNAM" | salario >= 5533)) # | significa o, o es unam o mayor igual a 5533
#select + filter
df %>% select(nombre, salario) %>% filter(salario == min(salario)) #seleciona solo las columnas que quieres y las filtra

#--------------------GROUP BY --------------------  
df %>% group_by(universidad) %>% count() %>% arrange(n) #agrupa cuantos tienes por uni con desc(n) de mayor a menor
df %>% group_by(Genero) %>% summarise(mean(salario)) #agrupar y sacar estadisitca
#cuando usas groupby o cualquiera de dplyr debe ir con summarise
mean(df$salario) #solo el valor sirven sin summarise

#Cuenta cuantas mujeres y hombres tiene por universidad y el total
df %>%
  group_by(Universidad, Género) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = Género, values_from = n, values_fill = 0) %>%
  mutate(Total = Femenino + Masculino)

#Suma por universidad
df %>%                                         
  group_by(Universidad) %>%                        
  summarise_at(vars(Articulos),                 
               list(TotalDeArticulos = sum))
#---------------------NOMBRE DE FILAS -----------------------
df <- df %>% column_to_rownames(var = "nombre") #Hace que la columna que pongas en "var" sean etiquetas de fila
df <- df %>% rownames_to_column(var = "nombre") #regresa las etiquetas a columna

#-----------------------RENOMBRAR COLUMNAS -----------------------
df %>% rename(NombreNuevo = NombreActual) #renombra las columnas 
df %>% select(NombreNuevo = NombreActual, Genero, universidad) #lo mismo
colnames(df) <- c("País", "Género","universidad")

#------------------- MUTATE------------------------------
df %>% mutate(SalarioDia = salario/30) #Crea una nueva columna diviendo la salario/30
df_new <- df %>% transform(NivelSalario = case_when(  
  salario < 3000 ~ "Bajo",                           
  salario >= 3000 & salario <= 7000 ~ "Medio",
  salario > 7000 ~ "Alto",
))

orden <- c("Bajo", "Medio", "Alto")
df_new$NivelSalario <- factor(df_new$NivelSalario, levels = orden)

df %>% 
  transform(Grado = as.factor(Grado) #también se puede con mutate
            
#----------------------------EDITAR CELDAS---------------------------
df_new %>% mutate(universidad = if_else(universidad == "UNAM", "UNAM C.U", universidad))

#----------------------------SEPARAR/UNIR COLUMNAS------------------------------
df_separado <- df %>%
  separate(nombre_completo, into = c("nombre", "apellido"), sep = " ") #puede ser sep =,.etc
#Unir
df %>%
  unite(columna_combinada, col1, col2, sep = "_")
            
#-----------------------AGREGAR COLUMNA EN BASE A OTRA------------------------------
#si la columna grado tiene un nombramiento de mujer se agrega femenino si es de hombre masculino, otro NA
df$Género <- ifelse(grepl("^DRA.|^MTRA.", df$Grado), "Femenino",
                    ifelse(grepl("^DR.|^MTRO.|^LIC", df$Grado), "Masculino", NA))
            
#---------------------------Juntar 3 columnas en una------------------------
#si el 2do apellido esta vacio solo deja el nombre y 1er apellido
df <- df %>% select(Nombre, PApellido, SApellido,Género, Areas, Residencia, Universidad, GScholar, ResearchG)
df$Nombre <- paste(df$Nombre, ifelse(is.na(df$PApellido), "", df$PApellido), ifelse(is.na(df$SApellido), "", df$SApellido))
            
#----------------------------JOINS------------------------------
df_personas <- data.frame(
  nombre = c("Alice", "Bob", "Charlie", "David"),
  trabajo = c("Ingeniera", "Doctor", "Profesor", "Estudiante")
  )
            
df_universidades <- data.frame(
  nombre = c("Alice", "Bob", "Eve"),
  universidad = c("MIT", "Harvard", "Stanford")
  )

df_personas
df_universidades
            
df_personas %>% left_join(df_universidades) 
df_personas %>% right_join(df_universidades)
df_personas %>% full_join(df_universidades)
df_universidades %>% anti_join(df_personas) #los que estan en un df y no en otro
            
#----------------------------SLICE MAX------------------------------
df %>% #regresa el mayor por grupo
  group_by(Genero) %>%
  slice_max(salario, n = 1)
            
df %>% #regresa el menor por grupo
  group_by(Genero) %>%
  slice_min(salario, n = 1)
            
#---------------------PASAR A DATOS LONGITUDINALES------------------------
dfLong <- melt(df, id.vars = "Pais") #variable pivote
#de esto 
#Pais      | Femenino  |Masculino
#Colombia  |  0.240    | 0.760
#México    |  0.199    | 0.801
##a esto 
#País     | Género    | Proporción
#Colombia | Femenino  | 0.2395833
#México   | Femenino  | 0.1987385
#Colombia | Masculino |  0.7604167
#México   | Masculino |  0.8012615
            
#para revertir
pivot_wider(df, names_from = "Género", values_from = "Proporción")
            
#------------------------Exportar a POWERPOINT------------------------
library(officer)
obj <- rvg::dml(ggobj = imagen)

officer::read_pptx() %>% 
  # añadir diapositiva ----
officer::add_slide() %>%
# especificar objeto y lugar ----
officer::ph_with(obj, ph_location()) %>%
# exportar diapositiva -----
base::print("nombredelarchivo.pptx")
            
#---------------------------EXPORTAR DATAFRAMES------------------------
write.csv(x= df, file = "/home/ruta/df.csv") #csv
write.xlsx(x= df, file = "/home/ruta/df.xlsx") #excel

#---------------------------TABLA LATEX------------------------
print(xtable(df, type = "latex"))
            
#---------------------------TABLA HTML------------------------
# Generar el código HTML
codigo_html <- "<table>\n"
codigo_html <- paste0(codigo_html, "<thead>\n<tr>\n<th>Nombre</th>\n<th>Género</th>\n<th>linea1</th>\n<th>Residencia</th>\n<th>UTrabajo</th>\n<th>Perfil</th>\n</tr>\n</thead>\n")
codigo_html <- paste0(codigo_html, "<tbody>\n")

for (i in 1:nrow(BasePagina)) {
  codigo_html <- paste0(codigo_html, "<tr>\n")
  codigo_html <- paste0(codigo_html, "<td>", BasePagina$Nombre[i], "</td>\n")
  codigo_html <- paste0(codigo_html, "<td>", BasePagina$Género[i], "</td>\n")
  codigo_html <- paste0(codigo_html, "<td>", BasePagina$linea1[i], "</td>\n")
  codigo_html <- paste0(codigo_html, "<td>", BasePagina$Residencia[i], "</td>\n")
  codigo_html <- paste0(codigo_html, "<td>", BasePagina$UTrabajo[i], "</td>\n")
  
  link <- BasePagina$Perfil[i]
  if (!is.na(link)) {
    codigo_html <- paste0(codigo_html, "<td><a href=\"", link, "\" target=\"_blank\">", link, "</a></td>\n")
  } else {
    codigo_html <- paste0(codigo_html, "<td></td>\n")
  }
  
  codigo_html <- paste0(codigo_html, "</tr>\n")
}


codigo_html <- paste0(codigo_html, "</tbody>\n</table>")

# Guardar el código HTML en un archivo
writeLines(codigo_html, "tabla.html")