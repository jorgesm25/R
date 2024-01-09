#---------------------------LIMPIEZA DE DATOS----------------------
library(stringr)
df %>%
  mutate(precio_new = as.numeric(str_remove(precio, ","))) #Quitar coma del precio 2,000 -> 2000

library(assertive)
assert_all_are_in_closed_range(df$precio, lower = 0, upper = 500) #encontrar valores fuera de rango

df %>% #reemplazar los valores fuera de rango por valor o NA 
  mutate(precio = #sobre escribe la columna precio ya existente
           replace(precio, precio > 500, NA))


#Fechas
assert_all_are_in_past(df$fecha) #verificar si todas la fechas están en pasado  

library(lubridate)
df %>% #encontrar fecha fuera de rango
  filter(fecha > today())

df %>% #filtras las fechas correctas
  filter(fecha <= today())

#Duplicados
sum(duplicated(df)) #Cuantas filas están duplicadas
filter(df, duplicated(df)) #ver las filas duplicadas
df <- distinct(df) #eliminar filas duplicadas


df_duplicados <- df %>% #encontrar duplicados parcialmente
  count(nombre, apellido) %>%
  filter(n > 1)

credit_scores %>% #ver duplicados parcialmente
  filter(nombre %in% df_duplicados$nombre,
         apellido %in% df_duplicados$apellido)

df %>% #eliminar duplicados
  distinct(nombre, apellido, .keep_all = TRUE) 

df %>% #eliminar duplicados conservando un promedio de las filas duplicadas
  group_by(nombre, apellido) %>%
  mutate(media_credito = mean(credito)) %>%
  distinct(nombre, apellido, .keep_all = TRUE) %>%
  select(-credito)

#--------------CATEGORIAS-------------------------------
df %>% #cambia la columna tipo a minúscula (str_to_upper para mayucula) 
  mutate(tipo_new = str_to_lower(tipo)) %>% #y cuenta las categorias
  count(type_new)

df %>% #elimina espacios antes y despues (no en medio)
  mutate(tipo = str_trim(tipo)) %>%
  count(tipo)

df %>% #contar categorias
  count(tipo, sort = TRUE)

perros = c("poodle", "labrador", "beagle") #colapsar similares

library(forcats)
df %>% #crea una nueva columna (o sobreescribe) las razas se colpasan en "perro"
  mutate(tipo_new = fct_collapse(tipo, perro = perros))

#--------------CADENAS-------------------------------
df %>% #ver tarjertas cuyo numero es "1234-1232-2343-2324"
  filter(str_detect(tarjeta, "-"))

df %>% #reemplaza los guiones por espacios
  mutate(tarjeta = str_replace_all(tarjeta, "-", " "))

tarjeta_clean <- df$tarjeta %>% #elimina los -
  str_remove_all("-") %>% #y espacios
  str_remove_all(" ")# de las tajeras
customers %>% # de "1234-1232-2343-2324" y "1234 1232 2343 2324"
  mutate(tarjeta = tarjeta_clean) #a "1234123223432324"

str_length(df$tarjeta) #medir la longitud de las tajetas debe ser 16

df %>%
  filter(str_length(tarjeta) != 16) #vemos cuales son distintas de 16

df %>%
  filter(str_length(tarjeta) == 16) #iguales a 16

library(stringdist)
stringdist("baboon", #para compara que tan similares son las cadenas
           "typhoon",
           method = "dl") #lcs

stringdist("baboon", "typhoon",
           method = "lcs")

stringdist("baboon", "typhoon",
           method = "jaccard") #entre 0 y 1

#ejemplo donde se usan entradas mal estritas de una encuesta y se corrigen
encuesta <- data.frame(
  city = c("mxico", "exico", "Mxic", "juadalajara", "uadalajara")
)
ciudad <- data.frame( #nombres correctos
  city = c("México", "Guadalajara"))

stringdist_left_join(encuesta, ciudad, by = "city", method = "dl")#resultado "mxico -> México"

#max_dist para controlar la distancia maxima entre las cadenas si es mayor a la asignda devuelve NA
stringdist_left_join(survey, cities, by = "city", method = "dl", max_dist = 1) 


#------------------------------------------------DATOS FALTANTES------------------------------------------------
# regla general no eliminar datos faltantes si rebasan el %5 de la prueba
library(naniar)
miss_var_summary(df) #devuelve la variable, # NAs y % de NAs
miss_case_summary(df) ##fila, #NAs y % de NAs

miss_var_table(df) #NAs #variables % de NAs
miss_case_table(df) #NAs #filas % de NAs

vis_miss(df) #Grafica de valores faltantes en cada variable
gg_miss_var(df)
gg_miss_case(df) #por fila

#Mas formas de graficar
gg_miss_var(df, facet = Residencia)
gg_miss_upset(df)
gg_miss_fct(x = df, fct = Residencia)
gg_miss_span(df, hourly_counts, span_every = 3000)

df %>% #buscar valores NA que estén en otro formato
  miss_scan_count(search = list("N/A",
                                "N/a"))

#remplazar este tipo de NAs
df %>%
  replace_with_na(replace = list(variable = c("N/A", "N/a")))

df %>% #remplazada por NAs donde las variables son = -99
  replace_with_na_all(condition = ~.x == -99)

df %>%
  replace_with_na_all(condition = ~.x %in% c("N/A", "missing", "na"))

#valores faltantes no registrados

nombre  | tiempo
Juan    | dia
Juan    | noche
Diego   | dia

df %>% #valores faltantes no registrados
  tidyr::complete(nombre, tiempo) 

nombre  | tiempo
Juan    | dia
Juan    | noche
Diego   | dia
Diego   | NA

df %>%
  tidyr::fill(name)

nombre  | tiempo
Juan    | dia
NA     | noche
Diego   | dia
NA      | noche

bind_shadow(df) #crea un df reemplazando valor x !NA o NA
#ejemplo 
nombre  | tiempo | nombre_NA  | tiempo_NA  
Juan    | dia    |    !NA     |  !NA
Juan    | NA     |    !NA     |   NA

#apartir de estos se puede hacer estadistica de descriptiva
df %>%
  bind_shadow() %>%
  group_by(Ozone_NA) %>%
  summarize(mean = mean(Wind))

ggplot(df,
       aes(x = Temp)) +
  geom_density()

df %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp,
             color = Ozone_NA)) +
  geom_density()

df %>%
  bind_shadow() %>%
  ggplot(aes(x = Ozone_NA,
             y = Temp)) +
  geom_boxplot()

df %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp)) +
  geom_density() +
  facet_wrap(~Ozone_NA)

df %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp,
             y = Wind)) +
  geom_point() +
  facet_wrap(~ Ozone_NA)

df %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp,
             y = Wind,
             color = Ozone_NA)) +
  geom_point()

df %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp,
             color = Ozone_NA)) +
  geom_density()
+
  facet_wrap(~ Solar.R_NA)

ggplot(df,
       aes(x = Ozone,
           y = Solar.R)) +
  geom_miss_point() #ver los valores NA que por defecto excluye ggplot

ggplot(df,
       aes(x = Wind,
           y = Ozone)) +
  geom_miss_point() +
  facet_wrap(~ Month)

df %>%
  bind_shadow() %>%
  ggplot(aes(x = Wind,
             y = Ozone)) +
  geom_miss_point() +
  facet_wrap(~ Solar.R_NA)

#imputar NAs
impute_below_if(data, is.numeric)
impute_below_at(data, vars(var1,var2))
impute_below_all(data)
bind_shadow(df) %>% impute_below_all()
