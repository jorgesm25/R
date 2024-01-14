#---------------------------LIMPIEZA DE DATOS----------------------
#Quitar coma del precio 2,000 -> 2000
library(stringr)
df %>%
  mutate(precio_new = as.numeric(str_remove(precio, ","))) 

#encontrar valores fuera de rango
library(assertive)
assert_all_are_in_closed_range(df$precio, lower = 0, upper = 500) 

#reemplazar los valores fuera de rango por valor o NA
df %>%  
  mutate(precio = #sobre escribe la columna precio ya existente
           replace(precio, precio > 500, NA))


#***************Fechas***************
#verificar si todas la fechas están en pasado
assert_all_are_in_past(df$fecha)   

#encontrar fecha fuera de rango
library(lubridate)
df %>% 
  filter(fecha > today())

#filtras las fechas correctas
df %>% 
  filter(fecha <= today())

#***************Duplicados***************
#Cuantas filas están duplicadas
sum(duplicated(df))

#ver las filas duplicadas
filter(df, duplicated(df))

#eliminar filas duplicadas
df <- distinct(df) 

#encontrar duplicados parcialmente
df_duplicados <- df %>% 
  count(nombre, apellido) %>%
  filter(n > 1)

#ver duplicados parcialmente
credit_scores %>% 
  filter(nombre %in% df_duplicados$nombre,
         apellido %in% df_duplicados$apellido)

df %>% #eliminar duplicados
  distinct(nombre, apellido, .keep_all = TRUE) 

#eliminar duplicados conservando un promedio de las filas duplicadas
df %>% 
  group_by(nombre, apellido) %>%
  mutate(media_credito = mean(credito)) %>%
  distinct(nombre, apellido, .keep_all = TRUE) %>%
  select(-credito)

#--------------CATEGORIAS-------------------------------
#cambia la columna tipo a minúscula (str_to_upper para mayúscula)
df %>%  
  mutate(tipo_new = str_to_lower(tipo)) %>% #cuenta las categorías
  count(type_new)

#elimina espacios antes y después (no en medio)
df %>% 
  mutate(tipo = str_trim(tipo)) %>%
  count(tipo)

#contar categorías
df %>% 
  count(tipo, sort = TRUE)

#colapsar similares en una sola categoria
perros = c("poodle", "labrador", "beagle") 

library(forcats)
df %>% #crea una nueva columna (o sobreescribe) las razas se colapsan en "perro"
  mutate(tipo_new = fct_collapse(tipo, perro = perros))

#--------------CADENAS-------------------------------
#ver tarjertas cuyo numero es "1234-1232-2343-2324"
df %>% 
  filter(str_detect(tarjeta, "-"))

#reemplaza los guiones por espacios
df %>% 
  mutate(tarjeta = str_replace_all(tarjeta, "-", " "))

tarjeta_clean <- df$tarjeta %>% #elimina los -
  str_remove_all("-") %>% #y espacios
  str_remove_all(" ")# de las tarjetas
customers %>% # de "1234-1232-2343-2324" y "1234 1232 2343 2324"
  mutate(tarjeta = tarjeta_clean) #a "1234123223432324"

#medir la longitud de las tarjetas debe ser 16
str_length(df$tarjeta) 

#vemos cuales son distintas de 16
df %>%
  filter(str_length(tarjeta) != 16) 

#iguales a 16
df %>%
  filter(str_length(tarjeta) == 16) 

#para compara que tan similares son las cadenas
library(stringdist)
stringdist("baboon", 
           "typhoon",
           method = "dl") #lcs

stringdist("baboon", "typhoon",
           method = "lcs")

stringdist("baboon", "typhoon",
           method = "jaccard") #entre 0 y 1

#ejemplo donde se usan entradas mal escritas de una encuesta y se corrigen
encuesta <- data.frame(
  city = c("mxico", "exico", "Mxic", "juadalajara", "uadalajara")
)
ciudad <- data.frame( #nombres correctos
  city = c("México", "Guadalajara"))

stringdist_left_join(encuesta, ciudad, by = "city", method = "dl")#resultado "mxico -> México"

#max_dist para controlar la distancia máxima entre las cadenas si es mayor a la asignada devuelve NA
stringdist_left_join(encuesta, ciudad, by = "city", method = "dl", max_dist = 1) 


#------------------------------------------------DATOS FALTANTES------------------------------------------------
# regla general no eliminar datos faltantes si rebasan el %5 de la prueba
library(naniar)
miss_var_summary(df) #devuelve la variable, # NAs y % de NAs
miss_case_summary(df) #N° de filas, #NAs y % de NAs

miss_var_table(df) #NAs, #variables % de NAs
miss_case_table(df) #NAs, N° de filas % de NAs

#Grafica de valores faltantes en cada variable
vis_miss(df) 
gg_miss_var(df)
gg_miss_case(df) #por fila

#Mas formas de graficar
gg_miss_var(df, facet = Residencia)
gg_miss_upset(df)
gg_miss_fct(x = df, fct = Residencia)
gg_miss_span(df, hourly_counts, span_every = 3000)

#buscar valores NA que estén en otro formato
df %>% 
  miss_scan_count(search = list("N/A",
                                "N/a"))

#remplazar este tipo de NAs
df %>%
  replace_with_na(replace = list(variable = c("N/A", "N/a")))

#remplazada por NAs donde las variables son = -99
df %>% 
  replace_with_na_all(condition = ~.x == -99)

df %>%
  replace_with_na_all(condition = ~.x %in% c("N/A", "missing", "na"))

#completar valores faltantes no registrados

#nombre  | tiempo
#Juan    | dia
#Juan    | noche
#Diego   | dia

df %>% 
  tidyr::complete(nombre, tiempo) 

#nombre  | tiempo
#Juan    | dia
#Juan    | noche
#Diego   | dia
#Diego   | NA

df %>%
  tidyr::fill(name)

#nombre  | tiempo
#Juan    | dia
#NA     | noche
#Diego   | dia
#NA      | noche

#crea un df reemplazando valor x !NA o NA
bind_shadow(df) 
#ejemplo 
#nombre  | tiempo | nombre_NA  | tiempo_NA  
#Juan    | dia    |    !NA     |  !NA
#Juan    | NA     |    !NA     |   NA

#a partir de estos se puede hacer estadística de descriptiva
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
