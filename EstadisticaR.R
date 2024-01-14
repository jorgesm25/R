################################################################################
#          			Estadísticas de resumen                        # 
################################################################################
group_by(df, Genero) %>% 
  summarise(
    Recuento = n(),
    Media = mean(Articulos, na.rm =TRUE),
    Mediana = median(Articulos, na.rm =TRUE),
    DesvEstandar = sd(Articulos, na.rm =TRUE),
    Mediana = median(Articulos, na.rm =TRUE),
    RICuartil = IQR(Articulos, na.rm =TRUE)
  )

CrossTable(df$Genero, df$Grado) #tabla cruzada

library(broom)
tab <- df %>%
  select(natarms, party) %>%
  table()
tab

tab %>% #tabla longitudinalmente
  tidy() %>%
  uncount(n)
################################################################################
#                           INFERENCIA ESTADÍSTICA                             # 
################################################################################
#+++++++++++++++++++++++++++++DATOS CATEGORICOS+++++++++++++++++++++++++++++++++

#--------------------- NORMALIDAD------------------
shapiro.test(df$Citas) 
df %>% ggplot(aes(sample = Masculino)) + 
  stat_qq()+
  stat_qq_line() + ggtitle("Gráfico Q-Q masculino") + 
  ylab("Cuantiles muestrales") +
  xlab("Cuantiles teóricos") +
  theme_cowplot()

#------------- INTERVALO DE CONFIANZA CON BOOSTRAP-----------
p_hat <- df %>%
  summarize(prop_doc = mean(Grado == "Doctorado")) %>%
  pull()

boot <- df %>%
  specify(response = Doctorado, success = "Doctorado") %>%
  generate(reps = 500, type = "bootstrap") %>% 
  #Calculamos de proporcion de drs
  calculate(stat = "prop")

#grafica de la densidad nula 
ggplot(boot, aes(x = stat)) +
  geom_density(fill = "blue", alpha = 0.2) +
  theme_minimal() +
  labs( x = "Proporción", y = "Densidad")+
  theme(axis.title = element_text(face = "bold")) 


#desviación estandar
SE <- boot %>%
  summarize(sd(stat)) %>%
  pull()

SE 

c(p_hat - 2 * SE, p_hat + 2 * SE) #IC utilizando formula
quantile(boot$stat, 0.975) #IC con cuartiles 0.975 y .025

#-------------BOOSTRAP PARA PRUEBA DE HIPOTESIS-----------
#Calculamos la diferencia de proporciones entre género
p_hats <- df %>%
  group_by(Género) %>%
  summarize(Doct = mean(Doctorado == "Doctorado")) %>%
  pull()

d_hat <- diff(p_hats)
d_hat #diferencia observada en el conjunto original

#Simulamos 500 conjuntos con h0 como cierta
H_null <- df %>%
  specify(Doctorado ~ Género, success = "Doctorado") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "diff in props", order = c("Masculino", "Femenino"))

#Calculamos un IC del 95% mediante percentiles
alpha <- 0.05
lower <- H_null %>%
  summarize(l = quantile(stat, probs = alpha / 2)) %>%
  pull()
upper <- H_null %>%
  summarize(u = quantile(stat, probs = 1 - alpha / 2)) %>%
  pull()

# Preguntamos si está dentro de los cortes
d_hat %>%
  between(lower, upper)

#graficamos la densidad nula para comparar la diferencia observada
#con el tipo de diferencias que se obtienen con h0 como cierta

ggplot(H_null, aes(x = stat)) +
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(xintercept = d_hat, color = "red") +
  geom_vline(xintercept = lower, color = "blue") +
  geom_vline(xintercept = upper, color = "blue") +
  ylab("Densidad") +
  xlab("Diferencia de proporciones") +
  theme_minimal() +
  labs(x = "Diferencia de proporciones", y = "Densidad", color = "") +
  # Agregar etiquetas a las líneas verticales
  geom_segment(aes(x = d_hat, y = 0, xend = d_hat, yend = max(H_null$stat), color = "Diferencia \n observada"), size = 1) +
  geom_segment(aes(x = lower, y = 0, xend = lower, yend = max(H_null$stat), color = "Intervalo de \n confianza \n del 95%"), size = 1) +
  scale_color_manual(values = c("Diferencia \n observada" = "red", "Intervalo de \n confianza \n del 95%" = "blue"))+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.title = element_text(face = "bold"))

#-------------X²-squared BOOTSTRAP-----------
#X²=∑(observed counts − expected counts)² / expected counts

null_spac <- df %>% #df con una estadística de X² asociada bajo h0 de que estas variables no están relacionadas
  specify(natspac ~ party) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 500, type = "permute") %>%
  calculate(stat = "Chisq")

ggplot(null_spac, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = chi_obs_spac, color = "red")

# p-value una cola
null_arms %>%
  summarize(pval = mean(stat <= chi_obs_arms)) %>% pull()

#-------------BONDAD DE AJUSTE bootstrap-----------
ggplot(df, aes(x = party)) +#proporciones observadas
  geom_bar() +
  geom_hline(yintercept = 149/3, color = "goldenrod", size = 2)#proporciones esperadas

tab <- df %>%
  select(party) %>%
  table()
tab

#prueba de bondad de ajuste
p_uniform <- c(Dem = 1/3, Ind = 1/3, Rep = 1/3)
chisq.test(tab, p = p_uniform)$stat

sim_1 <- df %>%
  specify(response = party) %>%
  hypothesize(null = “point”, p = p_uniform) %>%
  generate(reps = 1, type = "simulate")

ggplot(sim_1, aes(x = party)) +
  geom_bar()

#bonda de ajutes para la ley de Benford 
p_benford <- structure(c(0.30103000, 0.17609126, 0.12493874, 0.09691001, 0.07918125, 0.06694679, 0.05799195, 0.05115252, 0.04575749), names = 1:9)
p_benford

# Estadística observada
chi_obs_stat <- iran %>%
  infer::chisq_stat(response = first_digit, p = p_benford)

null <- iran %>%
  # respuesta
  specify(response = first_digit) %>%
  # establecer H0
  hypothesize(null = "point", p = p_benford) %>%
  # Generar 500 conjuntos
  generate(reps = 500, type = "simulate") %>%
  # Calcular la estadistica
  calculate(stat = "Chisq")

# calcular los grados de libertad
degrees_of_freedom <- iran %>% 
  pull(first_digit) %>% 
  nlevels() - 1

# distribucion nula
ggplot(null, aes(x = stat)) +
  geom_density() +
  # estadistica observada
  geom_vline(xintercept = chi_obs_stat, color = "red") + 
  # agregar la x² aproximada 
  stat_function(fun = dchisq, args = list(df = degrees_of_freedom), color = "blue")

# Permutacion p-value
null %>%
  summarize(pval = mean(stat >= chi_obs_stat))

# Approximacion p-value
pchisq(chi_obs_stat, df = degrees_of_freedom, lower.tail = FALSE)

#----------------------------ODDS RATIO---------------------------------------
CrossTable(df$Género, df$Doctorado) #obtener los valores
#tabla de contingencia para la prueba
tabla <- matrix(c(100, 611, 106, 255), nrow = 2, byrow = TRUE)
colnames(tabla) <- c("NoDoc", "Doctorado")
rownames(tabla) <- c("Masculino", "Femenino")

tabla

# Realiza la prueba de odds ratio
resultado_doc <- oddsratio(tabla)

# Muestra los resultados
print(resultado_doc)

#convertir la tabla de contingencia en un df para ggplot2
df <- data.frame(
  Género = rep(c("Masculino", "Femenino"), each = 2),
  Grado = rep(c("No doctorado", "Doctorado"), times = 2),
  Counts = c(100, 611, 106, 255)
)

# agrega una columna de las proporciones
df <- df %>%
  group_by(Género) %>%
  mutate(Proporción = prop.table(Counts) * 100)

df$Género <- factor(df$Género, levels = c("Masculino", "Femenino"))

# Crear el gráfico
ggplot(df, aes(x = Género, y = Proporción, fill = Grado)) +
  geom_bar(stat = "identity", position = "stack", width = .995) +
  geom_text(aes(label = sprintf("%.1f%%", Proporción), fontface = "bold"), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("No doctorado" = "#f70000", "Doctorado" = "#01f702")) +
  labs(
    x = "Género",
    y = "Proporción",
    title = "Grado académico por género",
    fill = "Grado académico"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 15, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(label.theme = element_text(face = "bold")))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++DATOS NUMERICOS++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#bootstrap para IC datos numericos
rent_med_ci <- df %>%
  specify(response = rent) %>%  
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "median")

str(rent_med_ci)

ggplot(rent_med_ci, aes(x = stat)) +
  geom_histogram(binwidth = 50)

#intervalo de confianza
rent_med_ci %>%
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  ) 


#bootstrap para test datos numericos
n_replicates <- 15000
rent_med_ht <- df %>%
  specify(response = rent) %>%
  hypothesize(null = "point", med = 2500) %>% 
  generate(reps = n_replicates, type = "bootstrap") %>% 
  calculate(stat = "median")
rent_med_obs <- df %>%
  summarize(median_rent = median(rent)) %>%
  pull()

# Calcular el  p-value (cola superior)
rent_med_ht %>%
  filter(stat >= rent_med_obs) %>%
  summarize(p_val = n() / n_replicates)

# Calcular el  p-value dos colas
weight_mean_ht %>%
  filter(stat >= weight_mean_obs) %>%
  summarize(
    one_sided_p_val = n() / n_replicates,
    two_sided_p_val = 2 * one_sided_p_val
  )

#Grafica de la densidad nula
rent_med_ht %>%
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  ) 

l <- 2312
u <- 3025
ggplot(rent_med_ht, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = rent_med_obs, color = "red") +
  # Add vertical blue line for lower cutoff
  geom_vline(xintercept = l, color = "blue") +
  # Add vertical blue line for upper cutoff
  geom_vline(xintercept = u, color = "blue")

#---------------------intervalo de confianza para media con T---------------
t.test(gss$moredays, conf.level = 0.95) #calcula la media en un IC del 95%

#T-IC para datos pareados
boxplot(df$read, df$write, names = c("Read", "Write")
diff = students$read - students$write
t.test(hsb2$diff, conf.level = 0.95)
#Diferencias significativas entre las medias de read y write
t.test(hsb2$diff, null = 0, alternative = "two.sided")

#---------------------graficar T-test, Mann-Whitney---------------
library(ggstatsplot)

ggbetweenstats(
data = df,
x = Género, # Variable de agrupación/independiente
y = Numero, # Variable dependiente
xlab = "Género", # etiqueta para el eje X
ylab = "Número de investigadores/as", # etiqueta para el eje y
type = "np",#eliges el tipo de prueba "p" (parametrica), "np" (no parametrica), "r" (robusta), "bf" (Bayes Factor).
effsize.type = "g", #el tipo de estimador de efecto además de "d" está "g", "r" y la robusta que viene por defecto si eliges la robusta.
conf.level = 0.99,
plot.type = "box", #"box" o "violin" o ambas
outlier.tagging = TRUE, # Te indica cuales son outliers
outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
outlier.label = IIEF, # La etiqueta que va a tener los outliers
outlier.label.args = list(color = "red"), # la etiqueta de los outlairs en rojo
messages = FALSE, # apagar los mensajes
ggtheme = ggplot2::theme_gray(), # cambiar el fondo de la gráfica todos los temas en https://ggplot2.tidyverse.org/reference/ggtheme.html
package = "yarrr", # El paquete asociado a la paleta de colores.
palette = "info2", # Elegir la paletta dentro del paquete 
#title = "Género ~ Cantidad",
#caption = "visualización"
)+ scale_color_manual(values=c("#E75480", "blue"), guide = "none")+
theme_minimal()+
theme(axis.text.x = element_text(size = 17, face = "bold"))+
theme(axis.title = element_text(size = 17, face = "bold"))+
theme(
plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
axis.text.x = element_text(face = "bold"),
axis.title = element_text(face = "bold"),
legend.title = element_text(face = "bold"),
legend.text = element_text(face = "bold")
)
        
#Con R base dependiendo de la normalidad
wilcox.test(Numero~Género, data = df, exact = FALSE) 
t.test(Numero~Género, data = df, exact = FALSE) 
        
#-----------------RANGO CON SIGNO DE WILCOXON DE UNA MUESTRA------------------------
gghistostats(
data = df, # dataframe
x = Femenino, # variable
type = "nonparametric", # nonparemetric = Wilcoxon, parametric = t-test
test.value = 0.50, # valor a comparar
centrality.line.args = list(color = "black", linewidth = 1, linetype = "dashed"),
bin.args = list(color = "magenta", fill = "magenta", alpha = 0.4),
ggtheme = ggplot2::theme_gray(),
caption = "Visualización"
)+
theme_minimal()+
theme(axis.title = element_text(size = 14, face = "bold")) 

#con R base
wilcox.test(df$Femenino, mu = 0.50)
t.test(df$Femenino, mu = 0.50)

#------------------PROGRAMACIÓN PARALELA-------------------
library(parallel)
#-----Numero de nucleos------
nucleos <- detectCores()
nucleos

#------funciones en paralelo-----
# determinar numero de nucleos
detectCores()

# crear clusters via makeCluster
cluster <- makeCluster(2)

# aplicar en paralelo
parApply(cluster, df, 2, median)

# detener el cluster
stopCluster(cluster)

#-----Simulación en paralelo----- 
library("parallel")

#Crear la simulación
play <- function() {
  total <- no_of_rolls <- 0
  while(total < 10) {
    total <- total + sample(1:6, 1)
    
    # If even. Reset to 0
    if(total %% 2 == 0) total <- 0 
    no_of_rolls <- no_of_rolls + 1
  }
  no_of_rolls
}

# Crear el cluster
cluster <- makeCluster(2)

# Exportar la funcion play() al cluster
clusterExport(cluster, "play")

# pasarlo a paralelo 
res <- parSapply(cluster, 1:100, function(i) play())

# detener el cluster
stopCluster(cluster)
