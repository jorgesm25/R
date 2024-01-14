#----------------------Para girar etiquetas--------------------
P + theme(axis.text = element_text(angle = 90))

#----------------- Grafica barras con los niveles de grado académico ordenados----
ggplot(df, aes(x = Grado, fill = Género)) +
  geom_bar(position = "dodge") + 
  xlab("Grado Académico") + ylab("Número de investigadores/as")+
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.title = element_text(face = "bold"))+
  guides(fill = guide_legend(title = "Género", label.theme = element_text(face = "bold")))

#-----------------Grafica de barras por género----------------------------------
ggplot(df, aes(x = Doctorado, fill = Género)) +
  geom_bar(position="dodge") + ylab("Número de investigadores/as")+
  theme_minimal()+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.title = element_text(face = "bold")) +
  guides(fill = guide_legend(title = "Género", label.theme = element_text(face = "bold")))

#-----------------Grafica de barras para proporciones-----------------------------
ggplot(df, aes(x = party, fill = natarms)) +
  geom_bar(position = "fill")

#-----------------Grafica de puntos por género----------------------------------
ggplot(data = df, aes(x = Citas, y = Articulos, shape = Género, color = Género)) + 
  geom_point(size = 3, alpha = .6) +
  scale_shape_manual(values = c(15, 19))+
  theme_minimal()+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.title = element_text(face = "bold"))+
  guides(fill = guide_legend(label.theme = element_text(face = "bold")))

#-----------------------Mapa de calor----------------------------------
ggplot(df, aes(x = Articulos, y = Citas, fill = Género)) +
  stat_bin2d() +
  scale_fill_manual(values = c("red", "black")) +
  labs(x = "Artículos", y = "Citas", fill = "Genero")

#-------------------------Grafica de puntos-----------------------
df %>% ggplot(aes(x= Pais, y= Femenino, color = Pais, size = 0.5))+ 
  geom_point() +
  stat_summary(fun = mean, geom = "point") + scale_color_manual(values = c("red", "green", "blue", "brown", "Magenta", "black", "purple")) +
  guides(color = FALSE, size = FALSE) + ylab("Porcentajate femenino") + theme_minimal() +
  theme(axis.text.x = element_text(size = 14, face = "bold"))+
  theme(axis.title = element_text(size = 14, face = "bold"))

#--------------------------Grafica de pastel----------------------
ggplot(df, aes(x="", y="", fill=Género)) +
  geom_bar(stat="identity", width=1) +
  labs(x = NULL, y = NULL)+
  coord_polar("y", start=0)

#--------------------------Grafica de pastel por universidad---------------------
ggplot(df, aes(x="", y=Porcentaje, fill=Género)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)  + facet_wrap(~Universidad)+ 
  xlab(NULL)+
  theme(
    strip.text = element_text(size = 20, face = "bold")  
  )+
  theme(axis.title = element_text(size = 24, face = "bold"))+
  guides(fill = guide_legend(title = "Género", label.theme = element_text(size = 14,face = "bold")))

#---------------------Grafica de columnas para proporciones------------------------
df %>% ggplot(aes(x = Universidad, y = Porcentaje, fill = Género)) +
  geom_col()

#------------------------Grafica de probabilidad acumulada------------------------
ggplot(df, aes(x = Femenino)) +
  stat_ecdf(geom = "step") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)) +
  labs(title = "Clúster 2", x = "x", y = "F(x)")

#------------------------ECDF para dos grupos------------------------
new_theme <- theme(axis.text=element_text(size=14),
                   axis.title=element_text(size=20,face="bold"), legend.text=element_text(size=22),
                   legend.title = element_text(face = "bold", size = 26), legend.position="top",
                   axis.line = element_line(colour="black", size=1, lineend="square"),
                   strip.text.y = element_text(size = 12, colour = "gray30"))

ggplot(df, aes(Femenino, color = Cluster)) + 
  stat_ecdf(geom = "step", size = 3)+
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  new_theme +
  xlab("Weight") +
  ylab("Percent")


#-----------------------------Correlación---------------------------------
pairs.panels(df, method = "spearman", cex.cor = 1, col = "blue", pch = 21, bg = "gray80") #puede ser cualquier tipo de correalación

#------------------------Densidad+Histograma por genero----------------------------
ggplot(df, aes(x=Porcentaje,fill=Género, color=Género)) +
  geom_histogram(alpha=0.5, position="identity")+
  geom_density(alpha=.3) + ylab("Densidad")+
  theme_minimal()+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.title = element_text(face = "bold"))+
  guides(fill = guide_legend(label.theme = element_text(face = "bold")))

#------------------------Densidad----------------------------
ggplot(df, aes(x = Citas)) + #{DensidadLog}
  geom_density(fill = "blue", alpha = 0.2) +
  theme_minimal()+
  labs( x = "Promedio de citas", y = "Densidad")+
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.title = element_text(face = "bold"))

#--------------------Calculo del indice H----------------------------------
# Crear un vector con los datos de las citas
citas <- c(4, 2, 4, 1, 8, 5, 10, 2, 10, 15)

# Obtener el orden de los datos según el número de citas de mayor a menor
orden_citas <- order(citas, decreasing = TRUE)
citas[orden_citas[5]]
# Crear un vector con las etiquetas personalizadas del eje X
etiquetas_x <- paste0(1:length(citas), "°")


# Crear el gráfico de barras
barplot(citas[orden_citas], names.arg = etiquetas_x, ylim = c(0, 15), 
        ylab = "Número de citas", xlab = "Artículos ordenados por número de citas",
        main = "Cómo se calcula el índice H", col = "lightgray") + theme_minimal() 


# Ajustar las etiquetas del eje Y para que muestren valores de 2 en 2
axis(2, at = seq(0, 15, by = 1))


# Agregar una línea vertical 
#lines(c(9, 9), c(0, 8), col = "red")
lines(c(5.5, 5.5), c(0, 5), col = "red")
# Agregar una línea horizontal 
lines(c(0, 5.5), c(5, 5), col = "red")

################################################################################
#                                     REDES                                    # 
################################################################################
grafos <- df

grafos %>% select(UEstudio, UTrabajo) %>% filter(UTrabajo == "UEM" | UTrabajo == "UFLA")
grafos <- grafos %>% filter(UTrabajo == "USP" | UTrabajo == "UEM" | UTrabajo == "UFLA") # Universidad Federal de Minas Gerais
grafos <- grafos %>% filter(!is.na(UEstudio))
arcos <- data.frame(origen = grafos$UEstudio, destino = grafos$UTrabajo)

grafos %>% group_by(UTrabajo) %>% count()
grafo <- graph_from_data_frame(arcos, directed = TRUE)

# Define una paleta de colores personalizada con colores llamativos
colores_personalizados <- c("#FF00FF", "#00FF00", "#FFFF00", "#FF0000", "#00FFFF", "#FFA500", "#FF1493", "#FF4500", "#9400D3", "#00FF7F", "#FF69B4", "#32CD32", "#FF8C00", "#008B8B", "#8A2BE2", "#FFD700")

# Crear el grafo y usar la paleta de colores personalizada
plot(grafo, 
     layout = layout.kamada.kawai(grafo, weights = NULL),
     vertex.color = colores_personalizados,  # Usa la paleta personalizada
     vertex.size = 15,            # Tamaño de los nodos
     vertex.label.color = "black",# Color de las etiquetas de los nodos
     vertex.label.cex = 1.2,      # Tamaño de las etiquetas de los nodos (ajustar según sea necesario)
     vertex.label.font = 2,       # Estilo de la etiqueta de los nodos (2 para negritas)
     edge.arrow.size = 0.4)       # Tamaño de las aristas

#--------------------------------RED DE ERDOS ------------------------------------
# Crear el vector con los nombres
nombres <- c("C.F. Gauss", "H. Minkowski", "H. Weyl", "A. Einstein",
             "H.A. Lorentz", "E.G. Straus", "J. Spencer", "L. Lovász", 
             "E. Szemerédi", "P. Erdos", "S.A. Burr", "R.L. Graham", "F. Chung", 
             "W.T. Tutte", "W. Feller")

# Crear la matriz de adyacencia
matriz_ady <- matrix(0, ncol = length(nombres), nrow = length(nombres), dimnames = list(nombres, nombres))

# Establecer las conexiones
matriz_ady["W. Feller", "P. Erdos"] <- 1
matriz_ady["P. Erdos", c("S.A. Burr", "L. Lovász", "J. Spencer", "E. Szemerédi", "R.L. Graham", "E.G. Straus", "W.T. Tutte", "W. Feller")] <- 1
matriz_ady["S.A. Burr", c("J. Spencer", "L. Lovász", "R.L. Graham")] <- 1
matriz_ady["L. Lovász", c("E.G. Straus", "J. Spencer", "R.L. Graham")] <- 1
matriz_ady["J. Spencer", c("E.G. Straus", "F. Chung", "R.L. Graham", "E. Szemerédi")] <- 1
matriz_ady["E. Szemerédi", c("F. Chung", "R.L. Graham")] <- 1
matriz_ady["R.L. Graham", c("E.G. Straus", "F. Chung")] <- 1
matriz_ady["E.G. Straus", "A. Einstein"] <- 1
matriz_ady["A. Einstein", c("H.A. Lorentz", "H. Minkowski", "H. Weyl")] <- 1
matriz_ady["H. Minkowski", c("H.A. Lorentz", "C.F. Gauss", "H. Weyl")] <- 1
matriz_ady["H. Weyl", "H.A. Lorentz"] <- 1

# Crear el grafo
g <- graph.adjacency(matriz_ady, mode = "undirected", diag = FALSE)

# Establecer colores para los nodos
colores <- rainbow(length(V(g)))
colores_personalizados2 <- c("#00FF00", "#FFFF00", "#FF0000", "#FFA500", "#FF4500", "#00FF7F", "#32CD32", "#FF8C00", "#008B8B", "#FFD700", "#FFA500", "#FF69B4", "#FF8C00")


# Definir la disposición de los nodos
layout <- layout_with_kk(g) #mds, kk o sugiyama

# Personalizar la visualización de la red
plot(g, layout = layout, vertex.size = 10, 
     vertex.label.cex = 0.8, vertex.color = colores, 
     vertex.frame.color = "white", vertex.label.color = "black",
     edge.color = "gray", edge.width = 1.5, vertex.label.font = 2, main = "Red de Erdős")

plot(g, layout = layout, vertex.size = 10, 
     vertex.label.cex = 1,  # Tamaño de las etiquetas de los nodos aumentado
     vertex.color = colores, 
     vertex.frame.color = "white",  # Color de fondo de los nodos
     vertex.label.color = "black",  # Color de las etiquetas de los nodos
     edge.color = "gray", edge.width = 1.5, vertex.label.font = 2, main = "Red de Erdős")
