## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(dema)
library(reshape2)
library(stringr)
library(psych)
setwd("C:/Users/adiaz/Documents/adiaz/Documents/Analisis multivariado/Especializacion Analitica/codigo_r")
survey <- read.delim("survey.csv",sep = ';')
dia <- filter(survey, Establecimiento == 'dia')
carrefour <- filter(survey, Establecimiento == 'carrefour')
mercadona <- filter(survey, Establecimiento == 'mercadona')

## ------------------------------------------------------------------------
survey_dist <- dist(dplyr::select(survey, starts_with('P')))
survey_segment <- hclust(survey_dist, method = 'ward.D2')
plot(survey_segment, main = 'Dendrograma todos los establecimientos', sub = NA,
      xlab = 'Individuos', ylab = 'Distancia',
     labels = FALSE, hang = -1)
rect.hclust(survey_segment, k = 2, border="red")

## ------------------------------------------------------------------------
plot(survey_segment, main = 'Dendrograma todos los establecimientos', sub = NA,
      xlab = 'Individuos', ylab = 'Distancia',
     labels = FALSE, hang = -1)
rect.hclust(survey_segment, k = 4, border="red")

## ------------------------------------------------------------------------
set.seed(1234)
survey_kmeans <- kmeans(dplyr::select(survey, starts_with('P')), 4)
table(survey_kmeans$cluster)
print(prop.table(table(survey_kmeans$cluster)), digits = 2)

## ------------------------------------------------------------------------
survey_kmeans_medias <- aggregate(dplyr::select(
  survey, -Establecimiento, -starts_with('C')), 
          by = list(survey_kmeans$cluster), FUN = mean)
survey_kmeans_medias <- data.frame(t(survey_kmeans_medias[,-1]))
print(survey_kmeans_medias, digits = 2)

## ------------------------------------------------------------------------
survey_kmeans_medias_grafico <- survey_kmeans_medias %>% 
  dplyr::select(Detractores = X4,  Descontentos = X1, Contentos = X2, Fans = X3)
survey_kmeans_medias_grafico$variables <- row.names(survey_kmeans_medias)
row.names(survey_kmeans_medias_grafico) <- NULL
survey_kmeans_medias_grafico <- survey_kmeans_medias_grafico %>%
  filter(stringr::str_detect(variables, 'P'))
dplyr::arrange(survey_kmeans_medias_grafico, desc(Fans))

## ------------------------------------------------------------------------
survey_kmeans_medias_grafico_melt <- 
  reshape2::melt(survey_kmeans_medias_grafico, id.vars = 'variables')
head(survey_kmeans_medias_grafico_melt)

## ------------------------------------------------------------------------
ggplot(survey_kmeans_medias_grafico_melt,
       aes(reorder(variables, value), value, 
           group = variable, color = variable)) + 
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  theme(legend.position="bottom",legend.title=element_blank()) +
  theme(panel.grid.major.x = element_line(colour="grey", size=0.5)) +
  ggtitle('Comparativa respuestas medias') +
  labs(x = '', y = "Medias de las respuestas") +
  coord_flip()

## ------------------------------------------------------------------------
survey_kmeans_medias_clasificacion <- survey_kmeans_medias %>% 
  dplyr::select(Detractores = X4,  Descontentos = X1, Contentos = X2, Fans = X3)
survey_kmeans_medias_clasificacion$variables <- row.names(survey_kmeans_medias)
row.names(survey_kmeans_medias_clasificacion) <- NULL
survey_kmeans_medias_clasificacion <- survey_kmeans_medias_clasificacion %>%
  filter(!stringr::str_detect(variables, 'P'))
print(dplyr::arrange(survey_kmeans_medias_clasificacion, desc(Fans)), digits = 2)


## ------------------------------------------------------------------------
survey$cluster <- factor(survey_kmeans$cluster,
  labels = c('Descontentos','Contentos', 'Fans','Detractores'))
survey$cluster <- factor(survey$cluster,
  levels = c('Fans','Contentos', 'Descontentos','Detractores'))
survey$C1.Edad <-
  factor(survey$C1.Edad,
         levels = c('de 18 a 24','de 25 a 35','de 35 a 44',
                    'de 45 a 54','de 55 a 64','mas de 64'))
survey$C4.CompraMedia <-
  factor(survey$C4.CompraMedia,
         levels =  c('menos de 15','entre 15 y 30','entre 31 y 45',
                    'entre 46 y 60','entre 61 y 75', 'entre 76 y 90',
                    'entre 91 y 120','mas de 120'))
survey$C5.IngMes <-
  factor(survey$C5.IngMes,
         levels = c('menos de 600','entre 601 y 1000','entre 1001 y 1500',
                    'entre 1501 y 2000','entre 2001 y 3000',
                    'entre 3001 y 4500','mas de 4500'))


## ------------------------------------------------------------------------
table(survey$cluster, survey$Establecimiento)

## ------------------------------------------------------------------------

ggplot(survey, aes(cluster, fill = cluster)) +
geom_bar(aes(y = (..count..)/sum(..count..))) +
facet_grid(Establecimiento ~ .) +
theme(legend.position="none") +
labs(x = 'Cluster', y = 'Frecuencia', 
     title = 'Cluster de pertenencia', subtitle = 'Por establecimiento') +
scale_y_continuous(labels = scales::percent) 

## ------------------------------------------------------------------------
ggplot(survey,
       aes(cluster, RC1.Edad)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, position = position_dodge(width = 0.75),
               colour = "red", size = 1, geom = "point") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ggtitle('Cluster de pertenencia por edades') +
  labs(x = 'Grupo', y = 'Edad')

## ------------------------------------------------------------------------
ggplot(survey,
       aes(cluster, RC4.CompraMedia, fill = Establecimiento)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, position=position_dodge(width = 0.75),
               colour = "white", size = 1, geom = "point") +
  theme(legend.position="bottom",legend.title=element_blank()) +
  ggtitle('Consumo medio') +
  labs(x = 'Grupo', y = 'Consumo medio (€)', subtitle = 'Por Grupo y Establecimiento')

## ------------------------------------------------------------------------
ggplot(survey,
       aes(Ph1.Satisfaccion, Ph1.CalidadPrecio, 
           color = factor(survey$cluster))) +
  geom_point() +
  theme(legend.position="bottom",legend.title=element_blank()) +
  # cambiamos el orden de la leyenda
  guides(colour = guide_legend(reverse=T)) +
  labs(x = 'Satisfaccion global', y = 'Relac. calidad precio',
       title = 'Relac. calidad precio/Satisfaccion', 
       subtitle = 'Por grupos')

## ------------------------------------------------------------------------
ggplot(survey,
       aes(Ph1.Satisfaccion, Ph1.CalidadPrecio, 
           color = factor(survey$cluster))) +
  geom_jitter() +
  theme(legend.position="bottom",legend.title=element_blank()) +
  guides(colour = guide_legend(reverse=T)) +
  labs(x = 'Satisfaccion global', y = 'Relac. calidad precio',
       title = 'Relac. calidad precio/Satisfaccion', 
       subtitle = 'Por grupos')

## ------------------------------------------------------------------------
survey_principal_rot <- psych::principal(dplyr::select(survey, starts_with('P')),
  nfactors = 4, rotate = 'varimax')
survey_principal_scores_rot <- data.frame(unclass(survey_principal_rot$scores))
survey_principal_scores_rot$cluster <- survey$cluster

## ------------------------------------------------------------------------
## Creamos variables temporales
# Agregamos las columnas a la nueva variable, pero dandoles nombre x o y
a <- dplyr::select(survey_principal_scores_rot, x = RC1, y = RC2, cluster)
a$factor <- 'Empleados y Establecimiento'
b <- dplyr::select(survey_principal_scores_rot, x = RC1, y = RC3, cluster)
b$factor <- 'Empleados y Tiempos'
c <- dplyr::select(survey_principal_scores_rot, x = RC1, y = RC4, cluster)
c$factor <- 'Empleados y Gama Productos'
d <- dplyr::bind_rows(a, b, c)
# Cambiamos el orden de los niveles para una mejor representacion grafica
d$cluster <- factor(d$cluster, levels = rev(levels(d$cluster)))

## ------------------------------------------------------------------------
ggplot(data = d, aes(x, y)) +
  geom_point(aes(colour = cluster), alpha = 0.4) +
  facet_grid(~ factor) +
  stat_ellipse(aes(color = cluster, fill = cluster), type = 'norm', 
               geom = "polygon", alpha = 0.1, show.legend = FALSE) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  labs(title = "Analisis factorial por grupo de pertenencia",
       x = '', y = '') +
  scale_colour_discrete(name = "Variable")

