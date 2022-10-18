library(fmsb)
library(ggplot2)
library(corrplot)
library(RColorBrewer)

setwd("/Users/goretty/R ladies")
datos <- read.csv("data/world_cup.csv", stringsAsFactors = TRUE)
str(datos)


ggplot(data= datos, aes(y= goals_z, x=season, 
      color=team)) + geom_point(size=1.7) +
      labs(title = "Goles anotados en los mundiales de soccer", 
      x = "Año", y = "Goles (sd)")

palette(brewer.pal(n = 12, name = "Paired"))
ggplot(data= datos, aes(y= goals_z, x=season, 
             color=team)) + geom_point(size=2.5) + 
             labs(title = "Goles anotados en los mundiales de soccer", 
             x = "Año", y = "Goles (sd)") +
             scale_color_brewer(palette = "Paired")

#players <- aggregate(datos$goals_z, by=list(datos$player), mean) #valor de la media

#ahora quiero todas las estadisticas promedio por país
attach(datos)
team_mean <- aggregate(cbind(goals_z,xg_z,crosses_z,boxtouches_z,passes_z,
                             progpasses_z,takeons_z,progruns_z,tackles_z,
                             interceptions_z,clearances_z,blocks_z,aerials_z,
                             fouls_z,fouled_z,nsxg_z), 
                       by = list(datos$team), mean)
team_mean
typeof(team_mean)
sort(team_mean$goals_z, decreasing = TRUE) #ordename los goles de menor a mayor
which(team_mean$goals_z > 0.12)
top10team <- team_mean[c(9,20,27,32,38,44,54,57,77,78),]
top10team
#modificando nombres de columnas:
colnames(top10team) <- c("Equipo","Goles","xg","Cruces","Toquescaja","Pases",
                         "Pasesprog","Tomas","Programas","Tacleos",
                         "Intercepciones","Autorizaciones","Bloqueos","Aereos",
                         "Faltas","Fouleados","nsxg")
top10team
top10team <- top10team[c(1,2,4,6,10,11,13:16)] #sobreescribiendo mi vector

#### correlograma:
cor <- cor(top10team[c(2:10)], method = "pearson")
cor
corrplot(cor, method="color", type= "upper")
#write.csv(cor, "CorSoccer.csv")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min
rmin <- rep(-0.28, 10)
rmax <- rep(0.38, 10)
top10team_modif <- rbind(rmin, rmax, top10team)

# Radar chart para los equipos
radarchart(top10team_modif,
           cglty = 3, #Tipo línea maya
           cglcol = "grey", #color línea maya
           cglwd = 1.5, #Ancho línea maya 
           plwd = 1, #Ancho de línea radar
           title= "Gráfico de radar por equipos")
legend("topleft",
       legend = paste("Team", 1:3), bty = "n", pch =0.2)

#ahora quiero todas las estadisticas promedio por jugador
player_mean <- aggregate(cbind(goals_z,xg_z,crosses_z,boxtouches_z,passes_z,
                             progpasses_z,takeons_z,progruns_z,tackles_z,
                             interceptions_z,clearances_z,blocks_z,aerials_z,
                             fouls_z,fouled_z,nsxg_z), 
                       by = list(datos$player), mean)
player_mean
typeof(player_mean)
sort(player_mean$goals_z, decreasing = TRUE) #ordename los goles de mayor a menor
which(player_mean$goals_z > 5.2)
top10player <- player_mean[c(703,1122,1179,1612,2502,2737,3243,3495,3690,3836,4188),]
top10player
#modificando nombres de columnas:
colnames(top10player) <- c("Jugador","Goles","xg","Cruces","Toquescaja","Pases",
                         "Pasesprog","Tomas","Programas","Tacleos",
                         "Intercepciones","Autorizaciones","Bloqueos","Aereos",
                         "Faltas","Fouleados","nsxg")
top10player
top10player <- top10player[c(1,2,4,6,10,11,13:16)] #sobreescribiendo mi vector
#construyendo radar plot para el top 10 de goleadores
rmin <- rep(0.28, 10)
rmax <- rep(-0.9, 10)
top10player_modif <- rbind(rmin, rmax, top10player)

# Radar chart 
radarchart(top10player_modif,
           cglty = 3, #Tipo línea maya
           cglcol = "grey", #color línea maya
           cglwd = 1.5, #Ancho línea maya 
           plwd = 1, #Ancho de línea radar
           title= "Gráfico de radar por jugador")

####
### Líneas extras no usadas aquí: 
