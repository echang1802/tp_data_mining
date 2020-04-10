
library(plotly)


# para maxDepthArbol (variables originales + totalSpend y winingRate)

load("Arbol.RData")

df<- data.frame(
  variable = names(maxDepthArbol$variable.importance),
  importance = as.numeric(maxDepthArbol$variable.importance),
  text = c("Suma de todas las partidas ganadas entre las partidas jugadas",
           "Suma de todo lo gastado en el juego (hard y soft negative)",
           rep("Variable original",24))
)
df<- df[order(df$importance, decreasing = TRUE)[1:15],]

plot_ly(df, type = "bar", orientation = "h",x = ~importance, y = ~reorder(variable,importance),
        text = ~paste(variable,": ",round(importance),"\n",text,sep = ""),hoverinfo = "text") %>%
  layout(title = "Importancia de variables", xaxis = list(title = "Importancia"),
         yaxis = list(title = ""))

# para maxdepthTree

load("Tree.RData")

df<- data.frame(
  variable = names(maxDepthTree$variable.importance),
  importance = as.numeric(maxDepthTree$variable.importance),
  text = c("Suma de todas las partidas ganadas entre las partidas jugadas",
           "Suma de todo lo gastado en el juego (hard y soft negative)",
           "Suma de todas las cartas compradas",
           "Tasa de victorias durante el primer día",
           "Tasa de victorias durante el segundo día",
           "Tasa de victorias durante el tercer día",
           "Tasa de victorias durante el cuarto día",
           "Si  hubo al menos un día en el que no se iniciara una partida",
           "Si el usuario es acreedor",
           "Suma de cambios de arena")
)
df<- df[order(df$importance, decreasing = TRUE),]

plot_ly(df, type = "bar", orientation = "h",x = ~importance, y = ~reorder(variable,importance),
        text = ~paste(variable,": ",round(importance),"\n",text,sep = ""),hoverinfo = "text") %>%
  layout(title = "Importancia de variables", xaxis = list(title = "Importancia"),
         yaxis = list(title = ""))
