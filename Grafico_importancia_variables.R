
library(plotly)

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


