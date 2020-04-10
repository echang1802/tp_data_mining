# Trabajo Practico 1 - Mineria de Datos
# Eloy Chang

# ----> Librerias <----

library(data.table)
library(plotly)
library(rpart)
library(dplyr)

# ----> Descarga de datos<----

load_csv_data <- function(csv_file, sample_ratio = 1, drop_cols = NULL,
                          sel_cols = NULL) {
    dt <- fread(csv_file, header = TRUE, sep = ",", stringsAsFactors = TRUE,
                na.strings = "", drop = drop_cols, select = sel_cols,
                showProgress = TRUE)
    if (sample_ratio < 1) {
        sample_size <- as.integer(sample_ratio * nrow(dt))
        dt <- dt[sample(.N, sample_size)]
    }
    return(dt)
}

load_train_data <- function(data_dir, train_file="train.csv", sample_ratio=1,
                            drop_cols=NULL, sel_cols=NULL) {
    train_days <- seq(1, 5, by=1)
    dfs <- list()
    for(day in train_days){
        sv_file<- paste(data_dir,"train_",day,".csv", sep = "")
        dfs[[day]]<- load_csv_data(sv_file, sample_ratio=1,
                                 drop_cols=NULL, sel_cols=NULL)
    }
    df <- (rbindlist(dfs, fill=TRUE))
    fwrite(df,paste(data_dir,train_file, sep = ""))
    gc()
    return(df)
}

load_new_data<- FALSE     # <<<<------ Para bajar nuevos datos
if(load_new_data){
    train_data<- load_train_data("Datasets/", sample_ratio = 0.05)
} else {
    train_data<- load_csv_data("Datasets/train.csv")
}

# ----> Analisis exploratorio <-----


# Exploratory analysis:
# Age y site tienen muy pocos datos
# traffic_type es una constante
# device_model tiene muchas categorias, pocas con repeticiones, quizas
#      convenga generar una variable binaria de algun dispositivo en 
#      particular si es que tiene relevancia. 
# BuyCard_sum_dsi1 tiene valores negativos


    # ----> Agregar variables <----

train_data[, creditor := ((soft_positive + hard_positive) -
                      (soft_negative + hard_negative)) > 0]

train_data[, label := Label_max_played_dsi == 3]

train_data[, participateInTournament := StartTournamentBattle_sum_dsi0 > 0 |
               StartTournamentBattle_sum_dsi1 > 0 | 
               StartTournamentBattle_sum_dsi2 > 0 |
               StartTournamentBattle_sum_dsi3 > 0]

train_data[, winning_rate_dsi0 := WinBattle_sum_dsi0 / (WinBattle_sum_dsi0 + LoseBattle_sum_dsi0)]
train_data[, winning_rate_dsi1 := WinBattle_sum_dsi1 / (WinBattle_sum_dsi1 + LoseBattle_sum_dsi1)]
train_data[, winning_rate_dsi2 := WinBattle_sum_dsi2 / (WinBattle_sum_dsi2 + LoseBattle_sum_dsi2)]
train_data[, winning_rate_dsi3 := WinBattle_sum_dsi3 / (WinBattle_sum_dsi3 + LoseBattle_sum_dsi3)]
train_data[, winning_rate := (WinBattle_sum_dsi0 + WinBattle_sum_dsi1 + WinBattle_sum_dsi2 + WinBattle_sum_dsi3) / 
               (LoseBattle_sum_dsi0 + LoseBattle_sum_dsi1 + LoseBattle_sum_dsi2 + LoseBattle_sum_dsi3 + WinBattle_sum_dsi0 + WinBattle_sum_dsi1 + WinBattle_sum_dsi2 + WinBattle_sum_dsi3)]

train_data[, notStartedBattleSomeDay := (is.na(StartBattle_sum_dsi0) | StartBattle_sum_dsi0 == 0) |
               (is.na(StartBattle_sum_dsi1) | StartBattle_sum_dsi1 == 0) |
               (is.na(StartBattle_sum_dsi2) | StartBattle_sum_dsi2 == 0) |
               (is.na(StartBattle_sum_dsi3) | StartBattle_sum_dsi3 == 0)]

train_data[, BuyCard_avg_ratio := (BuyCard_sum_dsi3 - BuyCard_sum_dsi2 +
                                   BuyCard_sum_dsi2 - BuyCard_sum_dsi1 +
                                   BuyCard_sum_dsi1 - BuyCard_sum_dsi0) / 3]

train_data[, BuyCard_sum := BuyCard_sum_dsi0 + 
               BuyCard_sum_dsi1 + BuyCard_sum_dsi2 + BuyCard_sum_dsi3]

train_data[, totalBattles := StartBattle_sum_dsi0 + 
               StartBattle_sum_dsi1 + StartBattle_sum_dsi2 + 
               StartBattle_sum_dsi3]

train_data[, totalSpended := hard_negative + soft_negative]

train_data[, ChangeArena_sum := ChangeArena_sum_dsi0 + 
               ChangeArena_sum_dsi1 + ChangeArena_sum_dsi2 + 
               ChangeArena_sum_dsi3]


    # ----> Estudio de probabilidades condicionales <-----
tapply(train_data$label,train_data$creditor,function(x) table(x)/length(x))

tapply(train_data$label,train_data$platform,function(x) table(x)/length(x))
# Vale la pena hacer un test de hipotesis a ver si esta variable es influyente

tapply(train_data$label,train_data$TutorialFinish,function(x) table(x)/length(x))

tapply(train_data$label,train_data$categorical_1,function(x) table(x)/length(x))
# Hay algunas categorias en las que la distribucion varia considerablemente

tapply(train_data$label,train_data$categorical_2,function(x) table(x)/length(x))

tapply(train_data$label,train_data$categorical_3,function(x) table(x)/length(x))

tapply(train_data$label,train_data$categorical_4,function(x) table(x)/length(x))

tapply(train_data$label,train_data$categorical_5,function(x) table(x)/length(x))

tapply(train_data$label,train_data$categorical_6,function(x) table(x)/length(x))

tapply(train_data$label,train_data$categorical_7,function(x) table(x)/length(x))

tapply(train_data$label,train_data$notStartedBattleSomeDay,function(x) table(x)/length(x))

tapply(train_data$BuyCard_sum_dsi3,train_data$label,summary)
# esta parece ser una variable influyente

tapply(train_data$BuyCard_sum_dsi3[train_data$BuyCard_sum_dsi1 > 0] - 
           train_data$BuyCard_sum_dsi2[train_data$BuyCard_sum_dsi1 > 0],
       train_data$label[train_data$BuyCard_sum_dsi1 > 0],summary)
# esta parece ser una variable influyente

tapply(train_data$BuyCard_sum_dsi3[train_data$BuyCard_sum_dsi1 > 0] + 
           train_data$BuyCard_sum_dsi2[train_data$BuyCard_sum_dsi1 > 0] + 
           train_data$BuyCard_sum_dsi1[train_data$BuyCard_sum_dsi1 > 0] + 
           train_data$BuyCard_sum_dsi0[train_data$BuyCard_sum_dsi1 > 0],
       train_data$label[train_data$BuyCard_sum_dsi1 > 0],summary)
# esta parece ser una variable influyente

tapply(train_data$ChangeArena_sum_dsi3,train_data$label,summary)

tapply(train_data$ChangeArena_sum_dsi3 + 
           train_data$ChangeArena_sum_dsi2 + 
           train_data$ChangeArena_sum_dsi1 + 
           train_data$ChangeArena_sum_dsi0,
       train_data$label,summary)
# esta parece ser una variable influyente

tapply(train_data$winning_rate,train_data$label,summary)

tapply(train_data$winning_rate_dsi3 - train_data$winning_rate_dsi0,train_data$label,summary)

tapply(train_data$StartSession_sum_dsi0,train_data$label,summary)

tapply(train_data$StartSession_sum_dsi1,train_data$label,summary)

tapply(train_data$StartSession_sum_dsi2,train_data$label,summary)

tapply(train_data$StartSession_sum_dsi3,train_data$label,summary)

tapply(train_data$StartSession_sum_dsi0 + 
           train_data$StartSession_sum_dsi1 + 
           train_data$StartSession_sum_dsi2 + 
           train_data$StartSession_sum_dsi3 ,
       train_data$label,summary)
# esta parece ser una variable influyente

tapply(train_data$StartBattle_sum_dsi0,train_data$label,summary)

tapply(train_data$StartBattle_sum_dsi1,train_data$label,summary)

tapply(train_data$StartBattle_sum_dsi2,train_data$label,summary)

tapply(train_data$StartBattle_sum_dsi3,train_data$label,summary)

tapply(train_data$StartBattle_sum_dsi0 + 
           train_data$StartBattle_sum_dsi1 +
           train_data$StartBattle_sum_dsi2 + 
           train_data$StartBattle_sum_dsi3,
       train_data$label,summary)

    # ----> Modelos exploratorios <----

treeFeatures<- rpart.control(maxdepth = 30, minsplit = 0, 
                             minbucket=0, cp=0)
maxDepthTree<- rpart(label ~ ., 
                     data = train_data[,c("label","creditor","ChangeArena_sum","totalSpended",
                                          "BuyCard_sum","BuyCard_avg_ratio","notStartedBattleSomeDay",
                                          "winning_rate","winning_rate_dsi0","winning_rate_dsi1",
                                          "winning_rate_dsi2","winning_rate_dsi3")],
                     control = treeFeatures, method = "class")


# BuyCards_sum
buyCards_clusters<- kmeans(x = train_data[,c("BuyCard_sum")],
                           centers = 2)
table(buyCards_clusters$cluster)/length(buyCards_clusters$cluster)
tapply(train_data$label, buyCards_clusters$cluster, 
       function(x) table(x)/length(x))

# BuyCards_avg_ratio
buyCards_clusters<- kmeans(x = train_data[,c("BuyCard_avg_ratio")],
                               centers = 2)
table(buyCards_clusters$cluster)/length(buyCards_clusters$cluster)
tapply(train_data$label, buyCards_clusters$cluster, 
       function(x) table(x)/length(x))

# totalSpended
big_spender_clusters<- kmeans(x = train_data[,c("totalSpended")],
                              centers = c(100,1000))
table(buyCards_clusters$cluster)/length(buyCards_clusters$cluster)
tapply(train_data$label, buyCards_clusters$cluster, 
       function(x) table(x)/length(x))

# ChangeArena_sum
ChangeArena_clusters<- kmeans(x = train_data[!is.na(train_data$ChangeArena_sum),
                                          c("ChangeArena_sum")],
                           centers = 2)
table(ChangeArena_clusters$cluster)/length(ChangeArena_clusters$cluster)
tapply(train_data$label[!is.na(train_data$ChangeArena_sum)], 
       ChangeArena_clusters$cluster, function(x) table(x)/length(x))

# CA_NSB
CA_NSB_clusters<- kmeans(x = train_data[!is.na(train_data$ChangeArena_sum),
                                             c("ChangeArena_sum","notStartedBattleSomeDay")],
                              centers = 2)
table(CA_NSB_clusters$cluster)/length(CA_NSB_clusters$cluster)
tapply(train_data$label[!is.na(train_data$ChangeArena_sum)], 
       CA_NSB_clusters$cluster, function(x) table(x)/length(x))

    # ----> Graficos exploratorios <----

minTS<- min(train_data$totalSpended)
train_data$totalSpended<- log(train_data$totalSpended + 1)

ggplot(train_data[sample(nrow(train_data),100000),c("totalSpended","winning_rate","label")], 
       aes(x=totalSpended, y=winning_rate, color=label, alpha = 0.5)) + 
    geom_point()

df<- data.frame(
    totalSpended = seq(from=0,to=1,by=0.01),
    winningRate = seq(from=0,to=1,by=0.01)
)
df$propChurm<- 0
for(ts in 1:(nrow(df) - 1)){
    for(wr in 1:(nrow(df) - 1)){
        pos<- train_data$totalSpended > df$totalSpended[ts] &
            train_data$totalSpended <= df$totalSpended[ts + 1] &
            train_data$winning_rate > df$winningRate[wr] &
            train_data$winning_rate <= df$winningRate[wr + 1]
        if(sum(pos) > 0){
            df$propChurm<- sum(train_data$label[pos])/sum(pos)
        }
    }
}
plot_ly(df, type = "heatmap", x = ~totalSpended, y = ~winningRate,
        z = ~propChurm, text = ) %>%
    layout(title = "Proporción de Churm",
           xaxis = list(title = "Total gastado (en monedas del juego)"),
           yaxis = list(title = "Tasa de victorias"))


df<- train_data %>% group_by(totalBattles,label) %>% 
    summarise(avg_winningRate = mean(winning_rate, na.rm = TRUE),
              data_points = length(totalBattles))
df<- as.data.frame(df)

ggplot(df[1:500,], aes(x = totalBattles, y = avg_winningRate, 
               color = label)) + 
    geom_line()

