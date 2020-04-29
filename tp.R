# Trabajo Practico 1 - Mineria de Datos
# Eloy Chang

# ----> Librerias <----

library(data.table)
library(plotly)
library(rpart)
library(rpart.plot)
library(dplyr)
library(xgboost)
# library(e1071)
# library(caret)

# ----> funciones <----

one_hot_sparse <- function(data_set) {
    
    require(Matrix)
    
    created <- FALSE
    
    if (sum(sapply(data_set, is.numeric)) > 0) {  # Si hay, Pasamos los numéricos a una matriz esparsa (sería raro que no estuviese, porque "Label"  es numérica y tiene que estar sí o sí)
        out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.numeric), with = FALSE]), "dgCMatrix")
        created <- TRUE
    }
    
    if (sum(sapply(data_set, is.logical)) > 0) {  # Si hay, pasamos los lógicos a esparsa y lo unimos con la matriz anterior
        if (created) {
            out_put_data <- cbind2(out_put_data,
                                   as(as.matrix(data_set[,sapply(data_set, is.logical),
                                                         with = FALSE]), "dgCMatrix"))
        } else {
            out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.logical), with = FALSE]), "dgCMatrix")
            created <- TRUE
        }
    }
    
    # Identificamos las columnas que son factor (OJO: el data.frame no debería tener character)
    fact_variables <- names(which(sapply(data_set, is.factor)))
    
    # Para cada columna factor hago one hot encoding
    i <- 0
    
    for (f_var in fact_variables) {
        
        f_col_names <- levels(data_set[[f_var]])
        f_col_names <- gsub(" ", ".", paste(f_var, f_col_names, sep = "_"))
        j_values <- as.numeric(data_set[[f_var]])  # Se pone como valor de j, el valor del nivel del factor
        
        if (sum(is.na(j_values)) > 0) {  # En categóricas, trato a NA como una categoría más
            j_values[is.na(j_values)] <- length(f_col_names) + 1
            f_col_names <- c(f_col_names, paste(f_var, "NA", sep = "_"))
        }
        
        if (i == 0) {
            fact_data <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                      x = rep(1, nrow(data_set)),
                                      dims = c(nrow(data_set), length(f_col_names)))
            fact_data@Dimnames[[2]] <- f_col_names
        } else {
            fact_data_tmp <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                          x = rep(1, nrow(data_set)),
                                          dims = c(nrow(data_set), length(f_col_names)))
            fact_data_tmp@Dimnames[[2]] <- f_col_names
            fact_data <- cbind(fact_data, fact_data_tmp)
        }
        
        i <- i + 1
    }
    
    if (length(fact_variables) > 0) {
        if (created) {
            out_put_data <- cbind(out_put_data, fact_data)
        } else {
            out_put_data <- fact_data
            created <- TRUE
        }
    }
    return(out_put_data)
}

addingVariables<- function(train_data){
    train_data[, creditor := as.numeric(((soft_positive + hard_positive) -
                                  (soft_negative + hard_negative)) > 0)]
    
    train_data[, label := as.numeric(Label_max_played_dsi == 3)]
    train_data[, Label_max_played_dsi := NULL]
    
    train_data[, participateInTournament := as.numeric(StartTournamentBattle_sum_dsi0 > 0 |
                   StartTournamentBattle_sum_dsi1 > 0 |
                   StartTournamentBattle_sum_dsi2 > 0 |
                   StartTournamentBattle_sum_dsi3 > 0)]
    
    train_data[, winning_rate_dsi0 := WinBattle_sum_dsi0 / (WinBattle_sum_dsi0 + LoseBattle_sum_dsi0)]
    train_data[, winning_rate_dsi1 := WinBattle_sum_dsi1 / (WinBattle_sum_dsi1 + LoseBattle_sum_dsi1)]
    train_data[, winning_rate_dsi2 := WinBattle_sum_dsi2 / (WinBattle_sum_dsi2 + LoseBattle_sum_dsi2)]
    train_data[, winning_rate_dsi3 := WinBattle_sum_dsi3 / (WinBattle_sum_dsi3 + LoseBattle_sum_dsi3)]
    train_data[, winning_rate := (WinBattle_sum_dsi0 + WinBattle_sum_dsi1 + WinBattle_sum_dsi2 + WinBattle_sum_dsi3) /
                   (LoseBattle_sum_dsi0 + LoseBattle_sum_dsi1 + LoseBattle_sum_dsi2 + LoseBattle_sum_dsi3 + WinBattle_sum_dsi0 + WinBattle_sum_dsi1 + WinBattle_sum_dsi2 + WinBattle_sum_dsi3)]

    train_data[, notStartedBattleSomeDay := as.numeric((is.na(StartBattle_sum_dsi0) | StartBattle_sum_dsi0 == 0) |
                   (is.na(StartBattle_sum_dsi1) | StartBattle_sum_dsi1 == 0) |
                   (is.na(StartBattle_sum_dsi2) | StartBattle_sum_dsi2 == 0) |
                   (is.na(StartBattle_sum_dsi3) | StartBattle_sum_dsi3 == 0))]
    
    # train_data[, BuyCard_avg_ratio := (BuyCard_sum_dsi3 - BuyCard_sum_dsi2 +
    #                                        BuyCard_sum_dsi2 - BuyCard_sum_dsi1 +
    #                                        BuyCard_sum_dsi1 - BuyCard_sum_dsi0) / 3]
    
    train_data[, BuyCard_sum := BuyCard_sum_dsi0 + 
                   BuyCard_sum_dsi1 + BuyCard_sum_dsi2 + BuyCard_sum_dsi3]
    
    train_data[, totalBattles := StartBattle_sum_dsi0 + 
                   StartBattle_sum_dsi1 + StartBattle_sum_dsi2 + 
                   StartBattle_sum_dsi3]
    
    train_data[, totalSpended := hard_negative + soft_negative]
    
    train_data[, ChangeArena_sum := ChangeArena_sum_dsi0 + 
                   ChangeArena_sum_dsi1 + ChangeArena_sum_dsi2 + 
                   ChangeArena_sum_dsi3]
    
    return(train_data)
}

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
        dfs[[day]]<- load_csv_data(sv_file, sample_ratio=sample_ratio,
                                 drop_cols=NULL, sel_cols=sel_cols)
    }
    df <- (rbindlist(dfs, fill=TRUE))
    fwrite(df,paste(data_dir,train_file, sep = ""))
    gc()
    return(df)
}

loadData<- function(load_new_data,sample_ratio = 0.1, columnas = NULL){
    if(load_new_data){
        train_data<- load_train_data("Datasets/", sample_ratio = sample_ratio, sel_cols = columnas)
        train_data[, train_sample := 1]
        eval_data <- load_csv_data("Datasets/evaluation.csv", sel_cols = columnas)
        eval_data[, train_sample := 0]
        data_set <- rbind(train_data, eval_data, fill = TRUE)
        data_set<- addingVariables(data_set)
        #data_set <- one_hot_sparse(data_set)
        train_data <- data_set[data_set$train_sample == 1,]
        # train_data <- train_data[,  -which(names(train_data) == "train_sample")]
        eval_data <- data_set[data_set$train_sample == 0,]
        # eval_data <- eval_data[, setdiff(colnames(eval_data), "train_sample")]
        rm(data_set)
        gc()
        save(train_data,eval_data,file = "Datasets/tp_data.RData")
    } else {
        # train_data<- load_csv_data("Datasets/train.csv")
        load("Datasets/tp_data.RData")
    }
    return(list(train = train_data,eval = eval_data))
}

# ----> Analisis exploratorio <-----


# Exploratory analysis:
# Age y site tienen muy pocos datos
# traffic_type es una constante
# device_model tiene muchas categorias, pocas con repeticiones, quizas
#      convenga generar una variable binaria de algun dispositivo en 
#      particular si es que tiene relevancia. 
# BuyCard_sum_dsi1 tiene valores negativos

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
                                          "winning_rate_dsi2","winning_rate_dsi3","EnterShop_sum_dsi0",
                                          "EnterShop_sum_dsi1","EnterShop_sum_dsi2","EnterShop_sum_dsi3",
                                          "OpenChest_sum_dsi0","OpenChest_sum_dsi1","OpenChest_sum_dsi2",
                                          "OpenChest_sum_dsi3","UpgradeCard_sum_dsi0","UpgradeCard_sum_dsi1",
                                          "UpgradeCard_sum_dsi2","UpgradeCard_sum_dsi3")],
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

train_data$totalSpended<- log(train_data$totalSpended + 1)

# ggplot(train_data[sample(nrow(train_data),100000),c("totalSpended","winning_rate","label")], 
#        aes(x=totalSpended, y=winning_rate, color=label, alpha = 0.5)) + 
#     geom_point()

df<- data.frame()
for(ts in seq(from=0,to=max(train_data$totalSpended),by=max(train_data$totalSpended)/20)){
    for(wr in seq(from=0,to=0.95,by=0.05)){
        pos<- train_data$totalSpended > ts &
            train_data$totalSpended <= ts + 0.01 &
            train_data$winning_rate > wr &
            train_data$winning_rate <= wr + 0.01
        if(sum(pos, na.rm = TRUE) > 0){
            df<- rbind(df, data.frame(
                totalSpended = ts,
                winningRate = wr,
                propChurm = sum(train_data$label[pos], na.rm = TRUE) / sum(pos, na.rm = TRUE),
                dataPoints = sum(pos, na.rm = TRUE)
            ))
        } else {
            df<- rbind(df, data.frame(
                totalSpended = ts,
                winningRate = wr,
                propChurm = 0,
                dataPoints = 0
            ))
        }
    }
}
plot_ly(df, type = "heatmap", x = ~totalSpended, y = ~winningRate,z = ~propChurm, hoverinfo = "text",
        text = ~paste("Log(totalGastado):",round(totalSpended,2),"\nTasa de victoria:",round(winningRate,2),
                      "\nProporción de churm:",round(propChurm,2),"\nNúmero de datos:",dataPoints)) %>%
    layout(title = "Proporción de Churm",
           xaxis = list(title = "Logaritmo del total gastado (en monedas del juego)"),
           yaxis = list(title = "Tasa de victorias"))


df<- train_data %>% group_by(totalBattles,label) %>% 
    summarise(avg_winningRate = mean(winning_rate, na.rm = TRUE),
              data_points = length(totalBattles))
df<- as.data.frame(df)

ggplot(df[1:500,], aes(x = totalBattles, y = avg_winningRate, 
               color = label)) + 
    geom_line()

# ----> Primer Modelo <----

# Haremos un NB discretizando las variables winning_rate y totalSpended,
# El punto de corte se escogera usando kfold cross - validation
# La metrica a usar para la seleccion del punto de corte sera f-score

train_data<- train_data %>% select(label,winning_rate,totalSpended) %>% 
    filter((!is.na(winning_rate)) & (!is.na(totalSpended)) )
gc()

testing<- train_data[sample(nrow(train_data),1000),]

discretizarPorCantidadDatos<- function(columna, N){
    longitud<- length(columna)
    if(longitud %% N == 0){
        n<- rep(longitud/N,N)
    } else {
        extra<- longitud %% N
        n<- rep(floor(longitud/N),N)
        for(i in 1:extra){
            n[i]<- n[i] + 1
        }
    }
    columna_ordenanda<- sort(columna)
    clusters<- columna
    inicio<- 1
    fin<- 0
    for(i in 1:N){
        fin<- fin + n[i]
        pos<- columna >= columna_ordenanda[inicio] & columna <= columna_ordenanda[fin]
        print(paste("Inicio:",columna_ordenanda[inicio]," | Fin:",columna_ordenanda[fin]," | Pos:",sum(pos, na.rm = TRUE)))
        inicio<- fin
        clusters[pos]<- i
    }
    return(clusters)
}

testing$winning_rate_cluster<- discretizarPorCantidadDatos(testing$winning_rate,5)
plot(testing$winning_rate_cluster,testing$winning_rate)
grid()
testing$totalSpended_cluster<- discretizarPorCantidadDatos(testing$totalSpended,5)
plot(testing$totalSpended_cluster,log(testing$totalSpended))
grid()

train_data$winning_rate_cluster<- discretizarPorCantidadDatos(train_data$winning_rate,5)
winning_rate_cluster<- data.frame(id = 1:5, min = rep(0,5), max = rep(0,5))
train_data$totalSpended_cluster<- discretizarPorCantidadDatos(train_data$totalSpended,5)
totalSpended_cluster<- data.frame(id = 1:5, min = rep(0,5), max = rep(0,5))
for(i in 1:5){
    winning_rate_cluster$min[i]<- min(train_data$winning_rate[train_data$winning_rate_cluster == i])
    winning_rate_cluster$max[i]<- max(train_data$winning_rate[train_data$winning_rate_cluster == i])
    totalSpended_cluster$min[i]<- min(train_data$totalSpended[train_data$totalSpended_cluster == i])
    totalSpended_cluster$max[i]<- max(train_data$totalSpended[train_data$totalSpended_cluster == i])
}

plot(x = train_data$winning_rate_cluster,y = train_data$winning_rate)
grid()
 plot(x = train_data$totalSpended_cluster,y = log(train_data$totalSpended))
grid()

Nfolds<- 10
folds<- sample(rep_len(1:Nfolds,nrow(train_data)),nrow(train_data))
cortes<- seq(from=0.2,to=0.3,by = 0.01)
performance<- data.frame()
for(corte in cortes){
    for(i in 1:Nfolds){
        naiveModel<- naiveBayes(formula = label ~ ., 
                                data = train_data[folds != i,c("label","winning_rate_cluster","totalSpended_cluster")])
        testPos<- folds == i
        pred<- predict(naiveModel,train_data[testPos,c("winning_rate_cluster","totalSpended_cluster")], 
                       type = "raw")[,2] > corte
        tp<- sum(pred & train_data$label[testPos])
        fp<- sum(pred & (!train_data$label[testPos]))
        tn<- sum((!pred) & (!train_data$label[testPos]))
        fn<- sum((!pred) & train_data$label[testPos])
        precision<- tp / (tp + fp)
        recall<- tp / (tp + fn)
        score<- 2 * (precision * recall) / (precision + recall)
        performance<- rbind(performance,data.frame(
            corte = corte,
            iteracion = i,
            f_score = score,
            precision = precision,
            recall = recall
        ))
    }
}

evaluacion<- performance %>% group_by(corte) %>% 
    summarise(f_score = mean(f_score),precision = mean(precision), recall = mean(recall))
plot(evaluacion$corte,evaluacion$f_score)

naiveModel<-  naiveBayes(formula = label ~ .,data = train_data[c("label","winning_rate_cluster","totalSpended_cluster")])
corte<- evaluacion$corte[which.max(evaluacion$f_score)]

#Corremos el modelo a entregar
columnas<- c("WinBattle_sum_dsi0","WinBattle_sum_dsi1","WinBattle_sum_dsi2","WinBattle_sum_dsi3","LoseBattle_sum_dsi0",
             "LoseBattle_sum_dsi1","LoseBattle_sum_dsi2","LoseBattle_sum_dsi3","hard_negative","soft_negative")
evalSet <- load_csv_data("Datasets/evaluation.csv",sel_cols = columnas)
evalSet[, winning_rate := (WinBattle_sum_dsi0 + WinBattle_sum_dsi1 + WinBattle_sum_dsi2 + WinBattle_sum_dsi3) / 
               (LoseBattle_sum_dsi0 + LoseBattle_sum_dsi1 + LoseBattle_sum_dsi2 + LoseBattle_sum_dsi3 + WinBattle_sum_dsi0 + WinBattle_sum_dsi1 + WinBattle_sum_dsi2 + WinBattle_sum_dsi3)]
evalSet[, totalSpended := hard_negative + soft_negative]


evalSet$winning_rate_cluster<- 0
evalSet$totalSpended_cluster<- 0
for(i in 1:5){
    pos<- evalSet$winning_rate >= winning_rate_cluster$min[i] & 
        evalSet$winning_rate <= winning_rate_cluster$max[i]
    evalSet$winning_rate_cluster[pos]<- i
    pos<- evalSet$totalSpended >= totalSpended_cluster$min[i] & 
        evalSet$totalSpended <= totalSpended_cluster$max[i]
    evalSet$totalSpended_cluster[pos]<- i
}

evalSet<- evalSet %>% select(winning_rate_cluster,totalSpended_cluster)
gc()
pred<- predict(naiveModel,evalSet,type = "raw")
salida<- data.frame(
    id = 1:nrow(evalSet),
    label = pred[,2] / (2 * corte)
)

write.csv(salida,file = "naiveModel_summit1.csv")


# ----> Segundo modelo <-----

treeFeatures<- rpart.control(maxdepth = 30, minsplit = 0, 
                             minbucket=0, cp=0)
maxDepthTree<- rpart(label ~ ., 
                     data = train_data[,c("label","creditor","ChangeArena_sum","totalSpended",
                                          "BuyCard_sum","BuyCard_avg_ratio","notStartedBattleSomeDay",
                                          "winning_rate","winning_rate_dsi0","winning_rate_dsi1",
                                          "winning_rate_dsi2","winning_rate_dsi3","EnterShop_sum_dsi0",
                                          "EnterShop_sum_dsi1","EnterShop_sum_dsi2","EnterShop_sum_dsi3",
                                          "OpenChest_sum_dsi0","OpenChest_sum_dsi1","OpenChest_sum_dsi2",
                                          "OpenChest_sum_dsi3","UpgradeCard_sum_dsi0","UpgradeCard_sum_dsi1",
                                          "UpgradeCard_sum_dsi2","UpgradeCard_sum_dsi3")],
                     control = treeFeatures, method = "class")

calculateAUC<- function(model,datos,columnasSinLabel){
    xs<- seq(from = 0.01, to = 1, by = 0.01)
    N<- length(xs)
    area<- 0
    last_recall<- 0
    for(i in 1:N){
        pred<- predict(model,datos %>% select(all_of(columnasSinLabel)),type = "prob")[,2]
        pred<- pred > xs[i]
        tp<- sum(pred & datos$label)
        fp<- sum(pred & (!datos$label))
        tn<- sum((!pred) & (!datos$label))
        fn<- sum((!pred) & datos$label)
        precision<- tp / (tp + fp)
        recall<- 1 - (tp / (tp + fn))
        area<- area + (precision * (recall - last_recall))
    }
    return(area)
}

columnas<- c("winning_rate", "totalSpended", "OpenChest_sum_dsi3", "EnterShop_sum_dsi3", "BuyCard_sum","label")
columnasSinLabel<- c("winning_rate", "totalSpended", "OpenChest_sum_dsi3", "EnterShop_sum_dsi3", "BuyCard_sum")
# testing_set<- train_data %>% filter(1:nrow(train_data) %in% sample(nrow(train_data),10000)) %>% select(all_of(columnas))
performance<- data.frame()
kfolds<- 3
values<- 3
folds<- sample(rep_len(1:kfolds,nrow(train_data)),nrow(train_data))
for(v in 1:values){
    cp_value<- runif(1, max = 0.5)
    treeSettings<-  rpart.control(cp = cp_value, maxdepth = 30)
    for(k in 1:kfolds){
        tree_fit<- rpart(label ~ ., 
                         data = train_data %>% filter(folds != k),
                         control = treeSettings)
        validationFold<- folds == k
        # pred<- predict(tree, testing_set %>% filter(validationFold) %>% select(columnasSinLabel), type="class")
        # tp<- sum(pred & testing_set$label[validationFold])
        # fp<- sum(pred & (!testing_set$label[validationFold]))
        # tn<- sum((!pred) & (!testing_set$label[validationFold]))
        # fn<- sum((!pred) & testing_set$label[validationFold])
        # precision<- tp / (tp + fp)
        # recall<- tp / (tp + fn)
        # f_score<- 2 * (precision * recall) / (precision + recall)
        auc<- calculateAUC(tree_fit,train_data %>% filter(validationFold),columnasSinLabel)
        performance<- rbind(performance,data.frame(
            value = cp_value,
            fold = k,
            auc = auc
            # precision = precision,
            # recall = recall,
            # f_score = f_score
        ))
        
        print(paste("Value:",v,"| cp:",cp_value,"| Fold:",k))
    }
}

# Nunca funciono.....

# ----> Tercer modelo <----

columnas<- c("totalSpended", "OpenChest_sum_dsi3", "EnterShop_sum_dsi3", "BuyCard_sum","label")
columnasSinLabel<- c("totalSpended", "OpenChest_sum_dsi3", "EnterShop_sum_dsi3", "BuyCard_sum")

# train_data<- as.data.frame(train_data)
# train_set<- sample(1:nrow(train_data),10000)
train_set<- train_data[,columnas]
# test_set<- sample(1:nrow(train_data),1000)
test_labels<- train_data$label
test_set<- train_data[,columnasSinLabel]

standar_parameters<- data.frame(
    columnas = columnasSinLabel,
    mean = 0,
    sd = 0
)
for(c in 1:nrow(standar_parameters)){
    standar_parameters$mean[c]<- mean(train_set[,columnasSinLabel[c]])
    standar_parameters$sd[c]<- sd(train_set[,columnasSinLabel[c]])
    train_set[,columnasSinLabel[c]]<- (train_set[,columnasSinLabel[c]] - standar_parameters$mean[c]) / standar_parameters$sd[c]
    test_set[,columnasSinLabel[c]]<- (test_set[,columnasSinLabel[c]] - standar_parameters$mean[c]) / standar_parameters$sd[c]
}

library(neuralnet)
n <- names(train_set)
f <- as.formula(paste("label ~", paste(n[!n %in% "label"], collapse = " + ")))
nn <- neuralnet(formula = f,data=train_set[complete.cases(train_set),],hidden=c(3),act.fct = "logistic",rep =  1)
plot(nn)

pred<- compute(nn,test_set)

# ----> Cuarto modelo <-----
# 
columnas<- c("BuyCard_sum_dsi0","BuyCard_sum_dsi1","BuyCard_sum_dsi2","BuyCard_sum_dsi3","ChangeArena_sum_dsi0",
             "ChangeArena_sum_dsi1","ChangeArena_sum_dsi2","ChangeArena_sum_dsi3","hard_negative",
             "hard_positive","Label_max_played_dsi","LoseBattle_sum_dsi0","LoseBattle_sum_dsi1","LoseBattle_sum_dsi2",
             "LoseBattle_sum_dsi3","OpenChest_sum_dsi0","OpenChest_sum_dsi1","OpenChest_sum_dsi2",
             "OpenChest_sum_dsi3","soft_negative","soft_positive","StartBattle_sum_dsi0","StartBattle_sum_dsi1",
             "StartBattle_sum_dsi2","StartBattle_sum_dsi3","StartSession_sum_dsi0","StartSession_sum_dsi1",
             "StartSession_sum_dsi2","StartSession_sum_dsi3","UpgradeCard_sum_dsi0",
             "UpgradeCard_sum_dsi1","UpgradeCard_sum_dsi2","UpgradeCard_sum_dsi3","WinBattle_sum_dsi0",
             "WinBattle_sum_dsi1","WinBattle_sum_dsi2","WinBattle_sum_dsi3","StartTournamentBattle_sum_dsi0",
             "StartTournamentBattle_sum_dsi1","StartTournamentBattle_sum_dsi2","StartTournamentBattle_sum_dsi3")
dataList<- loadData(TRUE,columnas = columnas, sample_ratio = 0.4)
# dataList<- loadData(FALSE)
train_data<- as.data.frame(dataList$train)
eval_data<- as.data.frame(dataList$eval)
rm(dataList)
gc()

kfolds<- 2
folds<- sample(rep_len(1:kfolds, nrow(train_data)),nrow(train_data))
performance<- data.frame()
models<- list()
i<- 1
for(i in 1:5){
    print(paste("-----",i,"-----"))
    Ntrees<- sample(1500:3000,1)
    treeDepth<- sample(6:20,1)
    learningRate<- runif(1,min = 0.0001,max = 0.01)
    lossReduction<- runif(1, min = 0.6)
    sampleColumns<- runif(1,min = 0.5, max = 0.75)
    sampleRows<- runif(1,min = 0.5, max = 0.70)
    minChield<- sample(1:100,1)
    print(paste("Trees:",Ntrees,"| Tree Depth:",treeDepth,"| Chields weigth:",minChield))
    print(paste("Learning rate:",learningRate,"| Loss Reduction:",lossReduction))
    print(paste("Sample Columns:",sampleColumns,"| Sample rows:",sampleRows))
    foldsValidation<- rep(0,kfolds)
    models[[i]]<- list()
    for(kf in 1:kfolds){
        train_index<- folds != kf
        val_index<- folds == kf
        dtrain <- xgb.DMatrix(data = as.matrix(train_data[train_index,
                                                          names(train_data) != "label"]),
                              label = train_data$label[train_index])
        
        dvalid <- xgb.DMatrix(data = as.matrix(train_data[val_index, names(train_data) != "label"]),
                              label = train_data$label[val_index])
        
        watchlist <- list(train = dtrain, valid = dvalid)
        
        boostParams<- list(max_depth = treeDepth,
                           min_child_weight = minChield,
                           eta = learningRate,
                           gamma = lossReduction,
                             subsample = sampleRows,
                           colsample = sampleColumns,
                           objective = "binary:logistic",
                           verbose = 1, eval_metric = "auc")
        
        boost_model <- xgb.train(data = dtrain, nrounds = Ntrees,
                                 params = boostParams,
                                 watchlist = watchlist,
                                 print_every_n = 200)
        
        foldsValidation[kf]<- boost_model$evaluation_log[Ntrees,3]
        models[[i]][[kf]]<- boost_model
    }
    foldsValidation<- as.numeric(foldsValidation)
    performance<- rbind(performance,data.frame(
        id = i,
        Ntrees = Ntrees,
        treeDepth = treeDepth,
        chieldWeigth = minChield, 
        learningRate = learningRate,
        lossReduction = lossReduction,
        sampleColumns = sampleColumns,
        sampleRows = sampleRows,
        auc_mean = mean(foldsValidation),
        auc_sd = sd(foldsValidation)
    ))
}

# NTrees = 1800

dtrain <- xgb.DMatrix(data = as.matrix(train_data[,names(train_data) != "label"]),
                      label = train_data$label)

pos<-which.max(performance$auc_mean)
boostParams<- list(max_depth = performance$treeDepth[pos],
                   min_child_weight = performance$chieldWeigth[pos],
                   eta = performance$learningRate[pos],
                   gamma = performance$lossReduction[pos],
                   subsample = performance$sampleRows[pos],
                   colsample = performance$sampleColumns[pos],
                   objective = "binary:logistic",
                   verbose = 1, eval_metric = "auc")

boost_model <- xgb.train(data = dtrain, nrounds = performance$Ntrees[pos],
                         params = boostParams,
                         watchlist = watchlist,
                         print_every_n = 100)

rm(dtrain);rm(dvalid)
gc()

boost_model<- models[[1]][[0]]

eval_preds <- data.frame(id = 0:(nrow(eval_data) - 1),
                         Label = predict(boost_model,
                                         newdata = as.matrix(eval_data[,names(eval_data) != "label"])))

summary(eval_preds)
options(scipen = 999)  # Para evitar que se guarden valores en formato científico
write.table(eval_preds, "boost_model.csv",
            sep = ",", row.names = FALSE, quote = FALSE)

