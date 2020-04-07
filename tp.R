library("data.table")

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

csv_file <- "TP/Datasets/train_1.csv"
df <- load_csv_data(csv_file, sample_ratio = 1)


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

train_data<- load_train_data("TP/Datasets/", sample_ratio = 0.1)

# Exploratory analysis:
# Age y site tienen muy pocos datos
# soft/hard possitive/negative, no entiendo que son pero en las diapo lo 
#      colocan para calcular si un usuario es "acreedor"
# traffic_type es una constante
# device_model tiene muchas categorias, pocas con repeticiones, quizas
#      convenga generar una variable binaria de algun dispositivo en 
#      particular si es que tiene relevancia. 
# BuyCard_sum_dsi1 tiene valores negativos

train_data[, creditor := ((soft_positive + hard_positive) -
                      (soft_negative + hard_negative)) > 0]

train_data[, label := Label_max_played_dsi == 3]

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
