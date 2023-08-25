# FC_Panel_Data
dt <- data.table::fread(file.choose())
dt <- dt[, list(Weekly_Sales = sum(Weekly_Sales)), by = "Date"]# c("Store","Dept")]
dt <- Rodeo::AutoLagRollStats(
  data                 = dt,
  DateColumn           = "Date",
  Targets              = "Weekly_Sales",
  TimeUnitAgg          = "weeks",
  TimeGroups           = "weeks", #c("days","weeks","months","quarters"),
  TimeUnit             = "weeks",
  RollOnLag1           = TRUE,
  Type                 = "Lag",
  SimpleImpute         = TRUE,
  Lags                 = 1:50, #list("days" = c(seq(1,5,1)), "weeks" = c(seq(1,3,1)), "months" = c(seq(1,2,1)), "quarters" = c(seq(1,2,1))),
  Debug                = FALSE)

ACF_Data <- data.table::data.table(Lag = 1:50, Cor = 0.0, `Lower 95th` = 0.0, `Upper 95th` = 0.0)
for(i in 1:50) {
  lag_test <- cor.test(x = dt$Weekly_Sales, y = dt[[paste0("weeks_LAG_",i,"_Weekly_Sales")]])
  data.table::set(ACF_Data, i = i, j = "Lag", value = i)
  data.table::set(ACF_Data, i = i, j = "Cor", value = lag_test$estimate)
  data.table::set(ACF_Data, i = i, j = "Lower 95th", value = lag_test$conf.int[1L])
  data.table::set(ACF_Data, i = i, j = "Upper 95th", value = lag_test$conf.int[2L])
}

# Build Line plot
x <- data.table::copy(ACF_Data)
x[, Lag := as.character(Lag)]
AutoPlots::Plot.Bar(
  dt = x,
  PreAgg = TRUE,
  YVar = "Weekly_Sales",
  LabelValues = NULL,
  YVarTrans = "Identity",
  AggMethod = "sum",
  Height = NULL,
  Width = NULL,
  Title = "Bar Plot",
  ShowLabels = FALSE,
  Title.YAxis = NULL,
  Title.XAxis = NULL,
  EchartsTheme = "macarons",
  TimeLine = TRUE,
  X_Scroll = TRUE,
  Y_Scroll = TRUE,
  TextColor = "white",
  title.fontSize = 22,
  title.fontWeight = "bold",
  title.textShadowColor = "#63aeff",
  title.textShadowBlur = 3,
  title.textShadowOffsetY = 1,
  title.textShadowOffsetX = -1,
  xaxis.fontSize = 14,
  yaxis.fontSize = 14,
  xaxis.rotate = 0,
  yaxis.rotate = 0,
  ContainLabel = TRUE,
  Debug = FALSE
)


YVar = "Weekly_Sales"
DateVar = "Date"
TimeUnit = "weeks"
LabelValues = NULL
YVarTrans = "Identity"
AggMethod = "sum"
Height = NULL
Width = NULL
Title = "Bar Plot"
ShowLabels = FALSE
Title.YAxis = NULL
Title.XAxis = NULL
EchartsTheme = "macarons"
TimeLine = TRUE
X_Scroll = TRUE
Y_Scroll = TRUE
TextColor = "white"
title.fontSize = 22
title.fontWeight = "bold"
title.textShadowColor = "#63aeff"
title.textShadowBlur = 3
title.textShadowOffsetY = 1
title.textShadowOffsetX = -1
xaxis.fontSize = 14
yaxis.fontSize = 14
xaxis.rotate = 0
yaxis.rotate = 0
ContainLabel = TRUE
Debug = FALSE
MaxLags = 50

