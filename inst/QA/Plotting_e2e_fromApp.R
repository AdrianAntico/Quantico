if(!exists('DataList')) DataList <- list()
library(data.table)
filename <- basename('FakeBevData.csv')
DataList[[filename]] <- DataMuse:::LoadCSV(file.choose())

# Plotting: LinePlot
data1 <- DataList[['FakeBevData.csv']]

# Plotting Variables
GroupVars <- 'Customer'
FilterVar1 <- NULL
FilterVar2 <- NULL
FilterVar3 <- NULL
FilterVar4 <- NULL
FilterValue_1_1 <- NULL
FilterValue_1_2 <- NULL
FilterValue_2_1 <- NULL
FilterValue_2_2 <- NULL
FilterValue_3_1 <- NULL
FilterValue_3_2 <- NULL
FilterValue_4_1 <- NULL
FilterValue_4_2 <- NULL
Levels1 <- c('Location 11','Location 14','Location 16','Location 17')
Levels2 <- NULL
Levels3 <- NULL
data1 <- data1[
  get(GroupVars[1L]) %chin% eval(Levels1)
  ,
  lapply(.SD, mean, na.rm = TRUE),
  .SDcols = c('Daily Liters'),
  by = c('Date','Customer')
]

# Build Plot
AutoPlots::Plot.StandardPlots(
  PlotType = 'LinePlot',
  dt = data1,
  AggMethod = mean,
  SampleSize = 100000,
  PreAgg = TRUE,
  YVar = c('Daily Liters'),
  DualYVar = NULL,
  XVar = c('Date'),
  ZVar = NULL,
  GroupVar = c('Customer'),
  YVarTrans = 'Identity',
  DualYVarTrans = 'Identity',
  XVarTrans = 'Identity',
  ZVarTrans = 'Identity',
  FacetRows = 1,
  FacetCols = 1,
  FacetLevels = Levels1,
  NumberBins = 30,
  Title = 'LinePlot',
  ShowLabels = FALSE,
  Title.YAxis = 'Daily Liters',
  Title.XAxis = 'Date',
  EchartsTheme = 'wef',
  TimeLine = FALSE,
  FontSize = 14,
  TextColor = '#e2e2e2')

# Standard Plots Function
PlotType = 'LinePlot'
dt = data1
AggMethod = mean
SampleSize = 100000
PreAgg = TRUE
YVar = c('Daily Liters')
DualYVar = NULL
XVar = c('Date')
ZVar = NULL
GroupVar = c('Customer')
YVarTrans = 'Identity'
DualYVarTrans = 'Identity'
XVarTrans = 'Identity'
ZVarTrans = 'Identity'
FacetRows = 1
FacetCols = 1
FacetLevels = c('Location 11','Location 13','Location 16','Location 18')
NumberBins = 30
Title = 'LinePlot'
ShowLabels = FALSE
Title.YAxis = 'Daily Liters'
Title.XAxis = 'Date'
EchartsTheme = 'wef'
TimeLine = FALSE
FontSize = 14
TextColor = '#e2e2e2'
Debug = TRUE
Width = "1100px"
Height = "600px"
Title.FontSize = 14

# LinePlot Args
dt = dt
PreAgg = PreAgg
AggMethod = AggMethod
XVar = XVar
YVar = YVar
DualYVar = DualYVar
GroupVar = GroupVar
YVarTrans = YVarTrans
DualYVarTrans = DualYVarTrans
XVarTrans = XVarTrans
FacetRows = FacetRows
FacetCols = FacetCols
FacetLevels = FacetLevels
Width = Width
Height = Height
Title = Title
ShowLabels = ShowLabels
Title.YAxis = Title.YAxis
Title.XAxis = Title.XAxis
EchartsTheme = EchartsTheme
TimeLine = TimeLine
X_Scroll = TRUE
Y_Scroll = TRUE
TextColor = TextColor
title.fontSize = Title.FontSize
Debug = Debug
Smooth = TRUE
ShowSymbol = TRUE
xaxis.fontSize = 14
xaxis.rotate = 0
ContainLabel = TRUE
yaxis.fontSize = 14
yaxis.rotate = 90
title.textShadowColor = '#63aeff'
title.textShadowBlur = 3
title.textShadowOffsetY = 1
title.textShadowOffsetX = -1
title.fontWeight = 14




