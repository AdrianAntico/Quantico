# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Get Data Columns                          ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
library(data.table)
data1 <- Rappture::DM.pgQuery(
  Host = 'localhost',
  DataBase = 'KompsProcessed',
  SELECT = c('ARTICLE','BRAND','CHILLED_Liters_PerDay','CHILLED_Margin_PerDay','CHILLED_Net_Revenue_PerDay','CHILLED_Units_PerDay','CUSTOMER_COD_char','DATE_ISO'),
  AggStat = 'AVG',
  FROM = 'POS_Processed_Long_Daily_backward',
  GroupBy = NULL,
  SamplePercent = 1,
  User = 'postgres',
  Port = 5432,
  Password = 'Aa1028#@')

TargetLevel <- NULL
PlotType <- "LinePlot"
Debug <- FALSE

YVar <- "CHILLED_Margin_PerDay"
XVar <- "DATE_ISO"
ZVar <- NULL
GroupVars <- NULL# c("BRAND","ARTICLE","CUSTOMER_COD_char")
Levels1 <- NULL# data1[, .N, by = BRAND][order(-N)][1L:10L, BRAND]
Levels2 <- NULL# data1[, .N, by = ARTICLE][order(-N)][1L:20L, ARTICLE]
Levels3 <- NULL# data1[, .N, by = CUSTOMER_COD_char][order(-N)][1L:10L, CUSTOMER_COD_char]

FilterVar1 <- NULL# "CHILLED_Margin_PerDay"
FilterValue_1_1 <- NULL#
FilterValue_1_2 <- NULL# NULL
FilterLogic1 <- NULL# ">"

FilterVar2 <- NULL# "CHILLED_Units_PerDay"
FilterValue_2_1 <- NULL# 0.05
FilterValue_2_2 <- NULL# NULL
FilterLogic2 <- NULL# ">"

FilterVar3 <- FilterVar4 <- NULL
data <- data.table::copy(data1)



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Subset Columns                            ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# Subset
Total_Start <- Sys.time()
Keep <- unique(
  c(
    YVar,
    XVar,
    ZVar,
    GroupVars,
    FilterVar1,
    FilterVar2,
    FilterVar3,
    FilterVar4,
    TargetLevel))
data1 <- data1[, .SD, .SDcols = c(Keep)]

# Filter Loop
if(length(eval(parse(text = 'FilterVar1'))) > 0L) {
  for(i in seq_len(4L)) {
    if(length(eval(parse(text = paste0('FilterVar', i)))) > 0L) {
      data1 <- Rappture:::FilterLogicData(
        data1,
        FilterLogic = get(paste0('FilterLogic', i)),
        FilterVariable = get(paste0('FilterVar', i)),
        FilterValue = get(paste0('FilterValue_',i,'_1')),
        FilterValue2 = get(paste0('FilterValue_',i,'_2')),
        Debug = Debug)
    }
  }
}

data1 <- AutoPlots:::PreparePlotData(
  SubsetOnly = F,
  data = data1,
  Aggregate = if(exists('BarPlotAgg')) BarPlotAgg else 'mean',
  TargetVariable = c(YVar),
  DateVariable = XVar,
  GroupVariables = GroupVars,
  G1Levels = Levels1,
  G2Levels = Levels2,
  G3Levels = Levels3)
Total_End <- Sys.time()
cat("Total Run Time = ", difftime(time1 = Total_End, Total_Start))

# 0.7854719
# 0.6474731

# Optimal Run Time

library(data.table)
data1 <- Rappture::DM.pgQuery(
  Host = 'localhost',
  DataBase = 'KompsProcessed',
  SELECT = c('ARTICLE','BRAND','CHILLED_Liters_PerDay','CHILLED_Margin_PerDay','CHILLED_Net_Revenue_PerDay','CHILLED_Units_PerDay','CUSTOMER_COD_char','DATE_ISO'),
  AggStat = 'AVG',
  FROM = 'POS_Processed_Long_Daily_backward',
  GroupBy = NULL,
  SamplePercent = 1,
  User = 'postgres',
  Port = 5432,
  Password = 'Aa1028#@')

# Filter Rows, Subset Columns 2, and Aggregation
PlotType <- "LinePlot"
YVar <- "CHILLED_Margin_PerDay"
XVar <- "DATE_ISO"
ZVar <- NULL
GroupVars <- c("BRAND","ARTICLE","CUSTOMER_COD_char")
Levels1 <- data1[, .N, by = BRAND][order(-N)][1L:10L, BRAND]
Levels2 <- data1[, .N, by = ARTICLE][order(-N)][1L:20L, ARTICLE]
Levels3 <- data1[, .N, by = CUSTOMER_COD_char][order(-N)][1L:10L, CUSTOMER_COD_char]

FilterVar1 <- "CHILLED_Margin_PerDay"
FilterValue_1_1 <- 0.25
FilterValue_1_2 <- 1
FilterLogic1 <- ">"

FilterVar2 <- "CHILLED_Units_PerDay"
FilterValue_2_1 <- 0.05
FilterValue_2_2 <- NULL
FilterLogic2 <- ">"

FilterVar3 <- FilterVar4 <- NULL
data <- data.table::copy(data1)

fv1 <- length(FilterVar1) > 0L && length(FilterLogic1) > 0L
fv2 <- length(FilterVar2) > 0L && length(FilterLogic2) > 0L
fv3 <- length(FilterVar3) > 0L && length(FilterLogic3) > 0L
fv4 <- length(FilterVar4) > 0L && length(FilterLogic4) > 0L
gv1 <- fv1 && length(GroupVars) > 0L && length(Levels1) > 0L

fv1a <- fv1 && (fv2 || gv1)
fv2a <- fv2 && (fv3 || gv1)
fv3a <- fv3 && (fv4 || gv1)

# FV1
if(fv1 && FilterLogic1 %in% c('<','>','<=','>=','%in%','%like%')) {
  if(fv1a) {
    FV1 <- paste0("get(FilterVar1) ", FilterLogic1, " eval(FilterValue_1_1) & \n  ")
  } else {
    FV1 <- paste0("get(FilterVar1) ", FilterLogic1, " eval(FilterValue_1_1)\n  ")
  }
} else if(fv1 && FilterLogic1 %in% '%between%') {
  if(fv1a) {
    FV1 <- paste0("get(FilterVar1) > eval(FilterValue_1_1) & get(FilterVar1) < eval(FilterValue_1_2) &\n  ")
  } else {
    FV1 <- paste0("get(FilterVar1) > eval(FilterValue_1_1) & get(FilterVar1) < eval(FilterValue_1_2)\n  ")
  }
} else if(fv1 && FilterLogic1 %in% c('not %between%')) {
  if(fv1a) {
    FV1 <- paste0("get(FilterVar1) < eval(FilterValue_1_1) & get(FilterVar1) > eval(FilterValue_1_2) & \n  ")
  } else {
    FV1 <- paste0("get(FilterVar1) < eval(FilterValue_1_1) & get(FilterVar1) > eval(FilterValue_1_2)\n  ")
  }
}

# FV2
if(fv2 && FilterLogic2 %in% c('<','>','<=','>=','%in%','%like%')) {
  if(fv2a) {
    FV2 <- paste0("get(FilterVar2) ", FilterLogic2, " eval(FilterValue_2_1) & \n  ")
  } else {
    FV2 <- paste0("get(FilterVar2) ", FilterLogic2, " eval(FilterValue_2_1)\n  ")
  }
} else if(fv2 && FilterLogic2 %in% '%between%') {
  if(fv2a) {
    FV2 <- paste0("get(FilterVar2) > eval(FilterValue_2_1) & get(FilterVar2) < eval(FilterValue_2_2) & \n  ")
  } else {
    FV2 <- paste0("get(FilterVar2) > eval(FilterValue_2_1) & get(FilterVar2) < eval(FilterValue_2_2)\n  ")
  }
} else if(fv2 && FilterLogic2 %in% c('not %between%')) {
  if(fv2a) {
    FV2 <- paste0("get(FilterVar2) < eval(FilterValue_2_1) & get(FilterVar2) > eval(FilterValue_2_2) & \n  ")
  } else {
    FV2 <- paste0("get(FilterVar2) < eval(FilterValue_2_1) & get(FilterVar2) > eval(FilterValue_2_2)\n  ")
  }
}
if(fv3 && FilterLogic3 %in% c('<','>','<=','>=','%in%','%like%')) {
  if(fv3a) {
    FV3 <- paste0("get(FilterVar3) ", FilterLogic3, " eval(FilterValue_3_1) & \n  ")
  } else {
    FV3 <- paste0("get(FilterVar3) ", FilterLogic3, " eval(FilterValue_3_1)\n  ")
  }

} else if(fv3 && FilterLogic3 %in% '%between%') {
  if(fv3a) {
    FV3 <- paste0("get(FilterVar3) > eval(FilterValue_3_1) & get(FilterVar3) < eval(FilterValue_3_2) & \n  ")
  } else {
    FV3 <- paste0("get(FilterVar3) > eval(FilterValue_3_1) & get(FilterVar3) < eval(FilterValue_3_2)\n  ")
  }

} else if(fv3 && FilterLogic3 %in% c('not %between%')) {
  if(fv3a) {
    FV3 <- paste0("get(FilterVar3) < eval(FilterValue_3_1) & get(FilterVar3) > eval(FilterValue_3_2) & \n  ")
  } else {
    FV3 <- paste0("get(FilterVar3) < eval(FilterValue_3_1) & get(FilterVar3) > eval(FilterValue_3_2)\n  ")
  }

}
if(fv4 && FilterLogic4 %in% c('<','>','<=','>=','%in%','%like%')) {
  if(length(GroupVars) > 0L && length(Levels1) > 0L) {
    FV4 <- paste0("get(FilterVar) ", FilterLogic4, " eval(FilterValue_4_1),\n  ")
  } else {
    FV4 <- paste0("get(FilterVar) ", FilterLogic4, " eval(FilterValue_4_1),\n  ")
  }

} else if(fv4 && FilterLogic4 %in% '%between%') {
  FV4 <- paste0("get(FilterVar4) > eval(FilterValue_4_1) & get(FilterVar4) < eval(FilterValue_4_2)\n  ")
} else if(fv4 && FilterLogic4 %in% c('not %between%')) {
  FV4 <- paste0("get(FilterVar4) < eval(FilterValue_4_1) & get(FilterVar4) > eval(FilterValue_4_2)\n  ")
}

DataPrepare <- paste0(
  "data1 <- data1[\n  ",
  if(fv1) FV1,
  if(fv2) FV2,
  if(fv3) FV3,
  if(fv4) FV4,
  if(length(GroupVars) > 0L && length(Levels1) > 0L) {
    "get(GroupVars[1L]) %chin% eval(Levels1)\n  "
  },
  if(length(GroupVars) > 0L && !is.na(GroupVars[2L]) && length(Levels2) > 0L) {
    "& get(GroupVars[2L]) %chin% eval(Levels2)\n  "
  },
  if(length(GroupVars) > 0L && !is.na(GroupVars[3L]) && length(Levels3) > 0L) {
    "& get(GroupVars[3L]) %chin% eval(Levels3),\n  "
  }  else {
    ",\n  "
  },
  if(PlotType %in% c('LinePlot','BarPlot')) {
    "lapply(.SD, mean, na.rm = TRUE),\n  .SDcols = c(YVar, ZVar),\n  by = c(XVar,GroupVars,TargetLevel)\n]"
  } else {
    ".SDcols = c(YVar, ZVar),\n  by = c(XVar,GroupVars,TargetLevel)\n]"
  })

parse(text = DataPrepare)
