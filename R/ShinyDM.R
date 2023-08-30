# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# CSV / Local                                                                                          ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title LoadCSV or Load Parquet
#'
#' @description Use this function to import csv's, track the time it was imported, and remove other objects
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param Infile Passthrough
#' @param Parquet Passthrough
#' @param Debug FALSE
#'
#' @noRd
LoadCSV <- function(Infile = NULL, Parquet = FALSE, Debug = FALSE) {
  if(Debug) print('LoadCSV 1')
  if(is.null(Infile)) return(NULL)
  if(!Parquet) {
    if('datapath' %in% names(Infile)) {
      x <- data.table::fread(file = Infile$datapath)
    } else {
      x <- data.table::fread(file = Infile)
    }
  } else {
    if('datapath' %in% names(Infile)) {
      x <- data.table::setDT(arrow::read_parquet(file = Infile$datapath))
    } else {
      x <- data.table::setDT(arrow::read_parquet(file = Infile))
    }
  }
  return(x)
}

#' @title DM.BenchmarkData
#'
#' @description Data generator from the h2o benchmark study
#'
#' @param N = 10000000,
#' @param Levels = 1000000
#' @param NAs = -1L
#' @param Sort = TRUE
#' @param Push2PostGRE = FALSE
#' @param Table = 'H2O_BM_10m_1m'
#' @param Host = 'localhost'
#' @param CloseConnection = FALSE
#' @param DBName = 'AutoQuant'
#' @param User = 'postgres'
#' @param Port = 5432
#' @param Password = 'Aa1028#
#'
#' @export
DM.BenchmarkData <- function(N = 10000000,
                          Levels = 1000000,
                          NAs = -1L,
                          Sort = TRUE,
                          Push2PostGRE = FALSE,
                          Table = 'H2O_BM_10m_1m',
                          Host = 'localhost',
                          CloseConnection = FALSE,
                          DBName = 'AutoQuant',
                          User = 'postgres',
                          Port = 5432,
                          Password = 'Aa1028#@') {
  library(data.table)
  N = as.integer(N); K = as.integer(Levels); nas = as.integer(NAs); sort = as.integer(Sort)
  stopifnot(nas<=100L, nas>=0L, sort%in%c(0L,1L))
  set.seed(108)
  #cat(sprintf("Producing data of %s rows, %s K groups factors, %s NAs ratio, %s sort flag\n", pretty_sci(N), pretty_sci(K), nas, sort))
  DT = list()
  # DT[["id1"]] = sample(sprintf("id%03d", seq_len(K)), N, TRUE)      # large groups (char)
  # DT[["id2"]] = sample(sprintf("id%03d", seq_len(ceiling(0.10 * K))), N, TRUE)      # small groups      (char)
  # DT[["id3"]] = sample(sprintf("id%010d",seq_len(ceiling(0.01 * K))), N, TRUE)      # large groups      (char)
  DT[["RandomEffect_100"]] = sample(K, N, TRUE)                      # Lowest grain Factor (individual)            (int)
  DT[["RandomEffect_10"]] = sample(ceiling(0.10 * K), N, TRUE)      # 2nd lowest grain Factor (1 hierarchy up)    (int)
  DT[["RandomEffect_1"]] = sample(ceiling(0.01 * K), N, TRUE)      # 3rd lowest grain Factor (2 hierarchy up)    (int)
  DT[["FixedEffect5"]] =  sample(5, N, TRUE)                          # fixed effect: int in range [1,5]
  DT[["FixedEffect15"]] =  sample(15, N, TRUE)                         # fixed effect: int in range [1,15]
  DT[["TargetVariable"]] =  round(runif(N,max=100),6)                   # numeric e.g. 23.574912
  setDT(DT)

  # Missing Values
  if(nas > 0L) {
    cat("Inputting NAs\n")
    for(col in names(DT)[seq_len(3L)]) {
      ucol = unique(DT[[col]])
      nna = as.integer(length(ucol) * (nas/100))
      if(nna) set(DT, DT[.(sample(ucol, nna)), on=col, which=TRUE], col, NA)
      rm(ucol)
    }
    nna = as.integer(nrow(DT) * (nas/100))
    if(nna) {
      for(col in names(DT)[4L:6L]) set(DT, sample(nrow(DT), nna), col, NA)
    }
  }

  # Sort Data
  if(sort == 1L) {
    cat("Sorting data\n")
    setkeyv(DT, names(DT)[seq_len(5L)])
  }

  if(Push2PostGRE == TRUE) {
    AutoQuant:::PostGRE_RemoveCreateAppend(
      data = DT,
      Table = 'H2O_BM_10m_1m',
      Host = 'localhost',
      CloseConnection = FALSE,
      DBName = 'AutoQuant',
      User = 'postgres',
      Port = 5432,
      Password = 'Aa1028#@')
  }
  return(DT)
}

#' @title DM.DataListUpdate
#'
#' @description Store useful info
#'
#' @param dl = DataList from App
#' @param dn = Dataset name stored inside DataList
#' @param Sample Create a sample
#' @param SampleSize default 1000 rows
#'
#' @return list with names 'data', 'cols', 'rows', 'colnames'
#' @keywords internal
DM.DataListUpdate <- function(dl, dn, Sample = TRUE, SampleSize = 1000) {

  print("DM.DataListUpdate 1")

  # Get data
  if(length(dl) > 0L) {
    print("DM.DataListUpdate 2")
    if(length(dn) > 0L) {
      print("DM.DataListUpdate 3")
      dt <- dl[[dn]][['data']]
    } else {
      print("DM.DataListUpdate 4")
      return(NULL)
    }
  } else {
    print("DM.DataListUpdate 5")
    return(NULL)
  }

  # Convert to data.table if not already
  if(!data.table::is.data.table(dt)) {
    print("DM.DataListUpdate 6")
    tryCatch({data.table::setDT(dt)}, error = function(x) return(NULL))
  }

  # Get data
  print("DM.DataListUpdate 7")
  print(dt)
  if(Sample && dt[,.N] > 0L) dl[[dn]][['sample']] <- dt[seq_len(min(.N, SampleSize))]

  # Colnames
  print("DM.DataListUpdate 8")
  if(dt[,.N] > 0L) dl[[dn]][['colnames']] <- names(dt)
  return(dl)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# ALL Querying                                                                                         ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title QueryBuilder
#'
#' @description QueryBuilder will build a query string that can be executed
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param DB value
#' @param Table value
#' @param Columns value
#' @param GroupByColumns value
#' @param AggStat "AVE", "COUNT", "MAX", "MIN", "SUM"
#' @param Limit 10
#' @param FilterVariable1 value
#' @param FilterVariable2 value
#' @param FilterVariable3 value
#' @param FilterVariable4 value
#' @param FilterLogic1 value
#' @param FilterLogic2 value
#' @param FilterLogic3 value
#' @param FilterLogic4 value
#' @param FilterValue1 value
#' @param FilterValue2 value
#' @param FilterValue3 value
#' @param FilterValue4 value
#'
#' @examples
#' \dontrun{
#' DB = 'AutoQuant'
#' Table = "BenchmarkData1.csv"
#' Columns = c("Region", "Store", "Dept", "Weekly_Sales")
#' GroupByColumns = c("Region", "Store", "Dept")
#' AggStat = c("AVG")
#' Limit = 10
#' FilterVariable1 = 'Region'
#' FilterVariable2 = 'Store'
#' FilterVariable3 = 'Dept'
#' FilterVariable4 = 'Weekly_Sales'
#' FilterLogic1 = 'in'
#' FilterLogic2 = 'in'
#' FilterLogic3 = 'in'
#' FilterLogic4 = '<'
#' FilterValue1 = 'A'
#' FilterValue2 = '1'
#' FilterValue3 = '1'
#' FilterValue4 = '5000'
#' }
#'
#' @return Query string
#'
#' @export
QueryBuilder <- function(DB = NULL,
                         Table = NULL,
                         Columns = NULL,
                         GroupByColumns = NULL,
                         AggStat = NULL,
                         SamplePercent = 1.0,
                         Limit = NULL) {

  # Only create a query if conditions are met
  if(length(Columns) > 0L || length(GroupByColumns) > 0L) {
    Column_iter <- seq_along(Columns)
    Column_maxiter <- max(Column_iter)
    Select <- paste0("SELECT \n  ")
    ColumnsVec <- c()

    # i = 1
    # i = 2
    # i = 3
    # SELECT Columns
    if(length(Columns) > 0L) {
      for(i in Column_iter) {
        if(i < Column_maxiter) {
          if(length(GroupByColumns) > 0L && Columns[i] %in% GroupByColumns) {
            ColumnsVec <- c(ColumnsVec, paste0(shQuote(Columns[i]), ",\n  "))
          } else if(length(GroupByColumns) > 0L && !Columns[i] %in% GroupByColumns) {
            ColumnsVec <- c(ColumnsVec, paste0(AggStat, "(", shQuote(Columns[i]), "),\n  "))
          } else {
            ColumnsVec <- c(ColumnsVec, paste0(shQuote(Columns[i]),",\n  "))
          }
        } else {
          if(length(GroupByColumns) > 0L && Columns[i] %in% GroupByColumns) {
            ColumnsVec <- c(ColumnsVec, paste0(shQuote(Columns[i]), "\n  "))
          } else if(length(GroupByColumns) > 0L && !Columns[i] %in% GroupByColumns) {
            ColumnsVec <- c(ColumnsVec, paste0(AggStat, "(", shQuote(Columns[i]), ")\n  "))
          } else {
            ColumnsVec <- c(ColumnsVec, paste0(shQuote(Columns[i]),"\n  "))
          }
        }
      }
    } else {
      ColumnsVec <- '* '
    }

    # Combine strings
    query <- paste0(c(Select, ColumnsVec, "FROM \n  "), collapse = "")
    query <- paste0(c(query, paste0(shQuote(Table))), collapse = " ")

    # GROUP BY
    if(length(GroupByColumns) > 0L) {

      GroupByColumnsVec <- c()
      GroupByColumn_iter <- seq_along(GroupByColumns)
      GroupByColumn_maxiter <- max(GroupByColumn_iter)

      # Build string
      for(i in GroupByColumn_iter) {
        if(i < GroupByColumn_maxiter) {
          GroupByColumnsVec <- c(GroupByColumnsVec, paste0(shQuote(GroupByColumns[i]), ",\n  "))
        } else {
          GroupByColumnsVec <- c(GroupByColumnsVec, paste0(shQuote(GroupByColumns[i]), ""))
        }
      }

      # Combine strings
      query <- paste0(c(query, paste0("\nGROUP BY \n  ")), collapse = " ")
      query <- paste0(c(query, GroupByColumnsVec), collapse = " ")
    }

    # Sample Percent
    if(SamplePercent < 1.0) query <- paste0(c(query, paste0("\nWHERE random() < ", SamplePercent)), collapse = "")

    # Limit
    if(length(Limit) > 0L && Limit != "") {
      query <- paste0(c(query, paste0("\nlimit ", Limit)), collapse = "")
    }

    # Return
    query <- paste0(c(query, paste0(";")), collapse = "")
    return(query)
  } else {

    # Combine strings
    query <- paste0(c("SELECT \n  * \nFROM \n  "), collapse = "")
    query <- paste0(c(query, paste0(shQuote(Table))), collapse = " ")

    # Sample Percent
    if(SamplePercent < 1.0) query <- paste0(c(query, paste0("\nWHERE random() < ", SamplePercent)), collapse = "")

    # Limit
    if(length(Limit) > 0L && Limit != "") {
      query <- paste0(c(query, paste0("\nlimit ", Limit)), collapse = "")
    }

    # Return
    query <- paste0(c(query, paste0(";")), collapse = "")
    return(query)
  }
}

#' @title DM.pgQuery
#'
#' @description PostGRE DM.pgQuery get data from a database table
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param Query SQL Statement in quotes
#' @param Host If Connection is NULL then this must be supplied. host
#' @param DBName If Connection is NULL then this must be supplied. dbname
#' @param User If Connection is NULL then this must be supplied. user
#' @param Port If Connection is NULL then this must be supplied. port
#' @param Password If Connection is NULL then this must be supplied. password
#' @param DataBase From App
#' @param SELECT From App
#' @param AggStat From App
#' @param FROM From App
#' @param GroupBy From App
#' @param SamplePercent From App
#'
#' @examples
#' \dontrun{
#'
#' PlottingExample <- qs::qload(file = file.choose())
#' Columns <- c(
#'   "DATE_ISO",
#'   "ARTICLE",
#'   "BRAND",
#'   "CUSTOMER_COD_char",
#'   "CHILLED_Margin_PerDay",
#'   "CHILLED_Liters_PerDay",
#'   "CHILLED_Units_PerDay")
#' data <- DataMuse::DM.pgQuery(
#'   Query = NULL,
#'   DataBase = "KompsProcessed",
#'   SELECT = Columns,
#'   AggStat = "AVG",
#'   FROM = "POS_Processed_Long_Daily_backward",
#'   GroupBy = NULL,
#'   SamplePercent = 1,
#'   Host = 'localhost',
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = "Aa...")
#' }
#'
#' @export
DM.pgQuery <- function(Host = NULL,
                       Query = NULL,
                       DataBase = NULL,
                       SELECT = "*",
                       AggStat = 'AVG',
                       FROM = NULL,
                       GroupBy = NULL,
                       SamplePercent = 1.0,
                       User = NULL,
                       Port = NULL,
                       Password = NULL) {

  # Connect to db
  Connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Host,
    dbname = DataBase,
    user = User,
    port = Port,
    password = Password)

  if(length(Query) == 0L) {
    if(all(SELECT == "*")) SELECT <- NULL
    Query <- DataMuse:::QueryBuilder(
      DB = DataBase,
      Table = FROM,
      Columns = SELECT,
      GroupByColumns = NULL,
      AggStat = NULL,
      SamplePercent = SamplePercent,
      Limit = NULL)
  }

  # Pull data from db
  rows <- DBI::dbSendQuery(Connection, Query)
  if(grepl(pattern = 'select', x = tolower(Query))) {x <- DBI::dbFetch(rows); data.table::setDT(x)}
  suppressWarnings(DBI::dbDisconnect(Connection))
  return(x)
}

#' @title DM.pgAppend
#'
#' @description PostGRE DM.pgAppend get data from a database table
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param data Source data.table
#' @param DataBase dbname
#' @param Append Set to TRUE to append data, FALSE to overwrite data
#' @param Host host
#' @param User If Connection is NULL then this must be supplied. user
#' @param Port If Connection is NULL then this must be supplied. port
#' @param Password If Connection is NULL then this must be supplied. password
#'
#' @examples
#' \dontrun{
#' DataMuse::DM.pgAppend(
#'   data = data,
#'   Table = 'somename',
#'   Append = FALSE,
#'   Host = 'localhost',
#'   DataBase = 'AutoQuant',
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = 'Aa...')
#'
#' # data = data
#' # Host = 'localhost'
#' # DataBase = 'Testing'
#' # User = 'postgres'
#' # Port = 5432
#' # Password = 'Aa...'
#' }
#'
#' @export
DM.pgAppend <- function(data = NULL,
                        DataBase = NULL,
                        Table = NULL,
                        Append = FALSE,
                        Host = NULL,
                        User = NULL,
                        Port = NULL,
                        Password = NULL) {

  # Connect to db
  Connection <- tryCatch({DBI::dbConnect(
    RPostgres::Postgres(),
    host = Host,
    dbname = DataBase,
    user = User,
    port = Port,
    password = Password)}, error = function(x) {
      print('DBI::dbWriteTable() generated an error')
      print(paste0("Host = ", Host))
      print(paste0("DataBase = ", DataBase))
      print(paste0("User = ", User))
      print(paste0("Port = ", Port))
      print(paste0("Password = ", Password))
      return(NULL)
    })

  # NULL Check
  if(length(Connection) == 0L) return(NULL)

  # Pull data from db
  tryCatch({DBI::dbWriteTable(
    conn = Connection,
    name = DBI::dbQuoteIdentifier(conn = Connection, x = Table),
    value = data,
    append = Append,
    row.names = FALSE,
    overwrite = if(Append) FALSE else TRUE)}, error = function(x) {
      print('DBI::dbWriteTable() generated an error')
      print(paste0("Host = ", Host))
      print(paste0("DataBase = ", DataBase))
      print(paste0("User = ", User))
      print(paste0("Port = ", Port))
      print(paste0("Password = ", Password))
    })
  suppressWarnings(DBI::dbDisconnect(Connection))
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# POSTGRE Tables                                                                                       ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title DM.pgCreateTable
#'
#' @description PostGRE DM.pgCreateTable get data from a database table
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param data Source data.table. If you supply a Schema, data will be ignored.
#' @param Table Name of table you want created
#' @param Schema Optional. Advised to use if type inference is fuzzy
#' @param Host If Connection is NULL then this must be supplied. host name
#' @param DataBase If Connection is NULL then this must be supplied. database name
#' @param User If Connection is NULL then this must be supplied. user name
#' @param Port If Connection is NULL then this must be supplied. port name
#' @param Password user password
#'
#' @examples
#' \dontrun{
#' DataMuse::DM.pgCreateTable(
#'   data,
#'   DataBase = 'Testing',
#'   Schema = NULL,
#'   Table = NULL,
#'   Host = 'localhost',
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = 'Aa...')
#' }
#'
#' @export
DM.pgCreateTable <- function(data = NULL,
                             DataBase = NULL,
                             Schema = NULL,
                             Table = NULL,
                             Host = NULL,
                             User = NULL,
                             Port = NULL,
                             Password = NULL) {

  # Args check
  if(is.null(data) && is.null(Schema)) {print('data or Schema must not be NULL'); return(NULL)}

  # Connect to db
  Connection <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    host = Host,
    dbname = DataBase,
    user = User,
    port = Port,
    password = Password)

  # Create table
  if(is.null(Schema)) {
    DBI::dbCreateTable(
      conn = Connection,
      name = DBI::dbQuoteIdentifier(conn = Connection, x = Table),
      fields = data,
      row.names = NULL,
      temporary = FALSE)
  } else {
    DBI::dbCreateTable(
      conn = Connection,
      name = DBI::dbQuoteIdentifier(conn = Connection, x = Table),
      fields = Schema,
      row.names = NULL,
      temporary = FALSE)
  }
  suppressWarnings(DBI::dbDisconnect(Connection))
}

#' @title DM.pgRemoveTable
#'
#' @description PostGRE DM.pgRemoveTable will DROP the table specified
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param Table Name of table you want created
#' @param Host If Connection is NULL then this must be supplied. Host name
#' @param DataBase If Connection is NULL then this must be supplied. database name
#' @param User If Connection is NULL then this must be supplied. user name
#' @param Port If Connection is NULL then this must be supplied. port name
#' @param Password If Connection is NULL then this must be supplied. user password
#'
#' @examples
#' \dontrun{
#' Rappure::DM.pgRemoveTable(
#'   Table = 'static_data',
#'   Host = 'localhost',
#'   DataBase = 'Testing',
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = 'Aa...')
#'
#' # Host = 'localhost'
#' # Table = 'static_data'
#' # Connection = NULL
#' # DataBase = 'Testing'
#' # User = 'postgres'
#' # Port = 5432
#' # Password = 'Aa...'
#' }
#'
#' @export
DM.pgRemoveTable <- function(DataBase = NULL,
                             Table = NULL,
                             Host = NULL,
                             User = NULL,
                             Port = NULL,
                             Password = NULL) {

  # Connect to db
  Connection <- DBI::dbConnect(
    RPostgres::Postgres(),host = Host, dbname = DataBase,
    user = User, port = Port, password = Password)

  # Truncate table & disconnect
  DBI::dbRemoveTable(conn = Connection, name = Table)
  suppressWarnings(DBI::dbDisconnect(Connection))
}

#' @title DM.pgRemoveCreateAppend
#'
#' @description PostGRE DM.pgRemoveCreateAppend will DROP the table specified
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param data See args from related functions
#' @param Table See args from related functions
#' @param Schema See args from related functions
#' @param Host See args from related functions
#' @param DataBase See args from related functions
#' @param User See args from related functions
#' @param Port See args from related functions
#' @param Password See args from related functions
#'
#' @export
DM.pgRemoveCreateAppend <- function(data = NULL,
                                    DataBase = NULL,
                                    Table = NULL,
                                    Schema = NULL,
                                    Host = NULL,
                                    User = NULL,
                                    Port = NULL,
                                    Password = NULL) {

  # Rename
  DataBase. <- DataBase
  Schema. <- Schema
  Table. <- Table
  Append. <- TRUE
  Host. <- Host
  User. <- User
  Port. <- Port
  Password. <- Password

  # Remove
  tryCatch({
    DM.pgRemoveTable(
      Table = Table.,
      Host = Host.,
      DataBase = DataBase.,
      User = User.,
      Port = Port.,
      Password = Password.)
  }, error = function(x) print("Table does not exist"))

  # Create
  tryCatch({
    DM.pgCreateTable(
      data = data,
      DataBase = DataBase.,
      Schema = Schema.,
      Table = Table.,
      Host = Host.,
      User = User.,
      Port = Port.,
      Password = Password.)
  }, error = function(x) print("Error is creating table"))

  # Add data
  tryCatch({
    DM.pgAppend(
      data = data,
      Table = Table.,
      Append = Append.,
      Host = Host.,
      DataBase = DataBase.,
      User = User.,
      Port = Port.,
      Password = Password.)
  }, error = function(x) print("Error in saving data to new table"))
  print("success")
}

#' @title DM.pgListTables
#'
#' @description PostGRE DM.pgListTables will list all tables with an associated db
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param DataBase See args from related functions
#' @param Host See args from related functions
#' @param User See args from related functions
#' @param Port See args from related functions
#' @param Password See args from related functions
#'
#' @export
DM.pgListTables <- function(DataBase = NULL,
                            Host = NULL,
                            Port = NULL,
                            User = NULL,
                            Password = NULL) {

  # Connect to db
  Connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Host,
    dbname = DataBase,
    user = User,
    port = Port,
    password = Password)

  # Truncate table
  x <- DBI::dbListTables(conn = Connection)

  # Close Connection
  suppressWarnings(DBI::dbDisconnect(Connection))
  return(x)
}

#' @title DM.pgTableColnames
#'
#' @description PostGRE DM.pgTableColnames will list all column names from a table
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param DataBase See args from related functions
#' @param Table Name of postgres table
#' @param Host See args from related functions
#' @param User See args from related functions
#' @param Port See args from related functions
#' @param Password See args from related functions
#' @param CloseConnection See args from related functions
#'
#' @return A character vector of names. Exactly like names R base function for a data.frame
#'
#' @export
DM.pgTableColnames <- function(Host = NULL,
                               DataBase = NULL,
                               Table = NULL,
                               User = NULL,
                               Port = NULL,
                               Password = NULL) {
  if(length(Table) > 0L) {
    return(names(DataMuse::DM.pgQuery(
      Host = Host,
      DataBase = DataBase,
      SELECT = "*",
      FROM = Table,
      SamplePercent = 0.001, GroupBy = NULL,
      AggStat = NULL, User = User,
      Port = Port, Password = Password)))
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# POSTGRE Databases                                                                                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title DM.pgCreateDB
#'
#' @description PostGRE DM.pgCreateDB will create a database with a name supplied by user
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param DataBase See args from related functions
#' @param Host See args from related functions
#' @param User See args from related functions
#' @param Port See args from related functions
#' @param Password See args from related functions
#'
#' @export
DM.pgCreateDB <- function(DataBase = NULL,
                          Host = 'localhost',
                          Port = 5432,
                          User = 'postgres',
                          Password = '') {

  # Connect to db
  Connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Host,
    dbname = NULL,
    user = User,
    port = Port,
    password = Password)

  # Create database
  DBI::dbSendQuery(Connection, paste0("CREATE DATABASE ", shQuote(DataBase), ";"))
  suppressWarnings(DBI::dbDisconnect(Connection))
}

#' @title DM.pgDropDB
#'
#' @description PostGRE DM.pgDropDB Drop selected database if it exists
#'
#' @author Adrian Antico
#' @family Database
#' @param DataBase name of db
#' @param Host See args from related functions
#' @param User See args from related functions
#' @param Port See args from related functions
#' @param Password See args from related functions
#'
#' @export
DM.pgDropDB <- function(DataBase = NULL,
                        Host = 'localhost',
                        Port = 5432,
                        User = 'postgres',
                        Password = '') {

  # Connect to db
  Connection <- DBI::dbConnect(
    RPostgres::Postgres(), host = Host, dbname = NULL,
    user = User, port = Port, password = Password)

  # Create database
  x <- DBI::dbSendQuery(conn = Connection, statement = paste0("DROP DATABASE IF EXISTS ", shQuote(DataBase)))
  suppressWarnings(DBI::dbDisconnect(Connection))
}

#' @title DM.pgListDatabases
#'
#' @description PostGRE DM.pgListDatabases list of available databases
#'
#' @author Adrian Antico
#' @family Database
#'
#' @param Host See args from related functions
#' @param User See args from related functions
#' @param Port See args from related functions
#' @param Password See args from related functions
#'
#' @export
DM.pgListDatabases <- function(Host = 'localhost',
                               Port = 5432,
                               User = 'postgres',
                               Password = '') {

  # Connect to db
  Connection <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Host,
    dbname = NULL,
    user = User,
    port = Port,
    password = Password)

  # Create database
  x <- data.table::setDT(DBI::dbGetQuery(Connection, "SELECT datname FROM pg_database WHERE datistemplate = FALSE"))
  data.table::setnames(x, 'datname','Available Databases')
  suppressWarnings(DBI::dbDisconnect(Connection))
  return(x)
}

# ----

# ----

#' @noRd
Post_Query_Helper <- function(RefTable) {
  .RefTable <- RefTable
  str1 <- "SELECT * FROM "
  str2 <- '"public"'
  str3 <- '.'
  AutoQuant::PostGRE_Query(
    Query = paste(str1, str2, str3, .RefTable, sep=""),
    Connection = NULL,
    CloseConnection = TRUE,
    Host = "localhost",
    DBName = "RemixAutoML",
    User = "postgres",
    Port = 5432,
    Password = "Aa1028#@"
  )
}


#' @noRd
Post_Append_Helper <- function(data, tableName) {
  AutoQuant::PostGRE_AppendData(
    data = data,
    Table = tableName,
    Connection = NULL,
    CloseConnection = TRUE,
    Append = FALSE,
    Host = "localhost",
    DBName = "RemixAutoML",
    User = "postgres",
    Port = 5432,
    Password = "Aa1028#@")
}
