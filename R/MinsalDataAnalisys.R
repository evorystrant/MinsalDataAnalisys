getData <- function(tablename = NULL, ...)
{
  # Configuracion
  dbname <- "indicadores"
  host <- "localhost"
  dbport <- 5432
  user <- "admin"
  pass <- "tesis"
  schema <- "public"

  if(is.null(tablename))
  {
    print("Falta el parametro tablename, quiza olvido su nombre?")
    print("usando tablename por defecto (tmp_ind_135)")
    tablename <- "tmp_ind_135"
  }

  # Coneccion
  drv <- DBI::dbDriver("PostgreSQL")
  con <- DBI::dbConnect(drv, dbname=dbname, host=host, port=dbport, user=user,password=pass)

  # Get Data
  dataframe <- DBI::dbReadTable(conn = con, name = c(schema, tablename))

  # Dicconect
  dbDisconnect(con)

  dataframe
}

linearRegression <- function(tablename, ...) UseMethod("linearRegression")

linearRegression.default <- function(tablename = NULL, formula = NULL, z = NULL, ...)
{
  if(is.null(tablename))
  {
    print("Falta el parametro tablename, quiza olvido su nombre?")
    print("usando tablename por defecto (tmp_ind_135)")
    tablename <- "tmp_ind_135"
  }

  if(is.null(formula))
  {
    print("Falta el parametro formula, quiza olvido su nombre?")
    print("usando formula por defecto (anio ~ id_departamento)")
    formula <- "anio ~ id_departamento"
  }
  
  if(is.null(z))
  {
    print("Falta el parametro Z, quiza olvido su nombre?")
    print("usando formula por defecto (0)")
    formula <- 0
  }


  formula <- as.formula(formula)
  # es posible enviar en formula mas de una variable independiente <col1 ~ col2 + col3mod1>
  data <- getData(tablename)
  model <- lm(formula = formula, data = data)
  varname <- all.vars(formula[[3]])
  newdata = data.frame(varname=z)
  predict(model, newdata, interval = "predict")
}


