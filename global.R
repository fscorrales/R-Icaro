options(encoding = 'UTF-8')

source("FuncionesICARO.R", local = TRUE)

if (!require(shiny)) install.packages('shiny')
library(shiny)
if (!require(shinythemes)) install.packages('shinythemes')
library(shinythemes)
if (!require(shinyjs)) install.packages('shinyjs')
library(shinyjs)
if (!require(shinyBS)) install.packages('shinyBS')
library(shinyBS)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)
if (!require(shinyFeedback)) install.packages('shinyFeedback')
library(shinyFeedback)
if (!require(reactable)) install.packages('reactable')
library(reactable)
if (!require(RSQLite)) install.packages('RSQLite')
library(RSQLite)
if (!require(zoo)) install.packages('zoo')
library(zoo)
if (!require(DBI)) install.packages('DBI')
library(DBI)
if (!require(readxl)) install.packages('readxl')
library(readxl)
if (!require(rpivotTable)) install.packages('rpivotTable')
library(rpivotTable)
if (!require(rvest)) install.packages('rvest')
library(rvest)
if (!require(shinydashboard)) install.packages('shinydashboard')
library(shinydashboard)
if (!require(shinyWidgets)) install.packages('shinyWidgets')
library(shinyWidgets)
if (!require(openxlsx)) install.packages('openxlsx')
library(openxlsx)
if (!require(DT)) install.packages('DT')
library(DT)

# library(tippy) #https://atomiks.github.io/tippyjs/#all-options
# library(shinypanels)

list.files("Modulos/Modulos Inputs y Outputs") %>%
  purrr::map(~ source(paste0("Modulos/Modulos Inputs y Outputs/", .)))

list.files("Modulos/Modulos Tabpanel") %>%
  purrr::map(~ source(paste0("Modulos/Modulos Tabpanel/", .)))

DATA_PATH <- str_remove(getwd(), "R Icaro")
print(DATA_PATH)

CargaIcaroTrigger <- makereactivetrigger()
CargaIcaro.df <- reactive({
  CargaIcaroTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "CARGA")
  DesconectarBD(con)
  Ans <- as_tibble(Ans) %>% 
    mutate(Fecha = zoo::as.Date(Fecha))
})

RetencionesIcaroTrigger <- makereactivetrigger()
RetencionesIcaro.df <- reactive({
  RetencionesIcaroTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "RETENCIONES")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

ProgramasIcaroTrigger <- makereactivetrigger()
ProgramasIcaro.df <- reactive({
  ProgramasIcaroTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "PROGRAMAS")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

SubprogramasIcaroTrigger <- makereactivetrigger()
SubprogramasIcaro.df <- reactive({
  SubprogramasIcaroTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "SUBPROGRAMAS")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

ProyectosIcaroTrigger <- makereactivetrigger()
ProyectosIcaro.df <- reactive({
  ProyectosIcaroTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "PROYECTOS")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

ActividadesIcaroTrigger <- makereactivetrigger()
ActividadesIcaro.df <- reactive({
  ActividadesIcaroTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "ACTIVIDADES")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

ProveedoresSGFTrigger <- makereactivetrigger()
ProveedoresSGF.df <- reactive({
  ProveedoresSGFTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "PROVEEDORES")
  DesconectarBD(con)
  Ans <- as_tibble(Ans) %>% 
    filter(!is.na(CUIT))
})

ObrasIcaroTrigger <- makereactivetrigger()
ObrasIcaro.df <- reactive({
  ObrasIcaroTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "OBRAS")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

EPAMIcaroSGFTrigger <- makereactivetrigger()
EPAMIcaroSGF.df <- reactive({
  EPAMIcaroSGFTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "EPAM")
  DesconectarBD(con)
  Ans <- as_tibble(Ans) %>% 
    mutate(FechaPago = zoo::as.Date(FechaPago))
})

CertificadosIcaroSGFTrigger <- makereactivetrigger()
CertificadosIcaroSGF.df <- reactive({
  CertificadosIcaroSGFTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "CERTIFICADOS")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

FuentesSIIFTrigger <- makereactivetrigger()
FuentesSIIF.df <- reactive({
  FuentesSIIFTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "FUENTES")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

EjecPresPorFteSIIFTrigger <- makereactivetrigger()
EjecPresPorFteSIIF.df <- reactive({
  EjecPresPorFteSIIFTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "EjecPresPorFteSIIF")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)
})

ComprobantesGtosPorPartidaSIIFTrigger <- makereactivetrigger()
ComprobantesGtosPorPartidaSIIF.df <- reactive({
  ComprobantesGtosPorPartidaSIIFTrigger$depend()
  con <- ConectarBD()
  Ans <- DBI::dbReadTable(con, "ComprobantesGtosPorPartidaSIIF")
  DesconectarBD(con)
  Ans <- as_tibble(Ans)  %>% 
    mutate(Fecha = zoo::as.Date(Fecha))
})

#Backup semanal de ICARO al abrir
BackupICARO()

# PartidasSIIF.df <- read_csv(paste0(DATA_PATH, "R Output/CSV Files/Listado Partidas SIIF.csv"),
#                             col_types = "cccccc", na = c("NA")) 
# 
# FuentesSIIF.df <- read_csv(paste0(DATA_PATH, "R Output/CSV Files/Listado Fuentes SIIF.csv"),
#                            col_types = "ccc", na = c("NA")) 
# 
# CuentasCorrientesSIIF.df <- read_csv(paste0(DATA_PATH, "R Output/CSV Files/Listado Cuentas Corrientes SIIF.csv"),
#                                      col_types = "ccccl", na = c("NA")) %>% 
#   filter(Activo == TRUE) %>% 
#   select(-Activo)



###Conección Directa a la Base de Datos de ACCESS (probar en INVICO)
# connect_to_access_dbi <- function(db_file_path)  {
#   require(DBI)
#   # make sure that the file exists before attempting to connect
#   if (!file.exists(db_file_path)) {
#     stop("DB file does not exist at ", db_file_path)
#   }
#   # Assemble connection strings
#   dbq_string <- paste0("DBQ=", db_file_path)
#   driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
#   db_connect_string <- paste0(driver_string, dbq_string)
#   
#   myconn <- dbConnect(odbc::odbc(),
#                       .connection_string = db_connect_string)
#   return(myconn)
# }