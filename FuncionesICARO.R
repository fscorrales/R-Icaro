#FUNCIONES SQLITE------

##Conectar a ICARO
ConectarBD <-function(){
  
  con <- dbConnect(SQLite(), dbname= paste0(DATA_PATH, "R Output/SQLite Files/ICARO.sqlite"))
  
}

##Desconectar de ICARO
DesconectarBD <-function(ConexionActiva){

  DBI::dbDisconnect(ConexionActiva)
    
}

##Listado Tablas DB
ListadoTablasBD <- function() {
  
  con <- ConectarBD()
  Ans <- dbListTables(con)
  DesconectarBD(con)
  return(Ans)
  
}

##Existe Tabla
ExisteTablaBD <- function(TableName){
  
  con <- ConectarBD()
  Ans <- dbExistsTable(con, TableName)
  DesconectarBD(con)
  Ans
  
}

##Copia de Seguridad (1 por semana)
BackupICARO <- function(){
  
  #File name generation
  Archivo <- lubridate::today()
  Archivo <- paste(week(Archivo), year(Archivo), sep = "_")
  Archivo <- paste0(DATA_PATH, "R Output/Backup ICARO/ICARO_", Archivo, ".sqlite")
   
  if (file.exists(Archivo)) {
    #Delete file if it exists
    file.remove(Archivo)
  }

  # Copy the built in databaseDb() to an in-memory database

  newdb <- dbConnect(SQLite(), dbname = Archivo)
  db <- ConectarBD()
  RSQLite::sqliteCopyDatabase(db, newdb)
  DesconectarBD(db)
  dbDisconnect(newdb)
  
}

##Trigger SYNC
makereactivetrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

##Agregar Filas a BD
AgregarRegistros <- function(Tabla, Data, append = FALSE, 
                             overwrite = FALSE) {
  
  con <- ConectarBD()
  DBI::dbWriteTable(con, name = Tabla, value = Data,
                    append = append, overwrite = overwrite)
  DesconectarBD(con)
  
}

OrdenarTabla <- function(TablaOrdenar, 
                      strSQLOrderBy = "") {
  
  con <- ConectarBD()
  SQLquery <- paste0("CREATE TABLE COPY AS SELECT * FROM ",
                     TablaOrdenar , " ", 
                     "ORDER BY " , strSQLOrderBy)
  DBI::dbExecute(con, SQLquery)
  SQLquery <- paste0("DROP TABLE ", TablaOrdenar)
  DBI::dbExecute(con, SQLquery)
  SQLquery <- paste0("ALTER TABLE COPY RENAME TO ", TablaOrdenar)
  DBI::dbExecute(con, SQLquery)
  DesconectarBD(con)
  
}

##Leer BD
LeerBD <- function(Tabla) {
  
  con <- ConectarBD()
  Ans <- dbReadTable(con, Tabla)
  DesconectarBD(con)
  return(Ans)
  
}

##Filtrar BD
FiltrarBD <- function(SQLquery, params = NULL) {
  
  con <- ConectarBD()
  Ans <- dbGetQuery(con, SQLquery, params = params)
  DesconectarBD(con)
  return(Ans)
  
}

##Ejecuctar SQL en BD
EjecutarBD <- function(SQLquery, params = NULL) {
  
  con <- ConectarBD()
  dbExecute(con, SQLquery, params = params)
  DesconectarBD(con)
  
}

EjecutarBDWithResponse <- function(SQLquery, params = NULL) {
  
  con <- ConectarBD()
  rs <- dbSendStatement(con, SQLquery, params = params)
  x <- dbGetRowsAffected(rs)
  dbClearResult(rs)
  DesconectarBD(con)
  x <- paste0("Nro Registros afectado: ", x)
  print(x)
  
}

##IMPORTAR DATOS----

###Importar CSV (genérico)
ImportarCSV <- function(datapathCSV, Clasificacion){
  
  Ans <- FALSE
  BD <- read_csv(datapathCSV, col_names = F)
  
  # cat(ncol(BD), "\n")
  
  if (Clasificacion == "ListadoProveedores"){
    TituloReporte <- BD$X2[1]
    if (ncol(BD) == 18 & TituloReporte == "Listado de Proveedores") {
      Ans <- TRUE
      BD <- read_csv(datapathCSV, col_names = F, col_types = str_c(rep("c", 18), collapse = ""),
                     locale = locale(encoding = 'ISO-8859-1'))
      
      BD <- BD[,10:16]
      
      names(BD) <- c("Codigo","Descripcion","Domicilio", "Localidad",
                     "Telefono","CUIT","CondicionIVA")
      
      BD <- BD %>% 
        mutate(Codigo = parse_integer(Codigo)) %>% 
        filter(!is.na(CUIT))
      
      BD$CUIT <- gsub('-', '', BD$CUIT)
      
      AgregarRegistros("PROVEEDORES", BD, overwrite = TRUE)
      
      ProveedoresSGFTrigger$trigger()
      
    }
  }
  
  if (Clasificacion == "CertificadosObrasSGF"){
    TituloReporte <- BD$X2[1]
    if (TituloReporte == "Resumen de Certificaciones:") {
      Ans <- TRUE
      print("1")
      BD <- read_csv(datapathCSV, col_names = F, col_types = str_c(rep("c", 50), collapse = ""),
                     locale = locale(encoding = 'ISO-8859-1'))

      BD <- BD %>%
        transmute(NroComprobanteSIIF = "",
                  TipoComprobanteSIIF = "",
                  Origen = "Obras",
                  Periodo = str_sub(X3[1], -4),
                  Beneficiario = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X22, NA),
                  Obra = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X23, X22),
                  CodigoObra = str_sub(Obra, 0, 9),
                  NroCertificado = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X24, X23),
                  NoSe = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X25, X24),
                  NoSe2 = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X26, X25),
                  MontoCertificado = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X27, X26),
                  FondoDeReparo = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X28, X27),
                  Otros = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X29, X28),
                  ImporteBruto = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X30, X29),
                  IIBB = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X31, X30),
                  LP = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X32, X31),
                  SUSS = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X33, X32),
                  Gcias = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X34, X33),
                  INVICO = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X35, X34),
                  Retenciones = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X36, X35),
                  ImporteNeto = ifelse(X38 == "TOTALES" | X49 == "TOTALES", X37, X36)) %>%
        select(-NoSe, -NoSe2) %>%
        zoo::na.locf()
      
      BD <- BD %>%
        mutate(MontoCertificado = parse_number(MontoCertificado),
               FondoDeReparo = parse_number(FondoDeReparo),
               Otros = parse_number(Otros),
               ImporteBruto = parse_number(ImporteBruto),
               IIBB = parse_number(IIBB),
               LP = parse_number(LP),
               SUSS = parse_number(SUSS),
               Gcias = parse_number(Gcias),
               INVICO = parse_number(INVICO),
               Retenciones = parse_number(Retenciones),
               ImporteNeto = parse_number(ImporteNeto)) %>% 
        select(-Retenciones, -CodigoObra, -Otros)
      
      if (ExisteTablaBD("CERTIFICADOS")) {
        DatosDepurados <- FiltrarBD(
          paste0("SELECT * FROM CERTIFICADOS WHERE NroComprobanteSIIF <> ''")
        )
        DatosDepurados <- DatosDepurados %>% 
          bind_rows(BD)
      } else {
        DatosDepurados <- BD
      }
    
      DatosDepurados <- DatosDepurados %>% 
        arrange(desc(TipoComprobanteSIIF), desc(NroComprobanteSIIF), 
                Beneficiario, Obra, NroCertificado)
      
      AgregarRegistros("CERTIFICADOS", DatosDepurados, overwrite = TRUE)
      # OrdenarTabla("CERTIFICADOS", "Beneficiario ASC, Obra ASC, NroCertificado ASC")
      CertificadosIcaroSGFTrigger$trigger()
      
    }
  }
  
  if (Clasificacion == "RendicionEPAMSGF"){
    TituloReporte <- BD$X2[1]
    if (TituloReporte == "Resumen de Rendiciones (por Obras)") {
      Ans <- TRUE
      BD <- read_csv(datapathCSV, col_names = F, col_types = str_c(rep("c", 64), collapse = ""),
                     locale = locale(encoding = 'ISO-8859-1'))
      
      origen_vec <- stringr::str_split(BD$X7[1]," - ", simplify = T)[1]
      origen_vec <- stringr::str_split(origen_vec, " = ", simplify = T)[2]
      origen_vec <- stringr::str_remove_all(origen_vec, '\\"')
      
      names_vec <- c("NroComprobanteSIIF", "TipoComprobanteSIIF",
                     "Obra" ,"Origen", "Beneficiario", "LibramientoSGF", "Destino", "FechaPago",
                     "Movimiento", "ImporteNeto", "Gcias", "Sellos", "TL","IIBB",
                     "SUSS", "Seguro", "Salud", "Mutual", "ImporteBruto")
      
      db_mod <- purrr::map_dfc(names_vec, stats::setNames,
                               object = list(character()))
      if (origen_vec == "EPAM") {
        
        BD <- BD %>% 
          select(-one_of(str_c("X", 1:25))) %>% 
          mutate(Obra = ifelse(is.na(X54), NA, X26))
        
        BD <- BD %>%
          transmute(NroComprobanteSIIF = "",
                    TipoComprobanteSIIF = "",
                    Origen = origen_vec,
                    Obra = zoo::na.locf(Obra, na.rm = F),
                    Obra = ifelse(!is.na(Obra), Obra, X39),
                    Beneficiario = ifelse(is.na(X54), X26, X37),
                    LibramientoSGF = ifelse(is.na(X54), X27, X38),
                    Destino = ifelse(is.na(X54), X28, X39),
                    FechaPago = ifelse(is.na(X54), X29, X40),
                    Movimiento = ifelse(is.na(X54), X30, X41),
                    ImporteNeto = ifelse(is.na(X54), X31, X42),
                    Gcias = ifelse(is.na(X54), X32, X43),
                    Sellos = ifelse(is.na(X54), X33, X44),
                    TL = ifelse(is.na(X54), X34, X45),
                    IIBB = ifelse(is.na(X54), X35, X46),
                    SUSS = ifelse(is.na(X54), X36, X47),
                    Seguro = ifelse(is.na(X54), X37, X48),
                    Salud = ifelse(is.na(X54), X38, X49),
                    Mutual = ifelse(is.na(X54), X39, X50),
                    ImporteBruto = ifelse(is.na(X54), X40, X51)) %>% 
          filter(!str_detect(Obra, "175-HONOR"))
      } else{
        
        BD <- BD %>% 
          select(-one_of(str_c("X", 1:25))) %>% 
          utils::tail(-1) %>%
          mutate(Obra = ifelse(is.na(X54), NA, X26))
        
        BD <- BD %>%
          transmute(NroComprobanteSIIF = "",
                    TipoComprobanteSIIF = "",
                    Origen = origen_vec,
                    Obra =  X28, #Igualamos a destino
                    Beneficiario =X26,
                    LibramientoSGF = X27,
                    Destino = X28,
                    FechaPago = X29,
                    Movimiento = X30,
                    ImporteNeto = X31,
                    Gcias = X32,
                    Sellos = X33,
                    TL = X34,
                    IIBB = X35,
                    SUSS = X36,
                    Seguro = X37,
                    Salud = X38,
                    Mutual = X39,
                    ImporteBruto = X40) %>% 
          #filter(!str_detect(Obra, "HONORARIOS")) %>% 
          filter(!str_detect(Obra, "COMISIONES")) %>% 
          filter(!str_detect(Obra, "ENTREGA DE CHEQUER"))
        
      }

      BD <- db_mod %>%
        dplyr::full_join(BD, by = colnames(BD))
      
      BD <- BD %>%
        mutate(FechaPago = dmy(FechaPago),
               Periodo = as.character(year(FechaPago)),
               ImporteNeto = parse_number(ImporteNeto),
               #No me agrada del todo
               Obra = ifelse(Obra == "0.00",
                             Destino, Obra),
               # ---
               Gcias = parse_number(Gcias),
               Sellos = parse_number(Sellos),
               Sellos = ifelse(is.na(Sellos), 0, Sellos),
               TL = parse_number(TL),
               TL = ifelse(is.na(TL), 0, TL),
               IIBB = parse_number(IIBB),
               SUSS = parse_number(SUSS),
               Seguro = parse_number(Seguro),
               Salud = parse_number(Salud),
               Mutual = parse_number(Mutual),
               Retenciones = Gcias + Sellos + TL + IIBB + SUSS + Seguro + Salud + Mutual,
               ImporteBruto = parse_number(ImporteBruto),
               Movimiento = ifelse(Movimiento == "2", "DEBITO", Movimiento)) %>% 
        select(-Movimiento, -Retenciones, -Destino,
               -Seguro, -Salud, -Mutual)

      if (ExisteTablaBD("EPAM")) {
        DatosDepurados <- FiltrarBD(
          paste0("SELECT * FROM EPAM WHERE NroComprobanteSIIF <> ''")
        )
        DatosDepurados <- DatosDepurados %>% 
          mutate(FechaPago = zoo::as.Date(FechaPago)) %>% 
          bind_rows(BD)
      } else {
        DatosDepurados <- BD
      }
      
      DatosDepurados <- DatosDepurados %>% 
        arrange(desc(TipoComprobanteSIIF), desc(NroComprobanteSIIF), 
                Obra, Beneficiario, FechaPago)
      
      AgregarRegistros("EPAM", DatosDepurados, overwrite = TRUE)
      # OrdenarTabla("EPAM", "Obra ASC, Beneficiario ASC, FechaPago ASC")
      EPAMIcaroSGFTrigger$trigger()
      
    }
  }
  
  if (Clasificacion == "RendicionFUNCSGF"){
    TituloReporte <- BD$X2[1]
    if (TituloReporte == "Resumen de Rendiciones (Detalle)") {
      # suppressMessages(
      #   db <- vroom::vroom(path, col_names = FALSE, delim = ",",
      #                      col_types = vroom::cols(.default = "c"),
      #                      locale = vroom::locale(encoding = 'ISO-8859-1'))
      # )
      # 
      # origen_vec <- stringr::str_split(db$X7[1]," - ", simplify = T)[1]
      # origen_vec <- stringr::str_split(origen_vec, " = ", simplify = T)[2]
      # origen_vec <- stringr::str_remove_all(origen_vec, '\\"')
      # 
      # names_vec <- c("origen", "beneficiario", "cta_cte", "libramiento_sgf", "fecha",
      #                "movimiento", "importe_neto", "gcias", "sellos", "iibb",
      #                "suss", "invico", "otras", "importe_bruto", "destino",
      #                "seguro", "salud", "mutual")
      # 
      # db_mod <- purrr::map_dfc(names_vec, stats::setNames,
      #                          object = list(character()))
      # 
      # db <- db %>%
      #   dplyr::mutate(origen = origen_vec)
      # 
      # if (origen_vec == "OBRAS") {
      #   db <- db %>%
      #     dplyr::select(-X1:-X23) %>%
      #     dplyr::select(-X37:-X47) %>%
      #     dplyr::rename(beneficiario = X24, cta_cte = X25,
      #                   libramiento_sgf = X26, fecha = X27, movimiento = X28,
      #                   importe_neto = X36, gcias = X30, sellos = X31,
      #                   iibb = X32, suss = X33,
      #                   invico = X34, otras = X35, importe_bruto = X29)
      #   
      # } else {
      #   db <- db %>%
      #     dplyr::select(-X1:-X26) %>%
      #     dplyr::select(-X42:-X53) %>%
      #     dplyr::rename(beneficiario = X27, destino = X28, cta_cte = X29,
      #                   libramiento_sgf = X30, fecha = X31, movimiento = X32,
      #                   importe_neto = X33, gcias = X34, sellos = X35,
      #                   iibb = X36, suss = X37, seguro = X38,
      #                   salud = X39, mutual = X40, importe_bruto = X41)
      # }
      # 
      # db <- db_mod %>%
      #   dplyr::full_join(db, by = colnames(db))
      # 
      # db <- db %>%
      #   dplyr::select(.data$origen, dplyr::everything()) %>%
      #   dplyr::mutate(fecha = lubridate::dmy(.data$fecha),
      #                 ejercicio = as.character(lubridate::year(.data$fecha)),
      #                 mes =  stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
      #                                       lubridate::year(.data$fecha), sep = "/"),
      #                 movimiento = ifelse(.data$movimiento == "TRANSF.",
      #                                     "DEBITO", .data$movimiento),
      #                 cta_cte = ifelse(is.na(.data$cta_cte) & .data$beneficiario == "CREDITO ESPECIAL",
      #                                  "130832-07", .data$cta_cte),
      #                 cta_cte = ifelse(.data$cta_cte == "71-1-10270-5", "22110270-05",
      #                                  .data$cta_cte)) %>%
      #   dplyr::mutate_at(c("importe_neto", "gcias", "sellos", "iibb",
      #                      "suss", "salud", "mutual", "importe_bruto",
      #                      "invico", "otras", "seguro"),
      #                    ~round(readr::parse_number(.), 2))
      # 
      # if (ExisteTablaBD("EPAM")) {
      #   DatosDepurados <- FiltrarBD(
      #     paste0("SELECT * FROM EPAM WHERE NroComprobanteSIIF <> ''")
      #   )
      #   DatosDepurados <- DatosDepurados %>% 
      #     mutate(FechaPago = zoo::as.Date(FechaPago)) %>% 
      #     bind_rows(BD)
      # } else {
      #   DatosDepurados <- BD
      # }
      # 
      # DatosDepurados <- DatosDepurados %>% 
      #   arrange(desc(TipoComprobanteSIIF), desc(NroComprobanteSIIF), 
      #           Obra, Beneficiario, FechaPago)
      # 
      # AgregarRegistros("EPAM", DatosDepurados, overwrite = TRUE)
      # # OrdenarTabla("EPAM", "Obra ASC, Beneficiario ASC, FechaPago ASC")
      # EPAMIcaroSGFTrigger$trigger()
      
    }
  }
  
  return(Ans)
  
  # 
  # file_contents <- read.csv(inFile$datapath, header = input$header)
  # 
  # required_columns <- c('req1', 'req2')
  # column_names <- colnames(file_contents)
  # max_columns <- 10
  # 
  # shiny::validate(
  #   need(ncol(file_contents) <= max_columns, "Your data has too many columns"),
  #   need(all(required_columns %in% column_names), "You don't have the right data")
  # )
  # 
  # file_contents
  
}

###Importar XLS (genérico)
ImportarXLS <- function(datapathXLS, Clasificacion){
  
  Ans <- FALSE
  BD <- readxl::read_excel(datapathXLS, col_names = F, col_types = "text")
  
  # cat(ncol(BD), "\n")
  
  if (Clasificacion == "SIIFrf602"){
    TituloReporte <- BD[[4,1]]
    TituloReporte <- str_sub(TituloReporte, 1, (str_length(TituloReporte) -5))
    if (ncol(BD) == 23 & TituloReporte == "DETALLE DE LA EJECUCION PRESUESTARIA") {
      Ans <- TRUE
      BD <- readxl::read_excel(datapathXLS,
                               col_types = "text",
                               col_names = paste("X", 1:23, sep = ""))
      
      BD <- BD %>%
        transmute(Ejercicio = str_sub(X1[4], -4), Programa = str_pad(X1, 2, pad = "0"), Subprograma = str_pad(X2, 2, pad = "0"),
                  Proyecto = str_pad(X5, 2, pad = "0"), Actividad = str_pad(X6, 2, pad = "0"),
                  Partida = X7, Grupo = str_c(str_sub(X7, 1,1), "00", ""), Fuente = X8, Org = X9, CreditoOriginal = X12,
                  CreditoVigente = X13, Comprometido = X14, Ordenado = X15, Saldo = X17, Pendiente = X19) %>%
        tail(-13) %>%
        filter(Programa != is.na(Programa)) %>% 
        mutate(CreditoOriginal = parse_number(CreditoOriginal, locale = locale(decimal_mark = ".")),
               CreditoVigente = parse_number(CreditoVigente, locale = locale(decimal_mark = ".")),
               Comprometido = parse_number(Comprometido, locale = locale(decimal_mark = ".")),
               Ordenado = parse_number(Ordenado, locale = locale(decimal_mark = ".")),
               Saldo = parse_number(Saldo, locale = locale(decimal_mark = ".")),
               Pendiente = parse_number(Pendiente, locale = locale(decimal_mark = ".")))
      
      EjecutarBD(
        "DELETE FROM EjecPresPorFteSIIF WHERE Ejercicio = ?",
        params = unique(BD[["Ejercicio"]])
      )
      
      AgregarRegistros("EjecPresPorFteSIIF", BD, append = TRUE)
      
      EjecPresPorFteSIIFTrigger$trigger()
      
    }
  }
  
  if (Clasificacion == "SIIFrcg01_par"){
    TituloReporte <- BD[[4,1]]
    # TituloReporte <- str_sub(TituloReporte, 1, (str_length(TituloReporte) -5))
    if (ncol(BD) == 19 & TituloReporte == "Resumen de Comprobantes de Gastos (excepto REM)") {
      Ans <- TRUE
      BD <- readxl::read_excel(datapathXLS,
                               col_types = "text",
                               col_names = paste("X", 1:19, sep = ""))
      
      BD <- BD %>%
        mutate(Ejercicio = str_sub(X1[2], -4)) %>% 
        select(-X2, -X4, -X9)
      
      BD <- tail(BD, -14) %>%
        filter(X1 != is.na(X1)) %>% 
        transmute(Ejercicio = Ejercicio,
                  NroEntrada =  parse_integer(X1),
                  NroOrigen =  parse_integer(X3),
                  Fuente =  X5,
                  ClaseReg =  X6,
                  ClaseGto =  X7,
                  Fecha =  as.Date(parse_integer(X8), origin = "1899-12-30"),
                  Partida =  X10,
                  Grupo = str_c(str_sub(Partida, 1,1), "00", ""),
                  Monto =  parse_number(X11, locale = locale(decimal_mark = ".")),
                  CUIT =  X12,
                  Beneficiario =  X13,
                  NroExpediente = X14,
                  Cuenta =  X15,
                  Comprometido =  ifelse(X16 == "S", T, F),
                  Verificado =  ifelse(X17 == "S", T, F),
                  Aprobado =  ifelse(X18 == "S", T, F),
                  Pagado = ifelse(X19 == "S", T, F))
      
      EjecutarBD(
        "DELETE FROM ComprobantesGtosPorPartidaSIIF WHERE Ejercicio = ?",
        params = unique(BD[["Ejercicio"]])
      )
      
      
      AgregarRegistros("ComprobantesGtosPorPartidaSIIF", BD, append = TRUE)
    
      ComprobantesGtosPorPartidaSIIFTrigger$trigger()
      
    }
  }
  
  return(Ans)
  
}

##AGREGAR / EDITAR REGISTROS-----

###Agregar o Editar Estructura (es posible con SQL pero requiere más pasos)
AgregarEditarEstructuraICARO <- function(IDModificacion, TipoEstructura, 
                                         DatosFormulario, NroFilaEditar){
  
  #No me funcionó el append = TRUE por lo que decidí reescribir todo el archivo... =(
  #Conectamos a ICARO
  con <- ConectarBD()

  if (TipoEstructura == 2){
    if (is.na(DatosFormulario["DescProg"])){
      DatosFormulario["DescProg"] <- ""
    }
    Datos <- tibble(DatosFormulario["NroProg"], DatosFormulario["DescProg"])
    DatosDepurados <- ProgramasIcaro.df()
    if (IDModificacion == 1) {
      names(Datos) <- names(DatosDepurados)
      DatosDepurados <- bind_rows(DatosDepurados, Datos)
    } else {
      DatosDepurados[NroFilaEditar,] <- unname(Datos)
    }
    DatosDepurados <- DatosDepurados %>%
      arrange(Programa)
    DBI::dbWriteTable(con, "PROGRAMAS", DatosDepurados, overwrite = TRUE)
    
    ProgramasIcaroTrigger$trigger()
  }
  
  # if (TipoEstructura == 2){
  #   if (is.na(DatosFormulario["DescProg"])){
  #     DatosFormulario["DescProg"] <- ""
  #   }
  #   Datos <- tibble(Programa = DatosFormulario[["NroProg"]], 
  #                   DescProg = DatosFormulario[["DescProg"]])
  #   DBI::dbWriteTable(con, "PROGRAMAS", Datos, append = TRUE)
  #   SQLquery <- paste0("CREATE TABLE COPY AS SELECT * FROM ",
  #                      "PROGRAMAS ORDER BY Programa ASC")
  #   dbExecute(con, SQLquery)
  #   SQLquery <- paste0("DROP TABLE PROGRAMAS")
  #   dbExecute(con, SQLquery)
  #   SQLquery <- paste0("ALTER TABLE COPY RENAME TO PROGRAMAS")
  #   dbExecute(con, SQLquery)
  #   
  #   ProgramasIcaroTrigger$trigger()
  # }

  if (TipoEstructura == 5){
    if (is.na(DatosFormulario["DescSubprog"])){
      DatosFormulario["DescSubprog"] <- ""
    }
    EstructuraCompleta <- paste(DatosFormulario["NroProg"],
                                DatosFormulario["NroSubprog"], 
                                sep = "-")
    Datos <- tibble(str_sub(EstructuraCompleta, 1, 2), EstructuraCompleta, 
                    DatosFormulario["DescSubprog"])
    DatosDepurados <- SubprogramasIcaro.df()
    if (IDModificacion == 1) {
      names(Datos) <- names(DatosDepurados)
      DatosDepurados <- bind_rows(DatosDepurados, Datos)
    } else {
      DatosDepurados[NroFilaEditar,] <- unname(Datos)
    }
    DatosDepurados <- DatosDepurados %>%
      arrange(Programa, Subprograma)
    DBI::dbWriteTable(con, "SUBPROGRAMAS", DatosDepurados, overwrite = TRUE)
    
    SubprogramasIcaroTrigger$trigger()

  }

  if (TipoEstructura == 8){
    if (is.na(DatosFormulario["DescProy"])){
      DatosFormulario["DescProy"] <- ""
    }
    EstructuraCompleta <- paste(DatosFormulario["NroProg"],
                                DatosFormulario["NroSubprog"], 
                                DatosFormulario["NroProy"],
                                sep = "-")
    Datos <- tibble(str_sub(EstructuraCompleta, 1, 5), EstructuraCompleta, 
                    DatosFormulario["DescProy"])
    DatosDepurados <- ProyectosIcaro.df()
    if (IDModificacion == 1) {
      names(Datos) <- names(DatosDepurados)
      DatosDepurados <- bind_rows(DatosDepurados, Datos)
    } else {
      DatosDepurados[NroFilaEditar,] <- unname(Datos)
    }
    DatosDepurados <- DatosDepurados %>%
      arrange(Subprograma, Proyecto)
    DBI::dbWriteTable(con, "PROYECTOS", DatosDepurados, overwrite = TRUE)
    
    ProyectosIcaroTrigger$trigger()
  }

  if (TipoEstructura == 11){
    if (is.na(DatosFormulario["DescAct"])){
      DatosFormulario["DescAct"] <- ""
    }
    EstructuraCompleta <- paste(DatosFormulario["NroProg"],
                                DatosFormulario["NroSubprog"], 
                                DatosFormulario["NroProy"],
                                DatosFormulario["NroAct"],
                                sep = "-")
    Datos <- tibble(str_sub(EstructuraCompleta, 1, 8), EstructuraCompleta, 
                    DatosFormulario["DescAct"])
    DatosDepurados <- ActividadesIcaro.df()
    if (IDModificacion == 1) {
      names(Datos) <- names(DatosDepurados)
      DatosDepurados <- bind_rows(DatosDepurados, Datos)
    } else {
      DatosDepurados[NroFilaEditar,] <- unname(Datos)
    }
    DatosDepurados <- DatosDepurados %>%
      arrange(Proyecto, Actividad)
    DBI::dbWriteTable(con, "ACTIVIDADES", DatosDepurados, overwrite = TRUE)
    
    ActividadesIcaroTrigger$trigger()
  }

  DesconectarBD(con)
  
}

###Agregar o Editar Obra (como OBRAS no tiene ID es complicado usar SQL)
AgregarEditarObraICARO <- function(IDModificacion, DatosFormulario){

  if (is.na(DatosFormulario["NormaLegalObra"])){
    DatosFormulario["NormaLegalObra"] <- ""
  }
  
  if (is.na(DatosFormulario["InformacionAdicionalObra"])){
    DatosFormulario["InformacionAdicionalObra"] <- ""
  }
    
  #No me funcionó el append = TRUE por lo que decidí reescribir todo el archivo... =(
  Datos <- as_tibble(t(DatosFormulario)) #Trasponemos para poder trabajar

  #Renombramos las columnas
  Datos <- Datos %>% 
    transmute(Localidad = LocalidadObra, CUIT = CUITObra, Imputacion = ImputacionObra,
           Partida = PartidaObra, Fuente = FuenteObra, Cuenta = CuentaObra,
           NormaLegal = NormaLegalObra, Descripcion = DescripcionObra,
           InformacionAdicional = InformacionAdicionalObra)
  
  #Agregasmos las columnas no previstas en ICARO Nuevo
  Datos <- Datos %>% 
    mutate(MontoDeContrato = 0, Adicional = 0)  

  DatosDepurados <- ObrasIcaro.df()
  if (IDModificacion == 1) {
    DatosDepurados <- bind_rows(DatosDepurados, Datos)
  } else {
    DatosDepurados <- rows_update(DatosDepurados, Datos, by = "Descripcion")
  }
  DatosDepurados <- DatosDepurados %>%
    arrange(Descripcion, Imputacion)
  
  con <- ConectarBD()
  DBI::dbWriteTable(con, "OBRAS", DatosDepurados, overwrite = TRUE)
  DesconectarBD(con)
  
  ObrasIcaroTrigger$trigger()
  
}

###Agregar o Editar Registro Gasto
AgregarEditarRegistroICARO <- function(DatosOriginales, DatosFormulario){
  
  #No me funcionó el append = TRUE por lo que decidí reescribir todo el archivo... =(
  DatosFormulario <- as_tibble(t(DatosFormulario)) #Trasponemos para poder trabajar
  
  #Renombramos las columnas
  DatosFormulario <- DatosFormulario %>% 
    transmute(Fecha = FechaRegistro, Fuente = FuenteRegistro, CUIT = CUITRegistro,
              Importe = MontoBrutoRegistro, FondoDeReparo = 0, Certificado = CertificadoRegistro,
              Cuenta = CuentaRegistro, Avance = AvanceRegistro,
              Comprobante = ComprobanteRegistro,  Obra = DescObraRegistro,
              Origen = "", Tipo = TipoRegistro)
  
  #Cargamos variables básicas
  IDModificacion <- DatosOriginales$IDModificacion
  NroFilaEditar <- unlist(DatosOriginales$NroFila, use.names = FALSE) %>% 
    as.numeric()
  OrigenOriginal <- unlist(DatosOriginales$Origen, use.names = FALSE)
  ComprobanteOriginal <- unlist(DatosOriginales$NroComprobante, use.names = FALSE)
  TipoOriginal <- unlist(DatosOriginales$Tipo, use.names = FALSE)
  FondoDeReparoOriginal <- unlist(DatosOriginales$FondoDeReparo, use.names = FALSE) %>% 
    as.numeric()
  
  ComprobanteNuevo <- unlist(DatosFormulario$Comprobante, use.names = FALSE)
  
  #Agregasmos la columna Imputacion y Partida
  DatosFormulario <- FiltrarBD(
    paste0("SELECT Imputacion, Partida FROM OBRAS ",
           "WHERE Descripcion = ?"), params = DatosFormulario[["Obra"]]) %>% 
    bind_cols(DatosFormulario, .)
  
  #Modificamos las columnas Origen y Fondo de Reparo con DatosOriginales
  DatosFormulario <- DatosFormulario %>%
    mutate(Origen = OrigenOriginal,
           FondoDeReparo = FondoDeReparoOriginal)
  
  #Modificamos los importes por posible error con separador decimal 
  DatosFormulario <- DatosFormulario %>% 
    mutate(Importe = as.numeric(gsub(",", ".", Importe)),
           Avance = (as.numeric(gsub(",", ".", Avance))) / 100)
  
  #Modificamos la columna fecha as Date
  DatosFormulario <- DatosFormulario %>% 
    mutate(Fecha = zoo::as.Date(as.numeric(Fecha)))

  #Proceso de Guardado Registro / Carga
  DatosDepurados <- CargaIcaro.df()
  if (IDModificacion == 1) {
    DatosDepurados <- bind_rows(DatosDepurados, DatosFormulario)
  } else {
    DatosDepurados <- rows_delete(DatosDepurados, tibble(Comprobante = ComprobanteOriginal,
                                                         Tipo = TipoOriginal),
                                  by = c("Comprobante", "Tipo"))
    DatosDepurados <- rows_insert(DatosDepurados, DatosFormulario, by = c("Comprobante", "Tipo"))
  }
  DatosDepurados <- DatosDepurados %>%
    arrange(desc(Fecha), desc(Comprobante))
  
  con <- ConectarBD()
  DBI::dbWriteTable(con, "CARGA", DatosDepurados, overwrite = TRUE)
  DesconectarBD(con)
  CargaIcaroTrigger$trigger()
  
  #Proceso de Guardado Autocarga (Para el Caso de Agregar)
  if ((OrigenOriginal != "")) {

    ImporteBrutoNuevo <- unlist(DatosFormulario$Importe, use.names = FALSE) %>% 
      as.numeric()
    NroCertificadoNuevo <- unlist(DatosFormulario$Certificado, use.names = FALSE)
    TipoNuevo <- unlist(DatosFormulario$Tipo, use.names = FALSE)
    DescripcionObra <- unlist(DatosFormulario$Obra, use.names = FALSE)

    if (OrigenOriginal == "Obras") {
      
      if (IDModificacion == 1) {
        DatosDepurados <- FiltrarBD(
          paste0("SELECT * FROM ",
                 "(SELECT ROW_NUMBER() OVER () NroFila, * ", 
                 "FROM CERTIFICADOS WHERE NroComprobanteSIIF = '')"
          )
        )
        DatosDepurados[NroFilaEditar, 
                       c("NroComprobanteSIIF", "ImporteBruto",
                         "NroCertificado", "TipoComprobanteSIIF")] <- list(ComprobanteNuevo, ImporteBrutoNuevo, 
                                                                       NroCertificadoNuevo, TipoNuevo)
        DatosCargados <- FiltrarBD(
          paste0("SELECT * FROM ",
                 "(SELECT ROW_NUMBER() OVER () NroFila, * ", 
                 "FROM CERTIFICADOS WHERE NroComprobanteSIIF <> '')"
          )
        )
        DatosDepurados <- DatosDepurados %>% 
          union(DatosCargados) %>% 
          select(-NroFila) %>% 
          arrange(Beneficiario, Obra, Periodo)
        
        AgregarRegistros("CERTIFICADOS", DatosDepurados, overwrite = TRUE)
        
      } else if ((IDModificacion == 2 )) {
        
        EjecutarBD(
          paste0("UPDATE CERTIFICADOS set NroComprobanteSIIF = '", ComprobanteNuevo, "', ",
                 "TipoComprobanteSIIF = '", TipoNuevo, "', ",
                 "ImporteBruto = ", ImporteBrutoNuevo, " ,",
                 "NroCertificado = '", NroCertificadoNuevo, "' ",
                 "WHERE NroComprobanteSIIF = ? AND TipoComprobanteSIIF = ?"),
          params = c(ComprobanteOriginal, TipoOriginal)
        )

      }
      CertificadosIcaroSGFTrigger$trigger()
      
    } else if (OrigenOriginal == "EPAM") {
      
      if (IDModificacion == 1) {
        DatosDepurados <- FiltrarBD(
          paste0("SELECT * FROM ",
                 "(SELECT ROW_NUMBER() OVER () NroFila, * ", 
                 "FROM EPAM WHERE NroComprobanteSIIF = '')"
          )
        )
        DatosDepurados[NroFilaEditar, c("NroComprobanteSIIF", 
                                        "Obra", "TipoComprobanteSIIF")] <- list(ComprobanteNuevo, 
                                                                                DescripcionObra, TipoNuevo)
        DatosCargados <- FiltrarBD(
          paste0("SELECT * FROM ",
                 "(SELECT ROW_NUMBER() OVER () NroFila, * ", 
                 "FROM EPAM WHERE NroComprobanteSIIF <> '')"
          )
        )
        DatosDepurados <- DatosDepurados %>% 
          union(DatosCargados) %>% 
          select(-NroFila) %>% 
          arrange(Obra, Periodo, Beneficiario)

        AgregarRegistros("EPAM", DatosDepurados, overwrite = TRUE)
        
      } else if ((IDModificacion == 2 )) {
        EjecutarBD(
          paste0("UPDATE EPAM set NroComprobanteSIIF = '", ComprobanteNuevo, "', ",
                 "TipoComprobanteSIIF = '", TipoNuevo, "', ",
                 "Obra = '", DescripcionObra, "' ",
                 "WHERE NroComprobanteSIIF = ? AND TipoComprobanteSIIF = ?"),
          params = c(ComprobanteOriginal, TipoOriginal)
        )
      }
      EPAMIcaroSGFTrigger$trigger()
      
    }

  }
  
  #Proceso de Guardado Retenciones
  if (IDModificacion == 1 & OrigenOriginal != "") {
    
    IIBB <- unlist(DatosOriginales$IIBB, use.names = FALSE) %>% 
      as.numeric()
    Sellos <- unlist(DatosOriginales$Sellos, use.names = FALSE) %>% 
      as.numeric()
    TL <- unlist(DatosOriginales$TL, use.names = FALSE) %>% 
      as.numeric()
    Gcias <- unlist(DatosOriginales$Gcias, use.names = FALSE) %>% 
      as.numeric()
    SUSS <- unlist(DatosOriginales$SUSS, use.names = FALSE) %>% 
      as.numeric()
    INVICO <- unlist(DatosOriginales$INVICO, use.names = FALSE) %>% 
      as.numeric()
    
    Datos <- tribble(~Codigo, ~Importe, ~Comprobante, ~Tipo,
                     "110", IIBB, ComprobanteNuevo, TipoNuevo,
                     "111", Sellos, ComprobanteNuevo, TipoNuevo,
                     "112", TL, ComprobanteNuevo, TipoNuevo,
                     "113", Gcias, ComprobanteNuevo, TipoNuevo,
                     "114", SUSS, ComprobanteNuevo, TipoNuevo,
                     "337", INVICO, ComprobanteNuevo, TipoNuevo) %>% 
      filter(!near(Importe, 0))
    
    con <- ConectarBD()
    DBI::dbWriteTable(con, "RETENCIONES", Datos, append = TRUE)
    DesconectarBD(con)
    RetencionesIcaroTrigger$trigger()
    
  }
  
  #Proceso de edición de Retenciones
  if (IDModificacion == 2 & (ComprobanteNuevo != ComprobanteOriginal)) {
    
    EjecutarBD(
      paste0("UPDATE RETENCIONES SET ",
             "Comprobante = '" , ComprobanteNuevo, "', ",
             "Tipo = '" , DatosFormulario[["Tipo"]], "' ",
             "WHERE Comprobante = '", ComprobanteOriginal, "' ",
             "AND Tipo = '", TipoOriginal, "'")
    )
    RetencionesIcaroTrigger$trigger()
    
  }
  
}

###Agregar o Editar Autocarga
EditarAutocargaICARO <- function(DatosOriginales, DatosFormulario){
  
  #No me funcionó el append = TRUE por lo que decidí reescribir todo el archivo... =(
  Datos <- as_tibble(t(DatosFormulario)) #Trasponemos para poder trabajar
  
  #Cargamos variables básicas
  NroFilaEditar <- unlist(DatosOriginales$NroFila, use.names = FALSE) %>% 
    as.numeric()
  OrigenOriginal <- unlist(DatosOriginales$Origen, use.names = FALSE)
  
  DescCUITNuevo <- unlist(Datos$DescCUITAutocarga, use.names = FALSE)
  ImporteBrutoNuevo <- unlist(Datos$ImporteBrutoAutocarga, use.names = FALSE) %>% 
    as.numeric()
  TotalRetenidoNuevo <- unlist(Datos$TotalRetenidoAutocarga, use.names = FALSE) %>% 
    as.numeric()
  ImporteNetoNuevo <- unlist(Datos$ImporteNetoAutocarga, use.names = FALSE) %>% 
    as.numeric()
  
  #Modificamos
  if (OrigenOriginal == "Obras") {
    DatosDepurados <- FiltrarBD(
      paste0("SELECT * FROM ",
             "(SELECT ROW_NUMBER() OVER () NroFila, * ", 
             "FROM CERTIFICADOS WHERE NroComprobanteSIIF = '')"
      )
    )
    if (TotalRetenidoNuevo == 0) {
      DatosDepurados[NroFilaEditar, 
                     c("Beneficiario", "ImporteBruto", "IIBB",
                       "LP", "SUSS", "Gcias", "INVICO",
                       "ImporteNeto")] <- list(DescCUITNuevo, ImporteBrutoNuevo, 
                                               0, 0, 0, 0, 0, ImporteNetoNuevo)
    } else {
      DatosDepurados[NroFilaEditar, 
                     c("Beneficiario", "ImporteBruto",
                       "ImporteNeto")] <- list(DescCUITNuevo, ImporteBrutoNuevo, 
                                               ImporteNetoNuevo)
    }
    
    DatosDepurados <- DatosDepurados %>% 
      select(-NroFila)
    
    con <- ConectarBD()
    DBI::dbWriteTable(con, "CERTIFICADOS", DatosDepurados, overwrite = TRUE)
    DesconectarBD(con)
    CertificadosIcaroSGFTrigger$trigger()
    
  } else if (OrigenOriginal == "EPAM") {
    DatosDepurados <- FiltrarBD(
      paste0("SELECT * FROM ",
             "(SELECT ROW_NUMBER() OVER () NroFila, * ", 
             "FROM EPAM WHERE NroComprobanteSIIF = '')"
      )
    )
    if (TotalRetenidoNuevo == 0) {
      DatosDepurados[NroFilaEditar, 
                     c("Beneficiario", "ImporteBruto", "IIBB",
                       "TL", "SUSS", "Gcias", "Sellos",
                       "ImporteNeto")] <- list(DescCUITNuevo, ImporteBrutoNuevo, 
                                               0, 0, 0, 0, 0, ImporteNetoNuevo)
    } else {
      DatosDepurados[NroFilaEditar, 
                     c("Beneficiario", "ImporteBruto",
                       "ImporteNeto")] <- list(DescCUITNuevo, ImporteBrutoNuevo, 
                                               ImporteNetoNuevo)
    }
    
    DatosDepurados <- DatosDepurados %>% 
      select(-NroFila)
    
    con <- ConectarBD()
    DBI::dbWriteTable(con, "EPAM", DatosDepurados, overwrite = TRUE)
    DesconectarBD(con)
    EPAMIcaroSGFTrigger$trigger()
    
  }
  
}

##ELIMINAR REGISTROS----

###Eliminar Estructura (SQL)
EliminarEstructuraICARO <- function(Estructura, NroFilaEliminar){
  
  TipoEstructura <- str_length(Estructura)
  
  if (TipoEstructura == 2){
    
    EjecutarBD(
      "DELETE FROM PROGRAMAS WHERE Programa = ?",
      params = Estructura
    )
    ProgramasIcaroTrigger$trigger()

  }
  
  if (TipoEstructura == 5){
    
    EjecutarBD(
      "DELETE FROM SUBPROGRAMAS WHERE Subprograma = ?",
      params = Estructura
    )
    SubprogramasIcaroTrigger$trigger()

  }
  
  if (TipoEstructura == 8){
    
    EjecutarBD(
      "DELETE FROM PROYECTOS WHERE Proyecto = ?",
      params = Estructura
    )
    ProyectosIcaroTrigger$trigger()
    
  }
  
  if (TipoEstructura == 11){
    
    EjecutarBD(
      "DELETE FROM ACTIVIDADES WHERE Actividad = ?",
      params = Estructura
    )
    ActividadesIcaroTrigger$trigger()
    
  }
  
}

###Eliminar Obra (como OBRAS no tiene ID es complicado usar SQL)
EliminarObraICARO <- function(NroFilaEliminar){
  
  DatosDepurados <- ObrasIcaro.df()
  DatosDepurados <- DatosDepurados[-(NroFilaEliminar),]
  
  con <- ConectarBD()
  DBI::dbWriteTable(con, "OBRAS", DatosDepurados, overwrite = TRUE)
  DesconectarBD(con)
  
  ObrasIcaroTrigger$trigger()

}

###Eliminar Comprobante (SQL)
EliminarComprobanteICARO <- function(DatosComprobanteAEliminar){
  
  NroFilaEliminar <- DatosComprobanteAEliminar[["NroFila"]] %>% 
    as.numeric()
  ComprobanteEliminar <- DatosComprobanteAEliminar[["NroComprobante"]] 
  TipoEliminar <- DatosComprobanteAEliminar[["Tipo"]]
  OrigenEliminar <- DatosComprobanteAEliminar[["Origen"]]
  
  #Procedemos a eliminar el registro principal
  EjecutarBD(
    paste0("DELETE FROM CARGA ",
           "WHERE Comprobante = '" , ComprobanteEliminar , "' ",
           "AND Tipo = '" , TipoEliminar, "'")
  )
  CargaIcaroTrigger$trigger()
  
  #Procedemos a eliminar las retenciones asociadas
  EjecutarBD(
    paste0("DELETE FROM RETENCIONES ",
           "WHERE Comprobante = '" , ComprobanteEliminar , "' ",
           "AND Tipo = '" , TipoEliminar, "'")
  )
  RetencionesIcaroTrigger$trigger()

  #Procedemos a eliminar los registros de Autocarga Obras o EPAM
  if (OrigenEliminar != "") {
    if (OrigenEliminar == "Obras") {
      EjecutarBD(
        paste0("DELETE FROM CERTIFICADOS ",
               "WHERE NroComprobanteSIIF = '" , ComprobanteEliminar , "' ",
               "AND TipoComprobanteSIIF = '" , TipoEliminar, "'")
      )
      CertificadosIcaroSGFTrigger$trigger()
      
    } else if (OrigenEliminar == "EPAM") {
      EjecutarBD(
        paste0("DELETE FROM EPAM ",
               "WHERE NroComprobanteSIIF = '" , ComprobanteEliminar , "' ",
               "AND TipoComprobanteSIIF = '" , TipoEliminar, "'")
      )
      EPAMIcaroSGFTrigger$trigger()
      
    }

  }

  
}


##VALIDAR REGISTROS----

###Validadación registros estructura presupuestaria
ValidarNroProg <- function(TipoEstructura, DatosFormulario,
                           IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
    )
  
  NroProg <- (DatosFormulario[["NroProg"]])
  data <- FiltrarBD(
    paste0("SELECT DescProg FROM PROGRAMAS ",
           "WHERE Programa = '", NroProg, "'")
  )
  # data <- ProgramasIcaro.df() %>%
  #   filter(Programa == NroProg)
  ExisteEstructura <- nrow(data) >= 1

  shinyFeedback::feedback("NroProg", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (is.na(as.numeric(NroProg)) | str_length(trimws(NroProg)) != 2) {
      shinyFeedback::feedbackWarning("NroProg", TRUE,
                                     "Debe ingresar un Nro de 2 cifras")
      
    } else if (TipoEstructura == 2 & ExisteEstructura) {
      shinyFeedback::feedbackDanger("NroProg", TRUE,
                                    "El Nro de Prog ya existe")
      
    } else if (TipoEstructura > 2 & !ExisteEstructura) {
      shinyFeedback::feedbackDanger("NroProg", TRUE,
                                    "No existe el Programa")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["DescProg"]])
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- (data[["DescProg"]])
    
  }

  Ans
  
}

ValidarNroSubprog <- function(TipoEstructura, DatosFormulario,
                              IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  NroProg <- (DatosFormulario[["NroProg"]])
  NroSubprog <- (DatosFormulario[["NroSubprog"]])
  
  EstructuraBuscada <- paste0(NroProg, "-",
                              NroSubprog)
  data <- FiltrarBD(
    paste0("SELECT DescSubprog FROM SUBPROGRAMAS ",
           "WHERE Subprograma = '", EstructuraBuscada, "'")
  )
  # data <- SubprogramasIcaro.df() %>%
  #   filter(Subprograma == EstructuraBuscada)
  ExisteEstructura <- nrow(data) >= 1
  
  shinyFeedback::feedback("NroSubprog", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (is.na(as.numeric(NroSubprog)) | str_length(trimws(NroSubprog)) != 2) {
      shinyFeedback::feedbackWarning("NroSubprog", TRUE,
                                     "Debe ingresar un Nro de 2 cifras")
      
    } else if (TipoEstructura == 5 & ExisteEstructura) {
      shinyFeedback::feedbackDanger("NroSubprog", TRUE,
                                    "El Nro de Subprog ya existe")
      
    } else if (TipoEstructura > 5 & !ExisteEstructura) {
      shinyFeedback::feedbackDanger("NroSubprog", TRUE,
                                    "No existe el Subprograma")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["DescSubprog"]])
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- (data[["DescSubprog"]])
    
  }
  
  Ans
  
}

ValidarNroProy <- function(TipoEstructura, DatosFormulario,
                           IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  NroProg <- (DatosFormulario[["NroProg"]])
  NroSubprog <- (DatosFormulario[["NroSubprog"]])
  NroProy <- (DatosFormulario[["NroProy"]])
  
  EstructuraBuscada <- paste0(NroProg, "-",
                              NroSubprog, "-",
                              NroProy)
  data <- FiltrarBD(
    paste0("SELECT DescProy FROM PROYECTOS ",
           "WHERE Proyecto = '", EstructuraBuscada, "'")
  )
  # data <- ProyectosIcaro.df() %>%
  #   filter(Proyecto == EstructuraBuscada)
  ExisteEstructura <- nrow(data) >= 1
  
  shinyFeedback::feedback("NroProy", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (is.na(as.numeric(NroProy)) | str_length(trimws(NroProy)) != 2) {
      shinyFeedback::feedbackWarning("NroProy", TRUE,
                                     "Debe ingresar un Nro de 2 cifras")
      
    } else if (TipoEstructura == 8 & ExisteEstructura) {
      shinyFeedback::feedbackDanger("NroProy", TRUE,
                                    "El Nro de Proyecto ya existe")
      
    } else if (TipoEstructura > 8 & !ExisteEstructura) {
      shinyFeedback::feedbackDanger("NroProy", TRUE,
                                    "No existe el Proyecto")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["DescProy"]])
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- (data[["DescProy"]])
    
  }
  
  Ans
  
}

ValidarNroAct <- function(TipoEstructura, DatosFormulario,
                          IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  NroProg <- (DatosFormulario[["NroProg"]])
  NroSubprog <- (DatosFormulario[["NroSubprog"]])
  NroProy <- (DatosFormulario[["NroProy"]])
  NroAct <- (DatosFormulario[["NroAct"]])
  
  EstructuraBuscada <- paste0(NroProg, "-",
                              NroSubprog, "-",
                              NroProy, "-",
                              NroAct)
  data <- FiltrarBD(
    paste0("SELECT DescAct FROM ACTIVIDADES ",
           "WHERE Actividad = '", EstructuraBuscada, "'")
  )
  # data <- ActividadesIcaro.df() %>%
  #   filter(Actividad == EstructuraBuscada)
  ExisteEstructura <- nrow(data) >= 1
  
  shinyFeedback::feedback("NroAct", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (is.na(as.numeric(NroAct)) | str_length(trimws(NroAct)) != 2) {
      shinyFeedback::feedbackWarning("NroAct", TRUE,
                                     "Debe ingresar un Nro de 2 cifras")
      
    } else if (TipoEstructura == 11 & ExisteEstructura) {
      shinyFeedback::feedbackDanger("NroAct", TRUE,
                                    "El Nro de Actividad ya existe")
      
    } else if (TipoEstructura > 11 & !ExisteEstructura) {
      shinyFeedback::feedbackDanger("NroAct", TRUE,
                                    "No existe la Actividad")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["DescAct"]])
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- (data[["DescAct"]])
    
  }
  
  Ans
  
}

ValidarEstructura <- function(IDModificacion = 0, TipoEstructura,
                              DatosFormulario){
  

  Ans <- FALSE
  if (IDModificacion == 1) {
    
    if (TipoEstructura == 2) {
      if (ValidarNroProg(TipoEstructura, DatosFormulario)$EsValido) {
        Ans <- TRUE
      }
    }
    
    if (TipoEstructura == 5) {
      if (ValidarNroProg(TipoEstructura, DatosFormulario)$EsValido &
          ValidarNroSubprog(TipoEstructura, DatosFormulario)$EsValido) {
        Ans <- TRUE
      }
    }
    
    if (TipoEstructura == 8) {
      if (ValidarNroProg(TipoEstructura, DatosFormulario)$EsValido &
          ValidarNroSubprog(TipoEstructura, DatosFormulario)$EsValido &
          ValidarNroProy(TipoEstructura, DatosFormulario)$EsValido) {
        Ans <- TRUE
      }
    }
    
    if (TipoEstructura == 11) {
      if (ValidarNroProg(TipoEstructura, DatosFormulario)$EsValido &
          ValidarNroSubprog(TipoEstructura, DatosFormulario)$EsValido &
          ValidarNroProy(TipoEstructura, DatosFormulario)$EsValido &
          ValidarNroAct(TipoEstructura, DatosFormulario)$EsValido) {
        Ans <- TRUE
      }
    }
      
  } else {
    
    Ans <- TRUE
    
  }

  Ans
  
}

ValidarBorrarEstructura <- function(EstructuraABorrar){
  
  Estructura <- unlist(EstructuraABorrar, use.names = FALSE)
  TipoEstructura <- str_length(Estructura)
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  if (TipoEstructura == 2) {
    data <- FiltrarBD(
      paste0("SELECT Programa FROM SUBPROGRAMAS ",
             "WHERE Programa = '", Estructura, "'")
    )
    # data <- SubprogramasIcaro.df() %>%
    #   filter(Programa == Estructura)
    ExisteEstructura <- nrow(data) >= 1
    if (ExisteEstructura) {
      Ans$Descripcion <- "NO es posible borrar porque el Prog contiene Subprogramas"
    } else {
      Ans$Descripcion <- paste("¿Esta seguro que desea borrar la Estructura ",
                               Estructura, " ?")
      Ans$EsValido <- TRUE
    }
  }
  
  if (TipoEstructura == 5) {
    data <- FiltrarBD(
      paste0("SELECT Subprograma FROM PROYECTOS ",
             "WHERE Subprograma = '", Estructura, "'")
    )
    # data <- ProyectosIcaro.df() %>%
    #   filter(Subprograma == Estructura)
    ExisteEstructura <- nrow(data) >= 1
    if (ExisteEstructura) {
      Ans$Descripcion <- "NO es posible borrar porque el Subprograma contiene Proyectos"
    } else {
      Ans$Descripcion <- paste("¿Esta seguro que desea borrar la Estructura ",
                   Estructura, " ?")
      Ans$EsValido <- TRUE
    }
  }
  
  if (TipoEstructura == 8) {
    data <- FiltrarBD(
      paste0("SELECT Proyecto FROM ACTIVIDADES ",
             "WHERE Proyecto = '", Estructura, "'")
    )
    # data <- ActividadesIcaro.df() %>%
    #   filter(Proyecto == Estructura)
    ExisteEstructura <- nrow(data) >= 1
    if (ExisteEstructura) {
      Ans$Descripcion <- "NO es posible borrar porque el Proyecto contiene Actividades"
    } else {
      Ans$Descripcion <- paste("¿Esta seguro que desea borrar la Estructura ",
                   Estructura, " ?")
      Ans$EsValido <- TRUE
    }
  }
  
  if (TipoEstructura == 11) {
    data <- FiltrarBD(
      paste0("SELECT Imputacion FROM OBRAS ",
             "WHERE Imputacion = '", Estructura, "'")
    )
    # data <- ObrasIcaro.df() %>%
    #   filter(Imputacion == Estructura)
    ExisteEstructura <- nrow(data) >= 1
    if (ExisteEstructura) {
      Ans$Descripcion <- "NO es posible borrar porque la Actividades contiene Obras"
    } else {
      Ans$Descripcion <- paste("¿Esta seguro que desea borrar la Estructura ",
                   Estructura, " ?")
      Ans$EsValido <- TRUE
    }
  }
  
  Ans
  
}

###Validadación registros Obra ICARO
ValidarDescripcionObra <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  DescripcionObra <- (DatosFormulario[["DescripcionObra"]])
  data <- FiltrarBD(
    paste0("SELECT Descripcion FROM OBRAS ",
           "WHERE Descripcion = '", DescripcionObra, "'")
  )
  # data <- ObrasIcaro.df() %>%
  #   filter(Descripcion == DescripcionObra)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("DescripcionObra", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (str_length(trimws(DescripcionObra)) == 0) {
      shinyFeedback::feedbackWarning("DescripcionObra", TRUE,
                                     "Debe ingresar una Descripcion no vacía")
      
    } else if (Existe) {
      shinyFeedback::feedbackDanger("DescripcionObra", TRUE,
                                    "Debe ingresar una Descripcion única")
      
    } else{
      
      Ans$EsValido <- TRUE
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    
  }
  
  Ans
  
}

ValidarCUITObra <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  CUITObra <- unlist(DatosFormulario["CUITObra"], use.names = FALSE)
  data <- FiltrarBD(
    paste0("SELECT Descripcion FROM PROVEEDORES ",
           "WHERE CUIT = '", CUITObra, "'")
  )
  # data <- ProveedoresSGF.df() %>%
  #   filter(CUIT == CUITObra)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("CUITObra", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (is.na(as.numeric(CUITObra)) | str_length(trimws(CUITObra)) == 0) {
      shinyFeedback::feedbackWarning("CUITObra", TRUE,
                                     "Debe ingresar un valor numérico")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("CUITObra", TRUE,
                                    "No existe el CUIT en la BD")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["Descripcion"]])
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- (data[["Descripcion"]])
    
  }
  
  Ans
  
}

ValidarDescCUITObra <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  DescCUITObra <- (DatosFormulario[["DescCUITObra"]])
  data <- FiltrarBD(
    paste0("SELECT CUIT FROM PROVEEDORES ",
           "WHERE Descripcion = '", DescCUITObra, "'")
  )
  # data <- ProveedoresSGF.df() %>%
  #   filter(Descripcion == DescCUITObra)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("DescCUITObra", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (str_length(trimws(DescCUITObra)) == 0) {
      shinyFeedback::feedbackWarning("DescCUITObra", TRUE,
                                     "Debe ingresar una Descripcion / CUIT")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("DescCUITObra", TRUE,
                                    "No existe el CUIT en la BD")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["CUIT"]])
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- (data[["CUIT"]])
    
  }
  
  Ans
  
}

ValidarImputacionObra <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )

  ImputacionObra <- (DatosFormulario[["ImputacionObra"]])
  data <- FiltrarBD(
    paste0("SELECT DescAct, DescProy FROM ACTIVIDADES A INNER JOIN PROYECTOS B ",
           "ON A.Proyecto = B.Proyecto ",
           "WHERE Actividad = '", ImputacionObra, "'")
  )
  # data <- ActividadesIcaro.df() %>%
  #   filter(Actividad == ImputacionObra)
  Existe <- nrow(data) >= 1
  
  if (Existe) {
    DescAct <- (data[["DescAct"]])
    # data <- ProyectosIcaro.df() %>%
    #   filter(Proyecto == str_sub(ImputacionObra, 1, 8))
    DescProy <- (data[["DescProy"]])
    if (is.na(DescProy)) {
      DescProy <- "Sin Desc."
    }
  }

  shinyFeedback::feedback("ImputacionObra", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (str_length(trimws(ImputacionObra)) != 11) {
      shinyFeedback::feedbackWarning("ImputacionObra", TRUE,
                                     "Debe ingresar una estructura")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("ImputacionObra", TRUE,
                                    "No existe la estructura presupuestaria")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- paste(DescProy, DescAct, sep = " - ")
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- paste(DescProy, DescAct, sep = " - ")
    
  }
  
  Ans
  
}

ValidarPartidaObra <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  PartidaObra <- (DatosFormulario[["PartidaObra"]])
  data <- FiltrarBD(
    paste0("SELECT Partida FROM PARTIDAS ",
           "WHERE Partida = '", PartidaObra, "'")
  )
  # data <- PartidasSIIF.df %>%
  #   filter(Partida == PartidaObra)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("PartidaObra", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (str_length(trimws(PartidaObra)) != 3) {
      shinyFeedback::feedbackWarning("PartidaObra", TRUE,
                                     "Debe ingresar una partida de 3 cifras")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("PartidaObra", TRUE,
                                    "No existe la partida ingresada")
      
    } else{
      
      Ans$EsValido <- TRUE
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    
  }
  
  Ans
  
}

ValidarFuenteObra <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  FuenteObra <- (DatosFormulario[["FuenteObra"]])
  data <- FiltrarBD(
    paste0("SELECT Fuente FROM FUENTES ",
           "WHERE Fuente = '", FuenteObra, "'")
  )
  # data <- FuentesSIIF.df %>%
  #   filter(Fuente == FuenteObra)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("FuenteObra", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (str_length(trimws(FuenteObra)) != 2) {
      shinyFeedback::feedbackWarning("FuenteObra", TRUE,
                                     "Debe ingresar una Fuente de 2 cifras")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("FuenteObra", TRUE,
                                    "No existe la Fuente ingresada")
      
    } else{
      
      Ans$EsValido <- TRUE
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    
  }
  
  Ans
  
}

ValidarCuentaObra <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  CuentaObra <- (DatosFormulario[["CuentaObra"]])
  data <- FiltrarBD(
    paste0("SELECT Descripcion FROM CUENTASBANCARIAS ",
           "WHERE Cuenta = '", CuentaObra, "'")
  )
  # data <- CuentasCorrientesSIIF.df %>%
  #   filter(Cuenta == CuentaObra)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("CuentaObra", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (is.na(as.numeric(CuentaObra)) | str_length(trimws(CuentaObra)) == 0) {
      shinyFeedback::feedbackWarning("CuentaObra", TRUE,
                                     "Debe ingresar un valor numérico")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("CuentaObra", TRUE,
                                    "No existe el Cta Cte en la BD")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["Descripcion"]])
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- (data[["Descripcion"]])
    
  }
  
  Ans
  
}

ValidarLocalidadObra <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  LocalidadObra <- (DatosFormulario[["LocalidadObra"]])
  
  shinyFeedback::feedback("LocalidadObra", show = FALSE)
  
  if (str_length(trimws(LocalidadObra)) == 0) {
    shinyFeedback::feedbackWarning("LocalidadObra", TRUE,
                                   "Debe ingresar una Localidad")
  } else {
    Ans$EsValido <- TRUE
  }

  Ans
  
}

ValidarObra <- function(IDModificacion = 0, DatosFormulario){
  
  Ans <- FALSE
  if (IDModificacion == 1) {

    if (ValidarDescripcionObra(DatosFormulario)$EsValido &
        ValidarCUITObra(DatosFormulario)$EsValido &
        ValidarImputacionObra(DatosFormulario)$EsValido &
        ValidarPartidaObra(DatosFormulario)$EsValido &
        ValidarFuenteObra(DatosFormulario)$EsValido &
        ValidarCuentaObra(DatosFormulario)$EsValido &
        ValidarLocalidadObra(DatosFormulario)$EsValido) {
      Ans <- TRUE
    }

  } else {
    
    Ans <- TRUE
    
  }
  
  Ans
  
}

ValidarBorrarObra <- function(ObraABorrar){
  
  ObraABorrar <- unlist(ObraABorrar, use.names = FALSE)
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  data <- FiltrarBD(
    paste0("SELECT Obra FROM CARGA ",
           "WHERE Obra = '", ObraABorrar, "'")
  )
  # data <- CargaIcaro.df() %>%
  #   filter(Obra == ObraABorrar)
  Existe <- nrow(data) >= 1
  if (Existe) {
    Ans$Descripcion <- "NO es posible BORRAR porque la Obra tiene ejecución presupuestaria"
  } else {
    Ans$Descripcion <- paste("¿Esta seguro que desea borrar la Obra ",
                             ObraABorrar, " ?")
    Ans$EsValido <- TRUE
  }
  
  Ans
  
}

###Validadación Registro / Carga ICARO
ValidarNroComprobanteRegistro <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  NroComprobanteRegistro <- (DatosFormulario[["NroComprobanteRegistro"]]) %>% 
    as.numeric()
  
  shinyFeedback::feedback("NroComprobanteRegistro", show = FALSE)
  
  if (is.na(NroComprobanteRegistro)) {
    shinyFeedback::feedbackWarning("NroComprobanteRegistro", TRUE,
                                   "Debe ingresar un Nro entre 1 y 99999")
  } else if (!between(NroComprobanteRegistro, 1, 99999)){
    shinyFeedback::feedbackWarning("NroComprobanteRegistro", TRUE,
                                   "Debe ingresar un Nro entre 1 y 99999")
  } else{
    
    Ans$EsValido <- TRUE
    
  }
  
  Ans
  
}

ValidarFechaRegistro <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  FechaRegistroNum <- (DatosFormulario[["FechaRegistro"]]) %>% 
    as.numeric()
  
  FechaRegistro <- zoo::as.Date(FechaRegistroNum) 
  
  shinyFeedback::feedback("FechaRegistro", show = FALSE)
  
  if (is.na(FechaRegistroNum)) {
    shinyFeedback::feedbackWarning("FechaRegistro", TRUE,
                                   "Debe ingresar una fecha válida")
  } else  if (!is.Date(FechaRegistro) | (FechaRegistroNum < 0)) {
    shinyFeedback::feedbackWarning("FechaRegistro", TRUE,
                                   "Debe ingresar una fecha válida")
  } else{
    
    Ans$EsValido <- TRUE
    
  }
  
  Ans
  
}

ValidarComprobanteRegistro <- function(DatosFormulario, IDModificacion = 1,
                                       ComprobanteOriginal){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )

  ComprobanteRegistro <- (DatosFormulario[["ComprobanteRegistro"]])
  TipoComprobante <- (DatosFormulario[["TipoRegistro"]])
  data <- FiltrarBD(
    paste0("SELECT Comprobante FROM CARGA ",
           "WHERE Comprobante = '", ComprobanteRegistro, "' ",
           "AND Tipo = '", TipoComprobante,"'")
  )
  # data <- CargaIcaro.df() %>%
  #   filter(Comprobante == ComprobanteRegistro)
  ExisteComprobante <- nrow(data) >= 1
  
  shinyFeedback::feedback("ComprobanteRegistro", show = FALSE)
  
  if (ComprobanteRegistro == ""){
    shinyFeedback::feedbackWarning("ComprobanteRegistro", TRUE,
                                  "Verificar Nro y Fecha")
  } else if(IDModificacion == 1) {
    if (ExisteComprobante){
      shinyFeedback::feedbackDanger("ComprobanteRegistro", TRUE,
                                    "Debe ser único. Verificar Nro, Fecha y Tipo")
    }else{
      
      Ans$EsValido <- TRUE
      
    }
  } else if(IDModificacion == 2 & (ComprobanteOriginal != ComprobanteRegistro)) {
    if (ExisteComprobante){
      shinyFeedback::feedbackDanger("ComprobanteRegistro", TRUE,
                                    "Debe ser único. Verificar Nro, Fecha y Tipo")
    } else{
      
      Ans$EsValido <- TRUE
      
    }
  } else{
    
    Ans$EsValido <- TRUE
    
  }
    
  Ans
  
}

ValidarCUITRegistro <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  CUITRegistro <- DatosFormulario[["CUITRegistro"]]
  data <- FiltrarBD(
    paste0("SELECT Descripcion FROM PROVEEDORES ",
           "WHERE CUIT = '", CUITRegistro, "'")
  )
  # data <- ProveedoresSGF.df() %>%
  #   filter(CUIT == CUITRegistro)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("CUITRegistro", show = FALSE)
  
  if (IDModificacion == 1) {
    
    if (is.na(as.numeric(CUITRegistro)) | str_length(trimws(CUITRegistro)) == 0) {
      shinyFeedback::feedbackWarning("CUITRegistro", TRUE,
                                     "Debe ingresar un valor numérico")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("CUITRegistro", TRUE,
                                    "No existe el CUIT en la BD")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["Descripcion"]])
      
    }
    
  }
  
  if (IDModificacion == 2) {
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- (data[["Descripcion"]])
    
  }
  
  Ans
  
}

ValidarDescObraRegistro <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )

  DescObraRegistro <- DatosFormulario[["DescObraRegistro"]]
  data <- FiltrarBD(
    paste0("SELECT CUIT FROM OBRAS ",
           "WHERE Descripcion = '", DescObraRegistro, "'")
  )
  # data <- ObrasIcaro.df() %>%
  #   filter(Descripcion == DescObraRegistro)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("DescObraRegistro", show = FALSE)
  
  if (IDModificacion %in% c(1, 2)) {
    
    if (str_length(trimws(DescObraRegistro)) == 0) {
      shinyFeedback::feedbackWarning("DescObraRegistro", TRUE,
                                     "Debe ingresar una Descripcion no vacía")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("DescObraRegistro", TRUE,
                                    "Debe ingresar una Descripcion válida")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["CUIT"]])
      
    }
    
  }
  
  Ans
  
}

ValidarMontoBrutoRegistro <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  MontoBrutoRegistro <- (DatosFormulario[["MontoBrutoRegistro"]])
  
  MontoBrutoRegistro <- gsub(",", ".", MontoBrutoRegistro) %>% 
    as.numeric()
  
  shinyFeedback::feedback("MontoBrutoRegistro", show = FALSE)
  
    
  if (is.na((MontoBrutoRegistro))) {
    shinyFeedback::feedbackWarning("MontoBrutoRegistro", TRUE,
                                   "Debe ingresar un importe numérico válido")
  } else{
    
    Ans$EsValido <- TRUE
    
  }
    
  Ans
  
}

ValidarAvanceRegistro <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  AvanceRegistro <- (DatosFormulario[["AvanceRegistro"]])
  
  AvanceRegistro <- gsub(",", ".", AvanceRegistro) %>% 
    as.numeric()
  
  shinyFeedback::feedback("AvanceRegistro", show = FALSE)
  
  if (is.na(AvanceRegistro)) {
    shinyFeedback::feedbackWarning("AvanceRegistro", TRUE,
                                   "Debe ingresar un Nro entre 0 y 100")
  } else if (!between(AvanceRegistro, 0, 100)){
    shinyFeedback::feedbackWarning("AvanceRegistro", TRUE,
                                   "Debe ingresar un Nro entre 0 y 100")
  } else{
    
    Ans$EsValido <- TRUE
    
  }
  
  Ans
  
}

ValidarFuenteRegistro <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  FuenteRegistro <- (DatosFormulario[["FuenteRegistro"]])
  data <- FiltrarBD(
    paste0("SELECT Descripcion FROM FUENTES ",
           "WHERE Fuente = '", FuenteRegistro, "'")
  )
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("FuenteRegistro", show = FALSE)
  
  if (IDModificacion %in% c(1, 2)) {
    
    if (str_length(trimws(FuenteRegistro)) != 2) {
      shinyFeedback::feedbackWarning("FuenteRegistro", TRUE,
                                     "Debe ingresar una Fuente del listado")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("FuenteRegistro", TRUE,
                                    "No existe la Fuente ingresada")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["Descripcion"]])
      
    }
    
  }
  
  Ans
  
}

ValidarCuentaRegistro <- function(DatosFormulario, IDModificacion = 1){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  CuentaRegistro <- (DatosFormulario[["CuentaRegistro"]])
  data <- FiltrarBD(
    paste0("SELECT Descripcion FROM CUENTASBANCARIAS ",
           "WHERE Cuenta = '", CuentaRegistro, "'")
  )
  # data <- CuentasCorrientesSIIF.df %>%
  #   filter(Cuenta == CuentaRegistro)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("CuentaRegistro", show = FALSE)
  
  if (IDModificacion %in% c(1, 2)) {
    
    if (is.na(as.numeric(CuentaRegistro)) | str_length(trimws(CuentaRegistro)) == 0) {
      shinyFeedback::feedbackWarning("CuentaRegistro", TRUE,
                                     "Debe ingresar una cuenta del listado")
      
    } else if (!Existe) {
      shinyFeedback::feedbackDanger("CuentaRegistro", TRUE,
                                    "No existe el Cta Cte en la BD")
      
    } else{
      
      Ans$EsValido <- TRUE
      Ans$Descripcion <- (data[["Descripcion"]])
      
    }
    
  }
  
  Ans
  
}

ValidarRegistro <- function(IDModificacion = 0, DatosFormulario,
                            NroComprobanteOriginal){
  
  Ans <- FALSE
  
  if (ValidarNroComprobanteRegistro(DatosFormulario)$EsValido &
      ValidarFechaRegistro(DatosFormulario)$EsValido &
      ValidarComprobanteRegistro(DatosFormulario,
                                 IDModificacion,
                                 NroComprobanteOriginal)$EsValido &
      ValidarCUITRegistro(DatosFormulario)$EsValido &
      ValidarDescObraRegistro(DatosFormulario)$EsValido &
      ValidarMontoBrutoRegistro(DatosFormulario)$EsValido &
      ValidarAvanceRegistro(DatosFormulario)$EsValido &
      ValidarCuentaRegistro(DatosFormulario)$EsValido &
      ValidarFuenteRegistro(DatosFormulario)$EsValido){
    Ans <- TRUE
  }
    
  # ValidarFondoDeReparoRegistro(DatosFormulario)$EsValido

  
  Ans
  
}

ValidarBorrarRegistro<- function(DatosRegistroABorrar){
  
  # ComprobanteABorrar <- unlist(DatosRegistroABorrar$NroComprobante, use.names = FALSE)
  # TipoABorrar <- unlist(DatosRegistroABorrar$Tipo, use.names = FALSE)
  ComprobanteABorrar <- DatosRegistroABorrar[["NroComprobante"]]
  TipoABorrar <- DatosRegistroABorrar[["Tipo"]]
  
  Ans <- list(
    EsValido = TRUE,
    Descripcion =  paste("¿Esta seguro que desea borrar el ", TipoABorrar,
                         " Nro ", ComprobanteABorrar, " ?")
  )
  
  # data <- CargaIcaro.df() %>%
  #   filter(Obra == ObraABorrar)
  # Existe <- nrow(data) >= 1
  # if (Existe) {
  #   Ans$Descripcion <- "NO es posible borrar porque la Obra tiene ejecución presupuestaria"
  # } else {
  #   Ans$Descripcion <- paste("¿Esta seguro que desea borrar la Obra ",
  #                            ObraABorrar, " ?")
  #   Ans$EsValido <- TRUE
  # }
  
  Ans
  
}

###Validación Autocarga ICARO
ValidarDescCUITAutocarga <- function(DescCUIT, Origen = "Obras"){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  DescripcionCUIT <- unlist(DescCUIT, use.names = FALSE)
  data <- FiltrarBD(
    paste0("SELECT CUIT FROM PROVEEDORES ",
           "WHERE Descripcion = '", DescripcionCUIT, "'")
  )
  Existe <- nrow(data) >= 1
  
  if (Origen == "Obras"){
    shinyFeedback::feedback("EstructuraAutocargaObras", show = FALSE)
  } else {
    shinyFeedback::feedback("EstructuraAutocargaEPAM", show = FALSE)
  }
  
  if (!Existe) {
    
    if (Origen == "Obras") {
      shinyFeedback::feedbackDanger("EstructuraAutocargaObras", TRUE,
                                    "Deberá proceder a AGREGAR el nuevo CUIT en la solopa PROVEEDORES. 
                                    Luego podrá AGREGAR la OBRA y continuar con el proceso de AUTOCARGA")      
    } else {
      # shinyFeedback::feedbackWarning("EstructuraAutocargaEPAM", TRUE,
      #                               "Deberá proceder a AGREGAR el nuevo CUIT si es que no va a utilizar el de INVICO. 
      #                               Luego podrá AGREGAR la OBRA y continuar con el proceso de AUTOCARGA")      
    }


  } else { 
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- data[["CUIT"]]
    
  }
  
  Ans
  
}

ValidarDescObraAutocarga <- function(DescObra, Origen = "Obras"){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )

  
  DescripcionObra <- unlist(DescObra, use.names = FALSE)
  data <- FiltrarBD(
    paste0("SELECT Imputacion, Partida FROM OBRAS ",
           "WHERE Descripcion = '", DescripcionObra, "'")
  )
  # data <- ObrasIcaro.df() %>%
  #   filter(Descripcion == DescripcionObra)
  Existe <- nrow(data) >= 1
  
  
  if (Origen == "Obras"){
    shinyFeedback::feedback("EstructuraAutocargaObras", show = FALSE)
  } else {
    shinyFeedback::feedback("EstructuraAutocargaEPAM", show = FALSE)
  }
  
  
  if (!Existe) {
    
    if (Origen == "Obras") {
      shinyFeedback::feedbackWarning("EstructuraAutocargaObras", TRUE,
                                    "Al presionar en AGREGAR, el sistema lo redireccionará para incorporar la Obra.
                                    Luego podrá continuar con el proceso de AUTOCARGA")
    } else {
      shinyFeedback::feedbackWarning("EstructuraAutocargaEPAM", TRUE,
                                    "Al presionar en AGREGAR, el sistema lo redireccionará para incorporar la Obra.
                                    Luego podrá continuar con el proceso de AUTOCARGA")
    }
    
  } else { 
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- paste(data[["Imputacion"]], data[["Partida"]], sep = "-")
  }
  
  Ans
  
}

###Validación Editar Autocarga ICARO
ValidarDescCUITEditarAutocarga <- function(DatosAutocarga){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  DescripcionCUIT <- DatosAutocarga[["DescCUITAutocarga"]] 
  data <- FiltrarBD(
    paste0("SELECT CUIT FROM PROVEEDORES ",
           "WHERE Descripcion = '", DescripcionCUIT, "'")
  )
  # DescripcionCUIT <- unlist(DescripcionCUIT, use.names = FALSE)
  # data <- ProveedoresSGF.df() %>%
  #   filter(Descripcion == DescripcionCUIT)
  Existe <- nrow(data) >= 1
  
  shinyFeedback::feedback("DescCUITAutocarga", show = FALSE)
  
  if (!Existe) {
    
    shinyFeedback::feedbackDanger("DescCUITAutocarga", TRUE,
                                  "El contratista seleccionado NO EXISTE en ICARO")
    
    
  } else { 
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- data[["CUIT"]]
  }
  
  Ans
  
}

ValidarImporteBrutoEditarAutocarga <- function(DatosFormulario){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  ImporteBrutoAutocarga <- (DatosFormulario[["ImporteBrutoAutocarga"]])
  
  ImporteBrutoAutocarga <- gsub(",", ".", ImporteBrutoAutocarga) %>% 
    as.numeric()
  
  # ImporteBrutoAutocarga <- gsub(",", ".", ImporteBrutoAutocarga) %>% 
  #   as.numeric()
  
  shinyFeedback::feedback("ImporteBrutoAutocarga", show = FALSE)
  
  
  if (is.na((ImporteBrutoAutocarga))) {
    shinyFeedback::feedbackWarning("ImporteBrutoAutocarga", TRUE,
                                   "Debe ingresar un importe numérico válido")
  } else{
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- ImporteBrutoAutocarga
    
  }
  
  Ans
  
}

ValidarTotalRetenidoEditarAutocarga <- function(DatosFormulario){
  
  Ans <- list(
    EsValido = FALSE,
    Descripcion =  ""
  )
  
  TotalRetenidoAutocarga <- (DatosFormulario[["TotalRetenidoAutocarga"]])
  
  TotalRetenidoAutocarga <- gsub(",", ".", TotalRetenidoAutocarga) %>% 
    as.numeric()
  
  shinyFeedback::feedback("TotalRetenidoAutocarga", show = FALSE)
  
  
  if (is.na((TotalRetenidoAutocarga))) {
    shinyFeedback::feedbackWarning("TotalRetenidoAutocarga", TRUE,
                                   "Debe ingresar un importe numérico válido")
  } else{
    
    Ans$EsValido <- TRUE
    Ans$Descripcion <- TotalRetenidoAutocarga
    
  }
  
  Ans
  
}

ValidarEditarAutocarga <- function(DatosFormulario){
  
  Ans <- FALSE
    
  if (ValidarDescCUITEditarAutocarga(DatosFormulario)$EsValido &
      ValidarImporteBrutoEditarAutocarga(DatosFormulario)$EsValido &
      ValidarTotalRetenidoEditarAutocarga(DatosFormulario)$EsValido) {
    Ans <- TRUE
    }
  
  Ans
  
}

##VENTANAS EMERGENTES----

###Evita que el bsModal se cierre
bsModalNoClose <-function(...) {
  b = bsModal(...)
  b[[2]]$`data-backdrop` = "static"
  b[[2]]$`data-keyboard` = "false"
  return(b)
}

###Ventana Error GENÉRICA
ShowModalError <- function(MensajeAdvertencia){
  
  showModal(modalDialog(title = "ADVERTENCIA",
                        MensajeAdvertencia,
                        footer = tagList(modalButton("Aceptar")), 
                        easyClose = TRUE))
  
}

###Ventana Error de Validacion Obra
ShowModalErrorValidacionObra<- function(MensajeAdvertencia){
  
  showModal(modalDialog(title = "ADVERTENCIA",
                        MensajeAdvertencia,
                        footer = tagList(actionButton("AceptarErrorValidacionObra", 
                                                      "Aceptar")), 
                        easyClose = FALSE))
  
}

##HABILITAR / DESHABILITAR INPUTS----

###Habilitar / Deshabilitar botones Registro Carga
HabilitarBotonesCargaRegistro <- function(Habilitar = TRUE){
  
  shinyjs::useShinyjs(rmd = TRUE)
  
  if(Habilitar == FALSE){
    shinyjs::disable("EditarComprobante")
    shinyjs::disable("BorrarComprobante")
  } else{
    shinyjs::enable("EditarComprobante")
    shinyjs::enable("BorrarComprobante")
  }
  
  
}

###Habilitar / Deshabilitar botones Autocarga
HabilitarBotonesAutocargaObras <- function(Habilitar = TRUE){
  
  shinyjs::useShinyjs(rmd = TRUE)
  
  if(Habilitar == FALSE){
    shinyjs::disable("AgregarAutocargaObras")
    shinyjs::disable("EditarAutocargaObras")
    shinyjs::disable("BorrarAutocargaObras")
  } else{
    shinyjs::enable("AgregarAutocargaObras")
    shinyjs::enable("EditarAutocargaObras")
    shinyjs::enable("BorrarAutocargaObras")
  }
  
  
}
HabilitarBotonesAutocargaEPAM <- function(Habilitar = TRUE){
  
  shinyjs::useShinyjs(rmd = TRUE)
  
  if(Habilitar == FALSE){
    shinyjs::disable("AgregarAutocargaEPAM")
    shinyjs::disable("EditarAutocargaEPAM")
    shinyjs::disable("BorrarAutocargaEPAM")
  } else{
    shinyjs::enable("AgregarAutocargaEPAM")
    shinyjs::enable("EditarAutocargaEPAM")
    shinyjs::enable("BorrarAutocargaEPAM")
  }
  
  
}

###Habilitar / Deshabilitar botones Estructura
HabilitarBotonesEstructura <- function(Habilitar = TRUE){
  
  shinyjs::useShinyjs(rmd = TRUE)
  
  if(Habilitar == FALSE){
    shinyjs::disable("AgregarEstructura")
    shinyjs::disable("EditarEstructura")
    shinyjs::disable("BorrarEstructura")
  } else{
    shinyjs::enable("AgregarEstructura")
    shinyjs::enable("EditarEstructura")
    shinyjs::enable("BorrarEstructura")
  }

  
}

###Habilitar / Deshabilitar botones Obra
HabilitarBotonesObra <- function(Habilitar = TRUE){
  
  shinyjs::useShinyjs(rmd = TRUE)
  
  if(Habilitar == FALSE){
    shinyjs::disable("AgregarObra")
    shinyjs::disable("EditarObra")
    shinyjs::disable("BorrarObra")
  } else{
    shinyjs::enable("AgregarObra")
    shinyjs::enable("EditarObra")
    shinyjs::enable("BorrarObra")
  }
  
  
}


##CLEAN INPUTS----

###Blanquear Editar Autocarga
CleanBsModalAutocarga <- function(BsModalSession){
  
  updateTextInput(BsModalSession, "CUITAutocarga", value = "")
  updateSelectizeInput(BsModalSession, "DescCUITAutocarga", selected = NULL, choices = NULL)
  updateSelectizeInput(BsModalSession, "DescObraAutocarga", selected = NULL, choices = NULL)
  updateNumericInput(BsModalSession, "ImporteBrutoAutocarga", value = NULL)
  updateSelectizeInput(BsModalSession, "TotalRetenidoAutocarga", selected = NULL, choices = NULL)
  updateTextInput(BsModalSession, "ImporteNetoAutocarga", value = "")
  
}

###Blanquear Autocarga Obras
CleanAutocargaObras <- function(AutocargaObrasSession){
  
  updateTextInput(AutocargaObrasSession, "EstructuraAutocargaObras", value = "")
  updateTextInput(AutocargaObrasSession, "ImporteBrutoAutocargaObras", value = "")
  updateTextInput(AutocargaObrasSession, "ImporteNetoAutocargaObras", value = "")
  
}
CleanAutocargaEPAM <- function(AutocargaEPAMSession){
  
  updateTextInput(AutocargaEPAMSession, "EstructuraAutocargaEPAM", value = "")
  updateTextInput(AutocargaEPAMSession, "ImporteBrutoAutocargaEPAM", value = "")
  updateTextInput(AutocargaEPAMSession, "ImporteNetoAutocargaEPAM", value = "")
  
}

###Blanquear BsModal Registro
CleanBsModalRegistro <- function(BsModalSession){

  
  updateSelectizeInput(BsModalSession, inputId = ('TipoRegistro'),
                       selected = "CYO")  
  updateSelectizeInput(BsModalSession, inputId = ('DescObraRegistro'), choices = "",
                       selected = "")
  
  updateSelectizeInput(BsModalSession, inputId = ('CUITRegistro'), choices = "",
                       selected = "")
  updateSelectizeInput(BsModalSession, inputId = ('DescCUITRegistro'), choices = "",
                       selected = "")

  updateSelectizeInput(BsModalSession, inputId = ('CuentaRegistro'), choices = "",
                       selected = "")
  updateSelectizeInput(BsModalSession, inputId = ('FuenteRegistro'), choices = "",
                       selected = "", server = TRUE)
  updateTextInput(BsModalSession, "NroComprobanteRegistro", value = "")
  updateDateInput(BsModalSession, "FechaRegistro", value = "")
  updateNumericInput(BsModalSession, "MontoBrutoRegistro", value = 0)
  updateTextInput(BsModalSession, "CertificadoRegistro", value = "")
  updateNumericInputIcon(BsModalSession, "AvanceRegistro", value = 0)
  updateTextInput(BsModalSession, "DescCuentaRegistro", value = "")
  updateTextInput(BsModalSession, "DescFuenteRegistro", value = "")
  
}

##UPDATE INPUTS----

###Update Descripcion Estructura
UpdateDescripcionEstructura <- function(Estructura = "", 
                                        DescripcionEstructura = "", 
                                        sessionID){
  
  TipoEstructura <- str_length(Estructura)
  
  EtiquetaNroEstructura <- case_when(
    TipoEstructura == 2 ~ "Nro Programa seleccionado",
    TipoEstructura == 5 ~ "Nro Subprograma seleccionado",
    TipoEstructura == 8 ~ "Nro Proyecto seleccionado",
    TipoEstructura == 11 ~ "Nro Actividad seleccionado",
    TRUE ~ "CLEAN")
  
  if (EtiquetaNroEstructura != "CLEAN") {
    updateTextInput(session = sessionID, inputId = "NroEstructura", 
                    label = EtiquetaNroEstructura,
                    value = str_sub(Estructura, -2))
  } else {
    updateTextInput(session = sessionID, inputId = "NroEstructura", 
                    value = "")    
  }

  
  EtiquetaDescripcionEstructura <- case_when(
    TipoEstructura == 2 ~ "Descripcion Programa seleccionado",
    TipoEstructura == 5 ~ "Descripcion Subprograma seleccionado",
    TipoEstructura == 8 ~ "Descripcion Proyecto seleccionado",
    TipoEstructura == 11 ~ "Descripcion Actividad seleccionado" ,
    TRUE ~ "CLEAN")
  
  if (EtiquetaDescripcionEstructura != "CLEAN") {
    updateTextAreaInput(session = sessionID, inputId = "DescripcionEstructura", 
                        label = EtiquetaDescripcionEstructura,
                        value = DescripcionEstructura)
  } else {
    updateTextAreaInput(session = sessionID, inputId = "DescripcionEstructura", 
                        value = "") 
  }
  
}

###Update Descripcion Obra
UpdateDescripcionObra <- function(Estructura, DescripcionEstructura, sessionID){
  

  updateTextInput(session = sessionID, inputId = "EstructuraObra", 
                  value = Estructura)

  updateTextAreaInput(session = sessionID, inputId = "DescripcionObra", 
                      value = DescripcionEstructura)
  
}


##DT OUTPUT OPTIONS-----

DTExtensions <- function(){
  
  Ans <- c("Scroller", "Buttons")
  
}

DTFilter <- function(){
  
  Ans <- list(position = 'top', clear = FALSE, plain=T)
  
}

DTOptions <- function(){
  
  #order = list(1, "asc")
  #order = list(list(1, "asc"), list(2, "asc")
  
  Ans <- list(pageLength = 100, deferRender = T,
              scroller = T, stateSave=T, filter = "top",
              searching = T, scrollY = '400px', scrollX = T,
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
              dom = 'Bfrtip')
  
}

##FUNCIONES VARIAS------

###Función para llenar cadena de texto y ocupar menos espacio en app
fix_string_output <- function(type){
  
  proveedores_sidebar <- "En caso que desee actualizar la Base de Datos de Contratistas / Proveedores de ICARO,
  es necesario descargar el listado de Provedores del Sistema de Gestión Financiara. Para
  ello, ingrese al SGF y seleccione el menú Archivo / Proveedores / Listado de Proveedores. Luego, 
  presione el botón Exportar y, en la ventana emergente, mantenga la opción 'Archivo...' antes
  de presionar aceptar. Por último, elija el destino del archivo a descargar y preste atención
  a que el tipo sea .CSV" 
  
  obras_sidebar <- "Seleccionar la fila de la tabla que desea modificar antes de 
  proceder a realizar cualquier cambio"
  
  estructura_sidebar <- "Seleccionar la fila de la tabla que desea modificar antes 
  de proceder a realizar cualquier cambio. 
  Esto permite que el programa se posicione en la estructura correcta."
  
  Ans <- switch (type,
                 ProveedoresSidebar = proveedores_sidebar,
                 ObrasSidebar = obras_sidebar,
                 EstructuraSidebar = estructura_sidebar
  )
  
}