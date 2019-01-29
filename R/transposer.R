#' transposer
#'
#'    Funcion principal para importar un conjunto de tablas presentes
#'    en  .GlobalEnv sobre una estructura IDS
#'
#' @param file.definition xlxs con las especificaciones de la expresión de importanción
#'        relativas a los objetos data.table que deben de estar previamente
#'        presentes en .GlobalEnv
#'
#' @param sheet nombre de la hoja dentro del libro
#'
#' @param db.filename (aun no implemetado, DB SQLite donde se almacenara
#'        la estructura IDS)
#' @param Name.DataBase de la BD en IDS
#'
#' @importFrom data.table .N :=
#'
#' @export
#'
#' @return  outcome  una lista con las tablas INDIVIDUAL, CONTEXT, ...
#'          con los resultados de la carga  (en versiones porterires se alamacenara
#'          sobre una base de datos SQLite)
#'
transposer <- function(file.definition  = 'EntityRelationDefinition.xls',
                       sheet = c('Entity','Relationship'),
                       Name.DataBase = 'DB release_0.1',
                       db.filename   = 'NA') {


  for (j in sheet) {
    data.table::as.data.table(readxl::read_xlsx(file.definition,
                                                sheet = j)) -> EAVs
    if (j == 'Entity' ) {
      #-------------------------------------
      ## Carga  de datos ..
      #-------------------------------------
      ### Crea contenedores tablas receptores
      INDIVIDUAL <- ids.skeleton$INDIVIDUAL
      CONTEXT     <- ids.skeleton$CONTEXT

      ### Bucle de carga
      for (i in 1:length(EAVs[,1][[1]]) ) { print (i)
        EAV <- EAVs[i]
        table.source  <- get(EAV$tableName)
        table.reciver <- get(EAV$entityType,ids.skeleton)

        ### En fase de depuracion ..
        # print(EAV$tableName)
        # print(EAV$ValueExpression)
        # print(EAV$DateExpression)
        ### Solo  se definen las variables columnas que se van  a rellenar,
        ### No es necesario definir las columnas que no recibiran datos y se rellenan a NA por defecto.
        if (EAV$entityType == 'INDIVIDUAL') {
          table.source[,{c(list( Id_D       = Name.DataBase ,
                                 Id_I       = eval(parse(text=EAV$entityID)),
                                 Source     = EAV$Source,
                                 Type       = EAV$Type,
                                 XXX       = eval(parse(text=EAV$ValueExpression)),
                                 Date_type  = EAV$Date_type,
                                 Estimation = EAV$DateEstimationType,
                                 Missing  =   EAV$DateMissingType),
                           eval(parse(text=EAV$DateExpression)))}]    -> pp ;
          if  ( (!is.na(EAV$output)) &  EAV$output == 'Value_Id_C') {
            setnames(pp,'XXX','Value_Id_C')
          } else {
            setnames(pp,'XXX','Value')
            }
          pp <- rbind(pp,table.reciver,fill= TRUE)
          INDIVIDUAL <- rbind(INDIVIDUAL,pp)

        } else if (EAV$entityType == 'CONTEXT') {
          table.source[,{c(list( Id_D       = Name.DataBase ,
                                 Id_C       = eval(parse(text=EAV$entityID)),
                                 Source     = EAV$Source,
                                 Type       = EAV$Type,
                                 Value       = eval(parse(text=EAV$ValueExpression)),
                                 Date_type  = EAV$Date_type,
                                 Estimation = EAV$DateEstimationType,
                                 Missing  =   EAV$DateMissingType),
                           eval(parse(text=EAV$DateExpression)))}]    -> pp ;
          pp <- rbind(pp,table.reciver,fill= TRUE)
          CONTEXT <- rbind(CONTEXT,pp)
        }
      }

    } else { # is Relationship

    }
  }

  # Value      = ifelse(EAV$output=='Value',
  #                     eval(parse(text=EAV$ValueExpression)),NA),
  # Value_Id_C = ifelse(EAV$output=='Value_id_C',
  #                     eval(parse(text=EAV$ValueExpression)),NA),
  #

  ## REORDENA
  setkey(INDIVIDUAL,Id_I)
  setkey(CONTEXT,Id_C)

  ### fill ID: primary key
  INDIVIDUAL[,':='(ID=1:.N)]
  CONTEXT[,':='(ID=1:.N)]

  outcome <- list (INDIVIDUAL=INDIVIDUAL,CONTEXT=CONTEXT)

  return(outcome)
}

