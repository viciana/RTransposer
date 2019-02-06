#' transposer
#'
#'  Main function to tranfer a set of data.table(s) present in
#'  .GlobalEnv on an IDS structure
#' 
#' @param file.definition xlsx-files with the instructions to perform
#'     the transfer of the data contained in the data.table objects
#'     present in .GlobalEnv to the tables required by the IDS format
#'
#'
#' @param sheet Name of the pages within the book that contain the
#'     instructions. Only 'Entity' and/or 'Relationship' is allowed.
#'
#
#'
#' @param db.filename Not yet implemented. The name will be the SQLite
#'     database where the results will be stored in IDS format.
#'
#' @param Name.DataBase Name of the database that identifies the data
#'     within the IDS network.
#'
#'
#' @importFrom data.table .N := setkey
#'
#' @importFrom  readxl read_xlsx 
#'
#' @export 
#'
#' @return outcome A list with the tables INDIVIDUAL, CONTEXT,
#'     INDIV_INDIV, INDIV_CONTEXT, CONTEXT_CONTEXT with the results of
#'     the data transfer,
#'
#'
#'
transposer <- function(file.definition  = 'EntityRelationDefinition.xls',
                       sheet = c('Entity','Relationship'),
                       Name.DataBase = 'DB release_0.1',
                       db.filename   = 'NA') {

  ### Crea contenedores tablas receptores
  INDIVIDUAL  <- ids.skeleton$INDIVIDUAL
  CONTEXT     <- ids.skeleton$CONTEXT
  INDIV_INDIV  <- ids.skeleton$INDIV_INDIV
  INDIV_CONTEXT   <- ids.skeleton$INDIV_CONTEXT
  CONTEXT_CONTEXT <- ids.skeleton$CONTEXT_CONTEXT

  for (j in sheet) {
    data.table::as.data.table(readxl::read_xlsx(file.definition,
                                                sheet = j)) -> EAVs
    if (j == 'Entity' ) {
      #-------------------------------------
      ## Carga  de datos ..
      #-------------------------------------

      ### Bucle de carga  ## No esta implementado la expresion RowFilter
      for (i in 1:length(EAVs[,1][[1]]) ) {
        EAV <- EAVs[i]
        print (paste0(j,': ',i,'> ',EAV$TableName))
        table.source  <- get(EAV$TableName)
        table.reciver <- get(EAV$EntityType,ids.skeleton)

        ### Solo  se definen las variables columnas que se van  a rellenar,
        ### No es necesario definir las columnas que no recibiran datos y se rellenan a NA por defecto.
        if (EAV$EntityType == 'INDIVIDUAL') {
          table.source[eval(parse(text=EAV$RowFilter)),
                       {c(list( Id_D       = Name.DataBase ,
                                 Id_I       = eval(parse(text=EAV$EntityID)),
                                 Source     = EAV$Source,
                                 Type       = EAV$Type,
                                 XXX       = eval(parse(text=EAV$ValueExpression)),
                                 Date_type  = EAV$DateType,
                                 Estimation = EAV$DateEstimationType,
                                 Missing  =   EAV$DateMissingType),
                           eval(parse(text=EAV$DateExpression)))}]    -> pp ;
          ## Esta fallando ... rellena los dos campos: Value y Value_Id_C
          if  ( (!is.na(EAV$Output)) &  EAV$Output == 'Value_Id_C') {
            setnames(pp,'XXX','Value_Id_C')
          } else {
            setnames(pp,'XXX','Value')
          }
          pp <- rbind(pp,table.reciver,fill= TRUE)
          INDIVIDUAL <- rbind(INDIVIDUAL,pp)
        } else if (EAV$EntityType == 'CONTEXT') {
          table.source[eval(parse(text=EAV$RowFilter)),
                       {c(list( Id_D       = Name.DataBase ,
                                 Id_C       = eval(parse(text=EAV$EntityID)),
                                 Source     = EAV$Source,
                                 Type       = EAV$Type,
                                 Value       = eval(parse(text=EAV$ValueExpression)),
                                 Date_type  = EAV$DateType,
                                 Estimation = EAV$DateEstimationType,
                                 Missing  =   EAV$DateMissingType),
                           eval(parse(text=EAV$DateExpression)))}]    -> pp ;
          pp <- rbind(pp,table.reciver,fill= TRUE)
          CONTEXT <- rbind(CONTEXT,pp)
        }
      }

    } else { # is Relationship

      for (i in 1:length(EAVs[,1][[1]]) ) {
        EAV <- EAVs[i]
        print (paste0(j,': ',i,'> ',EAV$TableName))
        table.source  <- get(EAV$TableName)
        table.reciver <- get(EAV$RelationshipType,ids.skeleton)
        if (EAV$RelationshipType == 'INDIV_INDIV') {
          # Relation
          table.source[eval(parse(text=EAV$RowFilter)),
                       {c(list( Id_D       = Name.DataBase ,
                                 Id_I_1       = eval(parse(text=EAV$FromEntityID)),
                                 Id_I_2       = eval(parse(text=EAV$ToEntityID)),
                                 Source     = EAV$Source,
                                 Relation    = eval(parse(text=EAV$ValueExpression)),
                                 Date_type  = EAV$DateType,
                                 Estimation = EAV$DateEstimationType,
                                 Missing  =   EAV$DateMissingType),
                           eval(parse(text=EAV$DateExpression)))}]    -> pp ;
          pp <- rbind(pp,table.reciver,fill= TRUE)
          INDIV_INDIV <- rbind(INDIV_INDIV,pp)
        } else if (EAV$RelationshipType == 'INDIV_CONTEXT') {
          table.source[eval(parse(text=EAV$RowFilter)),
                       {c(list( Id_D       = Name.DataBase ,
                                 Id_I       = eval(parse(text=EAV$FromEntityID)),
                                 Id_C       = eval(parse(text=EAV$ToEntityID)),
                                 Source     = EAV$Source,
                                 Relation    = eval(parse(text=EAV$ValueExpression)),
                                 Date_type  = EAV$DateType,
                                 Estimation = EAV$DateEstimationType,
                                 Missing  =   EAV$DateMissingType),
                           eval(parse(text=EAV$DateExpression)))}]    -> pp ;
          pp <- rbind(pp,table.reciver,fill= TRUE)
          INDIV_CONTEXT <- rbind(INDIV_CONTEXT,pp)
        } else if (EAV$RelationshipType == 'CONTEXT_CONTEXT') {
          # ID Id_D Id_C_1 Id_C_2 Source Relation DateType Estimation .... Missing
          table.source[eval(parse(text=EAV$RowFilter)),
                       {c(list( Id_D       = Name.DataBase ,
                                 Id_C_1       = eval(parse(text=EAV$FromEntityID)),
                                 Id_C_2       = eval(parse(text=EAV$ToEntityID)),
                                 Source     = EAV$Source,
                                 Relation    = eval(parse(text=EAV$ValueExpression)),
                                 Date_type  = EAV$DateType,
                                 Estimation = EAV$DateEstimationType,
                                 Missing  =   EAV$DateMissingType),
                           eval(parse(text=EAV$DateExpression)))}]    -> pp ;
          pp <- rbind(pp,table.reciver,fill= TRUE)
          CONTEXT_CONTEXT <- rbind(CONTEXT_CONTEXT,pp)
        }
      }
    }
  }

  ## REORDENA
  setkey(INDIVIDUAL,Id_I)
  setkey(CONTEXT,Id_C)
  setkey(INDIV_INDIV,Id_I_1,Id_I_2)
  setkey(INDIV_CONTEXT,Id_I,Id_C)
  setkey(CONTEXT_CONTEXT,Id_C_1,Id_C_2)

  ### fill ID: primary key
  INDIVIDUAL[,':='(ID=1:.N)]
  CONTEXT[,':='(ID=1:.N)]
  INDIV_INDIV[,':='(ID=1:.N)]
  INDIV_CONTEXT[,':='(ID=1:.N)]
  CONTEXT_CONTEXT[,':='(ID=1:.N)]


  outcome <- list (INDIVIDUAL=INDIVIDUAL,
                   CONTEXT=CONTEXT,
                   INDIV_INDIV=INDIV_INDIV,
                   INDIV_CONTEXT=INDIV_CONTEXT,
                   CONTEXT_CONTEXT=CONTEXT_CONTEXT
                   )

  return(outcome)
}

