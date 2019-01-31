################################################################################
#' ddate2date
#'
#' Transforma una fecha en formato decimal a formato fecha (puntual o intervalo)
#'
#' @param ddate Un valor numerico con el significado de años
#'
#' @param type !no implementado!
#'
#' @param with !no implementado!
#'
#' @export
#'
#' @return date.table con solo una fechas: start.date
#' @examples
#' ddate2date(1985 + seq(0,1,by=.1))
#' ddate2date(1984 + seq(0,1,by=.1))
#' ddate2date(1988 + seq(0,1,by=.1))
ddate2date <-  function (ddate,type='start',with = 0.1) {
  start.date <- Epi::as.Date.cal.yr(ddate)
  return( data.table::data.table(start.date=start.date))
}


################################################################################
#' ddate2date.3d
#'
#' Transforma una fecha en formato decimal a formato fecha (puntual o intervalo)
#'
#' @param ddate Un valor numerico con el significado de años decimales
#'
#' @param type  !no implementado!
#'
#' @param with  ancho del granulo temporal que comienza en ddate, por defecto 0.1
#'
#' @export
#'
#' @seealso ddate2date
#'
#' @return date.table con 3 fechas: start.date, end.date, central.date
#' @examples
#' ddate2date.3d(1985 + seq(0,1,by=.1))
#' ddate2date.3d(1984 + seq(0,1,by=.1))
#' ddate2date.3d(1988 + seq(0,1,by=.1))
ddate2date.3d <-  function (ddate,type='start',with = 0.1) {
  start.date <- Epi::as.Date.cal.yr(ddate)
  end.date  <- Epi::as.Date.cal.yr(ddate+with) -1
  central.date  <- Epi::as.Date.cal.yr(ddate+with/2)
  return( data.table::data.table(start.date=start.date, end.date=end.date, central.date=end.date))
}

################################################################################
#' date2ymd
#'
#' Transforma un vector de fechas en una lista  con year, month, day
#'
#' @param dates vector con fechas
#'
#' @param  names   contiene 'Year','Month' and 'Day'
#'
#' @return dt.out   year, month, day
#'
#' @export
#'
date2ymd <- function (dates,names=c('Year','Month','Day')) {
  if ('data.table' %in%  class(dates)  ) { dates <- dates [[1]] }
  data.table::data.table(
   Year=data.table::year(dates),
   Moth=data.table::month(dates),
   Day=data.table::mday(dates) ) -> dt.out

  data.table::setnames(dt.out,names(dt.out),names)
  return(dt.out)
}

################################################################################
#' Episod2ymd
#'
#' Transforma dos fechas(definen un episodio) en un data.tabla con 6 columnas:
#' Start_Year	Start_Month	Start_Day
#' End_Year	End_Month	End_Day
#'
#'
#' @param  start.date vector de fechas de comienzo
#'
#' @param  end.date vector de fechas de termininacion del episodio
#'
#' @return data.table con columnas: Start_Year	Start_Month	Start_Day
#'                                    End_Year	End_Month	End_Day
#'
#' @export
#'
#' @examples
#'     dt.date <-  ddate2date.3d(1985 + seq(0,1,by=.1))
#'     Episod2ymd(dt.date$start.date,dt.date$end.date)
Episod2ymd  <- function(start.date,end.date) {
  cbind(
    date2ymd( start.date, c('Start_Year','Start_Month','Start_Day')),
    date2ymd( end.date, c('End_Year','End_Month','End_Day'))
  ) -> dt.date.int
  return(dt.date.int)
}


################################################################################
#' granule.data2ymd
#'
#'  Matriz de 3 fechas a matiz númerica de 9 elementos
#'
#'
#' @param  dt.date  tabla con columnas: start.date   end.date central.date
#'
#' @return data.table con columnas: Year	Month	Day	Start_Year	Start_Month	Start_Day	End_Year	End_Month	End_Day
#'
#' @export
#'
#' @examples
#'     dt.date <-  ddate2date.3d(1985 + seq(0,1,by=.1))
#'     granule.data2ymd(dt.date)
#'
granule.data2ymd  <- function(dt.date) {
 nn <- names(dt.date)
 cbind(
   date2ymd( dt.date$central.date, c('Year','Month','Day')),
   date2ymd( dt.date$start.date, c('Start_Year','Start_Month','Start_Day')),
   date2ymd( dt.date$end.date, c('End_Year','End_Month','End_Day'))
   ) -> dt.date.int
 return(dt.date.int)
}
################################################################################
