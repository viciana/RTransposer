################################################################################
#' ddate2date
#'
#' Transform a date in decimal year format to a specific day or a range of days
#'
#' @param ddate A numeric value in years with decimals
#'
#' @param type Not yet implemented
#'
#' @export
#'
#' @return date.table con solo una fechas: start.date
#' @examples
#' ddate2date(1985 + seq(0,1,by=.1))
#' ddate2date(1984 + seq(0,1,by=.1))
ddate2date <-  function (ddate,type='start') {
  start.date <- Epi::as.Date.cal.yr(ddate)
  return( data.table::data.table(start.date=start.date))
}


################################################################################
#' ddate2date.3d
#'
#'Transform a date in decimal year format to  range of days
#' 
#'
#' @param ddate A numeric value in years with decimals
#'
#' @param type  !no implementado!
#'
#' @param with Width of the temporal granule that begins in ddate. By default 0.1 year
#'
#' @export
#'
#' @seealso ddate2date
#'
#' @return date.table con 3 fechas: start.date, end.date, central.date
#' @examples
#' ddate2date.3d(1985 + seq(0,1,by=.1))
#' ddate2date.3d(1984 + seq(0,1,by=.1))
ddate2date.3d <-  function (ddate,type='start',with = 0.1) {
  start.date <- Epi::as.Date.cal.yr(ddate)
  end.date  <- Epi::as.Date.cal.yr(ddate+with) -1
  central.date  <- Epi::as.Date.cal.yr(ddate+with/2)
  return( data.table::data.table(start.date=start.date, end.date=end.date, central.date=end.date))
}

################################################################################
#' date2ymd
#'
#' Transform a date vector into a list with  year, month, day
#'
#' @param dates vector with dates
#'
#' @param  names   Contains 'Year','Month' and 'Day'
#'
#' @return dt.out   data.table with column Year, Month and Day
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
#' Transform two dates (defining an episode) into a data.table with 6 columns:
#' 
#' Start_Year	Start_Month	Start_Day
#' End_Year	End_Month	End_Day
#'
#'
#' @param  start.date Vector with start dates of episod
#'
#' @param  end.date Vector with ending dates of the episode
#'
#' @return data.table con columnas: Start_year	Start_month	Start_day
#'                                    End_year	End_month	End_day
#'
#' @export
#'
#' @examples
#'     dt.date <-  ddate2date.3d(1985 + seq(0,1,by=.1))
#'     Episod2ymd(dt.date$start.date,dt.date$end.date)
Episod2ymd  <- function(start.date,end.date) {
  cbind(
    date2ymd( start.date, c('Start_year','Start_month','Start_day')),
    date2ymd( end.date, c('End_year','End_month','End_day'))
  ) -> dt.date.int
  return(dt.date.int)
}


################################################################################
#' granule.data2ymd
#'
#'  Transform a table of 3 columns in format dates to table with 9
#'  columns of integers (convert each date into 3 integers: year,
#'  month, day)
#'
#'
#' @param dt.date data.table with columns: start.date end.date
#'     central.date
#'
#' @return data.table with columns: Year Month Day Start_year
#'     Start_month Start_day End_year End_month End_day
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
   date2ymd( dt.date$start.date, c('Start_year','Start_month','Start_day')),
   date2ymd( dt.date$end.date, c('End_year','End_month','End_day'))
   ) -> dt.date.int
 return(dt.date.int)
}
################################################################################
