#'获取所有的时区名称
#'
#' @return 返回列表
#' @export
#'
#' @examples timeZoneNames();
timeZoneNames <- function(){
  OlsonNames();
};


#' 用于输入标准的日期时间
#'
#' @param x 标准的日期变量，
#'
#' @return 返回
#' @export
#' @importFrom lubridate ymd_hms
#' @examples
#' datetimeInput("2010-12-13 15:30:30");
datetimeInput <- function (x)
{
  time <- ymd_hms(x);
  time
};


#' 将UTC时间使用本地时间进行显示，实际存储还是UTC时间
#'
#' @param x 标准PISIXct格式日期
#' @param timeZone 时区名，详细见timeZoneNames
#'
#' @return 显示当天的时间格式，存储保持不变
#' @export
#' @importFrom lubridate with_tz
#'
#' @examples
#' datetime.LocalPrint(datetime('2018-02-16 12:31:47'),'PRC');
datetime.LocalPrint <- function(x,timeZone=Sys.timezone())
{
  with_tz(x,tzone = timeZone);
}

#' 输入本地的时间，
#'
#' @param x 输入长日期的日期与时分秒部分
#' @param timeZone 时区
#'
#' @return 返回当地存储的日期数据，非utc
#' @export
#' @importFrom  lubridate force_tz
#' @examples datetime.LocalInput('2018-12-31 12:21:22');
datetime.LocalInput <-function (x,timeZone=Sys.timezone())
{
  dt <- datetimeInput(x); # UTC value
  res <- force_tz(dt,tzone = timeZone);
}

