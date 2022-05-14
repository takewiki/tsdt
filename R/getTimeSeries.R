#' 获取指定日期时间开始的时间序号，具体到秒
#'
#' @param startDatetime 开始日期，带时间部分，UTC
#' @param seconds   秒数
#'
#' @return 返回标准UTC或CST的时间类型
#' @export
#'
#' @examples  getTimeSeries();
getTimeSeries <- function(startDatetime=Sys.time(),seconds=86400)
{
  con<- "POSIXct" %in% class(startDatetime);
  con <- !con
  {
    if ( con){
      stop('参数startDatetime必须为UTC或CST日期时间型，请先标准化！')
    } else if ( seconds >0){
      timeIndex <- 1:seconds -1;
      startDatetime+timeIndex;
    } else if (seconds <0){
      timeIndex <-seconds:-1 +1;
      startDatetime +timeIndex;
    } else
    {
      stop('seconds不能等于0！')
    }
  }
}
