#' 生成标准的日期型时间序列
#'
#' @param startDate 开始日期，默认为当天
#' @param days 天数，可以为正数或负数，不能等于0
#'
#' @return 返回时间序列
#' @export
#' @include dateInput.R
#' @examples
#' getDateSeries(as.Date('2018-01-01'),days-100);
getDateSeries <- function (startDate=Sys.Date(),days=100)
{
  if ( class(startDate) !='Date'){
    stop('参数startDate必须为日期型，请先标准化！')
  } else if ( days >0){
    dateIndex <- 1:days -1;
    startDate+dateIndex;
  } else if (days <0){
    dateIndex <-days:-1 +1;
    startDate +dateIndex;
  } else
  {
    stop('days不能等于0！')
  }
  }


