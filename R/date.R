
#' @importFrom lubridate ymd
#' @export
yyyymmdd <-function (x,sep='-')
{
  if (sep == '0' | sep =='n' | sep =='N' )
  {
    res <- ymd(x);
  }else if (sep =='-'){
    res <- as.Date(x);
  }else if (sep =='.'){
    res <- as.Date(x,format='%Y.%m.%d');
  }else if(sep == '/'){
    res <- as.Date(x,format='%Y/%m/%d');
  }else{
    warning('请设置正确的日期格式，并且不允许使用\\');
  }

}

#' 用于输入月日年格式数据
#'
#' @param x 日期数据
#' @param sep 分隔符
#'
#' @return 返回数据
#' @export
#'
#' @examples mmddyyyy(12-23-2018);
 mmddyyyy <-function(x,sep='-'){
  arg_year <-'%Y';
  arg_month <-'%m';
  arg_day <-'%d';
  format <-paste(arg_month,sep,arg_day,sep,arg_year,sep='');
  res <- as.Date(x,format=format);
 }


#' 用于输入日月年格式的数据
#'
#' @param x 输入数据
#' @param sep 分隔符
#'
#' @return 反回标准Date数据
#' @export
#'
#' @examples ddmmyyyy(01-02-2018);
 ddmmyyyy <-function(x,sep='-')
 {
   arg_year <-'%Y';
   arg_month <-'%m';
   arg_day <-'%d';
   format <-paste(arg_day,sep,arg_month,sep,arg_year,sep='');
   res <- as.Date(x,format=format);
 }

















