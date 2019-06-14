#' 返回所有R支持的基本的数据类型
#'
#' @return 返回值
#' @export
#'
#' @examples datatype();
datatype <- function() {
  res <-c(character='character',
          numeric='numeric',
          logical='logical',
          integer='integer',
          list='list',
          factor='factor',
          matrix='matrix',
          data.frame='data.frame',
          array='array',
          complex='complex',
          raw='raw'
          #后以新增的类型只可以做在此行后面
          #代码开始


          #代码结束
          );
  return(res);
}


