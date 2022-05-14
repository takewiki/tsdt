#' 数据类型转移
#'
#' @param x 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mysql_toR()
mysql_toR <- function(x) {
  type = class(x)
  res =switch (type,
               character = as.character(x),
               integer = as.integer(x),
               decimal.Decimal = as.numeric(x)
               
               
  )
  return(res)
}
