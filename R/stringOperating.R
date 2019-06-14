#' 获取字符赂量中每个元素的长度，而不是向量的长度
#'
#' @param x  字符型数据
#'
#' @return   数值型向量
#' @export
#' @import stringr
#' @examples len(letters);
len <- function (x)
{
  if (class(x) !='character')
  {
    stop("'x'参数必须是字符型向量!")
  }else{
    str_length(x)
  }
}

#' 从左边截取n个字符
#'
#' @param x 字符串向量
#' @param num_char  字符个数
#'
#' @return 返回字符向量
#' @export
#' @import stringr
#' @examples left(letters,1);
left <- function (x,num_char=1){
  nmax <- max(len(x));
  if (is.na(num_char)){
    ""
  }else if(is.na(nmax)){
    ""

  }else if (num_char >= nmax)
  {
    ""
  }else{
    str_sub(x, 1, num_char);
  }

}

#' 从右边截取指定位数的字符串
#'
#' @param x 签字文本
#' @param num_char  签字位数
#'
#' @return 返回字符串
#' @export
#' @import stringr
#' @examples right(letters,3);
right <- function (x,num_char){
  nstart <- len(x)-num_char+1;
  ncount <-length(x);
  res <- list(ncount);
  for (i in 1:ncount){
    res[[i]]=str_sub(x[i],nstart[i],-1L);
  }
  res <-unlist(res);
  res;

}

#' 从字符中间进行测试
#'
#' @param x 字符串
#' @param start 开始位置
#' @param num_char 位数
#'
#' @return 返回字符串
#' @export
#' @import stringr
#' @examples mid('sdfdsfdsf',2,4);
mid  <- function (x,startIndex,num_char){
      endIndex <- startIndex+num_char-1;
      str_sub(x,startIndex,endIndex);
}

#' 将字符串进行拆分开
#'
#' @param x 字符串
#' @param pattern 可以使用字符串或正则表达式[]
#'
#' @return 返回一个列表
#' @export
#' @import stringr
#' @examples split.str('afsdsdf,bbbfsdfds,sdfds',',');
splitStr <- function (x,pattern){
  str_split(x,pattern);
}

#' 自动过滤符合条件的字符串,也称为字符串的子集或截取
#'
#' @param x 字符向量
#' @param pattern 可以使用文本或表达式
#'
#' @return 返回符合条件的结果
#' @export
#' @import stringr
#' @examples subsetStr(letters,'a');
subsetStr <- function (x,pattern)
{
  str_subset(x,pattern);
}

#' 用于字符串的替代
#'
#' @param x 字符向量
#' @param pattern 字符或正则表达式
#' @param newStr 新的替代字符串
#'
#' @return 返回值
#' @export
#' @import stringr
#' @examples replaceStr('abcd','c','f');
replaceStr <- function (x,pattern,newStr)
{
  str_replace(x,pattern,newStr);
}


