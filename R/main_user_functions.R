#' Main package functions intended for users
#'


#' ISO 11784 Pattern detection routine to identify (guess!) the input format
#' @param .data A vector of character strings
#' @return One of 5 formats: c('unknown', 'isodecimal', 'isodothex','iso64bitl', 'iso64bitr')
#' @export
#' @examples
#' get_iso11784_format(c('3DD.ABC4567890'))

get_iso11784_format <- function(.data){

  ISOdothex <- "^[:xdigit:]{3}[\\.]{1}[:xdigit:]{10}$"
  ISO64bitl <- "^8000[:xdigit:]{12}$" # 64 bits and animal tag bit on the left
  ISO64bitr <- "^[:xdigit:]{12}0001$" # 64 bits and animal tag bit on the right
  ISOdecimal <- "^[0-9]{15}$"

  out <- c()
  for(id in .data){
    if(stringr::str_detect(id,ISOdothex)){
      if(hex2dec(stringr::str_sub(id,5,14)) > 274877906943){ # this number is the biggest 38 bit binary number, animal ID cannot be larger than 38 bits
        out <- append(out,'unknown')
      }else{
        out <- append(out,'ISOdothex')
      }
    }else  if(stringr::str_detect(id,ISOdecimal)){
      if(as.numeric(stringr::str_sub(id,4,-1)) > 274877906943){ # this number is the biggest 38 bit binary number, animal ID cannot be larger than 38 bits
        out <- append(out,'unknown')
      }else{
        out <- append(out,'ISOdecimal')
      }
    }else if(stringr::str_detect(id,ISO64bitl)){
      if(stringr::str_detect(id,"[a-fA-F]")){
        out <- append(out,'ISO64bitl')
      }else{
        out <- append(out,'unknown')
      }
    }else if(stringr::str_detect(id,ISO64bitr)){
      if(stringr::str_detect(id,"[a-fA-F]")){
        out <- append(out,'ISO64bitr')
      }else{
        out <- append(out,'unknown')
      }
    }else{out <- append(out,'unknown')}
  }
  out
}


#' Convert any recognised ISO11784/5 format to Decimal format
#' @param .data A vector of character strings
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_isodecimal(c('3E7.02DFDC1C35','8000F9C2DFDC1C36','EC383BFB439F0001'))
convert_to_isodecimal <- function(.data){
  out <- c()
  for(id in .data){
    if(get_iso11784_format(id) == 'ISOdecimal'){
      out <- append(out,id)
    }else if(get_iso11784_format(id) == 'ISOdothex'){
      out <- append(out,ISOdothexToISOdecimal(id))
    }else if(get_iso11784_format(id) == 'ISO64bitl'){
      out <- append(out,ISO64bitLeftToISODecimal(id))
    }else if(get_iso11784_format(id) == 'ISO64bitr'){
      out <- append(out,ISO64bitRightToISODecimal(id))
    }else{
      out <- append(out,NA)
    }
  }
  out
}


#' Convert any recognised ISO11784/5 format to Dot Hexadecimal format
#' @param .data A vector of character strings
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_isodothex(c('999012345678901','8000F9C2DFDC1C36','EC383BFB439F0001'))
convert_to_isodothex <- function(.data){
  out <- c()
  for(id in .data){
    if(get_iso11784_format(id) == 'ISOdothex'){
      out <- append(out,id)
    }else if(get_iso11784_format(id) == 'ISOdecimal'){
      out <- append(out,ISOdecimalToISOdothex(id))
    }else if(get_iso11784_format(id) == 'ISO64bitl'){
      out <- append(out,ISOdecimalToISOdothex(ISO64bitLeftToISODecimal(id)))
    }else if(get_iso11784_format(id) == 'ISO64bitr'){
      out <- append(out,ISOdecimalToISOdothex(ISO64bitRightToISODecimal(id)))
    }else{
      out <- append(out,NA)
    }
  }
  out
}


#' Convert any recognised ISO11784/5 format to Raw Hexadecimal format, animal tag bit on the Left
#' @param .data A vector of character strings
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_iso64bitl(c('999012345678901','3E7.02DFDC1C36','EC383BFB439F0001'))
convert_to_iso64bitl <- function(.data){
  out <- c()
  for(id in .data){
    if(get_iso11784_format(id) == 'ISO64bitl'){
      out <- append(out,id)
    }else if(get_iso11784_format(id) == 'ISOdecimal'){
      out <- append(out,ISODecimalToISO64bitLeft(id))
    }else if(get_iso11784_format(id) == 'ISOdothex'){
      out <- append(out,ISODecimalToISO64bitLeft(ISOdothexToISOdecimal(id)))
    }else if(get_iso11784_format(id) == 'ISO64bitr'){
      out <- append(out,ISODecimalToISO64bitLeft(ISO64bitRightToISODecimal(id)))
    }else{
      out <- append(out,NA)
    }
  }
  out
}


#' Convert any recognised ISO11784/5 format to Raw Hexadecimal format, animal tag bit on the Right
#' @param .data A vector of character strings
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_iso64bitr(c('999012345678901','8000F9C2DFDC1C36','3E7.02DFDC1C37'))
convert_to_iso64bitr <- function(.data){
  out <- c()
  for(id in .data){
    if(get_iso11784_format(id) == 'ISO64bitr'){
      out <- append(out,id)
    }else if(get_iso11784_format(id) == 'ISOdecimal'){
      out <- append(out,ISODecimalToISO64bitRight(id))
    }else if(get_iso11784_format(id) == 'ISO64bitl'){
      out <- append(out,ISODecimalToISO64bitRight(ISO64bitLeftToISODecimal(id)))
    }else if(get_iso11784_format(id) == 'ISOdothex'){
      out <- append(out,ISODecimalToISO64bitRight(ISOdothexToISOdecimal(id)))
    }else{
      out <- append(out,NA)
    }
  }
  out
}
