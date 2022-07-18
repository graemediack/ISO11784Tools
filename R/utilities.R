#' ISO11784 Pattern detection routine to identify (guess!) the input format
#' @param id A vector of character strings
#' @return One of 5 formats: c('unknown', 'isodecimal', 'isodothex','iso64bitl', 'iso64bitr')
#' @export
#' @examples
#' get_iso11784_format(c('3DD.ABC4567890'))

get_iso11784_format <- function(id){
  if(!is.na(id)){
    ISOdothex <- "^[:xdigit:]{3}[\\.]{1}[:xdigit:]{10}$"
    ISO64bitl <- "^8000[:xdigit:]{12}$" # 64 bits and animal tag bit on the left
    ISO64bitr <- "^[:xdigit:]{12}0001$" # 64 bits and animal tag bit on the right
    ISOdecimal <- "^[0-9]{15}$"

    out <- tibble::tibble(id = id)
    out$format <- "unknown"

    # capture basic format detection comparisons
    out[stringr::str_detect(out$id,ISOdecimal),]$format <- 'isodecimal'
    out[stringr::str_detect(out$id,ISOdothex),]$format <- 'isodothex'
    out[stringr::str_detect(out$id,ISO64bitl),]$format <- 'iso64bitl'
    out[stringr::str_detect(out$id,ISO64bitr),]$format <- 'iso64bitr'
    # reset outsider cases back to unknown
    out[out$format == 'isodecimal',][as.numeric(stringr::str_sub(out[out$format == 'isodecimal',]$id,4,-1)) > 274877906943,]$format <- 'unknown' # this number is the biggest 38 bit binary number, animal ID cannot be larger than 38 bits
    out[out$format == 'isodothex',][as.hexmode(stringr::str_sub(out[out$format == 'isodothex',]$id,5,5)) > as.hexmode('3'),]$format <- 'unknown' # this number is the biggest 38 bit binary number, animal ID cannot be larger than 38 bits
    out[out$format == 'iso64bitl',][!(stringr::str_detect(out[out$format == 'iso64bitl',]$id,"[a-fA-F]")),]$format <- 'unknown' # cautiously assume that 64bit hexadecimal should have at least 1 alpha character
    out[out$format == 'iso64bitr',][!(stringr::str_detect(out[out$format == 'iso64bitr',]$id,"[a-fA-F]")),]$format <- 'unknown' # cautiously assume that 64bit hexadecimal should have at least 1 alpha character

    return(out$format)
  }else{
    return('unknown')
  }
}


#' Convert any recognised ISO11784/5 format to Decimal format
#' @param .data A vector of character strings
#' @param format a string indicating which format the ID is in, intended to be used with output from get_iso11784_format
#'     c('unknown', 'isodecimal', 'isodothex','iso64bitl', 'iso64bitr')
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_isodecimal(c('3E7.02DFDC1C35','8000F9C2DFDC1C36','EC383BFB439F0001'))
convert_to_isodecimal <- function(.data,format = NULL){
  if(!(is.null(format))){
    if(format == 'isodecimal'){
      return(.data)
    }else if(format == 'isodothex'){
      return(isodothex_to_isodecimal(.data))
    }else if(format == 'iso64bitl'){
      return(iso64bitleft_to_isodecimal(.data))
    }else if(format == 'iso64bitr'){
      return(iso64bitright_to_isodecimal(.data))
    }else{
      warning(paste0("Unkown format ",format))
      return(rep(NA,length(.data)))
    }
  }else{
    ISOdecimal <- isodothex_to_isodecimal(isodecimal_to_isodothex(.data))
    ISOdothex <- isodothex_to_isodecimal(.data)
    ISO64bitLeft <- iso64bitleft_to_isodecimal(.data)
    ISO64bitRight <- iso64bitright_to_isodecimal(.data)
    return(dplyr::coalesce(ISOdecimal,ISOdothex,ISO64bitLeft,ISO64bitRight))
  }

}


#' Convert any recognised ISO11784/5 format to Dot Hexadecimal format
#' @param .data A vector of character strings
#' @param format a string indicating which format the ID is in, intended to be used with output from get_iso11784_format
#'     c('unknown', 'isodecimal', 'isodothex','iso64bitl', 'iso64bitr')
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_isodothex(c('999012345678901','8000F9C2DFDC1C36','EC383BFB439F0001'))
convert_to_isodothex <- function(.data,format = NULL){
  if(!(is.null(format))){
    if(format == 'isodecimal'){
      return(isodecimal_to_isodothex(.data))
    }else if(format == 'isodothex'){
      return(.data)
    }else if(format == 'iso64bitl'){
      return(isodecimal_to_isodothex(iso64bitleft_to_isodecimal(.data)))
    }else if(format == 'iso64bitr'){
      return(isodecimal_to_isodothex(iso64bitright_to_isodecimal(.data)))
    }else{
      warning(paste0("Unkown format ",format))
      return(rep(NA,length(.data)))
    }
  }else{
    ISOdecimal <- isodecimal_to_isodothex(.data)
    ISOdothex <- isodecimal_to_isodothex(isodothex_to_isodecimal(.data))
    ISO64bitLeft <- isodecimal_to_isodothex(iso64bitleft_to_isodecimal(.data))
    ISO64bitRight <- isodecimal_to_isodothex(iso64bitright_to_isodecimal(.data))
    return(dplyr::coalesce(ISOdecimal,ISOdothex,ISO64bitLeft,ISO64bitRight))
  }
}


#' Convert any recognised ISO11784/5 format to Raw Hexadecimal format, animal tag bit on the Left
#' @param .data A vector of character strings
#' @param format a string indicating which format the ID is in, intended to be used with output from get_iso11784_format
#'     c('unknown', 'isodecimal', 'isodothex','iso64bitl', 'iso64bitr')
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_iso64bitl(c('999012345678901','3E7.02DFDC1C36','EC383BFB439F0001'))
convert_to_iso64bitl <- function(.data,format = NULL){
  if(!(is.null(format))){
    if(format == 'isodecimal'){
      return(isodecimal_to_iso64bitleft(.data))
    }else if(format == 'isodothex'){
      return(isodecimal_to_iso64bitleft(isodothex_to_isodecimal(.data)))
    }else if(format == 'iso64bitl'){
      return(.data)
    }else if(format == 'iso64bitr'){
      return(isodecimal_to_iso64bitleft(iso64bitright_to_isodecimal(.data)))
    }else{
      warning(paste0("Unkown format ",format))
      return(rep(NA,length(.data)))
    }
  }else{
    ISOdecimal <- isodecimal_to_iso64bitleft(.data)
    ISOdothex <- isodecimal_to_iso64bitleft(isodothex_to_isodecimal(.data))
    ISO64bitLeft <- isodecimal_to_iso64bitleft(iso64bitleft_to_isodecimal(.data))
    ISO64bitRight <- isodecimal_to_iso64bitleft(iso64bitright_to_isodecimal(.data))
    return(dplyr::coalesce(ISOdecimal,ISOdothex,ISO64bitLeft,ISO64bitRight))
  }
}


#' Convert any recognised ISO11784/5 format to Raw Hexadecimal format, animal tag bit on the Right
#' @param .data A vector of character strings
#' @param format a string indicating which format the ID is in, intended to be used with output from get_iso11784_format
#'     c('unknown', 'isodecimal', 'isodothex','iso64bitl', 'iso64bitr')
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_iso64bitr(c('999012345678901','8000F9C2DFDC1C36','3E7.02DFDC1C37'))
convert_to_iso64bitr <- function(.data,format = NULL){
  if(!(is.null(format))){
    if(format == 'isodecimal'){
      return(isodecimal_to_iso64bitright(.data))
    }else if(format == 'isodothex'){
      return(isodecimal_to_iso64bitright(isodothex_to_isodecimal(.data)))
    }else if(format == 'iso64bitl'){
      return(isodecimal_to_iso64bitright(iso64bitleft_to_isodecimal(.data)))
    }else if(format == 'iso64bitr'){
      return(.data)
    }else{
      warning(paste0("Unkown format ",format))
      return(rep(NA,length(.data)))
    }
  }else{
    ISOdecimal <- isodecimal_to_iso64bitright(.data)
    ISOdothex <- isodecimal_to_iso64bitright(isodothex_to_isodecimal(.data))
    ISO64bitLeft <- isodecimal_to_iso64bitright(iso64bitleft_to_isodecimal(.data))
    ISO64bitRight <- isodecimal_to_iso64bitright(iso64bitright_to_isodecimal(.data))
    return(dplyr::coalesce(ISOdecimal,ISOdothex,ISO64bitLeft,ISO64bitRight))
  }
}
