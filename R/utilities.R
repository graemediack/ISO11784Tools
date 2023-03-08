#' Convert a list of codes to all other formats
#' @param .data A vector of character strings
#' @return  A tibble containing the original codes with a detected format column plus a column for each known format
#' @export
#' @examples
#' convert_to_all(c('3DD.ABC4567890'))
convert_to_all <- function(.data){

  out <- tibble::as_tibble(as.character(.data))
  out$format <- get_iso11784_format(.data)
  out$isodecimal <- convert_to_isodecimal(.data)
  out$isodothex <- convert_to_isodothex(.data)
  out$iso64bitl <- convert_to_iso64bitl(.data)
  out$iso64bitr <- convert_to_iso64bitr(.data)

  out
}

#' ISO11784 Pattern detection routine to identify (as far as possible) the input format
#' @param .data A vector of character strings
#' @return  A vector of character strings, each item being one of 5 formats: c('unknown', 'isodecimal', 'isodothex','iso64bitl', 'iso64bitr')
#' @export
#' @examples
#' get_iso11784_format(c('3DD.ABC4567890'))
get_iso11784_format <- function(.data){

  ISOdothex <- "^[:xdigit:]{3}[\\.]{1}[:xdigit:]{10}$" # also known as bi-hex
  ISO64bitl <- "^8000[:xdigit:]{12}$" # 64 bits and animal tag bit on the left
  ISO64bitr <- "^[:xdigit:]{12}0001$" # 64 bits and animal tag bit on the right
  #ISOdecimal <- "^[0-9]{15}$"
  ISOdecimal <- "^[0-9]{3}[_\\.]{0,1}[0-9]{12}$"

  out <- tibble::as_tibble(as.character(.data)) #convert to tibble and ensure
  out[is.na(out)] <- "" # replace all NA values with empty string
  out$format <- "unknown"

  # capture basic format detection comparisons
  out[stringr::str_detect(out$value,ISOdecimal),]$format <- 'isodecimal'
  out[stringr::str_detect(out$value,ISOdothex),]$format <- 'isodothex'
  out[stringr::str_detect(out$value,ISO64bitl),]$format <- 'iso64bitl'
  out[stringr::str_detect(out$value,ISO64bitr),]$format <- 'iso64bitr'
  # reset outsider cases back to unknown
  # NOTE special case for ISODECIMAL - Handle possible underscore or dot separating manufacturer and animalID segments
  out[out$format == 'isodecimal',][as.numeric(stringr::str_sub(stringr::str_remove_all(out[out$format == 'isodecimal',]$value, "[_\\.]"),4,-1)) > 274877906943,]$format <- 'unknown' # this number is the biggest 38 bit binary number, animal ID cannot be larger than 38 bits
  out[out$format == 'isodothex',][as.hexmode(stringr::str_sub(out[out$format == 'isodothex',]$value,5,5)) > as.hexmode('3'),]$format <- 'unknown' # this number is the biggest 38 bit binary number, animal ID cannot be larger than 38 bits
  out[out$format == 'iso64bitl',][!(stringr::str_detect(out[out$format == 'iso64bitl',]$value,"[a-fA-F]")),]$format <- 'unknown' # cautiously assume that 64bit hexadecimal should have at least 1 alpha character
  out[out$format == 'iso64bitr',][!(stringr::str_detect(out[out$format == 'iso64bitr',]$value,"[a-fA-F]")),]$format <- 'unknown' # cautiously assume that 64bit hexadecimal should have at least 1 alpha character

  out$format
}


#' Convert any recognised ISO11784/5 format to Decimal format
#' @param .data A vector of character strings
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_isodecimal(c('3E7.02DFDC1C35','8000F9C2DFDC1C36','EC383BFB439F0001'))
convert_to_isodecimal <- function(.data){

    ISOdecimal <- isodothex_to_isodecimal(isodecimal_to_isodothex(.data))
    ISOdothex <- isodothex_to_isodecimal(.data)
    ISO64bitLeft <- iso64bitleft_to_isodecimal(.data)
    ISO64bitRight <- iso64bitright_to_isodecimal(.data)
    return(dplyr::coalesce(ISOdecimal,ISOdothex,ISO64bitLeft,ISO64bitRight))

}


#' Convert any recognised ISO11784/5 format to Dot Hexadecimal format
#' @param .data A vector of character strings
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_isodothex(c('999012345678901','8000F9C2DFDC1C36','EC383BFB439F0001'))
convert_to_isodothex <- function(.data){

    ISOdecimal <- isodecimal_to_isodothex(.data)
    ISOdothex <- isodecimal_to_isodothex(isodothex_to_isodecimal(.data))
    ISO64bitLeft <- isodecimal_to_isodothex(iso64bitleft_to_isodecimal(.data))
    ISO64bitRight <- isodecimal_to_isodothex(iso64bitright_to_isodecimal(.data))
    return(dplyr::coalesce(ISOdecimal,ISOdothex,ISO64bitLeft,ISO64bitRight))
}


#' Convert any recognised ISO11784/5 format to Raw Hexadecimal format, animal tag bit on the Left
#' @param .data A vector of character strings
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_iso64bitl(c('999012345678901','3E7.02DFDC1C36','EC383BFB439F0001'))
convert_to_iso64bitl <- function(.data){

    ISOdecimal <- isodecimal_to_iso64bitleft(.data)
    ISOdothex <- isodecimal_to_iso64bitleft(isodothex_to_isodecimal(.data))
    ISO64bitLeft <- isodecimal_to_iso64bitleft(iso64bitleft_to_isodecimal(.data))
    ISO64bitRight <- isodecimal_to_iso64bitleft(iso64bitright_to_isodecimal(.data))
    return(dplyr::coalesce(ISOdecimal,ISOdothex,ISO64bitLeft,ISO64bitRight))
}


#' Convert any recognised ISO11784/5 format to Raw Hexadecimal format, animal tag bit on the Right
#' @param .data A vector of character strings
#' @return A converted vector of character strings
#' @export
#' @examples
#' convert_to_iso64bitr(c('999012345678901','8000F9C2DFDC1C36','3E7.02DFDC1C37'))
convert_to_iso64bitr <- function(.data){

    ISOdecimal <- isodecimal_to_iso64bitright(.data)
    ISOdothex <- isodecimal_to_iso64bitright(isodothex_to_isodecimal(.data))
    ISO64bitLeft <- isodecimal_to_iso64bitright(iso64bitleft_to_isodecimal(.data))
    ISO64bitRight <- isodecimal_to_iso64bitright(iso64bitright_to_isodecimal(.data))
    return(dplyr::coalesce(ISOdecimal,ISOdothex,ISO64bitLeft,ISO64bitRight))
}
