#' Main functions for converting from other formats TO ISO11784 15 Digit Decimal
#' These are wrappers for the functions in baseConverters.R
#'
#'

#' ISO 11784 Dot Hexadecimal format To ISO11784 15 Digit Decimal
#' @param hex A vector of Dot Hexadecimal format strings.
#' @return A vector of ISO11784 15 Digit Decimal format strings.
#' @export
#' @examples
#' ISOdothexToISOdecimal(c('3DD.ABC4567890'))
ISOdothexToISOdecimal <- function(hex){
  out <- c()
  for(i in hex){
    # extract manufacturer (left) and animal id (right) components
    manufacturer <- stringr::str_split(i,'\\.',simplify = T)[1]
    animalID <- stringr::str_split(i,'\\.',simplify = T)[2]
    # calculations LEFT
    manufacturer <- hex2dec(manufacturer)
    # calculations RIGHT
    animalID <- hex2dec(animalID)
    # Leading zero's are removed in this process and need added back on, animalID only
    animalID <- stringr::str_pad(string = animalID,width = 12, side = 'left',pad = '0')
    out <- append(out,paste0(manufacturer,animalID))
  }
  return(out)
}

#' ISO 11784 Raw Hexadecimal format, animal ID on the LEFT, To ISO11784 15 Digit Decimal
#' @param hex A vector of Raw Hexadecimal format strings.
#' @return A vector of ISO11784 15 Digit Decimal format strings.
#' @export
#' @examples
#' ISO64bitLeftToISODecimal(c('8000ABCDEF123456'))
ISO64bitLeftToISODecimal <- function(hex){
  out <- c()
  for(i in hex){
    # part the hex string into manufacturer chunk (10 bits following the left most 16) and animal ID (the right most 38 bits)
    manufacturer <- bin2dec(stringr::str_sub(hex2bin(stringr::str_sub(i,5,7)),1,10))
    animalID <- stringr::str_pad(bin2dec(stringr::str_sub(hex2bin(stringr::str_sub(i,7,16)),3,40)),
      width = 12,
      pad = "0",
      side = 'left')
    out <- append(out,paste0(manufacturer,animalID))
  }
  return(out)
}

#' ISO 11784 Raw Hexadecimal format, animal ID on the RIGHT, To ISO11784 15 Digit Decimal
#' @param hex A vector of Raw Hexadecimal format strings.
#' @return A vector of ISO11784 15 Digit Decimal format strings.
#' @export
#' @examples
#' ISO64bitRightToISODecimal(c('ABCDEF1234560001'))
ISO64bitRightToISODecimal <- function(hex){
  out <- c()
  for(i in hex){
    # part the hex string into manufacturer chunk (10 bits following the right most 16) and animal ID (the left most 38 bits)
    # note, reverses the binary strings because in this format the least significant bit is on the left
    manufacturer <- bin2dec(stringi::stri_reverse(stringr::str_sub(hex2bin(stringr::str_sub(i,10,12)),3,12)))
    animalID <- stringr::str_pad(bin2dec(stringi::stri_reverse(stringr::str_sub(hex2bin(stringr::str_sub(i,1,10)),1,38))),
      width = 12,
      pad = "0",
      side = 'left')
    out <- append(out,paste0(manufacturer,animalID))
  }
  return(out)
}
