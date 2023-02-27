#' ISO11784 15 Digit Decimal format To ISO11784 Dot Hexadecimal
#' @param .data A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Dot Hexadecimal format strings.
#' @export
#' @examples
#' isodecimal_to_isodothex(c('989737733408912'))
isodecimal_to_isodothex <- function(.data){

  formatTest <- ISO11784Tools::get_iso11784_format(as.character(.data)) == "isodecimal"

  out <- as.character(.data)
  out[!formatTest] <- NA

  if(!all(is.na(out))){
    manufacturer <- out
    animalID <- out

    # extract manufacturer (left) and animal id (right) components
    manufacturer[formatTest] <- stringr::str_sub(manufacturer[formatTest],1,3)
    animalID[formatTest] <- stringr::str_sub(animalID[formatTest],4,15)

    # calculations LEFT
    manufacturer[formatTest] <- lapply(manufacturer[formatTest],ISO11784Tools::decimal_to_hexadecimal)
    # calculations RIGHT
    animalID[formatTest] <- lapply(animalID[formatTest],ISO11784Tools::decimal_to_hexadecimal)
    # Leading zero's are removed in this process and need added back on, animalID only
    animalID[formatTest] <-  stringr::str_pad(animalID[formatTest], width = 10, side = 'left',pad = '0')

    out[formatTest] <- stringr::str_to_upper(paste0(manufacturer[formatTest],'.',animalID[formatTest]))
  }

  out

}

#' ISO11784 15 Digit Decimal format To ISO11784 Raw Hexadecimal format, animal ID on the LEFT
#' @param .data A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Raw Hexadecimal format strings.
#' @export
#' @examples
#' isodecimal_to_iso64bitleft(c('989737733408912'))
isodecimal_to_iso64bitleft <- function(.data){

  formatTest <- ISO11784Tools::get_iso11784_format(as.character(.data)) == "isodecimal"

  out <- as.character(.data)
  out[!formatTest] <- NA

  if(!all(is.na(out))){
    manufacturer <- out
    animalID <- out

    # extract manufacturer (left) and animal id (right) components
    manufacturer[formatTest] <- stringr::str_sub(manufacturer[formatTest],1,3)
    animalID[formatTest] <- stringr::str_sub(animalID[formatTest],4,15)

    manufacturer[formatTest] <- lapply(manufacturer[formatTest],ISO11784Tools::decimal_to_binary)
    animalID[formatTest] <- lapply(animalID[formatTest],ISO11784Tools::decimal_to_binary)

    # LEFT hand checks (manufacturer)
    # Force length 10 bits (assumption made that manafacturer <= 999)
    manufacturer[formatTest] <- stringr::str_trunc(manufacturer[formatTest],width = 10,side = 'left',ellipsis = '')
    manufacturer[formatTest] <- stringr::str_pad(manufacturer[formatTest],width = 10,pad = "0",side = 'left')

    # RIGHT hand checks (animalID)
    # Force length 10 bits (assumption made that animalID <= 274877906943)
    animalID[formatTest] <- stringr::str_trunc(animalID[formatTest],width = 38,side = 'left',ellipsis = '')
    animalID[formatTest] <- stringr::str_pad(animalID[formatTest],width = 38,pad = "0",side = 'left')

    out[formatTest] <- paste0('1000000000000000',manufacturer[formatTest],animalID[formatTest])

    out[formatTest] <- lapply(out[formatTest],ISO11784Tools::binary_to_hexadecimal)

  }

  stringr::str_to_upper(as.vector(out))
}

#' ISO11784 15 Digit Decimal format To ISO11784 Raw Hexadecimal format, animal ID on the RIGHT
#' @param .data A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Raw Hexadecimal format strings.
#' @export
#' @examples
#' isodecimal_to_iso64bitright(c('989737733408912'))
isodecimal_to_iso64bitright <- function(.data){

  formatTest <- ISO11784Tools::get_iso11784_format(as.character(.data)) == "isodecimal"

  out <- as.character(.data)
  out[!formatTest] <- NA

  if(!all(is.na(out))){
    manufacturer <- out
    animalID <- out

    # extract manufacturer (left) and animal id (right) components
    manufacturer[formatTest] <- stringr::str_sub(manufacturer[formatTest],1,3)
    animalID[formatTest] <- stringr::str_sub(animalID[formatTest],4,15)

    manufacturer[formatTest] <- lapply(manufacturer[formatTest],ISO11784Tools::decimal_to_binary)
    animalID[formatTest] <- lapply(animalID[formatTest],ISO11784Tools::decimal_to_binary)

    # LEFT hand checks (manufacturer)
    # Force length 10 bits (assumption made that manafacturer <= 999)
    manufacturer[formatTest] <- stringr::str_trunc(manufacturer[formatTest],width = 10,side = 'left',ellipsis = '')
    manufacturer[formatTest] <- stringr::str_pad(manufacturer[formatTest],width = 10,pad = "0",side = 'left')

    # RIGHT hand checks (animalID)
    # Force length 10 bits (assumption made that animalID <= 274877906943)
    animalID[formatTest] <- stringr::str_trunc(animalID[formatTest],width = 38,side = 'left',ellipsis = '')
    animalID[formatTest] <- stringr::str_pad(animalID[formatTest],width = 38,pad = "0",side = 'left')

    out[formatTest] <- paste0('1000000000000000',manufacturer[formatTest],animalID[formatTest])
    out[formatTest] <- stringi::stri_reverse(out[formatTest])
    out[formatTest] <- lapply(out[formatTest],ISO11784Tools::binary_to_hexadecimal)

  }

  stringr::str_to_upper(as.vector(out))
}
