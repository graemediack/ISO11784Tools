#' ISO 11784 Dot Hexadecimal format To ISO11784 15 Digit Decimal
#' @param .data A vector of Dot Hexadecimal format strings.
#' @return A vector of ISO11784 15 Digit Decimal format strings.
#' @export
#' @examples
#' isodothex_to_isodecimal(c('3DD.3BC4567890'))
isodothex_to_isodecimal <- function(.data){

  formatTest <- ISO11784Tools::get_iso11784_format(as.character(.data)) == "isodothex"

  out <- as.character(.data)
  out[!formatTest] <- NA

  if(!all(is.na(out))){
    manufacturer <- out
    animalID <- out

    # extract manufacturer (left) and animal id (right) components
    manufacturer[formatTest] <- stringr::str_split(manufacturer[formatTest],pattern = '\\.',simplify = T)[,1]
    animalID[formatTest] <- stringr::str_split(animalID[formatTest],pattern = '\\.',simplify = T)[,2]

    # calculations LEFT
    manufacturer[formatTest] <- lapply(manufacturer[formatTest],ISO11784Tools::hexadecimal_to_decimal)
    # calculations RIGHT
    animalID[formatTest] <- lapply(animalID[formatTest],ISO11784Tools::hexadecimal_to_decimal)
    # Leading zero's are removed in this process and need added back on, animalID only
    animalID[formatTest] <- animalID[formatTest] %>% stringr::str_pad(width = 12, side = 'left',pad = '0')

    out[formatTest] <- paste0(manufacturer[formatTest],animalID[formatTest])
  }

  out

}

#' ISO 11784 Raw Hexadecimal format, animal ID on the LEFT, To ISO11784 15 Digit Decimal
#' @param .data A vector of Raw Hexadecimal format strings.
#' @return A vector of ISO11784 15 Digit Decimal format strings.
#' @export
#' @examples
#' iso64bitleft_to_isodecimal(c('8000ABCDEF123456'))
iso64bitleft_to_isodecimal <- function(.data){

  formatTest <- ISO11784Tools::get_iso11784_format(as.character(.data)) == "iso64bitl"

  out <- as.character(.data)
  out[!formatTest] <- NA

  if(!all(is.na(out))){
    manufacturer <- out
    animalID <- out

    # extract manufacturer (left) and animal id (right) components
    # left transformations and calculations
    manufacturer[formatTest] <- stringr::str_sub(manufacturer[formatTest],5,7)
    manufacturer[formatTest] <- lapply(manufacturer[formatTest],ISO11784Tools::hexadecimal_to_binary)
    manufacturer[formatTest] <- stringr::str_sub(manufacturer[formatTest],1,10)
    manufacturer[formatTest] <- lapply(manufacturer[formatTest],ISO11784Tools::binary_to_decimal)
    #right transformations and calculations
    animalID[formatTest] <- stringr::str_sub(animalID[formatTest],7,16)
    animalID[formatTest] <- lapply(animalID[formatTest],ISO11784Tools::hexadecimal_to_binary)
    animalID[formatTest] <- stringr::str_sub(animalID[formatTest],3,40)
    animalID[formatTest] <- lapply(animalID[formatTest],ISO11784Tools::binary_to_decimal)
    # Leading zero's are removed in this process and need added back on, animalID only
    animalID[formatTest] <-  stringr::str_pad(animalID[formatTest],width = 12,pad = "0",side = 'left')

    out[formatTest] <- paste0(manufacturer[formatTest],animalID[formatTest])
  }

  out
}

#' ISO 11784 Raw Hexadecimal format, animal ID on the RIGHT, To ISO11784 15 Digit Decimal
#' @param .data A vector of Raw Hexadecimal format strings.
#' @return A vector of ISO11784 15 Digit Decimal format strings.
#' @export
#' @examples
#' iso64bitright_to_isodecimal(c('ABCDEF1234560001'))
iso64bitright_to_isodecimal <- function(.data){

  formatTest <- ISO11784Tools::get_iso11784_format(as.character(.data)) == "iso64bitr"

  out <- as.character(.data)
  out[!formatTest] <- NA

  if(!all(is.na(out))){
    manufacturer <- out
    animalID <- out

    # extract manufacturer (left) and animal id (right) components
    # left transformations and calculations
    manufacturer[formatTest] <- stringr::str_sub(manufacturer[formatTest],10,12)
    manufacturer[formatTest] <- lapply(manufacturer[formatTest],ISO11784Tools::hexadecimal_to_binary)
    manufacturer[formatTest] <- stringr::str_sub(manufacturer[formatTest],3,12)
    manufacturer[formatTest] <- stringi::stri_reverse(manufacturer[formatTest])
    manufacturer[formatTest] <- lapply(manufacturer[formatTest],ISO11784Tools::binary_to_decimal)
    #right transformations and calculations
    animalID[formatTest] <- stringr::str_sub(animalID[formatTest],1,10)
    animalID[formatTest] <- lapply(animalID[formatTest],ISO11784Tools::hexadecimal_to_binary)
    animalID[formatTest] <- stringr::str_sub(animalID[formatTest],1,38)
    animalID[formatTest] <- stringi::stri_reverse(animalID[formatTest])
    animalID[formatTest] <- lapply(animalID[formatTest],ISO11784Tools::binary_to_decimal)
    # Leading zero's are removed in this process and need added back on, animalID only
    animalID[formatTest] <-  stringr::str_pad(animalID[formatTest],width = 12,pad = "0",side = 'left')

    out[formatTest] <- paste0(manufacturer[formatTest],animalID[formatTest])
  }

  out
}
