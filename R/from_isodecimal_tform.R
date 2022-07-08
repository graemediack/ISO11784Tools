#' ISO11784 15 Digit Decimal format To ISO11784 Dot Hexadecimal
#' @param .data A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Dot Hexadecimal format strings.
#' @export
#' @examples
#' ISOdecimalToISOdothex(c('989737733408912'))
ISOdecimalToISOdothex <- function(.data){
  out <- c()
  for(i in .data){
    if(ISO11784Tools::get_iso11784_format(i) != 'ISOdecimal'){
      warning("Unexpected format does not match ISOdecimal")
      out <- append(out,NA)
    }else{
      # convert the input integer to hexadecimal ISO format ABC.1234567ABC
      manufacturer <- stringr::str_sub(i,1,3) # split the input value to 3/12
      animalID <- stringr::str_sub(i,4,15) # split the input value to 3/12

      # calculations LEFT
      manufacturer <- decimal_to_hexadecimal(manufacturer)
      # calculations RIGHT
      animalID <- decimal_to_hexadecimal(animalID)
      # Leading zero's are removed in this process and need added back on, animalID only
      animalID <- stringr::str_pad(string = animalID,width = 10,side = 'left',pad = '0')
      # return finished hexadecimal string
      out <- append(out,stringr::str_to_upper(paste0(manufacturer,'.',animalID)))
    }
  }
  out
}

#' ISO11784 15 Digit Decimal format To ISO11784 Raw Hexadecimal format, animal ID on the LEFT
#' @param .data A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Raw Hexadecimal format strings.
#' @export
#' @examples
#' ISODecimalToISO64bitLeft(c('989737733408912'))
ISODecimalToISO64bitLeft <- function(.data){
  out <- c()
  for(i in .data){
    if(ISO11784Tools::get_iso11784_format(i) != 'ISOdecimal'){
      warning("Unexpected format does not match ISOdecimal")
      out <- append(out,NA)
    }else{
      manufacturer <- stringr::str_sub(i,1,3) # split the input value to 3/12
      animalID <- stringr::str_sub(i,4,15) # split the input value to 3/12

      manufacturer <- decimal_to_binary(manufacturer)
      animalID <- decimal_to_binary(animalID)
      # check that manufacturer is exactly 10 characters long, if not remove leading 0 or pad leading 0
      manufacturerVector <- stringr::str_split(manufacturer,"",simplify = F)[[1]]
      if(min(which(manufacturerVector != 0)) == Inf){
        manufacturerVector <- rep("0",10) # simple solution, manufacturer was all 0, regardless of length return 10 character binary 0
      }else{
        manufacturerVector <- manufacturerVector[min(which(manufacturerVector != 0)):length(manufacturerVector)]
      }
      manufacturer <- paste0(manufacturerVector,collapse = "")
      manufacturer <- stringr::str_pad(manufacturer,width = 10,pad = "0",side = 'left')

      # check that animalID is exactly 38 characters long, if not remove leading 0 or pad leading 0
      animalIDVector <- stringr::str_split(animalID,"",simplify = F)[[1]]
      if(min(which(animalIDVector != 0)) == Inf){
        animalIDVector <- rep("0",38) # simple solution, manufacturer was all 0, regardless of length return 10 character binary 0
      }else{
        animalIDVector <- animalIDVector[min(which(animalIDVector != 0)):length(animalIDVector)]
      }
      animalID <- paste0(animalIDVector,collapse = "")
      animalID <- stringr::str_pad(animalID,width = 38,pad = "0",side = 'left')

      ISO64bitLeft <- paste0('1000000000000000',manufacturer,animalID)

      out <- append(out,stringr::str_to_upper(binary_to_hexadecimal(ISO64bitLeft)))
    }
  }
  out
}

#' ISO11784 15 Digit Decimal format To ISO11784 Raw Hexadecimal format, animal ID on the RIGHT
#' @param .data A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Raw Hexadecimal format strings.
#' @export
#' @examples
#' ISODecimalToISO64bitRight(c('989737733408912'))
ISODecimalToISO64bitRight <- function(.data){
  out <- c()
  for(i in .data){
    if(ISO11784Tools::get_iso11784_format(i) != 'ISOdecimal'){
      warning("Unexpected format does not match ISOdecimal")
      out <- append(out,NA)
    }else{
      manufacturer <- stringr::str_sub(i,1,3) # split the input value to 3/12
      animalID <- stringr::str_sub(i,4,15) # split the input value to 3/12
      # convert each to binary
      manufacturer <- decimal_to_binary(manufacturer)
      animalID <- decimal_to_binary(animalID)
      # check that binary manufacturer is exactly 10 characters long, if not remove leading 0 or pad leading 0
      manufacturerVector <- stringr::str_split(manufacturer,"",simplify = F)[[1]]
      if(min(which(manufacturerVector != 0)) == Inf){
        manufacturerVector <- rep("0",10) # simple solution, manufacturer was all 0, regardless of length return 10 character binary 0
      }else{
        manufacturerVector <- manufacturerVector[min(which(manufacturerVector != 0)):length(manufacturerVector)]
      }
      manufacturer <- paste0(manufacturerVector,collapse = "")
      manufacturer <- stringr::str_pad(manufacturer,width = 10,pad = "0",side = 'left')

      # check that animalID is exactly 38 characters long, if not remove leading 0 or pad leading 0
      animalIDVector <- stringr::str_split(animalID,"",simplify = F)[[1]]
      if(min(which(animalIDVector != 0)) == Inf){
        animalIDVector <- rep("0",38) # simple solution, manufacturer was all 0, regardless of length return 10 character binary 0
      }else{
        animalIDVector <- animalIDVector[min(which(animalIDVector != 0)):length(animalIDVector)]
      }
      animalID <- paste0(animalIDVector,collapse = "")
      animalID <- stringr::str_pad(animalID,width = 38,pad = "0",side = 'left')

      ISO64bitLeft <- paste0('1000000000000000',manufacturer,animalID)

      ISO64bitRight <- stringi::stri_reverse(ISO64bitLeft)

      out <- append(out,stringr::str_to_upper(binary_to_hexadecimal(ISO64bitRight)))
    }
  }
  out
}
