#' Main functions for converting from ISO11784 15 Digit Decimal to other formats
#' These are wrappers for the functions in baseConverters.R
#'
#'
#' ISO11784 15 Digit Decimal format To ISO11784 Dot Hexadecimal
#' @param dec A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Dot Hexadecimal format strings.
#' @export
#' @examples
#' ISOdecimalToISOdothex(c('989737733408912'))
ISOdecimalToISOdothex <- function(dec){
  out <- c()
  for(i in dec){
    if(nchar(i) != 15){
      warning(paste0("Format does not meet ISO11784/5 standard - Not 15 digits: ",i))
      out <- append(out,NA)
    }else if(as.numeric(stringr::str_sub(i,4,15)) > 274877906943){
      warning(paste0("Format does not meet ISO11784/5 standard - Animal ID exceeds max (274877906943/38bits): ",stringr::str_sub(i,4,15)))
      out <- append(out,NA)
    }else{
      # convert the input integer to hexadecimal ISO format ABC.1234567ABC
      manufacturer <- stringr::str_sub(i,1,3) # split the input value to 3/12
      animalID <- stringr::str_sub(i,4,15) # split the input value to 3/12

      # calculations LEFT
      manufacturer <- dec2hex(manufacturer)
      # calculations RIGHT
      animalID <- dec2hex(animalID)
      # Leading zero's are removed in this process and need added back on, animalID only
      animalID <- stringr::str_pad(string = animalID,width = 10,side = 'left',pad = '0')
      # return finished hexadecimal string
      out <- append(out,stringr::str_to_upper(paste0(manufacturer,'.',animalID)))
    }
  }
  return(out)
}

#' ISO11784 15 Digit Decimal format To ISO11784 Raw Hexadecimal format, animal ID on the LEFT
#' @param dec A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Raw Hexadecimal format strings.
#' @export
#' @examples
#' ISODecimalToISO64bitLeft(c('989737733408912'))
ISODecimalToISO64bitLeft <- function(dec){
  out <- c()
  for(i in dec){
    if(nchar(i) != 15){
      warning(paste0("Format does not meet ISO11784/5 standard - Not 15 digits: ",i))
      out <- append(out,NA)
    }else if(as.numeric(stringr::str_sub(i,4,15)) > 274877906943){
      warning(paste0("Format does not meet ISO11784/5 standard - Animal ID exceeds max (274877906943/38bits): ",stringr::str_sub(i,4,15)))
      out <- append(out,NA)
    }else{
      manufacturer <- stringr::str_sub(i,1,3) # split the input value to 3/12
      animalID <- stringr::str_sub(i,4,15) # split the input value to 3/12

      manufacturer <- stringr::str_pad(dec2bin(manufacturer),width = 10,pad = "0",side = 'left')
      animalID <- stringr::str_pad(dec2bin(animalID),width = 38,pad = "0",side = 'left')

      ISO64bitLeft <- paste0('1000000000000000',manufacturer,animalID)

      out <- append(out,stringr::str_to_upper(bin2hex(ISO64bitLeft)))
    }
  }
  return(out)
}

#' ISO11784 15 Digit Decimal format To ISO11784 Raw Hexadecimal format, animal ID on the RIGHT
#' @param dec A vector of ISO11784 15 Digit Decimal format strings.
#' @return A vector of Raw Hexadecimal format strings.
#' @export
#' @examples
#' ISODecimalToISO64bitRight(c('989737733408912'))
ISODecimalToISO64bitRight <- function(dec){
  out <- c()
  for(i in dec){
    if(nchar(i) != 15){
      warning(paste0("Format does not meet ISO11784/5 standard - Not 15 digits: ",i))
      out <- append(out,NA)
    }else if(as.numeric(stringr::str_sub(i,4,15)) > 274877906943){
      warning(paste0("Format does not meet ISO11784/5 standard - Animal ID exceeds max (274877906943/38bits): ",stringr::str_sub(i,4,15)))
      out <- append(out,NA)
    }else{
      manufacturer <- stringr::str_sub(i,1,3) # split the input value to 3/12
      animalID <- stringr::str_sub(i,4,15) # split the input value to 3/12

      manufacturer <- stringr::str_pad(dec2bin(manufacturer),width = 10,pad = "0",side = 'left')
      animalID <- stringr::str_pad(dec2bin(animalID),width = 38,pad = "0",side = 'left')
      ISO64bitLeft <- paste0('1000000000000000',manufacturer,animalID)
      ISO64bitRight <- stringi::stri_reverse(ISO64bitLeft)
      out <- append(out,stringr::str_to_upper(bin2hex(ISO64bitRight)))
    }
  }
  return(out)
}
