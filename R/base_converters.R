#' Hexadecimal to Decimal converter
#' @param hex A single hexadecimal number as a string. Max '40000000000000'
#' @return A single decimal number as a string
#' @export
#' @examples
#' hexadecimal_to_decimal('ABC123')
hexadecimal_to_decimal <- function(hex){
  if(length(hex) > 1){
    stop('hexadecimal_to_decimal function only accepts a single number')
  }
  hexVector <- stringr::str_split(hex,'',simplify = F)[[1]]
  if(length(hexVector) > 14 | (length(hexVector) == 14 & sum(as.numeric(hexVector[stringr::str_detect(hexVector,"[0-9]")]),na.rm = T) > 4)){
    stop(paste0(hex,' is too large for this package, sorry! (max = "40000000000000")'))
  }else{
    pow <- (length(hexVector)-1):0
    dec <- 0
    for(x in 1:length(hexVector)){
      dec <- dec + as.numeric((strtoi(hexVector[x],16)*(16**pow[x])))
    }
  }
  as.character(dec)
}


#' Binary to Decimal converter
#' @param bin A single binary number as a string - nchar(bin) <= 54
#' @return A single decimal number as a string
#' @export
#' @examples
#' binary_to_decimal('1001100')
binary_to_decimal <- function(bin){
  if(length(bin) > 1){
    stop('binary_to_decimal function only accepts a single number')
  }
  if(stringr::str_count(bin,'1')+stringr::str_count(bin,'0') != nchar(bin)){
    stop(paste0(bin,' contains non-binary digits or characters'))
  }
  binVector <- stringr::str_split(bin,"",simplify = F)[[1]]
  # https://stackoverflow.com/questions/24009982/remove-zeros-in-the-start-and-end-of-a-vector
  if(min(which(binVector != 0)) == Inf){
    return('0')
  }else{
    binVector <- binVector[min(which(binVector != 0)):length(binVector)] # remove leading zero's
    if(length(binVector) > 55 | (length(binVector) == 55 & sum(as.numeric(binVector)) > 1)){
      stop(paste0(bin,' is too large for this package, sorry! (max = "1000000000000000000000000000000000000000000000000000000")'))
    }else{
      powerSeq <- seq(length(binVector)-1,0)
      dec <- 0
      for(i in 1:length(binVector)){
        dec <- dec + (strtoi(binVector[i]) * (2**powerSeq[i]))
      }
    }
    as.character(dec)
  }
}

#' Decimal to Binary converter
#' @param dec A single decimal number as a string
#' @return A single binary number as a string
#' @export
#' @examples
#' decimal_to_binary('1234')
decimal_to_binary <- function(dec){
  dec <- as.numeric(dec)
  bin <- c() # initialise empty vector to capture binary characters
  while(dec != 0){
    bin <- append(bin,dec%%2,after = 0)
    dec <- dec%/%2
  }
  bin <- paste0(bin,collapse = "")
  # always return whole nibbles
  if(nchar(bin)%%4){
    return(stringr::str_pad(bin,width = nchar(bin)+(4-nchar(bin)%%4),pad = "0",side = 'left'))
  }else{
    return(bin)
  }

}

#' Decimal to Hexadecimal converter
#' @param dec A single decimal number as a string
#' @return A single hexadecimal number as a string
#' @export
#' @examples
#' decimal_to_hexadecimal('1234')
decimal_to_hexadecimal <- function(dec){
  dec <- as.numeric(dec)
  hex <- c() # initialise empty vector to capture hex characters
  while(dec != 0){
    hex <- append(hex, as.character(as.hexmode(dec%%16)),after = 0)
    dec <- dec%/%16
  }
  paste0(hex,collapse = "")
}

#' Hexadecimal to Binary converter
#' This is simply a wrapper for decimal_to_binary and hexadecimal_to_decimal
#' @param hex A single hexadecimal number as a string
#' @return A single binary number as a string
#' @export
#' @examples
#' hexadecimal_to_binary('ABC123')
hexadecimal_to_binary <- function(hex){
  decimal_to_binary(hexadecimal_to_decimal(hex))
}

#' Binary to Hexadecimal converter
#' @param bin A single binary number as a string
#' @return A single hexadecimal number as a string
#' @export
#' @examples
#' binary_to_hexadecimal('1001101')
binary_to_hexadecimal <- function(bin){
  binPadded <- stringr::str_pad(string = bin,
                                width = nchar(bin)+nchar(bin)%%4,
                                side = 'left',
                                pad = '0') # ensure binary string has whole nibbles
  binClustered <- stringi::stri_sub(binPadded,
                                    seq(1,nchar(binPadded),by=4),
                                    length=4) # convert to nibbles (4 bit chunks)
  dec <- strtoi(binClustered,2)
  hex <- as.hexmode(dec)
  paste0(hex,collapse = "")
}
