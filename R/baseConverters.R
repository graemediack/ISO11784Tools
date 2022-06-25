#' Base Converters
#' These functions are used by the main ISO11784 converters to switch between bases 2, 10 and 16


#' Hexadecimal to Decimal converter
#' @param hex A single hexadecimal number as a string. Max '40000000000000'
#' @return A single decimal number as a string
#' @examples
#' hex2dec('ABC123')
hex2dec <- function(hex){
  if(length(hex) > 1){
    stop('hex2dec function only accepts a single number')
  }
  hexVector <- stringr::str_split(hex,'',simplify = F)[[1]]
  if(length(hexVector) > 14 | (length(hexVector) == 14 & sum(as.numeric(hexVector),na.rm = T) > 4)){
    stop(paste0(hex,' is too large for this package, sorry! (max = 40000000000000)'))
  }else{
    pow <- (length(hexVector)-1):0
    dec <- 0
    for(x in 1:length(hexVector)){
      dec <- dec + as.numeric((strtoi(hexVector[x],16)*(16**pow[x])))
    }
  }
  return(dec)
}


#' Binary to Decimal converter
#' Will not work on binary numbers longer than 54bits. R integer maxes out at .Machine$integer.max, but
#' R will sum up to 18014398509481984, or 54bits, as long as it is not stored as an integer object.
#' @param bin A single binary number as a string with nchar(bin) <= 54
#' @return A single decimal number as a string
#' @examples
#' bin2dec('1001100')
bin2dec <- function(bin){
  if(length(bin) > 1){
    stop('bin2dec function only accepts a single number')
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
    if(length(binVector) > 54){
      stop(paste0('Binary number is too large for this package, sorry!(',length(binVector),'bit, max is 54bit)'))
    }else{
      powerSeq <- seq(nchar(bin)-1,0)
      dec <- 0
      for(i in 1:length(binVector)){
        dec <- dec + (strtoi(binVector[i]) * (2**powerSeq[i]))
      }
    }
    return(as.character(dec))
  }
}

#' Decimal to Binary converter
#' @param dec A single decimal number as a string
#' @return A single binary number as a string
#' @examples
#' dec2bin('1234')
dec2bin <- function(dec){
  dec <- as.numeric(dec)
  bin <- c() # initialise empty vector to capture binary characters
  while(dec != 0){
    bin <- append(bin,dec%%2,after = 0)
    dec <- dec%/%2
  }
  return(paste0(bin,collapse = ""))
}

#' Decimal to Hexadecimal converter
#' @param dec A single decimal number as a string
#' @return A single hexadecimal number as a string
#' @examples
#' dec2hex('1234')
dec2hex <- function(dec){
  dec <- as.numeric(dec)
  hex <- c() # initialise empty vector to capture hex characters
  while(dec != 0){
    hex <- append(hex, as.character(as.hexmode(dec%%16)),after = 0)
    dec <- dec%/%16
  }
  return(paste0(hex,collapse = ""))
}

#' Hexadecimal to Binary converter
#' This is simply a wrapper for dec2bin and hex2dec
#' @param hex A single hexadecimal number as a string
#' @return A single binary number as a string
#' @examples
#' hex2bin('ABC123')
hex2bin <- function(hex){
  out <- stringr::str_pad(dec2bin(hex2dec(hex)),width = nchar(hex)*4,pad = "0",side = 'left')
  return(out)
}

#' Binary to Hexadecimal converter
#' @param bin A single binary number as a string
#' @return A single hexadecimal number as a string
#' @examples
#' bin2hex('1001101')
bin2hex <- function(bin){
  binPadded <- stringr::str_pad(string = bin,
                                width = nchar(bin)+nchar(bin)%%4,
                                side = 'left',
                                pad = '0') # make sure binary string is divisible by 4
  binClustered <- stringi::stri_sub(binPadded,
                                    seq(1,nchar(binPadded),by=4),
                                    length=4) # convert to nibbles (4 bit chunks)
  dec <- strtoi(binClustered,2)
  hex <- as.hexmode(dec)
  return(paste0(hex,collapse = ""))
}
