# ISO11784 PIT Tag Conversion Functions
# NOTES R maxes out at 31 bit integer, DescTools pacakge has some functions that do the job of the ones written here but 
# MAXIMUMS:
# HexToDec('7fffffff') = 2147483647 (31 bits)
# DecToHex(2147483647) = 7fffffff (31 bits)
# DecToBin(536870911) = 11111111111111111111111111111 (29 bits)
# .Machine$integer.max = 2147483647 = 1111111111111111111111111111111 (31bits)
# All conversion calculations were written by Graeme Diack with the assistance of rapidtable.com

# Base Converters
hex2dec <- function(hex){
  # general purpose hexadecimal to decimal convertor
  # INPUT: single hexadecimal number as string
  # OUTPUT: single decimal number as string
  hex <- stringr::str_split(hex,'',simplify = F)[[1]]
  pow <- (length(hex)-1):0
  dec <- 0
  for(x in 1:length(hex)){
    dec <- dec + (strtoi(hex[x],16)*(16**pow[x]))
  }
  return(dec)
}

bin2dec <- function(bin){
  # general purpose binary to decimal convertor
  # DO NOT USE ON BINARY NUMBERS LONGER THAN 54bits. R integer maxes out at .Machine$integer.max
  # It seems after some testing R will sum up to 18014398509481984 or 54bits as long as it is not stored as an integer object! Weird, but good for me!
  # INPUT: single binary number as string (54bits or less)
  # OUTPUT: single decimal number as string
  binVector <- stringr::str_split(bin,"",simplify = F)[[1]]
  if(length(binVector) > 54){
    stop(paste0('Binary number is too large (',length(binVector),'bit)'))
  }else{
    powerSeq <- seq(nchar(bin)-1,0)
    dec <- 0
    for(i in 1:length(binVector)){
      dec <- dec + (strtoi(binVector[i]) * (2**powerSeq[i]))
    }
  }
  return(as.character(dec))
}

dec2bin <- function(dec){
  # general purpose decimal to binary convertor
  # worth noting potential time saver in postgresql:
  # SELECT tag_id_isodecimal,substring(tag_id_isodecimal,1,3)::int8::bit(10) AS manufacturer,substring(tag_id_isodecimal,4)::int8::bit(38) AS animal FROM tags WHERE tag_id_isodecimal != '';
  # INPUT: single decimal number as string
  # OUTPUT: single binary number as string
  dec <- as.numeric(dec)
  bin <- c()
  while(dec != 0){
    bin <- append(bin,dec%%2,after = 0)
    dec <- dec%/%2
  }
  #return(stringi::stri_reverse(paste0(bin,collapse = "")))
  return(paste0(bin,collapse = ""))
}

dec2hex <- function(dec){
  # general purpose decimal to binary convertor
  # INPUT: single decimal number as string
  # OUTPUT: single binary number as string
  dec <- as.numeric(dec)
  hex <- c() # initialise empty vector to capture hex characters
  while(dec != 0){
    hex <- append(hex, as.character(as.hexmode(dec%%16)),after = 0)
    dec <- dec%/%16
  }
  return(paste0(hex,collapse = ""))
}

hex2bin <- function(hex){
  # general purpose hexadecimal to binary convertor
  # this is a wrapper for dec2bin and hex2dec
  # INPUT: single hexadecimal number as string
  # OUTPUT: single binary number as string
  out <- stringr::str_pad(dec2bin(hex2dec(hex)),width = nchar(hex)*4,pad = "0",side = 'left')
  return(out)
}

bin2hex <- function(bin){
  # general purpose binary to hexadecimal convertor
  # INPUT: single binary number as string
  # OUTPUT: single hexadecimal number as string padded with zero's on the left to 16 characters
  hex <- stringi::stri_sub(bin, seq(1, stringi::stri_length(bin),by=4), length=4)
  hex <- strtoi(hex,2)
  hex <- as.hexmode(hex)
  return(paste0(hex,collapse = ""))
}


# wrappers to derive the correct format
# First stage, convert all of the formats to decimal
### 3 wrappers
ISOdothexToISOdecimal <- function(hex){
  # convert the input hexadecimal ISO format ABC.1234567ABC to decimal ISO format 123456789012345
  # INPUT: single ISO11784 Hexadecimal 'dot' format e.g., 3DD.ABC4567890
  # OUTPUT: single ISO11784 Decimal format e.g., 989078187493520
  out <- c()
  for(i in hex){
    # extract left and right components
    hexLeft <- stringr::str_split(i,'\\.',simplify = T)[1]
    hexRight <- stringr::str_split(i,'\\.',simplify = T)[2]
    # calculations LEFT
    decLeft <- hex2dec(hexLeft)
    # calculations RIGHT
    decRight <- hex2dec(hexRight)
    # Leading zero's are removed in this process and need added back on
    # only required for right hand component
    if(nchar(decRight) < 12){
      leadingZero <- paste0(rep('0',12-stringr::str_length(decRight)),collapse = '')
    }
    out <- append(out,paste0(decLeft,leadingZero,decRight))
  }
  return(out)
}

ISO64bitLeftToISODecimal <- function(hex){
  # function to convert 64 bit ISO in hex format to ISO decimal format
  # INPUT: single 64 bit hexadecimal number as string
  ##  ISO64bitl <- "^8000[:xdigit:]{12}$" # 64 bits and LSB on the left
  # OUTPUT: single ISO decimal number as string
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

ISO64bitRightToISODecimal <- function(hex){
  # function to convert 64 bit ISO in hex format to ISO decimal format
  # INPUT: single 64 bit hexadecimal number as string
  ##  ISO64bitr <- "^[:xdigit:]{12}0001$" # 64 bits and LSB on the right
  # OUTPUT: single ISO decimal number as string
  out <- c()
  for(i in hex){
    # part the hex string into manufacturer chunk (10 bits following the right most 16) and animal ID (the left most 38 bits)
    manufacturer <- bin2dec(stringi::stri_reverse(stringr::str_sub(hex2bin(stringr::str_sub(i,10,12)),3,12)))
    animalID <- stringr::str_pad(bin2dec(stringi::stri_reverse(stringr::str_sub(hex2bin(stringr::str_sub(i,1,10)),1,38))),
      width = 12,
      pad = "0",
      side = 'left')
    out <- append(out,paste0(manufacturer,animalID))
  }
  return(out)
}

#second stage, convert from decimal to all of the other formats
### 3 wrappers

ISOdecimalToISOdothex <- function(dec){
  out <- c()
  for(i in dec){
    # convert the input integer to hexadecimal ISO format ABC.1234567ABC
    decLeft <- as.numeric(stringr::str_sub(i,1,3)) # split the input value to 3/12
    decRight <- as.numeric(stringr::str_sub(i,4,15)) # split the input value to 3/12
    # calculations LEFT
    hexLeft <- dec2hex(decLeft)
    # calculations RIGHT
    hexRight <- dec2hex(decRight)
    # Leading zero's are removed in this process and need added back on
    leadingZero <- paste0(rep('0',10-stringr::str_length(hexRight)),collapse = '')
    # return finished hexadecimal string
    out <- append(out,stringr::str_to_upper(paste0(hexLeft,'.',leadingZero,hexRight)))
  }
  return(out)
}

ISODecimalToISO64bitLeft <- function(dec){
  # function to convert ISO decimal format to 64 bit ISO in hex format (left hand LSB variant) 
  # INPUT: single ISO decimal number as string
  ##  ISO64bitl <- "^8000[:xdigit:]{12}$" # 64 bits and LSB on the left
  # OUTPUT: single 64 bit hexadecimal number as string
  out <- c()
  for(i in dec){
    manufacturer <- stringr::str_pad(dec2bin(stringr::str_sub(i,1,3)),width = 10,pad = "0",side = 'left')
    animalID <- stringr::str_pad(dec2bin(stringr::str_sub(i,4,15)),width = 38,pad = "0",side = 'left')
    ISO64bitLeft <- paste0('1000000000000000',manufacturer,animalID)
    out <- append(out,stringr::str_to_upper(bin2hex(ISO64bitLeft)))
  }
  return(out)
}

ISODecimalToISO64bitRight <- function(dec){
  # function to convert ISO decimal format to 64 bit ISO in hex format (left hand LSB variant) 
  # INPUT: single ISO decimal number as string
  ##  ISO64bitr <- "^[:xdigit:]{12}0001$" # 64 bits and LSB on the right
  # OUTPUT: single 64 bit hexadecimal number as string
  out <- c()
  for(i in dec){
    manufacturer <- stringr::str_pad(dec2bin(stringr::str_sub(i,1,3)),width = 10,pad = "0",side = 'left')
    animalID <- stringr::str_pad(dec2bin(stringr::str_sub(i,4,15)),width = 38,pad = "0",side = 'left')
    ISO64bitLeft <- paste0('1000000000000000',manufacturer,animalID)
    ISO64bitRight <- stringi::stri_reverse(ISO64bitLeft)
    out <- append(out,stringr::str_to_upper(bin2hex(ISO64bitRight)))
  }
  return(out)
}