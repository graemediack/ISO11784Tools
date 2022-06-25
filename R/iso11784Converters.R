

# ISO11784 PIT Tag Conversion Functions
# NOTES R maxes out at 31 bit integer, DescTools pacakge has some functions that do the job of the ones written here but
# MAXIMUMS:
# HexToDec('7fffffff') = 2147483647 (31 bits)
# DecToHex(2147483647) = 7fffffff (31 bits)
# DecToBin(536870911) = 11111111111111111111111111111 (29 bits)
# .Machine$integer.max = 2147483647 = 1111111111111111111111111111111 (31bits)
# All conversion calculations were written by Graeme Diack with the assistance of rapidtable.com

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
