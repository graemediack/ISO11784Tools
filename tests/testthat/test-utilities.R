#' Set up testing dataframe

df <- data.frame(isodecimal = c('999012345678901','999012345678902','999012345678903','999012345678904','999012345678905','999012345678906','999012345678907','999012345678908','999012345678909','999012345678910'),
                 isodothex = c('3E7.02DFDC1C35','3E7.02DFDC1C36','3E7.02DFDC1C37','3E7.02DFDC1C38','3E7.02DFDC1C39','3E7.02DFDC1C3A','3E7.02DFDC1C3B','3E7.02DFDC1C3C','3E7.02DFDC1C3D','3E7.02DFDC1C3E'),
                 iso64bitleft = c('8000F9C2DFDC1C35','8000F9C2DFDC1C36','8000F9C2DFDC1C37','8000F9C2DFDC1C38','8000F9C2DFDC1C39','8000F9C2DFDC1C3A','8000F9C2DFDC1C3B','8000F9C2DFDC1C3C','8000F9C2DFDC1C3D','8000F9C2DFDC1C3E'),
                 iso64bitright = c('AC383BFB439F0001','6C383BFB439F0001','EC383BFB439F0001','1C383BFB439F0001','9C383BFB439F0001','5C383BFB439F0001','DC383BFB439F0001','3C383BFB439F0001','BC383BFB439F0001','7C383BFB439F0001'),
                 stringsAsFactors = F)

test_that("get_iso11784_format Works",{
  expect_equal(
    get_iso11784_format(
      c(df$isodecimal[1:2],df$isodothex[1:2],df$iso64bitleft[1:2],df$iso64bitright[1:2],c('1234567890123','ABD.4567890123'))
    ),
    c('ISOdecimal','ISOdecimal','ISOdothex','ISOdothex','ISO64bitl','ISO64bitl','ISO64bitr','ISO64bitr','unknown','unknown')
  )
})

test_that("Convert to ISO Decimal Works",{
  expect_equal(convert_to_isodecimal(df$isodecimal), df$isodecimal)
  expect_equal(convert_to_isodecimal(df$isodothex), df$isodecimal)
  expect_equal(convert_to_isodecimal(df$iso64bitleft), df$isodecimal)
  expect_equal(convert_to_isodecimal(df$iso64bitright), df$isodecimal)
})

test_that("Convert to ISO Dot Hexadecimal Works",{
  expect_equal(convert_to_isodothex(df$isodecimal), df$isodothex)
  expect_equal(convert_to_isodothex(df$isodothex), df$isodothex)
  expect_equal(convert_to_isodothex(df$iso64bitleft), df$isodothex)
  expect_equal(convert_to_isodothex(df$iso64bitright), df$isodothex)
})

test_that("Convert to ISO Raw Hexadecimal (Animal bit left) Works",{
  expect_equal(convert_to_iso64bitl(df$isodecimal), df$iso64bitleft)
  expect_equal(convert_to_iso64bitl(df$isodothex), df$iso64bitleft)
  expect_equal(convert_to_iso64bitl(df$iso64bitleft), df$iso64bitleft)
  expect_equal(convert_to_iso64bitl(df$iso64bitright), df$iso64bitleft)
})

test_that("Convert to ISO Raw Hexadecimal (Animal bit right) Works",{
  expect_equal(convert_to_iso64bitr(df$isodecimal), df$iso64bitright)
  expect_equal(convert_to_iso64bitr(df$isodothex), df$iso64bitright)
  expect_equal(convert_to_iso64bitr(df$iso64bitleft), df$iso64bitright)
  expect_equal(convert_to_iso64bitr(df$iso64bitright), df$iso64bitright)
})