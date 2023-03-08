#' Set up testing dataframe

df <- data.frame(isodecimal = c(NA,'999012345678901','999012345678902','999012345678903',NA,'999012345678904','999012345678905','999012345678906','999012345678907','999012345678908','999012345678909','999012345678910',NA),
                 isodothex = c(NA,'3E7.02DFDC1C35','3E7.02DFDC1C36','3E7.02DFDC1C37',NA,'3E7.02DFDC1C38','3E7.02DFDC1C39','3E7.02DFDC1C3A','3E7.02DFDC1C3B','3E7.02DFDC1C3C','3E7.02DFDC1C3D','3E7.02DFDC1C3E',NA),
                 iso64bitleft = c(NA,'8000F9C2DFDC1C35','8000F9C2DFDC1C36','8000F9C2DFDC1C37',NA,'8000F9C2DFDC1C38','8000F9C2DFDC1C39','8000F9C2DFDC1C3A','8000F9C2DFDC1C3B','8000F9C2DFDC1C3C','8000F9C2DFDC1C3D','8000F9C2DFDC1C3E',NA),
                 iso64bitright = c(NA,'AC383BFB439F0001','6C383BFB439F0001','EC383BFB439F0001',NA,'1C383BFB439F0001','9C383BFB439F0001','5C383BFB439F0001','DC383BFB439F0001','3C383BFB439F0001','BC383BFB439F0001','7C383BFB439F0001',NA),
                 isodecimal_extra = c(NA,'999.012345678901','999_012345678902','999_012345678903',NA,'999.012345678904','999.012345678905','999_012345678906','999_012345678907','999.012345678908','999.012345678909','999_012345678910',NA),
                 stringsAsFactors = F)
#'

test_that("Decimal to Dot Hex Works",{
  suppressWarnings({
    expect_equal(isodecimal_to_isodothex(df$isodecimal), df$isodothex)
  })
})

test_that("Decimal to 64 Bit Left Works",{
  suppressWarnings({
    expect_equal(isodecimal_to_iso64bitleft(df$isodecimal), df$iso64bitleft)
  })
})

test_that("Decimal to 64 Bit Right Works",{
  suppressWarnings({
    expect_equal(isodecimal_to_iso64bitright(df$isodecimal), df$iso64bitright)
  })
})
#' Test non standard decimal formats, e.g., underscores and dots separating manufacturer and animal codes
test_that("Decimal NON STANDARD to Dot Hex Works",{
  suppressWarnings({
    expect_equal(isodecimal_to_isodothex(df$isodecimal_extra), df$isodothex)
  })
})

test_that("Decimal NON STANDARD to 64 Bit Left Works",{
  suppressWarnings({
    expect_equal(isodecimal_to_iso64bitleft(df$isodecimal_extra), df$iso64bitleft)
  })
})

test_that("Decimal NON STANDARD to 64 Bit Right Works",{
  suppressWarnings({
    expect_equal(isodecimal_to_iso64bitright(df$isodecimal_extra), df$iso64bitright)
  })
})
