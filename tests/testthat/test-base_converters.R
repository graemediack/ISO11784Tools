testthat("Hexadecimal to Decimal converter works",{
  expect_equal(hexadecimal_to_decimal('2a'),'42')})

testthat("Binary to Decimal converter works",{
  expect_equal(binary_to_decimal('00101010'),'42')})

testthat("Decimal to Binary converter works",{
  expect_equal(decimal_to_binary('42'),'00101010')})

testthat("Decimal to Hexadecimal converter works",{
  expect_equal(decimal_to_hexadecimal('42'),'2a')})

testthat("Hexadecimal to Binary converter works",{
  expect_equal(hexadecimal_to_binary('2a'),'00101010')})

testthat("Binary to Hexadecimal converter works",{
  expect_equal(binary_to_hexadecimal('00101010'),'2a')})

# Test extremes
# case 0
testthat("Hexadecimal to Decimal converter works",{
  expect_equal(hexadecimal_to_decimal('0'),'0')})

testthat("Binary to Decimal converter works",{
  expect_equal(binary_to_decimal('0'),'0')})

testthat("Decimal to Binary converter works",{
  expect_equal(decimal_to_binary('0'),'0000')})

testthat("Decimal to Hexadecimal converter works",{
  expect_equal(decimal_to_hexadecimal('0'),'0')})

testthat("Hexadecimal to Binary converter works",{
  expect_equal(hexadecimal_to_binary('0'),'0000')})

testthat("Binary to Hexadecimal converter works",{
  expect_equal(binary_to_hexadecimal('0'),'0')})

# Case maximum
testthat("Hexadecimal to Decimal converter works",{
  expect_equal(hexadecimal_to_decimal('40000000000000'),'18014398509481984')})

testthat("Binary to Decimal converter works",{
  expect_equal(binary_to_decimal('01000000000000000000000000000000000000000000000000000000'),'18014398509481984')})

testthat("Decimal to Binary converter works",{
  expect_equal(decimal_to_binary('18014398509481984'),'01000000000000000000000000000000000000000000000000000000')})

testthat("Decimal to Hexadecimal converter works",{
  expect_equal(decimal_to_hexadecimal('18014398509481984'),'40000000000000')})

testthat("Hexadecimal to Binary converter works",{
  expect_equal(hexadecimal_to_binary('40000000000000'),'01000000000000000000000000000000000000000000000000000000')})

testthat("Binary to Hexadecimal converter works",{
  expect_equal(binary_to_hexadecimal('01000000000000000000000000000000000000000000000000000000'),'40000000000000')})
