library(phonicsFR)
context("Phonex")

test_that("phonex returns expected values", {
  expect_equal(phonex('amoxiciline'), phonex('amoxicilline') )
  expect_equal(phonex('i'), phonex('y'))
  expect_equal(phonex('hache'), phonex('ache'))
  expect_equal(phonex('phenix'), phonex('fenix'))
  expect_equal(phonex('gain'), phonex('guein'))
  expect_equal(phonex('peau'), phonex('po'))
  expect_equal(phonex('peine'), phonex('pène'))
  expect_equal(phonex('satisfaisant'), phonex('satisfeisent'))
  expect_equal(phonex('numerize'), phonex('numerise'))
  expect_equal(phonex('euf'), phonex('oeuf'))
  expect_equal(phonex('silice'), phonex('cilice'))
  expect_equal(phonex('hirshprung'), phonex('hirschprung'))
  expect_equal(phonex('banane'), phonex('bannane'))
  expect_equal(phonex('genou'), phonex('genoux'))
  expect_equal(phonex('antoine'), phonex('entouane'))
  expect_equal(phonex('amoxicilline'), 6.272727, tolerance = 1e-6 )
})

test_that('phonex trims expected characters', {
  expect_equal(phonex('amoxi ciline'), phonex('amoxicilline') )
  expect_equal(phonex('amoxi-ciline'), phonex('amoxicilline') )
})

test_that('phonex throughs error if input is invalid', {
  testthat::expect_error(phonex(456))
  expect_error(phonex('amoxi-cilline', char_to_remove = ''))
})

test_that('phonex returns string when needed', {
  expect_equal(phonex('amoxicilline', convert_to_num = FALSE), "ONOXISILINE")
})
