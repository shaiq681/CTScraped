context('count_clinical_trials test')


test_that("Expected Output", {
  expect_gte(count_clinical_trials(query = ''), 353491)
  expect_gte(count_clinical_trials(query = 'nsclc'), 5600)
  expect_gte(count_clinical_trials(query = 'covid19'), 3688)
  expect_gte(count_clinical_trials(query = 'covid19 AND (newyork OR italy)'), 380)
  expect_equal(count_clinical_trials(query = 'jhefjwejwhvf12199jbkbv'),0)
  expect_equal(class(count_clinical_trials(query = 'jhefjwejwhvf12199jbkbv')), 'integer')
  expect_equal(class(count_clinical_trials(query = 'covid19 AND (newyork OR italy)')), 'integer')
  expect_warning(count_clinical_trials(query = ''))
  expect_warning(count_clinical_trials())
  expect_error(query = 101010101)
  expect_visible(count_clinical_trials(query = 'covid19'))

})
