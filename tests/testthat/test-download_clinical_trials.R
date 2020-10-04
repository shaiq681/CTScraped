context('download_clinical_trials test')


test_that("API Limits", {
  expect_warning(download_clinical_trials('ALL'))
  expect_warning(download_clinical_trials('cancer'))
  expect_equal(dim(download_clinical_trials('cancer'))[1], 10000)
  expect_equal(dim(download_clinical_trials('cancer'))[2], 10)
  expect_true(any(c("Rank","NCTId","BriefTitle","Condition","OverallStatus",
                "LeadSponsorClass","LeadSponsorName","InterventionName",
                "InterventionType","Keyword") %in% names(download_clinical_trials('covid19'))))
  expect_true('tbl' %in% class(download_clinical_trials('covid19')))
  expect_true('data.frame' %in% class(download_clinical_trials('covid19')))
  expect_true(all(row.names(download_clinical_trials('cancer')) %in% 1:10000))
})
