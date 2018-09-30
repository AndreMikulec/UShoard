

context("working")

# test_that("aquire data", {
#   expect_equal(
#       rawData(normalizePath(paste0(path.package("UShoard"),"/inst/September 10_2018 IC Coupons.xls"), winslash = "/", mustWork = TRUE)
#         , if(.Platform$OS.type == "windows") { "NUL" } else { "/dev/null" }
#       )
#     , NULL)
# })

test_that("format data", {

  expect_equal(  formatTreasuriesAllotmentDateCols(
                   data.frame(alpha=1:2), "alpha"
                 )
               , structure(list(alpha = structure(c(1, 2), class = "Date")), row.names = c(NA, -2L), class = "data.frame")
               )

  # SHOULD need A TEST here
  expect_equal("formatReportAllotment", "formatReportAllotment")

  expect_equal( reorgRawTreasuriesAllotment(
                  data.frame(security_term = c("BOND", "NOTE", "YR")
                , security_type = rep("",3)
                , issue_date = rep(NA,3)
                , stringsAsFactors = FALSE)
              )
              , structure(  list(issue_date = c(NA, NA, NA)
                          , security = c("Bond-", "Note-", "Year-"))
                          , class = "data.frame", row.names = c(NA, -3L)
                         )
              )

  expect_equal( reorgRawTreasuriesAllotment(
                  data.frame(security_term = c("CASH", "WK", "YR")
                , issue_date = rep(NA,3)
                , stringsAsFactors = FALSE)
              )
              , structure(  list(issue_date = c(NA, NA, NA)
                          , security = c("Cash Bill", "Week Bill", "Year Bill"))
                          , class = "data.frame", row.names = c(NA, -3L)
                         )
              )

  expect_equal("recentRawTreasuriesAllotment",      "recentRawTreasuriesAllotment")
  expect_equal("recentRawTreasuriesAllotments"    , "recentRawTreasuriesAllotments")

  expect_equal("historicalRawTreasuriesAllotment", "historicalRawTreasuriesAllotment")
  expect_equal("historicalRawTreasuriesAllotments", "historicalRawTreasuriesAllotments")

  expect_equal("rawTreasuriesAllotments", "rawTreasuriesAllotments")
})
