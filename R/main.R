
#' sets the enviroment
#'
#' space-saver - meant to be used at the beginning of a function
#'
#' @param init list with names entries of alernate/other options
#' @param envir where to return the options
#' @return environment is set
#' @examples
#' \dontrun{
#' # > options(max.print=88888L)
#' # initEnv()
#' # > getOption("max.print")
#' # [1] 99999
#' }
initEnv <- function(init = NULL, envir = parent.frame()) {
  tryCatchLog::tryCatchLog({
  require(data.table)
  require(xts)
  require(quantmod)

  eval(parse(text="assign(\"env\", environment())"), envir = parent.frame())

  assign("%>%", magrittr::`%>%`, envir = envir)

  assign("str_detect", stringr::str_detect, envir = envir)
  assign("str_c",           stringr::str_c, envir = envir)
  assign("str_replace",     stringr::str_replace, envir = envir)
  assign("str_replace_all", stringr::str_replace_all, envir = envir)

  assign("MoveFront", DataCombine::MoveFront, envir = envir)

  assign("as.Date", zoo::as.Date, envir = envir)

  assign("select",      dplyr::select, envir = envir)
  assign("mutate",      dplyr::mutate, envir = envir)
  assign("filter",      dplyr::filter, envir = envir)

  ops <- options()

  # tryCatchLog: what level to activate
  futile.logger::flog.threshold(futile.logger::ERROR)

  options(warn=2L)
  assign("ops", ops, envir = envir)

  invisible()

})}


#' unsets the enviroment
#'
#' space-saver - meant to be used at the beginning of a function
#'
#' @return environment is un-set
#' @examples
#' \dontrun{
#' # > uninitEnv()
#' # getOption("digits")
#' # [1] 5
#' }
uninitEnv <- function() {
  tryCatchLog::tryCatchLog({

  options(get("ops", envir = parent.frame()))
  invisible()

})}


#' Get data from the internet/filesystem
#'
#' Aquire data.
#'
#' @param url internet url or filesystem path
#' @param newLocalFileName current area location to save
#' @examples
#' \dontrun{
#'
#' # recent coupon auctions
#' # Coupon Auctions – Data from October 2009-present?
#' # url <- "https://home.treasury.gov/system/files/291/September%2010_2018%20IC%20Coupons.xls"
#' rawData(url = url, newLocalFileName = "recentCoupons.xls")
#' df <- as.data.frame(
#'       suppressWarnings(readxl::read_excel("recentCoupons.xls"
#'         , col_types = "guess", skip  = 4, guess_max = 1)
#'       ))
#' col_names <- c("issue_date", "security", "coupon_rate"
#'              , "cusip", "maturity_date", "total_issue"
#'              , "SOMA_issue", "institutions_issue", "individuals_issue"
#'              , "dealers_issue", "pensions_issue"
#'              , "investment_funds_issue", "foreign_issue","other_issue"
#'              )
#' colnames(df) <- col_names
#' df$issue_date    <- zoo::as.Date(df$issue_date)
#' df$maturity_date <- zoo::as.Date(df$maturity_date)
#' str(df, vec.len = 3)
#' # 'data.frame':	803 obs. of  14 variables:
#' #  $ issue_date            : Date, format: "2018-08-31" "2018-08-31" "2018-08-31" ...
#' #  $ security              : chr  "2-Year FRN" "2-Year Note" "5-Year TIPS Note" ...
#' #  $ rate                  : num  0.043 2.625 0.625 2.75 ...
#' #  $ cusip                 : chr  "912828Y53" "9128284Y3" "9128284H0" ...
#' #  $ maturity_date         : Date, format: "2020-07-31" "2020-08-31" "2023-04-15" ...
#' #  $ total_issue           : num  18.2 38.5 15 39.6 ...
#' #  $ soma_issue            : num  1.198 2.537 0.987 2.608 ...
#' #  $ institutions_issue    : num  0 0.001 0 0.07 0.07 0.00051 0 0 ...
#' #  $ individuals_issue     : num  0.0203 0.393 0.0351 0.056 ...
#' #  $ dealers_issue         : num  8.9 15.7 2.73 9.8 ...
#' #  $ pensions_issue        : num  0.217 0.025 0 0 ...
#' #  $ investment_funds_issue: num  3.72 15.37 10.52 23.49 ...
#' #  $ foreign_issue         : num  0.49 4.426 0.715 3.134 ...
#' #  $ other_issue           : num  3.65 0.08 0 0.444 ...
#' }
#' @export
rawData <- function(url = NULL, newLocalFileName = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(str_detect(url, "^http")) {
     # url <- URLdecode(URL = url)
    download.file(url = url, destfile = newLocalFileName, mode = "wb")
  } else {
     file.copy(from = url, to = newLocalFileName, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  }
  invisible()

})}


#' Get one recent Investor Class Auction Allotment
#'
#' @param treasClassUrl url of one single recent allotment
#' @examples
#' \dontrun{
#' recentRaw <- recentRawTreasuriesAllotment()
#' recentRaw
#' }
#' @export
recentRawTreasuriesAllotment <- function(treasClassUrl = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv();unlink(tmpf)})

  # recent coupon auctions
  # Coupon Auctions – Data from October 2009-present?
  if(is.null(treasClassUrl)) treasClassUrl <- "https://home.treasury.gov/system/files/291/September%2010_2018%20IC%20Coupons.xls"
  tmpf <- tempfile()
  rawData(url = treasClassUrl, newLocalFileName = tmpf)

  df <- as.data.frame(
        suppressWarnings(readxl::read_excel(tmpf, col_types = "guess", skip  = 4, guess_max = 1)
        ))                                # rate: action rate, coupon rate, or spread
  col_names <- c("issue_date", "security", "rate", "cusip"
               , "maturity_date", "total_issue", "SOMA_issue", "institutions_issue"
               , "individuals_issue", "dealers_issue", "pensions_issue"
               , "investment_funds_issue", "foreign_issue","other_issue"
               )
  colnames(df)     <- col_names
  df <- formatTreasuriesAllotmentDateCols(df, cols = c("issue_date","maturity_date"))

  df

})}


#' Get many recent Investor Class Auction Allotments
#'
#' @param treasList list() containting urls of recent allotments
#' @examples
#' \dontrun{
#' recents <- recentRawTreasuriesAllotment()
#' recents
#' }
#' @export
recentRawTreasuriesAllotments <- function(treasList = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(is.null(treasList)) treasList <- list("https://home.treasury.gov/system/files/291/September%2010_2018%20IC%20Coupons.xls"
                                          ,"https://home.treasury.gov/system/files/291/September%2010_2018%20IC%20Bills_1.xls")
  treasAllotments <- data.frame()
  for (treasList_i in treasList) {
     treasAllotments <- rbind(treasAllotments, recentRawTreasuriesAllotment(treasClassUrl = treasList_i))
  }

  treasAllotments

})}


#' Get one historical Investor Class Auction Allotment
#'
#' @param treasClassUrl url of one historical allotment
#' @examples
#' \dontrun{
#'  historical <- historicalRawTreasuriesAllotment()
#'  historical
#' }
#' @export
historicalRawTreasuriesAllotment <- function(treasClassUrl = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # recent coupon auctions
  # Coupon Auctions – Data from old to just before October?
  if(is.null(treasClassUrl)) treasClassUrl <- "https://www.treasury.gov/resource-center/data-chart-center/Documents/Website%20PDO-4-A-Coupons%20Jan%202000-Sep%202009.xls"
  tempfile <- tempfile()
  rawData(url = treasClassUrl, newLocalFileName = tempfile)

  df <- as.data.frame(
        suppressWarnings(readxl::read_excel(tempfile, col_types = "guess", skip  = 4, guess_max = 1)
        ))

  if(grepl("Coupons", treasClassUrl))  { security_type = "security_type" }
  if(grepl("Bills",    treasClassUrl)) { security_type = NULL } # rate: action rate, coupon rate, or spread
  col_names <- c("issue_date",  security_type, "security_term", "rate", "cusip"
               , "maturity_date", "total_issue", "SOMA_issue", "institutions_issue"
               , "individuals_issue", "dealers_issue", "pensions_issue"
               , "investment_funds_issue", "foreign_issue","other_issue"
               )
  colnames(df)     <- col_names

  df <- formatTreasuriesAllotmentDateCols(df, cols = c("issue_date","maturity_date"))

  df

})}



#' format Investor Class Auction Allotments date-ish columns
#'
#' change data columns to be of  R class Date
#'
#' @param treasuriesAllot Auction Allotments
#' @param cols columns in a character vector, to change to type Date
#' @examples
#' \dontrun{
#' formatted <- formatTreasuriesAllotmentDateCols(
#'                 data.frame(alpha=1:2), "alpha"
#'               )
#' formatted
#' }
#' @export
formatTreasuriesAllotmentDateCols <- function(treasuriesAllot = NULL, cols = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(!length(cols)) stop("To formatTreasuriesAllotmentDateCol on must pass cols (column names)")

  for(cols_i in cols) {
    treasuriesAllot[ , cols_i] <- as.Date(treasuriesAllot[ , cols_i] )
  }

  treasuriesAllot

})}



#' reorganize Investor Class Auction Allotments
#'
#' for 'old' Class Auction Allotments, change 'security_type' and/or 'security_term'
#' to be just (condensed to one column) 'security' with proper naming
#' compatible with the 'new'(er) Class Auction Allotments
#'
#' @param treasAllotment treasury allotmetn data.frame to be reorganized
#' @examples
#' \dontrun{
#' reorgedTreasury1 <-  reorgRawTreasuriesAllotment(
#'                data.frame(security_term = c("BOND", "NOTE", "YR")
#'              , security_type = rep("",3)
#'              , issue_date = rep(NA,3)
#'              , stringsAsFactors = FALSE)
#'            )
#' reorgedTreasury1
#'
#' reorgedTreasury2 <-  reorgRawTreasuriesAllotment(
#'                data.frame(security_term = c("CASH", "WK", "YR")
#'              , issue_date = rep(NA,3)
#'              , stringsAsFactors = FALSE)
#'            )
#' reorgedTreasury2
#' }
#' @export
reorgRawTreasuriesAllotment <- function(treasAllotment = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

    if(any(colnames(treasAllotment) %in% c("security_type", "security_term"))) {
      if(all(c("security_type", "security_term") %in% colnames(treasAllotment))) {
         treasAllotment
         with( treasAllotment, {
           security <- str_c(security_term,  " ", security_type)
           str_replace(security, "NOTE", "Note") %>%
             str_replace("BOND", "Bond")            %>%
               str_replace("YR",  "Year")             %>%
                 str_replace_all("-", " ")              %>%
                   str_replace(" ", "-")                  -> # first space only
                     security
        }) -> security
      } else if(all(c("security_term") %in% colnames(treasAllotment))) {
         with( treasAllotment, {
           str_replace(security_term, "CASH", "Cash") %>%
             str_replace("WK", "Week")                  %>%
               str_replace("YR",  "Year")                 %>%
                str_replace(" ", "-")                        %>% # first space only
                  str_replace("$", " Bill")                    ->
                     security
        }) -> security
      }

    }

    within(treasAllotment, {
        suppressWarnings(rm("security_type", "security_term"))
        assign("security", security)
    }) %>%
    MoveFront(c("issue_date", "security")) -> treasAllotment

     treasAllotment

})}



#' Get historical Investor Class Auction Allotments
#'
#' @param treasList list() containting urls of historical allotments
#' @param reorgRawTreasuriesAllotmentFn
#' @examples
#' \dontrun{
#'  rawHistoricals <- historicalRawTreasuriesAllotments()
#'  rawHistoricals
#' }
#' @export
historicalRawTreasuriesAllotments <- function(treasList = NULL, reorgRawTreasuriesAllotmentFn = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(is.null(treasList)) treasList <- list("https://www.treasury.gov/resource-center/data-chart-center/Documents/Website%20PDO-4-A-Coupons%20Jan%202000-Sep%202009.xls"
                                          ,"https://www.treasury.gov/resource-center/data-chart-center/Documents/Website%20IC%20allotments---Bills--Aug%202001-Sep%202009.xls")
  treasAllotments <- data.frame()
  for (treasList_i in treasList) {

    if(is.null(reorgRawTreasuriesAllotmentFn)) reorgRawTreasuriesAllotmentFn <- reorgRawTreasuriesAllotment
    historicalRawTreasuriesAllotment(treasClassUrl = treasList_i) %>%
       reorgRawTreasuriesAllotmentFn %>%
         { rbind(treasAllotments,. , stringsAsFactors = FALSE)} ->
          treasAllotments

  }

  treasAllotments

})}


#' Get Investor Class Auction Allotments
#'
#' Get both historical and recent Investor Class Auction Allotments
#'
#' @param treasuriesAllotments character vector of function names
#' @examples
#' \dontrun{
#' rawTreasuries <- rawTreasuriesAllotments()
#' rawTreasuries
#' }
#' @export
rawTreasuriesAllotments <- function(treasuriesAllotments = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(is.null(treasuriesAllotments)) treasuriesAllotments <- c("historicalRawTreasuriesAllotments"
                                                            , "recentRawTreasuriesAllotments")

  allotments <- data.frame()
  for (treasuriesAllotments_i in treasuriesAllotments) {
    rawTreasuriesAllotments_i <- do.call(treasuriesAllotments_i,list())

    allotments <- rbind(allotments, rawTreasuriesAllotments_i, stringsAsFactors = FALSE)

  }

  allotments

})}


#' Quantmod internal conversion
#'
#'
#' @param fr data.frame of information: left most column is the 'time' column
#' @param return.class call of the object sent back to the user
#' @examples
#' \dontrun{
#' quantmod___convert.time.series <- "rawTreasuriesAllotments()""
#' }
quantmod___convert.time.series <- function (fr, return.class) {
    tryCatchLog::tryCatchLog({
    initEnv();on.exit({uninitEnv()})
    if ("quantmod.OHLC" %in% return.class) {
        class(fr) <- c("quantmod.OHLC", "zoo")
        return(fr)
    }
    else if ("xts" %in% return.class) {
        return(fr)
    }
    if ("zoo" %in% return.class) {
        return(as.zoo(fr))
    }
    else if ("ts" %in% return.class) {
        fr <- as.ts(fr)
        return(fr)
    }
    else if ("data.frame" %in% return.class) {
        fr <- as.data.frame(fr)
        return(fr)
    }
    else if ("matrix" %in% return.class) {
        fr <- as.data.frame(fr)
        return(fr)
    }
    else if ("timeSeries" %in% return.class) {
        if (requireNamespace("timeSeries", quietly = TRUE)) {
            fr <- timeSeries::timeSeries(coredata(fr), charvec = as.character(index(fr)))
            return(fr)
        }
        else {
            warning(paste("'timeSeries' from package 'timeSeries' could not be loaded:",
                " 'xts' class returned"))
        }
    }
})}



#' format df in preparation for dplyr (future)selection
#'
#' @param df dplyr compatabile input type
#' @param AllotCat alotment category point of view, either issue "ISS" or maturity "MTR"
#' @param AllotmentCol 'issue_date" xor 'maturity_date"
#' @param unAllotmentCol 'maturity_date' xor 'issue_date"
#' @examples
#' \dontrun{

#' NEED one
#'
#' }
formatReportAllotment <- function(df, AllotmentCat, AllotmentCol, unAllotmentCol ) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  select(df, - "cusip", - unAllotmentCol) %>%
    mutate( security = str_c(AllotmentCat, security) ) %>%
      MoveFront(c("security", AllotmentCol, "rate")) -> df

  mutate(df, security = toupper(security)
         , security = str_c("TR", security)
         , security = str_replace_all(security, "[ ]+", "")
         , security = str_replace_all(security,   "-+", "")
      ) -> df
  df

})}



#' Aquire Bills and Bonds allotment data from the US treasury
#'
#' @param Symbols One or more treasury 'issue' or 'maturity' bills/bonds symbols
#' @param env possibly returned data is loaded into this environment
#' @param return.class call of the object sent back to the user
#' @examples
#' \dontrun{
#' library(quantmod)
#'
#' ## issue dates
#' treasuryTRISS10YEARNOTE <- getSymbols("TRISS10YEARNOTE", src = "treasury", auto.assign = FALSE)
#' head(treasuryTRISS10YEARNOTE)
#'
#' ## maturity dates
#' treasuryTRMTR10YEARNOTE <- getSymbols("TRMTR10YEARNOTE", src = "treasury", auto.assign = FALSE)
#' head(treasuryTRMTR10YEARNOTE)
#'
#' }
#' @export
getSymbols.treasury <- function (Symbols, env, return.class = "xts", ...) {
    tryCatchLog::tryCatchLog({
    initEnv();on.exit({uninitEnv()})

    importDefaults("getSymbols.treasury")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    treasuries <- rawTreasuriesAllotments()
    issuesCreated     <- FALSE
    maturitiesCreated <- FALSE

    for (i in 1:length(Symbols)) {
        if (verbose)
            cat("processing ", Symbols[[i]], ".....\n\n")
        default.return.class <- return.class

        if(str_detect(Symbols[[i]], "^TRISS")) {
          if(!issuesCreated) {
            formatReportAllotment(treasuries, "ISS", "issue_date", "maturity_date") %>%
              as.data.table(na.rm= FALSE) %>%
                setkey(security) -> issues
            issuesCreated <- TRUE
            }
          treasuries_i <- issues
        } else
        if(str_detect(Symbols[[i]], "^TRMTR")) {
          if(!maturitiesCreated) {
            formatReportAllotment(treasuries, "MTR", "maturity_date", "issue_date") %>%
              as.data.table(na.rm= FALSE) %>%
                setkey(security) -> maturities
            maturitiesCreated <- TRUE
            }
          treasuries_i <- maturities
        }
        else { stop(str_c("getSymbols.treasury has an invalid Symbol: ", Symbols[[i]])) }
        filter(treasuries_i, security == Symbols[[i]]) %>%  select(- "security") -> fr
        colnames(fr) <- str_c(Symbols[[i]], ".", colnames(fr))
        fr <- xts(as.matrix(fr[, -1]), as.Date(fr[, 1], origin = "1970-01-01"),
            src = "treasury", updated = Sys.time())
        colnames(fr) <- toupper(colnames(fr))
        fr <- quantmod___convert.time.series(fr = fr, return.class = return.class)
        if (auto.assign)
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign)
        return(Symbols)
    return(fr)
})}




