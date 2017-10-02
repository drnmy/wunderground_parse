library(httr)
library(XML)
library(RCurl)
library(rvest)

getTempData <- function(code, year) {

 url <- sprintf("https://www.wunderground.com/history/airport/%s/%d/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=%d&req_city=NA&req_state=NA&req_statename=NA", code, year, year)
 
 print(url)
 
 webpage <- getURL(url)
 
 webpage <- readLines(tc <- textConnection(webpage)); close(tc)
 
 pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
 
 # parse the tree by tables

 tablehead <- xpathApply(pagetree, "//table[contains(@id,'obsTable')]/thead/tr/th", xmlValue)
 
 results <- xpathApply(pagetree, "//table[contains(@id,'obsTable')]/tbody/tr/td", xmlValue)
 
 results = gsub("\n", "", results)

 # Convert character vector to dataframe
 content <- as.data.frame(matrix(results, ncol = 21, byrow = TRUE))
 
 colnames(content) <- c("Date",	"t_high", "t_avg", "t_low",
                        "dp_high",	"dp_avg", "dp_low",	
                        "h_high",	"h_avg", "h_low",	
                        "hpa_high",	"hpa_avg", "hpa_low",
                        "v_high",	"v_avg",	"v_low",
                        "w_high",	"w_avg",	"w_high",	
                        "prcp_sum", "evt")
 
 return(content)
 
}

monthFromText <- function(theText)
{
  Month <- 1
  if (theText == "Jan") {
    Month = 1
  } else
    if (theText == "Feb") {
      Month = 2
    } else
      if (theText == "Mar") {
        Month = 3
      } else
        if (theText == "Apr") {
          Month = 4
        } else
          if (theText == "May") {
            Month = 5
          } else
            if (theText == "Jun") {
              Month = 6
            } else
              if (theText == "Jul") {
                Month = 7
              } else
                if (theText == "Aug") {
                  Month = 8
                } else
                  if (theText == "Sep") {
                    Month = 9
                  } else
                    if (theText == "Oct") {
                      Month = 10
                    } else
                      if (theText == "Nov") {
                        Month = 11
                      } else
                        if (theText == "Dec") {
                          Month = 12
                        }
    return (Month)                      
}

fixDates <- function(df, year)
{
  ret <- data.frame()
  validMonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                   "Nov", "Dec")
  mnt <- 1
  for (i in 1:nrow(df)) {
    row <- df[i,]
    
    if (row$Date %in% validMonths)
    {
      mnt <- monthFromText(row$Date)
    } else {
      row$Date <- sprintf("%d-%d-%d", year, mnt, as.numeric(as.character(row$Date)))
      ret <- rbind(ret, row)
    }
    # do more things with the data frame...
  } 
  return(ret)
}

location <- "LRIA"

data2000 <- getTempData(location, 2000)
data2000 <- fixDates(data2000, 2000)

data2001 <- getTempData(location, 2001)
data2001 <- fixDates(data2001, 2001)

data2002 <- getTempData(location, 2002)
data2002 <- fixDates(data2002, 2002)

data2003 <- getTempData(location, 2003)
data2003 <- fixDates(data2003, 2002)

data2004 <- getTempData(location, 2004)
data2004 <- fixDates(data2004, 2004)

data2005 <- getTempData(location, 2005)
data2005 <- fixDates(data2005, 2005)

data2006 <- getTempData(location, 2006)
data2006 <- fixDates(data2006, 2006)

data2007 <- getTempData(location, 2007)
data2007 <- fixDates(data2007, 2007)

data2008 <- getTempData(location, 2008)
data2008 <- fixDates(data2008, 2008)

data2009 <- getTempData(location, 2009)
data2009 <- fixDates(data2009, 2009)

data2010 <- getTempData(location, 2010)
data2010 <- fixDates(data2010, 2010)

data2011 <- getTempData(location, 2011)
data2011 <- fixDates(data2011, 2011)

data2012 <- getTempData(location, 2012)
data2012 <- fixDates(data2012, 2012)

data2013 <- getTempData(location, 2013)
data2013 <- fixDates(data2013, 2013)

data2014 <- getTempData(location, 2014)
data2014 <- fixDates(data2014, 2014)

data2015 <- getTempData(location, 2015)
data2015 <- fixDates(data2015, 2015)

data2016 <- getTempData(location, 2016)
data2016 <- fixDates(data2016, 2016)

total    <- rbind(data2000, data2001, data2002, data2003, data2004, data2005, data2006, 
                  data2007, data2008, data2009, data2010, data2011, data2012, data2012,
                  data2013, data2014, data2015, data2016)

write.csv(total, file="I:/2017/Iasi/LRIA.csv")
