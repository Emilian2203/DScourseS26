dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
install.packages('jsonlite', repos='http://cran.rstudio.com/', lib=Sys.getenv("R_LIBS_USER"))
library(jsonlite, lib.loc=Sys.getenv("R_LIBS_USER"))

system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

system('cat dates.json')

mylist <- fromJSON('dates.json')
mydf <- do.call(rbind, mylist$result[-1])

class(mydf)
class(mydf$date)

head(mydf)


