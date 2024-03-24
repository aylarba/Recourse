# tricks

## setwd to source file path ####
selfwdJS <- function() setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load multiple packages ----
invisible(lapply(c('data.table','XML','xml2','openxlsx'), library, character.only = TRUE))

## bi-intersect #####
biintersectJS <- function(x,y) c(length(setdiff(x,y)),length(setdiff(y,x)))

## moving files ####
filesstrings::file.move()

## unrowname ####
unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

## unfactor ####
unfactor <- function(df){
  id <- sapply(df, is.factor)
  df[id] <- lapply(df[id], as.character)
  df
}

### pc paste code #####
pc <- function(){
  L <- readLines(pipe("pbpaste"))
  rx <- "^[[:blank:]]*[^>+[:blank:]]*[>+]"
  is.cmd <- grepl(rx, L)
  L[is.cmd] <- gsub(paste(rx, "?"), "", L[is.cmd])
  L[!is.cmd] <- paste("#", L[!is.cmd])
  for(el in L) cat(el, "\n")
  invisible(L)
}


###### LISTS and DATA.TABLE ########
## convert a list of characters into a list of lists for rbindlist ----
l0 <- list(
  CASE=list(
      id='id1',
      details=list(name='bla',date='11/03/20'),
      report=list(
          section=list(
            TITLE = 'Dx',TEXT='asldkjl'
          ),
          section=list(
            TITLE = 'Sp',TEXT='eye'
          )
      )),
  CASE=list(

      id='id2',
      details=list(name='bla',date='11/03/20'),
      report=list(

          section=list(
            TITLE = 'Dx',TEXT='asldkjl'
          ),
          section=list(
            TITLE = 'Sx',TEXT='pain'
          )
        
      ))
)
l3 <- lapply(1:length(l0),function(x) {
  nms <- unname(sapply(l0[[x]][[3]],'[',"TITLE"))
  txt <- unname(sapply(l0[[x]][[3]],'[',"TEXT"))
  names(txt) <- nms
  txt
})
dt <- rbindlist(l3,fill=T)


## Function to initialize function defaults after coping from function code ####
# make sure the last character is a hard return
ddd <- function(){
  y <- unlist(strsplit(readLines(pipe("pbpaste")),","))
  y <- paste(y[grepl("=",y)],collapse=";")
  eval(parse(text=y),envir = 0)
  cat(y)
}



### functions of NULL or all NA yield NULL #####
minnull=function(x,...){
  if(is.null(na.omit(x)) | !length(na.omit(x))) return(NULL)
  return(min(x,...))
}
maxnull=function(x,...){
  if(is.null(na.omit(x)) | !length(na.omit(x))) return(NULL)
  return(max(x,...))
}
meannull=function(x,...){
  if(is.null(na.omit(x)) | !length(na.omit(x))) return(NULL)
  return(mean(x,...))
}
## appends NA row to data.frame #####
dfAppend=function(x){
  y=x[1,]
  y[!is.na(y)]<-NA
  rbind(x,y)
}
### function to rename objects #####
reName=function(subs,oldn,newn,delete=F){
  xlist=ls(pattern = subs,envir=.GlobalEnv)
  for(x in xlist){
    assign(gsub(oldn,newn,x),get(x),envir=.GlobalEnv)
  }
  if(delete) rm(list=xlist,envir=.GlobalEnv)
}

## #find repeat columns ####
#i=1
df[paste0(colnames(df[1]),".avg")]=rowMeans(df[,which(colnames(df) %in% colnames(df)[1])])
colnames(df)[which(colnames(df) %in% colnames(df)[1])]

#### zip columns #####
zipcols=function(df){
  a=character()
  for(i in 1:nrow(df)){
    for(j in 1:ncol(df)) a=c(a,df[i,j])
  }
  return(a)
}


##### number of bins #####
nbins = function(X,scv1=0.1) length(geomSeq(min(X,na.rm=T),max(X,na.rm=T),scv1))-1

####### geomSeq ####
geomSeq = function(start,end,ratio){
  if(end<start | ratio<=0) {
    print("invalid parameter")
    break
  }
  x=start
  result=numeric()
  result=start
  repeat{
    x*(1+ratio)->x
    result=c(result,x)
    if(x>end) break
  }
  return(result)
}

##### GRAPHIC FUNCTIONS #######
###### add alpha ####
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


#### STAT FUNCTIONS #####
##### lognormal functions #####
# ntol.mean=function(x) {log(mean(x))-0.5*log(1+(sd(x)/mean(x))**2)} # same as mean(log(x))
# ntol.sd=function(x) {sqrt(log(1+(sd(x)/mean(x))**2))} # same as sd(log(x))
ntol.mean=function(m,s) {log(m)-0.5*log(1+(s/m)**2)}
ntol.sdcv=function(cv) sqrt(log(1+cv**2))
ntol.cv=function(m,s) {ntol.sdcv(s/m)/ntol.mean(m,s)}
lton.mean=function(m,s) {exp(m+0.5*s**2)}
lton.cv=function(s) {sqrt(exp(s**2)-1)}
lton.sd=function(m,s) {lton.mean(m,s)*lton.cv(s)}
# this function converts rcv based on N distribution to rcv based on lognormal distribution, as expressed in normal units ####
ntol.rcv=function(rcvn,zpn=qnorm(1-0.05/2),zpl=qnorm(1-0.05/2)) {
  list(rcvl.dn=exp(-sqrt(2)*zpl*sqrt(log((rcvn/(sqrt(2)*zpn))**2+1)))-1,
       rcvl.up=exp(+sqrt(2)*zpl*sqrt(log((rcvn/(sqrt(2)*zpn))**2+1)))-1)
}
# this function converts rcv based on lognormal distribution to rcv based on N distribution, both expressed in normal units ####
lton.rcv = function(rcvl,zpn=qnorm(1-0.05/2),zpl=qnorm(1-0.05/2)) {
  sqrt(exp((log(rcvl+1)/sqrt(2)/zpl)**2)-1)*zpn*sqrt(2)}

EnvStats::geoMean()
EnvStats::geoSD()


# Additional data.table tricks #####
### set column order ####
setcolorder(mydt, c("colB", "colC")) # colB now in position 1 and colC in position 2
### filter by condition ####
# In some cases setkey(mydt, colA, colB) will speed performance # for logical tests on colA and colB; same with other columns
mydt2 <- mydt[`logical expression`]
### Filter rows where colA equals string1 or string2	####
mydt2 <- mydt[colA %chin% c("string1", "string2")]  # %chin% has no speed advantage over %in% when options(data.table.auto.index = T)
### Filter rows where colA matches a regular expression
mydt2 <- mydt[colA %like% "mypattern"]	
myt2 <- filter(myt, stringr::str_detect(colA, "mypattern")) # tidyverse alternative
### Filter rows where colA values are between 2 numbers	####
mydt2 <- mydt[colA %between% c(n1, n2)]	
### Filter for first n rows by group	####
mydt2 <- mydt[, .SD[1:n], by = groupcol]	
### Filter rows for maximum value by group	####
mydt2 <- mydt[, .SD[which.max(valcol)], by = groupcol]
### Exclude multiple columns	#### 
mydt2 <- mydt[, -c("colA", "colB")] #OR
mydt2 <- mydt[, !c("colA", "colB")] #OR
my_col_names <- c("colA", "colB")
mydt2 <- mydt[, !..my_col_names]
### Remove duplicate rows based on values in multiple columns	####
mydt2 <- unique(mydt, by = c("colA", "colB")) 
#### Count unique rows based on multiple columns ####
uniqueN(mydt, by = c("colA", "colB")) # OR
mydt[,uniqueN(.SD,by=c('colA','colB'))]
### Summarize multiple columns by group and return results in multiple columns ####
mydt2 <- mydt[
  , lapply(.SD, myfun),
  .SDcols = c("colA", "colB"), by = groupcol]
### Add multiple columns at once	####
# use any function or expression
mydt[, `:=`(NewCol1 = myfun(colA), NewCol2 = colB + colC )] #OR
mydt[, c("NewCol1", "newCol2") := list(myfun(colA), colB + colC)]
### Add column with row ID numbers by group (group index)	####
mydt[, myid := 1:.N, by = groupcol]
### Add column based on several conditions without using multiple if else statements (like SQL's CASE) ####
mydt2 <- mydt[, NewCol := fcase(
  condition1, "Value1",
  condition2, "Value2",
  condition3, "Value3",
  default = "Other" # value for all else
)]
### Append rows to an existing CSV file	####
fwrite(mydt2, "myfile.csv", append = TRUE, compress = 'gzip')

### pivot table like SQL ####
#### cube ####
cubed <- data.table::cube(
  flights,
  .(distance = sum(distance)),
  by = c("month", "origin")
)
# gives usual by aggregation + subtotals by each group (month, origin) + grand total
# using dcast gets subtotals like contingency table
dcast(cubed, origin ~ month,  value.var = "distance")
#### subtotals controlled by groupingsets ####
# e.g. to give only the subtotals per combinations of 2 dimensions:
groupingsets(
  flights,
  j = .(distance = sum(distance)),
  by = c("month", "origin", "carrier"),
  sets = list(
    c("month", "origin"),
    c("month", "carrier"),
    c("origin", "carrier")
  )
)
#### rollup is another shortcut for combinations from rigth to left ####
rollup(flights, sum(distance),  by = c("month", "origin", "carrier"))
# same as
groupingsets(
  flights,
  j = .(distance = sum(distance)),
  by = c("month", "origin", "carrier"),
  sets = list(
    c("month", "origin", "carrier"),
    c("month", "origin"),
    c("month"),
    character(0)
  )
)
### fcoalesce - finds missing data in each row by looking into the first non-NA element in other columns
dt <- data.table(a <- c(1,  NA,  NA, 4, NA),
                 b <- c(NA, NA, NA, 5, 6),
                 c <- c(7,  8,  NA, 9, 10))
fcoalesce(dt)
## data.table operators 
### between ####
dt <- data.table(a=1:5, b=6:10, c=c(5:1))
dt[a %between% c(3,8)]
dt[between(a,3,8,incbounds = F)]
dt[between(c,a,b)] # vectorized
dt[c %between% list (a,b)]
### range asks if x is in any of the ranges in range ####
range <-data.table(start=c(1:3),end=c(4,4,4))
dt[c %inrange% range]
### like, ilike, flike ####
DT = data.table(Name=c("Mary","George","Martha"), Salary=c(2,3,4))
DT[Name %like% "^Mar"] # case sensitive, anywhere in word (unless ^ is used)
DT[Name %ilike% "mar"] # case insensitive (ignore.case=T), anywhere
DT[Name %flike% "Mar"] # case sensitive, fixed = T, anywhere

### TRUTHY FUNCTIONS ####
truthy <- function(x,y=FALSE)
  ifelse(shiny::isTruthy(x),x,y)
truthyNA <- function(x)
  ifelse(shiny::isTruthy(x),x,NA)
truthyNULL <- function(x)
  if(shiny::isTruthy(x))
    x else NULL
TruthyCols <- function(dt){
  keepcols <- apply(dt,2,function(x) any(shiny::isTruthy(x)))
  keepcols <- names(keepcols[keepcols])
  dt[,..keepcols]
}
