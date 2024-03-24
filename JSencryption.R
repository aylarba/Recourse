# write encrypted data frame to file
write.aes <- function(df,filename, key) {
  require(digest)
  zz <- textConnection("out","w")
  write.csv(df,zz, row.names=F)
  close(zz)
  out <- paste(out,collapse="\n")
  raw <- charToRaw(out)
  raw <- c(raw,as.raw(rep(0,16-length(raw)%%16)))
  aes <- AES(key,mode="ECB")
  aes$encrypt(raw)
  writeBin(aes$encrypt(raw),filename)  
}
# read encypted data frame from file
read.aes <- function(filename,key) {
  require(digest)
  dat <- readBin(filename,"raw",n=1000)
  aes <- AES(key,mode="ECB")
  raw <- aes$decrypt(dat, raw=TRUE)
  txt <- rawToChar(raw[raw>0])
  read.csv(text=txt)
}   

JSaes.encrypt <- function(df,key){
  require(digest)
  raw <- charToRaw(paste(df,collapse='\n'))
  raw <- c(raw,as.raw(rep(0,16-length(raw)%%16)))
  aes <- AES(key,mode="ECB")
  enc <- aes$encrypt(raw)
  paste(enc,collapse="")
}

JSaes.decrypt <- function(enc,key){
  enc=hex2raw(enc)
  require(digest)
  aes <- AES(key,mode="ECB")
  aes$decrypt(enc)
}




hex2raw <- function(hex) {
  if(!(is.character(hex) || (is.list(hex) &&
                             all(vapply(X = hex, FUN = is.character, FUN.VALUE = logical(1)))))) {
    stop("hex must be a character string or character vector")
  }
  if(is.list(hex) || (length(hex) > 1 &&
                      all(vapply(X = hex, FUN = nchar, FUN.VALUE = integer(1)) > 2))) {
    lapply(hex, .hex2raw)
  } else {
    .hex2raw(hex)
  }
}

.hex2raw <- function(hex) {
  hex <- gsub("[^0-9a-fA-F]", "", hex)
  if(length(hex) == 1) {
    if(nchar(hex) < 2 || nchar(hex) %% 2 != 0) {
      stop("hex is not a valid hexadecimal representation")
    }
    hex <- substring(hex, seq(1, nchar(hex), 2), seq(2, nchar(hex), 2))
  }
  if(!all(vapply(X = hex, FUN = nchar, FUN.VALUE = integer(1)) == 2)) {
    stop("hex is not a valid hexadecimal representation")
  }
  as.raw(as.hexmode(hex))
}