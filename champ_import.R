library(ChAMP)

champ.import.local <- function(directory = getwd(),
                         offset = 100,
                         arraytype="450K")
{
  message("[===========================]")
  message("[<<<< ChAMP.IMPORT START >>>>>]")
  message("-----------------------------")
  
  message("\n[ Section 1: Read PD Files Start ]")
  if(!file.exists(directory) || is.na(file.info(directory)$isdir) || file.info(directory)$isdir == FALSE)
  {
    stop("  Your 'directory' for loading does not exists, please assign a correct directory.")
  }
  csvfile <- list.files(directory,recursive=TRUE,pattern="csv$",full.names=TRUE)
  if(length(csvfile) == 0) {
    stop(paste("  champ.import can not find any csv file in ",directory,". Please check your folder."))
  } else if (length(csvfile)>=2) {
    stop(paste("  champ.import finds more than one csv file in ",directory,". Please check your folder."))
  }
  
  message("  CSV Directory: ",csvfile)
  message("  Find CSV Success")
  
  message("  Reading CSV File")
  
  skipline <- which(substr(readLines(csvfile),1,6) == "[Data]")
  if(length(skipline)==0)
    suppressWarnings(pd <- read.csv(csvfile,stringsAsFactor=FALSE,header=TRUE))
  else
    suppressWarnings(pd <- read.csv(csvfile,skip=skipline,stringsAsFactor=FALSE,header=TRUE))
  
  if("Sentrix_Position" %in% colnames(pd))
  {
    colnames(pd)[which(colnames(pd)=="Sentrix_Position")] <- "Array"
    message("  Replace Sentrix_Position into Array")
  } else 
  {
    message("  Your pd file contains NO Array(Sentrix_Position) information.")
  }
  
  if("Sentrix_ID" %in% colnames(pd))
  {
    colnames(pd)[which(colnames(pd)=="Sentrix_ID")] <- "Slide"
    message("  Replace Sentrix_ID into Slide")
  } else 
  {
    message("  Your pd file contains NO Slide(Sentrix_ID) information.")
  }
  
  sapply(c("Pool_ID", "Sample_Plate", "Sample_Well"),function(x) if(x %in% colnames(pd)) pd[,x] <- as.character(pd[,x]) else message("  There is NO ",x, " in your pd file."))
  
  # GrnPath <- unlist(sapply(paste(pd$Slide,pd$Array,"Grn.idat",sep="_"), function(x) grep(x,list.files(directory,recursive=T,full.names=TRUE), value = TRUE)))
  # RedPath <- unlist(sapply(paste(pd$Slide,pd$Array,"Red.idat",sep="_"), function(x) grep(x,list.files(directory,recursive=T,full.names=TRUE), value = TRUE)))
  GrnPath <- list.files(directory,recursive=TRUE,pattern="*Grn*.idat*",full.names=TRUE)
  RedPath <- list.files(directory,recursive=TRUE,pattern="*Red*.idat*",full.names=TRUE)
  # browser()
  # if(!identical(names(GrnPath),paste(pd$Slide,pd$Array,"Grn.idat",sep="_")))
  #   stop("  Error Match between pd file and Green Channel IDAT file.")
  # if(!identical(names(RedPath),paste(pd$Slide,pd$Array,"Red.idat",sep="_")))
  #   stop("  Error Match between pd file and Red Channel IDAT file.")
  
  message("[ Section 1: Read PD file Done ]")
  
  message("\n\n[ Section 2: Read IDAT files Start ]")

  count <- 1
  G.idats <- lapply(GrnPath, function(x){ message("  Loading:",x," ---- (",which(GrnPath == x),"/",length(GrnPath),")");readIDAT(x)})
  count <- 1
  R.idats <- lapply(RedPath, function(x){ message("  Loading:",x," ---- (",which(RedPath == x),"/",length(RedPath),")");readIDAT(x)})
  
  names(G.idats) <- pd$Sample_Name
  names(R.idats) <- pd$Sample_Name
  
  checkunique <- unique(c(sapply(G.idats, function(x) nrow(x$Quants)),sapply(R.idats, function(x) nrow(x$Quants))))
  
  if(length(checkunique) > 1) 
  {
    message("\n  !!! Important !!! ")
    message("  Seems your IDAT files not from one Array, because they have different numbers of probe.")
    message("  ChAMP wil continue analysis with only COMMON CpGs exist across all your IDAt files. However we still suggest you to check your source of data.\n")
  }
  
  CombineIDAT <- append(G.idats, R.idats)
  commonAddresses <- as.character(Reduce("intersect", lapply(CombineIDAT, function(x) rownames(x$Quants))))
  
  message("\n  Extract Mean value for Green and Red Channel Success")
  GreenMean <- do.call(cbind, lapply(G.idats, function(xx) xx$Quants[commonAddresses, "Mean"]))
  RedMean <- do.call(cbind, lapply(R.idats, function(xx) xx$Quants[commonAddresses, "Mean"]))
  message("    Your Red Green Channel contains ",nrow(GreenMean)," probes.")
  
  G.Load <- do.call(cbind,lapply(G.idats,function(x) x$Quants[commonAddresses,"Mean"]))
  R.Load <- do.call(cbind,lapply(R.idats,function(x) x$Quants[commonAddresses,"Mean"]))
  
  message("[ Section 2: Read IDAT Files Done ]")
  
  message("\n\n[ Section 3: Use Annotation Start ]")
  
  message("\n  Reading ", arraytype, " Annotation >>")
  if(arraytype == "EPIC") data(AnnoEPIC) else data(Anno450K)
  
  message("\n  Fetching NEGATIVE ControlProbe.")
  control_probe <- rownames(Anno$ControlProbe)[which(Anno$ControlProbe[,1]=="NEGATIVE")]
  message("    Totally, there are ",length(control_probe)," control probes in Annotation.")
  control_probe <- control_probe[control_probe %in% rownames(R.Load)]
  message("    Your data set contains ",length(control_probe)," control probes.")
  rMu <- matrixStats::colMedians(R.Load[control_probe,])
  rSd <- matrixStats::colMads(R.Load[control_probe,])
  gMu <- matrixStats::colMedians(G.Load[control_probe,])
  gSd <- matrixStats::colMads(G.Load[control_probe,])
  
  rownames(G.Load) <- paste("G",rownames(G.Load),sep="-")
  rownames(R.Load) <- paste("R",rownames(R.Load),sep="-")
  
  IDAT <- rbind(G.Load,R.Load)
  
  message("\n  Generating Meth and UnMeth Matrix")
  
  message("    Extracting Meth Matrix...")
  M.check <- Anno$Annotation[,"M.index"] %in% rownames(IDAT)
  message("      Totally there are ",nrow(Anno$Annotation)," Meth probes in ",arraytype," Annotation.")
  message("      Your data set contains ",length(M.check), " Meth probes.")
  M <- IDAT[Anno$Annotation[,"M.index"][M.check],]
  
  message("    Extracting UnMeth Matrix...")
  U.check <- Anno$Annotation[,"U.index"] %in% rownames(IDAT)
  message("      Totally there are ",nrow(Anno$Annotation)," UnMeth probes in ",arraytype," Annotation.")
  message("      Your data set contains ",length(U.check), " UnMeth probes.")
  U <- IDAT[Anno$Annotation[,"U.index"][U.check],]
  
  if(!identical(M.check,U.check))
  {
    stop("  Meth Matrix and UnMeth Matrix seems not paried correctly.")
  } else {
    CpG.index <- Anno$Annotation[,"CpG"][M.check]
  }
  
  rownames(M) <- CpG.index
  rownames(U) <- CpG.index
  
  
  message("\n  Generating beta Matrix")
  BetaValue <- M / (M + U + offset)
  message("  Generating M Matrix")
  MValue <- log2(M/U)
  message("  Generating intensity Matrix")
  intensity <-  M + U
  
  message("  Calculating Detect P value")
  detP <- matrix(NA,nrow=nrow(intensity),ncol=ncol(intensity))
  rownames(detP) <- rownames(intensity)
  colnames(detP) <- colnames(intensity)
  
  type_II <- rownames(Anno$Annotation)[Anno$Annotation[,"Channel"] == "g+r"]
  type_II <- type_II[type_II %in% rownames(detP)]
  type_I.red <- rownames(Anno$Annotation)[Anno$Annotation[,"Channel"] == "r"]
  type_I.red <- type_I.red[type_I.red %in% rownames(detP)]
  type_I.grn <- rownames(Anno$Annotation)[Anno$Annotation[,"Channel"] == "g"]
  type_I.grn <- type_I.grn[type_I.grn %in% rownames(detP)]
  for(i in 1:ncol(detP))
  {
    detP[type_II,i] <- 1 - pnorm(intensity[type_II,i], mean=rMu[i]+gMu[i], sd=rSd[i]+gSd[i])
    detP[type_I.red,i] <- 1 - pnorm(intensity[type_I.red,i], mean=rMu[i]*2, sd=rSd[i]*2)
    detP[type_I.grn,i] <- 1 - pnorm(intensity[type_I.grn,i], mean=gMu[i]*2, sd=gSd[i]*2)
  }
  if(sum(is.na(detP))) message("    !!! There are NA values in your detP matrix.\n")
  
  
  message("  Counting Beads")
  NBeads <- do.call(cbind, lapply(R.idats, function(x) x$Quants[commonAddresses, "NBeads"]))
  Mbead <- NBeads[substr(Anno$Annotation$M.index[M.check],3,100),]
  Ubead <- NBeads[substr(Anno$Annotation$U.index[U.check],3,100),]
  Ubead[Ubead < 3 | Mbead < 3] <- NA
  rownames(Ubead) <- rownames(intensity)
  
  message("[ Section 3: Use Annotation Done ]")
  message("\n[<<<<< ChAMP.IMPORT END >>>>>>]")
  message("[===========================]")
  message("[You may want to process champ.filter() next.]\n")
  return(list("beta"=BetaValue,"M"=MValue,"pd"=pd,"intensity"=intensity,"detP"=detP,"beadcount"=Ubead,"Meth"=M,"UnMeth"=U))
}


readIDAT <- function(file, what = c("all", "IlluminaID", "nSNPsRead")) {
  
  ## Wrapper function to determine IDAT format and call appropriate reading routine.
  ## Currently this just checks the magic "IDAT" string and then reads the version number
  ## The file name is then passed and the file opened again by the read function.
  
  stopifnot(is.character(file) || length(file) != 0)
  file <- path.expand(file)
  if(!file.exists(file)) {
    stop("Unable to find file ", file)
  }
  
  if(grepl("\\.gz", file))
    con <- gzfile(file, "rb")
  else
    con <- file(file, "rb")
  on.exit({
    if (!is.null(con)) close(con)
  })
  what <- match.arg(what)
  
  ## Assert file format
  magic <- readChar(con, nchars=4)
  if (magic != "IDAT") {
    stop("Cannot read IDAT file. File format error. Unknown magic: ", magic)
  }
  
  ## Read IDAT file format version
  version <- readBin(con, what="integer", size=4, n=1, signed = TRUE, endian = "little")
  
  close(con)
  con <- NULL
  
  if (version == 1) {
    if(what != "all")
      stop("This file is encrypted. For encrypted files we need `what` equal to `all`.")
    res <- readIDAT_enc(file)
  }
  else if (version == 3) {
    res <- readIDAT_nonenc(file, what = what)
  }
  else {
    stop("Cannot read IDAT file. Unsupported IDAT file format version: ", version)
  }
  
  res
}


readIDAT_nonenc <- function(file, what = c("all", "IlluminaID", "nSNPsRead")) {
  readByte <- function(con, n=1, ...) {
    readBin(con, what="integer", n=n, size=1, endian="little", signed=FALSE)
  }
  
  readShort <- function(con, n=1, ...) {
    readBin(con, what="integer", n=n, size=2, endian="little", signed=FALSE)
  }
  
  readInt <- function(con, n=1, ...) {
    readBin(con, what="integer", n=n, size=4, endian="little", signed=TRUE)
  }
  
  readLong <- function(con, n=1, ...) {
    readBin(con, what="integer", n=n, size=8, endian="little", signed=TRUE)
  }
  
  readBytesToRead <- function(con, ...) {
    m <- readByte(con, n=1)
    n <- m %% 128
    shift <- 0L
    while (m %/% 128 == 1) {
      ## Read next length byte ...
      m <- readByte(con, n=1)
      ## ... which represents the next 7 hi-bits
      shift <- shift + 7L
      k <- (m %% 128) * 2^shift
      ## Total number of bytes to read
      n <- n + k
    }
    
    ## 'n' is a numeric; not an integer
    n
  }
  
  readString <- function(con, ...) {
    ## From [1] https://code.google.com/p/glu-genetics/source/browse/glu/lib/illumina.py#86:
    ## String data are encoded as a sequence of one or more length
    ## bytes followed by the specified number of data bytes.
    ##
    ## The lower 7 bits of each length byte encodes the bits that
    ## comprise the length of the following byte string.  When the
    ## most significant bit it set, then an additional length byte
    ## follows with 7 additional high bits to be added to the current
    ## length.  The following string lengths are accommodated by
    ## increasing sequences of length bytes:
    ##
    ## length  maximum
    ## bytes   length
    ## ------  --------
    ##   1       127 B
    ##   2        16 KB
    ##   3         2 MB
    ##   4       256 MB
    ##   5        32 GB
    ##
    ## While this seems like a sensible progression, there is some
    ## uncertainty about this interpretation, since the longest of
    ## string observed in the wild has been of length 6,264 with
    ## two length bytes.
    ##
    ## EXAMPLES: by HB (2015-09-11)
    ## Length   Len bytes  Iterations (n, m, k, shift)
    ## 1        (1)
    ## 127      (127)       -> n=128
    ## 128      (128,1)     -> n=0  ,m=1  ,shift=7, k=128 -> n=128
    ## 255      (255,1)     -> n=128,m=1  ,shift=7, k=128 -> n=255
    ## 256      (128,2)     -> n=0  ,m=2  ,shift=7, k=256 -> n=256
    ## 257      (129,2)     -> n=1  ,m=2  ,shift=7, k=256 -> n=257
    ## 512      (128,4)     -> n=0  ,m=4  ,shift=7, k=512 -> n=512
    ## 81921    (129,128,5) -> n=1  ,m=128,shift=7, k=0 -> n=1+0=1
    ##                      -> n=0  ,m=5  ,shift=14,k=81920
    ##                                              -> n=1+81920=81921
    
    ## Parse the number of characters to read
    n <- readBytesToRead(con)
    
    ## Read the data bytes
    readChar(con, nchars=n)
  }
  
  readUnknownBytes <- function(con, ...) {
    ## Parse the number of characters to read
    n <- readBytesToRead(con)
    
    ## Read the data bytes
    c(as.integer(n), readByte(con, n = n))
  }
  
  readField <- function(con, field) {
    switch(field,
           "IlluminaID" = readInt(con = con, n=nSNPsRead),
           "SD" = readShort(con = con, n=nSNPsRead),
           "Mean"= readShort(con = con, n=nSNPsRead),
           "NBeads" = readByte(con = con, n=nSNPsRead),
           "MidBlock" = {
             nMidBlockEntries <- readInt(con = con, n=1)
             MidBlock <- readInt(con = con, n=nMidBlockEntries)
           },
           "RedGreen" = readInt(con = con, n=1),
           "MostlyNull" = readString(con = con),
           "Barcode" = readString(con = con),
           "ChipType" = readString(con = con),
           "MostlyA" = readString(con = con),
           "Unknown.1" = readString(con = con),
           "Unknown.2" = readString(con = con),
           "Unknown.3" = readString(con = con),
           "Unknown.4" = readString(con = con),
           "Unknown.5" = readString(con = con),
           ## We don't know what 'Unknown.6' should be, but we know of at
           ## least one instance [2] where using readString() would trigger
           ## a warning about nulls in the string stemming from the byte
           ## sequence (1, 0). Because of this, we choose to read it as
           ## a byte sequence based of readUnknownBytes(), because it's
           ## the closest to readString() which we have used for years.
           ## We might change how this field is parsed in a future version
           ## when better understood what this field contains, e.g.
           ## it could be that readShort() should be used. /HB 2022-09-23
           ## [2] https://github.com/HenrikBengtsson/illuminaio/issues/21
           "Unknown.6" = readUnknownBytes(con = con),
           "Unknown.7" = readString(con = con),
           "RunInfo" = {
             nRunInfoBlocks <- readInt(con = con, n=1)
             naValue <- as.character(NA)
             RunInfo <- matrix(naValue, nrow=nRunInfoBlocks, ncol=5)
             colnames(RunInfo) <- c("RunTime", "BlockType", "BlockPars",
                                    "BlockCode", "CodeVersion")
             for (ii in seq_len(nRunInfoBlocks)) {
               for (jj in 1:5) {
                 RunInfo[ii,jj] <- readString(con = con)
               }
             }
             RunInfo
           },
           stop("readIDAT_nonenc: unknown field"))
  }
  
  if(! (is.character(file) || try(isOpen(file))))
    stop("argument 'file' needs to be either a character or an open, seekable connection")
  what <- match.arg(what)
  
  if(is.character(file)) {
    stopifnot(length(file) == 1)
    file <- path.expand(file)
    stopifnot(file.exists(file))
    fileSize <- file.info(file)$size
    if(grepl("\\.gz$", file))
      con <- gzfile(file, "rb")
    else
      con <- file(file, "rb")
    on.exit({
      close(con)
    })
  } else {
    con <- file
    fileSize <- 0
  }
  
  if(!isSeekable(con))
    stop("The file connection needs to be seekable")
  
  ## Assert file format
  magic <- readChar(con, nchars=4)
  if (magic != "IDAT") {
    stop("Cannot read IDAT file. File format error. Unknown magic: ", magic)
  }
  
  ## Read IDAT file format version
  version <- readLong(con, n=1)
  if (version < 3) {
    stop("Cannot read IDAT file. Unsupported IDAT file format version: ", version)
  }
  
  ## Number of fields
  nFields <- readInt(con, n=1)
  
  fields <- matrix(0L, nrow=nFields, ncol=3)
  colnames(fields) <- c("fieldCode", "byteOffset", "Bytes")
  for (ii in 1:nFields) {
    fields[ii,"fieldCode"] <- readShort(con, n=1)
    fields[ii,"byteOffset"] <- readLong(con, n=1)
  }
  
  ## The below stems from [1].  However, there is now also [3], which seems to
  ## have identified a few more of the 'Unknowns' fields. /HB 2022-09-23
  ## [3] https://github.com/bioinformed/glu-genetics/blob/dcbbbf67a308d35e157b20a9c76373530510379a/glu/lib/illumina.py#L44-L61
  knownCodes <- c(
    "nSNPsRead"  = 1000,
    "IlluminaID" =  102,
    "SD"         =  103,
    "Mean"       =  104,
    "NBeads"     =  107,
    "MidBlock"   =  200,
    "RunInfo"    =  300,
    "RedGreen"   =  400,
    "MostlyNull" =  401, # 'Manifest' [1,3]
    "Barcode"    =  402,
    "ChipType"   =  403,
    "MostlyA"    =  404, # 'Stripe' [1], 'label' [3]
    "Unknown.1"  =  405, # 'opa' [3]
    "Unknown.2"  =  406, # 'Sample ID' [1,3]
    "Unknown.3"  =  407, # 'descr' [3]
    "Unknown.4"  =  408, # 'Plate' [1,3]
    "Unknown.5"  =  409, # 'Well' [1,3]
    "Unknown.6"  =  410, 
    "Unknown.7"  =  510  # 'unknown' [1,3]
  )
  
  nNewFields <- 1
  rownames(fields) <- paste("Null", 1:nFields)
  for (ii in 1:nFields) {
    temp <- match(fields[ii,"fieldCode"], knownCodes)
    if (!is.na(temp)) {
      rownames(fields)[ii] <- names(knownCodes)[temp]
    } else {
      rownames(fields)[ii] <- paste("newField", nNewFields, sep=".")
      nNewFields <- nNewFields + 1
    }
  }
  
  stopifnot(min(fields[, "byteOffset"]) == fields["nSNPsRead", "byteOffset"])
  
  seek(con, where=fields["nSNPsRead", "byteOffset"], origin="start")
  nSNPsRead <- readInt(con, n=1)
  if(what == "nSNPsRead")
    return(nSNPsRead)
  if(what == "IlluminaID") {
    where <- fields["IlluminaID", "byteOffset"]
    seek(con, where = where, origin = "start")
    res <- readField(con = con, field = "IlluminaID")
    return(as.character(res))
  }
  
  res <- rownames(fields)
  names(res) <- res
  res <- res[order(fields[res, "byteOffset"])]
  res <- res[names(res) != "nSNPsRead"]
  res <- lapply(res, function(xx) {
    where <- fields[xx, "byteOffset"]
    seek(con, where = where, origin = "start")
    readField(con = con, field = xx)
  })
  
  Unknowns <- list(
    MostlyNull=res$MostlyNull,
    MostlyA=res$MostlyA
  )
  names <- grep("^Unknown[.]", names(res), value = TRUE)
  Unknowns[names] <- res[names]
  
  Quants <- cbind(res$Mean, res$SD, res$NBeads)
  colnames(Quants) <- c("Mean", "SD", "NBeads")
  rownames(Quants) <- as.character(res$IlluminaID)
  
  list(
    fileSize=fileSize,
    versionNumber=version,
    nFields=nFields,
    fields=fields,
    nSNPsRead=nSNPsRead,
    Quants=Quants,
    MidBlock=res$MidBlock,
    RedGreen=res$RedGreen,
    Barcode=res$Barcode,
    ChipType=res$ChipType,
    RunInfo=res$RunInfo,
    Unknowns=Unknowns
  )
}