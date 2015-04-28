# Clay Ford (jcf2d)

# Script to read in C636 screen data
# calculate ratios: protein/cell stain
# calculate z-scores
# calculate percent inhibition
# create plots to identify values beyond +/- 3 SDs

# install.packages("reshape2")
# install.packages("xlsx")
library(reshape2)
library(xlsx)

# Read and prepare data ---------------------------------------------------
dat <- read.xlsx("C636_screen_data.xlsx",sheetIndex = 1) # takes about 60 secs!
ORIG <- dat # back up so I don't have to run previous line in case I mess up
# fix column names
names(dat) <- c("LAB.plate","CB.plate","Signal","Well",
                "Virus.DMSO_1",
                paste0("C",2:11),
                "No.Virus_1",
                "Virus.DMSO_2",
                paste0("C",14:23),
                "No.Virus_2")
# drop row 1
dat <- dat[-1,]

# fill in plates and signals
# write function for this task:
fill <- function(x){
  for(i in seq_along(x)){
    if(!is.na(x[i])) tmp <- x[i]
    else x[i] <- tmp
  }
  x
}

dat$LAB.plate <- fill(dat$LAB.plate)
dat$CB.plate <- fill(dat$CB.plate)
dat$Signal <- fill(dat$Signal)

# remove "empty" rows
# identify by "" or NA
xr <- which(dat$Well=="" | is.na(dat$Well))
# out <- dat[xr,] # verify junk
dat <- dat[-xr,]

# sanity check: 32 rows for each lab plate
# all(table(dat$LAB.plate) == 32)

# clean up row numbers
row.names(dat) <- NULL

# clean up values, converted to factor due to Excel formatting
# ALERT: cell Q1188 AND U1534 contain "=#DIV/0!"
# dat[1055,17]
dat[,5:28] <- apply(dat[,5:28],2,function(x)as.numeric(as.character(x)))

# make lab plate and cb plate factors
dat$LAB.plate <- factor(dat$LAB.plate)
dat$CB.plate <- factor(dat$CB.plate)

# fix Signal, has three levels, should have two
dat$Signal <- factor(toupper(as.character(dat$Signal)))

# drop unused level from Well
dat$Well <- droplevels(dat$Well)



# Create ratios data frame ------------------------------------------------

# identify columns with all numbers
cols <- which(sapply(dat,is.numeric))

# split according to lab plate 
tmp <- split(dat,dat$LAB.plate)

# tmp[[1]][tmp[[1]]$Signal=="E PROTEIN",cols]/ tmp[[1]][tmp[[1]]$Signal=="CELL STAIN",cols]

# function to calculate ratio and retain lab plate and well IDs
calRatio <- function(x){
  rats <- x[x$Signal=="E PROTEIN",cols]/ x[x$Signal=="CELL STAIN",cols]
  data.frame(LAB.plate=x$LAB.plate[1:nrow(rats)],Well=x$Well[1:nrow(rats)],rats)
}
# calculate ratios
ratios <- lapply(tmp, calRatio)
# combine into data frame
ratios <- do.call(rbind,ratios)
row.names(ratios) <- NULL

# Calculate z-scores & percent inhibition ---------------------------------

cols <- which(sapply(ratios,is.numeric))
tmp <- split(ratios, ratios$LAB.plate)
# subtract mean of 1st and 13th column
zScore <- function(x){
  noDrug <- c(x$Virus.DMSO_1, x$Virus.DMSO_2)
  DMSLmean <- mean(noDrug, na.rm=T)
  DMSLsd <- sd(noDrug, na.rm=T)
  zs <- apply(x[,cols], 2, function(x)(x - DMSLmean)/DMSLsd)
  data.frame(LAB.plate=x$LAB.plate[1:nrow(zs)],Well=x$Well[1:nrow(zs)],zs)
}

zscores <- lapply(tmp,zScore)
zscores <- do.call(rbind,zscores)
row.names(zscores) <- NULL

# percent inhibition
PI <- function(x){
  meanVDMSO <- mean(c(x$Virus.DMSO_1, x$Virus.DMSO_2), na.rm=T)
  meanNoVirus <- mean(c(x$No.Virus_1, x$No.Virus_2), na.rm=T)
  pis <- apply(x[,cols],2,function(x)(meanVDMSO - x)/(meanVDMSO - meanNoVirus)*100)
  data.frame(LAB.plate=x$LAB.plate[1:nrow(pis)],Well=x$Well[1:nrow(pis)],pis)
}

PIs <- lapply(tmp, PI)
PIs <- do.call(rbind, PIs)
row.names(PIs) <- NULL

# tidy up
rm(tmp, cols)

# reshape zscores and PI to faciliate plotting
zscoresL <- melt(zscores, id.vars = 1:2)
row.names(zscoresL) <- NULL

PIL <- melt(PIs, id.vars = 1:2)
row.names(PIL) <- NULL


# tidy up
rm(zScore, calRatio, PI, zscores, PIs)

# remove the no virus rows
xx <- which(zscoresL$variable %in% c("No.Virus_1","No.Virus_2"))
zscoresL <- zscoresL[-xx,]
zscoresL$variable <- droplevels(zscoresL$variable)
PIL <- PIL[-xx,]
PIL$variable <- droplevels(PIL$variable)
rm(xx)
# save data for later use
save(zscoresL, PIL, dat, ratios, file="c636.RData")


# Points of interest ------------------------------------------------------

# view points of interest (absolute value > 2.99)
zscoresL[abs(zscoresL$value)>2.99,]

# percent inhibition >= 80%
PIL[abs(PIL$value)>=80,]

# Plots -------------------------------------------------------------------

# create a stripchart for a given lab plate
# num = the lab plate number
labPlot <- function(num){
  op <- par(mai=c(0.5,2,0.5,0.5))
  stripchart(value ~ variable, data=zscoresL, subset= LAB.plate==num, pch=1, jitter = T, las=1,
             main=paste("Lab Plate",num))
  abline(h=1:22,lty=3)  
  on.exit(op)
}

# do for all 64 lab plates
for(i in levels(zscoresL$LAB.plate)){
  labPlot(num=i)
}
