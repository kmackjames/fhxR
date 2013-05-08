#   CONTENTS:
#   1: order.fhx() - used in other functions
#   2: xlsx.to.fhx() - read in fire scar data
#   3: fhx() - read in datdets file for establishment data
#   4: +.fhx() - combine the two
#   5: get.fire.years - get the list of years in which there were fires
#   6: ggplot.fhx() - plot the fire diagram
#   * other functions not used here are in fhsR_working_other.r


require("ggplot2")
require("plyr")
require("reshape")
require("xlsx")


#################################
# 1 #############################
###### START FUNCTION order.fhx

order.fhx <- function(x) {
  stopifnot(class(x) == "fhx")
	test <- subset(x$rings,x$rings$type == "inner.year" | x$ring$type == "pith.year")
	#stopifnot(nrow(test)==length(x$series.names) # there should be one inner or pith year for each series
	i <- order(test$year, decreasing = FALSE) #line up series by their start year
	# decreasing was TRUE in original - ordered opposite to what we want
	x$rings$series <- factor(x$rings$series,
													 levels = unique(test$series[i]),
													 ordered = TRUE) #this appears not to change the order of the actual data
	x$series.names <- levels(x$rings$series)  # Push the ordering to series.names
	i <- order(x$rings$series, x$rings$year, decreasing = FALSE) #was TRUE in original - ordered opposite to what we want
	x$rings <- x$rings[i, ] #now $rings is ordered with each series's years going from the present backwards
	x
}

###### END OF FUNCTION order.fhx
##################################



#################################
# 2 #############################
###### START FUNCTION xlsx.to.fhx

xlsx.to.fhx <- function(fires,siteName=NULL,serieschar=8,distill=FALSE,dist.thresh=1) {
	# Read excel file and return an fhx object
	# Input: 
	#		fires - path of excel file containing fire data
	#			 make sure there are NO empty columns
	#   siteName - name of worksheet in the xlsx, e.g. "c88"
	#   serieschar - number of characters in the series names, default 8 characters
	# Output:
	#   An fhx object.
	#con <- file(fires)
	#on.exit(close(con))
	# first get fire data spreadsheet
	# TEST:
	#firedat.raw <- read.xlsx("/Users/kendramack/Dropbox/dendro_sections/FHX2/FIRE/FireScarData.xlsx",sheetName="c143",header=FALSE)
	firedat.raw <- read.xlsx(fires,sheetName=siteName,header=FALSE)
	#if there are any empty cells in the data, the script will fail
	if(distill==TRUE){
		fire.dist <- firedat.raw[,c(1,ncol(firedat.raw))]
		fire.dist[,1] <- as.character(fire.dist[,1])
		for (i in (serieschar+1):(nrow(firedat.raw))) {
			#print(i)
			fire.dist[i,1] <- NA
			if((rowSums(firedat.raw=="U" | 
										firedat.raw=="A" | 
										firedat.raw=="D" |
										firedat.raw=="E" | 
										firedat.raw=="L" |
										firedat.raw=="M"
									)[i])>=dist.thresh) #if there are fires in the threshold number of samples
			#if((rowSums(firedat.raw=="U")[i])>0 |
			#	 	(rowSums(firedat.raw=="A")[i])>0 |
			#	 	(rowSums(firedat.raw=="D")[i])>0 |
			#	 	(rowSums(firedat.raw=="E")[i])>0 |
			#	 	(rowSums(firedat.raw=="L")[i])>0 |
			#	 	(rowSums(firedat.raw=="M")[i])>0
			#)
				fire.dist[i,1] <- "U"
			else
				fire.dist[i,1] <- "|"
			if(is.na(fire.dist[i,1]))
				stop()
		}
		firedat.raw <- fire.dist
		firedat.raw[4:8,1] <- "0" #reset series name for distilled data
	}
	# make an empty fhx object
	f <- list(first.year = NA,  # First year of all the series.
						last.year = NA,  # Last year of all the series.
						series.names = NA,  # Ordered factor of the series.names.
						meta = list(),  # Odd list for collecting various bits of metadata.
						rings = NA)  # Data frame that actually contains the ring data.
	class(f) <- "fhx"	
	
	# Defining the series names.
	#get the header from the xlsx file
	###### maybe i should first remove any series that have no data entered...?
	sernames <- firedat.raw[1:serieschar,]
	#remove NAs and transpose
	sernames <- as.data.frame(t(sernames[colSums(is.na(sernames))<nrow(sernames)]))
	#concatenate
	sernames$series <- as.character(sernames[,1])
	# error check to make sure that serieschar=ncol(sernames)
	for (i in 2:(ncol(sernames)-1)) {
		sernames$series <- paste(sernames$series,as.character(sernames[,i]),sep="")
	}
	#print(sernames$series)
	f$series.names <- sernames$series
#print(f$series.names)
	# Filling the class with info from the fhx file body.

	# remove the header rows containing the series names
	firedat <- firedat.raw[(serieschar+1):nrow(firedat.raw),]
	colnames(firedat) <- c(sernames$series,"year")
	# keep year as type int, since that's how they do it in the other function
	firedat$year <- as.integer(firedat$year)
	
	# identify the first year that is not null...
	numbernulls <- as.data.frame(cbind(firedat$year,rowSums(is.na(firedat))))
	# set first and last year of data in the fhx object
	f$first.year <- min(numbernulls$V1[numbernulls$V2<(ncol(firedat)-1)])
	f$last.year <- max(numbernulls$V1[numbernulls$V2<(ncol(firedat)-1)])
	#print(str(f))
	# subset firedat to those years
	firedat <- firedat[firedat$year %in% c(f$first.year:f$last.year),]
	
	# now make the rings data.frame, which involves putting all the data into three lonnnng columns
	rng <- firedat[,c(1,ncol(firedat))]
	rng$series <- rep(colnames(firedat[1]),nrow(rng))
	colnames(rng)[1] <- "type.symbol"
	#print(summary(as.factor(rng$series)))
	# add the rest of the series
	if(ncol(firedat)>2) {
	for (i in 2:(ncol(firedat)-1)) { 
		rng.tmp <- firedat[,c(i,ncol(firedat))]
		rng.tmp$series <- rep(colnames(firedat[i]),nrow(rng.tmp))
		colnames(rng.tmp)[1] <- "type.symbol"
		rng <- rbind(rng,rng.tmp)
		#print(summary(as.factor(rng$series)))
	}
	}
	# replace type symbols with text descriptions
	rng$type.symbol <- as.character(rng$type.symbol)
	# remove NAs
	rng$type.symbol[is.na(rng$type.symbol)] <- "."
	# output the levels of type.symbol present, to see if i've missed any
	#print(unique(rng$type.symbol))
	symbol.list <- c("?",
									 ".",
									 "|",
									 "A",
									 "a",
									 "D",
									 "d",
									 "E",
									 "e",
									 "i", 
									 "L",
									 "l",
									 "M",
									 "m",
									 "s",
									 "U",
									 "u",
									 "[",
									 "]",
									 "{",
									 "}")
	undefined <- rng[!(rng$type.symbol%in%symbol.list),]
	if(nrow(undefined)>0){
		cat("The following symbols are used in this data but is not yet defined:\n",as.character(rng$type.symbol[!(rng$type.symbol%in%symbol.list)]),"\n")
		stop()
	}
	# translate type symbol into text
	rng$type <- NA
	if("." %in% rng$type.symbol)
	rng$type[rng$type.symbol=="."] <- "null.year"
	if("?" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="?"] <- "estimate"
	if("|" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="|"] <- "recorder.year"
	if("U" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="U"] <- "unknown.fs"
	if("u" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="u"] <- "unknown.fi"
	if("s" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="s"] <- "unknown.fi"
	if("D" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="D"] <- "dormant.fs"
	if("d" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="d"] <- "dormant.fi"
	if("E" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="E"] <- "early.fs"
	if("e" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="e"] <- "early.fi"
	if("i" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="i"] <- "white.ring"
	if("M" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="M"] <- "middle.fs"
	if("m" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="m"] <- "middle.fi"
	if("L" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="L"] <- "late.fs"
	if("l" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="l"] <- "late.fi"
	if("A" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="A"] <- "latewd.fs"
	if("a" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="a"] <- "latewd.fi"
	if("[" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="["] <- "pith.year"
	if("]" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="]"] <- "bark.year"
	if("{" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="{"] <- "inner.year"
	if("}" %in% rng$type.symbol)
	rng$type[rng$type.symbol=="}"] <- "outer.year"
	rng$type <- as.factor(rng$type)

	stopifnot(colSums(is.na(rng))[4]==0)
	stopifnot(colSums(is.na(rng))[3]==0)
	# set levels of $type to include all possible types, not just those used in the current dataset?
	# i think this would have bearing on whether datasets can be merged (have to have same factor levels)
  rng <- rng[,c(2,4,3)]
	f$rings <- rng
	if(distill==FALSE)
		order.fhx(f)
	f	
}

##### END FUNCTION xlsx.to.fhx
##############################


#################################
# 3 #############################
###### START FUNCTION fhx - THIS READS IN OUR DETS FILE

fhx <- function(est) {
  # Read input "establishment" data.frame return an fhx object.
  #
  # Input:
  #   est - "establishment" data.frame, i.e. our datdets file
  #
  # Output:
  #   An fhx object.
  stopifnot(class(est) == "data.frame")
  stopifnot(c("unique", "site", "plot", "tree", "species",
              "pith.date", "inner.ring", "outer.ring",
              "bark.date") %in% names(est))
  # First subset out samples at 0.3m
  est <- est[!is.na(est$smpl.ht)&est$smpl.ht<1.3,]
  # FOR NOW subset out samples that have pith
  est <- est[!is.na(est$pith.date),]
  # Draw out the columns we need from est:
  est <- data.frame("unique" = est$unique,
                    "site" = est$site,
                    "plot" = est$plot,
                    "tree" = est$tree,
                    "species" = est$species,
                    "pith.date" = est$pith.date,
                    "inner.ring" = est$inner.ring,
                    "outer.ring" = est$outer.ring,
                    "bark.date" = est$bark.date)
  #remove duplicates
  est$unique <- factor(est$unique)  # To reset the levels.
  if(nrow(est)>length(unique(est$unique))) { #if there are more rows than unique trees
  	est.trees <- removedupe.trees(est)
  	est <- est.trees[, c("unique", "site", "plot","tree","species","pith.date", "inner.ring",
  													"outer.ring", "bark.date")]
  }
  #if there are any trees missing both pith and inner, or both bark and outer, remove
  bad.eggs <- rownames(est[(is.na(est$pith.date)&is.na(est$inner.ring)) |
  								(is.na(est$outer.ring) & is.na(est$bark.date)),])
	est <- est[!(rownames(est)%in%bad.eggs),] 
  
  est$unique <- factor(as.character(est$unique)) # To reset the levels.
  # Trying to stay true to the vocab used in the FHX2 manual...
  f <- list(first.year = NA,  # First year of all the series.
            last.year = NA,  # Last year of all the series.
            series.names = NA,  # Ordered factor of the series.names.
            meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$series.names <- unique(est$unique)
  f$meta$est <- data.frame("series" = est$unique,
                           "site" = est$site,
                           "plot" = est$plot,
                           "tree" = est$tree,
                           "species" = est$species)
 
  est <- est[, c("unique", "pith.date", "inner.ring",
                 "outer.ring", "bark.date")]
  f$first.year <- with(est, min(c(min(pith.date, na.rm = TRUE),
                                  min(inner.ring, na.rm = TRUE))))
  f$last.year <- with(est, max(c(max(bark.date, na.rm = TRUE),
                                 max(outer.ring, na.rm = TRUE))))
  est.melt <- melt(est, id = "unique")
  tmp.year <- rep(NA, length(f$series.names) * (f$last.year - f$first.year + 1)) 
  tmp.type <- factor(rep(NA, length(f$series.names) * (f$last.year - f$first.year + 1)),
                     levels = c("null.year", "recorder.year", "unknown.fs",
                                "unknown.fi", "dormant.fs", "dormant.fi",
                                "early.fs", "early.fi", "middle.fs",
                                "middle.fi", "late.fs", "late.fi",
                                "latewd.fs", "latewd.fi", "pith.year",
                                "bark.year", "inner.year", "outer.year",
                                "estimate"))
  tmp.series <- rep(NA, length(f$series.names) * (f$last.year - f$first.year))
  yr.seq <- seq(f$first.year, f$last.year)
  k <- 0
 
  for ( i in seq(1, length(f$series.names)) ) { # for each tree
    s <- f$series.names[i] # get the tree name
    tmp.pith.date <- as.numeric(est$pith.date[est$unique == s]) #get the pith date for that tree
    tmp.inner.ring <- est$inner.ring[est$unique == s] #get the inner ring date for that tree
    tmp.outer.ring <- est$outer.ring[est$unique == s] # get the outer ring date
    tmp.bark.date <- est$bark.date[est$unique == s] # get the bark date
    for ( j in seq(1, length(yr.seq)) ) { # for each year in the total number of years in est
      k <- k + 1
      #print(k)  # DEBUG
      foo <- NA
      y <- yr.seq[j] #get the year
     
      if (y<min(c(tmp.pith.date, tmp.inner.ring),na.rm=TRUE)) {
        foo <- "null.year"
      } else if (!is.na(tmp.pith.date)&y==tmp.pith.date) {
        foo <- "pith.year"
      } else if ((!is.na(tmp.pith.date)&y>tmp.pith.date) & 
                 (!is.na(tmp.inner.ring)&y<tmp.inner.ring)) {
        foo <- "estimate" #if between the pith date and inner ring date
      } else if ((!is.na(tmp.inner.ring)&y==tmp.inner.ring) & 
                 ((is.na(tmp.pith.date)) | (tmp.pith.date<tmp.inner.ring))) {
        foo <- "inner.year" #if there's no pith date or inner ring doesn't match pith date
      } else if ((y>max(c(tmp.pith.date, tmp.inner.ring),na.rm=TRUE)) &
      					 	(y<min(c(tmp.bark.date, tmp.outer.ring),na.rm=TRUE))) {
        foo <- "recorder.year" #if between inner ring date and outer ring/bark date
        #since these are cores, they have no events, so all years are null years
        #but just call the recorder so they will be solid lines instead of dotted
      } else if (!is.na(tmp.bark.date)&y==tmp.bark.date) {
      	foo <- "bark.year" #this comes before outer.ring because 
      					# if there is both a bark date and outer ring date and they are the same
      					# we want to display bark date
      } else if ((!is.na(tmp.outer.ring)&y>tmp.outer.ring) & 
      					 	(!is.na(tmp.bark.date)&y<tmp.bark.date)) {
      	foo <- "estimate" #if between the outer.ring and bark.date
      }else if ((!is.na(tmp.outer.ring)&y==tmp.outer.ring) & 
                  ((is.na(tmp.bark.date) | tmp.bark.date>tmp.outer.ring))) {
        foo <- "outer.year" #if there's no bark date or outer ring doesn't match bark date
      } else if (y>max(c(tmp.bark.date, tmp.outer.ring),na.rm=TRUE)) {
        foo <- "null.year"
      } else {
        # If all else fails, a weak debug message:
        cat("\n\nSomething went wrong! Here is some debugging data:\n")
        cat(paste("series name: ", s, "\n", "iter year: ", y, "\n", sep = ""))
        cat(paste("pith.date: ", tmp.pith.date, "\n",
                  "inner.ring: ", tmp.inner.ring, "\n",
                  "outer.ring: ", tmp.outer.ring, "\n",
                  "bark.date: ", tmp.bark.date, "\n", sep = ""))
        stop()
      }
      tmp.year[k] <- y
      tmp.series[k] <- as.character(s)  # Gets around inserting numbers bug.
      tmp.type[k] <- foo
    }
  }
  f$rings <- data.frame(year = tmp.year, series = factor(tmp.series),
                        type = factor(tmp.type, 
                                      levels = c("null.year", "recorder.year", 
                                                 "unknown.fs", "unknown.fi", 
                                                 "dormant.fs", "dormant.fi", 
                                                 "early.fs", "early.fi", 
                                                 "middle.fs", "middle.fi", 
                                                 "late.fs", "late.fi", 
                                                 "latewd.fs", "latewd.fi", 
                                                 "pith.year", "bark.year", 
                                                 "inner.year", "outer.year", 
                                                 "estimate")))
  order.fhx(f)
}

###### END OF FUNCTION fhx
##########################


#################################
# 3b #############################
###### START FUNCTION trees.fhx - THIS CONVERTS THE TREES DATAFRAME TO FHX

trees.fhx <- function(tr) {
	# Read input "establishment" data.frame return an fhx object.
	#
	# Input:
	#   trees - dataframe that is result of estimating age at 0.3 for each tree
	#
	# Output:
	#   An fhx object.
	stopifnot(class(tr) == "data.frame")
	stopifnot(c("tree.id", "site", "plot.id", "tag", "sp",
							"pith.date", "inner.ring", "outer.ring",
							"bark.date") %in% names(tr))
	# FOR NOW subset out samples that have pith
	tp <- tr[!is.na(tr$pith.date),]
	# Draw out the columns we need from tp:
	tp <- data.frame("unique" = tp$tree.id,
										"site" = tp$site,
										"plot" = tp$plot.id,
										"tree" = tp$tag,
										"species" = tp$sp,
										"pith.date" = tp$pith.date,
										"inner.ring" = tp$inner.ring,
										"outer.ring" = tp$outer.ring,
										"bark.date" = tp$bark.date)

	tp$unique <- factor(tp$unique)  # To reset the levels.
	
	#if there are any trees missing both pith and inner, or both bark and outer, remove
	bad.eggs <- rownames(tp[(is.na(tp$pith.date)&is.na(tp$inner.ring)) |
													 	(is.na(tp$outer.ring) & is.na(tp$bark.date)),])
	tp <- tp[!(rownames(tp)%in%bad.eggs),] 
	
	tp$unique <- factor(as.character(tp$unique)) # To reset the levels.
	# Trying to stay true to the vocab used in the FHX2 manual...
	f <- list(first.year = NA,  # First year of all the series.
						last.year = NA,  # Last year of all the series.
						series.names = NA,  # Ordered factor of the series.names.
						meta = list(),  # Odd list for collecting various bits of metadata.
						rings = NA)  # Data frame that actually contains the ring data.
	class(f) <- "fhx"
	f$series.names <- unique(tp$unique)
	f$meta$tp <- data.frame("series" = tp$unique,
													 "site" = tp$site,
													 "plot" = tp$plot,
													 "tree" = tp$tree,
													 "species" = tp$species)
	
	tp <- tp[, c("unique", "pith.date", "inner.ring",
								 "outer.ring", "bark.date")]
	f$first.year <- with(tp, min(c(min(pith.date, na.rm = TRUE),
																	min(inner.ring, na.rm = TRUE))))
	f$last.year <- with(tp, max(c(max(bark.date, na.rm = TRUE),
																 max(outer.ring, na.rm = TRUE))))
	tp.melt <- melt(tp, id = "unique")
	tmp.year <- rep(NA, length(f$series.names) * (f$last.year - f$first.year + 1)) 
	tmp.type <- factor(rep(NA, length(f$series.names) * (f$last.year - f$first.year + 1)),
										 levels = c("null.year", "recorder.year", "unknown.fs",
										 					 "unknown.fi", "dormant.fs", "dormant.fi",
										 					 "early.fs", "early.fi", "middle.fs",
										 					 "middle.fi", "late.fs", "late.fi",
										 					 "latewd.fs", "latewd.fi", "pith.year",
										 					 "bark.year", "inner.year", "outer.year",
										 					 "estimate"))
	tmp.series <- rep(NA, length(f$series.names) * (f$last.year - f$first.year))
	yr.seq <- seq(f$first.year, f$last.year)
	k <- 0
	
	for ( i in seq(1, length(f$series.names)) ) { # for each tree
		s <- f$series.names[i] # get the tree name
		tmp.pith.date <- as.numeric(tp$pith.date[tp$unique == s]) #get the pith date for that tree
		tmp.inner.ring <- tp$inner.ring[tp$unique == s] #get the inner ring date for that tree
		tmp.outer.ring <- tp$outer.ring[tp$unique == s] # get the outer ring date
		tmp.bark.date <- tp$bark.date[tp$unique == s] # get the bark date
		for ( j in seq(1, length(yr.seq)) ) { # for each year in the total number of years in est
			k <- k + 1
			#print(k)  # DEBUG
			foo <- NA
			y <- yr.seq[j] #get the year
			
			if (y<min(c(tmp.pith.date, tmp.inner.ring),na.rm=TRUE)) {
				foo <- "null.year"
			} else if (!is.na(tmp.pith.date)&y==tmp.pith.date) {
				foo <- "pith.year"
			} else if ((!is.na(tmp.pith.date)&y>tmp.pith.date) & 
								 	(!is.na(tmp.inner.ring)&y<tmp.inner.ring)) {
				foo <- "estimate" #if between the pith date and inner ring date
			} else if ((!is.na(tmp.inner.ring)&y==tmp.inner.ring) & 
								 	((is.na(tmp.pith.date)) | (tmp.pith.date<tmp.inner.ring))) {
				foo <- "inner.year" #if there's no pith date or inner ring doesn't match pith date
			} else if ((y>max(c(tmp.pith.date, tmp.inner.ring),na.rm=TRUE)) &
								 	(y<min(c(tmp.bark.date, tmp.outer.ring),na.rm=TRUE))) {
				foo <- "recorder.year" #if between inner ring date and outer ring/bark date
				#since these are cores, they have no events, so all years are null years
				#but just call the recorder so they will be solid lines instead of dotted
			} else if (!is.na(tmp.bark.date)&y==tmp.bark.date) {
				foo <- "bark.year" #this comes before outer.ring because 
				# if there is both a bark date and outer ring date and they are the same
				# we want to display bark date
			} else if ((!is.na(tmp.outer.ring)&y>tmp.outer.ring) & 
								 	(!is.na(tmp.bark.date)&y<tmp.bark.date)) {
				foo <- "estimate" #if between the outer.ring and bark.date
			}else if ((!is.na(tmp.outer.ring)&y==tmp.outer.ring) & 
									((is.na(tmp.bark.date) | tmp.bark.date>tmp.outer.ring))) {
				foo <- "outer.year" #if there's no bark date or outer ring doesn't match bark date
			} else if (y>max(c(tmp.bark.date, tmp.outer.ring),na.rm=TRUE)) {
				foo <- "null.year"
			} else {
				# If all else fails, a weak debug message:
				cat("\n\nSomething went wrong! Here is some debugging data:\n")
				cat(paste("series name: ", s, "\n", "iter year: ", y, "\n", sep = ""))
				cat(paste("pith.date: ", tmp.pith.date, "\n",
									"inner.ring: ", tmp.inner.ring, "\n",
									"outer.ring: ", tmp.outer.ring, "\n",
									"bark.date: ", tmp.bark.date, "\n", sep = ""))
				stop()
			}
			tmp.year[k] <- y
			tmp.series[k] <- as.character(s)  # Gets around inserting numbers bug.
			tmp.type[k] <- foo
		}
	}
	f$rings <- data.frame(year = tmp.year, series = factor(tmp.series),
												type = factor(tmp.type, 
																			levels = c("null.year", "recorder.year", 
																								 "unknown.fs", "unknown.fi", 
																								 "dormant.fs", "dormant.fi", 
																								 "early.fs", "early.fi", 
																								 "middle.fs", "middle.fi", 
																								 "late.fs", "late.fi", 
																								 "latewd.fs", "latewd.fi", 
																								 "pith.year", "bark.year", 
																								 "inner.year", "outer.year", 
																								 "estimate")))
	order.fhx(f)
}

###### END OF FUNCTION trees.fhx
##########################



#################################
# 4 #############################
###### START FUNCTION +.fhx

"+.fhx" <- function(a, b,multisite=FALSE) {
  # Concatenate two fhx objects and return the combination.
  stopifnot(class(b) == "fhx")
  f <- list(first.year = NA,  # First year of all the series.
            last.year = NA,  # Last year of all the series.
            series.names = NA,  # Ordered factor of the series.names.
            meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$first.year <- min(a$first.year, b$first.year)
  f$last.year <- max(a$last.year, b$last.year)
  f$series.names <- c(a$series.names, b$series.names)
  f$rings <- rbind(a$rings, b$rings)
  if ( length(a$meta) | length(b$meta) > 0 ) {  # If meta data present...
    f$meta <- c(a$meta, b$meta)
  }
  tmp.year <- c()
  tmp.series <- c()
  tmp.type <- c()
  # Stuffing data so that it matches the combined year-range of a and b.
  for ( i in f$series.names ) { # for each series
    i.min <- with(f$rings, min(year[series == i], na.rm = TRUE)) #get the min year
    i.max <- with(f$rings, max(year[series == i], na.rm = TRUE)) #get the max year
    low.seq <- c()
    high.seq <- c()
    if (i.min > f$first.year) #if the first year of the series is later than the first year of all series
      low.seq <- c(seq(i.min, f$first.year), low.seq)
    if (i.max < f$last.year)
      high.seq <- c(high.seq, seq(i.max, f$last.year))
    tmp.year <- c(tmp.year, low.seq, high.seq) #get list of earliest and latest years
    tmp.series <- c( tmp.series, rep(i, length(c(low.seq, high.seq))) ) # repeat the current series name once for each year
  }
  tmp.type <- rep("null.year", length(tmp.year))
  tmp.rings <- data.frame(year = tmp.year,
                          series = tmp.series,
                          type = tmp.type)
  f$rings <- rbind(f$rings, tmp.rings)
  #f <- remove_duplicates(f)  # DEBUG
  if(multisite==FALSE)
  	f <- order.fhx(f)
  f
}

###### END OF FUNCTION +.fhx
#############################


#################################
# 5 #############################
###### START FUNCTION get.fire.years
get.fire.years <- function(fire,thresh=1) {
	fireyrs <- sort(unique(fire$rings$year[regexpr(".fs",fire$rings$type)>0]))
	fireyrs <- data.frame(cbind(fireyrs,fireyrs))
	colnames(fireyrs) <- c("year","label")
	# get number of samples per fire, in order to threshold
	clean <- subset(fire$rings, fire$rings$type != "null.year") # everything that's not a null.year
	clean.nonrec <- subset(clean, clean$type != "recorder.year") # everything that's an event or a pith/inner/outer/bark
	events <- subset(clean.nonrec, regexpr(".fs",clean.nonrec$type)>0) #i.e. keep just capital letters
	samples.per.fire <- as.data.frame(table(events$year))
	colnames(samples.per.fire) <- c("year","n")
	#print(exists("samples.per.fire"))
	stopifnot(fireyrs$year==samples.per.fire$year)
	fireyrs$n <- samples.per.fire$n
	#check for back to back fires
	i=1
	while((nrow(fireyrs)>i)){
		if(fireyrs$year[i+1]==fireyrs$year[i]+1) { #if there are back to back fire years
			lastdigit <- as.numeric( #get the last digit of the earlier fire year
				substr(
					as.character(fireyrs$year[i]), #string
					nchar(as.character(fireyrs$year[i])), #first
					nchar(as.character(fireyrs$year[i])) #last
					)
				) 
			fireyrs$label[i] <- paste(fireyrs$year[i],"-",
																(lastdigit+1),sep="") # create the label for the combined year
			fireyrs$year[i] <- mean(c(fireyrs$year[i],fireyrs$year[i+1])) # set the year of the combined year
			fireyrs$n[i] <- fireyrs$n[i] + fireyrs$n[i+1]
			fireyrs <- fireyrs[-(i+1),]	# remove the back-to-back year
		}
		i=i+1
	}
	fireyrs <- fireyrs[fireyrs$n>=thresh,]
	fireyrs
}

###### END OF FUNCTION get.fire.years
#############################

#################################
# 6 #############################
###### START FUNCTION ggplot.fhx

ggplot.fhx <- function(x, vline=TRUE,events.thresh=1,vline.thresh=1,label.thresh=events.thresh,filled=FALSE,label.fires=TRUE,multisite=FALSE,legend=TRUE,xmin=round_any(x$first.year,10,f=floor),xmax=(x$last.year+10),main="",save=FALSE) {
  # Return a ggplot2 object for plotting.
	# events.thresh sets the number of samples recording an event for it to show up on plot as a triangle
	# vline.thresh sets the number of samples recording an event for it to have a vline drawn through it
	# filled makes the fire triangles solid black
  stopifnot(is.logical(vline))
  clean <- subset(x$rings, x$rings$type != "null.year") # everything that's not a null.year
  clean.nonrec <- subset(clean, clean$type != "recorder.year") # everything that's an event or a pith/inner/outer/bark
  events <- subset(clean.nonrec, regexpr(".fs",clean.nonrec$type)>0) #i.e. keep just capital letters
  samples.per.fire <- as.data.frame(table(events$year))
  colnames(samples.per.fire) <- c("year","n")
  if(label.fires==TRUE) {
  	fireyrs <- data.frame(cbind(samples.per.fire$year,samples.per.fire))
  	colnames(fireyrs) <- c("year","label","n")
  	fireyrs$year <- as.integer(as.character(fireyrs$year))
  	fireyrs$label <- as.character(fireyrs$label)
  	#check for back to back fires
  	i=1
  	while(nrow(fireyrs)>i){
  		if(fireyrs$year[i+1]==fireyrs$year[i]+1) { #if there are back to back fire years
  			lastdigit <- as.numeric( #get the last digit of the earlier fire year
  				substr(
  					as.character(fireyrs$year[i]), #string
  					nchar(as.character(fireyrs$year[i])), #first
  					nchar(as.character(fireyrs$year[i])) #last
  				)
  			) 
  			fireyrs$label[i] <- paste(fireyrs$year[i],"-",
  																(lastdigit+1),sep="") # create the label for the combined year
  			fireyrs$year[i] <- mean(c(fireyrs$year[i],fireyrs$year[i+1])) # set the year of the combined year
  			fireyrs$n[i] <- fireyrs$n[i] + fireyrs$n[i+1]
  			fireyrs <- fireyrs[-(i+1),]	# remove the back-to-back year
  		}
  		i=i+1
  	}
  	fireyrs <- fireyrs[fireyrs$n>=label.thresh,]
  	fireyrs
  }
  # subset the events that meet the vline.thresh and store for use when plotting vline below
  bigyears <- as.numeric(as.character((samples.per.fire$year[samples.per.fire$n>=vline.thresh])))
  vline.events <- subset(events,events$year %in% bigyears)
  # now get years of fire events that meet the events.thresh
  bigyears <- as.numeric(as.character((samples.per.fire$year[samples.per.fire$n>=events.thresh])))
  # subset events to include just those years
  events <- subset(events,events$year %in% bigyears)
  
  # then subset out just the pith and bark dates:
  ends <- subset(clean.nonrec, clean.nonrec$type %in% c("pith.year", "bark.year") &
                 !(clean.nonrec$type %in% c("estimate")))

  estimate <- subset(clean, clean$type == "estimate")
  live <- NA
  if ( dim(estimate)[1] > 0 ) {  # If we have estimate years.
    # Get the min and max of the estimate years.
    estimate <- aggregate(estimate$year,
                          by = list(estimate$series),
                          FUN = range,
                          na.rm = TRUE)
    estimate <- data.frame(series = estimate$Group.1,
                           first = estimate$x[, 1],
                           last = estimate$x[, 2],
                           type = rep("estimate", dim(estimate)[1]))
    live <- subset(clean, clean$type != "estimate")
    # TODO: This is a problem because it will plot on top of "estimate" years.
    # Get min and max of living years.
    live <- aggregate(live$year,
                      by = list(live$series),
                      FUN = range,
                      na.rm = TRUE)
  } else {  # If we don't have estimate years.
    live <- aggregate(clean$year,
                      by = list(clean$series),
                      FUN = range,
                      na.rm = TRUE)
  }
  live <- data.frame(series = live$Group.1,
                     first = live$x[, 1],
                     last = live$x[, 2],
                     type = rep("non-recording", dim(live)[1]))

  recorder <- subset(x$rings, x$rings$type == "recorder.year")
  if ( dim(recorder)[1] > 0 ) {  # If there are recorder years...
    # Get the min and max of the recorder years.
    recorder <- aggregate(recorder$year,  # TODO: rename this var.
                           by = list(recorder$series, recorder$type),
                           FUN = range,
                           na.rm = TRUE)
    recorder <- data.frame(series = recorder$Group.1,
                           first = recorder$x[, 1],
                           last = recorder$x[, 2],
                           type = rep("recording", dim(recorder)[1]))
    segs <- rbind(recorder, live)
  } else {  # If there are no recorder years...
    segs <- live
  }
  if ( dim(estimate)[1] > 0 ) { # If we have estimate years.
    segs <- rbind(segs, estimate)
  }
  levels(segs$type) <- c("recording", "non-recording", "estimate")
  #levels(events$type) <- c()
  #levels(ends$type) <- c()
  p <- ggplot(data = x$rings, aes(y = series, x = year)) 
  p <- p + 
  	   coord_cartesian(xlim=c(xmin,xmax)) +
  	   scale_x_continuous(breaks=c(xmin,seq(round_any(xmin,50,f=ceiling),xmax,50)))
  p <- p +
       geom_segment(aes(x = first, xend = last,
                        y = series, yend = series, linetype = type, size = type),
                        data = segs) +
       scale_linetype_manual(values = c(1, 3, 1)) +
       scale_size_manual(values = c(0.5, 0.5, 0.3)) +
  	   ggtitle(main) + 
  	   theme(plot.title = element_text(face="bold")) + theme_bw()
  		if(nrow(ends)>0)		
  		   p <- p + geom_point(data = ends, shape = 124)
  		 # shapes:
  	   # 16 med solid cirlce
  	   # 20 sm solid circle
  		 # 47 /
  	   # 92 \
  	   # 124 |
  	   # 91 [
  	   # 93 ]
  	   # 60 <
  	   # 62 >
  		 
  p <- p +
  	   theme(axis.title.y = element_blank(), #remove ylab
  				 axis.ticks.y=element_blank(), #remove axis ticks
  				 panel.grid=element_blank()) #remove gridlines
  if(multisite==FALSE)
  	p <- p + theme(axis.text.y=element_blank()) #remove series names
  if(label.fires==TRUE) {
  	#fireyrs <- get.fire.years(x,thresh=label.thresh)
  	#p <- p + annotate("text",x=fireyrs$year,y=(length(x$series.names)-.1*(length(x$series.names))),
  	#									label=c(fireyrs$label),angle=90,size=3)
  	p <- p + annotate("text",x=fireyrs$year,y=Inf,hjust=1,
  						    	  label=c(fireyrs$label),angle=90,size=3)
  #	p <- p + annotate("text",
  #						aes(label = c(fireyrs$label),
  #						x = fireyrs$year,
  #						y = Inf), #this indicates the top extent of the plotting panel
  #						vjust = -1
  #						)
  	#code to override clipping
  	#gt <- ggplot_gtable(ggplot_build(p))
  	#gt$layout$clip[gt$layout$name == "panel"] <- "off"
  	#grid.draw(gt)
  }
  if (legend == FALSE)
  		 p <- p + theme(legend.position="none")
  if (dim(events)[1] > 0) { # If we actually have events...
  	if (filled == TRUE) #option to have them filled in
    	p <- p + geom_point(data = events, shape = 25,bg="black")
  	else
  		p <- p + geom_point(data = events, shape = 25)
    if (vline == TRUE) # style of vertical line for events
        p <- p + geom_vline(xintercept = vline.events$year, alpha = 0.05, size = 1.5) 
  			#p <- p + geom_seg()
  }
  if (save == TRUE)
  	ggsave(paste(getwd(),"/dendro_r/graphs/fire_diagrams/",main,".pdf",sep=""),width=10.5,height=4,dpi=300)
  p
}

###### END OF FUNCTION ggplot.fhx
