#' Compute summary statistics and frequency tables
#' 
#' @param dat data frame containing the variables to analyze.
#' @param varnum string or array with the continuous variables for which summary statistics are computed.
#' @param varclass string or array with the categorical variables for which frequency tables are computed.
#' @param target string or array with the categorical target variable to analyze in conjunction with the categorical variables.
#' @param event target event of interest to analyze.
#' @param percentiles percentiles to be computed on each analysis variable (given as values between 0 and 100 --e.g. c(0, 10, 90, 100)).
#' @param stats string containing the statistics to compute on the analysis variables (e.g. "mean sd").
#' @param top for categorical variables number of top most frequent categories to store in the output frequency table as long as their frequency
#' is at least \code{freqmin}. Use \code{top = NULL} to display all cases in alphabetical order. Use \code{top = 0} to display
#' all cases sorted by decreasing frequency.
#' @param freqmin for categorical variables minimum number of cases for a category to be stored in the output frequency table.
#' @param propmin for categorical variables minimum proportion of cases computed on nonmissing values to be stored in the output frequency table.
#' This is useful to get frequency resolution when a variable has too many missing values and almost no category satisfy the \code{freqmin} condition.
#' @param miss array containing the values that should be considered missing (for both numeric and character variables).
#' @return If both \code{varnum} and \code{varclass} are given, a list with names \code{summary} and \code{table} containing
#' respectively the summary statistics of continuous variables and the frequency table of categorical variables.
#'
#' If only \code{varnum} or \code{varclass} is given, a data frame containing the summary statistics or the frequency table,
#' respectively.
#'
#' The summary statistics table contains one row per variable and one column per percentile/statistic requested.
#' The percentiles are named using prefix 'p' as in 'p25' to indicate percentile 25.
#'
#' The frequency table contains one row per valid variable value, where "valid" means that their occurrence
#' frequency is at least \code{freqmin} (in absolute terms) or \code{propmin} (in relative terms).
#' All the values listed in parameter \code{miss} are considered as missing values and counted separately.
#'
#' For each variable, the following frequency information is shown:
#' - when \code{top} is not \code{NULL} and is \code{>0}, first the \code{top} most frequent categories
#' are shown, otherwise, all nonmissing values are shown.
#' - when \code{top} is not \code{NULL} and is \code{>0}, a row with the information on the "_other_" category
#' (that groups all remaining nonmissing values) is shown
#' - a row containing information on the missing cases (if any exist)
#' - a row containing the total count of valid top values, their proportion, and their target penetration
#' (when a target variable has been requested)
#' Note that summing the frequency of valid top values, the frequency of the "_other_" group, and the frequency
#' of missing values gives the total number of cases in the data, and their proportion sum to 1.
#'
#' The columns in the output frequency table are:
#' \itemize{
#' \item var analysis variable name
#' \item value value taken by the variable
#' \item freq frequency of the value
#' \item prop relative frequency of the value w.r.t. all the records
#' \item <target> (when a target variable is specified) target event penetration when the variable takes the corresponding value.
#' The name of the column is the name of the target variable analyzed followed by the target event value (e.g. y_1, where "y" is
#' the target variable and "1" is the event of interest)
#' }
#' The rightmost column contains the proportion of valid values w.r.t. the total number of nonmissing values, except
#' for the missing category, for which the proportiont is computed as the number of missing values w.r.t. the total number of cases.
#' 
#' @details
#' Missing values are allowed. For continuous variables they are removed from the summary statistics calculation;
#' for categorical variables they are considered as possible value, but treated specially as explained above.
#'
#' Quantiles 0 and 1, if given, are displayed on columns "min" and "max" in the data frame containing the summary statistics.
dm_summary = function(
		dat, 
		varnum, 
		varclass=NULL,
		target=NULL,
		event=1,
		percentiles=c(0, 1, 5, 10, 25, 50, 75, 90, 95, 99, 100),
		stats="mean sd",
		top=NULL,
		freqmin=1,
		propmin=0.01,
		miss=c(NA, NaN, "")) {

  #-- Parse input parameters
  varnum = parseVariables(varnum)
  varclass = parseVariables(varclass)
  stats = parseVariables(stats)
  target = parseVariables(target)

  # Check existence of variables
  varsNotFound = checkVariables(dat, c(varnum, varclass, target), )
  stopifnot(is.null(target) || length(target) == 1)
  stopifnot(is.null(varsNotFound))

  # Continue parsing...
  targetFlag = FALSE
  if (!is.null(target)) targetFlag = TRUE
 	if (!is.null(top) && top < 0) top = NULL
 	if (!is.null(freqmin) && freqmin < 0 || is.null(freqmin)) freqmin = 0
 	if (is.null(propmin)) propmin = 1		# No additional filtering on propmin when propmin=NULL
 	if (!is.null(propmin) && propmin < 0 || is.null(propmin)) propmin = 0
  #-- Parse input parameters

	# Check if the user passed any variables at all
	if ( (is.null(varnum) || length(varnum) == 0) && (is.null(varclass) || length(varclass) == 0)) {
		# No variables to analyze...
		return(NULL)
	}

	#-- Summary statistics for continuous variables
  # Remove any factor attribute of continuous variables... (note that the simpler way of using dat[,varnum] = as.numeric(as.character(dat[,varnum])) does not work!)
  # NOTE that I don't update the 'dat' data frame because I don't want to remove the factor property for variables that are
  # analyzed both as continuous AND categorical variables.
	if (!is.null(varnum) && length(varnum) > 0) {	# varnum can be empty either by being NULL or being equal to ""
		dat4summary = sapply(dat[,varnum, drop=FALSE], function(x) as.numeric(as.character(x)))
		if (!is.null(percentiles) && length(percentiles) > 0) {
			dat.percentiles = apply(dat4summary, 2, FUN=quantile, probs=percentiles/100, na.rm=TRUE)
		} else {
			dat.percentiles = NULL
		}
		dat.stats = matrix(nrow=0, ncol=length(varnum))
		# TODO: (2017/04/10) Try to improve performance here by computing all statistics at once!
		for (stat in stats) {
			dat.stats = rbind( dat.stats, apply(dat4summary, 2, stat, na.rm=TRUE) )
		}
		# Add the number of observations and number of non missing values
		dat.stats = rbind( rep(nrow(dat4summary), ncol(dat4summary)), apply(dat4summary, 2, function(x) sum(!is.na(x)) ), dat.stats )
		rownames(dat.stats) = c("n", "neff", stats)

		# Transpose the stats data frame so that the summary statistics are along the columns
		summary.out = as.data.frame( t( rbind(dat.stats, dat.percentiles) ) )
		summary.out$pmiss = (summary.out$n - summary.out$neff) / summary.out$n
		summary.out = summary.out[, c("n", "neff", "pmiss", stats)]

		# Replace "0%" with "min" and "100%" with "max"
		if ("0%" %in% colnames(summary.out)) summary.out = rename.vars(summary.out, from="0%", to="min", info=FALSE)
		if ("100%" %in% colnames(summary.out)) summary.out = rename.vars(summary.out, from="100%", to="max", info=FALSE)
	}
  
	#-- Frequency table for categorical variables
	if (!is.null(varclass) && length(varclass) > 0) {	# varclass can be empty either by being NULL or being equal to ""
		# Compute the frequency table of all variables and return the result as a list (using lapply())
		if (targetFlag) {
			dat.tablelist = lapply(dat[, varclass, drop=FALSE], table, dat[, target], useNA="ifany")
		} else {
			dat.tablelist = lapply(dat[, varclass, drop=FALSE], table, useNA="ifany")
		}

		# Store the list with the results in a data frame
		tab.out = list()
		for (v in varclass) {
			# Frequency table for the currently analyzed variable
			# NOTE the use of as.data.frame.matrix() instead of as.data.frame() in order to preserve the structure of the mxn contingency table!
			# (as.data.frame() would stack the horizonal dimension along the vertical dimension! --and we don't want that)
			# Ref: https://www.r-bloggers.com/how-to-convert-contingency-tables-to-data-frames-with-r/
			# NOTE about the output table:
			# - the variable's values frequencies are stored in column Freq
			# - the variable's values are stored in:
			# 	- column Var1 when no target variable is given
			# 	- as row names when a target variable is given
			if (targetFlag) {
				tabv = as.data.frame.matrix( dat.tablelist[[v]] )
			} else {
				tabv = as.data.frame( dat.tablelist[[v]] )
			}

			# Compute the total number of cases by variable value
			if (targetFlag) {
				# Total number of cases per variable's value
				tabv$Freq = apply(tabv, 1, sum)
				# Variable's values are stored as a column
				tabv$Var1 = rownames(tabv)
				rownames(tabv) = 1:nrow(tabv)
				# Compute the target penetration for the event of interest
				tabv[, "target"] = tabv[, as.character(event)] / tabv$Freq
				# Keep just the variable's values, number of cases, and target penetration
				tabv = tabv[, c("Var1", "Freq", "target")]
				# Name of the target column in the output frequency table
				targetcol = paste(target, as.character(event), sep="_")
			} else {
				# Name of the target column in the output frequency table (no column)
				targetcol = NULL
			}			

			# Total frequency and proportion on nonmissing cases (the proportion may be used for filtering of categories (based on parameter propmin)
			indna = tabv$Var1 %in% miss			# Note that when miss contains 'NaN', the corresponding name in tabv is "NaN" and a comparison of this name with NaN or "NaN" yields TRUE (great!)
			ntotal_notmiss = sum(tabv[!indna, "Freq"])
			freq = tabv$Freq
			prop = freq / ntotal_notmiss
			ntotal = sum(freq)

			# Top N, freq min and prop min
			if (!is.null(top) || freqmin > 0 || propmin > 0) {
				# Filter variable's values based on freqmin, propmin and top
				# (first the freqmin and propmin filters are applied and then the top 'top' cases are selected among those satisfying the freqmin OR propmin filter)
				# The result of using | prop >= propmin is that variables having too few nonmissing values can also be analyzed, since the propmin
				# condition is applied on the proportions calculated over nonmissing values.
				indvalid = !indna & (freq >= freqmin | prop >= propmin)
				nvalid = sum(indvalid)
				if (nvalid == 0) {
					indtop = NULL
				} else {
					if (top == 0) {		# top = 0 means that we want ALL occurrences of the categorical variable (but sorted)
						indtop = 1:nvalid
					} else {
						indtop = 1:min(top, nvalid)  # Note that top is guaranteed to be >= 1 or NULL (NULL is ok with the min() function)
					}
				}
				# Sort nonmissing values by decreasing frequency on the filtered variable's values
				tabv.valid = tabv[indvalid,]
				if (targetFlag) {
					# Sort by decreasing target penetration
					ord = order( tabv.valid$target, decreasing=TRUE )
				} else {
					ord = order( tabv.valid$Freq, decreasing=TRUE )
				}
				tabv.valid.sorted = tabv.valid[ord,]

				# Compute values for the "other" group (i.e. all the rest that has not been selected and is not missing)f
				indother = !indna & !indvalid
				# Construct the "other" table as the indother cases + the non-top valid cases
				tabv.other = rbind( tabv[indother, , drop=FALSE], tabv.valid.sorted[-indtop, , drop=FALSE])
				nother = nrow(tabv.other)
				if (nother > 0) {
					tabv.other.agg = data.frame(Var1=paste("_other_(n=", nother, ")", sep=""), Freq=sum(tabv.other$Freq))
					if (targetFlag) {
						tabv.other.agg$target = weighted.mean(tabv.other$target, tabv.other$Freq)
					}
				} else {
					tabv.other.agg = NULL
				}

				# Update the information on proportions (because now the data is sorted by decreasing frequency)
				# NOTE that this proportion is computed BEFORE adding the NA information because the NA row stores
				# a proportion that is computed differentely (on the total number of cases)
				tabv.valid.sorted$prop = tabv.valid.sorted$Freq / ntotal_notmiss
				if (nother > 0) {
					tabv.other.agg$prop = tabv.other.agg$Freq / ntotal
						## Note that for the "other" group we divide by the total number of cases (NOT by the total number of nonmissing cases)
						## This is because freq("other") + freq("validtop") + freq("NA") = ntotal
						## therefore we want prop("other") + prop("validtop") + prop("NA") = 100%
				}

				# Put all the information together
				# TODO: (2017/04/10) The output of cbind() may be inapropriate when indna contains more than one index... we should sum on them, but
				# it's not easy because of the NA values... aggregations using aggregate() omit NAs by default and I haven't found a way of including them
				# as valid values (as there is no na.action= option that includes NAs!! (all the options EXCLUDE NAs (see help(na.action))
				# In any case, right now in principle we get one row per type of missing value.
				tab.freq = rbind( tabv.valid.sorted[indtop, , drop=FALSE], tabv.other.agg, cbind( tabv[indna, , drop=FALSE], prop=as.numeric(tabv[indna, "Freq"])/ntotal ))
					## drop=FALSE and as.numeric() are important to avoid the error "names contain missing values",
					## arising from the fact that the NA category generates a "missing value" name.

				# Number of valid and top variable's values (in this case this is the number of nonmissing values)
				ntotal_validtop = sum(tabv.valid.sorted[indtop, "Freq"])
				# Target penetration on the valid and top variable's values (i.e. nonmissing values)
				target_validtop = weighted.mean(tabv.valid.sorted[indtop, "target"], tabv.valid.sorted[indtop, "Freq"])
			} else {
				tab.freq = tabv
				# Compute the proportion of cases w.r.t. the number of nonmissing cases
				tab.freq$prop = tab.freq$Freq / ntotal_notmiss
				# For the NA cases, compute its percentage w.r.t. the total number of cases
				tab.freq$prop[indna] = tab.freq$Freq[indna] / ntotal

				# Number of valid and top variable's values (in this case this is the number of nonmissing values)
				ntotal_validtop = ntotal_notmiss
				# Target penetration on the valid and top variable's values (i.e. nonmissing values)
				target_validtop = weighted.mean(tab.freq[!indna, "target"], tab.freq[!indna, "Freq"])
			}

			# Add the current frequency table to the output table
			# (note that we add a new row with the total information, where the total nonmissing cases are shown
			if (targetFlag) {
				tab.out = rbind(tab.out,
												cbind(var=rep(v, nrow(tab.freq)), tab.freq[, c("Var1", "Freq", "prop", "target")]),	# Resort the columns in tab.freq
												cbind(var=v, Var1="--TOTAL(valid&top)--", Freq=ntotal_validtop, prop=ntotal_validtop/ntotal, target=target_validtop))
													## IMPORTANT: We need to use cbind() here and NOT c() because in the latter case we get an error that
													## "--TOTAL--" is not a valid factor level! (because 'var' in the output data frame is considered a factor!)
			} else {
				tab.out = rbind(tab.out,
												cbind(var=rep(v, nrow(tab.freq)), tab.freq),
												cbind(var=v, Var1="--TOTAL(valid&top)--", Freq=ntotal_validtop, prop=ntotal_validtop/ntotal))		# IMPORTANT: We need to use cbind() here and NOT c() because in the latter case we get an error that "--TOTAL--" is not a valid factor level! (because 'var' in the output data frame is considered a factor!)
			}			
		}
		colnames(tab.out) = c("var", "value", "freq", "prop", targetcol)
	}

	if (is.null(varclass) || length(varclass) == 0) {
		return(summary.out)
	} else if (is.null(varnum) || length(varnum) == 0) {
		return(tab.out)
  } else {
  	return(list(summary=summary.out, table=tab.out))
  }
}
