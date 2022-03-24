#' Qualify variables into categorical, continuous, and rejected
#
#' Qualifies variables in:
#' - categorical (factor variables, character variables, or with few categories)
#' - continuous (numeric with many distinct values)
#' - rejected (categorical variables with 0 or 1 distinct value, with a category with too many values
#' --including missing values)
# Needs package sqldf, gdata
#library(sqldf)			--> for sqldf()
#library(gdata)			--> for rename.vars()
#library(dmtools)		--> for checkVariables(), parseVariables()
dm_qualify_vars <- QualifyVars <- function(
		dat,								# Data frame containing the data for analysis.
		target=NULL,				# String with the categorical target variable to analyze. It must be a column of the dataset.
		target2=NULL,				# String with a second categorical target variable to analyze. It must be a column of the dataset.
		vars=NULL,					# String or array containing the dataset variables to analyze. When NULL, all variables in the dataset are used.
		value=0,						# Value of interest for numeric analysis variables (the number and percentage of cases taking this value is computed
		event=1,						# Event of interest (either numeric or character)
		maxncat=25,					# Maximum number of categories to consider a variable categorical.
		maxpropcat=0.95,		# Maximum proportion in category to consider a variable as useful.
		maxpropmiss=0.50,		# Maximum proportion of missing values to consider a variable as useful.
		minpropcat=0.01,		# Minimum proportion of cases in a category to consider it relevant. It is used to compute the actual minncat as max(minncat, minpropcat*<totalcases>)
		minncat=50,					# Minimum number of cases in a category to consider it relevant. The final minncat is computed as max(minncat, minpropcat*<totalcases>)
		print=TRUE)					# Whether to show messages.
{

	#------------------------------------ Auxiliary functions -----------------------------------
	crosstab = function(tab, totals, var, target, event, nvalues, targetcol, rankcol)	{
			## tab: 					(in/out) data frame with the frequency table of 'var'.
			##								This table should have the following columns:
			##								- value (with the values taken by 'var')
			##								- freq (with the frequency of each value)
			## totals: 				(in/out) data frame with totals information of the frequency table of 'var'
			## var:						(in) array with the values of the analysis variable
			## target:				(in) array with the values of the target variable
			## event:					(in) event of interest in the target variable
			## nvalues:				(in) number of distinct values taken by 'var'
			## targetcol:			(out) name of the variable where the target information will be stored in the 'tab' and 'totals' tables.
			## rankcol:				(out) name of the variable where the rank of the categories of 'var' will be stored in the 'tab' and 'totals' tables.

			#-- 2x2 frequency table
			xtab = table(var, target, useNA="ifany")

			#-- Update tab data frame
			tab[,targetcol] = xtab[tab$value, as.character(event)] / tab$freq
				## Note the use of as.character() to enclose event because otherwise the event must be alwasy numeric
				## In addition, if event=1, it should refer to the column with label "1" and NOT to column number 1!
			# Compute the target penetration for NA values of the analysis variable since these return NA when retrieved above!
			ind_navalues = is.na(rownames(xtab))
			if (length(ind_navalues) > 0)
				tab[ind_navalues, targetcol] = xtab[ind_navalues, as.character(event)] / tab$freq[ind_navalues]

			# Rank the values by decreasing target average
			tab[,rankcol] = nvalues - rank(tab[,targetcol]) + 1

			# Sort variable values by rank of target variable if the variable is character or factor
			if (type %in% c("character", "factor") ) {
				tab = tab[ order(tab[,rankcol]), ]
			}

			#-- Update totals data frame
			# Totals row
			totals[,rankcol] = NA
			totals[,targetcol] = weighted.mean(tab[,targetcol], tab$freq)
			
			return(list(tab=tab, totals=totals))
	}
	#------------------------------------ Auxiliary functions -----------------------------------

  #---------------------------------- Parse input parameters ----------------------------------
  ### VARS
  if (missing(vars) || is.null(vars)) {
    vars = colnames(dat)
  } else {
    vars = parseVariables(vars)
  }
  if (!is.null(checkVariables(dat, c(target, target2, vars)))) stop("Variables not found. Processing stops")

  ### VALUE
  if (!is.null(value) && !is.na(value)) {
  	valueFlag = TRUE
  	nvaluestr = paste("n", value, sep="")		# Ex: "n0", which is the name of the column to add to the output summary table containing the number of cases with 'value'
  	pvaluestr = paste("p", value, sep="")		# Ex: "p0", which is the name of the column to add to the output summary table containing the proportion of cases with 'value'
  } else {
  	valueFlag = FALSE
  	nvaluestr = ""
  	pvaluestr = "pvalue"										# Name of the column in the summary tables that should be removed at the end
  }
  #---------------------------------- Parse input parameters ----------------------------------


	#--------------------------------------- Query the data -------------------------------------
	# Prepare the query to the data that computes # distinct values and # missing values in one go
	if (print) cat("Preparing SQL query...\n")
 	sqlstr = "select '1' as first"	# Add a column at the beginning so that we can start all statements below with a comma!
 	for (i in seq_along(vars)) {
	  sqlstr = paste(sqlstr, ",count(distinct(", vars[i], ")) as nvalues_", i, ",sum(case when ", vars[i], " is null then 1 else 0 end) as nmiss_", i, sep="")
	  if (valueFlag) {
	  	sqlstr = paste(sqlstr, ",sum(case when ", vars[i], " = ", value, " then 1 else 0 end) as ", nvaluestr, "_", i, sep="")
	  } else {
	  	# Create a dummy column so that I don't need to do so many IF-THEN-ELSE below when creating the output summary tables.
	  	# I use '0' and not NULL because I will do operations with this result, but in the end this information will not be output to the final summary table.
	  	sqlstr = paste(sqlstr, ",0 as ", nvaluestr, "_", i, sep="")
	  }
	}
  sqlstr = paste(sqlstr, "from dat")
  
  # Run the query
	if (print) cat("Running SQL query on input data...\n")
	result = sqldf(sqlstr)
	#--------------------------------------- Query the data -------------------------------------


	#--------------------------------- Process the query results --------------------------------
	#--- Prepare output data
  # Indicators of whether target variables were passed to the function
  targetFlag = FALSE
  target2Flag = FALSE

  # Output summary table
  df.varcont <- df.varcat <- df.varrej <- data.frame(var=character(0), class=character(0), type=character(0), nobs=numeric(0), nvalues=numeric(0), pvalues=numeric(0), pmiss=numeric(0), pvalue=numeric(0), pmaxcat=numeric(0))
		## The above columns are as follows:
		## var 			=>	variable name
		## class		=> 	variable classification ("categorical", "continuous", "rejected")
		## type 		=>	variable type ("character", "numeric")
		## nvalues	=>	numb. distinct values taken by the variable
		## pvalues	=>	prop. distinct values taken by the variable w.r.t. # observations
		## pmiss		=>	prop. missing values
		## pvalue		=> 	prop. cases equal to 'value' --> at the end, this column is renamed to p<value> if parameter 'value' has been given
		## pmaxcat	=>	prop. cases in the categorical level with largest number of cases 

	# Output frequency table
  tab.freq = data.frame(var=character(0), index=character(0), value=character(0), freq=numeric(0), prop=numeric(0), rejected_value=numeric(0), rejected_var=numeric(0))
  rankvars <- targetvars <- targetvarnames <- NULL
  if (!missing(target) && !is.null(target)) {
    targetFlag = TRUE
    tab.freq$rank = numeric(0)
    tab.freq$Target = numeric(0)
    rankvars = "rank"
		targetvars = "Target"
		targetvarnames = target  # Recall that target is already a string!
  }
  if (!missing(target2) && !is.null(target2)) {
    target2Flag = TRUE
    tab.freq$rank2 = numeric(0)
    tab.freq$Target2 = numeric(0)
		rankvars = c(rankvars, "rank2")
		targetvars = c(targetvars, "Target2")
		targetvarnames = c(targetvarnames, target2) # Recall that target2 is already a string!
  }

  #--- Process each analysis variable
  nvarcont = 0
  nvarcat = 0
  nvarrej = 0
  ntotal = nrow(dat)
  nvars = length(vars)
  for (i in seq_along(vars)) {
  	#--- Prepare
  	# Name of current variable to analyze
  	v = vars[i]
  	# Name of columns in the query result from where the required information about the variable should be retrieved
  	nvaluescol = paste("nvalues_", i, sep="")
  	nmisscol = paste("nmiss_", i, sep="")
  	nvaluecol = paste(nvaluestr, "_", i, sep="")
    if (print) cat("Analyzing variable", i, "of", nvars, ":")

		#--- First classification of variable into: "categorical", "continuous", "rejected"
    # Variable type (e.g. "character", "factor", "numeric")
    type = class(dat[,v])
    if (result[,nvaluescol] <= 1 || result[,nmisscol]/ntotal > maxpropmiss) {
    	varclass = "rejected"
      nvarrej = nvarrej + 1
      df.varrej = rbind(df.varrej, data.frame(var=v, class=varclass, type=type, nobs=ntotal, nvalues=result[,nvaluescol], pvalues=result[,nvaluescol]/ntotal, pmiss=result[,nmisscol]/ntotal, pvalue=result[,nvaluecol]/ntotal, pmaxcat=NA))
      if (print) cat(" REJECTED ", nvarrej, " (", v, ")\n", sep="")
    } else if (type %in% c("character", "factor") || result[,nvaluescol] <= maxncat) {
    	varclass = "categorical"
      nvarcat = nvarcat + 1
      # WE STILL DON'T ADD THIS VARIABLE TO THE DATA FRAME OF CATEGORICAL VARIABLES BECAUSE THE VARIABLE
      # COULD BE REJECTED IF THE PROPORTION IN THE MOST COMMON CATEGORY IS LARGER THAN maxpropcat
      # (this is done below when computing the frequency table)
      if (print) cat(" CATEGORICAL ", nvarcat, " (", v, ")", sep="")
    } else {
    	varclass = "continuous"
      nvarcont = nvarcont + 1
      if (type == "numeric") type = "double"
      df.varcont = rbind(df.varcont, data.frame(var=v, class=varclass, type=type, nobs=ntotal, nvalues=result[,nvaluescol], pvalues=result[,nvaluescol]/ntotal, pmiss=result[,nmisscol]/ntotal, pvalue=result[,nvaluecol]/ntotal, pmaxcat=NA))
      if (print) cat(" CONTINUOUS ", nvarcont, " (", v, ")\n", sep="")
    }

		#--- Frequency distribution
		# This is done for CATEGORICAL and REJECTED variables
		# with at least one valid value (i.e. not all missing) and at most maxncat distinct values
		if (varclass %in% c("categorical", "rejected") && 0 < result[,nvaluescol] && result[,nvaluescol] <= maxncat) {
			tab = as.data.frame( table(dat[,v], useNA="ifany") )
			tab$prop = tab$Freq / sum(tab$Freq)
			tab = rename.vars(tab, from=c("Var1", "Freq"), to=c("value", "freq"), info=FALSE)

			# Compute the totals row
			totals = data.frame(var=v,
													index="--TOTAL--",
													value=v,							# We set the variable name as the value in the totals so that we can plot all the variables together in Excel!
													freq=sum(tab$freq),
													prop=NA)

			# Rank values and Targets' penetration
			if (target2Flag) {
				# Cross tabulation with target2
				xtab = crosstab(tab, totals, dat[,v], dat[,target2], event, result[,nvaluescol], "Target2", "rank2")
				tab = xtab$tab
				totals = xtab$totals
			}
			if (targetFlag) {
				# Cross tabulation with target
				xtab = crosstab(tab, totals, dat[,v], dat[,target], event, result[,nvaluescol], "Target", "rank")
				tab = xtab$tab
				totals = xtab$totals
			} else {
				# Create the rank variable by ranking the number of cases from larger to smaller
				# Note that rank2 still contains the ranking of the target2 variable if any was given
				tab$rank = result[,nvaluescol] - rank(tab$freq) + 1

				if (!target2Flag && type %in% c("character", "factor") ) {
					# Sort values by rank of number of cases when the variable is character or factor
					tab = tab[ order(tab$rank), ]
				}

				#-- Totals
				# Totals row
				totals$rank = NA
			}

			# Update variable values so that they are numbered and easily filtered in Excel (with ':')
			tab$index = rownames(tab)
			tab$var = v

			# For non-rejected variables...
			# Check max proportion in categorical value to see if the variable should be rejected,
			# and if not, add the variable to the categorical variable output data frame
			if (varclass != "rejected") {
				propmax = max(tab$prop)
				if (propmax > maxpropcat) {
					# Reject the variable
					varclass = "rejected"
					rejected = 1
					nvarrej = nvarrej + 1
					df.varrej = rbind(df.varrej, data.frame(var=v, class=varclass, type=type, nobs=ntotal, nvalues=result[,nvaluescol], pvalues=result[,nvaluescol]/ntotal, pmiss=result[,nmisscol]/ntotal, pvalue=result[,nvaluecol]/ntotal, pmaxcat=propmax))
					if (print) cat(" --> REJECTED ", nvarrej, " (max prop. category = ", formatC(propmax, digits=2), " > ", maxpropcat, ")\n", sep="")
				} else {
					rejected = 0
					if (varclass != "rejected" && print) cat("\n")
					df.varcat = rbind(df.varcat, data.frame(var=v, class=varclass, type=type, nobs=ntotal, nvalues=result[,nvaluescol], pvalues=result[,nvaluescol]/ntotal, pmiss=result[,nmisscol]/ntotal, pvalue=result[,nvaluecol]/ntotal, pmaxcat=propmax))
				}
			} else {
			  rejected = 1
			}

			# Mark as non-relevant (rejected) the categories having less than minncat / minpropcat number of cases
			# Update the final minncat to use
			minncat = ceiling( max(minncat, minpropcat*totals$freq) )
			tab$rejected_value = ifelse(tab$freq < minncat, 1, 0)

			# Update the output tables with the frequency table for the currently analyzed variable
			tab$rejected_var = rejected
			totals$rejected_value = NA
			totals$rejected_var = rejected
			tab.freq = rbind(tab.freq[,c("var", "index", rankvars, "value", "freq", "prop", targetvars, "rejected_value", "rejected_var")],
											 tab[,c("var", "index", rankvars, "value", "freq", "prop", targetvars, "rejected_value", "rejected_var")],
											 totals[,c("var", "index", rankvars, "value", "freq", "prop", targetvars, "rejected_value", "rejected_var")]
			)
		} else if (varclass == "categorical") {
			# This means that the variable is categorical with number of distinct values > maxncat
			# => Add the character or factor variable to the data frame containing the categorical variables
			if (print) cat("\n")
			df.varcat = rbind(df.varcat, data.frame(var=v, class=varclass, type=type, nobs=ntotal, nvalues=result[,nvaluescol], pvalues=result[,nvaluescol]/ntotal, pmiss=result[,nmisscol]/ntotal, pvalue=result[,nvaluecol]/ntotal, pmaxcat=NA))
		}
  }

	#--- Summary table
	# Put together the classification of each variable into a single dataset
	if (valueFlag) {
		# Rename the pvalue column to p<value>
		summary = rbind(rename.vars(df.varcat, from="pvalue", to=pvaluestr, info=FALSE), rename.vars(df.varcont, from="pvalue", to=pvaluestr, info=FALSE), rename.vars(df.varrej, from="pvalue", to=pvaluestr, info=FALSE))
	} else {
		# Remove the pvalue column from the summary tables
		summary = rbind(remove.vars(df.varcat, pvaluestr, info=FALSE), remove.vars(df.varcont, pvaluestr, info=FALSE), remove.vars(df.varrej, pvaluestr, info=FALSE))
	}

	#--- Frequency table
  # Update the column names in the frequency table to show the actual target names
  colnames(tab.freq) = c("var", "index", rankvars, "value", "freq", "prop", targetvarnames, "rejected_value", "rejected_var")
	# Sort the table alphabetically
	tab.freq = tab.freq[ order(tab.freq[, "var"]), ]
	#--------------------------------- Process the query results --------------------------------

  return(list(summary=summary, tabfreq=tab.freq))
}
# ------------------------------------- Qualify Variables ------------------------------------------
