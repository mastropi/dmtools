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
qualify_vars <- QualifyVars <- function(
		dat,								# Data frame containing the data for analysis.
		target=NULL,				# String with the target variable to analyze. It must be a column of the dataset.
		target2=NULL,				# String with a second target variable to analyze. It must be a column of the dataset.
		vars=NULL,					# String or array containing the dataset variables to analyze. When NULL, all variables in the dataset are used.
		event=1,						# Event of interest (either numeric or character)
		maxncat=25,					# Maximum number of categories to consider a variable categorical.
		maxpropcat=0.95,		# Maximum proportion in category to consider a variable as useful.
		maxpropmiss=0.50,		# Maximum proportion of missing values to consider a variable as useful.
		minpropcat=0.01,		# Minimum proportion of cases in a category to consider it relevant. It is used to compute the actual minncat as max(minncat, minpropcat*<totalcases>)
		minncat=50,					# Minimum number of cases in a category to consider it relevant. The final minncat is computed as max(minncat, minpropcat*<totalcases>)
		print=TRUE)					# Whether to show messages.
{
  # Parse variables
  if (missing(vars) || is.null(vars)) {
    vars = colnames(dat)
  } else {
    vars = parseVariables(vars)
  }
  if (!is.null(checkVariables(dat, c(target, target2, vars)))) stop("Variables not found. Processing stops")

  # Indicators of whether target variables were passed to the function
  targetFlag = FALSE
  target2Flag = FALSE

  # Output variables
  tab.freq = data.frame(var=character(0), index=character(0), value=character(0), freq=numeric(0), prop=numeric(0), rejected_value=numeric(0), rejected_var=numeric(0))
  if (!missing(target) && !is.null(target)) {
    targetFlag = TRUE
    tab.freq$Target = numeric(0)
    tab.freq$rank = numeric(0)
  }
  if (!missing(target2) && !is.null(target2)) {
    target2Flag = TRUE
    tab.freq$Target2 = numeric(0)
    tab.freq$rank2 = numeric(0)
  }
  df.varcont <- df.varcat <- df.varrej <- data.frame(var=character(0), class=character(0), type=character(0), nobs=numeric(0), nvalues=numeric(0), pvalues=numeric(0), pmiss=numeric(0), pmaxcat=numeric(0))
		## The above columns are as follows:
		## var 			=>	variable name
		## class		=> 	variable classification ("categorical", "continuous", "rejected")
		## type 		=>	variable type ("character", "numeric")
		## nvalues	=>	numb. distinct values taken by the variable
		## pvalues	=>	prop. distinct values taken by the variable w.r.t. # observations
		## pmiss		=>	prop. missing values
		## pmaxcat	=>	prop. cases in the categorical level with largest number of cases 
  # Start process
  i = 0
  nvarcont = 0
  nvarcat = 0
  nvarrej = 0
  ntotal = nrow(dat)
  for (v in vars) {
    i = i + 1
    if (print) cat("Analyzing variable", i, "of", length(vars), ":")

    # Variable type, and number of distinct values
    type = class(dat[,v])
    result = sqldf(paste("select count(distinct(", v, ")) as nvalues, sum(case when", v, "is null then 1 else 0 end) as nmiss from dat"))

    if (result$nvalues <= 1 || result$nmiss/ntotal > maxpropmiss) {
      nvarrej = nvarrej + 1
      df.varrej = rbind(df.varrej, data.frame(var=v, class="rejected", type=type, nobs=ntotal, nvalues=result$nvalues, pvalues=result$nvalues/ntotal, pmiss=result$nmiss/ntotal, pmaxcat=NA))
      if (print) cat(" REJECTED ", nvarrej, " (", v, ")\n", sep="")
    } else if (type %in% c("character", "factor") || result$nvalues <= maxncat) {
      nvarcat = nvarcat + 1
      if (print) cat(" CATEGORICAL ", nvarcat, " (", v, ")", sep="")

      # Frequency distribution for categorical variables
      if (result$nvalues <= maxncat) {
        tab = as.data.frame( table(dat[,v]) )
        tab$prop = tab$Freq / sum(tab$Freq)
        tab = rename.vars(tab, from=c("Var1", "Freq"), to=c("value", "freq"), info=FALSE)

        # Compute the totals row
        totals = data.frame(var=v,
                            index="--TOTAL--",
                            value=v,							# We set the variable name as the value in the totals so that we can plot all the variables together in Excel!
                            freq=sum(tab$freq),
                            prop=NA)

        # Rank values and Targets' penetration
        rankvars <- "rank"
        targetvars <- targetvarnames <- NULL
        if (target2Flag) {
        	#-- Frequency table
          xtab2 = table(dat[,v], dat[,target2])
          tab$Target2 = xtab2[tab$value, as.character(event)] / tab$freq
            ## Note the use of as.character() to enclose event because otherwise the event must be alwasy numeric
            ## In addition, if event=1, it should refer to the column with label "1" and NOT to column number 1!

					# Rank the values by decreasing target average
					tab$rank2 = totals$freq - rank(tab$Target2)

					# Sort variable values by rank of target variable if the variable is character or factor
        	if (type %in% c("character", "factor") ) {
        		tab = tab[ order(tab$rank2), ]
        	}

					#-- Totals
          # Totals row
          totals$rank2 = NA
          totals$Target2 = weighted.mean(tab$Target2, tab$freq)

          # Update the rank and target variable names to construct the output frequency table
          rankvars = c(rankvars, "rank2")
          targetvars = c(targetvars, "Target2")
          targetvarnames = c(targetvarnames, target2) # Recall that target2 is already a string!
        }
        if (targetFlag) {
        	#-- Frequency table
          xtab = table(dat[,v], dat[,target])
          tab$Target = xtab[tab$value, as.character(event)] / tab$freq
            ## Note the use of as.character() to enclose event because otherwise the event must be alwasy numeric
            ## In addition, if event=1, it should refer to the column with label "1" and NOT to column number 1!

					# Rank the values by decreasing target average
					tab$rank = totals$freq - rank(tab$Target)
					
					# Sort variable values by rank of target variable if the variable is character or factor
        	if (type %in% c("character", "factor") ) {
        		tab = tab[ order(tab$rank), ]
        	}

					#-- Totals
          # Totals row
					totals$rank = NA
          totals$Target = weighted.mean(tab$Target, tab$freq)

          # Update the target variable names to construct the output frequency table
          targetvars = "Target"
          targetvarnames = target  # Recall that target is already a string!
        } else {
        	# Create the rank variable by ranking the number of cases from larger to smaller
        	# Note that rank2 still contains the ranking of the target2 variable if any was given
        	tab$rank = totals$freq - rank(tab$freq)
        	
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

        # Check max proportion in categorical value
        propmax = max(tab$prop)
        if (propmax > maxpropcat) {
          # Reject the variable
          rejected = 1
          nvarrej = nvarrej + 1
          df.varrej = rbind(df.varrej, data.frame(var=v, class="rejected", type=type, nobs=ntotal, nvalues=result$nvalues, pvalues=result$nvalues/ntotal, pmiss=result$nmiss/ntotal, pmaxcat=propmax))
          if (print) cat(" --> REJECTED ", nvarrej, " (max prop. category = ", formatC(propmax, digits=2), " > ", maxpropcat, ")\n", sep="")
        } else {
          rejected = 0
          if (print) cat("\n")
	        df.varcat = rbind(df.varcat, data.frame(var=v, class="categorical", type=type, nobs=ntotal, nvalues=result$nvalues, pvalues=result$nvalues/ntotal, pmiss=0.0, pmaxcat=propmax))
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
      } else {
      	# Add a new line after the message shown above that the variable was classified as categorical
      	if (print) cat("\n")
      }
    } else {
      nvarcont = nvarcont + 1
      if (type == "numeric") type = "double"
      df.varcont = rbind(df.varcont, data.frame(var=v, class="continuous", type=type, nobs=ntotal, nvalues=result$nvalues, pvalues=result$nvalues/ntotal, pmiss=result$nmiss/ntotal, pmaxcat=NA))
      if (print) cat(" CONTINUOUS ", nvarcont, " (", v, ")\n", sep="")
    }
  }

	#--- Categorical variables frequency table
  # Update the column names in the frequency table to show the actual target names
  colnames(tab.freq) = c("var", "index", rankvars, "value", "freq", "prop", targetvarnames, "rejected_value", "rejected_var")
	# Sort the table alphabetically
	tab.freq = tab.freq[ order(tab.freq[, "var"]), ]

	#--- Summary table
	# Put together the classification of each variable into a single dataset
	summary = rbind(df.varcat, df.varcont, df.varrej)

  return(list(summary=summary, tabfreq=tab.freq))
}
# ------------------------------------- Qualify Variables ------------------------------------------
