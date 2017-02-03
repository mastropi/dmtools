#' Qualify variables into categorical, continuous, and rejected
#
#' Qualifies variables in:
#' - categorical (character or few categories)
#' - continuous (numeric with many distinct values)
#' - rejected (categorical variables with 0 or 1 distinct value, with a category with too many values
#' --including missing values)
# Needs package sqldf, gdata
#library(sqldf)			--> for sqldf()
#library(gdata)			--> for rename.vars()
#library(dmtools)		--> for checkVariables(), parseVariables()
qualify_vars <- QualifyVars <- function(dat, target=NULL, target2=NULL, vars=NULL, event=1, maxncat=25, maxpropcat=0.95, maxpropmiss=0.50, print=TRUE) {
  # Parameters
  # dat								 # Data frame containing the data for analysis.
  # target						 # String with the target variable to analyze. It must be a column of the dataset.
  # target2						 # String with a second target variable to analyze. It must be a column of the dataset.
  # vars							 # String or array containing the dataset variables to analyze. When NULL, all variables in the dataset are used.
  # maxncat = 25       # Maximum number of categories to consider a variable categorical.
  # maxpropcat = 0.95  # Maximum proportion in category to consider a variable as useful.
  # maxpropmiss = 0.50 # Maximum proportion of missing values to consider a variable as useful.
  # print = TRUE		   # Whether to show messages.

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
  tab.freq = data.frame(var=character(0), index=character(0), value=character(0), freq=numeric(0), prop=numeric(0), rejected=numeric(0))
  if (!missing(target) && !is.null(target)) {
    targetFlag = TRUE
    tab.freq$Target = numeric(0)
  }
  if (!missing(target2) && !is.null(target2)) {
    target2Flag = TRUE
    tab.freq$Target2 = numeric(0)
  }
  df.varcont <- df.varcat <- df.varrej <- data.frame(var=character(0), class=character(0), type=character(0), nvalues=numeric(0), pvalues=numeric(0), pmiss=numeric(0), pmaxcat=numeric(0))

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
      df.varrej = rbind(df.varrej, data.frame(var=v, class="rejected", type=type, nvalues=result$nvalues, pmiss=result$nmiss/ntotal, pmaxcat=NA))
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
                            value="",
                            freq=sum(tab$freq),
                            prop=NA)

        # Targets' penetration
        targetvars <- targetvarnames <- NULL
        if (targetFlag) {
          xtab = table(dat[,v], dat[,target])
          tab$Target = xtab[tab$value, as.character(event)] / tab$freq
            ## Note the use of as.character() to enclose event because otherwise the event must be alwasy numeric
            ## In addition, if event=1, it should refer to the column with label "1" and NOT to column number 1!

          # Totals row
          totals$Target = weighted.mean(tab$Target, tab$freq)

          # Update the target variable names to construct the output frequency table
          targetvars = "Target"
          targetvarnames = target  # Recall that target is already a string!
        }
        if (target2Flag) {
          xtab2 = table(dat[,v], dat[,target2])
          tab$Target2 = xtab2[tab$value, as.character(event)] / tab$freq
            ## Note the use of as.character() to enclose event because otherwise the event must be alwasy numeric
            ## In addition, if event=1, it should refer to the column with label "1" and NOT to column number 1!

          # Totals row
          totals$Target2 = weighted.mean(tab$Target2, tab$freq)

          # Update the target variable names to construct the output frequency table
          targetvars = c(targetvars, "Target2")
          targetvarnames = c(targetvarnames, target2) # Recall that target2 is already a string!
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
          df.varrej = rbind(df.varrej, data.frame(var=v, class="rejected", type=type, nvalues=result$nvalues, pvalues=1.0, pmiss=result$nmiss/ntotal, pmaxcat=propmax))
          if (print) cat(" --> REJECTED ", nvarrej, " (max prop. category = ", formatC(propmax, digits=2), " > ", maxpropcat, ")\n", sep="")
        } else {
          rejected = 0
          if (print) cat("\n")
	        df.varcat = rbind(df.varcat, data.frame(var=v, class="categorical", type=type, nvalues=result$nvalues, pvalues=1.0, pmiss=0.0, pmaxcat=propmax))
        }

        # Update the output tables with the frequency table for the currently analyzed variable
        tab$rejected = rejected
        totals$rejected = rejected
        tab.freq = rbind(tab.freq[,c("var", "index", "value", "freq", "prop", targetvars, "rejected")],
                         tab[,c("var", "index", "value", "freq", "prop", targetvars, "rejected")],
                         totals[,c("var", "index", "value", "freq", "prop", targetvars, "rejected")]
        )
      }
    } else {
      nvarcont = nvarcont + 1
      df.varcont = rbind(df.varcont, data.frame(var=v, class="continuous", type=type, nvalues=result$nvalues, pvalues=result$nvalues/ntotal, pmiss=result$nmiss/ntotal, pmaxcat=NA))
      if (print) cat(" CONTINUOUS ", nvarcont, " (", v, ")\n", sep="")
    }
  }

  # Update the column names in the frequency table to show the actual target names
  colnames(tab.freq) = c("var", "index", "value", "freq", "prop", targetvarnames, "var_rejected")

	# Put together the classification of each variable into a single dataset
	summary = rbind(df.varcat, df.varcont, df.varrej)

  return(list(summary=summary, tabfreq=tab.freq))
}
# ------------------------------------- Qualify Variables ------------------------------------------
