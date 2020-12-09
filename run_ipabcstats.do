* ------------------------------------------------------------------------------
* filename	: test_ipabcstats_beta.do
* purpose	: Test beta version of ipabcstats.do
* 			  https://github.com/PovertyAction/ipabcstats
* date		: 15th October 2020
* -----------------------------------------------------------------------------*/

* INSTALLATION
* ------------
	net install ipabcstats, from("https://raw.githubusercontent.com/PovertyAction/ipabcstats/master") replace 

* DESCRIPTION
/* -----------
	
	* datasets
		
		surveydata() 		: survey data
		bcdata()			: back check data
	
	* required
	
		id()				: [varlist] unique ID. eg. hhid
		
		enumerator()  		: [varname] enumerator or enumerator id variable in survey. eg. enum_id
		backchecker() 		: [varname] back checker or backchecker id variable in backcheck survey eg. bcer_id
		
		surveydate() 		: [varname] date variable in survey eg. starttime (must be in %tc or %td format)
		bcdate()			: [varname] date variable in back check (must be in %tc or %td format)
		
		t1vars()			: [varlist] list of type 1 variables
		t2vars()			: [varlist] list of type 2 variables 
		t3vars()			: [varlist] list of type 3 variables 
		
		filename()			: [filename] save as filename

	* options (optional)
	
		enumteam() 			: [varname] enumerator team variable in survey. eg. enum_team.  
		bcteam() 			: [varname] back checker team variable in back check survey. eg. bcer_team
		
		okrange() 			: [numeric varlist]
							
							Do not count a value of varname in the back check data as a difference 
							if it falls within range of the survey data
							
							how to specify okrange:
							
							okrange(varlist [-min, max], ...)
							
							NB: min must be prefixed with a negative sign
							NB: min and max can be absolute values ie. [-5, 10], relative values [-10%, 10%] or both [-5%, 15]
							
						
		nodiffnum() 		: [numlist] do not count back check responses that equal numlist (for numeric variables) eg. -777, -555
		nodiffstr() 		: [string] do not count back check responses that equal strings (for string variables) eg. "NA"
		
		ttest()				: [varlist] run paired two-sample mean-comparison tests for varlist in the back check and survey data using ttest eg. age
		prtest()			: [varlist] run two-sample test of equality of proportions in the back check and survey data for dichotmous variables if varlist using prtest 
		signrank() 			: [varlist] run Wilcoxon matched-pairs signed-ranks tests for varlist in the back check and survey data using signrank
		level() 			: [num] set confidence level for ttest and prtest; default is level(95)
		reliability() 		: [varlist] calculate the simple response variance (SRV) and reliability ratio for type 2 and 3 variables in varlist (t2 & t3 vars only)
		
		showid() 			: [num or percentage] display unique IDs with at least integer differences or at least an integer% error rate; default is showid(30%). eg. 10 or 20%
				
		keepsurvey() 		: [varlist] include varlist in the survey data in the comparisons data set
		keepbc()			: [varlist] include varlist in the back check data in the comparisons data set
		full 				: include all comparisons in report, not just differences. Indicate "full" or leave blank
		
		excludenum()  		: [numlist] do not compare back check responses that equal # (for numeric variables)
		excludestr()  		: [string] do not compare back check responses that equal string (for string variables)
		excludemissing	 	: do not compare if survey or backcheck value is missing (input text excludemissing to use this option)  
		
		lower				: convert text to lower case before comparison
		upper				: convert text to upper case before comparison
		nosymbol			: remove symbols before comparison
		trim				: trim strings before comparison
		nolabel				: do not export value labels		
	
*/

ipabcstats, 				///
      surveydata("")  		///
      bcdata("")  			///
      id()	              	///
	  surveydate()   		///
	  bcdate()		  		///
      enumerator()     		///
      enumteam()   			///
      backchecker()    		///
      bcteam()     			///
      t1vars()     			///
      t2vars()     			///
      t3vars()     			///
      ttest()       		///
      keepbc()     			///
      keepsurvey()   		///
      reliability() 		///
      filename() 			///
	  excludemissing 		///
	  okrange() 			///
      excludenum() 			///
      lower					/// 
	  nosymbol 				///
	  trim 					///
	  full 					///	
	  replace
