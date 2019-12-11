*! version 2.2 Innovations for Poverty Action

/*
This update to bcstats includes work previously done by Chris boyer and the TI network 
at Innovations for Poverty Action. 

bcstats was originally programmed by:
Matt White (Innovations for Poverty Action)

Todo:
	- okrange
		-- parse okrange -- done
		-- include syntax check -- done
	- test
		-- ttest
		-- prtest
		-- stability test
	- exports sheets:
		-- Comparison
		-- enumerator error rates
		-- back checker error rates
		-- test
			-- ttest
			-- prtest
			-- stability test
			
			* export all test to the same page with min, mean, max, sd for survey and bc
			
Wishlist:
	-- dialog
	-- excel inputs file
	-- fuzzy match strings (preferable write new command, alternative use matchit, reclink or strgroup)

*/

cap program drop ipabcstats
cap program drop change_str
cap program drop create_stats

program ipabcstats, rclass
    version 	14.2
    cap version 15.1 /* written in stata 15.1 but will run on stata 14.2 */ 
	set graphics off
   
    #d;
    syntax, 																			
    	Surveydata(str) Bcdata(str)
    	id(namelist) 
    	ENUMerator(name) [ENUMTeam(name)] 
    	BACKchecker(name) [BCTeam(name)]
		[t1vars(namelist) t2vars(namelist) t3vars(namelist)] 
   	[okrange(str) NODIFFNum(numlist) NODIFFStr(string asis)]
   	[ttest(namelist) signrank(namelist) prtest(namelist) RELiability(namelist) Level(real -1)] 
   	[showid(str)] 
   	[KEEPSUrvey(namelist) keepbc(namelist) full FILEname(str) replace] 
   	[EXCLUDENum(numlist) EXCLUDEStr(str asis) EXCLUDEMISSing LOwer UPper NOSymbol TRim]
   	surveydate(name) bcdate(name)
    ;
	#d cr			
		* check syntax
		* check that at least one variable is specified in t1, t2 or t3
		if "`t1vars'`t2vars'`t3vars'" == "" {
			disp as err "option level incorrectly specified: must specify, at minimum, one of options t1vars(), t2vars(), or t3vars()"
			ex 198
		}

		* check level
		if `level' ~= -1 & "`ttest'`prtest'" ~= "" {
			di as err "option level must be specified with option ttest or option prtest"
			ex 198
		}
				 

		* parse okrange
		if "`okrange'" ~= "" {
			loc okrange = subinstr("`okrange'", " ", "", .)
			
			* count the number combinations
			loc comb_cnt = length("`okrange'") -  length(subinstr("`okrange'", "]", "", .))
			while length("`okrange'") > 0 {
				loc okrcomb = substr("`okrange'", 1, strpos("`okrange'", "]"))
				gettoken okrvar okrcomb: okrcomb, parse([)

				* Check that combo has "[", "," and "[" inspecified order
				cap assert (strpos("`okrcomb'", "[") > 0) & (strpos("`okrcomb'", ",") > strpos("`okrcomb'", "[")) & (strpos("`okrcomb'", "]") > strpos("`okrcomb'", ","))
				if _rc {
					di as err `"option okrange() incorrectly specified: range "`okrcomb'" not allowed"'
					ex 198
				}
				* check that range was specified
				else if strpos("`okrvar'", ",") > 0 {
					di as err `"option okrange() incorrectly specified: variable list "`okrvar'" not allowed"'
					ex 198
				}
				
				* gen okrmin and okrange
				loc okrmin = substr("`okrcomb'", strpos("`okrcomb'", "[") + 1, strpos("`okrcomb'", ",") - strpos("`okrcomb'", "[") - 1)
				loc okrmax = substr("`okrcomb'", strpos("`okrcomb'", ",") + 1, strpos("`okrcomb'", "]") - strpos("`okrcomb'", ",") - 1)
				* check that range specified meets requirement.
				* ie. minumum must be prefixed by "-", at least minimum or max must be specified
				if "`okrmin'" == "" & "`okrmax'" == "" {
					di as err `"option okrange() incorrectly specified: range "`okrcomb'" not allowed"'
					ex 198
				}
				else if ("`okrmin'" ~= "" & !regexm("`okrmin'", "\-|^(0)")) | ("`okrmax'" ~= "" & regexm("`okrmax'", "\-")) {
					di as err `"option okrange() incorrectly specified: range "`okrcomb'" not allowed"'
					ex 198
				}

				* substr with 0 if min or max is not specified 
				loc okrmin = cond("`okrmin'" == "", "0", "`okrmin'")
				loc okrmax = cond("`okrmax'" == "", "0", "`okrmax'")
				
				* aggregate okrvar, okrmin and okrmax
				loc okrvars = trim(itrim("`okrvars' `okrvar'")) 
				loc okrmins = trim(itrim("`okrmins' `okrmin'"))
				loc okrmaxs = trim(itrim("`okrmaxs' `okrmax'"))
				
				loc okrange = subinstr("`okrange'", "`okrvar'`okrcomb'", "", 1)
				loc okrange = subinstr("`okrange'", ",", "", 1)		
			}
		}

		* check specifications in nodiff vs exclude
		if "`nodiffnum'" ~= "" & "`excludenum'" ~= "" {
			loc nv_both: list nodiffnum | excludenum
			if wordcount(`"`nodiffnum' `excludenum'"') > wordcount("`nv_both'") {
				di as err `"value(s) "`nv_both'" not allowed in both nodiffnum() and excludenum()"'
				ex 198
			}
		}

		loc nodiffnum_list = subinstr(trim(itrim("`nodiffnum'")), " ", ",", .)
		
		if `"`nodiffstr'"' ~= "" & `"`excludestr'"' ~= "" {
			loc nd_rest `"`nodiffstr'"'
			loc c = 1
			while `"`nd_rest'"' ~= "" {
				gettoken nd_val nd_rest: nd_rest
				loc ex_rest `"`excludestr'"'
				while `"`ex_rest'"' ~= "" {
					gettoken ex_val ex_rest: ex_rest
					if "`nd_val'" == "`ex_val'" {
						di as err `"value(s) "`ex_val'" not allowed in both nodiffstr() and excludestr()"'
						ex 198
					}
				}

				loc ++c
				if `c' > 6 {
					disp as err "expression too long. You can only specify a maximum of 6 strings with option nodiffstr()"
					ex 130
				}

				loc nodiffstr_list = cond(`c' == 1, "`nd_val'", "`nodiffstr_list'" + ", " + "`nd_val'")
			}
		}
		
		* temp datasets and vars
		tempfile _sdata _bdata _mdata _diffs _cdata _enumdata _enumteamdata _bcerdata _bcerteamdata _checks _fmdata _bconly

		qui {
			* import only relevant variables in survey dataset
			use `id' `t1vars' `t2vars' `t3vars' `enumerator' `enumteam'  ///
				`ttest' `signrank' `prtest' `reliability' `keepsurvey' `surveydate' ///
				using `surveydata', clear 

			* check that datsets is unique on id
			isid `id'
			if _rc == 459 {
				disp as err `"variable(s) "`id'" does not uniquely identify the observations in survey data"'
				ex 459
			} 

			* check that no variable is prefixed with _bc
			cap ds _bc*
			if !_rc {
				disp as err "variable `:word 1 of `r(varlist)'' has illegal prefix _bc in survey data"
				ex 198
			}

			* check survey and bbdates
			foreach opt in surveydate bcdate {
				cap confirm numeric var `opt'
				if _rc == 7 {
					di as err "Variable `opt' in option `opt'() must be a date formatted variable"
					ex 7
				}
			}

			* change string variables if applicable
			unab t1vars: `t1vars'
			unab t2vars: `t2vars'
			unab t3vars: `t3vars'
			unab tvars: `t1vars' `t2vars' `t3vars'
			change_str `tvars', `nosymbol' `lower' `upper' `trim'

			* expand okrange variables
			if "`okrvars'" ~= "" {
				cap unab okrvars_list: `okrvars'
				if _rc == 111 {
					foreach item in `okrvars' {
						cap unab check: item
						if _rc == 111 {
							di as err "variable `item' specified in okrange() not found in survey data"
							ex 111
						}
					}
				}
			}

			* check that okrange variables is specified as t1, t2 or t3
			* check the comparison variables
			foreach var of loc rangevars {
				if !`:list var in tvars' {
					di as err "option okrange() incorrectly specified: `var' not type 1, type 2, or type 3 variable"
					ex 198
				}
			}

			* check that stability and reliability test vars are specified as t2 and t3 vars
			* stability checks
			
			if "`ttest'`prtest'`signrank'`reliability'" ~= "" {
				if "`t2vars'`t3vars'" ~= "" {
					unab t2t3vars: `t2vars' `t3vars'
					foreach opt in ttest prtest signrank reliability {
						if "``opt''" ~= "" {
							unab `opt'vars: ``opt''
							foreach var of varlist ``opt'vars' {
								if !`:list var in t2t3vars' {
									di as err "variable `var' in option `opt' must be specified as t2vars() or t2vars() variables"
									ex 198
								}

								* check that variable is numeric
								cap confirm numeric var `var'
								if _rc == 7 {
									di as err "variable `var' in option `opt' must be a numeric variable"
									ex 7
								}								
							}
						}
					}
				}
				else {
					foreach opt in ttest prtest signrank reliability { 
						di as err "option `opt' must be specified with option t2vars() or t3vars() variables"
						ex 198
					}
				}

				* unab check vars
				if "`ttest'" ~= "" unab ttest: `ttest'
				if "`prtest'" ~= "" unab prtest: `prtest'
				if "`signrank'" ~= "" unab signrank: `signrank'
				unab checkvars: `ttest' `prtest' `signrank' `reliability'
				loc checkvars: list uniq checkvars

				* check that vars specified in ttest, prtest and signrank are mutualy exclusive
				if wordcount("`ttest' `prtest' `signrank'") > wordcount("`checkvars'") {
					loc ttest_prtest: list ttest & prtest
					loc ttest_signrank: list ttest & signrank
					loc prtest_signrank: list prtest & signrank

					if wordcount("`ttest_prtest'") > 0 {
						di as err `"variable(s) "`ttest_prtest'" cannot be specified in both ttest and prtest"'
						ex 198
					}
					if wordcount("`ttest_signrank'") > 0 {
						di as err `"variable(s) "`ttest_signrank'" cannot be specified in both ttest and signrank"'
						ex 198
					}
					if wordcount("`prtest_signrank'") > 0 {
						di as err `"variable(s) "`prtest_signrank'" cannot be specified in both prtest and signrank"'
						ex 198
					}
				}

			}
			
			* unab survey list
			if "`keepsurvey'" 	~= "" unab keepsurvey   :	`keepsurvey'

			save `_sdata'

			* create and save data for enumerators
			keep `enumerator'
			bys `enumerator': gen surveys = _N
			bys `enumerator': keep if _n == 1

			save `_enumdata'

			* creare enumerator team statistics
			if "`enumteam'" ~= "" {
				use `_sdata', clear
				keep `enumteam'
				bys `enumteam': gen surveys = _N 
				bys `enumteam': keep if _n == 1
				save `_enumteamdata'
			}
			
			
			* import only relevant variables in bcdata
			use `id' `t1vars' `t2vars' `t3vars' `backchecker' `bcteam'  ///
				`ttest' `signrank' `prtest' `reliability' `keepbc' `bcdate'	 	///
				using `bcdata', clear

			* check that datsets is unique on id
			isid `id'
			if _rc == 459 {
				disp as err `"variable(s) "`id'" does not uniquely identify the observations in backcheck data"'
				exit 459
			}

			* change str
			change_str `tvars', `nosymbol' `lower' `upper' `trim'

			* expand okrange variables in bc data
			if "`okrvars'" ~= "" {
				cap unab okrvars_list: `okrvars'
				if _rc == 111 {
					foreach item in `okrvars' {
						cap unab check: item
						if _rc == 111 {
							di as err "variable `item' specified in okrange() not found in backcheck data"
							ex 111
						}
					}
				}
			}

			* add _bc prefix to backcheck dataset
			foreach var of varlist `t1vars' `t2vars' `t3vars' `ttest' `signrank' `prtest' `reliability' `keepbc' `bcdate' {
				* check that variables is not prefixed by _bc
				if regexm("`var'", "^(_bc)") {
					disp as error "variable `var' has illegal prefix _bc in backcheck data"
					ex 198
				}
				
				* check that variale length is not greater than 29
				else if `=length("`var'")' > 29 {
					disp as err `"variable `var' is too long. Rename variable"'
				}
				* prefix the backcheck var with _bc
				else {
					cap confirm var `var'
					if !_rc {
						ren `var' _bc`var'
					}
					if `:list var in keepbc' {
						loc bc_keepbc = trim(itrim("`bc_keepbc' _bc`var'"))
					}
				}
			}

			* add prefix to bcdate local
			if "`bcdate'" ~= "" loc bcdate _bc`bcdate'

			save `_bdata' 

			* create and save data for backcheckers
			keep `backchecker'
			bys `backchecker': gen backchecks = _N
			bys `backchecker': keep if _n == 1

			save `_bcerdata'

			* creare enumerator team statistics
			if "`bcteam'" ~= "" {
				use `_bdata', clear
				keep `bcteam'
				bys `bcteam': gen backchecks = _N 
				bys `bcteam': keep if _n == 1
			}
			
			* merge datasets
			use `_sdata', clear
			merge 1:1 `id' using `_bdata', gen (_mergebc)
			count if _mergebc == 1
			loc survey_only `r(N)'
			count if _mergebc == 2
			loc bc_only `r(N)'
			
			if "`ttest'`prtest'`signrank'`reliability'" ~= "" save `_fmdata'

			keep if inlist(_mergebc, 2, 3)


			unab admin:	`id' `enumerator' `enumteam' `backchecker' `bcteam' `surveydate' `bcdate'
			save `_mdata', replace

			keep if _mergebc == 2
			save `_bconly', emptyok

			use `_mdata', clear
			keep if _mergebc == 3
			save `_mdata', replace 

			* keep data of number of survey back checked by enumerator and enumteam
			keep `enumerator'
			bys `enumerator': gen backchecks = _N
			bys `enumerator': keep if _n == 1
			merge 1:1 `enumerator' using `_enumdata', nogen
			order `enumerator' surveys backchecks
			save `_enumdata', replace

			if "`enumteam'" ~= "" {
				use `_mdata', clear
				keep `enumteam'
				bys `enumteam': gen backchecks = _N
				bys `enumteam': keep if _n == 1
				merge 1:1 `enumteam' using `_enumteamdata', nogen
				order `enumteam' surveys backchecks
				save `_enumteamdata', replace
			}

			* foreach variable compare and save comparison in long format
			clear
			save `_diffs', emptyok
			loc i 1
			foreach var in `tvars' {
				use `_mdata', clear

				keep `admin' `keepsurvey' `bc_keepbc' `var' _bc`var' `surveydate' `bcdate'
				
				* generate variable to mark type
				gen _vtype = cond(`:list var in t1vars', "type 1", cond(`:list var in t2vars', "type 2", "type 3"))
				
				* Mark variables that need to be compared
				if "`excludemissing'" ~= "" gen _compared = !missing(`var') & !missing(_bc`var')
				else gen _compared = 1

				* check change to not compared for values included EXCLUDENum
				cap confirm numeric var `var' 
				if !_rc {
					if "`excludenum'" ~= "" {
						loc exn_cnt = wordcount("`excludenum'")
						forval x = 1/`exn_cnt' {
							loc exn_val = word("`excludenum'", `x')
							replace _compared = 0 if `var' == `exn_val' | _bc`var' == `exn_val'
						}
					}
				}
				else {
					if `"`excludestr'"' ~= "" {
						local rest `"`excludestr'"'
						while `"`rest'"' ~= "" {
							gettoken exs_val rest: rest
							replace _compared = 0 if `var' == "`exs_val'" | _bc`var' == "`exs_val'"
						}

						gettoken val rest: rest,  
					}
				}

				* generate variable to mark if values are different
				gen _vdiff = `var' ~= _bc`var' if _compared == 1
				
				* For numeric vars: 
				* Check that the variable has an okrange
				* Check that there is at least one difference. 
				* Apply okrange.
				* apply nodiffnum
				cap confirm numeric var `var'
				if !_rc {
					* check that the variable has an okrange
					if `:list var in okrvars_list' {
						* check for range combination of var
						forval j = 1/`=wordcount("`okrvars'")' {
							loc okr_item = word("`okrvars'", `j') 
							unab okr_item_list: `okr_item'
							
							if `:list var in okr_item_list' {

								loc min = word("`okrmins'", `j') 
								loc max = word("`okrmaxs'", `j') 
								
								* apply minimum and max okranges. 
								* if relative, apply percentage
								if regexm("`min'", "%$") {
									loc perc = subinstr("`min'", "%", "", .)
									loc perc = abs(float(`perc')/100)
									gen _okmin = `var' - (`perc'*`var')

								}
								else gen _okmin = `min'
								if regexm("`max'", "%$") {
									loc perc = subinstr("`max'", "%", "", .)
									loc perc = abs(float(`perc')/100)
									gen _okmax = `var' + (`perc'*`var')
								}

								else gen _okmax = `max'

								* replace comparison
								replace _vdiff = 0 if _bc`var' >= float(_okmin) & _bc`var' <= float(_okmax) & !missing(_bc`var')

								continue, break
							}
						}

					}

					* apply nodiff
					if "`nodiffnum'" ~= "" {
						replace _vdiff = 0 if inlist(`var', `nodiffnum_list') & inlist(_bc`var', `nodiffnum_list')
					}
				}
				else {
					* apply nodiff for string variables
					if `"`nodiffstr'"' ~= "" {
						local rest "`nodiffstr_list'"
						while `"`rest'"' ~= "" {
							gettoken nds_val rest: rest, parse(,)
							replace _vdiff = 0 if `var' == "`nds_val'" & _bc`var' == "`nds_val'"

							loc ex_rest "`nodiffstr_list'"
							while `"`ex_rest'"' ~= "" {
								gettoken ndx_val ex_rest: ex_rest, parse(,)
								replace _vdiff = 0 if inlist("`nds_val'", "`ndx_val'",  `var') &  inlist("`nds_val'", "`ndx_val'",  _bc`var')
							}
						}

						gettoken val rest: rest, parse(,) 
					}
				}

				* generate variable to hold variable name
				gen _vvar = "`var'"

				* generate variable to hold variable label
				gen _vvlab = "`:var label `var''"

				* generate vars to hold survey and bc values
				cap confirm string var `var'
				if !_rc {
					gen _survey    = `var'
					gen _backcheck = _bc`var' 
				}
				else {
					tostring `var', gen (_survey) format(%100.0f)
					tostring _bc`var', gen (_backcheck) format(%100.0f)
					cap decode `var'	, gen (_surveylab)
					cap decode _bc`var'	, gen (_backchecklab)
				}

				gen _seq 	= `i'
				loc ++i
				gen _seqid 	= _n 

				keep `admin' `keepsurvey' `bc_keepbc' _v* _survey* _backcheck* _seq* _compared `surveydate' `bcdate'

				append using `_diffs'
				save `_diffs', replace
			}


			* rename variables in comparison data
			ren (_vtype _vvar _vvlab _survey _backcheck) ///
				(type variable label survey backcheck)

			cap ren (_surveylab _backchecklab) ///
					(surveylabel backchecklabel)			

			* create days difference variable
			gen _surveyday = dofc(`surveydate')
			gen _bcday = dofc(`bcdate')
			format _surveyday _bcday %td
			gen days = _bcday - _surveyday
					
			* add average difference between survey and backcheck
			bysort `id' : gen first = _n
			sum days if first == 1
			loc days_diff : piece 1 4 of  "`r(mean)'"

			* export comparison/differences
			gen result = cond(_vdiff == ., "not compared", cond(!_vdiff, "not different", "different"))

			cap gen surveylabel = ""
			cap gen backchecklabel = ""

			foreach name of varlist survey surveylabel backcheck backchecklab `id' `enumerator' `enumteam' `backchecker' `bcteam' `keepsurvey' {
				lab var `name' "`name'"
			}

			order `id' `enumerator' `enumteam' `backchecker' `bcteam' variable label type survey surveylabel ///
			 backcheck backchecklabel result `keepsurvey' `bc_keepbc' `surveydate' `bcdate' days ///
			 _surveyday _vdiff
			
			keep `id' `enumerator' `enumteam' `backchecker' `bcteam' variable label type survey surveylabel ///
			backcheck backchecklabel result `keepsurvey' `bc_keepbc' `surveydate' `bcdate' days ///
			_surveyday _vdiff

			save `_cdata'

			* Create summary sheet
			encode type, gen(vartype)

			* collapse to surveydate and type
			collapse (count) valcount = _vdiff (sum) _vdiff, by(vartype _surveyday)
			

			keep _surveyday vartype _vdiff valcount 

			* reshape to table	
			reshape wide _vdiff valcount , i(_surveyday) j(vartype)


			*create daily, weekly, or monthly graph
			sum _surveyday
			loc mindate = `r(min)'
			loc maxdate = `r(max)'
			loc count = `r(max)' - `r(min)'

			if `count' <= 30 {
				gen days = _n
				loc unit = "days"
				loc titleunit "Daily"
			}

			else {

				if `count' > 210 {
					loc unit "months"
					loc numberofdays 30
					loc titleunit "Monthly"
				}

				else {
					loc unit "weeks"
					loc numberofdays 7
					loc titleunit "Weekly"
				}
				
				loc units = ceil((`maxdate' - `mindate')/`numberofdays')
				gen `unit' = .
				forval i = 1/`units' {
					replace `unit' = `i' if _surveyday >= `mindate'
					loc mindate = `mindate' + `numberofdays'

				}
	
				collapse (sum) valcount* _vdiff*, by(`unit')
			}


			forval i = 1/3 {
				gen error_rate`i' = _vdiff`i'/valcount`i', after(_vdiff`i')
				lab var error_rate`i' "Type `i'"
			}


			egen valcounttotal = rowtotal(valcount1 valcount2 valcount3)
			egen _vdifftotal = rowtotal(_vdiff1 _vdiff2 _vdiff3)
			gen error_rate_total = _vdifftotal / valcounttotal
			lab var error_rate_total "Total" 


			graph twoway connected error_rate* `unit', title("Error Rates (`titleunit')") scheme(s1color) name(summary)
			graph export errorrates.png, width(460) replace name(summary)
			drop error_rate*
			reshape long _vdiff valcount, i(`unit') j(vartype)

			collapse (sum) valcount _vdiff, by(vartype)

			set obs 4
			replace vartype = 4 in 4
			lab define vartype 4 "All", add
			foreach var in valcount _vdiff {
				qui sum `var'
				replace `var' = `r(sum)' in 4
			}
			g error_rate = _vdiff / valcount

			gen varcount = ., after(vartype)
			forval i = 1/3 {
				replace varcount = `: word count `t`i'vars'' if _n == `i'
			}

			replace varcount = `: word count `tvars'' in 4

			g empty = "", after(vartype)

			lab var varcount "# variables"
			lab var valcount "# values"
			lab var _vdiff "differences"
			lab var error_rate "Error rate (%)"
			lab var vartype "Type"

			export excel using "`filename'", sheet("summary") `replace' first(varlabel) cell(C11)
			
			mata: add_summary_formatting("`filename'", "summary", "`c(current_date)'")

			use `_cdata', clear

			if "`full'" == "" keep if _vdiff == 1
			save `_cdata', replace

			lab var `surveydate' "`surveydate'"
			loc lab = substr("`bcdate'", 4, .)
			lab var `bcdate' "`lab'"

			export excel `id' `enumerator' `enumteam' `backchecker' `bcteam' variable label type survey surveylabel backcheck backchecklabel result `keepsurvey' `surveydate' `bcdate' days ///
				using "`filename'", sheet("comparison") first(varl) cell(B4)
			
			if "`bc_keepbc'" ~= "" {
				
				unab exp_vars: `id' `enumerator' `enumteam' `backchecker' `bcteam' variable label type survey surveylabel backcheck backchecklabel result `keepsurvey' `surveydate' `bcdate' days
				loc range_cnt = wordcount("`exp_vars'")

				mata: st_local("alphavar", invtokens(numtobase26(`=`range_cnt'+2')))

				keep `bc_keepbc'
				ren _bc* *
		
				export excel using "`filename'", sheet("comparison", modify) first(var) cell(`alphavar'3)
			}
			
			use `_cdata', clear
			gen _a = "", before(`id')
			unab id: `id'

			* mata: adjust_column_width("`filename'", "comparison")

			loc idcount `:word count `id''
			loc enumcount `:word count `enumerator' `enumteam''
			loc bcer `:word count `backchecker' `bcteam''
			loc keeps `: word count `keepsurvey''
			loc keepb `:word count `keepbc''

			mata: format_comparison("`filename'", "comparison")

			* create and export enumerator and bcer statistics
			create_stats using "`_diffs'", enum(`enumerator') enumdata("`_enumdata'") type(_vtype) compared(_compared) different(_vdiff) enumlabel(enumerator) 
			export excel using "`filename'", sheet("enumerator stats", replace) first(varl) cell(B3)

			if "`enumteam'" ~= "" {
				create_stats using "`_diffs'", enum(`enumteam') enumdata("`_enumteamdata'") type(_vtype) compared(_compared) different(_vdiff) enumlabel(enum team)
				export excel using "`filename'", sheet("enumerator team stats", replace) first(varl) cell(B3)
			}

			create_stats using "`_diffs'", bc enum(`backchecker') enumdata("`_bcerdata'") type(_vtype) compared(_compared) different(_vdiff) enumlabel(backchecker)
			export excel using "`filename'", sheet("backchecker stats", replace) first(varl) cell(B3)

			if "`bcteam'" ~= "" {
				create_stats using "`_diffs'", bc enum(`backchecker') enumdata("`_bcerteamdata'") type(_vtype) compared(_compared) different(_vdiff) enumlabel(bc team)
				export excel using "`filename'", sheet("backchecker team stats", replace) first(varl) cell(B3)
			}

		}	

end

* program to remove symbols, trim and change cases of string values
program define change_str
	syntax varlist [, nosymbol trim upper lower]

	ds, has(type string)
	if `:word count `r(varlist)'' > 0 {
		foreach var of varlist `r(varlist)' {
			cap confirm str var `var' 
			if !_rc {
				* remove symbols
				if "`nosymbol'" ~= "" {
					foreach i of numlist 33/47 58/64 91/96 123/126 {
						replace `var' = subinstr(`var', char(`i'), " ", .)
					}
				}

				* change case to lower
				if "`lower'`upper'" ~= "" {
					if "`lower'" ~= "" & "`upper'" ~= "" {
						disp as err "options lower and upper are mutually exclusive"
						ex 198
					}
					else if "`lower'" ~= "" replace `var' = lower(`var')
					else replace `var' = upper(`var')
				}

				* trim string variables
				if "`trim'" ~= "" replace `var' = trim(itrim(ustrltrim(`var')))
			}
		}
	}

end

* create_stats: program to aggregate and create stats for enum and bcer
program define create_stats, rclass
	syntax using/, enum(name) type(name) enumdata(string) compared(name) different(name) enumlabel(string) [bc]

	use `using', clear

	collapse (sum) compared = `compared' differences = `different', by (`enum' `type')
	replace `type' = trim(itrim(subinstr(`type', "type ", "", 1)))
	reshape wide compared differences, i(`enum') j(`type') str

	merge 1:1 `enum' using `enumdata', nogen keep(master match)

	if "`bc'" == "" {
		order `enum' surveys backchecks
		* generate back check percentage and backcheck error rates
		gen backcheck_percent = round((backchecks/surveys) * 100, 0.01), after(backchecks)
		label var backcheck_percent "% backchecked"
	}
	else order `enum' backchecks

	* generate percentages for each type
	forval i = 1/3 {
		cap confirm var compared`i'
		if !_rc {
			gen error_rate`i' = round((differences`i'/compared`i') * 100, 0.01), after(differences`i')
			label var compared`i' "# compared"
			label var differences`i' "# different"
			label var error_rate`i' "% different"

			loc type`i' 1
		}
		else loc type`i' 0
	}

	* generate aggregated values
	egen compared = rowtotal(compared*)
	label var compared "# compared"
	egen differences = rowtotal(differences*)
	label var differences "# different"
	gen error_rate = round((differences/compared) * 100, 0.01)
	label var error_rate "% different"
	label var `enum' "`enumlabel'"
	
	ds `enum', not
	recode `r(varlist)' (. = 0) // recode missing/no comparisons to 0

	return local type1 = `type1'
	return local type1 = `type2'
	return local type1 = `type3'

end


* format_comparison: formats comparison sheet
* format_enumstats: formats enumerator statistics sheet
* adjust_column_width: adjust column width of excel workbook using datset in memory
mata:
mata clear

void format_enumstats(string scalar filename, string scalar sheetname, real scalar type1, real scalar type2, real scalar type3)
{

	class xl scalar b
	real scalar column_width, columns, ncols, nrows, i, colmaxval, current_col

	ncols = st_nvar()
	nrows = st_nobs() + 2
	current_col = 6

	ncols
	nrows

	b = xl()

	b.load_book(filename)
	b.set_sheet(sheetname)
	b.set_mode("open")

	b.set_sheet_gridlines(sheetname, "off")
	b.set_border((3, nrows + 1), (2, ncols), "thin")
	b.set_top_border(3, (2, ncols), "medium")
	b.set_bottom_border(3, (2, ncols), "medium")
	b.set_bottom_border(nrows + 1, (2, ncols), "medium")
	b.set_left_border((3, nrows + 1), 2, "medium")
	b.set_right_border((3, nrows + 1), 2, "medium")
	b.set_right_border((3, nrows + 1), 5, "medium")
	b.set_column_width(1, 1, 1)
	b.set_row_height(1, 1, 10)

	if (type1 = 1) {
		b.set_sheet_merge(sheetname, (2, 2), (current_col, current_col + 2))
		b.set_left_border((3, nrows + 1), current_col + 3, "medium")
		b.put_string(2, current_col, "type 1")
		b.set_horizontal_align(2, current_col, "center")

		current_col = current_col + 3
	}

	if (type2 = 1) {
		b.set_sheet_merge(sheetname, (2, 2), (current_col, current_col + 2))
		b.set_left_border((3, nrows + 1), current_col + 3, "medium")
		b.put_string(2, current_col, "type 2")
		b.set_horizontal_align(2, current_col, "center")

		current_col = current_col + 3
	}

	if (type3 = 1) {
		b.set_sheet_merge(sheetname, (2, 2), (current_col, current_col + 2))
		b.set_left_border((3, nrows + 1), current_col + 3, "medium")
		b.put_string(2, current_col, "type 3")
		b.set_horizontal_align(2, current_col, "center")

		current_col = current_col + 3
	}

	b.set_sheet_merge(sheetname, (2, 2), (current_col, current_col + 2))
	b.set_left_border((3, nrows + 1), current_col + 3, "medium")
	b.put_string(2, current_col, "all")
	b.set_horizontal_align(2, current_col, "center")

	b.set_font_bold((2, 3), (6, current_col + 3), "on")

	b.close_book()
}

void adjust_column_width(string scalar filename, string scalar sheetname)
{

	class xl scalar b
	real scalar column_width, columns, ncols, nrows, i, colmaxval

	ncols = st_nvar()
	nrows = st_nobs() + 2

	b = xl()

	b.load_book(filename)
	b.set_sheet(sheetname)
	b.set_mode("open")

	for (i = 1;i <= ncols;i ++) {
		namelen = strlen(st_varname(i))
		
		if (st_isnumvar(i)) {
			colmaxval = colmax(st_data(., i))
			
			if (colmaxval == 0) {
				collen = 0
			}
			else {
				collen = log(colmax(st_data(., i)))
			}
		}
		else {
			collen = colmax(strlen(st_sdata(., i)))
		}
		
		if (namelen > collen) {
			column_width = namelen + 1
		}
		else {
			column_width = collen + 1
		}
		
		if (column_width > 101) {
			column_width = 101
		}	
		
		b.set_column_width(i, i, column_width)

		b.close_book()
	}
}

void add_summary_formatting(string scalar filename, string scalar sheetname, string scalar date) 
{

	class xl scalar b
	numeric scalar border

	b = xl()
	
	b.load_book(filename)
	b.set_sheet(sheetname)
	b.set_mode("open")
	b.set_sheet_gridlines(sheetname, "off")

	b.put_string(2, 3, "Back Check Analysis")
	b.put_string(4, 3, "Average Days between Survey and Backcheck: " + st_local("days_diff") ) 


	b.put_string(6, 3, "Date: ")
	b.put_string(6, 5, date)

	b.put_string(8, 3, "Survey Data:")
	b.put_string(8, 5, st_local("surveydata"))

	b.put_string(9, 3, "Backcheck Data:")
	b.put_string(9, 5, st_local("bcdata"))

	b.set_font(2, 3, "Calibri", 18)
	b.set_font_bold((2, 9), 3, "on")
	b.set_font_italic((6, 9), 4, "on")
	b.set_column_width(1, 2, 2)
	b.set_column_width(9, 9, 2)
	b.set_column_width(4, 5, 5)
	b.set_column_width(5, 8, 12)


	b.set_horizontal_align((11, 15), (3, 8), "center")
	b.set_number_format((12, 15), 8, "percent_d2")

	b.set_sheet_merge(sheetname, (2, 2), (3, 8))
	b.set_sheet_merge(sheetname, (4, 4), (3, 8))


	for (i = 6; i<=15; i++) {
		b.set_sheet_merge(sheetname, (i, i), (3, 4))		
	}
	
	b.set_horizontal_align((6, 9), 3, "left")
	b.set_font_bold((11, 15), 3, "on")
	b.set_font_bold(11, (5, 8), "on")
	b.set_horizontal_align((2, 4), 3, "center")
	
	border = 16

	if (strtoreal(st_local("count")) > 1) {
		b.put_picture(18, 3, "errorrates.png")
		border = 35
	}

	b.set_bottom_border(border, (2, 9), "thin")
	b.set_left_border((2, border), 2, "thin")
	b.set_right_border((2, border), 9, "thin")
	b.set_top_border(2, (2, 9), "thin")
	
	b.close_book()
}



void format_comparison(string scalar filename, string scalar sheetname)
{

	class xl scalar b
	real scalar idpos, enumpos, bcerpos, varpos, spos, bcpos, respos, datepos, keepspos, keepbcpos
	real matrix positions 

	b = xl()

	b.load_book(filename)
	b.set_sheet(sheetname)
	b.set_mode("open")
	b.set_sheet_gridlines(sheetname, "off")
	idpos = 2 + strtoreal(st_local("idcount"))
	enumpos = idpos + strtoreal(st_local("enumcount"))
	bcerpos = enumpos + strtoreal(st_local("bcer"))
	varpos = bcerpos + 3
	spos = varpos + 2
	bcpos = spos + 2
	respos = bcpos + 1
	datepos = respos + 3
	keepspos = datepos + strtoreal(st_local("keeps"))
	keepbcpos = keepspos + strtoreal(st_local("keepb"))

	nrows = 2821 

	positions = (idpos\enumpos\bcerpos\varpos\spos\bcpos\respos\datepos\keepspos\keepbcpos)
	positions
	
	b.set_left_border((4, nrows), 2, "medium")
	
	for (i = 1; i<=10; i++) {
		b.set_left_border((4, 2821), positions[i], "medium")
	}



	b.close_book()

}


end

