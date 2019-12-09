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

program bcstats0, rclass
    version 	14
    cap version 15.1 /* written in stata 15.1 but will run on stata 14.2 */ 

    #d;
    syntax, 																			
    	Surveydata(str) Bcdata(str)
    	id(namelist) 
    	ENUMerator(name) [ENUMTeam(name)] 
    	BACKchecker(name) [BCTeam(name)]
    	[t1vars(namelist) t2vars(namelist) t3vars(namelist)] 
    	okrange(str)
    	[ttest(namelist) Level(real -1) signrank(namelist) prtest(namelist) RELiability(namelist)] 
    	[showid(str)] 
    	[KEEPSUrvey(namelist) keepbc(namelist) full FILEname(str) replace] 
    	[EXCLUDENum(numeric) EXCLUDEStr(str) EXCLUDEMISSing LOwer UPper NOSymbol TRim]
    	[surveydate(str) bcdate(str)]
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
		
		* temp datasets and vars
		tempfile _sdata _bdata _mdata _diffs _cdata _enumdata _enumteamdata _bcerdata _bcerteamdata _checks _fmdata

		qui {
			* import only relevant variables in survey dataset
			use `id' `t1vars' `t2vars' `t3vars' `enumerator' `enumteam'  ///
				`ttest' `signrank' `prtest' `reliability' `keepsurvey'	 ///
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

			* change string variables if applicable
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
				unab checkvars: `ttest' `prtest' `signrank' `reliability'
				loc checkvars: list uniq checkvars
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
				`ttest' `signrank' `prtest' `reliability' `keepbc'	 	///
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
			foreach var of varlist `t1vars' `t2vars' `t3vars' `ttest' `signrank' `prtest' `reliability' `keepbc' {
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

			unab admin:			`id' `enumerator' `enumteam' `backchecker' `bcteam'
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

				keep `admin' `keepsurvey' `bc_keepbc' `var' _bc`var'
				
				* generate variable to mark type
				gen _vtype = cond(`:list var in t1vars', "type 1", cond(`:list var in t2vars', "type 2", "type 3"))
				
				* Mark variables that need to be compared
				if "`nomiss'" ~= "" gen _compared = !missing(`var') & !missing(_bc`var')
				else gen _compared = 1

				* generate variable to mark if values are different
				gen _vdiff = `var' ~= _bc`var' if _compared == 1
				gen _comp_comment = ""
				
				* For numeric vars: 
					* Check that the variable has an okrange
					* Check that there is at least one difference. 
					* Apply okrange. Only display okrange 
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
									replace _comp_comment = "okrange is " + string(_okmin) + " to " + string(_okmax) if _compared

									continue, break
								}
							}

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

				keep `admin' `keepsurvey' `bc_keepbc' _v* _survey* _backcheck* _seq* _compared _comp_comment
				
				append using `_diffs'
				save `_diffs', replace
			}

			* rename variables in comparison data
			ren (_vtype _vvar _vvlab _survey _backcheck) ///
				(type variable label survey backcheck)

			cap ren (_surveylab _backchecklab _comp_comment) ///
					(surveylabel backchecklabel comment)			

			* export comparison/differences
			gen result = cond(_vdiff == ., "not compared", cond(!_vdiff, "not different", "different"))

			cap gen surveylabel = ""
			cap gen backchecklabel = ""
			
			if "`full'" == "" keep if _vdiff == 1

			order `id' `enumerator' `enumteam' `backchecker' `bcteam' variable label type survey surveylabel backcheck backchecklabel result `keepsurvey' `bc_keepbc'
			keep `id' `enumerator' `enumteam' `backchecker' `bcteam' variable label type survey surveylabel backcheck backchecklabel result `keepsurvey' `bc_keepbc'
			
			save `_cdata'
			
			export excel `id' `enumerator' `enumteam' `backchecker' `bcteam' variable label type survey surveylabel backcheck backchecklabel result `keepsurvey' ///
				using "`filename'", sheet("comparison") `replace' first(var) cell(B3)
			
			if "`bc_keepbc'" ~= "" {
				
				unab exp_vars: `id' `enumerator' `enumteam' `backchecker' `bcteam' variable label type survey surveylabel backcheck backchecklabel result `keepsurvey'
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
			* mata: format_comparison("`filename'", "comparison", `:word count `id'', `:word count `keepsurvey'', `:word count `keepbc'')
			
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


			* Export Test
			use `_fmdata', clear
			postfile postchecks str32 variable str80 label double(tt_sN tt_bN tt_sM tt_bM tt_d tt_p)
			foreach var in `checkvars' {
				ttest `var' == _bc`var'
				
				* post postchecks	("`var'") ("`:var label `var''") (`r(N_1)') (`r(N_2)') (`r(mu_1)') (`r(mu_2)') ()
				
				loc sN `r(N_1)'
				loc bN `r(N_2)'
				loc sM `r(mu_1)'
				loc bM `r(mu_2)'
				loc p   `r(p_l)'

				

			}
			
		}	

end

* program to remove symbols, trim and change cases of string values
program define change_str
	syntax varlist [, nosymbol trim upper lower]

	ds, has(type string)
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

end

* create_stats: program to aggregate and create stats for enum and bcer
program define create_stats, rclass
	syntax using/, enum(name) type(name) enumdata(string) compared(name) different(name) enumlabel(string) [bc]

	use `using', clear

	collapse (sum) compared = `compared' differences = `different', by (`enum' `type')
	replace `type' = trim(itrim(subinstr(`type', "type ", "", 1)))
	reshape wide compared differences, i(`enum') j(`type') str

	merge 1:1 `enum' using `enumdata', nogen assert(match)

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

	return local type1 = `type1'
	return local type1 = `type2'
	return local type1 = `type3'

end


* format_comparison: formats comparison sheet
* format_enumstats: formats enumerator statistics sheet
* adjust_column_width: adjust column width of excel workbook using datset in memory
mata:
mata clear
void format_comparison(string scalar filename, string scalar sheetname, real scalar id_cnt, real scalar keepsv_cnt, real scalar keepbc_cnt)
{

	class xl scalar b
	real scalar column_width, columns, ncols, nrows, i, colmaxval

	ncols = st_nvar()
	nrows = st_nobs() + 2

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
	b.set_left_border((3, nrows + 1), id_cnt + 1, "medium")
	b.set_right_border((3, nrows + 1), id_cnt + 1, "medium")
	b.set_right_border((3, nrows + 1), id_cnt + 3, "medium")
	b.set_right_border((3, nrows + 1), id_cnt + 6, "medium")
	b.set_right_border((3, nrows + 1), id_cnt + 8, "medium")
	b.set_right_border((3, nrows + 1), id_cnt + 10, "medium")
	b.set_right_border((3, nrows + 1), id_cnt + 11, "medium")	
	b.set_row_height(1, 1, 10)
	b.set_column_width(1, 1, 1)

	if (ncols > (id_cnt + 11)) {
		b.set_right_border((3, nrows + 1), id_cnt + 11 + keepsv_cnt, "medium")
		b.set_right_border((3, nrows + 1), ncols, "medium")

		if (keepsv_cnt > 0) {
			if (keepsv_cnt > 1) {
				b.set_sheet_merge(sheetname, (2, 2), (id_cnt + 12, id_cnt + 11 + keepsv_cnt))
			}
			b.put_string(2, id_cnt + 12, "survey keeplist")
			b.set_horizontal_align(2, id_cnt + 12, "center")
		}
		
		if (keepbc_cnt > 0) {
			if (keepbc_cnt > 1) {
				b.set_sheet_merge(sheetname, (2, 2), (id_cnt + 12 + keepsv_cnt, ncols))
			}
			b.put_string(2, id_cnt + 12 + keepsv_cnt, "backcheck keeplist")
			b.set_horizontal_align(2, id_cnt + 12 + keepsv_cnt, "center")
		}
		b.set_border(2, (id_cnt + 12, ncols), "medium")
		b.set_font_bold((2, 3), (2, ncols), "on")
	}

	b.close_book()
}


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

end
