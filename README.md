# ipabcstats
New version of bcstats with summary sheet, variable error rates, variable type error rates, and collapsed options

## Installation instructions:
```net install ipabcstats, from("https://raw.githubusercontent.com/PovertyAction/ipabcstats/master") replace```

## Help file
<pre>
<b><u>Title</u></b>
<p>
    <b>ipabcstats</b> -- Compare survey and back check data, producing an Excel
        output of comparisons and error rates.
<p>
<p>
<a name="syntax"></a><b><u>Syntax</u></b>
<p>
        <b>ipabcstats,</b> <b><u>s</u></b><b>urveydata(</b><i>filename</i><b>)</b> <b><u>b</u></b><b>cdata(</b><i>filename</i><b>)</b> <b>id(</b><i>varlist</i><b>)</b>
          <b><u>enum</u></b><b>erator(</b><i>varlist</i><b>)</b> <b><u>back</u></b><b>checker(</b><i>varlist</i><b>)</b> <b><u>file</u></b><b>name(</b><i>filename</i><b>)</b>
          <b>surveydate(</b><i>varlist</i><b>)</b> <b>bcdate(</b><i>varlist</i><b>)</b> [<i>options</i>]
<p>
<p>
    <i>options</i>                  Description
    -------------------------------------------------------------------------
    Main
    * <b><u>s</u></b><b>urveydata(</b><i>filename</i><b>)</b>   the survey data
    * <b><u>b</u></b><b>cdata(</b><i>filename</i><b>)</b>       the back check data
    * <b>id(</b><i>varlist</i><b>)</b>            the unique ID(s)
    * <b><u>enum</u></b><b>erator(</b><i>varlist</i><b>)</b>    the enumerator ID
    * <b><u>back</u></b><b>checker(</b><i>varlist</i><b>)</b>   the backchecker ID
    * <b><u>file</u></b><b>name(</b><i>filename</i><b>)</b>     save output file as <i>filename</i>
    * <b>surveydate(</b><i>varlist</i><b>)</b>    the survey date variable (%tc format)
    * <b>bcdate(</b><i>varlist</i><b>)</b>        the survey date variable (%tc format)
<p>
    Comparison variables
    + <b>t1vars(</b><i>varlist</i><b>)</b>        the list of type 1 variables
    + <b>t2vars(</b><i>varlist</i><b>)</b>        the list of type 2 variables
    + <b>t3vars(</b><i>varlist</i><b>)</b>        the list of type 3 variables
<p>
    Enumerator checks
      <b><u>enumt</u></b><b>eam(</b><i>varname</i><b>)</b>      display the overall error rates of all
                               enumerator teams; <i>varname</i> in survey data is
                               used
      <b><u>bct</u></b><b>eam(</b><i>varname</i><b>)</b>        display the overall error rates of all back
                               check teams; <i>varname</i> in back check data is
                               used
      <b>showid(</b><i>integer</i>[%]<b>)</b>     display unique IDs with at least <i>integer</i>
                               differences or at least an <i>integer</i>% error rate
                               or <i>integer</i> differences if <i>%</i> is not included;
                               default is <b>showid(30%)</b>
<p>
    Stability checks
      <b>ttest(</b><i>varlist</i><b>)</b>         run paired two-sample mean-comparison tests for 
                               <i>varlist</i> in the back check and survey data
                               using <b>ttest</b>
      <b>prtest(</b><i>varlist</i><b>)</b>        run two-sample test of equality of proportions
                               in the back check and survey data for
                               dichotmous variables in <i>varlist</i> using <b>prtest</b>
      <b><u>l</u></b><b>evel(</b><i>#</i><b>)</b>               set confidence level for <b>ttest</b> and <b>prtest</b>;
                               default is <b>level(95)</b>
      <b>signrank(</b><i>varlist</i><b>)</b>      run Wilcoxon matched-pairs signed-ranks tests
                               for <i>varlist</i> in the back check and survey data
                               using <b>signrank</b>
<p>
    Reliability checks
      <b><u>rel</u></b><b>iability(</b><i>varlist</i><b>)</b>   calculate the simple response variance (SRV) and
                               reliability ratio for type 2 and 3 variables
                               in <i>varlist</i>
<p>
    Comparisons
      <b><u>keepsu</u></b><b>rvey(</b><i>varlist</i><b>)</b>    include <i>varlist</i> in the survey data in the
                               comparisons data set
      <b>keepbc(</b><i>varlist</i><b>)</b>        include <i>varlist</i> in the back check data in the
                               comparisons data set
      <b>full</b>                   include all comparisons, not just differences
      <b><u>nol</u></b><b>abel</b>                do not use value labels
      <b>replace</b>                overwrite existing file
<p>
    Options
      <b>okrange(</b><i>varname</i> <i>range</i> [, <i>varname</i> <i>range</i> ...]<b>)</b>
                               do not count a value of <i>varname</i> in the back
                               check data as a difference if it falls within
                               <i>range</i> of the survey data
      <b><u>nodiffn</u></b><b>um(</b><i>numlist</i><b>)</b>      do not count back check responses that equal <i>#</i>
                               as differences
<p>
      <b><u>nodiffs</u></b><b>tr(</b><i>"string"</i> [ <i>"string"</i> ...]<b>)</b>
                               do not count back check responses that equal
                               <i>string(s)</i> as differences. Use quotations
                               around each individual string.
<p>
      <b><u>excluden</u></b><b>um(</b><i>numlist</i><b>)</b>     do not compare back check responses that equal
                               any number in <i>numlist</i>.
      <b><u>excludes</u></b><b>tr(</b><i>"string"</i> [ <i>"string"</i> ...]<b>)</b>
                               do not compare back check responses that equal
                               any string.
      <b><u>excludemiss</u></b><b>ing</b>          do not compare back check responses that are
                               missing. Includes extended missing values for
                               numeric variables [., .a, .b, ..., .z] and
                               blanks "" for string variables.
      <b><u>lo</u></b><b>wer</b>                  convert all string variables to lower case
                               before comparing
      <b><u>up</u></b><b>per</b>                  convert all string variables to upper case
                               before comparing
      <b><u>nos</u></b><b>ymbol</b>               replace symbols with spaces in string variables
                               before comparing
      <b><u>tr</u></b><b>im</b>                   remove leading or trailing blanks and multiple,
                               consecutive internal blanks in string
                               variables before comparing
    -------------------------------------------------------------------------
    * <b>surveydata()</b>, <b>bcdata()</b>, and <b>id()</b> are required.
    * <b>t1vars()</b>, <b>t2vars()</b>, or <b>t3vars()</b> is required.
<p>
<p>
<a name="description"></a><b><u>Description</u></b>
<p>
    <b>bcstats</b> compares back check data and survey data, producing a data set of
    comparisons.  It completes enumerator checks for type 1 and type 2
    variables and stability checks for type 2 and type 3 variables.
<p>
<p>
<a name="remarks"></a><b><u>Remarks</u></b>
<p>
    The GitHub repository for <b>bcstats</b> is here.  Previous versions may be
    found there: see the tags.
<p>
<p>
<a name="options"></a><b><u>Options</u></b>
<p>
        +----------------------+
    ----+ Comparison variables +---------------------------------------------
<p>
<a name="type1"></a>    <b>t1vars(</b><i>varlist</i><b>)</b> specifies the list of type 1 variables.  Type 1 variables
        are expected to stay constant between the survey and back check, and
        differences may result in action against the enumerator. Display
        variables with high error rates and complete enumerator checks.  See
        the Innovations for Poverty Action back check manual for more on the
        three types.
<p>
<a name="type2"></a>    <b>t2vars(</b><i>varlist</i><b>)</b> specifies the list of type 2 variables.  Type 2 variables
        may be difficult for enumerators to administer.  For instance, they
        may involve complicated skip patterns or many examples.  Differences
        may indicate the need for further training, but will not result in
        action against the enumerator.  Display the error rates of all
        variables and complete enumerator and stability checks.  See the
        Innovations for Poverty Action back check manual for more on the
        three types.
<p>
<a name="type3"></a>    <b>t3vars(</b><i>varlist</i><b>)</b> specifies the list of type 3 variables.  Type 3 variables
        are variables whose stability between the survey and back check is of
        interest.  Differences will not result in action against the
        enumerator.  Display the error rates of all variables and complete
        stability checks.  See the Innovations for Poverty Action back check
        manual for more on the three types.
<p>
        +------------------+
    ----+ Stability checks +-------------------------------------------------
<p>
    <b>level(</b><i>#</i><b>)</b> specifies the confidence level, as a percentage, for confidence
        intervals calculated by <b>ttest</b> and <b>prtest</b>.  The default is <b>level(95)</b>
        or as set by <b>set level</b>.
<p>
        +----------------------+
    ----+ Comparisons data set +---------------------------------------------
<p>
    <b>keepsurvey(</b><i>varlist</i><b>)</b> specifies that variables in <i>varlist</i> in the survey
        data are to be included in the output file.  <b>keepbc(</b><i>varlist</i><b>)</b>
        specifies that variables in <i>varlist</i> in the back check data are to be
        included in the output file.
<p>
    <b>nolabel</b> specifies that survey and back check responses are not to be
        value-labeled in the comparisons data set.  Variables specified
        through <b>keepsurvey</b> or <b>keepbc</b> are also not value-labeled.
<p>
        +---------+
    ----+ Options +----------------------------------------------------------
<p>
    <b>okrange(</b><i>varname</i> <i>range</i> [, <i>varname</i> <i>range</i> ...]<b>)</b> specifies that a value of 
        <i>varname</i> in the back check data will not be counted as a difference if
        it falls within <i>range</i> of the survey data.  <i>range</i> may be of the form
        <b>[</b><i>-x</i>, <i>y</i><b>]</b> (absolute) or <b>[</b><i>-x%</i>, <i>y%</i><b>]</b> (relative).
<p>
    <b>excludenum(</b><i>numlist</i><b>)</b> specifies that back check responses that equal any
        value in <i>numlist</i> will not be compared.  These responses will not
        affect error rates and will be marked as "not compared" if you use
        the <i>full</i> option. Otherwise, they will not appear in the output file.
<p>
    <b>excludestr(</b><i>"string"</i> [, <i>"string"</i> ...]<b>)</b> specifies that back check responses
        that equal any string in this list will not be compared.  These
        responses will not affect error rates and will be marked as "not
        compared" if you use the <i>full</i> option. Otherwise, they will not appear
        in the output file. Be sure to keep each string in its own
        quotations, especially if there are spaces within a string.
<p>
    <b>excludemissing</b> specifies that back check responses that are missing will
        not be compared. This uses the <b>[R] missing</b> command, so any extended
        missing value [., .a, .b, ..., .z] for numeric variables, and blanks
        ("") for string variables. Used when the back check data set contains
        data for multiple back check survey versions.
<p>
    <b>nodiffstr(</b><i>"string"</i> [, <i>"string"</i> ...]<b>)</b> specifies that if a back check
        response equal any string in the list, it will not be counted as
        difference, regardless of what the survey response is.
<p>
    <b>excludenum(</b><i>numlist</i><b>)</b> specifies that if a back check response equals any
        number in <i>numlist</i>, it will not be counted as difference, regardless
        of what the survey response is.
<p>
    <b>nosymbol</b> replaces the following characters in string variables with a
        space before comparing:  <b>. , ! ? ' / ; : ( ) ` ~ @ # $ % ^ &amp; * - _ +</b>
        <b>= [ ] { } | \ " &lt; &gt;</b>
<p>
    <b>trim</b> removes leading or trailing blanks and multiple, consecutive
        internal blanks before comparing.  If <b>nosymbol</b> is specified, this
        occurs after symbols are replaced with a space.
<p>
<p>
<a name="examples"></a><b><u>Examples</u></b>
<p>
    Assume that missing values were not asked in the back check survey
    version.
        <b>bcstats, surveydata(bcstats_survey) bcdata(bcstats_bc) id(id) ///</b>
            <b>okrange(gameresult [-1, 1], itemssold [-5%, 5%]) excludemissing</b>
                <b>///</b>
            <b>t1vars(gender) enumerator(enum) enumteam(enumteam)</b>
                <b>backchecker(bcer) ///</b>
            <b>t2vars(gameresult) signrank(gameresult) ///</b>
            <b>t3vars(itemssold) ttest(itemssold) ///</b>
            <b>surveydate(submissiondate) bcdate(submissiondate)</b>
                <b>keepbc(comments) keepsurvey(comments) full replace</b>
<p>
<p>
<a name="results"></a><b><u>Stored results</u></b>
<p>
    <b>bcstats</b> saves the following in <b>r()</b>:
<p>
    Scalars        
      <b>r(showid)</b>           1 if <b>showid()</b> displayed number of IDs over the
                            threshold specified in <b>showid</b>
      <b>r(bc_only)</b>          1 number of IDs only in backcheck data
      <b>r(total_rate)</b>       1 total error rate
      <b>r(avd)</b>              1 average days between survey and backcheck
      <b>r(survey)</b>           1 number of survey observations
      <b>r(bc)</b>               1 number of backcheck observations
<p>
    Matrices       
      <b>r(enum)</b>             the total error rates of all enumerators
      <b>r(enum1)</b>            the type 1 variable error rates of all enumerators
      <b>r(enum2)</b>            the type 2 variable error rates of all enumerators
      <b>r(enum3)</b>            the type 3 variable error rates of all enumerators
      <b>r(backchecker)</b>      the total error rates of the back checkers
      <b>r(backchecker1)</b>     the type 1 variable error rates of the back
                            checkers
      <b>r(backchecker2)</b>     the type 2 variable error rates of the back
                            checkers
      <b>r(backchecker3)</b>     the type 3 variable error rates of the back
                            checkers
      <b>r(enumteam)</b>         the total error rates of the enumerator teams
      <b>r(enumteam1)</b>        the type 1 variable error rates of the enumerator
                            teams
      <b>r(enumteam2)</b>        the type 2 variable error rates of the enumerator
                            teams
      <b>r(enumteam3)</b>        the type 3 variable error rates of the enumerator
                            teams
<p>
      <b>r(bcteam)</b>           the total error rates of the back checker teams
      <b>r(bcteam1)</b>          the type 1 variable error rates of the back checker
                            teams
      <b>r(bcteam2)</b>          the type 2 variable error rates of the back checker
                            teams
      <b>r(bcteam3)</b>          the type 3 variable error rates of the back checker
                            teams
<p>
      <b>r(var)</b>              the error rates of all variables
      <b>r(var1)</b>             the error rates of all type 1 variables
      <b>r(var2)</b>             the error rates of all type 2 variables
      <b>r(var3)</b>             the error rates of all type 3 variables
      <b>r(ttest)</b>            the results of <b>ttest</b> for selected variables
      <b>r(ttest2)</b>           the results of <b>ttest</b> for type 2 variables
      <b>r(ttest3)</b>           the results of <b>ttest</b> for type 3 variables
      <b>r(signrank)</b>         the results of <b>signrank</b> for selected variables
      <b>r(signrank2)</b>        the results of <b>signrank</b> for type 2 variables
      <b>r(signrank3)</b>        the results of <b>signrank</b> for type 3 variables
      <b>r(prtest)</b>           the results of <b>prtest</b> for selected variables
      <b>r(prtest2)</b>          the results of <b>prtest</b> for type 2 variables
      <b>r(prtest3)</b>          the results of <b>prtest</b> for type 3 variables
<p>
      <b>r(enum_bc)</b>          percentage of surveys backchecked for all
                            enumerators
      <b>r(rates)</b>            error rates by variable type
      <b>r(rates_unit)</b>       error rates by variable type over time
<p>
<p>
<a name="references"></a><b><u>References</u></b>
<p>
<a name="back_check_manual"></a>    Innovations for Poverty Action Back Check Manual
<p>
<p>
<a name="acknowledgements"></a><b><u>Acknowledgements</u></b>
<p>
    Hana Scheetz Freymiller of Innovations for Poverty Action conceived of
    the three variable types. bcstats was originally written by Matthew White
    with edits from Christopher Boyer, and the code and structure of this
    command draws heavily from their work.
<p>
<a name="author"></a><b><u>Author</u></b>
<p>
    Ishmail Azindoo Baako
    Rosemarie Sandino
<p>
    For questions or suggestions, submit a GitHub issue or e-mail
    researchsupport@poverty-action.org.
<p>
<p>
<b><u>Also see</u></b>
<p>
    Help:  <b>[R] ttest</b>, <b>[R] prtest</b>, <b>[R] signrank</b>
<p>
</pre>
