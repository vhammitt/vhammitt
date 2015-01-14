/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Wednesday, January 14, 2015     TIME: 2:09:19 PM
PROJECT: HammittV_SAS_project_01.14.15
PROJECT PATH: P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\vhammitt' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\vhammitt' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\vhammitt" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Variables1 FilAdult   */
%LET _CLIENTTASKLABEL='Variables1 FilAdult';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_FULLYR_2012_FILTER);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_FULLYR_2012_FILTER(label="MEPS_FULLYR_2012_FILTER") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F
      FROM EC100003.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Data Set Attributes of Variables1 FilAdult   */
%LET SYSLAST=MYDATA.MEPS_FULLYR_2012_FILTER;
%LET _CLIENTTASKLABEL='Code For Data Set Attributes of Variables1 FilAdult';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\vhammitt\Code For Data Set Attributes of Variables1 FilAdult.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 11:21:37 AM
   By task: Data Set Attributes

   Input Data: Local:MYDATA.MEPS_FULLYR_2012_FILTER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForMEPS_FULLYR_2012_);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.MEPS_FULLYR_2012_FILTER ;

RUN;






GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for MEPS Adult Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for MEPS Adult Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:04 PM
   By task: One-Way Frequencies for MEPS Adult Subset

   Input Data: Local:MYDATA.MEPS_FULLYR_2012_FILTER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MEPS_FULLYR_2012_FILTER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.SEX, T.REGION12, T.RACETHX, T.MARRY12X, T.EDRECODE, T.EMPST31, T.EMPST42, T.EMPST53, T.EDUCYR, T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42
		     , T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHECR42, T.ADHOPE42, T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42
		     , T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42, T.ADPRX42, T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42
		     , T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADWRTH42, T.K6SUM42, T.MCS42, T.PCS42, T.PHQ242, T.SFFLAG42, T.AMASST12, T.AMCHIR12, T.AMDRC12, T.AMNURS12, T.AMTHER12, T.HHTOTD12, T.IPDIS12, T.IPNGTD12, T.IPZERO12, T.CHLDP12X, T.DEPDNT12
		     , T.DIVDP12X, T.FAMINC12, T.FAMS1231, T.ERTOT12, T.INSURC12, T.PRIEU12, T.PRING12, T.PRIOG12, T.PRIS12, T.PRIV12, T.PUB12X, T.TOTEXP12, T.TOTMCD12, T.TOTMCR12, T.TOTOTH12, T.TOTPRV12, T.TOTSLF12, T.PERWT12F
	FROM MYDATA.MEPS_FULLYR_2012_FILTER(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 Adult MEPS Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES EDRECODE / MISSPRINT  SCORES=TABLE;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE;
	TABLES EDUCYR / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES AMASST12 / MISSPRINT  SCORES=TABLE;
	TABLES AMCHIR12 / MISSPRINT  SCORES=TABLE;
	TABLES AMDRC12 / MISSPRINT  SCORES=TABLE;
	TABLES AMNURS12 / MISSPRINT  SCORES=TABLE;
	TABLES AMTHER12 / MISSPRINT  SCORES=TABLE;
	TABLES HHTOTD12 / MISSPRINT  SCORES=TABLE;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE;
	TABLES IPNGTD12 / MISSPRINT  SCORES=TABLE;
	TABLES IPZERO12 / MISSPRINT  SCORES=TABLE;
	TABLES CHLDP12X / MISSPRINT  SCORES=TABLE;
	TABLES DEPDNT12 / MISSPRINT  SCORES=TABLE;
	TABLES DIVDP12X / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMS1231 / MISSPRINT  SCORES=TABLE;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES INSURC12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIEU12 / MISSPRINT  SCORES=TABLE;
	TABLES PRING12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIOG12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIS12 / MISSPRINT  SCORES=TABLE;
	TABLES PRIV12 / MISSPRINT  SCORES=TABLE;
	TABLES PUB12X / MISSPRINT  SCORES=TABLE;
	TABLES TOTEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTMCD12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTMCR12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTOTH12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTPRV12 / MISSPRINT  SCORES=TABLE;
	TABLES TOTSLF12 / MISSPRINT  SCORES=TABLE;
	TABLES PERWT12F / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Missing Variable   */
%LET _CLIENTTASKLABEL='Recode Missing Variable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_FULLYR_2012_FILTER_MANAGED);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_FULLYR_2012_FILTER_MANAGED(label="MEPS_FULLYR_2012_FILTER_MANAGED") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          /* EDU */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Education recode (recoded missing)" AS EDU, 
          /* EMPLY31 */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="Employment statues on 3/1 (recoded missing)" AS EMPLY31, 
          /* EMPLY42 */
            (CASE 
               WHEN -1 = t1.EMPST42 THEN .
               WHEN -7 = t1.EMPST42 THEN .
               WHEN -8 = t1.EMPST42 THEN .
               WHEN -9 = t1.EMPST42 THEN .
               ELSE t1.EMPST42
            END) LABEL="Employment status 4/2 (recoded missing)" AS EMPLY42, 
          /* EMPLY53 */
            (CASE 
               WHEN -7 = t1.EMPST53 THEN .
               WHEN -8 = t1.EMPST53 THEN .
               WHEN -9 = t1.EMPST53 THEN .
               ELSE t1.EMPST53
            END) LABEL="Employment status 5/3 (recoded missing)" AS EMPLY53, 
          /* MED_OFF_VIS */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="SAQ: Number of visits to a medical office for care (recoded missing)" AS MED_OFF_VIS, 
          /* CALM_PE_4 */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="SAQ 4WKS: elt calm/peaceful (recoded missing)" AS CALM_PE_4, 
          /* STAIRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="SAQ: health limits climbing stairs (recoded missing)" AS STAIRS, 
          /* HLTH_LIM_MOD_AC */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="SAQ: health limits for moderate activites (recoded missing)" AS HLTH_LIM_MOD_AC, 
          /* DOWN */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="SAQ 4WKS: felt downhearted or depressed (recoded missing)" AS DOWN, 
          /* EZ_MED_CARE */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="SAQ: easy getting needed medical care (recoded missing)" AS EZ_MED_CARE, 
          /* GEN_HEALTH */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="SAQ: health in general (recoded missing)" AS GEN_HEALTH, 
          /* HEALTH_RATE */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="SAQ: rating of health care (recoded missing)" AS HEALTH_RATE, 
          /* IMMED_CARE */
            (CASE 
               WHEN -1 = t1.ADILCR42 THEN .
               WHEN -9 = t1.ADILCR42 THEN .
               ELSE t1.ADILCR42
            END) LABEL="SAQ: illness or injury needing immediate care (recoded missing)" AS IMMED_CARE, 
          /* NO_NEED_INS */
            (CASE 
               WHEN -1 = t1.ADINSA42 THEN .
               WHEN -7 = t1.ADINSA42 THEN .
               WHEN -8 = t1.ADINSA42 THEN .
               WHEN -9 = t1.ADINSA42 THEN .
               ELSE t1.ADINSA42
            END) LABEL="SAQ: do not need health insurance (recoded missing)" AS NO_NEED_INS, 
          /* ACC_LESS_MNTL */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="SAQ 4WKS: accomplish less because of mental problems (recoded missing)" AS ACC_LESS_MNTL, 
          /* WRK_LIM_MNTL */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="SAQ 4WKS: work limited because of mental problems (recoded missing)" AS WRK_LIM_MNTL, 
          /* LOT_NRGY */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="SAQ 4WKS: had a lot of energy (recoded missing)" AS LOT_NRGY, 
          /* OVER_NO_HELP */
            (CASE 
               WHEN -1 = t1.ADOVER42 THEN .
               WHEN -7 = t1.ADOVER42 THEN .
               WHEN -8 = t1.ADOVER42 THEN .
               WHEN -9 = t1.ADOVER42 THEN .
               ELSE t1.ADOVER42
            END) LABEL="SAQ: can overcome ills without medical help (recoded missing)" AS OVER_NO_HELP, 
          /* PAIN_LIM_WRK */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="SAQ 4WKS: pain limits normal work (recoded missing)" AS PAIN_LIM_WRK, 
          /* ACC_LESS_PHYS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="SAQ 4WKS: accomplish less because of physical problems (recoded missing)" AS ACC_LESS_PHYS, 
          /* WRK_LIM_PHYS */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="SAQ 4WKS: work limited because of physical problems (recoded missing)" AS WRK_LIM_PHYS, 
          /* RISK */
            (CASE 
               WHEN -1 = t1.ADRISK42 THEN .
               WHEN -7 = t1.ADRISK42 THEN .
               WHEN -8 = t1.ADRISK42 THEN .
               WHEN -9 = t1.ADRISK42 THEN .
               ELSE t1.ADRISK42
            END) LABEL="SAQ: more likely to take risks (recoded missing" AS RISK, 
          /* ROUT_CARE */
            (CASE 
               WHEN -1 = t1.ADRTCR42 THEN .
               WHEN -8 = t1.ADRTCR42 THEN .
               WHEN -9 = t1.ADRTCR42 THEN .
               ELSE t1.ADRTCR42
            END) LABEL="SAQ: made appointment for routine medical care" AS ROUT_CARE, 
          /* SMOKE */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="SAQ: currently smoke (recoded missing)" AS SMOKE, 
          /* SPEC */
            (CASE 
               WHEN -1 = t1.ADSPEC42 THEN .
               WHEN -9 = t1.ADSPEC42 THEN .
               ELSE t1.ADSPEC42
            END) LABEL="SAQ: needed to see specialist (recoded missing)" AS SPEC, 
          /* HLTH_STP_SOC */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="SAQ 4WKS: health stopped social activity (recoded missing)" AS HLTH_STP_SOC, 
          /* MARRY */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital status (recoded missing)" AS MARRY
      FROM MYDATA.MEPS_FULLYR_2012_FILTER t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:04 PM
   By task: Table Analysis

   Input Data: Local:MYDATA.MEPS_FULLYR_2012_FILTER_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MEPS_FULLYR_2012_FILTER_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ACC_LESS_PHYS, T.ACC_LESS_MNTL, T.DOWN, T.ADPALS42, T.ADMALS42, T.ADDOWN42, T.ADCAPE42, T.CALM_PE_4, T.EDU, T.EDRECODE
	FROM MYDATA.MEPS_FULLYR_2012_FILTER_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDRECODE * EDU /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCAPE42 * CALM_PE_4 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDOWN42 * DOWN /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMALS42 * ACC_LESS_MNTL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPALS42 * ACC_LESS_PHYS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse coding   */
%LET _CLIENTTASKLABEL='Reverse coding';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.SUBSET_REVERSE);

PROC SQL;
   CREATE TABLE MYDATA.SUBSET_REVERSE(label="SUBSET_REVERSE") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          /* HLTH_LIM_MOD_ACC_R */
            (4-t1.HLTH_LIM_MOD_AC) LABEL="SAQ: health limits moderate activites (recoded missing)(reverse coded)" AS 
            HLTH_LIM_MOD_ACC_R, 
          /* STAIRS_R */
            (4-t1.STAIRS) LABEL="SAQ: health limits climbing stairs (recoded missing)(reverse coded)" AS STAIRS_R, 
          /* ACC_LESS_PHYS_R */
            (6-t1.ACC_LESS_PHYS) LABEL=
            "SAQ 4WKS: accomplish less because of physical problems (recoded missing)(reverse coded)" AS ACC_LESS_PHYS_R, 
          /* WRK_LIM_PHYS_R */
            (6-t1.WRK_LIM_PHYS) LABEL=
            "SAQ 4WKS: work limited because of physicall problems (recoded missing)(reverse coded)" AS WRK_LIM_PHYS_R, 
          /* ACC_LESS_MNTL_R */
            (6-t1.ACC_LESS_MNTL) LABEL=
            "SAQ 4WKS: accomplish less because of mental problems (recoded missing) (reverse coded)" AS ACC_LESS_MNTL_R, 
          /* WRK_LIM_MNTL_R */
            (6-t1.WRK_LIM_MNTL) LABEL=
            "SAQ 4WKS: work limited because of mental problems (recoded missing)(reverse coded)" AS WRK_LIM_MNTL_R, 
          /* DOWN_R */
            (6-t1.DOWN) LABEL="SAQ 4WKS: felt downhearted or depressed (recoded missing)(reverse coded)" AS DOWN_R, 
          /* HLTH_STP_SOC_R */
            (6-t1.HLTH_STP_SOC) LABEL="SAQ 4WKS: health stopped social activites (recoded missing)(reverse coded)" AS 
            HLTH_STP_SOC_R
      FROM MYDATA.MEPS_FULLYR_2012_FILTER_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Sum   */
%LET _CLIENTTASKLABEL='Sum';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA."Aggregate Health Score"n);

PROC SQL;
   CREATE TABLE MYDATA."Aggregate Health Score"n(label="Aggregate Health Score") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          t1.HLTH_LIM_MOD_ACC_R, 
          t1.STAIRS_R, 
          t1.ACC_LESS_PHYS_R, 
          t1.WRK_LIM_PHYS_R, 
          t1.ACC_LESS_MNTL_R, 
          t1.WRK_LIM_MNTL_R, 
          t1.DOWN_R, 
          t1.HLTH_STP_SOC_R, 
          /* AGG_HLTH_SCORE */
            
            (SUM(t1.CALM_PE_4,t1.GEN_HEALTH,t1.LOT_NRGY,t1.PAIN_LIM_WRK,t1.HLTH_LIM_MOD_ACC_R,t1.STAIRS_R,t1.ACC_LESS_PHYS_R,t1.WRK_LIM_PHYS_R,t1.ACC_LESS_MNTL_R,t1.WRK_LIM_MNTL_R,t1.DOWN_R,t1.HLTH_STP_SOC_R)) 
            LABEL="Aggregate health score by sum of SF12 after reverse coding and recode missing" AS AGG_HLTH_SCORE
      FROM MYDATA.SUBSET_REVERSE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:05 PM
   By task: List Data

   Input Data: Local:MYDATA.AGGREGATE HEALTH SCORE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.AGGREGATE HEALTH SCORE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HLTH_LIM_MOD_ACC_R, T.STAIRS_R, T.ACC_LESS_PHYS_R, T.WRK_LIM_PHYS_R, T.ACC_LESS_MNTL_R, T.WRK_LIM_MNTL_R, T.DOWN_R, T.HLTH_STP_SOC_R, T.CALM_PE_4, T.GEN_HEALTH, T.LOT_NRGY, T.PAIN_LIM_WRK, T.AGG_HLTH_SCORE
	FROM MYDATA.'AGGREGATE HEALTH SCORE'n as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Variable Coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR HLTH_LIM_MOD_ACC_R STAIRS_R ACC_LESS_PHYS_R WRK_LIM_PHYS_R ACC_LESS_MNTL_R WRK_LIM_MNTL_R DOWN_R HLTH_STP_SOC_R CALM_PE_4 GEN_HEALTH LOT_NRGY PAIN_LIM_WRK AGG_HLTH_SCORE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Aggregate Health Score   */
%LET _CLIENTTASKLABEL='Summary Statistics for Aggregate Health Score';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:05 PM
   By task: Summary Statistics for Aggregate Health Score

   Input Data: Local:MYDATA.AGGREGATE HEALTH SCORE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.AGGREGATE HEALTH SCORE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGG_HLTH_SCORE
	FROM MYDATA.'AGGREGATE HEALTH SCORE'n(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for Aggregate Health Score SF-12 MEPS Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGG_HLTH_SCORE;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Aggregate Health Score   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate Health Score';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:05 PM
   By task: Distribution Analysis for Aggregate Health Score

   Input Data: Local:MYDATA.AGGREGATE HEALTH SCORE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.AGGREGATE HEALTH SCORE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGG_HLTH_SCORE
	FROM MYDATA.'AGGREGATE HEALTH SCORE'n as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of Aggregate Health Score";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS QUANTILES;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR AGG_HLTH_SCORE;
	HISTOGRAM   AGG_HLTH_SCORE / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorize Health Score   */
%LET _CLIENTTASKLABEL='Categorize Health Score';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA."AGGREGATE_HEALTH_SCORE_CAT"n);

PROC SQL;
   CREATE TABLE MYDATA."AGGREGATE_HEALTH_SCORE_CAT"n AS 
   SELECT /* AGG_HLTH_SCORE_categorical */
            (CASE  
               WHEN t1.AGG_HLTH_SCORE <=16 
               THEN 1
               WHEN t1.AGG_HLTH_SCORE <=19 
               THEN 2
               WHEN t1.AGG_HLTH_SCORE <=27
               THEN 3
               ELSE 4
            END) LABEL="Quantile-based categories of aggregate health score" AS AGG_HLTH_SCORE_categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          t1.HLTH_LIM_MOD_ACC_R, 
          t1.STAIRS_R, 
          t1.ACC_LESS_PHYS_R, 
          t1.WRK_LIM_PHYS_R, 
          t1.ACC_LESS_MNTL_R, 
          t1.WRK_LIM_MNTL_R, 
          t1.DOWN_R, 
          t1.HLTH_STP_SOC_R, 
          t1.AGG_HLTH_SCORE
      FROM MYDATA.'AGGREGATE HEALTH SCORE'n t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis for categorizing health score   */
%LET _CLIENTTASKLABEL='Table Analysis for categorizing health score';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:06 PM
   By task: Table Analysis for categorizing health score

   Input Data: Local:MYDATA.AGGREGATE_HEALTH_SCORE_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.AGGREGATE_HEALTH_SCORE_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGG_HLTH_SCORE, T.AGG_HLTH_SCORE_categorical
	FROM MYDATA.AGGREGATE_HEALTH_SCORE_CAT(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis for categorizing health score";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES AGG_HLTH_SCORE * AGG_HLTH_SCORE_categorical /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorize MARRY EDU   */
%LET _CLIENTTASKLABEL='Categorize MARRY EDU';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.EDU_MARRY_CAT);

PROC SQL;
   CREATE TABLE MYDATA.EDU_MARRY_CAT(label="EDU_MARRY_CAT") AS 
   SELECT /* EDU_CAT */
            (CASE  
               WHEN t1.EDU=0
               THEN 0
               WHEN t1.EDU <=8
               THEN 1
               WHEN t1.EDU <=12
               THEN 2
               WHEN t1.EDU =13
               THEN 3
               WHEN t1.EDU =14
               THEN 4
               WHEN t1.EDU =15
               THEN 5
               ELSE 6
            END
            ) LABEL="Education recode categorical" AS EDU_CAT, 
          t1.AGG_HLTH_SCORE_categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          t1.HLTH_LIM_MOD_ACC_R, 
          t1.STAIRS_R, 
          t1.ACC_LESS_PHYS_R, 
          t1.WRK_LIM_PHYS_R, 
          t1.ACC_LESS_MNTL_R, 
          t1.WRK_LIM_MNTL_R, 
          t1.DOWN_R, 
          t1.HLTH_STP_SOC_R, 
          t1.AGG_HLTH_SCORE, 
          /* MARRY_CAT */
            (CASE  
               WHEN t1.MARRY =1
               THEN 1
               WHEN t1.MARRY <=4
               THEN 2
               ELSE 3
            END) LABEL="Marital status categorical" AS MARRY_CAT
      FROM MYDATA.AGGREGATE_HEALTH_SCORE_CAT t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis for MARRY and EDU categorization   */
%LET _CLIENTTASKLABEL='Table Analysis for MARRY and EDU categorization';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:06 PM
   By task: Table Analysis for MARRY and EDU categorization

   Input Data: Local:MYDATA.EDU_MARRY_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.EDU_MARRY_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDU, T.MARRY, T.MARRY_CAT, T.EDU_CAT
	FROM MYDATA.EDU_MARRY_CAT(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis for MARRY and EDU categorization";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY * MARRY_CAT /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDU * EDU_CAT /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse and bin   */
%LET _CLIENTTASKLABEL='Reverse and bin';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA."Prep for concern"n);

PROC SQL;
   CREATE TABLE MYDATA."Prep for concern"n(label="Prep for concern") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          /* TOT_EXP_CAT */
            (CASE  
               WHEN t1.TOTEXP12 <3
               THEN 0
               WHEN t1.TOTEXP12 >=1 and t1.TOTEXP12 <269
               THEN 1
               WHEN t1.TOTEXP12 >=269 and  t1.TOTEXP12 <907
               THEN 2
               WHEN t1.TOTEXP12 >=907 and  t1.TOTEXP12 <3342
               THEN 3
               ELSE 4
            END) LABEL="Total health care expenditures categorized according to codebook" AS TOT_EXP_CAT, 
          /* IMMED_CARE_R */
            (3-t1.IMMED_CARE) LABEL="SAQ: Illnes or injury needing immediate care (recoded missing)(reverse coded)" AS 
            IMMED_CARE_R, 
          /* NO_NEED_INS_R */
            (6-t1.NO_NEED_INS) LABEL="SAQ: Do not need health insurance (recoded missing)(reverse coded)" AS 
            NO_NEED_INS_R, 
          /* OVER_NO_HELP_R */
            (6-t1.OVER_NO_HELP) LABEL="SAQ: can overcome ills without medical help (recoded missing)(reverse coded)" AS 
            OVER_NO_HELP_R, 
          /* RISK_R */
            (6-t1.RISK) LABEL="SAQ: more likely to take risks (recoded missing)(reverse coded)" AS RISK_R, 
          /* ROUT_CARE_R */
            (3-t1.ROUT_CARE) LABEL="SAQ: made appointment for routine medical care (recoded missing)(reverse coded)" AS 
            ROUT_CARE_R
      FROM MYDATA.MEPS_FULLYR_2012_FILTER_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Sum   */
%LET _CLIENTTASKLABEL='Sum';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA."Concern for Health Aggregate"n);

PROC SQL;
   CREATE TABLE MYDATA."Concern for Health Aggregate"n(label="Concern for Health Aggregate") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.TOT_EXP_CAT, 
          t1.IMMED_CARE_R, 
          t1.NO_NEED_INS_R, 
          t1.OVER_NO_HELP_R, 
          t1.RISK_R, 
          t1.ROUT_CARE_R, 
          /* AGG_CONCERN */
            
            ((t1.TOT_EXP_CAT+t1.IMMED_CARE_R+t1.NO_NEED_INS_R+t1.OVER_NO_HELP_R+t1.RISK_R+t1.ROUT_CARE_R+t1.MED_OFF_VIS+t1.EZ_MED_CARE+t1.GEN_HEALTH)) 
            LABEL="Aggregate of concern for health" AS AGG_CONCERN
      FROM MYDATA.'PREP FOR CONCERN'n t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data Concern for Health Agreggate   */
%LET _CLIENTTASKLABEL='List Data Concern for Health Agreggate';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:06 PM
   By task: List Data Concern for Health Agreggate

   Input Data: Local:MYDATA.CONCERN FOR HEALTH AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.CONCERN FOR HEALTH AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.GEN_HEALTH, T.EZ_MED_CARE, T.MED_OFF_VIS, T.TOT_EXP_CAT, T.IMMED_CARE_R, T.NO_NEED_INS_R, T.OVER_NO_HELP_R, T.RISK_R, T.ROUT_CARE_R, T.AGG_CONCERN
	FROM MYDATA.'CONCERN FOR HEALTH AGGREGATE'n(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing Concern for Health Aggregate";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR GEN_HEALTH EZ_MED_CARE MED_OFF_VIS TOT_EXP_CAT IMMED_CARE_R NO_NEED_INS_R OVER_NO_HELP_R RISK_R ROUT_CARE_R AGG_CONCERN;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Concern for Health Aggregate   */
%LET _CLIENTTASKLABEL='Summary Statistics for Concern for Health Aggregate';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:06 PM
   By task: Summary Statistics for Concern for Health Aggregate

   Input Data: Local:MYDATA.CONCERN FOR HEALTH AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.CONCERN FOR HEALTH AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGG_CONCERN
	FROM MYDATA.'CONCERN FOR HEALTH AGGREGATE'n as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics for Concern for Health Aggregate";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		STDERR 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGG_CONCERN;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Concern for Health Aggregate   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Concern for Health Aggregate';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:07 PM
   By task: Distribution Analysis for Concern for Health Aggregate

   Input Data: Local:MYDATA.CONCERN FOR HEALTH AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.CONCERN FOR HEALTH AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGG_CONCERN
	FROM MYDATA.'CONCERN FOR HEALTH AGGREGATE'n(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Concern for Health Aggregate";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR AGG_CONCERN;
	HISTOGRAM   AGG_CONCERN / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorize Concern for Health   */
%LET _CLIENTTASKLABEL='Categorize Concern for Health';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.CONCERN_FOR_HEALTH_AGG_CAT);

PROC SQL;
   CREATE TABLE MYDATA.CONCERN_FOR_HEALTH_AGG_CAT(label="CONCERN_FOR_HEALTH_AGG_CAT") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.TOT_EXP_CAT, 
          t1.IMMED_CARE_R, 
          t1.NO_NEED_INS_R, 
          t1.OVER_NO_HELP_R, 
          t1.RISK_R, 
          t1.ROUT_CARE_R, 
          t1.AGG_CONCERN, 
          /* CONCERN_HLTH_AGG_CAT */
            (CASE  
               WHEN t1.AGG_CONCERN <=20
               THEN 1
               WHEN t1.AGG_CONCERN <=25
               THEN 2
               WHEN t1.AGG_CONCERN <=30
               THEN 3
               WHEN t1.AGG_CONCERN <=35
               THEN 4
               ELSE 5
            END) LABEL="Concern for health aggregate categorical" AS CONCERN_HLTH_AGG_CAT
      FROM MYDATA.'CONCERN FOR HEALTH AGGREGATE'n t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis for categorizing Concen   */
%LET _CLIENTTASKLABEL='Table Analysis for categorizing Concen';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:07 PM
   By task: Table Analysis for categorizing Concen

   Input Data: Local:MYDATA.CONCERN_FOR_HEALTH_AGG_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.CONCERN_FOR_HEALTH_AGG_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CONCERN_HLTH_AGG_CAT, T.AGG_CONCERN
	FROM MYDATA.CONCERN_FOR_HEALTH_AGG_CAT(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis for categorizing Concern for Health Aggregate";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES AGG_CONCERN * CONCERN_HLTH_AGG_CAT /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Initial Data Set Attributes   */
%LET _CLIENTTASKLABEL='Initial Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:08 PM
   By task: Initial Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Flag   */
%LET _CLIENTTASKLABEL='Flag';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_FULLYR_2012_WRK_FLAG);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_FULLYR_2012_WRK_FLAG(label="MEPS_FULLYR_2012_WRK_FLAG") AS 
   SELECT t1.EDU_CAT, 
          t1.AGG_HLTH_SCORE_categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          t1.HLTH_LIM_MOD_ACC_R, 
          t1.STAIRS_R, 
          t1.ACC_LESS_PHYS_R, 
          t1.WRK_LIM_PHYS_R, 
          t1.ACC_LESS_MNTL_R, 
          t1.WRK_LIM_MNTL_R, 
          t1.DOWN_R, 
          t1.HLTH_STP_SOC_R, 
          t1.AGG_HLTH_SCORE, 
          t1.MARRY_CAT, 
          /* INFULLYR */
            (1) AS INFULLYR
      FROM MYDATA.MEPS_FULLYR_2012_WRK t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Flag   */
LIBNAME EC100017 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Flag';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_ER_2012_FLAG);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_ER_2012_FLAG(label="MEPS_ER_2012_FLAG") AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* INER */
            (1) AS INER
      FROM EC100017.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Outer join   */
%LET _CLIENTTASKLABEL='Outer join';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_FULLYR_ER_JOIN);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_FULLYR_ER_JOIN(label="MEPS_FULLYR_ER_JOIN") AS 
   SELECT t1.EDU_CAT, 
          t1.AGG_HLTH_SCORE_categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F AS PERWT12F1, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.INER, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          t1.HLTH_LIM_MOD_ACC_R, 
          t1.STAIRS_R, 
          t1.ACC_LESS_PHYS_R, 
          t1.WRK_LIM_PHYS_R, 
          t1.ACC_LESS_MNTL_R, 
          t1.WRK_LIM_MNTL_R, 
          t1.DOWN_R, 
          t1.HLTH_STP_SOC_R, 
          t1.AGG_HLTH_SCORE, 
          t1.MARRY_CAT, 
          t1.INFULLYR
      FROM MYDATA.MEPS_FULLYR_2012_WRK_FLAG t1
           FULL JOIN MYDATA.MEPS_ER_2012_FLAG t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Filter   */
%LET _CLIENTTASKLABEL='Filter';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.FILTER_FOR_MEPS_FULLYR_ER_JOIN);

PROC SQL;
   CREATE TABLE MYDATA.FILTER_FOR_MEPS_FULLYR_ER_JOIN(label="FILTER_FOR_MEPS_FULLYR_ER_JOIN") AS 
   SELECT t1.EDU_CAT, 
          t1.AGG_HLTH_SCORE_categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          t1.HLTH_LIM_MOD_ACC_R, 
          t1.STAIRS_R, 
          t1.ACC_LESS_PHYS_R, 
          t1.WRK_LIM_PHYS_R, 
          t1.ACC_LESS_MNTL_R, 
          t1.WRK_LIM_MNTL_R, 
          t1.DOWN_R, 
          t1.HLTH_STP_SOC_R, 
          t1.AGG_HLTH_SCORE, 
          t1.MARRY_CAT, 
          t1.INFULLYR, 
          t1.INER
      FROM MYDATA.MEPS_FULLYR_ER_JOIN t1
      WHERE t1.INER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data of join   */
%LET _CLIENTTASKLABEL='List Data of join';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:09 PM
   By task: List Data of join

   Input Data: Local:MYDATA.FILTER_FOR_MEPS_FULLYR_ER_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.FILTER_FOR_MEPS_FULLYR_ER_JOIN
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.INER, T.DUPERSID1, T.DUPERSID
	FROM MYDATA.FILTER_FOR_MEPS_FULLYR_ER_JOIN(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report of Join";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR INER DUPERSID1 DUPERSID;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:09 PM
   By task: Data Set Attributes

   Input Data: Local:MYDATA.FILTER_FOR_MEPS_FULLYR_ER_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MYDATA.ATTRIBUTES_RFILTER_FOR_MEPS);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.FILTER_FOR_MEPS_FULLYR_ER_JOIN OUT=WORK.SUCOUT1;

RUN;

DATA MYDATA.ATTRIBUTES_RFILTER_FOR_MEPS(LABEL="Contents Details for FILTER_FOR_MEPS_FULLYR_ER_JOIN");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM MYDATA.ATTRIBUTES_RFILTER_FOR_MEPS
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='FILTER_FOR_MEPS_FULLYR_ER_JOIN';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=MYDATA.ATTRIBUTES_RFILTER_FOR_MEPS OUT=MYDATA.ATTRIBUTES_RFILTER_FOR_MEPS;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM MYDATA.ATTRIBUTES_RFILTER_FOR_MEPS
		WHERE memname='FILTER_FOR_MEPS_FULLYR_ER_JOIN';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode missing   */
%LET _CLIENTTASKLABEL='Recode missing';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_JOIN_Recode);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_JOIN_Recode(label="MEPS_JOIN_Recode") AS 
   SELECT t1.EDU_CAT, 
          t1.AGG_HLTH_SCORE_categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          t1.HLTH_LIM_MOD_ACC_R, 
          t1.STAIRS_R, 
          t1.ACC_LESS_PHYS_R, 
          t1.WRK_LIM_PHYS_R, 
          t1.ACC_LESS_MNTL_R, 
          t1.WRK_LIM_MNTL_R, 
          t1.DOWN_R, 
          t1.HLTH_STP_SOC_R, 
          t1.AGG_HLTH_SCORE, 
          t1.MARRY_CAT, 
          t1.INFULLYR, 
          t1.INER, 
          /* MRI_R */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 2
               ELSE t1.MRI
            END) LABEL="This visit did p have an MRI/CATSCAN (recoded missing)" AS MRI_R, 
          /* XRAYS_R */
            (CASE 
               WHEN 1 = t1.XRAYS THEN 1
               WHEN 2 = t1.XRAYS THEN 2
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 2
            END) LABEL="This visit did P have X-RAYS (recoded missing)" AS XRAYS_R
      FROM MYDATA.FILTER_FOR_MEPS_FULLYR_ER_JOIN t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Count ER visits   */
%LET _CLIENTTASKLABEL='Count ER visits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.COUNT);

PROC SQL;
   CREATE TABLE MYDATA.COUNT(label="COUNT") AS 
   SELECT /* COUNT_of_DUPERSID1 */
            (COUNT(t1.DUPERSID1)) AS COUNT_of_DUPERSID1, 
          t1.DUPERSID1 AS DUPERSID11
      FROM MYDATA.MEPS_JOIN_RECODE t1
      GROUP BY t1.DUPERSID1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Join   */
%LET _CLIENTTASKLABEL='Join';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.JOIN_WITH_COUNT);

PROC SQL;
   CREATE TABLE MYDATA.JOIN_WITH_COUNT(label="JOIN_WITH_COUNT") AS 
   SELECT t1.COUNT_of_DUPERSID1, 
          t1.DUPERSID11, 
          t2.EDU_CAT, 
          t2.AGG_HLTH_SCORE_categorical, 
          t2.DUPERSID, 
          t2.AGE12X, 
          t2.SEX, 
          t2.REGION12, 
          t2.RACETHX, 
          t2.MARRY12X, 
          t2.EDRECODE, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F1, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.EMPST31, 
          t2.EMPST42, 
          t2.EMPST53, 
          t2.EDUCYR, 
          t2.ADAPPT42, 
          t2.ADCAPE42, 
          t2.ADCLIM42, 
          t2.ADCMPD42, 
          t2.ADCMPM42, 
          t2.ADCMPY42, 
          t2.ADDAYA42, 
          t2.ADDOWN42, 
          t2.ADDPRS42, 
          t2.ADDRBP42, 
          t2.ADEFRT42, 
          t2.ADEGMC42, 
          t2.ADEXPL42, 
          t2.ADEZUN42, 
          t2.ADFFRM42, 
          t2.ADFHLP42, 
          t2.ADGENH42, 
          t2.ADHECR42, 
          t2.ADHOPE42, 
          t2.ADILCR42, 
          t2.ADILWW42, 
          t2.ADINSA42, 
          t2.ADINSB42, 
          t2.ADINST42, 
          t2.ADINTR42, 
          t2.ADLANG42, 
          t2.ADLIST42, 
          t2.ADMALS42, 
          t2.ADMWLM42, 
          t2.ADNDCR42, 
          t2.ADNERV42, 
          t2.ADNRGY42, 
          t2.ADNSMK42, 
          t2.ADOVER42, 
          t2.ADPAIN42, 
          t2.ADPALS42, 
          t2.ADPRTM42, 
          t2.ADPRX42, 
          t2.ADPWLM42, 
          t2.ADRESP42, 
          t2.ADREST42, 
          t2.ADRISK42, 
          t2.ADRTCR42, 
          t2.ADRTWW42, 
          t2.ADSAD42, 
          t2.ADSMOK42, 
          t2.ADSOCA42, 
          t2.ADSPEC42, 
          t2.ADSPRF42, 
          t2.ADTLHW42, 
          t2.ADWRTH42, 
          t2.K6SUM42, 
          t2.MCS42, 
          t2.PCS42, 
          t2.PHQ242, 
          t2.SFFLAG42, 
          t2.AMASST12, 
          t2.AMCHIR12, 
          t2.AMDRC12, 
          t2.AMNURS12, 
          t2.AMTHER12, 
          t2.HHTOTD12, 
          t2.IPDIS12, 
          t2.IPNGTD12, 
          t2.IPZERO12, 
          t2.CHLDP12X, 
          t2.DEPDNT12, 
          t2.DIVDP12X, 
          t2.FAMINC12, 
          t2.FAMS1231, 
          t2.ERTOT12, 
          t2.INSURC12, 
          t2.PRIEU12, 
          t2.PRING12, 
          t2.PRIOG12, 
          t2.PRIS12, 
          t2.PRIV12, 
          t2.PUB12X, 
          t2.TOTEXP12, 
          t2.TOTMCD12, 
          t2.TOTMCR12, 
          t2.TOTOTH12, 
          t2.TOTPRV12, 
          t2.TOTSLF12, 
          t2.PERWT12F, 
          t2.EDU, 
          t2.EMPLY31, 
          t2.EMPLY42, 
          t2.EMPLY53, 
          t2.MED_OFF_VIS, 
          t2.CALM_PE_4, 
          t2.STAIRS, 
          t2.HLTH_LIM_MOD_AC, 
          t2.DOWN, 
          t2.EZ_MED_CARE, 
          t2.GEN_HEALTH, 
          t2.HEALTH_RATE, 
          t2.IMMED_CARE, 
          t2.NO_NEED_INS, 
          t2.ACC_LESS_MNTL, 
          t2.WRK_LIM_MNTL, 
          t2.LOT_NRGY, 
          t2.OVER_NO_HELP, 
          t2.PAIN_LIM_WRK, 
          t2.ACC_LESS_PHYS, 
          t2.WRK_LIM_PHYS, 
          t2.RISK, 
          t2.ROUT_CARE, 
          t2.SMOKE, 
          t2.SPEC, 
          t2.HLTH_STP_SOC, 
          t2.MARRY, 
          t2.HLTH_LIM_MOD_ACC_R, 
          t2.STAIRS_R, 
          t2.ACC_LESS_PHYS_R, 
          t2.WRK_LIM_PHYS_R, 
          t2.ACC_LESS_MNTL_R, 
          t2.WRK_LIM_MNTL_R, 
          t2.DOWN_R, 
          t2.HLTH_STP_SOC_R, 
          t2.AGG_HLTH_SCORE, 
          t2.MARRY_CAT, 
          t2.INFULLYR, 
          t2.INER, 
          t2.MRI_R, 
          t2.XRAYS_R
      FROM MYDATA.COUNT t1
           INNER JOIN MYDATA.MEPS_JOIN_RECODE t2 ON (t1.DUPERSID11 = t2.DUPERSID1);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequency for number of ER visits   */
%LET _CLIENTTASKLABEL='One-Way Frequency for number of ER visits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:09 PM
   By task: One-Way Frequency for number of ER visits

   Input Data: Local:MYDATA.JOIN_WITH_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.JOIN_WITH_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID1
	FROM MYDATA.JOIN_WITH_COUNT(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies Number of ER Visits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:09 PM
   By task: Distribution Analysis

   Input Data: Local:MYDATA.JOIN_WITH_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.JOIN_WITH_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID1
	FROM MYDATA.JOIN_WITH_COUNT(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID1";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID1;
	HISTOGRAM   COUNT_of_DUPERSID1 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorize Number of ER Visits   */
%LET _CLIENTTASKLABEL='Categorize Number of ER Visits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.NUM_ER_CAT);

PROC SQL;
   CREATE TABLE MYDATA.NUM_ER_CAT(label="NUM_ER_CAT") AS 
   SELECT t1.COUNT_of_DUPERSID1, 
          t1.DUPERSID11, 
          t1.EDU_CAT, 
          t1.AGG_HLTH_SCORE_categorical, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.EDUCYR, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SFFLAG42, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMNURS12, 
          t1.AMTHER12, 
          t1.HHTOTD12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.IPZERO12, 
          t1.CHLDP12X, 
          t1.DEPDNT12, 
          t1.DIVDP12X, 
          t1.FAMINC12, 
          t1.FAMS1231, 
          t1.ERTOT12, 
          t1.INSURC12, 
          t1.PRIEU12, 
          t1.PRING12, 
          t1.PRIOG12, 
          t1.PRIS12, 
          t1.PRIV12, 
          t1.PUB12X, 
          t1.TOTEXP12, 
          t1.TOTMCD12, 
          t1.TOTMCR12, 
          t1.TOTOTH12, 
          t1.TOTPRV12, 
          t1.TOTSLF12, 
          t1.PERWT12F, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.MED_OFF_VIS, 
          t1.CALM_PE_4, 
          t1.STAIRS, 
          t1.HLTH_LIM_MOD_AC, 
          t1.DOWN, 
          t1.EZ_MED_CARE, 
          t1.GEN_HEALTH, 
          t1.HEALTH_RATE, 
          t1.IMMED_CARE, 
          t1.NO_NEED_INS, 
          t1.ACC_LESS_MNTL, 
          t1.WRK_LIM_MNTL, 
          t1.LOT_NRGY, 
          t1.OVER_NO_HELP, 
          t1.PAIN_LIM_WRK, 
          t1.ACC_LESS_PHYS, 
          t1.WRK_LIM_PHYS, 
          t1.RISK, 
          t1.ROUT_CARE, 
          t1.SMOKE, 
          t1.SPEC, 
          t1.HLTH_STP_SOC, 
          t1.MARRY, 
          t1.HLTH_LIM_MOD_ACC_R, 
          t1.STAIRS_R, 
          t1.ACC_LESS_PHYS_R, 
          t1.WRK_LIM_PHYS_R, 
          t1.ACC_LESS_MNTL_R, 
          t1.WRK_LIM_MNTL_R, 
          t1.DOWN_R, 
          t1.HLTH_STP_SOC_R, 
          t1.AGG_HLTH_SCORE, 
          t1.MARRY_CAT, 
          t1.INFULLYR, 
          t1.INER, 
          t1.MRI_R, 
          t1.XRAYS_R, 
          /* NUM_ER_CAT */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID1 =1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID1 =2
               THEN 2
               WHEN t1.COUNT_of_DUPERSID1 <=5
               THEN 3
               ELSE 4
            END) LABEL="Number of ER visits categorical" AS NUM_ER_CAT
      FROM MYDATA.JOIN_WITH_COUNT t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis of Number of ER Visits   */
%LET _CLIENTTASKLABEL='Table Analysis of Number of ER Visits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:10 PM
   By task: Table Analysis of Number of ER Visits

   Input Data: Local:MYDATA.NUM_ER_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.NUM_ER_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.NUM_ER_CAT, T.COUNT_of_DUPERSID1
	FROM MYDATA.NUM_ER_CAT(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis of Number of ER Visits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 * NUM_ER_CAT /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for Categorical Number of ER Visits   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for Categorical Number of ER Visits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:10 PM
   By task: One-Way Frequencies for Categorical Number of ER Visits

   Input Data: Local:MYDATA.NUM_ER_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.NUM_ER_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.NUM_ER_CAT
	FROM MYDATA.NUM_ER_CAT(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies of categorical Number of ER Visits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES NUM_ER_CAT /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for MRI and XRAY   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for MRI and XRAY';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 14, 2015 at 2:04:10 PM
   By task: One-Way Frequencies for MRI and XRAY

   Input Data: Local:MYDATA.MEPS_JOIN_RECODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MEPS_JOIN_RECODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYS_R, T.MRI_R
	FROM MYDATA.MEPS_JOIN_RECODE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies MRI and X-RAYS";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Victoria Hammitt";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAYS_R /  SCORES=TABLE;
	TABLES MRI_R /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Program from written code   */
%LET _CLIENTTASKLABEL='Program from written code';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\vhammitt\Assignments\HammittV_SAS_project_01.14.15.egp';
%LET _CLIENTPROJECTNAME='HammittV_SAS_project_01.14.15.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\vhammitt\Assignments\Program from written code.sas';

GOPTIONS ACCESSIBLE;
*Data management for SF-12 variables;
*program written by Victoria hammitt 01/14/15;
LIBNAME MYDATA1  "P:\QAC\qac200\students\vhammitt" ;
LIBNAME INDATA  "P:\QAC\QAC200\DATA\MEPS" ;

*Create a data subset with important variables and observations ages 18+;
data mydata1.subset_MEPSadult_new;
set indata.meps_fullyr_2012;

keep DUPERSID 
          AGE12X 
          SEX 
          REGION12 
          RACETHX 
          MARRY12X 
          EDRECODE 
          EMPST31 
          EMPST42 
          EMPST53 
          EDUCYR 
          ADAPPT42 
          ADCAPE42 
          ADCLIM42 
          ADCMPD42 
          ADCMPM42 
          ADCMPY42 
          ADDAYA42 
          ADDOWN42 
          ADDPRS42 
          ADDRBP42 
          ADEFRT42 
          ADEGMC42 
          ADEXPL42 
          ADEZUN42 
          ADFFRM42 
          ADFHLP42 
          ADGENH42 
          ADHECR42 
          ADHOPE42 
          ADILCR42 
          ADILWW42 
          ADINSA42 
          ADINSB42 
          ADINST42 
          ADINTR42 
          ADLANG42 
          ADLIST42 
          ADMALS42 
          ADMWLM42 
          ADNDCR42 
          ADNERV42 
          ADNRGY42 
          ADNSMK42 
          ADOVER42 
          ADPAIN42 
          ADPALS42 
          ADPRTM42 
          ADPRX42 
          ADPWLM42 
          ADRESP42 
          ADREST42 
          ADRISK42 
          ADRTCR42 
          ADRTWW42 
          ADSAD42 
          ADSMOK42 
          ADSOCA42 
          ADSPEC42 
          ADSPRF42 
          ADTLHW42 
          ADWRTH42 
          K6SUM42 
          MCS42 
          PCS42 
          PHQ242 
          SFFLAG42 
          AMASST12 
          AMCHIR12 
          AMDRC12 
          AMNURS12 
          AMTHER12 
          HHTOTD12 
          IPDIS12 
          IPNGTD12 
          IPZERO12 
          CHLDP12X 
          DEPDNT12 
          DIVDP12X 
          FAMINC12 
          FAMS1231 
          ERTOT12 
          INSURC12 
          PRIEU12 
          PRING12 
          PRIOG12 
          PRIS12 
          PRIV12 
          PUB12X 
          TOTEXP12 
          TOTMCD12 
          TOTMCR12 
          TOTOTH12 
          TOTPRV12 
          TOTSLF12 
          PERWT12F;
      
      WHERE AGE12X >= 18;
	  run;

*sort by DUPERSID;
	  proc sort;
	  by DUPERSID;
	  run;
*create temporary data set with managed variables;
data new;
set mydata1.subset_mepsadult_new;

*use an array to recode missing values for important and necessary variables;
array old (*) MARRY12X
			EDRECODE
			EMPST31
			EMPST42
			EMPST53
			ADAPPT42
			ADCAPE42
			ADCLIM42
			ADDAYA42
			ADDOWN42
			ADEGMC42
			ADGENH42
			ADHECR42
			ADILCR42
			ADINSA42
			ADMALS42
			ADMWLM42
			ADNRGY42
			ADOVER42
			ADPAIN42
			ADPALS42
			ADPWLM42
			ADRISK42
			ADRTCR42
			ADSMOK42
			ADSOCA42
			ADSPEC42;
array new(*) MARRY
			EDU
			EMPLY31
			EMPLY42
			EMPLY53
			MED_OFF_VIS
			CALM_PE_4
			STAIRS
			HLTH_LIM_MOD_AC
			DOWN
			EZ_MED_CARE
			GEN_HEALTH
			HEALTH_RATE
			IMMED_CARE
			NO_NEED_INS
			ACC_LESS_MNTL
			WRK_LIM_MNTL
			LOT_NRGY
			OVER_NO_HELP
			PAIN_LIM_WORK
			ACC_LESS_PHYS
			WRK_LIM_PHYS
			RISK
			ROUT_CARE
			SMOKE
			HLTH_STP_SOC
			SPEC;
do i=1 to dim (old);
if old(i)<0 then new(i)=.;
else new(i)=old(i);
end;

*label recoded missing;
LABEL MARRY="Marital status (recoded missing)";
LABEL EDU="Education recode (recoded missing)";
LABEL EMPLY31="Employment status on 3/1 (recoded missing)" ;
LABEL EMPLY42="Employment status on 4/2 (recoded missing)" ;
LABEL EMPLY53="Employment status on 5/3 (recoded missing)" ;
LABEL MED_OFF_VIS="SAQ: Number of visits to a medical office for care (recoded missing)" ;
LABEL CALM_PE_4="SAQ 4WKS: felt calm/peaceful (recoded missing)" ;
LABEL STAIRS="SAQ: health limits climbing stairs (recoded missing)" ;
LABEL HLTH_LIM_MOD_AC="SAQ: health limits for moderate activites (recoded missing)" ;
LABEL DOWN="SAQ 4WKS: felt downhearted or depressed (recoded missing)" ;
LABEL EZ_MED_CARE="SAQ: easy getting needed medical care (recoded missing)" ;
LABEL GEN_HEALTH="SAQ: health in general (recoded missing)" ;
LABEL HEALTH_RATE="SAQ: rating of health care (recoded missing)" ;
LABEL IMMED_CARE="SAQ: illness or injury needing immediate care (recoded missing)" ;
LABEL NO_NEED_INS="SAQ: do not need health insurance (recoded missing)" ;
LABEL ACC_LESS_MNTL="SAQ 4WKS: accomplish less because of mental problems (recoded missing)" ;
LABEL WRK_LIM_MNTL="SAQ 4WKS: work limited because of mental problems (recoded missing)" ;
LABEL LOT_NRGY="SAQ 4WKS: had a lot of energy (recoded missing)" ;
LABEL OVER_NO_HELP="SAQ: can overcome ills without medical help (recoded missing)" ;
LABEL PAIN_LIM_WORK="SAQ 4WKS: pain limits normal work (recoded missing)" ;
LABEL ACC_LESS_PHYS="SAQ 4WKS: accomplish less because of physical problems (recoded missing)" ;
LABEL WRK_LIM_PHYS="SAQ 4WKS: work limited because of physical problems (recoded missing)" ;
LABEL RISK="SAQ: more likely to take risks (recoded missing" ;
LABEL ROUT_CARE="SAQ: made appointment for routine medical care" ;
LABEL SMOKE="SAQ: currently smoke (recoded missing)" ;
LABEL HLTH_STP_SOC="SAQ 4WKS: health stopped social activity (recoded missing)" ;
LABEL SPEC="SAQ: needed to see specialist (recoded missing)" ;

*use array to reverse code necessary SF-12 variables;
array old1(*) CALM_PE_4
			GEN_HEALTH
			LOT_NRGY
			PAIN_LIM_WORK;
array new1(*) CALM_PE_4_R
			GEN_HEALTH_R
			LOT_NRGY_R
			PAIN_LIM_WORK_R;
do i=1 to dim (old1);
if old1(i)=1 then new1(i)=5;
if old1(i)=2 then new1(i)=4;
if old1(i)=4 then new1(i)=2;
if old1(i)=5 then new1(i)=1;
else new1(i)=old1(i);
end;

*label reverse coding;
LABEL CALM_PE_4_R="SAQ 4WKS: felt calm/peaceful (recoded missing)(reverse coded)" ;
LABEL GEN_HEALTH_R="SAQ: health in general (recoded missing)(reverse coded)" ;
LABEL LOT_NRGY_R="SAQ 4WKS: had a lot of energy (recoded missing)(reverse coded)" ;
LABEL PAIN_LIM_WORK="SAQ 4WKS: pain limits normal work (recoded missing)" ;

*sum SF-12 variables;
AGG_HLTH_SCORE=SUM(CALM_PE_4_R,GEN_HEALTH_R,LOT_NRGY_R,PAIN_LIM_WRK_R,HLTH_LIM_MOD_ACC,STAIRS,ACC_LESS_PHYS,WRK_LIM_PHYS,ACC_LESS_MNTL,WRK_LIM_MNTL,DOWN,HLTH_STP_SOC); 

*label aggregate;
LABEL AGG_HLTH_SCORE="Aggregate health score by sum of SF12 after reverse coding and recode missing";

*create aggregate health score categorical variable;            
if AGG_HLTH_SCORE <=16 then AGG_HLTH_SCORE_categorical=1;
else if AGG_HLTH_SCORE <=19 then AGG_HLTH_SCORE_categorical=2;
else if AGG_HLTH_SCORE <=27 then AGG_HLTH_SCORE_categorical=3;
else if AGG_HLTH_SCORE >27 then AGG_HLTH_SCORE_categorical=4;

*label categorical aggregate;
LABEL AGG_HLTH_SCORE_categorical="Quantile-based categories of aggregate health score";
run;

*summary statistics for aggregate health score;
PROC MEANS DATA=new
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR AGG_HLTH_SCORE;

RUN;

*generate contingency tables for recoded SF-12;
PROC FREQ DATA = new;

	TABLES ADCAPE42 * CALM_PE_4 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCLIM42 * STAIRS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDAYA42 * HLTH_LIM_MOD_AC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDOWN42 * DOWN /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADGENH42 * GEN_HEALTH /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMALS42 * ACC_LESS_MNTL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMWLM42 * WRK_LIM_MNTL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42 * LOT_NRGY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPAIN42 * PAIN_LIM_WORK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPALS42 * ACC_LESS_PHYS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPWLM42 * WRK_LIM_PHYS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSOCA42 * HLTH_STP_SOC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
RUN;

*generate frequency tables for SF-12 variables;
PROC FREQ DATA = new;

	TABLES CALM_PE_4 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES STAIRS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HLTH_LIM_MOD_AC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES DOWN /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES GEN_HEALTH /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ACC_LESS_MNTL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES WRK_LIM_MNTL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES LOT_NRGY /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PAIN_LIM_WORK /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ACC_LESS_PHYS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES WRK_LIM_PHYS /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HLTH_STP_SOC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
RUN;

*generate list of 12 recoded SF-12 variables and SF-12 aggregate;
PROC PRINT DATA=new
	(OBS=100);
	VAR HLTH_LIM_MOD_ACC STAIRS ACC_LESS_PHYS WRK_LIM_PHYS ACC_LESS_MNTL WRK_LIM_MNTL DOWN HLTH_STP_SOC CALM_PE_4_R GEN_HEALTH_R LOT_NRGY_R PAIN_LIM_WRK_R AGG_HLTH_SCORE;
RUN;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
