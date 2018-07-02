" Vim syntax file
" Language: SAS
" Maintainer: James Kidd <james.kidd@covance.com>
" Last Change: XX Dec 2006
" Programmer: J. Manuel Picaza <jmpicaza@gmail.com>
" Version: 1.0
"
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" *****************************************************
" GENERAL OPTIONS SOME USERS COULD WANT AND OTHERS NOT.
" *****************************************************
" Adjust the font. Comment the line if you want to use your own font!
"set gfn=courier_new:h10:cANSI

" Modify the used words for letting '&' be part of a word.
"set iskeyword+=038

syn case ignore
syn cluster sasCommon contains=sasComment,sasString,sasNumber,sasFormat,sasUnderscore,sasMacroVar,sasMacroName,sasSection

syn keyword sasSection RUN QUIT

" ********
" Comments
" ********
syn region sasComment start=/\/\*/  end=/\*\// fold keepend extend contains=sasTodo
syn region sasComment start="^\s*\*" end=";" keepend extend contains=sasTodo
syn region sasComment start=";\s*\*"hs=s+1 end=";" keepend extend contains=sasTodo
syn match sasComment "\(^\s*%*\*.\{-}\)\=\(;\+\s*%\=\*.\{-}\)\+;\+" extend contains=sasTodo

syn keyword sasTodo contained TODO OJO FIXME JMP JOSE NOTE

" *******
" Strings
" *******
syn region sasString start=+"+  skip=+\\\\\|\\"+  end=+"+ keepend extend contains=sasMacroVar
syn region sasString start=+'+  skip=+\\\\\|\\"+  end=+'+ keepend extend

" *******
" Numbers
" *******
syn match sasNumber /-\=\<\d*\.\=\d\+\>\|-\=\<\d\+\>\|-\=\<\d\+\.\=\d*[Ee]\d\+\>/ 
syn match sasNumber /-\=\<\d*\.\=\d\+\>\|-\=\<\d\+\>\|-\=\<\d\+\.\=\d*[Ee]\d\+\>/

" *******
" Formats
" *******
syn match sasFormat /\<\w\+\d*\.\(,\|)\|;\|\s\|$\)/he=e-1
syn match sasFormat /$\d\+\.\(,\|)\|;\|\s\|$\)/he=e-1

" *********
" Data Step
" *********
syn keyword sasData contained DATA

syn match sasDataOption contained "\<ALTER\s*="
syn match sasDataOption contained "\<BUFNO\s*="
syn match sasDataOption contained "\<BUFSIZE\s*="
syn match sasDataOption contained "\<CNTLLEV\s*="
syn match sasDataOption contained "\<COMPRESS\s*="
syn match sasDataOption contained "\<DLDMGACTION\s*="
syn match sasDataOption contained "\<DROP\s*="
syn match sasDataOption contained "\<ENCODING\s*="
syn match sasDataOption contained "\<ENCRYPT\s*="
syn match sasDataOption contained "\<FILECLOSE\s*="
syn match sasDataOption contained "\<FIRSTOBS\s*="
syn match sasDataOption contained "\<GENMAX\s*="
syn match sasDataOption contained "\<GENNUM\s*="
syn match sasDataOption contained "\<IDXNAME\s*="
syn match sasDataOption contained "\<IDXWHERE\s*="
syn match sasDataOption contained "\<IN\s*="
syn match sasDataOption contained "\<INDEX\s*="
syn match sasDataOption contained "\<KEEP\s*="
syn match sasDataOption contained "\<LABEL\s*="
syn match sasDataOption contained "\<OBS\s*="
syn match sasDataOption contained "\<OBSBUF\s*="
syn match sasDataOption contained "\<OUTREP\s*="
syn match sasDataOption contained "\<POINTOBS\s*="
syn match sasDataOption contained "\<PW\s*="
syn match sasDataOption contained "\<PWREQ\s*="
syn match sasDataOption contained "\<READ\s*="
syn match sasDataOption contained "\<RENAME\s*="
syn match sasDataOption contained "\<REPEMPTY\s*="
syn match sasDataOption contained "\<REPLACE\s*="
syn match sasDataOption contained "\<REUSE\s*="
syn match sasDataOption contained "\<SORTEDBY\s*="
syn match sasDataOption contained "\<SORTSEQ\s*="
syn match sasDataOption contained "\<SPILL\s*="
syn match sasDataOption contained "\<TOBSNO\s*="
syn match sasDataOption contained "\<TYPE\s*="
syn match sasDataOption contained "\<WHERE\s*="
syn match sasDataOption contained "\<WHEREUP\s*="
syn match sasDataOption contained "\<WRITE\s*="

syn region sasDataOpt    start=/(/ end=/)/ contained  transparent extend contains=sasDataOption,@sasCommon
syn region sasDataHeader start=/\(^\|;\)\s*data\>/ end=/;/ keepend transparent extend contains=sasDataOpt,sasData,@sasCommon
syn region sasDataSet start=/\(^\|;\)\s*set\>/ end=/;/ keepend transparent extend contains=sasDataOpt,@sasCommon

syn region sasCards start="^\s*CARDS.*" end="^\s*;\s*$"
syn region sasCards start="^\s*DATALINES.*" end="^\s*;\s*$"

"syn match sasOption /\c\s\+DATA\s*=/he=e-1
"syn keyword sasOption ACCESS CNTLOUT CNTLIN LIBRARY CATALOG COPY OUT
"syn keyword sasOption ENTRYTYPE KILL

syn match sasUnderscore "_NULL_"
syn match sasUnderscore "_INFILE_"
syn match sasUnderscore "_N_"
syn match sasUnderscore "_WEBOUT_"
syn match sasUnderscore "_NUMERIC_"
syn match sasUnderscore "_CHARACTER_"
syn match sasUnderscore "_ALL_"

" *********
" Functions
" *********
syn keyword sasFunction ABS ADDR ADDRLONG AIRY ANYALNUM ANYALPHA ANYCNTRL ANYDIGIT ANYFIRST ANYGRAPH
syn keyword sasFunction ANYLOWER ANYNAME ANYPRINT ANYPUNCT ANYSPACE ANYUPPER ANYXDIGIT ARCOS ARSIN ATAN
syn keyword sasFunction ATAN2 ATTRC ATTRN BAND BETA BETAINV BLSHIFT BNOT BOR BRSHIFT BXOR BYTE
syn keyword sasFunction CAT CATS CATT CATX CDF CEIL CEILZ CEXIST CHOOSEC CHOOSEN
syn keyword sasFunction CINV CLOSE CNONCT COALESCE COALESCEC COLLATE COMB COMPARE COMPBL COMPGED
syn keyword sasFunction COMPLEV COMPOUND COMPRESS CONSTANT CONVX CONVXP COS COSH COUNT COUNTC
syn keyword sasFunction CSS CUROBS CV DACCDB DACCDBSL DACCSL DACCSYD DACCTAB DAIRY DATDIF
syn keyword sasFunction DATE DATEJUL DATEPART DATETIME DAY DCLOSE DCREATE DEPDB DEPDBSL DEPSL
syn keyword sasFunction DEPSYD DEPTAB DEQUOTE DEVIANCE DHMS DIF DIGAMMA DIM DINFO DNUM
syn keyword sasFunction DOPEN DOPTNAME DOPTNUM DREAD DROPNOTE DSNAME DUR DURP ERF ERFC
syn keyword sasFunction EUROCURR EXIST EXP FACT FAPPEND FCLOSE FCOL FDELETE FETCH FETCHOBS
syn keyword sasFunction FEXIST FGET FILEEXIST FILENAME FILEREF FIND FINDC FINFO FINV FIPNAME
syn keyword sasFunction FIPNAMEL FIPSTATE FLOOR FLOORZ FNONCT FNOTE FOPEN FOPTNAME FOPTNUM FPOINT
syn keyword sasFunction FPOS FPUT FREAD FREWIND FRLEN FSEP FUZZ FWRITE GAMINV GAMMA
syn keyword sasFunction GEOMEAN GEOMEANZ GETOPTION GETVARC GETVARN HARMEAN HARMEANZ HBOUND HMS HOUR
syn keyword sasFunction HTMLDECODE HTMLENCODE IBESSEL IFC IFN INDEX INDEXC INDEXW INPUT INPUTC
syn keyword sasFunction INPUTN INT INTCK INTNX INTRR INTZ IORCMSG IQR IRR JBESSEL
syn keyword sasFunction JULDATE JULDATE7 KCOMPARE KCOMPRESS KCOUNT KCVT KINDEX KINDEXC KLEFT KLENGTH
syn keyword sasFunction KLOWCASE KREVERSE KRIGHT KSCAN KSTRCAT KSUBSTR KSUBSTRB KTRANSLATE KTRIM KTRUNCATE
syn keyword sasFunction KUPCASE KUPDATE KUPDATEB KURTOSIS KVERIFY LAG LARGEST LBOUND LEFT LENGTH
syn keyword sasFunction LENGTHC LENGTHM LENGTHN LGAMMA LIBNAME LIBREF LOG LOG10 LOG2 LOGBETA
syn keyword sasFunction LOGCDF LOGPDF LOGSDF LOWCASE MAD MAX MDY MEAN MEDIAN MIN
syn keyword sasFunction MINUTE MISSING MOD MODULEC MODULEIC MODULEIN MODULEN MODZ MONTH MOPEN
syn keyword sasFunction MORT N NETPV NLDATE NLDATM NLTIME NLITERAL NMISS NORMAL NOTALNUM
syn keyword sasFunction NOTALPHA NOTCNTRL NOTDIGIT NOTE NOTFIRST NOTGRAPH NOTLOWER NOTNAME NOTPRINT NOTPUNCT
syn keyword sasFunction NOTSPACE NOTUPPER NOTXDIGIT NPV NVALID OPEN ORDINAL PATHNAME PCTL PDF
syn keyword sasFunction PEEK PEEKC PEEKCLONG PEEKLONG PERM POINT POISSON PROBBETA PROBBNML PROBBNRM
syn keyword sasFunction PROBCHI PROBF PROBGAM PROBHYPR PROBIT PROBMC PROBNEGB PROBNORM PROBT PROPCASE
syn keyword sasFunction PRXCHANGE PRXMATCH PRXPAREN PRXPARSE PRXPOSN PTRLONGADD PUT PUTC PUTN PVP
syn keyword sasFunction QTR QUANTILE QUOTE RANBIN RANCAU RAND RANEXP RANGAM RANGE RANK
syn keyword sasFunction RANNOR RANPOI RANTBL RANTRI RANUNI REPEAT RESOLVE REVERSE REWIND RIGHT
syn keyword sasFunction RMS ROUND ROUNDE ROUNDZ RXMATCH RXPARSE SAVING SCAN SCANQ SDF
syn keyword sasFunction SECOND SIGN SIN SINH SKEWNESS SLEEP SMALLEST SOUNDEX SPEDIS SQRT
syn keyword sasFunction STD STDERR STFIPS STNAME STNAMEL STRIP SUBPAD SUBSTR SUBSTRN SUM
syn keyword sasFunction SYMEXIST SYMGET SYMGLOBL SYMLOCAL SYSGET SYSMSG SYSPARM SYSPROCESSID SYSPROCESSNAME SYSPROD
syn keyword sasFunction SYSRC SYSTEM TAN TANH TIME TIMEPART TINV TNONCT TODAY TRANSLATE
syn keyword sasFunction TRANTAB TRANWRD TRIGAMMA TRIM TRIMN TRUNC UNIFORM UPCASE URLDECODE URLENCODE
syn keyword sasFunction USS UUIDGEN VAR VARFMT VARINFMT VARLABEL VARLEN VARNAME VARNUM VARRAY
syn keyword sasFunction VARRAYX VARTRANSCODE VARTYPE VERIFY VFORMAT VFORMATD VFORMATDX VFORMATN VFORMATNX VFORMATW
syn keyword sasFunction VFORMATWX VFORMATX VINARRAY VINARRAYX VINFORMAT VINFORMATD VINFORMATDX VINFORMATN VINFORMATNX VINFORMATW
syn keyword sasFunction VINFORMATWX VINFORMATX VLABEL VLABELX VLENGTH VLENGTHX VNAME VNAMEX VTRANSCODE VTRANSCODEX
syn keyword sasFunction VTYPE VTYPEX VVALUE VVALUEX WEEK WEEKDAY YEAR YIELDP YRDIF YYQ
syn keyword sasFunction ZIPCITY ZIPFIPS ZIPNAME ZIPNAMEL ZIPSTATE

" ********
" Routines
" ********
syn match sasRoutine "\<CALL\s\+ALLPERM"
syn match sasRoutine "\<CALL\s\+CATS"
syn match sasRoutine "\<CALL\s\+CATT"
syn match sasRoutine "\<CALL\s\+CATX"
syn match sasRoutine "\<CALL\s\+COMPCOST"
syn match sasRoutine "\<CALL\s\+EXECUTE"
syn match sasRoutine "\<CALL\s\+LABEL"
syn match sasRoutine "\<CALL\s\+LOGISTIC"
syn match sasRoutine "\<CALL\s\+MISSING"
syn match sasRoutine "\<CALL\s\+MODULE"
syn match sasRoutine "\<CALL\s\+MODULEI"
syn match sasRoutine "\<CALL\s\+POKE"
syn match sasRoutine "\<CALL\s\+POKELONG"
syn match sasRoutine "\<CALL\s\+PRXCHANGE"
syn match sasRoutine "\<CALL\s\+PRXDEBUG"
syn match sasRoutine "\<CALL\s\+PRXFREE"
syn match sasRoutine "\<CALL\s\+PRXNEXT"
syn match sasRoutine "\<CALL\s\+PRXPOSN"
syn match sasRoutine "\<CALL\s\+PRXSUBSTR"
syn match sasRoutine "\<CALL\s\+RANBIN"
syn match sasRoutine "\<CALL\s\+RANCAU"
syn match sasRoutine "\<CALL\s\+RANEXP"
syn match sasRoutine "\<CALL\s\+RANGAM"
syn match sasRoutine "\<CALL\s\+RANNOR"
syn match sasRoutine "\<CALL\s\+RANPERK"
syn match sasRoutine "\<CALL\s\+RANPERM"
syn match sasRoutine "\<CALL\s\+RANPOI"
syn match sasRoutine "\<CALL\s\+RANTBL"
syn match sasRoutine "\<CALL\s\+RANTRI"
syn match sasRoutine "\<CALL\s\+RANUNI"
syn match sasRoutine "\<CALL\s\+RXCHANGE"
syn match sasRoutine "\<CALL\s\+RXFREE"
syn match sasRoutine "\<CALL\s\+RXSUBSTR"
syn match sasRoutine "\<CALL\s\+SCAN"
syn match sasRoutine "\<CALL\s\+SCANQ"
syn match sasRoutine "\<CALL\s\+SET"
syn match sasRoutine "\<CALL\s\+SLEEP"
syn match sasRoutine "\<CALL\s\+SOFTMAX"
syn match sasRoutine "\<CALL\s\+STDIZE"
syn match sasRoutine "\<CALL\s\+STREAMINIT"
syn match sasRoutine "\<CALL\s\+SYMPUT"
syn match sasRoutine "\<CALL\s\+SYMPUTX"
syn match sasRoutine "\<CALL\s\+SYSTEM"
syn match sasRoutine "\<CALL\s\+TANH"
syn match sasRoutine "\<CALL\s\+VNAME"
syn match sasRoutine "\<CALL\s\+VNEXT"

" **********
" Statements
" **********
syn match sasStatement "%INC\>"
syn match sasStatement "%INCLUDE\>"
syn match sasStatement "%LIST\>"
syn match sasStatement "%PUT\>"
syn match sasStatement "%RUN\>"
syn match sasStatement "\<DO\s\+UNTIL\>"
syn match sasStatement "\<DO\s\+WHILE\>"
syn match sasStatement "\<GO\s\+TO\>"
syn match sasStatement "\<ODS\s\+CHTML\>"
syn match sasStatement "\<ODS\s\+CSVALL\>"
syn match sasStatement "\<ODS\s\+DECIMAL_ALIGN\>"
syn match sasStatement "\<ODS\s\+DOCBOOK\>"
syn match sasStatement "\<ODS\s\+DOCUMENT\>"
syn match sasStatement "\<ODS\s\+EXCLUDE\>"
syn match sasStatement "\<ODS\s\+HTML3\>"
syn match sasStatement "\<ODS\s\+HTMLCSS\>"
syn match sasStatement "\<ODS\s\+HTML\>"
syn match sasStatement "\<ODS\s\+IMODE\>"
syn match sasStatement "\<ODS\s\+LISTING\>"
syn match sasStatement "\<ODS\s\+MARKUP\>"
syn match sasStatement "\<ODS\s\+OUTPUT\>"
syn match sasStatement "\<ODS\s\+PATH\>"
syn match sasStatement "\<ODS\s\+PCL\>"
syn match sasStatement "\<ODS\s\+PDF\>"
syn match sasStatement "\<ODS\s\+PHTML\>"
syn match sasStatement "\<ODS\s\+PRINTER\>"
syn match sasStatement "\<ODS\s\+PROCLABEL\>"
syn match sasStatement "\<ODS\s\+PROCTITLE\>"
syn match sasStatement "\<ODS\s\+PS\>"
syn match sasStatement "\<ODS\s\+RESULTS\>"
syn match sasStatement "\<ODS\s\+RTF\>"
syn match sasStatement "\<ODS\s\+SELECT\>"
syn match sasStatement "\<ODS\s\+SHOW\>"
syn match sasStatement "\<ODS\s\+TRACE\>"
syn match sasStatement "\<ODS\s\+USEGOPT\>"
syn match sasStatement "\<ODS\s\+VERIFY\>"
syn match sasStatement "\<ODS\s\+WML\>"
syn match sasStatement "\<ODS\s\+\w\+\s\+CLOSE\>"

syn keyword sasStatement _NEW_ ABORT ARRAY ATTRIB BY CALL CARDS CARDS4 CATNAME CONTINUE
syn keyword sasStatement DATALINES DATALINES4 DECLARE DCL DELETE DESCRIBE DISPLAY DM DO DROP
syn keyword sasStatement ELSE END ENDSAS ERROR EXECUTE FILE FILENAME FOOTNOTE FORMAT IF
syn keyword sasStatement INFILE INFORMAT INPUT KEEP LABEL LEAVE LENGTH LIBNAME LINK LIST
syn keyword sasStatement LOCK LOSTCARD MERGE MISSING MODIFY Null OPTIONS OUTPUT PAGE PUT
syn keyword sasStatement PUTLOG REDIRECT REMOVE RENAME REPLACE RETAIN RETURN SASFILE SELECT
syn keyword sasStatement SET SKIP STOP THEN TITLE UPDATE WHERE WINDOW X

" *******
" Options
" *******
syn match sasOption "\<APPLETLOC\s*="
syn match sasOption "\<ARMAGENT\s*="
syn match sasOption "\<ARMLOC\s*="
syn match sasOption "\<ARMSUBSYS\s*="
syn match sasOption "\<AUTHPROVIDERDOMAIN\s*="
syn match sasOption "\<AUTOSAVELOC\s*="
syn match sasOption "\<BINDING\s*="
syn match sasOption "\<BOTTOMMARGIN\s*="
syn match sasOption "\<BUFNO\s*="
syn match sasOption "\<BUFSIZE\s*="
syn match sasOption "\<CATCACHE\s*="
syn match sasOption "\<CBUFNO\s*="
syn match sasOption "\<CMPLIB\s*="
syn match sasOption "\<CMPOPT\s*="
syn match sasOption "\<COMPRESS\s*="
syn match sasOption "\<COPIES\s*="
syn match sasOption "\<CPUCOUNT\s*="
syn match sasOption "\<DATASTMTCHK\s*="
syn match sasOption "\<DATESTYLE\s*="
syn match sasOption "\<DEVICE\s*="
syn match sasOption "\<DFLANG\s*="
syn match sasOption "\<DKRICOND\s*="
syn match sasOption "\<DKROCOND\s*="
syn match sasOption "\<DLDMGACTION\s*="
syn match sasOption "\<DMSLOGSIZE\s*="
syn match sasOption "\<DMSOUTSIZE\s*="
syn match sasOption "\<EMAILAUTHPROTOCOL\s*="
syn match sasOption "\<EMAILID\s*="
syn match sasOption "\<EMAILPW\s*="
syn match sasOption "\<ENGINE\s*="
syn match sasOption "\<ERRORCHECK\s*="
syn match sasOption "\<ERRORS\s*="
syn match sasOption "\<FIRSTOBS\s*="
syn match sasOption "\<FMTSEARCH\s*="
syn match sasOption "\<FONTSLOC\s*="
syn match sasOption "\<FORMCHAR\s*="
syn match sasOption "\<FORMDLIM\s*="
syn match sasOption "\<FORMS\s*="
syn match sasOption "\<GISMAPS\s*="
syn match sasOption "\<IBUFSIZE\s*="
syn match sasOption "\<INITSTMT\s*="
syn match sasOption "\<INVALIDDATA\s*="
syn match sasOption "\<LEFTMARGIN\s*="
syn match sasOption "\<LINESIZE\s*="
syn match sasOption "\<LOGPARM\s*="
syn match sasOption "\<MAPS\s*="
syn match sasOption "\<MCOMPILENOTE\s*="
syn match sasOption "\<METAAUTORESOURCES\s*="
syn match sasOption "\<METACONNECT\s*="
syn match sasOption "\<METAENCRYPTALG\s*="
syn match sasOption "\<METAENCRYPTLEVEL\s*="
syn match sasOption "\<METAID\s*="
syn match sasOption "\<METAPASS\s*="
syn match sasOption "\<METAPORT\s*="
syn match sasOption "\<METAPROFILE\s*="
syn match sasOption "\<METAPROTOCOL\s*="
syn match sasOption "\<METAREPOSITORY\s*="
syn match sasOption "\<METASERVER\s*="
syn match sasOption "\<METAUSER\s*="
syn match sasOption "\<MINDELIMITER\s*="
syn match sasOption "\<MISSING\s*="
syn match sasOption "\<MSGLEVEL\s*="
syn match sasOption "\<MSYMTABMAX\s*="
syn match sasOption "\<MVARSIZE\s*="
syn match sasOption "\<NEWS\s*="
syn match sasOption "\<OBS\s*="
syn match sasOption "\<ORIENTATION\s*="
syn match sasOption "\<PAGENO\s*="
syn match sasOption "\<PAGESIZE\s*="
syn match sasOption "\<PAPERDEST\s*="
syn match sasOption "\<PAPERSIZE\s*="
syn match sasOption "\<PAPERSOURCE\s*="
syn match sasOption "\<PAPERTYPE\s*="
syn match sasOption "\<PARM\s*="
syn match sasOption "\<PARMCARDS\s*="
syn match sasOption "\<PRINTERPATH\s*="
syn match sasOption "\<REUSE\s*="
syn match sasOption "\<RIGHTMARGIN\s*="
syn match sasOption "\<S2\s*="
syn match sasOption "\<S\s*="
syn match sasOption "\<SASAUTOS\s*="
syn match sasOption "\<SASHELP\s*="
syn match sasOption "\<SASMSTORE\s*="
syn match sasOption "\<SASUSER\s*="
syn match sasOption "\<SEQ\s*="
syn match sasOption "\<SKIP\s*="
syn match sasOption "\<SORTDUP\s*="
syn match sasOption "\<SORTSEQ\s*="
syn match sasOption "\<SORTSIZE\s*="
syn match sasOption "\<SUMSIZE\s*="
syn match sasOption "\<SYSPARM\s*="
syn match sasOption "\<SYSPRINTFONT\s*="
syn match sasOption "\<TERMSTMT\s*="
syn match sasOption "\<TEXTURELOC\s*="
syn match sasOption "\<TOPMARGIN\s*="
syn match sasOption "\<TRAINLOC\s*="
syn match sasOption "\<TRANTAB\s*="
syn match sasOption "\<USER\s*="
syn match sasOption "\<UTILLOC\s*="
syn match sasOption "\<UUIDCOUNT\s*="
syn match sasOption "\<UUIDGENDHOST\s*="
syn match sasOption "\<V6CREATEUPDATE\s*="
syn match sasOption "\<VALIDFMTNAME\s*="
syn match sasOption "\<VALIDVARNAME\s*="
syn match sasOption "\<WORK\s*="
syn match sasOption "\<YEARCUTOFF\s*="
syn match sasOption "\<_LAST_\s*="

syn keyword sasOption ASYNCHIO BATCH BYERR BYLINE BYSORTED CAPS CARDIMAGE CENTER CHARCODE CLEANUP
syn keyword sasOption CMDMAC COLLATE COLORPRINTING CPUID DATE DETAILS DMR DMS DMSEXP DMSSYNCHK
syn keyword sasOption DSNFERR DTRESET DUPLEX ECHOAUTO EMAILHOST EMAILPORT ERRORABEND ERRORBYABEND EXPLORER FMTERR
syn keyword sasOption GWINDOW HELPENCMD IMPLMAC INITCMD LABEL MACRO MAUTOLOCDISPLAY MAUTOSOURCE MERGENOBY MERROR
syn keyword sasOption MFILE MLOGIC MLOGICNEST MPRINT MPRINTNEST MRECALL MSTORED MULTENVAPPL NOTES NUMBER
syn keyword sasOption OBJECTSERVER OVP PAGEBREAKINITIAL PRINTINIT PRINTMSGLIST QUOTELENMAX REPLACE RSASUSER SERROR SETINIT
syn keyword sasOption SOLUTIONS SORTEQUALS SOURCE SOURCE2 SPOOL STARTLIB SYMBOLGEN SYNTAXCHECK TERMINAL THREADS
syn keyword sasOption TOOLSMENU UNIVERSALPRINT VIEWMENU VNFERR WORKINIT WORKTERM

" **********
" Procedures
" **********
syn keyword sasProc contained PROC

syn keyword sasProcName contained APPEND BMDP CALENDAR CATALOG CHART CIMPORT COMPARE CONTENTS CONVERT COPY
syn keyword sasProcName contained CORR CPORT CV2VIEW DATASETS DBCSTAB DOCUMENT EXPORT FONTREG FORMAT FREQ
syn keyword sasProcName contained FSLIST IML IMPORT MEANS OPTIONS OPTLOAD OPTSAVE PDS PDSCOPY PLOT PMENU
syn keyword sasProcName contained PRINT PRINTTO PRTDEF PRTEXP PWENCODE RANK REGISTRY RELEASE REPORT SORT
syn keyword sasProcName contained SOURCE SQL STANDARD SUMMARY TABULATE TAPECOPY TAPELABEL TEMPLATE TIMEPLOT TRANSPOSE
syn keyword sasProcName contained TRANTAB UNIVARIATE
syn region sasProcedure start=/\(^\|;\)\s*PROC\>/ end=/;/  keepend transparent extend contains=sasProc,sasProcName,@sasCommon

" PROC APPEND
" Adds observations from one SAS data set to the end of another SAS data set.

" PROC BMDP
" Invokes a BMDP program to analyze data in a SAS data set.

" PROC CALENDAR
" Displays data from a SAS data set in a monthly calendar format. PROC CALENDAR can display 
" holidays in the month, schedule tasks, and process data for multiple calendars with work schedules 
" that vary.

" PROC CATALOG
" Manages entries in SAS catalogs. PROC CATALOG is an interactive, nonwindowing procedure that 
" enables you to display the contents of a catalog, copy an entire catalog or specific entries 
" in a catalog, and rename, exchange, or delete entries in a catalog.

" PROC CHART
" Produces vertical and horizontal bar charts, block charts, pie charts, and star charts. These 
" charts provide a quick visual representation of the values of a single variable or several variables. 
" PROC CHART can also display a statistic associated with the values.

" PROC CIMPORT
" Restores a transport file created by the CPORT procedure to its original form (a SAS data library, 
" catalog, or data set) in the format appropriate to the operating environment. Coupled with the 
" CPORT procedure, PROC CIMPORT enables you to move SAS data libraries, catalogs, and data sets 
" from one operating environment to another.

" PROC COMPARE
" Compares the contents of two SAS data sets. You can also use PROC COMPARE to compare the values 
" of different variables within a single data set. PROC COMPARE produces a variety of reports 
" on the comparisons that it performs.

" PROC CONTENTS
" Prints descriptions of the contents of one or more files in a SAS data library.

" PROC CONVERT
" Converts BMDP system files, OSIRIS system files, and SPSS portable files to SAS data sets.

" PROC COPY
" Copies an entire SAS data library or specific members of the library. You can limit processing 
" to specific types of library members.

" PROC CORR
" Computes Pearson product-moment and weighted product-moment correlation coefficients between 
" variables and descriptive statistics for these variables. In addition, PROC CORR can compute 
" three nonparametric measures of association (Spearman's rank-order correlation, Kendall's tau-b, 
" and Hoeffding's measure of dependence, D), partial correlations (Pearson's partial correlation, 
" Spearman's partial rank-order correlation, and Kendall's partial tau-b), and Cronbach's coefficient alpha.

" PROC CPORT
" Writes SAS data libraries, data sets, and catalogs in a special format called a transport file. 
" Coupled with the CIMPORT procedure, PROC CPORT enables you to move SAS libraries, data sets, 
" and catalogs from one operating environment to another.

" PROC CV2VIEW
" Converts SAS/ACCESS view descriptors to PROC SQL views. Starting in SAS System 9, conversion 
" of SAS/ACCESS view descriptors to PROC SQL views is recommended because PROC SQL views are platform 
" independent and enable you to use the LIBNAME statement.

" PROC DATASETS
" Lists, copies, renames, and deletes SAS files and SAS generation groups, manages indexes, and 
" appends SAS data sets in a SAS data library. The procedure provides all the capabilities of 
" the APPEND, CONTENTS, and COPY procedures. You can also modify variables within data sets, manage 
" data set attributes, such as labels and passwords, or create and delete integrity constraints.

" PROC DBCSTAB
" Produces conversion tables for the double-byte character sets that SAS supports.

" PROC DOCUMENT
" Manipulates procedure output that is stored in ODS documents. PROC DOCUMENT enables a user to 
" browse and edit output objects and hierarchies, and to replay them to any supported ODS output format.

" PROC EXPORT
" Reads data from a SAS data set and writes it to an external data source.

" PROC FONTREG
" Adds system fonts to the SAS registry.

" PROC FORMAT
" Creates user-defined informats and formats for character or numeric variables. PROC FORMAT also 
" prints the contents of a format library, creates a control data set to write other informats 
" or formats, and reads a control data set to create informats or formats.

" PROC FREQ
" Produces one-way to n-way frequency tables and reports frequency counts. PROC FREQ can compute 
" chi-square tests for one-way to n-way tables, tests and measures of association and of agreement 
" for two-way to n-way cross-tabulation tables, risks and risk difference for 2×2 tables, trends 
" tests, and Cochran-Mantel-Haenszel statistics. You can also create output data sets.

" PROC FSLIST
" Displays the contents of an external file or copies text from an external file to the SAS Text Editor.

" PROC IML
" SAS/IML software gives you access to a powerful and flexible programming language (Interactive 
" Matrix Language) in a dynamic, interactive environment. The fundamental object of the language 
" is a data matrix.
syn keyword sasCallIML contained APPCORT ARMACOV ARMALIK CHANGE COMPORT DELETE EIGEN EXECUTE FARMACOV FARMAFIT
syn keyword sasCallIML contained FARMALIK FARMASIM FDIF GAEND GAGETMEM GAGETVAL GAINIT GAREEVAL GAREGEN GASETCRO
syn keyword sasCallIML contained GASETMUT GASETOBJ GASETSEL GBLKVP GBLKVPD GCLOSE GDELETE GDRAW GDRAWL GENEIG
syn keyword sasCallIML contained GGRID GINCLUDE GOPEN GPIE GPIEXY GPOINT GPOLY GPORT GPORTPOP GPORTSTK GTEXT GVTEXT GXAXIS GYAXIS
syn keyword sasCallIML contained GSCALE GSCRIPT GSET GSHOW GSORTH GSTART GSTOP GSTRLEN GWINDOW IPF
syn keyword sasCallIML contained ITBICG ITCGRAD ITMINRES KALCVF KALCVS KALDFF KALDFS LAV LCP LMS
syn keyword sasCallIML contained LP LTS LUPDT MARG MAXQFORM MCD MVE NLPCG NLPDD NLPFDD
syn keyword sasCallIML contained NLPFEA NLPHQN NLPLM NLPNMS NLPNRA NLPNRR NLPQN NLPQUA NLPTR ODE
syn keyword sasCallIML contained ORTVEC PGRAF PUSH QR QUAD QUEUE RANDGEN RANDSEED RENAME RUPDT RDODT RUPDT SPLINEC SEQ SEQSCALE SEQSHIFT
syn keyword sasCallIML contained RZLIND SEQSCALE SEQSHIFT SOLVELIN SORT SORTNDX SOUND SVD TEIGEN TPSPLINE
syn keyword sasCallIML contained TPSPLNEV TSBAYSEA TSDECOMP TSMLOCAR TSMLOMAR TSMULMAR TSPEARS TSPRED TSROOT TSTVCAR
syn keyword sasCallIML contained TSUNIMAR VALSET VARMACOV VARMALIK VARMASIM VNORMAL VTSROOT WAVFT WAVGET WAVIFT WAVPRINT WAVTHRSH

syn keyword sasFunctionIML contained ABS ALL ANY APPLY ARMASIM BLOCK BRANKS BTRAN BYTE CHAR
syn keyword sasFunctionIML contained CHOOSE CONCAT CONTENTS CONVEXIT COVLAG CSHAPE CUSUM CVEXHULL DATASETS DESIGN
syn keyword sasFunctionIML contained DESIGNF DET DIAG DO DURATION ECHELON EIGVAL EIGVEC EXP FFT
syn keyword sasFunctionIML contained FORWARD GASETUP GINV HALF HANKEL HDIR HERMITE HOMOGEN I IFFT
syn keyword sasFunctionIML contained INSERT INT INV INVUPDT J JROOT LENGTH LOC LOG MAD
syn keyword sasFunctionIML contained MAX MIN MOD NAME NCOL NLENG NORMAL NROW NUM OPSCAL
syn keyword sasFunctionIML contained ORPOL POLYROOT PRODUCT PV RANK RANKTIE RATES RATIO REMOVE REPEAT
syn keyword sasFunctionIML contained ROOT ROWCAT ROWCATC SETDIF SHAPE SOLVE SPLINEV SPOT SQRSYM SQRT
syn keyword sasFunctionIML contained SSQ STORAGE SUBSTR SUM SWEEP SYMSQR T TEIGVAL TEIGVEC TOEPLITZ
syn keyword sasFunctionIML contained TRACE TRISOLV TYPE UNIFORM UNION UNIQUE UNIQUEBY VALUE VECDIAG XMULT XSECT YIELD

syn keyword sasStatementIML contained ABORT APPEND CALL CLOSE CLOSEFILE CREATE DELETE DISPLAY EDIT END
syn keyword sasStatementIML contained FILE FIND FINISH FORCE FREE GOTO INDEX INFILE INPUT
syn keyword sasStatementIML contained LINK LIST LOAD MATTRIB PAUSE PRINT PURGE PUT READ
syn keyword sasStatementIML contained REMOVE REPLACE RESET RESUME RETURN RETURN RUN SAVE SETIN SETOUT
syn keyword sasStatementIML contained SHOW SORT SPLINE START STOP STORE SUMMARY USE WINDOW
syn match sasStatementIML contained "DO DATA"

syn region sasProcIML start=/\<PROC\s\+IML\>/ end=/\<QUIT\s*;/ fold transparent keepend extend contains=sasCallIML,sasFunctionIML,sasStatementIML,sasProc,sasProcName,@sasCommon

" PROC IMPORT
" Reads data from an external data source and writes them to a SAS data set.

" PROC MEANS
" Computes descriptive statistics for numeric variables across all observations and within groups 
" of observations. You can also create an output data set that contains specific statistics and 
" identifies minimum and maximum values for groups of observations.

" PROC OPTIONS
" Lists the current values of all SAS system options.

" PROC OPTLOAD
" Reads SAS system option settings from the SAS registry or a SAS data set, and puts them into effect.

" PROC OPTSAVE
" Saves SAS system option settings to the SAS registry or a SAS data set.

" PROC PDS
" Lists, deletes, and renames the members of a partitioned data set.

" PROC PDSCOPY
" Copies partitioned data sets from disk to tape, disk to disk, tape to tape, or tape to disk.

" PROC PLOT
" Produces scatter plots that graph one variable against another. The coordinates of each point 
" on the plot correspond to the two variables' values in one or more observations of the input data set.

" PROC PMENU
" Defines menus that you can use in DATA step windows, macro windows, and SAS/AF windows, or in 
" any SAS application that enables you to specify customized menus.

" PROC PRINT
" Prints the observations in a SAS data set, using all or some of the variables. PROC PRINT can 
" also print totals and subtotals for numeric variables.

" PROC PRINTTO
" Defines destinations for SAS procedure output and the SAS log.

" PROC PRTDEF
" Creates printer definitions for individual SAS users or all SAS users.

" PROC PRTEXP
" Exports printer definition attributes to a SAS data set so that they can be easily replicated and modified.

" PROC PWENCODE
" Encodes passwords for use in SAS programs.

" PROC RANK
" Computes ranks for one or more numeric variables across the observations of a SAS data set. 
" The ranks are written to a new SAS data set. Alternatively, PROC RANK produces normal scores or other rank scores.

" PROC REGISTRY
" Imports registry information into the USER portion of the SAS registry.

" PROC RELEASE
" Releases unused space at the end of a disk data set in the z/OS environment.

" PROC REPORT
" Combines features of the PRINT, MEANS, and TABULATE procedures with features of the DATA step 
" in a single report-writing tool that can produce both detail and summary reports.
syn keyword sasProcReportWords contained HEADSKIP HEADLINE NOWD SPLIT MISSING COLUMN ORDER DISPLAY
syn keyword sasProcReportWords contained BREAK AFTER DEFINE SPACING WIDTH CENTER LEFT RIGHT INTERNAL

" PROC SORT
" Sorts observations in a SAS data set by one or more variables. PROC SORT stores the resulting 
" sorted observations in a new SAS data set or replaces the original data set.

" PROC SOURCE
" Provides an easy way to back up and process source library data sets.

" PROC SQL
" Implements a subset of the Structured Query Language (SQL) for use in SAS. SQL is a standardized, 
" widely used language that retrieves and updates data in SAS data sets, SQL views, and DBMS tables, 
" as well as views based on those tables. PROC SQL can also create tables and views, summaries, 
" statistics, and reports and perform utility functions such as sorting and concatenating.
syn keyword sasProcSqlOption contained DOUBLE NODOUBLE ERRORSTOP NOERRORSTOP EXEC NOEXEC FEEDBACK NOFEEDBACK 
syn keyword sasProcSqlOption contained FLOW NOFLOW NUMBER NONUMBER PRINT NOPRINT PROMPT NOPROMPT 
syn keyword sasProcSqlOption contained SORTMSG NOSORTMSG STIMER NOSTIMER THREADS NOTHREADS
syn match sasProcSqlOption contained "BUFFERSIZE\s*="
syn match sasProcSqlOption contained "DQUOTE\s*="
syn match sasProcSqlOption contained "INOBS\s*="
syn match sasProcSqlOption contained "LOOPS\s*="
syn match sasProcSqlOption contained "OUTOBS\s*="
syn match sasProcSqlOption contained "SORTSEQ\s*="
syn match sasProcSqlOption contained "UNDO_POLICY\s*="

syn keyword sasProcSql contained ADD ALL  ALTER AS AVG BETWEEN BTRIM BY
syn keyword sasProcSql contained CALCULATED CASE CHAR[ACTER] COALESCE CONNECT CONNECTION CONSTRAINT CONSTRAINTS
syn keyword sasProcSql contained CORRESPONDING COUNT CREATE CROSS CSS CV DATE
syn keyword sasProcSql contained DEC[IMAL] DELETE DESCRIBE DISCONNECT DISTINCT DOUBLE PRECISION DROP ELSE
syn keyword sasProcSql contained END EXCEPT EXECUTE EXISTS FLOAT FOREIGN FREQ FROM
syn keyword sasProcSql contained FULL GROUP HAVING IN INDEX INNER INSERT INTERSECT
syn keyword sasProcSql contained INTO INT[EGER] IS JOIN KEY LEFT LIKE LOWCASE
syn keyword sasProcSql contained LOWER MAX MEAN MIN MISSING MODIFY N NMISS
syn keyword sasProcSql contained NULL NUMERIC ON ORDER OUTER PRIMARY PRT
syn keyword sasProcSql contained RANGE REAL RESET RIGHT SELECT SET STD STDERR
syn keyword sasProcSql contained SUBSTRING SUM SUMWGT T TABLE TO UNION UNIQUE
syn keyword sasProcSql contained UPDATE UPPER USING USS VALIDATE VALUES VAR VARCHAR 
syn keyword sasProcSql contained VIEW WHEN WHERE

syn match sasProcSql contained "\<CONTAINS\>" 
syn match sasProcSql contained "\<FORMAT\s*="
syn match sasProcSql contained "\<INFORMAT\s*="
syn match sasProcSql contained "\<LABEL\s*="
syn match sasProcSql contained "\<LENGTH\s*="
syn match sasProcSql contained "\<TRANSCODE\s*="

syn region sasProcSqlOpt start="\<PROC\s\+SQL\>" end=";" transparent extend contains=sasProc,sasProcName,sasProcSqlOption,@sasCommon
syn region sasProcSqlStatement start="\<PROC\s\+SQL\>" end="\<QUIT\s*;" fold keepend transparent extend contains=sasProcSqlOpt,sasProcSql,sasFunction,sasDataOpt,@sasCommon

" PROC STANDARD
" Standardizes some or all of the variables in a SAS data set to a given mean and standard deviation 
" and produces a new SAS data set that contains the standardized values.

" PROC SUMMARY
" Computes descriptive statistics for the variables in a SAS data across all observations and 
" within groups of observations and outputs the results to a new SAS data set.

" PROC TABULATE
" Displays descriptive statistics in tabular form. The value in each table cell is calculated 
" from the variables and statistics that define the pages, rows, and columns of the table. The 
" statistic associated with each cell is calculated on values from all observations in that category. 
" You can write the results to a SAS data set.

" PROC TAPECOPY
" Copies an entire tape volume or files from one or more tape volumes to one output tape volume.

" PROC TAPELABEL
" Lists the label information of an IBM standard-labeled tape volume under the z/OS environment.

" PROC TEMPLATE
" Customizes ODS output for an entire SAS job or a single ODS output object.

" PROC TIMEPLOT
" Produces plots of one or more variables over time intervals.

" PROC TRANSPOSE
" Transposes a data set that changes observations into variables and vice versa.

" PROC TRANTAB
" Creates, edits, and displays customized translation tables.

" PROC UNIVARIATE
" Computes descriptive statistics (including quantiles), confidence intervals, and robust estimates 
" for numeric variables. Provides detail on the distribution of numeric variables, which include 
" tests for normality, plots to illustrate the distribution, frequency tables, and tests of location.



" ******
" Macros
" ******
syn match sasMacroVar "&\w\+\>\.\="
syn match sasMacroName	/%\w\+\>/
syn match sasMacroBold	"%MACRO\>\s\+\w\+\>"
syn match sasMacroBold	"%MEND\>\(\s\+\w\+\>\)\="

syn match sasMacro contained "%BQUOTE\>"
syn match sasMacro contained "%CMPRES\>"
syn match sasMacro contained "%COMPSTOR\>"
syn match sasMacro contained "%DATATYP\>"
syn match sasMacro contained "%DISPLAY\>"
syn match sasMacro contained "%DO\>"
syn match sasMacro contained "%ELSE\>"
syn match sasMacro contained "%END\>"
syn match sasMacro contained "%EVAL\>"
syn match sasMacro contained "%GLOBAL\>"
syn match sasMacro contained "%GOTO\>"
syn match sasMacro contained "%IF\>"
syn match sasMacro contained "%INDEX\>"
syn match sasMacro contained "%INPUT\>"
syn match sasMacro contained "%KEYDEF\>"
syn match sasMacro contained "%LABEL\>"
syn match sasMacro contained "%LEFT\>"
syn match sasMacro contained "%LENGTH\>"
syn match sasMacro contained "%LET\>"
syn match sasMacro contained "%LOCAL\>"
syn match sasMacro contained "%LOWCASE\>"
syn match sasMacro contained "%NRBQUOTE\>"
syn match sasMacro contained "%NRBQUOTE\>"
syn match sasMacro contained "%NRQUOTE\>"
syn match sasMacro contained "%NRSTR\>"
syn match sasMacro contained "%QCMPRES\>"
syn match sasMacro contained "%QCMPRES\>"
syn match sasMacro contained "%QLEFT\>"
syn match sasMacro contained "%QLOWCASE\>"
syn match sasMacro contained "%QSCAN\>"
syn match sasMacro contained "%QSUBSTR\>"
syn match sasMacro contained "%QSYSFUNC\>"
syn match sasMacro contained "%QTRIM\>"
syn match sasMacro contained "%QUOTE\>"
syn match sasMacro contained "%QUPCASE\>"
syn match sasMacro contained "%SCAN\>"
syn match sasMacro contained "%STR\>"
syn match sasMacro contained "%SUBSTR\>"
syn match sasMacro contained "%SUPERQ\>"
syn match sasMacro contained "%SYSCALL\>"
syn match sasMacro contained "%SYSEVALF\>"
syn match sasMacro contained "%SYSEXEC\>"
syn match sasMacro contained "%SYSFUNC\>"
syn match sasMacro contained "%SYSGET\>"
syn match sasMacro contained "%SYSLPUT\>"
syn match sasMacro contained "%SYSPROD\>"
syn match sasMacro contained "%SYSRC\>"
syn match sasMacro contained "%SYSRPUT\>"
syn match sasMacro contained "%THEN\>"
syn match sasMacro contained "%TO\>"
syn match sasMacro contained "%TRIM\>"
syn match sasMacro contained "%UNQUOTE\>"
syn match sasMacro contained "%UNTIL\>"
syn match sasMacro contained "%UPCASE\>"
syn match sasMacro contained "%VERIFY\>"
syn match sasMacro contained "%WHILE\>"
syn match sasMacro contained "%WINDOW\>"

" ************
" SCL Programs
" ************
syn region  sasClassDecl start="^\s*class\s" end=";" contains=sasScopeWords,sasPathWords,sasOOPWords,sasString,@sasCommon
syn region  sasMethodDecl start="^\s*\w\+:\(\s\|$\)" end=";" contains=sasScopeWords,sasOOPWords,sasLabel,sasString,@sasCommon
syn match   sasListDecl  "^\s*\(public\|protected\|private\|abstract\)\=\s\+list\s\+\w\+\>" contains=sasScopeWords,sasOOPWords,@sasCommon
syn keyword sasClassEnd  endclass
syn keyword sasMethodEnd endmethod
syn keyword sasScopeWords contained public protected private abstract extends implements interface
syn keyword sasScopeWords contained static synchronized transient volatile final strictfp serializable
syn keyword sasOOPWords_2 import dcl declare char int num if eq ne ge gt le lt return
syn match sasPathWords contained "\.class\>"
syn keyword sasOOPWords  contained class method list import
syn match sasLabel  contained "\<\w\+:"

" *********
" Highlight
" *********
if version >= 508 || !exists("did_sas_syntax_inits")
	if version < 508
		let did_sas_syntax_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif
	" setlocal cursorcolumn
	" setlocal cursorline

	" Default sas enhanced editor color syntax
	" I have changed guibg=White to NONE
	hi sComment  term=bold cterm=NONE ctermfg=Green  ctermbg=Black gui=NONE   guifg=DarkGreen guibg=NONE
	hi sasTodo  term=bold cterm=NONE ctermfg=Green  ctermbg=Black gui=bold,underline guifg=DarkGreen guibg=NONE
	hi sCard  term=bold cterm=NONE ctermfg=Black  ctermbg=Yellow gui=NONE   guifg=Black  guibg=LightYellow
	hi sDate_Time term=NONE cterm=bold ctermfg=Green  ctermbg=Black gui=bold   guifg=SeaGreen guibg=NONE
	hi sKeyword  term=NONE cterm=NONE ctermfg=Blue  ctermbg=Black gui=NONE   guifg=Blue  guibg=NONE
	hi sFmtInfmt term=NONE cterm=NONE ctermfg=LightGreen ctermbg=Black gui=NONE   guifg=SeaGreen guibg=NONE
	hi sString  term=NONE cterm=NONE ctermfg=Magenta ctermbg=Black gui=NONE   guifg=Purple guibg=NONE
	hi sText  term=NONE cterm=NONE ctermfg=White  ctermbg=Black gui=bold   guifg=Black  guibg=NONE
	hi sNumber  term=NONE cterm=bold ctermfg=Green  ctermbg=Black gui=bold   guifg=SeaGreen guibg=NONE
	" hi sNumber  term=NONE cterm=NONE ctermfg=Green  ctermbg=Black gui=NONE   guifg=SeaGreen guibg=NONE
	hi sasFormat term=NONE cterm=NONE ctermfg=Green  ctermbg=Black gui=NONE   guifg=SeaGreen guibg=NONE
	hi sProc  term=NONE cterm=bold ctermfg=Blue  ctermbg=Black gui=bold   guifg=Navy  guibg=NONE
	hi sSection  term=NONE cterm=bold ctermfg=Blue  ctermbg=Black gui=bold   guifg=Navy  guibg=NONE
	hi mDefine  term=NONE cterm=bold ctermfg=White  ctermbg=Black gui=bold   guifg=Black  guibg=NONE
	hi mKeyword  term=NONE cterm=NONE ctermfg=Blue  ctermbg=Black gui=NONE   guifg=Blue  guibg=NONE
	hi sasMacroBold term=NONE cterm=Bold ctermfg=Blue  ctermbg=Black gui=Bold   guifg=DarkBlue guibg=NONE
	hi sasMacroName term=NONE cterm=Bold ctermfg=Black  ctermbg=Black gui=Bold   guifg=Black  guibg=NONE
	hi mReference term=NONE cterm=bold ctermfg=White  ctermbg=Black gui=bold   guifg=Blue  guibg=NONE
	hi mSection  term=NONE cterm=NONE ctermfg=Blue  ctermbg=Black gui=bold   guifg=Navy  guibg=NONE
	hi mText  term=NONE cterm=NONE ctermfg=White  ctermbg=Black gui=bold   guifg=Black  guibg=NONE
	hi sLabel  term=NONE cterm=NONE ctermfg=Magenta ctermbg=Black gui=bold   guifg=Purple guibg=NONE

	hi cursorColumn guibg=white
	hi cursorLine guibg=white

	" Special hilighting for the SAS proc section

	HiLink sasComment  sComment
	HiLink sasConditional sKeyword
	HiLink sasSection   sSection
	HiLink sasFunction  sKeyword
	HiLink sasMacro  sKeyword
	HiLink sasMacroVar  NonText
	HiLink sasNumber  sNumber
	HiLink sasStatement sKeyword
	HiLink sasString  sString
	HiLink sasProc   sProc
	HiLink sasProcName   sProc
	HiLink sasProcIML  sProc
	HiLink sasFunctionIML  sKeyword
	HiLink sasProcSql sKeyword
	HiLink sasProcSqlOption	sKeyword
	HiLink sasCallIML      sKeyword
	HiLink sasStatementIML sasStatement
	HiLink  sasDataOption sKeyword

	" OOP statements (JMP)

	" HiLink sasOOPWords   sProc
	HiLink sasOOPWords   sKeyword
	HiLink sasOOPWords_2  sKeyword
	HiLink sasClassDecl  NONE
	HiLink sasMethodDecl  NONE
	HiLink sasListDecl   NONE
	HiLink sasClassEnd   sasOOPWords
	HiLink sasMethodEnd  sasOOPWords
	HiLink sasScopeWords  sKeyword
	HiLink sasPathWords  sasClassDecl
	HiLink sasLabel   sLabel

	" (Bob Heckel)
	" HiLink sasTodo    Todo
	HiLink sasCards   sCard
	" (Bob Heckel)
	HiLink sasUnderscore  PreProc
	" (Manuel Picaza)
	HiLink sasOption   sKeyword
	HiLink sasData    sSection
	" HiLink sasGlobalStatement sKeyword
	HiLink sasProcReportWords sKeyword

	delcommand HiLink
endif

" *******
" Folding
" *******
"syn region sasSQLfold  start="^\s*proc\s\+sql\>" end="\<quit\s*;\s*" fold transparent keepend extend
syn region sasSUBMITfold start="^\s*submit\(\s\+continue\>\)\=\s*;" end="\<endsubmit\s*;\s*" fold transparent keepend extend
syn region sasDATAfold  start="^\s*data\s\+&\=\w" end="\<run\s*;\s*" fold transparent keepend extend
syn region sasFORMATfold start="^\s*proc\s\+format\>" end="\<run\s*;\s*" fold transparent keepend extend
syn region sasREPORTfold start="^\s*proc\s\+report\>" end="\<run\s*;\s*" fold transparent keepend extend
syn region sasMACROfold  start="^\s*%macro\s\+\w\+" end="^\s*%mend\s\+\w\+" fold transparent keepend extend contains=sasMacro
syn region sasDOfold1  start="%do\>.\{-};" end="%end\s*;" fold transparent keepend extend contains=ALL
syn region sasDOfold2  start="\(\_^\|[^%]\)\<do\>.\{-};" end="\(\_^\|[^%]\)\<end\s*;" fold transparent keepend extend contains=ALL
"syn region sasTHEN_DOfold start="\<then\>\(\s*\n\)*\s*%\=\<do\s*;" end="\<end\s*;" fold transparent keepend extend contains=ALL
"syn region sasELSE_DOfold start="\<else\>\(\s*\n\)*\s*%\=\<do\s*;" end="\<end\s*;" fold transparent keepend extend contains=ALL
syn region sasMETHODfold start="^\s*\w\+\s*:.*\<method\>" end="\<endmethod\>" fold transparent keepend extend contains=ALL
syn region sasLABELfold  start="^\s*\w\+:\s*\n" end="^\s*return\s*;" fold transparent keepend extend contains=ALL
"syn region sasCOMMENTfold Done in sasComment syn!!!
set foldmethod=syntax
set foldcolumn=4
"set foldopen=all
set foldlevel=100
"set foldlevelstart=100
highlight Folded guibg=NONE guifg=LightBlue gui=Bold
highlight FoldColumn guibg=White guifg=LightBlue

" **********************
" Ending highlight stuff
" **********************
syn sync fromstart

let b:current_syntax = "sas"

set sw=4
set ts=4
set expandtab
set nobackup

" *********
" Indenting
" *********
" Only load this indent file when no other was loaded.
if exists("b:did_indent")
	finish
endif
let b:did_indent = 1

"flag indicating if it is in a comments area
let b:comments = 0

"flag indicating if it is in a data or proc step. If at the end of the
"step, there is not a "run;" statement, manually adjust the indent
let b:step = 0
"flag indicating if it is in a macro
let b:macro = 0
"flag indicating if it is in a if..then statement without a DO
let b:ifThen=0
"flag indicating if it is in a do..end statement
"let b:doEnd=0

setlocal indentexpr=SASGetIndent(v:lnum)
setlocal indentkeys='*;,o,O,0=),0=},0=]
"setlocal indentkeys+='*<Return>'
"setlocal indentkeys+==~DO,=~END,=~IF,=~THEN,=~ELSE
"setlocal indentkeys+==~PROC,=~DATA,
"setlocal indentkeys+==~PROC,=~DATA,=~RUN,=~QUIT
"setlocal indentkeys+==~%DO,=~%END,=~%IF,=~%THEN,=~%ELSE
"setlocal indentkeys+==~%MACRO,=~%MEND

" Only define the function once.
if exists("*SASGetIndent")
	finish
endif

" Similar to java.vim indent file
function! SkipSASBlanksAndComments(startline)
	let lnum = a:startline
	while lnum > 1
		let lnum = prevnonblank(lnum)
		if getline(lnum) =~ '\*/\s*$'
			while getline(lnum) !~ '/\*' && lnum > 1
				let lnum = lnum - 1
			endwhile
			if getline(lnum) =~ '^\s*/\*'
				let lnum = lnum - 1
			else
				break
			endif
		elseif getline(lnum) =~ '^\s*\*'
			let lnum = lnum - 1
		else
			break
		endif
	endwhile
	return lnum
endfunction

function SASGetIndent(lnum)
	let currstat = getline(v:lnum)

	"Skip comments line
	if b:comments == 1 || currstat =~ '^\s*\*' || currstat =~ '^\s*/\*'
		if currstat =~ '^\s*/\*'
			let b:comments = 1
		endif
		if currstat =~ '\*/\s*$'
			let b:comments = 0
		endif
		return -1
	endif

	"there is a run; statement missing in the last step, manually adjust the indent
	"if currstat =~? '^\s*\(data\|proc\)\>' && b:macro == 0
	"  let b:step = 1
	"  return 0
	"endif
	if currstat =~? '^\s*%macro\>'
		let b:macro = 1
		if b:step == 1
			let b:step = 0
		endif
		return 0
	endif

	"Find the previous non-blank and non comments line
	let plnum = SkipSASBlanksAndComments(v:lnum-1)

	"Use zero indent at the top of the file
	"if plnum == 0
	"  return 0
	"endif

	let ind = indent(plnum)
	let prevstat = getline(plnum)

	"Add a shiftwidth to statements following class,
	"method, do, %do, data, proc, submit, catch and %macro
	if (prevstat =~? '^\s*\(%\=do\|data\|proc\|%macro\)\>'
				\ || prevstat =~? '^\s*\(submit\|catch\)\>'
					\ || prevstat =~? '\<class\s\+\w\+\.\w\+\>\|\<method\>\|\<%\=do\s*;')
		let ind = ind + &sw

		" Remove unwanted indent after logical and arithmetic ifs
		"    if (prevstat =~? '^\s*\(if\|else\)\>'
			"          \ && prevstat =~? ';' && prevstat !~? '\<do\>'
			"          \ && currstat !~? '^\s*else\>')
			"      let ind = ind - &sw
			"    endif
			"    if (prevstat =~? '^\s*\(%if\|%else\)\>'
			"          \ && prevstat =~? ';' && prevstat !~? '%do\>'
			"          \ && currstat !~? '^\s*%else\>')
			"      let ind = ind - &sw
			"    endif

			" If it's a do; statement, remove the unwanted indent
			if ((currstat =~? '^\s*%\=do\>') && (prevstat !~? '^\s*%\=do\>')
						\ && (currstat !~? '\<%\=to\>'))
				let ind = ind - &sw
			endif

			" If previous statement is like proc ... run; or data ... run;
			" remove the indent
			if prevstat =~? '^\s*\(data\|proc\)\>' && prevstat =~? '\(\<run\|\<quit\)\>\s*;'
				let ind = ind - &sw
			endif

			" If previous statement is like class ... endclass;
			"  or method ...  endmethod; remove the indent
			if prevstat =~? '\<class\s\+\w\+\.\w\+\>' && prevstat =~? '\<endclass\s*;'
				let ind = ind - &sw
			endif
			if prevstat =~? '\<method\>' && prevstat =~? '\<endmethod\s*;'
				let ind = ind - &sw
			endif
		endif

		" If there is lists in brakets, (...) align them
		if prevstat =~? '\((\|{\|[\)\s*$'
			let ind = ind + &sw
		endif
		if currstat =~? '^\s*\()\|}\|]\)'
			let ind = ind - &sw
		endif



		"Subtract a shiftwidth from else, end, %end, run and %mend
		if (currstat =~? '^\s*\(%\=end\|endcatch\|endsubmit\)\>')
			let ind = ind - &sw
		endif

		"If it is a %mend statement, indent is 0
		if currstat =~? '^\s*%mend\>'
			let b:step = 0
			let b:macro = 0
			return 0
		endif

		"If it is run statement, align to the corresponding data or proc statement
		if currstat =~? '\(\<run\|\<quit\)\s*;' && currstat !~? '^\s*\(data\|proc\)\>'
			let b:step = 0
			"if b:macro == 0
			"  return 0
			"else
			let ind = ind - &sw
			let alignline = v:lnum-1
			while alignline > 1
				if getline(alignline) =~? '^\s*\(data\|proc\)\>'
					let ind = indent(alignline)
					return ind
				else
					let alignline = alignline - 1
				endif
			endwhile
			"endif
		endif

		"If it is endclass statement, align to the corresponding class statement
		if currstat =~? '\<endclass\>'
			let ind = ind - &sw
			let alignline = v:lnum - 1
			while alignline > 1
				if getline(alignline) =~? '\<class\s\+\w\+\.\w\+\>'
					let ind = indent(alignline)
					return ind
				else
					let alignline = alignline - 1
				endif
			endwhile
		endif

		"If it is endmethod statement, align to the corresponding method statement
		if currstat =~? '\<endmethod\>'
			let ind = ind - &sw
			let alignline = v:lnum - 1
			while alignline > 1
				if getline(alignline) =~? '\<method\>'
					let ind = indent(alignline)
					return ind
				else
					let alignline = alignline - 1
				endif
			endwhile
		endif


		"Add a shiftwidth to statements following (if..then (without DO) in two
		"lines.
		"  if (prevstat =~? '%\=\<then\s*$')
		"    let ind = ind + &sw
		"  let b:ifThen=1
		" endif
		" if ((b:ifThen == 1) && (prevstat =~? ';\s*$'))
		"   let ind = ind - &sw
		"  let b:ifThen = 0
		" endif




		"If previous is a if...; or end; statement and current is a else statment,
		"add an additional indent
		"if ((prevstat =~? '\<end\s*;' && currstat =~? '\<else\>')
		"      \ || (prevstat =~? '\<%end\>' && currstat =~? '\<%else\>'))
		"  let ind = ind + &sw
		"endif

		"if..then without do; in two lines
		"if (getline(v:lnum-2) =~? '\s*if\>.*\<then\s*$' && prevstat !~? '^\s*do\s*;')
		"  let ind = ind - &sw
		"endif
		"if (getline(v:lnum-2) =~? '\s*else\s*$' && prevstat !~? '^\s*do\s*;'(
		"  let ind = ind - &sw
		"endif

		return ind
	endfunction

	" ***********************
	" Executing in Batch Mode
	" ***********************
	"function! SASForMakeLogPrint()
	" "Path to SAS.exe location (Substitute in l:sasPrgm all '\' by '\\' and all ' ' by '\ ')
	" let l:sasPrgm = "c:\\Archivos\ de\ programa\\SAS\\SAS\ 9.1\\sas.exe"
	" "Path to SASVx.CFG location (substitute in l:sasCFG all '\' by '\\' and all ' ' by '\ ')
	" let l:sasCFG = "c:\\Archivos\ de\ programa\\SAS\\SAS\ 9.1\\nls\\en\\sasv9.cfg"
	"" let l:fecha = strftime("_%Y%m%d_%H%M%S")
	" let l:fecha = strftime("_%Y%m%d")
	" let l:makeprgSAS = "set makeprg=" . l:sasPrgm . "\\ -CONFIG\\ " . l:sasCFG . "\\ -sysin\\ %\\ -log\\ %<" . l:fecha . ".log\\ -print\\ %<" . l:fecha . ".lst"
	" execute l:makeprgSAS
	" unlet l:fecha
	" unlet l:makeprgSAS
	" unlet l:sasPrgm
	" unlet l:sasCFG
	"" unlet l:makeprgSAS
	"
	" return makeprgSAS
	" echo l:makeprgSAS
	"endfunction
	"call SASForMakeLogPrint()

