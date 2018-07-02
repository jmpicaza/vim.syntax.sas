" ****************************************************
" Vim syntax file
" Language: SAS
" Last Change: XX Dec 2007
" Programmer: J. Manuel Picaza <jmpicaza@gmail.com>
" Version: 1.0
" ***************************************************
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

" *****************************************************
" GENERAL OPTIONS SOME USERS COULD WANT AND OTHERS NOT.
" *****************************************************
" Adjust the font. Comment the line if you want to use your own font!
"setlocal gfn=courier_new:h10:cANSI

" Modify the used words for letting '&' be part of a word.
"setlocal iskeyword+=038
setlocal iskeyword=@,48-57,_
setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:\*,ex:*/


setlocal formatoptions=tcqronB

" ft-vim-syntax
"let sh_minlines = 500
"let sh_maxlines = 1000
"syn sync fromstart
"syn sync ccomment sasComment
syntax sync minlines=200

syn case ignore
syn cluster sasCommon contains=sasComment,sasString,sasNumber,sasFormat,sasUnderscore,sasMacroVar,sasMacroName,sasMacro,sasSection

syn keyword sasSection RUN QUIT

" ********
" Comments
" ********
syn region  sasComment start=/\/\*/  end=/\*\// fold keepend extend contains=sasTodo
syn region  sasComment start="^\s*\*" end=";" keepend extend contains=sasTodo
syn region  sasComment start=";\s*\*"hs=s+1 end=";" keepend extend contains=sasTodo
syn match   sasComment "\(^\s*%*\*.\{-}\)\=\(;\+\s*%\=\*.\{-}\)\+;\+" extend contains=sasTodo

syn keyword sasTodo contained OJO FIXME JMP JOSE NOTE
syn match sasTodo contained "TODO:"

" *******
" Strings
" *******
"syn region sasString start=+"+  skip=+\\\\\|\\"+  end=+"+ keepend extend contains=sasMacroVarInString
syn region sasString start=+"+  skip=+""+  end=+"+ keepend extend contains=sasMacroVarInString
syn match sasMacroVarInString contained "&\w\+\>\.\="
"syn region sasString start=+'+  skip=+\\\\\|\\'+  end=+'+ keepend extend
syn region sasString start=+'+  skip=+''+  end=+'+ keepend extend

" *******
" Numbers
" *******
syn match sasNumber /-\=\<\d*\.\=\d\+\>\|-\=\<\d\+\>\|-\=\<\d\+\.\=\d*[Ee]\d\+\>/ 
syn match sasNumber /-\=\<\d*\.\=\d\+\>\|-\=\<\d\+\>\|-\=\<\d\+\.\=\d*[Ee]\d\+\>/
syn match sasNumber /'\w\{-4,10}'d\>\|"\w\{-4,10}"d\>/
syn match sasNumber /'\d\+\(:\d\+\.\=\d*\)*'t\>\|"\d\+\(:\d\+\.\=\d*\)*"t\>/

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

syn region sasCards start="^\s*CARDS.*" end="^\s*;\s*$"
syn region sasCards start="^\s*DATALINES.*" end="^\s*;\s*$"

"syn match sasOption /\c\s\+DATA\s*=/he=e-1
"syn keyword sasOption ACCESS CNTLOUT CNTLIN LIBRARY CATALOG COPY OUT
"syn keyword sasOption ENTRYTYPE KILL

syn match sasUnderscore "\<_NULL_\>"
syn match sasUnderscore "\<_INFILE_\>"
syn match sasUnderscore "\<_N_\>"
syn match sasUnderscore "\<_WEBOUT_\>"
syn match sasUnderscore "\<_NUMERIC_\>"
syn match sasUnderscore "\<_CHARACTER_\>"
syn match sasUnderscore "\<_ALL_\>"

"syn region sasDataOpt    start=/(/ end=/)/ contained  transparent extend contains=sasDataOption,@sasCommon
"syn region sasDataOpt    start=/(/ end=/)/ contained  transparent keepend contains=sasDataOption,@sasCommon
syn region sasDataHeader start=/\(^\|;\)\s*data\>/ end=/;/ keepend transparent extend contains=sasDataOption,sasData,@sasCommon
syn region sasDataSet start=/\(^\|;\)\s*set\>/ end=/;/ keepend transparent extend contains=sasDataOption,@sasCommon,sasStatement 
syn region sasDataStep  start="^\s*data\s\+&\=\w" end="\<run\s*;\s*" fold transparent keepend extend

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
syn match sasStatement "%LIST\>"
syn match sasStatement "%RUN\>"
syn match sasStatement "\<DO\s\+UNTIL\>"
syn match sasStatement "\<DO\s\+WHILE\>"
syn match sasStatement "\<GO\s\+TO\>"
"syn match sasStatement "\<ODS\s\+CHTML\>"
"syn match sasStatement "\<ODS\s\+CSVALL\>"
"syn match sasStatement "\<ODS\s\+DECIMAL_ALIGN\>"
"syn match sasStatement "\<ODS\s\+DOCBOOK\>"
"syn match sasStatement "\<ODS\s\+DOCUMENT\>"
"syn match sasStatement "\<ODS\s\+EXCLUDE\>"
"syn match sasStatement "\<ODS\s\+HTML3\>"
"syn match sasStatement "\<ODS\s\+HTMLCSS\>"
"syn match sasStatement "\<ODS\s\+HTML\>"
"syn match sasStatement "\<ODS\s\+IMODE\>"
"syn match sasStatement "\<ODS\s\+LISTING\>"
"syn match sasStatement "\<ODS\s\+MARKUP\>"
"syn match sasStatement "\<ODS\s\+OUTPUT\>"
"syn match sasStatement "\<ODS\s\+PATH\>"
"syn match sasStatement "\<ODS\s\+PCL\>"
"syn match sasStatement "\<ODS\s\+PDF\>"
"syn match sasStatement "\<ODS\s\+PHTML\>"
"syn match sasStatement "\<ODS\s\+PRINTER\>"
"syn match sasStatement "\<ODS\s\+PROCLABEL\>"
"syn match sasStatement "\<ODS\s\+PROCTITLE\>"
"syn match sasStatement "\<ODS\s\+PS\>"
"syn match sasStatement "\<ODS\s\+\(NO\)\=RESULTS\>"
"syn match sasStatement "\<ODS\s\+RTF\>"
"syn match sasStatement "\<ODS\s\+SELECT\>"
"syn match sasStatement "\<ODS\s\+SHOW\>"
"syn match sasStatement "\<ODS\s\+TRACE\>"
"syn match sasStatement "\<ODS\s\+USEGOPT\>"
"syn match sasStatement "\<ODS\s\+VERIFY\>"
"syn match sasStatement "\<ODS\s\+WML\>"
"syn match sasStatement "\<ODS\s\+\w\+\s\+CLOSE\>"

syn keyword sasStatement _NEW_ ABORT ARRAY ATTRIB BY CALL CARDS CARDS4 CATNAME CONTINUE
syn keyword sasStatement DATALINES DATALINES4 DECLARE DCL DELETE DESCRIBE DISPLAY DM DO DROP
syn keyword sasStatement ELSE END ENDSAS ERROR EXECUTE FILE FILENAME FOOTNOTE FORMAT IF
syn keyword sasStatement INFILE INFORMAT INPUT KEEP LABEL LEAVE LENGTH LIBNAME LINK LIST
syn keyword sasStatement LOCK LOSTCARD MERGE MISSING MODIFY Null OUTPUT PAGE PUT
syn keyword sasStatement PUTLOG REDIRECT REMOVE RENAME REPLACE RETAIN RETURN SASFILE SELECT
syn keyword sasStatement SET SKIP STOP THEN TITLE TO UPDATE WHERE WINDOW X

" *******
" Options
" *******
syn match sasOption contained "\<APPLETLOC\s*="
syn match sasOption contained "\<ARMAGENT\s*="
syn match sasOption contained "\<ARMLOC\s*="
syn match sasOption contained "\<ARMSUBSYS\s*="
syn match sasOption contained "\<AUTHPROVIDERDOMAIN\s*="
syn match sasOption contained "\<AUTOSAVELOC\s*="
syn match sasOption contained "\<BINDING\s*="
syn match sasOption contained "\<BOTTOMMARGIN\s*="
syn match sasOption contained "\<BUFNO\s*="
syn match sasOption contained "\<BUFSIZE\s*="
syn match sasOption contained "\<CATCACHE\s*="
syn match sasOption contained "\<CBUFNO\s*="
syn match sasOption contained "\<CMPLIB\s*="
syn match sasOption contained "\<CMPOPT\s*="
syn match sasOption contained "\<COMPRESS\s*="
syn match sasOption contained "\<COPIES\s*="
syn match sasOption contained "\<CPUCOUNT\s*="
syn match sasOption contained "\<DATASTMTCHK\s*="
syn match sasOption contained "\<DATESTYLE\s*="
syn match sasOption contained "\<DEVICE\s*="
syn match sasOption contained "\<DFLANG\s*="
syn match sasOption contained "\<DKRICOND\s*="
syn match sasOption contained "\<DKROCOND\s*="
syn match sasOption contained "\<DLDMGACTION\s*="
syn match sasOption contained "\<DMSLOGSIZE\s*="
syn match sasOption contained "\<DMSOUTSIZE\s*="
syn match sasOption contained "\<EMAILAUTHPROTOCOL\s*="
syn match sasOption contained "\<EMAILID\s*="
syn match sasOption contained "\<EMAILPW\s*="
syn match sasOption contained "\<ENGINE\s*="
syn match sasOption contained "\<ERRORCHECK\s*="
syn match sasOption contained "\<ERRORS\s*="
syn match sasOption contained "\<FIRSTOBS\s*="
syn match sasOption contained "\<FMTSEARCH\s*="
syn match sasOption contained "\<FONTSLOC\s*="
syn match sasOption contained "\<FORMCHAR\s*="
syn match sasOption contained "\<FORMDLIM\s*="
syn match sasOption contained "\<FORMS\s*="
syn match sasOption contained "\<GISMAPS\s*="
syn match sasOption contained "\<IBUFSIZE\s*="
syn match sasOption contained "\<INITSTMT\s*="
syn match sasOption contained "\<INVALIDDATA\s*="
syn match sasOption contained "\<LEFTMARGIN\s*="
syn match sasOption contained "\<LINESIZE\s*="
syn match sasOption contained "\<LS\s*="
syn match sasOption contained "\<LOGPARM\s*="
syn match sasOption contained "\<MAPS\s*="
syn match sasOption contained "\<MCOMPILENOTE\s*="
syn match sasOption contained "\<METAAUTORESOURCES\s*="
syn match sasOption contained "\<METACONNECT\s*="
syn match sasOption contained "\<METAENCRYPTALG\s*="
syn match sasOption contained "\<METAENCRYPTLEVEL\s*="
syn match sasOption contained "\<METAID\s*="
syn match sasOption contained "\<METAPASS\s*="
syn match sasOption contained "\<METAPORT\s*="
syn match sasOption contained "\<METAPROFILE\s*="
syn match sasOption contained "\<METAPROTOCOL\s*="
syn match sasOption contained "\<METAREPOSITORY\s*="
syn match sasOption contained "\<METASERVER\s*="
syn match sasOption contained "\<METAUSER\s*="
syn match sasOption contained "\<MINDELIMITER\s*="
syn match sasOption contained "\<MISSING\s*="
syn match sasOption contained "\<MSGLEVEL\s*="
syn match sasOption contained "\<MSYMTABMAX\s*="
syn match sasOption contained "\<MVARSIZE\s*="
syn match sasOption contained "\<NEWS\s*="
syn match sasOption contained "\<OBS\s*="
syn match sasOption contained "\<ORIENTATION\s*="
syn match sasOption contained "\<PAGENO\s*="
syn match sasOption contained "\<PAGESIZE\s*="
syn match sasOption contained "\<PAPERDEST\s*="
syn match sasOption contained "\<PAPERSIZE\s*="
syn match sasOption contained "\<PAPERSOURCE\s*="
syn match sasOption contained "\<PAPERTYPE\s*="
syn match sasOption contained "\<PARM\s*="
syn match sasOption contained "\<PARMCARDS\s*="
syn match sasOption contained "\<PRINTERPATH\s*="
syn match sasOption contained "\<REUSE\s*="
syn match sasOption contained "\<RIGHTMARGIN\s*="
syn match sasOption contained "\<S2\s*="
syn match sasOption contained "\<S\s*="
syn match sasOption contained "\<SASAUTOS\s*="
syn match sasOption contained "\<SASHELP\s*="
syn match sasOption contained "\<SASMSTORE\s*="
syn match sasOption contained "\<SASUSER\s*="
syn match sasOption contained "\<SEQ\s*="
syn match sasOption contained "\<SKIP\s*="
syn match sasOption contained "\<SORTDUP\s*="
syn match sasOption contained "\<SORTSEQ\s*="
syn match sasOption contained "\<SORTSIZE\s*="
syn match sasOption contained "\<SUMSIZE\s*="
syn match sasOption contained "\<SYSPARM\s*="
syn match sasOption contained "\<SYSPRINTFONT\s*="
syn match sasOption contained "\<TERMSTMT\s*="
syn match sasOption contained "\<TEXTURELOC\s*="
syn match sasOption contained "\<TOPMARGIN\s*="
syn match sasOption contained "\<TRAINLOC\s*="
syn match sasOption contained "\<TRANTAB\s*="
syn match sasOption contained "\<USER\s*="
syn match sasOption contained "\<UTILLOC\s*="
syn match sasOption contained "\<UUIDCOUNT\s*="
syn match sasOption contained "\<UUIDGENDHOST\s*="
syn match sasOption contained "\<V6CREATEUPDATE\s*="
syn match sasOption contained "\<VALIDFMTNAME\s*="
syn match sasOption contained "\<VALIDVARNAME\s*="
syn match sasOption contained "\<WORK\s*="
syn match sasOption contained "\<YEARCUTOFF\s*="
syn match sasOption contained "\<_LAST_\s*="

syn keyword sasOptionS contained OPTIONS
syn keyword sasOption contained ASYNCHIO BATCH BYERR BYLINE BYSORTED CAPS CARDIMAGE CENTER CHARCODE CLEANUP
syn keyword sasOption contained CMDMAC COLLATE COLORPRINTING CPUID DATE DETAILS DMR DMS DMSEXP DMSSYNCHK
syn keyword sasOption contained DSNFERR DTRESET DUPLEX ECHOAUTO EMAILHOST EMAILPORT ERRORABEND ERRORBYABEND EXPLORER FMTERR
syn keyword sasOption contained GWINDOW HELPENCMD IMPLMAC INITCMD LABEL MACRO MAUTOLOCDISPLAY MAUTOSOURCE MERGENOBY MERROR
syn keyword sasOption contained MFILE MLOGIC MLOGICNEST MPRINT MPRINTNEST MRECALL MSTORED MULTENVAPPL NOTES NUMBER
syn keyword sasOption contained OBJECTSERVER OVP PAGEBREAKINITIAL PRINTINIT PRINTMSGLIST QUOTELENMAX REPLACE RSASUSER SERROR SETINIT
syn keyword sasOption contained SOLUTIONS SORTEQUALS SOURCE SOURCE2 SPOOL STARTLIB SYMBOLGEN SYNTAXCHECK TERMINAL THREADS
syn keyword sasOption contained TOOLSMENU UNIVERSALPRINT VIEWMENU VNFERR WORKINIT WORKTERM

syn region sasOptionStatement start="\<OPTIONS\>" end=";" transparent keepend extend contains=sasOption,sasOptionS,sasStatement,sasProcName,@sasCommon

" ***************
" Graphic Options
" ***************
syn keyword sasGOptionS contained GOPTIONS
syn keyword sasGOption contained ASYNCHIO BATCH BYERR BYLINE BYSORTED CAPS CARDIMAGE CENTER CHARCODE CLEANUP
syn keyword sasGOption contained ADMGDF ASPECT AUTOCOPY AUTOFEED AUTOSIZE BINDING BORDER CBACK CBY CELL 
syn keyword sasGOption contained CHARACTERS CHARREC CHARTYPE CIRCLEARC CMAP COLLATE COLORS COLORTYPE COLS CPATTERN 
syn keyword sasGOption contained CSYMBOL CTEXT CTITLE DASH DASHLINE DASHSCALE DELAY DESCRIPTION DEVADDR DEVICE 
syn keyword sasGOption contained DEVMAP DEVOPTS DEVTYPE DISPLAY DISPOSAL DRVINIT DRVQRY DRVTERM DUPLEX ERASE 
syn keyword sasGOption contained EXTENSION FASTTEXT FBY FCACHE FILECLOSE FILEONLY FILL FILLINC FONT NAME FONTRES 
syn keyword sasGOption contained FORMAT FTEXT FTITLE FTRACK GACCESS GCLASS GCOPIES GDDMCOPY GDDMNICKNAME GDDMTOKEN 
syn keyword sasGOption contained GDEST GEND GEPILOG GFORMS GOUTMODE GPROLOG GPROTOCOL GRAPHRC GSFLEN GSFMODE 
syn keyword sasGOption contained GSFNAME GSFPROMPT GSIZE GSTART GUNIT GWAIT GWRITER HANDSHAKE HBY HEADER 
syn keyword sasGOption contained HEADERFILE HORIGIN HOSTSPEC HPOS HSIZE HTEXT HTITLE ID INTERACTIVE INTERLACED 
syn keyword sasGOption contained INTERPOL ITERATION KEYMAP LCOLS LFACTOR LROWS MAXCOLORS MAXPOLY MODEL MODULE 
syn keyword sasGOption contained NAK OFFSHADOW PAPERDEST PAPERFEED PAPERLIMIT PAPERSIZE PAPERSOURCE PAPERTYPE PATH PCLIP 
syn keyword sasGOption contained PCOLS PENMOUNTS PENSORT PIEFILL POLYGONCLIP POLYGONFILL POSTGEPILOG POSTGPROLOG POSTGRAPH PPDFILE 
syn keyword sasGOption contained PREGEPILOG PREGPROLOG PREGRAPH PROCESS PROCESSINPUT PROCESSOUTPUT PROMPT PROMPTCHARS PROWS QMSG 
syn keyword sasGOption contained RECTFILL RENDER RENDERLIB REPAINT RESET REVERSE ROTATE ROTATION ROWS SCALABLE 
syn keyword sasGOption contained SIMFONT SPEED SWAP SYMBOL SYMBOLS TARGETDEVICE TRAILER TRAILERFILE TRANSPARENCY TRANTAB 
syn keyword sasGOption contained TYPE UCC VORIGIN VPOS VSIZE V6COMP XMAX XPIXELS YMAX YPIXELS
syn region sasGOptionStatement start="\<GOPTIONS\>" end=";" transparent keepend extend contains=sasGOption,sasGOptionS,sasStatement,sasProcName,@sasCommon

syn match sasAxisS contained "\<AXIS\d*\>"
syn keyword sasAxis contained ANGLE AUTOREF BOTTOM CELLS CENTER CM COLOR E PI EXPAND FONT
syn keyword sasAxis contained HEIGHT IN JUSTIFY LABEL LEFT LENGTH LOGBASE LOGSTYLE MAJOR MIDDLE
syn keyword sasAxis contained MINOR NOBRACKETS NONE NOPLANE OFFSET ORDER ORIGIN PCT POSITION POWER
syn keyword sasAxis contained PT REFLABEL RIGHT ROTATE SPLIT STYLE T TOP VALUE WIDTH
syn region sasAxisStatement start="\<AXIS\d*\>" end=";" transparent keepend extend contains=sasAxisS,sasAxis,sasStatement,sasProcName,@sasCommon

syn match sasLegendS contained "\<LEGEND\d*\>"
syn keyword sasLegend contained ACROSS BAR BOTTOM CBLOCK CBORDER CENTER CFRAME CM COLOR CSHADOW
syn keyword sasLegend contained DOWN FONT FRAME FWIDTH HEIGHT IN INSIDE JUSTIFY LABEL LEFT
syn keyword sasLegend contained LINE MIDDLE MODE NONE OFFSET ORDER ORIGIN OUTSIDE PCT POSITION
syn keyword sasLegend contained PROTECT PT RESERVE RIGHT SHAPE SHARE SYMBOL TOP VALUE
syn region sasLegendStatement start="\<LEGEND\d*\>" end=";" transparent keepend extend contains=sasLegendS,sasLegend,sasStatement,sasProcName,@sasCommon

syn match sasSymbolS contained "\<SYMBOL\d*\>"
syn keyword sasSymbol contained BOX BWIDTH C CELLS CI CLM CM CO COLOR CV
syn keyword sasSymbol contained EXCLUDE FONT HEIGHT HILO IN INCLUDE INTERPOL JOIN L LINE
syn keyword sasSymbol contained MODE NEEDLE NONE P PCT POINTLABEL PT R REPEAT S
syn keyword sasSymbol contained SM SPLINE STD STD1 STD2 STD3 STEP VALUE WIDTH
syn match sasSymbol contained "\<CLI\d\+\>"
syn region sasSymbolStatement start="\<SYMBOL\d*\>" end=";" transparent keepend extend contains=sasSymbolS,sasSymbol,sasStatement,sasProcName,@sasCommon

" ***********
" ODS Options
" ***********
syn keyword sasOdsOption contained CLOSE EXCLUDE GFOOTNOTE GTITLE NOGFOOTNOTE NOGTITLE SELECT SHOW
syn match sasOdsOption contained "\<GPATH\s*="
syn match sasOdsOption contained "\<HEADTEXT\s*="
syn match sasOdsOption contained "\<METATEXT\s*="
syn match sasOdsOption contained "\<NEWFILE\s*="
syn match sasOdsOption contained "\<PATH\s*="
syn match sasOdsOption contained "\<STYLE\s*="
syn match sasOdsOption contained "\<TRANTAB\s*="
syn match sasOdsOption contained "\<ANCHOR\s*="
syn match sasOdsOption contained "\<BASE\s*="

syn match sasOdsOpt contained "\<ODS\s\+CHTML\>"
syn match sasOdsOpt contained "\<ODS\s\+CSVALL\>"
syn match sasOdsOpt contained "\<ODS\s\+DECIMAL_ALIGN\>"
syn match sasOdsOpt contained "\<ODS\s\+DOCBOOK\>"
syn match sasOdsOpt contained "\<ODS\s\+DOCUMENT\>"
syn match sasOdsOpt contained "\<ODS\s\+EXCLUDE\>"
syn match sasOdsOpt contained "\<ODS\s\+HTML3\>"
syn match sasOdsOpt contained "\<ODS\s\+HTMLCSS\>"
syn match sasOdsOpt contained "\<ODS\s\+HTML\>"
syn match sasOdsOpt contained "\<ODS\s\+IMODE\>"
syn match sasOdsOpt contained "\<ODS\s\+LISTING\>"
syn match sasOdsOpt contained "\<ODS\s\+MARKUP\>"
syn match sasOdsOpt contained "\<ODS\s\+OUTPUT\>"
syn match sasOdsOpt contained "\<ODS\s\+PATH\>"
syn match sasOdsOpt contained "\<ODS\s\+PCL\>"
syn match sasOdsOpt contained "\<ODS\s\+PDF\>"
syn match sasOdsOpt contained "\<ODS\s\+PHTML\>"
syn match sasOdsOpt contained "\<ODS\s\+PRINTER\>"
syn match sasOdsOpt contained "\<ODS\s\+PROCLABEL\>"
syn match sasOdsOpt contained "\<ODS\s\+PROCTITLE\>"
syn match sasOdsOpt contained "\<ODS\s\+PS\>"
syn match sasOdsOpt contained "\<ODS\s\+\(NO\)\=RESULTS\>"
syn match sasOdsOpt contained "\<ODS\s\+RTF\>"
syn match sasOdsOpt contained "\<ODS\s\+SELECT\>"
syn match sasOdsOpt contained "\<ODS\s\+SHOW\>"
syn match sasOdsOpt contained "\<ODS\s\+TRACE\>"
syn match sasOdsOpt contained "\<ODS\s\+USEGOPT\>"
syn match sasOdsOpt contained "\<ODS\s\+VERIFY\>"
syn match sasOdsOpt contained "\<ODS\s\+WML\>"
syn match sasOdsOpt contained "\<ODS\s\+\w\+\s\+CLOSE\>"

syn region sasOdsStatement start="\<ODS\s\+\w\+\>" end=";" keepend transparent extend contains=sasOdsOpt,sasOdsOption,@sasCommon

" **********
" Procedures
" **********
"syn keyword sasProc contained PROC

syn match sasProcName contained "\<PROC\s\+\<\(APPEND\|BMDP\|CALENDAR\|CATALOG\|CHART\|CIMPORT\|COMPARE\|CONTENTS\|CONVERT\|COPY\)\>"
syn match sasProcName contained "\<PROC\s\+\<\(CORR\|CPORT\|CV2VIEW\|DATASETS\|DBCSTAB\|DOCUMENT\|EXPORT\|FONTREG\|FORMAT\|FREQ\)\>"
syn match sasProcName contained "\<PROC\s\+\<\(FSLIST\|GPLOT\|IML\|IMPORT\|LIFETEST\|MEANS\|METALIB\|OLAP\|OPTIONS\|OPTLOAD\|OPTSAVE\|PDS\|PDSCOPY\|PHREG\|PLOT\|PMENU\)\>"
syn match sasProcName contained "\<PROC\s\+\<\(PRINT\|PRINTTO\|PRTDEF\|PRTEXP\|PWENCODE\|RANK\|REGISTRY\|RELEASE\|REPORT\|SORT\)\>"
syn match sasProcName contained "\<PROC\s\+\<\(SOURCE\|SQL\|STANDARD\|SUMMARY\|SURVEYSELECT\|TABULATE\|TAPECOPY\|TAPELABEL\|TEMPLATE\)\>"
syn match sasProcName contained "\<PROC\s\+\<\(TIMEPLOT\|TRANSPOSE\|TRANTAB\|UNIVARIATE\)\>"

syn region sasProcedure start=/\(^\|;\)\s*PROC\>/ end=/;/  contained keepend transparent extend contains=sasProcName,@sasCommon

" PROC APPEND:
" Adds observations from one SAS data set to the end of another SAS data set.
syn region sasProcAPPEND start="^\s*\<PROC\s\+APPEND\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC BMDP:
" Invokes a BMDP program to analyze data in a SAS data set.
syn region sasProcBMDP start="^\s*\<PROC\s\+BMDP\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CALENDAR:
" Displays data from a SAS data set in a monthly calendar format. PROC CALENDAR can display 
" holidays in the month, schedule tasks, and process data for multiple calendars with work schedules 
" that vary.
syn region sasProcCALENDAR start="^\s*\<PROC\s\+CALENDAR\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CATALOG:
" Manages entries in SAS catalogs. PROC CATALOG is an interactive, nonwindowing procedure that 
" enables you to display the contents of a catalog, copy an entire catalog or specific entries 
" in a catalog, and rename, exchange, or delete entries in a catalog.
syn region sasProcCATALOG start="^\s*\<PROC\s\+CATALOG\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CHART:
" Produces vertical and horizontal bar charts, block charts, pie charts, and star charts. These 
" charts provide a quick visual representation of the values of a single variable or several variables. 
" PROC CHART can also display a statistic associated with the values.
syn region sasProcCHART start="^\s*\<PROC\s\+CHART\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CIMPORT:
" Restores a transport file created by the CPORT procedure to its original form (a SAS data library, 
" catalog, or data set) in the format appropriate to the operating environment. Coupled with the 
" CPORT procedure, PROC CIMPORT enables you to move SAS data libraries, catalogs, and data sets 
" from one operating environment to another.
syn region sasProcCIMPORT start="^\s*\<PROC\s\+CIMPORT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC COMPARE:
" Compares the contents of two SAS data sets. You can also use PROC COMPARE to compare the values 
" of different variables within a single data set. PROC COMPARE produces a variety of reports 
" on the comparisons that it performs.
syn region sasProcCOMPARE start="^\s*\<PROC\s\+COMPARE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CONTENTS:
" Prints descriptions of the contents of one or more files in a SAS data library.
syn region sasProcCONTENTS start="^\s*\<PROC\s\+CONTENTS\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CONVERT:
" Converts BMDP system files, OSIRIS system files, and SPSS portable files to SAS data sets.
syn region sasProcCONVERT start="^\s*\<PROC\s\+CONVERT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC COPY:
" Copies an entire SAS data library or specific members of the library. You can limit processing 
" to specific types of library members.
syn region sasProcCOPY start="^\s*\<PROC\s\+COPY\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CORR:
" Computes Pearson product-moment and weighted product-moment correlation coefficients between 
" variables and descriptive statistics for these variables. In addition, PROC CORR can compute 
" three nonparametric measures of association (Spearman's rank-order correlation, Kendall's tau-b, 
" and Hoeffding's measure of dependence, D), partial correlations (Pearson's partial correlation, 
" Spearman's partial rank-order correlation, and Kendall's partial tau-b), and Cronbach's coefficient alpha.
syn region sasProcCORR start="^\s*\<PROC\s\+CORR\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CPORT:
" Writes SAS data libraries, data sets, and catalogs in a special format called a transport file. 
" Coupled with the CIMPORT procedure, PROC CPORT enables you to move SAS libraries, data sets, 
" and catalogs from one operating environment to another.
syn region sasProcCPORT start="^\s*\<PROC\s\+CPORT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC CV2VIEW:
" Converts SAS/ACCESS view descriptors to PROC SQL views. Starting in SAS System 9, conversion 
" of SAS/ACCESS view descriptors to PROC SQL views is recommended because PROC SQL views are platform 
" independent and enable you to use the LIBNAME statement.
syn region sasProcCV2VIEW start="^\s*\<PROC\s\+CV2VIEW\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC DATASETS:
" Lists, copies, renames, and deletes SAS files and SAS generation groups, manages indexes, and 
" appends SAS data sets in a SAS data library. The procedure provides all the capabilities of 
" the APPEND, CONTENTS, and COPY procedures. You can also modify variables within data sets, manage 
" data set attributes, such as labels and passwords, or create and delete integrity constraints.
syn region sasProcDATASETS start="^\s*\<PROC\s\+DATASETS\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC DBCSTAB:
" Produces conversion tables for the double-byte character sets that SAS supports.
syn region sasProcDBCSTAB start="^\s*\<PROC\s\+DBCSTAB\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC DOCUMENT:
" Manipulates procedure output that is stored in ODS documents. PROC DOCUMENT enables a user to 
" browse and edit output objects and hierarchies, and to replay them to any supported ODS output format.
syn region sasProcDOCUMENT start="^\s*\<PROC\s\+DOCUMENT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC EXPORT:
" Reads data from a SAS data set and writes it to an external data source.
syn region sasProcEXPORT start="^\s*\<PROC\s\+EXPORT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC FONTREG:
" Adds system fonts to the SAS registry.
syn region sasProcFONTREG start="^\s*\<PROC\s\+FONTREG\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC FORMAT:
" Creates user-defined informats and formats for character or numeric variables. PROC FORMAT also 
" prints the contents of a format library, creates a control data set to write other informats 
" or formats, and reads a control data set to create informats or formats.
syn region sasProcFORMAT start="^\s*\<PROC\s\+FORMAT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC FREQ:
" Produces one-way to n-way frequency tables and reports frequency counts. PROC FREQ can compute 
" chi-square tests for one-way to n-way tables, tests and measures of association and of agreement 
" for two-way to n-way cross-tabulation tables, risks and risk difference for 2×2 tables, trends 
" tests, and Cochran-Mantel-Haenszel statistics. You can also create output data sets.
syn region sasProcFREQ start="^\s*\<PROC\s\+FREQ\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC FSLIST:
" Displays the contents of an external file or copies text from an external file to the SAS Text Editor.
syn region sasProcFSLIST start="^\s*\<PROC\s\+FSLIST\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC GPLOT:
" The GPLOT procedure plots the values of two or more variables on a set of coordinate axes (X and Y).
" The coordinates of each point on the plot correspond to two variable values in an observation of the input data set.
" The procedure can also generate a separate plot for each value of a third (classification) variable.
" It can also generate bubble plots in which circles of varying proportions representing the values of a third variable are drawn at the data points.
syn keyword sasProcGplotOption contained UNIFORM
syn match sasProcGplotOption contained "\<DATA\s*="
syn match sasProcGplotOption contained "\<ANNOTATE\s*="
syn match sasProcGplotOption contained "\<GOUT\s*="
syn match sasProcGplotOption contained "\<IMAGEMAP\s*="

syn keyword sasProcGplot contained BUBBLE[2]
syn keyword sasProcGplot contained PLOT[2] AUTOHREF AUTOVREF BY FRAME NOFRAME GRID HZERO LEGEND NOAXIS NOAXES NOLEGEND OVERLAY REGEQN SKIPMISS VREVERSE VZERO WHERE
syn match sasProcGplot contained "\<ANNOTATE\s*="
syn match sasProcGplot contained "\<AREAS\s*="
syn match sasProcGplot contained "\<CAXIS\s*="
syn match sasProcGplot contained "\<CFRAME\s*="
syn match sasProcGplot contained "\<CHREF\s*="
syn match sasProcGplot contained "\<CTEXT\s*="
syn match sasProcGplot contained "\<CVREF\s*="
syn match sasProcGplot contained "\<DESCRIPTION\s*="
syn match sasProcGplot contained "\<HAXIS\s*="
syn match sasProcGplot contained "\<AXIS\d\+\>"
syn match sasProcGplot contained "\<HMINOR\s*="
syn match sasProcGplot contained "\<HREF\s*="
syn match sasProcGplot contained "\<HTML\s*="
syn match sasProcGplot contained "\<HTML_LEGEND\s*="
syn match sasProcGplot contained "\<LHREF\s*="
syn match sasProcGplot contained "\<LVREF\s*="
syn match sasProcGplot contained "\<NAME\s*="
syn match sasProcGplot contained "\<VAXIS\s*="
syn match sasProcGplot contained "\<VMINOR\s*="
syn match sasProcGplot contained "\<VREF\s*="

syn region sasProcGplotOpt start="\<PROC\s\+GPLOT\>" end=";" transparent extend contains=sasProcName,sasProcGplotOption,sasDataOption,@sasCommon
syn region sasProcGplotStatement start="\<PROC\s\+GPLOT\>" end="\<\(QUIT\|RUN\)\s*;" fold keepend transparent extend contains=sasProcGplotOpt,sasProcGplot,@sasCommon


" PROC IML:
" SAS/IML software gives you access to a powerful and flexible programming language (Interactive 
" Matrix Language) in a dynamic, interactive environment. The fundamental object of the language 
" is a data matrix.
syn keyword sasCallIML contained APPCORT ARMACOV ARMALIK CHANGE COMPORT DELETE EIGEN EXECUTE FARMACOV FARMAFIT
syn keyword sasCallIML contained FARMALIK FARMASIM FDIF GAEND GAGETMEM GAGETVAL GAINIT GAREEVAL GAREGEN GASETCRO
syn keyword sasCallIML contained GASETMUT GASETOBJ GASETSEL GBLKVP GBLKVPD GCLOSE GDELETE GDRAW GDRAWL GENEIG
syn keyword sasCallIML contained GGRID GINCLUDE GOPEN GPIE GPIEXY GPOINT GPOLY GPORT GPORTPOP
syn keyword sasCallIML contained GPORTSTK GTEXT GVTEXT GXAXIS GYAXIS
syn keyword sasCallIML contained GSCALE GSCRIPT GSET GSHOW GSORTH GSTART GSTOP GSTRLEN GWINDOW IPF
syn keyword sasCallIML contained ITBICG ITCGRAD ITMINRES KALCVF KALCVS KALDFF KALDFS LAV LCP LMS
syn keyword sasCallIML contained LP LTS LUPDT MARG MAXQFORM MCD MVE NLPCG NLPDD NLPFDD
syn keyword sasCallIML contained NLPFEA NLPHQN NLPLM NLPNMS NLPNRA NLPNRR NLPQN NLPQUA NLPTR ODE
syn keyword sasCallIML contained ORTVEC PGRAF PUSH QR QUAD QUEUE RANDGEN RANDSEED RENAME RUPDT
syn keyword sasCallIML contained RDODT RUPDT SPLINEC SEQ SEQSCALE SEQSHIFT
syn keyword sasCallIML contained RZLIND SEQSCALE SEQSHIFT SOLVELIN SORT SORTNDX SOUND SVD TEIGEN TPSPLINE
syn keyword sasCallIML contained TPSPLNEV TSBAYSEA TSDECOMP TSMLOCAR TSMLOMAR TSMULMAR TSPEARS TSPRED TSROOT TSTVCAR
syn keyword sasCallIML contained TSUNIMAR VALSET VARMACOV VARMALIK VARMASIM VNORMAL
syn keyword sasCallIML contained VTSROOT WAVFT WAVGET WAVIFT WAVPRINT WAVTHRSH

syn keyword sasFunctionIML contained ABS ALL ANY APPLY ARMASIM BLOCK BRANKS BTRAN BYTE CHAR
syn keyword sasFunctionIML contained CHOOSE CONCAT CONTENTS CONVEXIT COVLAG CSHAPE CUSUM CVEXHULL DATASETS DESIGN
syn keyword sasFunctionIML contained DESIGNF DET DIAG DURATION ECHELON EIGVAL EIGVEC EXP FFT
syn keyword sasFunctionIML contained FORWARD GASETUP GINV HALF HANKEL HDIR HERMITE HOMOGEN I IFFT
syn keyword sasFunctionIML contained INSERT INT INV INVUPDT J JROOT LENGTH LOC LOG MAD
syn keyword sasFunctionIML contained MAX MIN MOD NAME NCOL NLENG NORMAL NROW NUM OPSCAL
syn keyword sasFunctionIML contained ORPOL POLYROOT PRODUCT PV RANK RANKTIE RATES RATIO REMOVE REPEAT
syn keyword sasFunctionIML contained ROOT ROWCAT ROWCATC SETDIF SHAPE SOLVE SPLINEV SPOT SQRSYM SQRT
syn keyword sasFunctionIML contained SSQ STORAGE SUBSTR SUM SWEEP SYMSQR T TEIGVAL TEIGVEC TOEPLITZ
syn keyword sasFunctionIML contained TRACE TRISOLV TYPE UNIFORM UNION UNIQUE UNIQUEBY VALUE VECDIAG XMULT XSECT YIELD

syn keyword sasStatementIML contained ABORT APPEND CALL CLOSE CLOSEFILE CREATE DELETE DISPLAY DO EDIT ELSE END
syn keyword sasStatementIML contained FILE FIND FINISH FORCE FREE GOTO IF INDEX INFILE INPUT
syn keyword sasStatementIML contained LINK LIST LOAD MATTRIB PAUSE PRINT PURGE PUT READ
syn keyword sasStatementIML contained REMOVE REPLACE RESET RESUME RETURN RUN SAVE SETIN SETOUT
syn keyword sasStatementIML contained SHOW SORT SPLINE STOP STORE SUMMARY THEN TO UNTIL USE WHILE WINDOW
syn match sasStatementIML contained "\<START\>"
syn match sasStatementIML contained "DO DATA"

syn region sasProcIMLmodule start="\<start\s\+\w\+\>" end="\<finish\s\+\w\+\s*;" keepend extend contained fold transparent contains=sasStatementIML,sasCallIML,sasFunctionIML,@sasCommon,sasDOfold1,sasDOfold2
syn region sasProcIML start="^\s*\<PROC\s\+IML\>" end="\<QUIT\s*;" fold transparent keepend extend contains=sasProcIMLmodule,sasCallIML,sasFunctionIML,sasStatementIML,sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC IMPORT:
" Reads data from an external data source and writes them to a SAS data set.
syn region sasProcIMPORT start="^\s*\<PROC\s\+IMPORT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC LIFETEST:
" The LIFETEST procedure can be used with data that may be right-censored to compute nonparametric estimates of the survival distribution
" and to compute rank tests for association of the response variable with other variables. The survival estimates are computed within
" defined strata levels, and the rank tests are pooled over the strata and are therefore adjusted for strata differences. 
" A common feature of lifetime or survival data is the presence of right-censored observations due either to withdrawal of experimental units
" or to termination of the experiment. For such observations, you know only that the lifetime exceeded a given value;
" the exact lifetime remains unknown. Such data cannot be analyzed by ignoring the censored observations because, among other considerations,
" the longer-lived units are generally more likely to be censored. The analysis methodology must correctly use the censored observations
" as well as the noncensored observations. Usually, a first step in the analysis of survival data is the estimation of the distribution of
" the survival times. Survival times are often called failure times, and event times are uncensored survival times. The survival distribution
" function (SDF), also known as the survivor function, is used to describe the lifetimes of the population of interest. The SDF evaluated at t
" is the probability that an experimental unit from the population will have a lifetime exceeding t, that is 
"
" S(t) = Pr(T > t) 
" where S(t) denotes the survivor function and T is the lifetime of a randomly selected experimental unit.
" The LIFETEST procedure can be used to compute nonparametric estimates of the survivor function either by the product-limit method
" (also called the Kaplan-Meier method) or by the life table method.

syn keyword sasProcLifetestOption contained LINEPRINTER MISSING NOCENSPLOT NOPRINT NOTABLE REDUCEOUT            
syn match sasProcLifetestOption contained "ALPHA\s*="
syn match sasProcLifetestOption contained "ALPHAQT\s*="
syn match sasProcLifetestOption contained "ANNOTATE\s*="
syn match sasProcLifetestOption contained "CENSOREDSYMBOL\s*="
syn match sasProcLifetestOption contained "DATA\s*="
syn match sasProcLifetestOption contained "DESCRIPTION\s*="
syn match sasProcLifetestOption contained "EVENTSYMBOL\s*="
syn match sasProcLifetestOption contained "FORMCHAR\((1,2,7,9)\)\=\s*="
syn match sasProcLifetestOption contained "GOUT\s*="
syn match sasProcLifetestOption contained "INTERVALS\s*="
syn match sasProcLifetestOption contained "LANNOTATE\s*="
syn match sasProcLifetestOption contained "MAXTIME\s*="
syn match sasProcLifetestOption contained "METHOD\s*="
syn match sasProcLifetestOption contained "NINTERVAL\s*="
syn match sasProcLifetestOption contained "OUTSURV\s*="
syn match sasProcLifetestOption contained "OUTTEST\s*="
syn match sasProcLifetestOption contained "PLOTS\s*="
syn match sasProcLifetestOption contained "SINGULAR\s*="
syn match sasProcLifetestOption contained "TIMELIM\s*="
syn match sasProcLifetestOption contained "TIMELIST\s*="
syn match sasProcLifetestOption contained "WIDTH\s*="
syn match sasProcLifetestOption contained "\s*="

syn keyword sasProcLifetest contained TIME BY FREQ ID STRATA TEST

syn region sasProcLifetestOpt start="\<PROC\s\+LIFETEST\>" end=";" transparent extend contains=sasProcName,sasProcLifetestOption,@sasCommon
syn region sasProcLifetestStatement start="^\s*\<PROC\s\+LIFETEST\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcLifetestOpt,sasProcLifetest,sasFunction,sasDataOpt,@sasCommon

" PROC MEANS:
" Computes descriptive statistics for numeric variables across all observations and within groups 
" of observations. You can also create an output data set that contains specific statistics and 
" identifies minimum and maximum values for groups of observations.
syn region sasProcMEANS start="^\s*\<PROC\s\+MEANS\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC METALIB:
" The METALIB procedure supports SAS data sets (data files and data views), DBMS data, and SAS Information Maps. The data source or information map
" is referred to as a table in this documentation and the procedure's output.
" When you run PROC METALIB, you specify a SAS library that is already defined in the SAS Metadata Server. A SAS library is defined in the metadata by a SASLibrary object.
" A table in a SAS library is defined in the metadata by several objects that are collectively called a table definition. All of the table definitions
" that are associated with a SASLibrary object are tracked in an association list.
" The METALIB procedure updates the metadata in the metadata server to match the tables in a library. By default, the procedure performs the following tasks:
"  · creates metadata for any table that does not have metadata
"  · updates metadata about all of the tables' columns, indexes, unique keys, foreign keys, and key associations
" With optional statements, PROC METALIB can perform the following additional tasks:
"  · select or exclude specific tables from processing
"  · specify where new metadata is stored in SAS folders
"  · limit the update of table definitions that would affect Job or Transformation objects
"  · add a prefix to the name of all new metadata objects
"  · generate a report of changes that the procedure made to metadata
"  · generate a report of needed metadata changes without making the changes
"  · in the generated report, include an impact analysis for Job and Transformation objects
"  · in the generated report, include a list of tables that match the metadata
"  · suppress the metadata add action, the metadata update action, or both
"  · delete metadata that is obsolete or duplicated
syn keyword sasProcMETALIBOption contained VERBOSE
syn match sasProcMETALIBOption contained "IN\s*="
syn match sasProcMETALIBOption contained "OUT\s*="
syn match sasProcMETALIBOption contained "HEADER\s*=\s*\(NONE\|SIMPLE\|FULL\)"
syn match sasProcMETALIBOption contained "PORT\s*=\s*\d\="
syn match sasProcMETALIBOption contained "METAPORT\s*=\s*\d\="
syn match sasProcMETALIBOption contained "PROTOCOL\s*="
syn match sasProcMETALIBOption contained "METAPROTOCOL\s*="
syn match sasProcMETALIBOption contained "REPOSITORY\s*="
syn match sasProcMETALIBOption contained "REPOS\s*="
syn match sasProcMETALIBOption contained "SERVER\s*="
syn match sasProcMETALIBOption contained "HOST\s*="
syn match sasProcMETALIBOption contained "IPADDR\s*="
syn match sasProcMETALIBOption contained "METASERVER\s*="
syn match sasProcMETALIBOption contained "USER\s*="
syn match sasProcMETALIBOption contained "ID\s*="
syn match sasProcMETALIBOption contained "METAUSER\s*="
syn match sasProcMETALIBOption contained "USERID\s*="

syn keyword sasProcMETALIB contained OMR EXCLUDE SELECT FOLDER NOEXEC PREFIX REPORT
syn keyword sasProcMETALIB contained LIBID LIBRARY LIBURI REPID
syn match sasProcMETALIB contained "USER\s*="
syn match sasProcMETALIB contained "ID\s*="
syn match sasProcMETALIB contained "METAUSER\s*="
syn match sasProcMETALIB contained "USERID\s*="
syn match sasProcMETALIB contained "HOST\s*="
syn match sasProcMETALIB contained "IPADDR\s*="
syn match sasProcMETALIB contained "METAPASS\s*="
syn match sasProcMETALIB contained "METAPORT\s*="
syn match sasProcMETALIB contained "METAPROTOCOL\s*="
syn match sasProcMETALIB contained "METAREPOSITORY\s*="
syn match sasProcMETALIB contained "METASERVER\s*="
syn match sasProcMETALIB contained "PASSWORD\s*="
syn match sasProcMETALIB contained "PORT\s*="
syn match sasProcMETALIB contained "PROTOCOL\s*="
syn match sasProcMETALIB contained "PW\s*="
syn match sasProcMETALIB contained "REPID\s*="
syn match sasProcMETALIB contained "REPID\s*="
syn match sasProcMETALIB contained "REPNAME\s*="
syn match sasProcMETALIB contained "SERVER\s*="
syn match sasProcMETALIB contained "UPDATE_RULE\s*=\s*(\s*\(DELETE\|NOADD\|NODELDUP\|NOUPDATE\)\s*)"
syn match sasProcMETALIB contained "IMPACT_LIMIT\s*=\s*\d\="

syn region sasProcMETALIBOpt start="\<PROC\s\+METALIB\>" end=";" transparent extend contains=sasProcName,sasProcMETALIBOption,@sasCommon
syn region sasProcMETALIBStatement start="^\s*\<PROC\s\+METALIB\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcMETALIBOpt,sasProcMETALIB,sasFunction,sasDataOpt,@sasCommon

" PROC OPTIONS:
" Lists the current values of all SAS system options.
"syn region sasProcOPTIONS start="^\s*\<PROC\s\+OPTIONS\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasOption,sasOptionStatement,@sasCommon,sasDOfold1,sasDOfold2

" PROC OLAP:
" The OLAP procedure is one of the SAS tools that you can use to create, update, and delete cubes. This includes adding and deleting cube aggregations.
" Note. You can also use the Cube Designer wizard to maintain OLAP cubes. The Cube Designer wizard can be launched from SAS Data Integration Studio and SAS OLAP Cube Studio.
"       Help on using the wizard to build cubes is available from within both applications.
" In addition to the basic cube creation tasks, PROC OLAP also enables you to do the following:
" · build cubes with ragged hierarchies
" · rename cubes
" · change nonstructural cube elements
" · control resummarization behavior on cube security
" · control options that can be used to optimize cube creation and query performance
" · specify data set options on detail, fact, dimension, and drill-through tables
" · create TIME dimensions
" · design dimensions that have more than one hierarchy
" · define global calculated members and named sets
" · include SAS code when you submit PROC OLAP in batch mode
" · read alternate locale data sets and create locale-specific metadata for use at query time
syn keyword sasProcOLAPOption contained ADD_DATA COALESCE_AGGREGATIONS COMPACT_NWAY COMPRESS DELETE_PHYSICAL DELETE NOCOMPRESS DRILLTHROUGH_TABLE DT_TABLE ESRI_REPLACE INDEX NOINDEX
syn keyword sasProcOLAPOption contained NO_NWAY NONUPDATEABLE REGISTER_ONLY RENAME REORGANIZE_LEVELS REORG_LEVELS SYNCHRONIZE_COLUMNS UPDATE_DISPLAY_NAMES UPDATE_IN_PLACE UPDATE_INPLACE
syn match sasProcOLAPOption contained "ASYNCINDEXLIMIT\s*=\s*\d\="
syn match sasProcOLAPOption contained "CONCURRENT\s*=\s*\d\="
syn match sasProcOLAPOption contained "CUBE\s*="
syn match sasProcOLAPOption contained "DATA\s*="
syn match sasProcOLAPOption contained "DESCRIPTION\s*="
syn match sasProcOLAPOption contained "DATAPATH\s*="
syn match sasProcOLAPOption contained "DT_TBL\s*="
syn match sasProcOLAPOption contained "EMPTY_CHAR\s*="
syn match sasProcOLAPOption contained "EMPTY_NUM\s*="
syn match sasProcOLAPOption contained "ESRI_MAP_SERVER\s*="
syn match sasProcOLAPOption contained "FACT\s*="
syn match sasProcOLAPOption contained "IGNORE_MISSING_DIMKEYS\s*=\s*\(TERSE\|VERBOSE\)"
syn match sasProcOLAPOption contained "INDEXSORTSIZE\s*=\s*\d\="
syn match sasProcOLAPOption contained "MAX_RETRY_WAIT\s*=\s*\d\="
syn match sasProcOLAPOption contained "MAXTHREADS\s*=\s*\d\="
syn match sasProcOLAPOption contained "MIN_RETRY_WAIT\s*=\s*\d\="
syn match sasProcOLAPOption contained "OUTCUBE\s*="
syn match sasProcOLAPOption contained "OUTSCHEMA\s*="
syn match sasProcOLAPOption contained "PARTSIZE\s*=\s*\d\="
syn match sasProcOLAPOption contained "PATH\s*="
syn match sasProcOLAPOption contained "SECURITY_SUBSET\s*=\s*\(YES\|NO\)"
syn match sasProcOLAPOption contained "SEGSIZE\s*=\s*\d\="
syn match sasProcOLAPOption contained "WORKPATH\s*="

syn keyword sasProcOLAP contained METASVR HOST PORT PW REPOSITORY USERID DIMENSION LEVEL LEVELS PROPERTY HIERARCHY MEASURE
syn keyword sasProcOLAP contained HIERARCHIES AGGR_COLUMN ALL_MEMBER ANALYSIS CAPTION COLUMN DEFAULT DESC DESCRIPTION DIMKEY DIMTBL DIMTABLELIBREF DIMTABLEMEMPREF
syn keyword sasProcOLAP contained EMPTY EMPTY_CHAR EMPTY_NUM FACTKEY ESRI_MAP_LAYER FORMAT IGNORE_EMPTY INCLUDE_CALCULATED_MEMBER INCLUDE_CALC MAP_SERVICE
syn keyword sasProcOLAP contained NAME NONUPDATEABLE NOINCLUDE_CALCULATED_MEMBER NOINCLUDE_CALC OLAP_SCHEMA PARTSIZE SAS_SPATIAL_ID SORT_ORDER STAT TYPE
syn keyword sasProcOLAP contained UPDATE_DIMENSION UNITS MEMBERS MEMBERS_AND_PROPERTIES OFF SEGSIZE TABLE
syn keyword sasProcOLAP contained AGGREGATION DROP_AGGREGATION DEFINE MEMBER SET AS UNDEFINE USER_DEFINED_TRANSLATIONS REORGANIZE_LEVEL REORG_LEVEL

syn region sasProcOLAPOpt start="\<PROC\s\+OLAP\>" end=";" transparent extend contains=sasProcName,sasProcOLAPOption,@sasCommon
syn region sasProcOLAPStatement start="^\s*\<PROC\s\+OLAP\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcOLAPOpt,sasProcOLAP,sasFunction,sasDataOpt,@sasCommon

" PROC OPTLOAD:
" Reads SAS system option settings from the SAS registry or a SAS data set, and puts them into effect.
syn region sasProcOPTLOAD start="^\s*\<PROC\s\+OPTLOAD\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC OPTSAVE:
" Saves SAS system option settings to the SAS registry or a SAS data set.
syn region sasProcOPTSAVE start="^\s*\<PROC\s\+OPTSAVE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC PDS:
" Lists, deletes, and renames the members of a partitioned data set.
syn region sasProcPDS start="^\s*\<PROC\s\+PDS\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC PDSCOPY:
" Copies partitioned data sets from disk to tape, disk to disk, tape to tape, or tape to disk.
syn region sasProcPDSCOPY start="^\s*\<PROC\s\+PDSCOPY\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC PHREG:
" The analysis of survival data requires special techniques because the data are almost always incomplete,
" and familiar parametric assumptions may be unjustifiable. Investigators follow subjects until they reach a prespecified endpoint
" (for example, death). However, subjects sometimes withdraw from a study, or the study is completed before the endpoint is reached.
" In these cases, the survival times (also known as failure times) are censored; subjects survived to a certain time
" beyond which their status is unknown. The noncensored survival times are referred to as event times. Methods for survival analysis
" must account for both censored and noncensored data. 
"
" There are many types of models that have been used for survival data. Two of the more popular types of models are the
" accelerated failure time model and the Cox proportional hazards model. Each has its own assumptions on the underlying distribution of the survival times.
" Two closely related functions often used to describe the distribution of survival times are the survivor function and the hazard function. 
"
" The accelerated failure time model assumes a parametric form for the effects of the explanatory variables and usually assumes
" a parametric form for the underlying survivor function. Cox's proportional hazards model also assumes a parametric form for the effects
" of the explanatory variables, but it allows an unspecified form for the underlying survivor function. 
"
" The PHREG procedure performs regression analysis of survival data based on the Cox proportional hazards model. Cox's semiparametric model
" is widely used in the analysis of survival data to explain the effect of explanatory variables on survival times.
"
" The PHREG procedure also enables you to 
"	· include an offset variable in the model 
"	· test linear hypotheses about the regression parameters 
"	· perform conditional logistic regression analysis for matched case-control studies 
"	· create a SAS data set containing survivor function estimates, residuals, and regression diagnostics 
"	· create a SAS data set containing survival distribution estimates and confidence interval for the survivor function at each
"	  event time for a given realization of the explanatory variables 

syn keyword sasProcPhregOption contained COVOUT MULTIPASS NOPRINT NOSUMMARY SIMPLE       
syn match sasProcPhregOption contained "DATA\s*="
syn match sasProcPhregOption contained "OUTEST\s*="
syn match sasProcPhregOption contained "COVSANDWICH\s*\((AGGREGATE)\)\="

syn keyword sasProcPhreg contained MODEL BY FREQ ID STRATA TEST OUTPUT BASELINE

syn region sasProcPhregOpt start="\<PROC\s\+Phreg\>" end=";" transparent extend contains=sasProcName,sasProcPhregOption,@sasCommon
syn region sasProcPhregStatement start="^\s*\<PROC\s\+Phreg\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcPhregOpt,sasProcPhreg,sasFunction,sasDataOpt,@sasCommon

" PROC PLOT:
" Produces scatter plots that graph one variable against another. The coordinates of each point 
" on the plot correspond to the two variables' values in one or more observations of the input data set.
syn region sasProcPLOT start="^\s*\<PROC\s\+PLOT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC PMENU:
" Defines menus that you can use in DATA step windows, macro windows, and SAS/AF windows, or in 
" any SAS application that enables you to specify customized menus.
syn region sasProcPMENU start="^\s*\<PROC\s\+PMENU\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC PRINT:
" Prints the observations in a SAS data set, using all or some of the variables. PROC PRINT can 
" also print totals and subtotals for numeric variables.
syn region sasProcPRINT start="^\s*\<PROC\s\+PRINT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2


" PROC PRINTTO:
" Defines destinations for SAS procedure output and the SAS log.
syn region sasProcPRINTTO start="^\s*\<PROC\s\+PRINTTO\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC PRTDEF:
" Creates printer definitions for individual SAS users or all SAS users.
syn region sasProcPRTDEF start="^\s*\<PROC\s\+PRTDEF\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC PRTEXP:
" Exports printer definition attributes to a SAS data set so that they can be easily replicated and modified.
syn region sasProcPRTEXP start="^\s*\<PROC\s\+PRTEXP\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC PWENCODE:
" Encodes passwords for use in SAS programs.
syn region sasProcPWENCODE start="^\s*\<PROC\s\+PWENCODE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC RANK:
" Computes ranks for one or more numeric variables across the observations of a SAS data set. 
" The ranks are written to a new SAS data set. Alternatively, PROC RANK produces normal scores or other rank scores.
syn region sasProcRANK start="^\s*\<PROC\s\+RANK\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC REGISTRY:
" Imports registry information into the USER portion of the SAS registry.
syn region sasProcREGISTRY start="^\s*\<PROC\s\+REGISTRY\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC RELEASE:
" Releases unused space at the end of a disk data set in the z/OS environment.
syn region sasProcRELEASE start="^\s*\<PROC\s\+RELEASE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC REPORT:
" Combines features of the PRINT, MEANS, and TABULATE procedures with features of the DATA step 
" in a single report-writing tool that can produce both detail and summary reports.
syn keyword sasProcReportOption contained HEADSKIP HEADLINE NOWD SPLIT MISSING ORDER DATA NOWINDOWS
syn keyword sasProcReport contained COLUMN[S] DEFINE DISPLAY FLOW GROUP BREAK AFTER SPACING WIDTH CENTER LEFT RIGHT INTERNAL

syn region sasProcReportOpt start="\<PROC\s\+REPORT\>" end=";" transparent extend contains=sasProcName,sasProcReportOption,@sasCommon
syn region sasProcReportStatement start="^\s*\<PROC\s\+REPORT\>" end="\<\(QUIT\|RUN\)\s*;" fold keepend transparent extend contains=sasProcReportOpt,sasProcReport,@sasCommon

" PROC SORT:
" Sorts observations in a SAS data set by one or more variables. PROC SORT stores the resulting 
" sorted observations in a new SAS data set or replaces the original data set.
syn keyword sasProcSortOption contained ASCII DANISH DATECOPY EBCDIC EQUALS FINNISH FORCE
syn keyword sasProcSortOption contained NATIONAL NODUPKEY NODUPRECS NOEQUALS NORWEGIAN NOTHREADS
syn keyword sasProcSortOption contained OVERWRITE REVERSE SWEDISH TAGSORT THREADS
syn match sasProcSortOption contained "\<DATA\s*="
syn match sasProcSortOption contained "\<DUPOUT\s*="
syn match sasProcSortOption contained "\<OUT\s*="
syn match sasProcSortOption contained "\<SORTSEQ\s*="
syn match sasProcSortOption contained "\<SORTSIZE\s*="
syn keyword sasProcSort contained BY DESCENDING

syn region sasProcSortOpt start="\<PROC\s\+SORT\>" end=";" transparent extend contains=sasProcName,sasProcSortOption,sasDataOption,@sasCommon
syn region sasProcSortStatement start="\<PROC\s\+SORT\>" end="\<\(QUIT\|RUN\)\s*;" keepend transparent extend contains=sasProcSortOpt,sasProcSort,@sasCommon

" PROC SOURCE:
" Provides an easy way to back up and process source library data sets.
syn region sasProcSOURCE start="^\s*\<PROC\s\+SOURCE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC SQL:
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

syn region sasProcSqlOpt start="\<PROC\s\+SQL\>" end=";" transparent extend contains=sasProcName,sasProcSqlOption,@sasCommon
syn region sasProcSqlStatement start="\<PROC\s\+SQL\>" end="\<QUIT\s*;" fold keepend transparent extend contains=sasProcSqlOpt,sasProcSql,sasFunction,sasDataOpt,@sasCommon

" PROC STANDARD:
" Standardizes some or all of the variables in a SAS data set to a given mean and standard deviation 
" and produces a new SAS data set that contains the standardized values.
syn region sasProcSTANDARD start="^\s*\<PROC\s\+STANDARD\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC SUMMARY:
" Computes descriptive statistics for the variables in a SAS data across all observations and 
" within groups of observations and outputs the results to a new SAS data set.
syn region sasProcSUMMARY start="^\s*\<PROC\s\+SUMMARY\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC SURVEYSELECT:
" The SURVEYSELECT procedure provides a variety of methods for selecting probability-based random samples.
" The procedure can select a simple random sample or can sample according to a complex multistage sample design that includes stratification,
" clustering, and unequal probabilities of selection. With probability sampling, each unit in the survey population has a known,
" positive probability of selection. This property of probability sampling avoids selection bias and enables you to use
" statistical theory to make valid inferences from the sample to the survey population.
syn region sasProcSUMMARY start="^\s*\<PROC\s\+SURVEYSELECT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC TABULATE:
" Displays descriptive statistics in tabular form. The value in each table cell is calculated 
" from the variables and statistics that define the pages, rows, and columns of the table. The 
" statistic associated with each cell is calculated on values from all observations in that category. 
" You can write the results to a SAS data set.
syn keyword sasProcTabulateOption contained TRAP NOTRAP EXCLUSIVE MISSING EXCLNPWGTS NOSEPS
syn match sasProcTabulateOption contained "\<DATAs*="
syn match sasProcTabulateOption contained "\<OUTs*="
syn match sasProcTabulateOption contained "\<CLASSDATAs*="
syn match sasProcTabulateOption contained "\<QMARKERSs*="
syn match sasProcTabulateOption contained "\<QMETHODs*="
syn match sasProcTabulateOption contained "\<QNTLDEFs*="
syn match sasProcTabulateOption contained "\<VARDEFs*="
syn match sasProcTabulateOption contained "\<CONTENTSs*="
syn match sasProcTabulateOption contained "\<FORMATs*="
syn match sasProcTabulateOption contained "\<FORMCHARs*="
syn match sasProcTabulateOption contained "\<ORDERs*="
syn match sasProcTabulateOption contained "\<STYLEs*="

syn keyword sasProcTabulate contained BY CLASS CLASSLEV FREQ KEYLABEL KEYWORD TABLE VAR WEIGHT
syn keyword sasProcTabulate contained DESCENDING NOTSORTED ASCEND[ING] DESCEND[ING] EXCLUSIVE GROUPINTERNAL MISSING MLF ORDER DATA FORMATTED UNFORMATTED PRELOADFMT
syn keyword sasProcTabulate contained S[TYLE] PARENT BOX _PAGE_ CONDENSE CONTENTS FUZZ INDENT MISSTEXT NOCONTINUED PRINTMISS ROW CONSTANT FLOAT RTS[PACE]
syn keyword sasProcTabulate contained ALL COLPCTN PCTSUM COLPCTSUM RANGE CSS REPPCTN CV REPPCTSUM MAX ROWPCTN MEAN ROWPCTSUM MIN STDDEV STD N STDERR NMISS
syn keyword sasProcTabulate contained SUM PAGEPCTN SUMWGT PAGEPCTSUM USS PCTN VAR MEDIAN P50 Q3 P75 P1 P90 P5 P95 P10 P99 Q1 P25 QRANGE PROBT T 



syn region sasProcTabulateOpt start="\<PROC\s\+TABULATE\>" end=";" transparent extend contains=sasProcName,sasProcTabulateOption,@sasCommon
syn region sasProcTabulateStatement start="^\s*\<PROC\s\+TABULATE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcTabulateOpt,sasProcTabulate,@sasCommon

" PROC TAPECOPY:
" Copies an entire tape volume or files from one or more tape volumes to one output tape volume.
syn region sasProcTAPECOPY start="^\s*\<PROC\s\+TAPECOPY\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1

" PROC TAPELABEL:
" Lists the label information of an IBM standard-labeled tape volume under the z/OS environment.
syn region sasProcTAPELABEL start="^\s*\<PROC\s\+TAPELABEL\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC TEMPLATE:
" Customizes ODS output for an entire SAS job or a single ODS output object.
syn region sasProcTEMPLATE start="^\s*\<PROC\s\+TEMPLATE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1

" PROC TIMEPLOT:
" Produces plots of one or more variables over time intervals.
syn region sasProcTIMEPLOT start="^\s*\<PROC\s\+TIMEPLOT\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC TRANSPOSE:
" Transposes a data set that changes observations into variables and vice versa.
syn region sasProcTRANSPOSE start="^\s*\<PROC\s\+TRANSPOSE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC TRANTAB:
" Creates, edits, and displays customized translation tables.
syn region sasProcTRANTAB start="^\s*\<PROC\s\+TRANTAB\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2

" PROC UNIVARIATE:
" Computes descriptive statistics (including quantiles), confidence intervals, and robust estimates 
" for numeric variables. Provides detail on the distribution of numeric variables, which include 
" tests for normality, plots to illustrate the distribution, frequency tables, and tests of location.
syn region sasProcUNIVARIATE start="^\s*\<PROC\s\+UNIVARIATE\>" end="\<\(QUIT\|RUN\)\s*;" fold transparent keepend extend contains=sasProcName,sasProcedure,@sasCommon,sasDOfold1,sasDOfold2



" ******
" Macros
" ******
syn match sasMacroVar "&\w\+\>\.\="
syn match sasMacroName	/%\w\+\>/
syn match sasMacroBold	"%MACRO\>\s\+\w\+\>"
syn match sasMacroBold	"%MEND\(\s\+\w\+\s*\)\=;"
"syn region sasMACROfold  start="^\s*%macro\s\+\w\+" end="%MEND\(\s\+\w\+\s*\)\=;" fold transparent keepend extend contains=ALL
syn region sasMACROfold  start="^\s*%macro\s\+\w\+" end="%MEND\(\s\+\w\+\s*\)\=;" fold transparent keepend extend contains=TOP

syn match sasMacro "%INCLUDE\>"
syn match sasMacro "%INC\>"
syn match sasMacro "%LET\>"
syn match sasMacro "%PUT\>"
syn match sasMacro "%SYSFUNC\>"
syn match sasMacro "%SYSRPUT\>"
syn match sasMacro "%SYSLPUT\>"
syn match sasMacro "%BQUOTE\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%BY\>"       containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%CMPRES\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%COMPSTOR\>" containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%DATATYP\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%DISPLAY\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%DO\>"       containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%ELSE\>"     containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%END\>"      containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%EVAL\>"     containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%GLOBAL\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%GOTO\>"     containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%IF\>"       containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%INDEX\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%INPUT\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%KEYDEF\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%LABEL\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%LEFT\>"     containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%LENGTH\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%LOCAL\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%LOWCASE\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%NRBQUOTE\>" containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%NRBQUOTE\>" containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%NRQUOTE\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%NRSTR\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QCMPRES\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QCMPRES\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QLEFT\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QLOWCASE\>" containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QSCAN\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QSUBSTR\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QSYSFUNC\>" containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QTRIM\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QUOTE\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%QUPCASE\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%RETURN\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SCAN\>"     containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%STR\>"      containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SUBSTR\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SUPERQ\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SYSCALL\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SYSEVALF\>" containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SYSEXEC\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SYSGET\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SYSPROD\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%SYSRC\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%THEN\>"     containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%TO\>"       containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%TRIM\>"     containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%UNQUOTE\>"  containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%UNTIL\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%UPCASE\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%VERIFY\>"   containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%WHILE\>"    containedin=sasMACROfold,sasDOfold1 contained
syn match sasMacro "%WINDOW\>"   containedin=sasMACROfold,sasDOfold1 contained

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
	hi sStringBold  term=NONE cterm=NONE ctermfg=Magenta ctermbg=Black gui=BOLD   guifg=Purple guibg=NONE
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
	hi TESTING  term=NONE cterm=NONE ctermfg=Magenta ctermbg=Red gui=none   guifg=Red guibg=Green

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
    HiLink sasMacroVarInString sStringBold
	"HiLink sasProc   sProc
	HiLink sasProcName   sProc
	"HiLink sasProcIML  TESTING
	HiLink sasFunctionIML  sKeyword
	HiLink sasProcSql sKeyword
    HiLink sasProcSort sKeyword
    HiLink sasProcSortOption sKeyword
    HiLink sasProcGplot sKeyword
    HiLink sasProcGplotOption sKeyword
	HiLink sasProcSqlOption	sKeyword
	HiLink sasCallIML      sKeyword
	HiLink sasStatementIML sasStatement
	HiLink sasDataOption sKeyword
	HiLink sasProcLifetest sKeyword
	HiLink sasProcLifetestOption sKeyword
	HiLink sasProcOLAP sKeyword
	HiLink sasProcOLAPOption sKeyword
	HiLink sasProcMETALIB sKeyword
	HiLink sasProcMETALIBOption sKeyword
	HiLink sasProcPhreg sKeyword
	HiLink sasProcPhregOption sKeyword
	HiLink sasProcTabulateOption sKeyword
	HiLink sasProcTabulate sKeyword

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
	HiLink sasOption    sKeyword
	HiLink sasOptionS   sProc
	HiLink sasGOption   sKeyword
	HiLink sasGOptionS  sProc
	HiLink sasAxis      sKeyword
	HiLink sasAxisS     sProc
	HiLink sasLegend    sKeyword
	HiLink sasLegendS   sProc
	HiLink sasSymbol    sKeyword
	HiLink sasSymbolS   sProc
	HiLink sasData      sSection
	HiLink sasOdsOption sKeyword
	HiLink sasOdsOpt    sProc

	" HiLink sasGlobalStatement sKeyword
	HiLink sasProcReportOption sKeyword
	HiLink sasProcReport sKeyword

	"HiLink sasProcIMLmodule TESTING

	delcommand HiLink
endif

" *******
" Folding
" *******
" TODO: Move folding information to procedure or whatever... see proc format
" as examle
"syn region sasFORMATfold start="^\s*proc\s\+format\>" end="\<run\s*;\s*" fold transparent keepend extend
"syn region sasSUBMITfold start="^\s*submit\(\s\+continue\>\)\=\s*;" end="\<endsubmit\s*;\s*" fold transparent keepend extend
syn region sasRSUBMITfold start="\<rsubmit\>.*;" end="\<endrsubmit\s*;" fold transparent keepend contains=ALL
syn match sasRSUBMIT "\<rsubmit\>.*;" contained
syn match sasRSUBMIT "\<endrsubmit\s*;" contained
hi sasRSUBMIT term=NONE cterm=Bold ctermfg=Blue  ctermbg=Black gui=Bold   guifg=DarkBlue guibg=NONE
"syn region sasREPORTfold start="^\s*proc\s\+report\>" end="\<run\s*;" fold transparent keepend extend
syn region sasDOfold1  start="%do\>.\{-};" end="%end\s*;" fold transparent keepend extend contains=ALL
syn region sasDOfold2  start="\(\_^\|[^%]\)\<do\>.\{-};" end="\(\_^\|[^%]\)\<end\s*;" fold transparent keepend extend contains=ALL
"syn region sasTHEN_DOfold start="\<then\>\(\s*\n\)*\s*%\=\<do\s*;" end="\<end\s*;" fold transparent keepend extend contains=TOP
"syn region sasELSE_DOfold start="\<else\>\(\s*\n\)*\s*%\=\<do\s*;" end="\<end\s*;" fold transparent keepend extend contains=TOP
syn region sasMETHODfold start="^\s*\w\+\s*:.*\<method\>" end="\<endmethod\>" fold transparent keepend extend contains=TOP
syn region sasLABELfold  start="^\s*\w\+:\s*\n" end="^\s*return\s*;" fold transparent keepend extend contains=TOP
"syn region sasCOMMENTfold Done in sasComment syn!!!

setlocal foldmethod=syntax
setlocal foldcolumn=4
"set foldopen=all
setlocal foldlevel=100
"set foldlevelstart=100
highlight Folded guibg=NONE guifg=LightBlue gui=Bold
highlight FoldColumn guibg=White guifg=LightBlue

" ***************
" MATCHIT options
" ***************
" MATCHIT: configuration for SAS
	:let b:match_skip = 's:comment\|string'
	:let b:match_ignorecase = 1
	:let b:match_words ='%do\>:%end;,[^%]\<do\>:[^%]\<end;,%macro\>:%mend\>,\<data\>:\<run;,\<proc\>:\<\(quit\|run\);'
	:hi MatchError guifg=lightblue guibg=lightblue

" **********************
" Ending highlight stuff
" **********************

let b:current_syntax = "sas"

setlocal sw=4
setlocal ts=4
"set expandtab
setlocal nobackup

