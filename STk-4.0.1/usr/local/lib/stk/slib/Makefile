# Makefile for Scheme Library
# Author: Aubrey Jaffer
#
# This code is in the public domain.

SHELL = /bin/sh
intro:
	@echo
	@echo "Welcome to SLIB.  Read \"README\" and \"slib.info\" (or"
	@echo "\"slib.texi\") to learn how to install and use SLIB."
	@echo
	-make slib.info

include srcdir.mk
srcdir.mk: .. Makefile
	echo -e "srcdir = `pwd`/\n" > srcdir.mk
#srcdir=$(HOME)/slib/

VERSION = 2d5
RELEASE = 1

rpm_prefix=/usr/src/redhat/
prefix = /usr/local
exec_prefix = $(prefix)
# directory where `make install' will put executable.
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
infodir = $(exec_prefix)/info

PREVDOCS = prevdocs/

htmldir=../public_html/
dvidir=../dvi/
dvi:	$(dvidir)slib.dvi
$(dvidir)slib.dvi:	version.txi slib.texi $(dvidir)slib.fn \
		htmlform.txi objdoc.txi schmooz.texi
#	cd $(dvidir);export TEXINPUTS=$(srcdir):;texi2dvi $(srcdir)slib.texi
	-(cd $(dvidir);export TEXINPUTS=$(srcdir):;texindex slib.??)
	cd $(dvidir);export TEXINPUTS=$(srcdir):;tex $(srcdir)slib.texi
$(dvidir)slib.fn:
	cd $(dvidir);export TEXINPUTS=$(srcdir):;tex $(srcdir)slib.texi \
	$(srcdir)schmooz.texi
xdvi:	$(dvidir)slib.dvi
	xdvi -s 6 $(dvidir)slib.dvi

pdf:	$(htmldir)slib.pdf
$(htmldir)slib.pdf:	version.txi slib.texi $(dvidir)slib.fn schmooz.texi
#	cd $(dvidir);dvipdf slib.dvi # doesn't have links!
	cd $(dvidir);export TEXINPUTS=$(srcdir):;pdftex $(srcdir)slib.texi
	mv $(dvidir)slib.pdf $(htmldir)
xpdf:	$(htmldir)slib.pdf
	xpdf -z 3 $(htmldir)slib.pdf

TEXI2HTML = /usr/local/bin/texi2html -split -verbose
slib_toc.html:	version.txi slib.texi htmlform.txi schmooz.texi
	${TEXI2HTML} slib.texi
html:	$(htmldir)slib_toc.html
$(htmldir)slib_toc.html:	slib_toc.html Makefile
	-rm -f slib_stoc.html
	if [ -f $(PREVDOCS)slib_toc.html ]; \
		then hitch $(PREVDOCS)slib_\*.html slib_\*.html $(htmldir); \
		else cp slib_*.html $(htmldir);fi

RUNNABLE = scheme48
LIB = $(libdir)/$(RUNNABLE)
VM = scheme48vm
IMAGE = slib.image
INSTALL_DATA = install -c

$(LIB)/slibcat:
	touch $(LIB)/slibcat

catalogs:
	-if type scm; then scm -c "(require 'new-catalog)"; fi
	-if type guile; then guile -l guile.init -c\
	  "(use-modules (ice-9 slib)) (require 'new-catalog)"; fi
	-if type umb-scheme; then export SCHEME_INIT=umbscheme.init;\
	  echo "(require 'new-catalog)" | umb-scheme; fi
	-if type mzscheme; then export SCHEME_LIBRARY_PATH=`pwd`/;\
	  mzscheme -f DrScheme.init -e "(require 'new-catalog)" </dev/null; fi
	-if type scheme48; then make install48; fi

MKNMDB = (require 'color-names) (make-slib-color-name-db) (slib:exit)
clrnamdb: clrnamdb.scm
clrnamdb.scm:  colornam.scm Makefile
	if type scm; then scm -e"$(MKNMDB)";\
	elif type guile; then guile -l guile.init -c\
	 "(use-modules (ice-9 slib)) $(MKNMDB)";\
	elif type slib48; then echo -e "$(MKNMDB)\n,exit" | slib48 -h 3000000;\
	elif type umb-scheme; then export SCHEME_INIT=`pwd`/umbscheme.init;\
	 echo "$(MKNMDB)" | umb-scheme;\
	elif type mzscheme; then export SCHEME_LIBRARY_PATH=`pwd`/;\
	 echo "$(MKNMDB)" | mzscheme -f DrScheme.init;\
	fi

slib48:	$(IMAGE)
$(IMAGE):	Makefile scheme48.init
	export S48_VERSION="`echo ,exit | scheme48 | sed -n 's/Welcome to Scheme 48 //;s/ ([^)]*)[.]//;p;q'`";\
	export S48_VICINITY="$(LIB)/";\
	export SCHEME_LIBRARY_PATH="`pwd`/";\
	scheme48 < scheme48.init
install48:	$(IMAGE)
	$(INSTALL_DATA) $(IMAGE) $(LIB)
	(echo '#! /bin/sh';\
	 echo exec $(RUNNABLE) -i '$(LIB)/$(IMAGE)' \"\$$\@\") \
	  > $(bindir)/slib48
	chmod +x $(bindir)/slib48

#### Stuff for maintaining SLIB below ####

ver = $(VERSION)
version.txi:	Makefile
	echo @set SLIBVERSION $(VERSION) > version.txi
	echo @set SLIBDATE `date +"%B %Y"` >> version.txi

scheme = scm

htmlform.txi:	 *.scm
	$(scheme) -rschmooz -e'(schmooz "slib.texi")'
slib$(VERSION).info:	version.txi slib.texi htmlform.txi objdoc.txi schmooz.texi
	makeinfo slib.texi --no-warn --no-split -o slib.info
	mv slib.info slib$(VERSION).info
slib.info:	slib$(VERSION).info
	if [ -f $(PREVDOCS)slib.info ];\
		then infobar $(PREVDOCS)slib.info slib$(VERSION).info slib.info;\
		else cp slib$(VERSION).info slib.info;fi
info:	installinfo
installinfo:	$(infodir)/slib.info
$(infodir)/slib.info:	slib.info
	cp -a slib.info $(infodir)/slib.info
	-install-info $(infodir)/slib.info $(infodir)/dir
	-rm $(infodir)/slib.info.gz
infoz:	installinfoz
installinfoz:	$(infodir)/slib.info.gz
$(infodir)/slib.info.gz:	$(infodir)/slib.info
	gzip -f $(infodir)/slib.info

ffiles = printf.scm format.scm genwrite.scm obj2str.scm pp.scm \
	ppfile.scm strcase.scm debug.scm trace.scm lineio.scm \
	strport.scm scanf.scm chap.scm qp.scm break.scm stdio.scm \
	strsrch.scm prec.scm schmooz.scm differ.scm
lfiles = sort.scm comlist.scm tree.scm logical.scm random.scm tsort.scm \
	coerce.scm
revfiles = sc4opt.scm sc4sc3.scm sc2.scm mularg.scm mulapply.scm \
	trnscrpt.scm withfile.scm dynwind.scm promise.scm values.scm \
	eval.scm null.scm
afiles = ratize.scm randinex.scm modular.scm factor.scm \
	 charplot.scm root.scm minimize.scm cring.scm determ.scm \
	 selfset.scm psxtime.scm cltime.scm timezone.scm tzfile.scm \
	 matfile.scm
bfiles = collect.scm fluidlet.scm object.scm recobj.scm yasyn.scm
scfiles = r4rsyn.scm scmacro.scm synclo.scm synrul.scm synchk.scm \
	repl.scm macwork.scm mwexpand.scm mwdenote.scm mwsynrul.scm
scafiles = scainit.scm scaglob.scm scamacr.scm scaoutp.scm scaexpp.scm \
	structure.scm
dfiles = defmacex.scm mbe.scm srfi.scm
srfiles = srfi-1.scm
efiles = record.scm dynamic.scm queue.scm process.scm \
	priorque.scm hash.scm hashtab.scm alist.scm \
	wttree.scm wttest.scm array.scm arraymap.scm subarray.scm \
	sierpinski.scm soundex.scm byte.scm transact.scm pnm.scm \
	simetrix.scm cvs.scm dirs.scm
rfiles = rdms.scm alistab.scm dbutil.scm paramlst.scm report.scm \
	batch.scm crc.scm dbrowse.scm comparse.scm getopt.scm \
	htmlform.scm db2html.scm http-cgi.scm getparam.scm glob.scm \
	fft.scm uri.scm dbcom.scm html4each.scm dbsyn.scm
gfiles = solid.scm colorspc.scm color.scm cie1931.xyz cie1964.xyz \
	colornam.scm daylight.scm resenecolours.txt saturate.txt
docfiles = ANNOUNCE README COPYING FAQ slib.info slib.texi schmooz.texi	\
	 ChangeLog coerce.txi lineio.txi transact.txi factor.txi minimize.txi \
	 obj2str.txi randinex.txi random.txi uri.txi db2html.txi htmlform.txi \
	 http-cgi.txi version.txi fmtdoc.txi objdoc.txi solid.txi \
	 color.txi colornam.txi daylight.txi determ.txi dbutil.txi array.txi \
	 subarray.txi matfile.txi differ.txi srfi.txi srfi-1.txi cvs.txi
mfiles = Makefile require.scm Template.scm syncase.sh mklibcat.scm \
	Bev2slib.scm slib.spec
ifiles = bigloo.init chez.init elk.init macscheme.init mitscheme.init \
	scheme2c.init scheme48.init s48-0_57.init gambit.init t3.init \
	vscm.init scm.init scsh.init pscheme.init STk.init \
	RScheme.init DrScheme.init umbscheme.init guile.init
tfiles = plottest.scm formatst.scm macrotst.scm	dwindtst.scm
sfiles = $(ffiles) $(lfiles) $(revfiles) $(afiles) $(scfiles) $(efiles) \
	$(rfiles) $(gfiles) $(scafiles) $(dfiles) $(srfiles)
allfiles = $(docfiles) $(mfiles) $(ifiles) $(sfiles) $(tfiles) $(bfiles)

makedev = make -f $(HOME)/makefile.dev
CHPAT=$(HOME)/bin/chpat
RSYNC=rsync -avessh
dest = $(HOME)/dist/
temp/slib:	$(allfiles)
	-rm -rf temp
	mkdir temp
	mkdir temp/slib
	ln  $(allfiles) temp/slib

infotemp/slib:	slib.info
	-rm -rf infotemp
	mkdir infotemp
	mkdir infotemp/slib
	ln slib.info slib.info-* infotemp/slib
#For change-barred HTML.
$(PREVDOCS)slib_toc.html:
$(PREVDOCS)slib.info:	srcdir.mk Makefile
	cd $(PREVDOCS); unzip -ao $(dest)slib*.zip
	rm $(PREVDOCS)slib/slib.info
	cd $(PREVDOCS)slib; make slib.info; make slib_toc.html
	cd $(PREVDOCS); mv -f slib/slib.info slib/*.html ./
	rm -rf $(PREVDOCS)slib

distinfo:	$(dest)slib.info.zip
$(dest)slib.info.zip:	infotemp/slib
	$(makedev) TEMP=infotemp/ DEST=$(dest) PROD=slib ver=.info zip
	rm -rf infotemp

release:	dist rpm tar.gz
	cvs tag -F slib$(VERSION)
	cp ANNOUNCE $(htmldir)SLIB_ANNOUNCE.txt
	cp COPYING  $(htmldir)SLIB_COPYING.txt
	$(RSYNC) $(htmldir)SLIB.html $(htmldir)SLIB_ANNOUNCE.txt \
	 $(htmldir)SLIB_COPYING.txt nestle.ai.mit.edu:public_html/
	$(RSYNC) $(dest)README $(dest)slib$(VERSION).zip \
	 $(dest)slib$(VERSION).tar.gz \
	 $(dest)slib-$(VERSION)-$(RELEASE).noarch.rpm\
	 $(dest)slib-$(VERSION)-$(RELEASE).src.rpm nestle.ai.mit.edu:dist/
#	upload $(dest)README $(dest)slib$(VERSION).zip ftp.gnu.org:gnu/jacal/
#	$(MAKE) indiana
indiana:
	upload $(dest)slib$(VERSION).zip ftp@ftp.cs.indiana.edu:/pub/scheme-repository/incoming
	echo -e \
	'I have uploaded slib$(VERSION).zip to ftp.cs.indiana.edu:/pub/scheme-repository/incoming\n' \
	'for placement into ftp.cs.indiana.edu:/pub/scheme-repository/code/lib/' \
	 | mail -s 'SLIB upload' -b jaffer scheme-repository-request@cs.indiana.edu

postnews:
	echo -e "Newsgroups: comp.lang.scheme\n" | cat - ANNOUNCE | \
	inews -h -O -S \
	-f "announce@voluntocracy.org (Aubrey Jaffer & Radey Shouman)" \
	 -t "SLIB$(VERSION) Released" -d world

upzip:	$(HOME)/pub/slib.zip
	$(RSYNC) $(HOME)/pub/slib.zip nestle.ai.mit.edu:pub/

dist:	$(dest)slib$(VERSION).zip
$(dest)slib$(VERSION).zip:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=$(VERSION) zip

tar.gz:	$(dest)slib$(VERSION).tar.gz
$(dest)slib$(VERSION).tar.gz:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=$(VERSION) tar.gz

rpm:	pubzip
#$(dest)slib-$(VERSION)-$(RELEASE).noarch.rpm:	$(dest)slib$(VERSION).zip
	cp $(HOME)/pub/slib.zip $(rpm_prefix)SOURCES/slib$(VERSION).zip
	rpm -ba slib.spec	# --clean
	rm $(rpm_prefix)SOURCES/slib$(VERSION).zip
	mv $(rpm_prefix)RPMS/noarch/slib-$(VERSION)-$(RELEASE).noarch.rpm \
	   $(rpm_prefix)SRPMS/slib-$(VERSION)-$(RELEASE).src.rpm $(dest)

shar:	slib.shar
slib.shar:	temp/slib
	$(makedev) PROD=slib shar
dclshar:	slib.com
com:	slib.com
slib.com:	temp/slib
	$(makedev) PROD=slib com
zip:	slib.zip
slib.zip:	temp/slib
	$(makedev) DEST=../ PROD=slib zip
doszip:	/c/scm/dist/slib$(VERSION).zip
/c/scm/dist/slib$(VERSION).zip:	temp/slib
	$(makedev) DEST=/c/scm/dist/ PROD=slib ver=$(VERSION) zip
	zip -d /c/scm/dist/slib$(VERSION).zip slib/slib.info
pubzip:	temp/slib
	$(makedev) DEST=$(HOME)/pub/ PROD=slib zip

diffs:	pubdiffs
pubdiffs:	temp/slib
	$(makedev) DEST=$(HOME)/pub/ PROD=slib pubdiffs
distdiffs:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=$(ver) distdiffs
announcediffs:	temp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=$(VERSION) announcediffs

psdfiles=COPYING.psd README.psd cmuscheme.el comint.el instrum.scm pexpr.scm \
	primitives.scm psd-slib.scm psd.el read.scm runtime.scm version.scm
psdocfiles=article.bbl article.tex manual.bbl manual.tex quick-intro.tex

psdtemp/slib:
	-rm -rf psdtemp
	mkdir psdtemp
	mkdir psdtemp/slib
	mkdir psdtemp/slib/psd
	cd psd; ln $(psdfiles) ../psdtemp/slib/psd
	mkdir psdtemp/slib/psd/doc
	cd psd/doc; ln $(psdocfiles) ../../psdtemp/slib/psd/doc

psdist:	$(dest)slib-psd.tar.gz
$(dest)slib-psd.tar.gz:	psdtemp/slib
	$(makedev) DEST=$(dest) PROD=slib ver=-psd tar.gz TEMP=psdtemp/

new:
	echo `date -I` \ Aubrey Jaffer \ \<`whoami`@`hostname`\>> change
	echo>> change
	echo \	\* require.scm \(*SLIB-VERSION*\): Bumped from $(VERSION) to $(ver).>>change
	echo>> change
	cat ChangeLog >> change
	mv -f change ChangeLog
	$(CHPAT) slib$(VERSION) slib$(ver) ANNOUNCE FAQ ../scm/ANNOUNCE \
		../jacal/ANNOUNCE ../wb/README ../wb/ANNOUNCE \
		../synch/ANNOUNCE \
		$(htmldir)README.html ../dist/README \
		$(htmldir)JACAL.html \
		$(htmldir)SCM.html \
		$(htmldir)SIMSYNCH.html ../scm/scm.texi \
		/c/scm/dist/install.bat /c/scm/dist/makefile \
		/c/scm/dist/mkdisk.bat
	$(CHPAT) slib-$(VERSION) slib-$(ver) ANNOUNCE FAQ ../scm/ANNOUNCE \
		../jacal/ANNOUNCE ../wb/README ../wb/ANNOUNCE \
		../synch/ANNOUNCE \
		$(htmldir)README.html ../dist/README \
		$(htmldir)JACAL.html \
		$(htmldir)SCM.html \
		$(htmldir)SIMSYNCH.html ../scm/scm.texi \
		/c/scm/dist/install.bat /c/scm/dist/makefile \
		/c/scm/dist/mkdisk.bat
	$(CHPAT) $(VERSION) $(ver) README slib.texi require.scm Makefile \
		 $(htmldir)SLIB.html slib.spec scheme48.init s48-0_57.init
	cvs commit -lm '(*SLIB-VERSION*): Bumped from $(VERSION) to $(ver).'
	cvs tag -lF slib$(ver)

tagfiles = README version.txi slib.texi \
	$(mfiles) $(sfiles) $(bfiles) $(tfiles) $(ifiles)
# README and $(ifiles) cause semgentation faults in ETAGS for Emacs version 19.
tags:	$(tagfiles)
	etags $(tagfiles)
test:	$(sfiles)
	$(scheme) Template.scm $(sfiles)
rights:
	$(scheme) -ladmin -e"(admin:check-all)" $(sfiles) $(tfiles) \
		$(bfiles) $(ifiles)
report:
	$(scheme) -e"(slib:report #t)"
clean:
	-rm -f *~ *.bak *.orig *.rej core a.out *.o \#*
	-rm -rf *temp
distclean:	realclean
realclean:
	-rm -f *~ *.bak *.orig *.rej TAGS core a.out *.o \#*
	-rm -f slib.info* slib.?? slib.???
	-rm -rf *temp
realempty:	temp/slib
	-rm -f $(allfiles)
