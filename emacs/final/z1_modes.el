;; Hard-set the auto-mode-alist and magic-mode-alist variables.
;; I prefer things to be explicit!

(setq auto-mode-alist
      '(("#\\*mail\\*" . mail-mode)
        ("/Message[0-9]*\\'" . text-mode)
        ("/X11.+app-defaults/" . conf-xdefaults-mode)
        ("/X11.+locale/.+/Compose\\'" . conf-colon-mode)
        ("/X11.+locale/compose\\.dir\\'" . conf-javaprop-mode)
        ("/\\.?X\\(?:default\\|resource\\|re\\)s\\>" . conf-xdefaults-mode)
        ("/\\.?\\(?:gnokiirc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\'" . conf-mode)
        ("/\\.[a-z0-9-]*gdbinit" . gdb-script-mode)
        ("/\\.\\(?:enigma\\|gltron\\|gtk\\|hxplayer\\|net\\|neverball\\|qt/.+\\|realplayer\\|scummvm\\|sversion\\|sylpheed/.+\\|xmp\\)rc\\'" . conf-mode)
        ("/\\.\\(?:gdbtkinit\\|grip\\|orbital/.+txt\\|rhosts\\|tuxracer/options\\)\\'" . conf-mode)
        ("/config\\.\\(?:bat\\|log\\)\\'" . fundamental-mode)
        ("/crontab\\.X*[0-9]+\\'" . shell-script-mode)
        ("BROWSE\\'" . ebrowse-tree-mode)
        ("Cakefile" . coffee-mode)
        ("Imakefile\\'" . makefile-imake-mode)
        ("Makeppfile\\(?:\\.mk\\)?\\'" . makefile-makepp-mode)
        ("Project\\.ede$" . emacs-lisp-mode)
        ("[/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-mode-maybe)
        ("[:/]_emacs\\'" . emacs-lisp-mode)
        ("[Mm]akefile\\'" . makefile-gmake-mode)
        ("[]>:/\\]\\..*\\(emacs\\|gnus\\|viper\\)\\'" . emacs-lisp-mode)
        ("[cC]hange[lL]og[-.][-0-9a-z]+\\'" . change-log-mode)
        ("[cC]hange[lL]og[-.][0-9]+\\'" . change-log-mode)
        ("[cC]hange\\.?[lL]og?\\'" . change-log-mode)
        ("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode)
        ("\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'" . sh-mode)
        ("\\(/\\|\\`\\)\\.\\(bash_logout\\|shrc\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\)\\'" . sh-mode)
        ("\\(/\\|\\`\\)\\.\\(bash_profile\\|z?login\\|bash_login\\|z?logout\\)\\'" . sh-mode)
        ("\\(Root\\)?ProjStep\\.ede" . emacs-lisp-mode)
        ("\\.Z\\(~\\|\\.~[0-9]+~\\)?\\'" nil jka-compr)
        ("\\.[1-9]\\'" . nroff-mode)
        ("\\.[ch]\\'" . c-mode)
        ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
        ("\\.[ck]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\'" . sh-mode)
        ("\\.[ds]?v\\'" . verilog-mode)
        ("\\.[eE]?[pP][sS]\\'" . ps-mode)
        ("\\.[fF]\\'" . fortran-mode)
        ("\\.[sS]\\'" . asm-mode)
        ("\\.[tT]e[xX]\\'" . tex-mode)
        ("\\.\\(?:PDF\\|DVI\\|pdf\\|dvi\\)\\'" . doc-view-mode)
        ("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode)
        ("\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\'" . conf-mode)
        ("\\.\\(?:desktop\\|la\\)\\'" . conf-unix-mode)
        ("\\.\\(?:orig\\|in\\|[bB][aA][kK]\\)\\'" nil t)
        ("\\.\\(CC?\\|HH?\\)\\'" . c++-mode)
        ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . perl-mode)
        ("\\.\\(arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\|rar\\|ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\|RAR\\)\\'" . archive-mode)
        ("\\.\\(as\\|mi\\|sm\\)2\\'" . snmpv2-mode)
        ("\\.\\(asn\\|mib\\|smi\\)\\'" . snmp-mode)
        ("\\.\\(cc\\|hh\\)\\'" . c++-mode)
        ("\\.\\(deb\\|[oi]pk\\)\\'" . archive-mode)
        ("\\.\\(dif\\|pat\\)\\'" . diff-mode)
        ("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode)
        ("\\.\\(scm\\|stk\\|ss\\|sch\\)\\'" . scheme-mode)
        ("\\.\\(soa\\|zone\\)\\'" . dns-mode)
        ("\\.\\(sx[dmicw]\\|od[fgpst]\\|oxt\\)\\'" . archive-mode)
        ("\\.\\(u?lpc\\|pike\\|pmod\\(.in\\)?\\)\\'" . pike-mode)
        ("\\.ad[abs]\\'" . ada-mode)
        ("\\.ad[bs].dg\\'" . ada-mode)
        ("\\.adb\\'" . ada-mode)
        ("\\.adb\\.dg\\'" . ada-mode)
        ("\\.ads\\'" . ada-mode)
        ("\\.ads\\.dg\\'" . ada-mode)
        ("\\.am\\'" . makefile-automake-mode)
        ("\\.article\\'" . text-mode)
        ("\\.asd\\'" . lisp-mode)
        ("\\.asm\\'" . asm-mode)
        ("\\.awk\\'" . awk-mode)
        ("\\.bash\\'" . sh-mode)
        ("\\.bbl\\'" . latex-mode)
        ("\\.bib\\'" . bibtex-mode)
        ("\\.bsh\\'" . bsh-script-mode)
        ("\\.bst\\'" . bibtex-style-mode)
        ("\\.by$" . bovine-grammar-mode)
        ("\\.bz2\\(~\\|\\.~[0-9]+~\\)?\\'" nil jka-compr)
        ("\\.cabal\\'" . haskell-cabal-mode)
        ("\\.cconf" . python-mode)
        ("\\.cgr\\'" . cogre-mode)
        ("\\.cinc" . python-mode)
        ("\\.cl[so]\\'" . latex-mode)
        ("\\.clj$" . clojure-mode)
        ("\\.cljs$" . clojure-mode)
        ("\\.coffee$" . coffee-mode)
        ("\\.com\\'" . dcl-mode)
        ("\\.css\\'" . css-mode)
        ("\\.ctest" . python-mode)
        ("\\.d[i]?\\'" . d-mode)
        ("\\.docbook\\'" . sgml-mode)
        ("\\.dot\\'" . cogre-dot-mode)
        ("\\.ds\\(ss\\)?l\\'" . dsssl-mode)
        ("\\.dtd\\'" . sgml-mode)
        ("\\.dtx\\'" . doctex-mode)
        ("\\.dz\\'" nil jka-compr)
        ("\\.ebrowse\\'" . ebrowse-tree-mode)
        ("\\.el\\'" . emacs-lisp-mode)
        ("\\.exp\\'" . tcl-mode)
        ("\\.f9[05]\\'" . f90-mode)
        ("\\.fs[iylx]?$" . fsharp-mode)
        ("\\.for\\'" . fortran-mode)
        ("\\.g?z\\(~\\|\\.~[0-9]+~\\)?\\'" nil jka-compr)
        ("\\.g\\'" . antlr-mode)
        ("\\.gcov\\'" . compilation-mode)
        ("\\.gif\\'" . image-mode)
        ("\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" nil epa-file)
        ("\\.gradle\\'" . groovy-mode)
        ("\\.gsp\\'" . gsp-html-mumamo-mode)
        ("\\.gss\\'" . css-mode)
        ("\\.gyp\\'" . python-mode)
        ("\\.gypi\\'" . python-mode)
        ("\\.h\\'" . objc-mode)
        ("\\.hsc\\'" . haskell-c-mode)
        ("\\.htm\\'" . html-mode)
        ("\\.html\\'" . html-mode)
        ("\\.i?tcl\\'" . tcl-mode)
        ("\\.i\\'" . c-mode)
        ("\\.icn\\'" . icon-mode)
        ("\\.idl\\'" . idl-mode)
        ("\\.ii\\'" . c++-mode)
        ("\\.inc\\'" . php-mode)
        ("\\.indent\\.pro\\'" . fundamental-mode)
        ("\\.ins\\'" . tex-mode)
        ("\\.itk\\'" . tcl-mode)
        ("\\.java\\'" . java-mode)
        ("\\.jpe?g\\'" . image-mode)
        ("\\.js$" . js2-mode)
        ("\\.l[gh]s\\'" . literate-haskell-mode)
        ("\\.l\\'" . lisp-mode)
        ("\\.ld[si]?\\>" . ld-script-mode)
        ("\\.less\\'" . less-css-mode)
        ("\\.letter\\'" . text-mode)
        ("\\.lex\\'" . c-mode)
        ("\\.li?sp\\'" . lisp-mode)
        ("\\.ltx\\'" . latex-mode)
        ("\\.lua\\'" . lua-mode)
        ("\\.m?spec\\'" . sh-mode)
        ("\\.m[4c]\\'" . m4-mode)
        ("\\.m[mes]\\'" . nroff-mode)
        ("\\.m\\'" . objc-mode)
        ("\\.makepp\\'" . makefile-makepp-mode)
        ("\\.man\\'" . nroff-mode)
        ("\\.mf\\'" . metafont-mode)
        ("\\.mixal\\'" . mixal-mode)
        ("\\.mk\\'" . makefile-gmake-mode)
        ("\\.ml[iylp]?" . tuareg-mode)
        ("\\.mp\\'" . metapost-mode)
        ("\\.mss\\'" . scribe-mode)
        ("\\.muse\\'" . muse-mode)
        ("\\.mx-chart\\'" . chart-mode)
        ("\\.nmb$" . nimbus-mode)
        ("\\.oak\\'" . scheme-mode)
        ("\\.org\\'" . org-mode)
        ("\\.p[bpgn]m\\'" . image-mode)
        ("\\.p\\'" . pascal-mode)
        ("\\.pas\\'" . pascal-mode)
        ("\\.ppd\\'" . conf-ppd-mode)
        ("\\.pro$" . prolog-mode)
        ("\\.proto$" . protobuf-mode)
        ("\\.prolog\\'" . prolog-mode)
        ("\\.properties\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'" . conf-javaprop-mode)
        ("\\.py\\'" . python-mode)
        ("\\.rb\\'" . ruby-mode)
        ("\\.re?st\\'" . rst-mode)
        ("\\.rnc\\'" . rnc-mode)
        ("\\.s\\(v\\|iv\\|ieve\\)\\'" . sieve-mode)
        ("\\.scm\\.[0-9]*\\'" . scheme-mode)
        ("\\.ses\\'" . ses-mode)
        ("\\.sgml?\\'" . sgml-mode)
        ("\\.sim\\'" . simula-mode)
        ("\\.soa\\'" . dns-mode)
        ("\\.soy\\'" . closure-template-html-mode)
        ("\\.sql\\'" . sql-mode)
        ("\\.srt$" . srecode-template-mode)
        ("\\.st\\'" . stringtemplate-mode)
        ("\\.strings\\'" . javascript-mode)
        ("\\.sty\\'" . latex-mode)
        ("\\.tar\\'" . tar-mode)
        ("\\.tbz2?\\'" . tar-mode)
        ("\\.te?xi\\'" . texinfo-mode)
        ("\\.te?xt\\'" . text-mode)
        ("\\.texinfo\\'" . texinfo-mode)
        ("\\.tgz\\'" . tar-mode)
        ("\\.thrift$" . thrift-mode)
        ("\\.tiff?\\'" . image-mode)
        ("\\.tw" . python-mode)
        ("\\.tw" . python-mode)
        ("\\.vhdl?\\'" . vhdl-mode)
        ("\\.vr[hi]?\\'" . vera-mode)
        ("\\.wy$" . wisent-grammar-mode)
        ("\\.x[bdsru]?[cn]?\\'" . ld-script-mode)
        ("\\.x[bp]m\\'" . c-mode)
        ("\\.x[bp]m\\'" . image-mode-maybe)
        ("\\.y\\(acc\\)?\\'" . c-mode)
        ("\\.zone\\'" . zone-mode)
        ("\\.~?[0-9]+\\.[0-9][-.0-9]*~?\\'" nil t)
        ("\\/TARGETS$" . python-mode)
        ("\\SConscript" . python-mode)
        ("\\SConstruct" . python-mode)
        ("\\`/etc/\\(?:DIR_COLORS\\|ethers\\|.?fstab\\|.*hosts\\|lesskey\\|login\\.?de\\(?:fs\\|vperm\\)\\|magic\\|mtab\\|pam\\.d/.*\\|permissions\\(?:\\.d/.+\\)?\\|protocols\\|rpc\\|services\\)\\'" . conf-space-mode)
        ("\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|opera6rc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\'" . conf-mode)
        ("\\`/tmp/Re" . text-mode)
        ("\\`/tmp/fol/" . text-mode)
        ("\\`\\..*emacs\\'" . emacs-lisp-mode)
        ("configure\\.\\(ac\\|in\\)\\'" . autoconf-mode)
        ("java.+\\.conf\\'" . conf-javaprop-mode)
        ("svn-commit" . svncommit-mode)))
