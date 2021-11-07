all:
	happy -gca LispVal/Par.y
	alex -g LispVal/Lex.x
	ghc --make LispVal/Test.hs -o LispVal/Test

clean:
	-rm -f LispVal/*.log LispVal/*.aux LispVal/*.hi LispVal/*.o LispVal/*.dvi

distclean: clean
	-rm -f LispVal/Doc.* LispVal/Lex.* LispVal/Par.* LispVal/Layout.* LispVal/Skel.* LispVal/Print.* LispVal/Test.* LispVal/Abs.* LispVal/Test LispVal/ErrM.* LispVal/SharedString.* LispVal/ComposOp.* LispVal/LispVal.dtd LispVal/XML.* Makefile*
		-rmdir -p LispVal/

