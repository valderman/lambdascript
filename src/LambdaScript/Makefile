.parser: Par.y
	happy --info=happy.info -gca Par.y
	touch .parser

.lexer: Lex.x
	alex -g Lex.x
	touch .lexer

clean:
	rm Abs.* Doc.* ErrM.* Lex.* Par.* Print.* Skel.* Test.* happy.info
	rm .lexer .parser
