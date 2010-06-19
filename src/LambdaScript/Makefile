.parser: Par.y
	happy -gca Par.y
	touch .parser

.lexer: Lex.x
	alex -g Lex.x
	touch .lexer

clean:
	rm Abs.* Doc.* ErrM.* Lex.* Par.* Print.* Skel.* Test.*
	rm .lexer .parser
