from pyparsing import *

# Production  = production_name "=" Expression "." .
# Expression  = Alternative { "|" Alternative } .
# Alternative = Term { Term } .
# Term        = production_name | token [ "..." token ] | Group | Option | Repetition .
# Group       = "(" Expression ")" .
# Option      = "[" Expression "]" .
# Repetition  = "{" Expression "}" .

Production  = Forward()
Expression  = Forward()
Alternative = Forward()
Term        = Forward()
Group       = Forward()
Option      = Forward()
Repetition  = Forward()

EQUAL = Literal("=")
DOT = Literal(".")
PIPE = Literal("|")
DQUOTE = Literal('"')
TICK = Literal("`")
LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET, LANGLE, RANGLE = map(Literal, "(){}[]<>")


token = (DQUOTE + Word(alphanums+"=()[]{}&!@#$%^&*|+=-/<>-;:_,.*'`") + DQUOTE |
         TICK + Or(map(Literal, alphanums+"""_."'""" + "\\")) + TICK )
         
def tokenAction(result):
    return ''.join(result)
token.setParseAction(tokenAction)


production_name = Word(alphas + "_")
#------------------------------------------------------------------
# Production  = production_name "=" Expression "." .
Production << production_name + EQUAL + Expression + DOT

#------------------------------------------------------------------
# Expression  = Alternative { "|" Alternative } .
Expression << Alternative + ZeroOrMore(PIPE + Alternative) 

#------------------------------------------------------------------
# Alternative = Term { Term } .
Alternative <<  ZeroOrMore(Term)

#------------------------------------------------------------------
# Term        = production_name | token [ "..." token ] | Group | Option | Repetition .
Term <<  (production_name |
          (token + Optional(Literal("...") + token)) |          
          Group |
          Option |
          Repetition)

#------------------------------------------------------------------
# Group       = "(" Expression ")" .
Group << (LPAREN + Expression + RPAREN)


#------------------------------------------------------------------
# Option      = "[" Expression "]" .
Option << (LBRACKET + Expression + RBRACKET)


#------------------------------------------------------------------
# Repetition  = "{" Expression "}" .
Repetition << (LBRACE + Expression + RBRACE)


Grammar = ZeroOrMore(Production)

temp = open('./go.ebnf').read()

#print temp
print Grammar.parseString(temp)
