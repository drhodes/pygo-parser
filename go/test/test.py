from pyparsing import *

ex = "(((><>><(><)<>(<>><(>)<()><>))<<><>)<><(><>>)<><(<>)<>><>)"

LPAREN = Literal("(")
RPAREN = Literal(")")
RANGLE = Literal(">")
LANGLE = Literal("<")
RFISH = Literal("><>")
LFISH = Literal("<><")
DOH = Literal("><")

Value = Word("<>")

Call = Forward()
Call << LPAREN + Optional(Value) + Optional( Value | Call ) + Optional(Value) + RPAREN

for i in Call.scanString(ex):
    print i

print Call.parseString(ex)
