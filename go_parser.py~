from pyparsing import *

#==================================================================
# a testing function, the global thing makes testing cleaner.

def testParse(name, xs):
    upperName = ''.join(x.upper() for x in name ) 
    f = globals()[name].parseString # cleaner
    print "--------------------------------------------------------------------"
    print upperName
    print
    for x in xs:
        print x, (40-len(x))*" ", "=>", f(x)

LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE = map(Literal, "()[]{}")
COMMA = Literal(",")
COLON = Literal(":")
STAR = Literal("*")
SEMI = Literal(";")
EQUAL = Literal("=")

#-------------------------------------------------------------------------
#  unicode_char   = /* an arbitrary Unicode code point */ .
#  unicode_letter = /* a Unicode code point classified as "Letter" */ .
#  unicode_digit  = /* a Unicode code point classified as "Digit" */ .

def literals_(xs): return Or(map(Literal, xs))

unicode_char   = alphanums #'''   # Not really sure about this.
                           # what about -, or + ?? 
                           # will it muck up the parser?

#unicode_char   = ''.join(map(chr, range(28, 128)))
unicode_letter = alphas
unicode_digit  = "0123456789"


#-------------------------------------------------------------------------
#  letter        = unicode_letter | "_" .
#  decimal_digit = "0" ... "9" .
#  octal_digit   = "0" ... "7" .
#  hex_digit     = "0" ... "9" | "A" ... "F" | "a" ... "f" .

letter        = unicode_letter + "_"
decimal_digit = unicode_digit
octal_digit   = "01234567"
hex_digit     = unicode_digit + "ABCDEF" + "abcdef"


#-------------------------------------------------------------------------
#  identifier = letter { letter | unicode_digit } .
identifier = Word( letter + unicode_digit )

#-------------------------------------------------------------------------
#  PackageClause  = "package" PackageName .
#  PackageName    = identifier .
PackageName = identifier
PackageClause = Suppress("package") + PackageName

testParse("PackageClause", ["package foo"])

#-------------------------------------------------------------------------
#  QualifiedIdent = [ PackageName "." ] identifier .
DOT = Literal( "." )
QualifiedIdent = Optional( PackageName + DOT ) + identifier 

testParse("QualifiedIdent", ["fmt.Printf",
                             "asdf",
                             ])


#-------------------------------------------------------------------------
#  TypeName  = QualifiedIdent.
TypeName = QualifiedIdent


#-------------------------------------------------------------------------
#  decimal_lit = ( "1" ... "9" ) { decimal_digit } .
#  octal_lit   = "0" { octal_digit } .
#  hex_lit     = "0" ( "x" | "X" ) hex_digit { hex_digit } .
#  int_lit     = decimal_lit | octal_lit | hex_lit .

decimal_lit = literals_("123456789") + Word(decimal_digit)
octal_lit   = Literal("0") + Word(octal_digit)
hex_lit     = (Literal("0x") | Literal("0X")) + Word(hex_digit)
int_lit     = decimal_lit | octal_lit | hex_lit

testParse("int_lit", ["0x345", "0345", "345"])


#-------------------------------------------------------------------------
# decimals  = decimal_digit { decimal_digit } .
# exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals .

decimals = Word(decimal_digit)
exponent =  literals_("eE") + Optional(literals_("+-")) + decimals


#-------------------------------------------------------------------------
# float_lit = decimals "." [ decimals ] [ exponent ] |
#             decimals exponent |
#             "." decimals [ exponent ] .
float_lit = (Or( decimals + Literal(".") + Optional(decimals) + Optional(exponent)) |
             (decimals + exponent) |
             (Literal(".") + decimals | exponent))

testParse("float_lit", ["2.34e34",
                        "0234e-34",
                        "0234e+34"])


#-------------------------------------------------------------------------
#  escaped_char     = `\` ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | `\` | "'" | `"` ) .
escaped_char = Literal("\\") + literals_('''abfnrtv'"''') | Literal(r"\\")

print 
testParse("escaped_char", [r'\n',
                           r'\a',
                           r'\"',
                           r"\'",
                           r'\\',
                           ])

#-------------------------------------------------------------------------
#  octal_byte_value = `\` octal_digit octal_digit octal_digit .
#  hex_byte_value   = `\` "x" hex_digit hex_digit .
#  little_u_value   = `\` "u" hex_digit hex_digit hex_digit hex_digit .
#  big_u_value      = `\` "U" hex_digit hex_digit hex_digit hex_digit
#                             hex_digit hex_digit hex_digit hex_digit .
BACKSLASH = Literal("\\")
OctalDigit = literals_(octal_digit)
HexDigit = literals_(hex_digit)

octal_byte_value = BACKSLASH + OctalDigit + OctalDigit + OctalDigit
hex_byte_value   = BACKSLASH + Literal("x") + HexDigit + HexDigit
little_u_value   = BACKSLASH + Literal("u") + HexDigit + HexDigit + HexDigit + HexDigit
big_u_value      = (BACKSLASH + Literal("U") + HexDigit + HexDigit + HexDigit + HexDigit 
                                             + HexDigit + HexDigit + HexDigit + HexDigit)

testParse('octal_byte_value', ["""\\123"""])
testParse('hex_byte_value', ["""\\x00"""])
testParse('little_u_value', ["""\\u1234"""])
testParse('big_u_value', ["""\\U12345678"""])


#-------------------------------------------------------------------------
#  byte_value       = octal_byte_value | hex_byte_value
#  unicode_value    = unicode_char | little_u_value | big_u_value | escaped_char .
#  char_lit         = "'" ( unicode_value | byte_value ) "'" .

SQUOTE = Literal("'")
byte_value       = octal_byte_value | hex_byte_value
unicode_value    = little_u_value | big_u_value | escaped_char | literals_(unicode_char)
char_lit         = SQUOTE + (byte_value | unicode_value ) + SQUOTE

testParse("byte_value", ["""\\111""",
                         """\\x12"""
                         ])

testParse("unicode_value", ["\\u1111",
                            "\\U11112222",
                            ])

testParse("char_lit", [""" '\\x12' """,
                       """ '\\x11' """,
                       """ '\\x12' """,
                       """ '\\u1111' """,
                       """ '\\U11112222' """,                  
                       """ 'j' """,
                       ])
                       
#-------------------------------------------------------------------------
#  raw_string_lit         = "`" { unicode_char } "`" .
#  interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
#  string_lit             = raw_string_lit | interpreted_string_lit .

TICK = Literal( "`" )
DQUOTE = Literal( '"' )
raw_string_lit         = TICK + Word(unicode_char) + TICK
interpreted_string_lit = DQUOTE + ( Word(unicode_char) | byte_value ) + DQUOTE
#NEED TO DISCOVER IF PYPARSING CAN DEAL WITH UNICODE
#interpreted_string_lit = DQUOTE + ( Word(unicode_value) | Word(byte_value) ) + DQUOTE
string_lit             = raw_string_lit | interpreted_string_lit

testParse("raw_string_lit", [" `a` ",
                             " `asd123f` ",
                             ])

testParse("interpreted_string_lit", [' "000000" ',
                                     ])

testParse("string_lit", [' "000000" ',
                                     ])

#-------------------------------------------------------------------------
log_op     = literals_(["||", "&&"])
com_op     = Literal("<-")
rel_op     = literals_([ "==", "!=", "<", "<=", ">", ">=" ])
add_op     = literals_([ "+", "-", "|", "^" ])
mul_op     = literals_([ "*", "/", "%", "<<", ">>", "&", "&^" ])
binary_op  = log_op | com_op | rel_op | add_op | mul_op 
unary_op   = literals_([ "+", "-", "!", "^", "*", "&", "<-" ])


#-------------------------------------------------------------------------
# These are mutually recursive in one way or another. so they need to
# be initialized here with Forward()

ArrayType, \
ElementType, \
ElementList, \
FunctionType, \
InterfaceType, \
LiteralType, \
PointerType, \
SliceType, \
StructType, \
MapType, \
ChannelType  = [Forward() for x in range(11)]

#-------------------------------------------------------------------------
#  IdentifierList = identifier { "," identifier } .
IdentifierList = delimitedList( identifier )

#-------------------------------------------------------------------------
BasicLit   = int_lit | float_lit | char_lit | string_lit

#-------------------------------------------------------------------------
#  LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
#                  SliceType | MapType | TypeName | "(" LiteralType ")" .
LiteralType   << ( StructType | ArrayType | LBRACKET + Literal("...") + RBRACKET + ElementType |
                   SliceType | MapType | TypeName | LPAREN + LiteralType + RPAREN )


#-------------------------------------------------------------------------
#  CompositeLit  = LiteralType "{" [ ElementList [ "," ] ] "}" .
CompositeLit  = LiteralType + LBRACE + Optional( ElementList + Optional(COMMA)) + RBRACE

#-------------------------------------------------------------------------
#  GoLiteral    = BasicLit | CompositeLit | FunctionLit .
#  Operand    = GoLiteral | QualifiedIdent | MethodExpr | "(" Expression ")" .


#-------------------------------------------------------------------------
#  PrimaryExpr =
#          Operand |
#          Conversion |
#          BuiltinCall |
#          PrimaryExpr Selector |
#          PrimaryExpr Index |
#          PrimaryExpr Slice |
#          PrimaryExpr TypeAssertion |
#          PrimaryExpr Call .
PrimaryExpr = Forward()

#-------------------------------------------------------------------------
#  UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
#  Expression = UnaryExpr | Expression binary_op UnaryExpr .
UnaryExpr  = Forward()
Expression = Forward()

UnaryExpr  << ( PrimaryExpr | unary_op + UnaryExpr )
Expression << ( UnaryExpr | ( Expression + binary_op + UnaryExpr ))

#-------------------------------------------------------------------------
#  ExpressionList = Expression { "," Expression } .
ExpressionList = delimitedList( Expression )

#  TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
#              SliceType | MapType | ChannelType .
#  Type      = TypeName | TypeLit | "(" Type ")" .

TypeLit   = Forward()
Type      = Forward()
Type     << (TypeName | TypeLit | LPAREN + Type + RPAREN)

#-------------------------------------------------------------------------
#  FieldName     = identifier .
#  ElementIndex  = Expression .
#  Value         = Expression .
#  Key           = FieldName | ElementIndex .
#  Element       = [ Key ":" ] Value .
#  ElementList   = Element { "," Element } .
FieldName     = identifier 
ElementIndex  = Expression
Value         = Expression 
Key           = FieldName | ElementIndex
Element       = Optional( Key + COLON ) + Value
ElementList   << delimitedList( Element )

#-------------------------------------------------------------------------
#  Selector       = "." identifier .
#  Index          = "[" Expression "]" .
#  Slice          = "[" Expression ":" [ Expression ] "]" .
#  TypeAssertion  = "." "(" Type ")" .
#  Call           = "(" [ ExpressionList [ "," ] ] ")" .

Selector       = DOT + identifier
Index          = LBRACKET + Expression + RBRACKET
Slice          = LBRACKET + Expression + COLON + Optional(LBRACKET + Expression + RBRACKET) 
TypeAssertion  = DOT + LPAREN + Type + RPAREN
Call           = LPAREN + Optional(ExpressionList + Optional( COMMA )) + RPAREN

#-------------------------------------------------------------------------
MethodName         = identifier

#-------------------------------------------------------------------------
#  MethodExpr    = ReceiverType "." MethodName .
#  ReceiverType  = TypeName | "(" "*" TypeName ")" .
ReceiverType  = TypeName | LPAREN + STAR + TypeName + RPAREN
MethodExpr    = ReceiverType + DOT + MethodName

#-------------------------------------------------------------------------
#TypeLit   = (ArrayType | StructType | PointerType | FunctionType | 
#             InterfaceType | SliceType | MapType | ChannelType)
TypeLit   << (ArrayType | StructType | PointerType | FunctionType | 
             InterfaceType | SliceType | MapType | ChannelType)

#-------------------------------------------------------------------------
#  ArrayType   = "[" ArrayLength "]" ElementType .
#  ArrayLength = Expression .
#  ElementType = Type .
ElementType << Type 
ArrayLength = Expression 
ArrayType   << LBRACKET + ArrayLength + RBRACKET + ElementType

#-------------------------------------------------------------------------
#SliceType = "[" "]" ElementType .
SliceType << LBRACKET + RBRACKET + ElementType


#-------------------------------------------------------------------------
#  AnonymousField = [ "*" ] TypeName .
#  FieldDecl      = (IdentifierList Type | AnonymousField) [ Tag ] .
#  StructType     = "struct" "{" { FieldDecl ";" } "}" .
#  Tag            = string_lit .
AnonymousField = Optional(STAR) + TypeName
Tag            = string_lit 
FieldDecl      = (IdentifierList  + Type | AnonymousField) +Optional( Tag )
StructType     << Literal("struct") + LBRACE + ZeroOrMore( FieldDecl + SEMI ) + RBRACE


#-------------------------------------------------------------------------
#  BaseType = Type .
#  PointerType = "*" BaseType .
BaseType = Type
PointerType << STAR + BaseType


#-------------------------------------------------------------------------
#  ParameterDecl  = [ IdentifierList ] ( Type | "..." ) .
#  ParameterList  = ParameterDecl { "," ParameterDecl } .
#  Parameters     = "(" [ ParameterList [ "," ] ] ")" .
#  Result         = Parameters | Type .
#  Signature      = Parameters [ Result ] .
#  FunctionType   = "func" Signature .

ParameterDecl  = Optional( IdentifierList ) + Group( Type | "..." ) 
ParameterList  = ParameterDecl + ZeroOrMore( COMMA + ParameterDecl )
Parameters     = LPAREN + Optional( ParameterList + Optional( COMMA )) + RPAREN
Result         = Parameters | Type
Signature      = Parameters + Optional( Result )
FunctionType   << Literal("func") + Signature 


#-------------------------------------------------------------------------
#  InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
#  MethodSpec         = MethodName Signature | InterfaceTypeName .
#  InterfaceTypeName  = TypeName .
InterfaceTypeName  = TypeName 
MethodSpec         = MethodName + Signature | InterfaceTypeName 
InterfaceType      << Literal("interface") + LBRACE + ZeroOrMore( MethodSpec + SEMI ) + RBRACE


#-------------------------------------------------------------------------
#  MapType     = "map" "[" KeyType "]" ElementType .
#  KeyType     = Type .
KeyType     = Type 
MapType     << Literal("map") + LBRACKET + KeyType + RBRACKET + ElementType 


#-------------------------------------------------------------------------
#  RecvChannel   = "<-" "chan" ElementType .
#  SendChannel   = "chan" "<-" ElementType .
#  Channel       = "chan" ElementType .
#  ChannelType   = Channel | SendChannel | RecvChannel .
LARROW = Literal( "<-" )
CHAN = Literal( "chan" )

RecvChannel   = LARROW + CHAN + ElementType
SendChannel   = CHAN + LARROW + ElementType 
Channel       = CHAN + ElementType 
ChannelType   << ( Channel | SendChannel | RecvChannel )

#-------------------------------------------------------------------------
#  Statement =
#          Declaration | LabeledStmt | SimpleStmt |
#          GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
#          FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
#          DeferStmt .
Declaration, LabeledStmt, SimpleStmt, GoStmt, ReturnStmt, BreakStmt, ContinueStmt, GotoStmt, FallthroughStmt, Block, IfStmt, SwitchStmt, SelectStmt, ForStmt, DeferStmt = [Forward() for x in range(15)]

Statement = Declaration | LabeledStmt | SimpleStmt | GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt | DeferStmt

#-------------------------------------------------------------------------
#  Block = "{" { Statement ";" } "}" .
Block << LBRACE + ZeroOrMore( Statement + SEMI ) + RBRACE

#-------------------------------------------------------------------------
#  ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .
#  ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
ConstSpec      = IdentifierList + Optional( Optional( Type ) + EQUAL + ExpressionList)
ConstDecl      = Literal("const") + Group( ConstSpec | LPAREN + ZeroOrMore( ConstSpec + SEMI ) + RPAREN) 

#-------------------------------------------------------------------------
#  TypeSpec     = identifier Type .
#  TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
TypeSpec     = identifier + Type
TypeDecl     = Literal("type") + Group( TypeSpec | LPAREN + ZeroOrMore( TypeSpec + SEMI ) + RPAREN)

#-------------------------------------------------------------------------
#  VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
#  VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
VarSpec     = IdentifierList + Group(Type + Optional(EQUAL + ExpressionList) | EQUAL + ExpressionList)
VarDecl     = Literal("var") + Group( VarSpec | LPAREN + ZeroOrMore(VarSpec + SEMI) + RPAREN)

#-------------------------------------------------------------------------
#  Body         = Block.
#  FunctionDecl = "func" identifier Signature [ Body ] .
Body         = Block
FunctionDecl = Literal("func") + identifier + Signature + Optional( Body )

#-------------------------------------------------------------------------
#  BaseTypeName = identifier .
#  Receiver     = "(" [ identifier ] [ "*" ] BaseTypeName ")" .
#  MethodDecl   = "func" Receiver MethodName Signature [ Body ] .
BaseTypeName = identifier
Receiver     = LPAREN + Optional(identifier ) + Optional(STAR) +  BaseTypeName + RPAREN
MethodDecl   = Literal("func") + Receiver + MethodName + Signature + Optional( Body )

#-------------------------------------------------------------------------
#  Declaration   = ConstDecl | TypeDecl | VarDecl .
#  TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .
Declaration   << ( ConstDecl | TypeDecl | VarDecl )
TopLevelDecl  = Declaration | FunctionDecl | MethodDecl 

#-------------------------------------------------------------------------
#  ShortVarDecl = IdentifierList ":=" ExpressionList .
ShortVarDecl = IdentifierList + Literal(":=") + ExpressionList 

#-------------------------------------------------------------------------
#  FunctionLit = FunctionType Body .
FunctionLit = FunctionType + Body

#-------------------------------------------------------------------------
#  Conversion = LiteralType "(" Expression ")" .
Conversion = LiteralType + LPAREN + Expression + RPAREN

#-------------------------------------------------------------------------
#  ExpressionStmt = Expression .
#  assign_op = [ add_op | mul_op ] "=" .
#  IncDecStmt = Expression ( "++" | "--" ) .
#  Assignment = ExpressionList assign_op ExpressionList .
ExpressionStmt = Expression
assign_op = Optional(add_op | mul_op) + EQUAL
IncDecStmt = Expression + ( Literal("++") | Literal("--") )
Assignment = ExpressionList + assign_op + ExpressionList

#-------------------------------------------------------------------------
#  SimpleStmt = EmptyStmt | ExpressionStmt | IncDecStmt | Assignment | ShortVarDecl .
#  EmptyStmt = .

EmptyStmt = Word('') # !! ATTN THIS COULD BE THE PROBLEM. !!
SimpleStmt << ( EmptyStmt | ExpressionStmt | IncDecStmt | Assignment | ShortVarDecl )

#-------------------------------------------------------------------------
#  Label       = identifier .
#  LabeledStmt = Label ":" Statement .
Label       = identifier 
LabeledStmt << ( Label + COLON + Statement )

#-------------------------------------------------------------------------
#  IfStmt    = "if" [ SimpleStmt ";" ] [ Expression ] Block [ "else" Statement ] .
IF = Literal("if")
ELSE = Literal("else")

IfStmt << IF + Optional( SimpleStmt + SEMI) + Optional( Expression ) + Block + Optional( ELSE + Statement )

#-------------------------------------------------------------------------
#  TypeList        = Type { "," Type } .
#  TypeSwitchCase  = "case" TypeList | "default" .
#  TypeCaseClause  = TypeSwitchCase ":" { Statement ";" } .
#  TypeSwitchGuard = [ identifier ":=" ] Expression "." "(" "type" ")" .
#  TypeSwitchStmt  = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
TypeList        = Type + ZeroOrMore( COMMA + Type)
TypeSwitchCase  = Literal("case") + (TypeList | Literal("default"))
TypeCaseClause  = TypeSwitchCase + COLON + ZeroOrMore( Statement + SEMI )
TypeSwitchGuard = Optional( identifier + Literal(":=")) +  Expression + DOT + LPAREN + Literal("type") + RPAREN
TypeSwitchStmt  = (Literal("switch") + Optional( SimpleStmt + SEMI ) + 
                   TypeSwitchGuard + LBRACE + ZeroOrMore( TypeCaseClause ) + RBRACE)

#-------------------------------------------------------------------------
#  ExprSwitchCase = "case" ExpressionList | "default" .
#  ExprCaseClause = ExprSwitchCase ":" { Statement ";" } .
#  ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
#  SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .

ExprSwitchCase = Literal("case") + (ExpressionList | Literal("default"))
ExprCaseClause = ExprSwitchCase + COLON + ZeroOrMore( Statement + SEMI )
ExprSwitchStmt = ( Literal("switch") + Optional( SimpleStmt + SEMI ) + 
                   Optional( Expression ) + LBRACE + ZeroOrMore( ExprCaseClause ) + RBRACE )
SwitchStmt << ( ExprSwitchStmt | TypeSwitchStmt )

#-------------------------------------------------------------------------
#  InitStmt = SimpleStmt .
#  PostStmt = SimpleStmt .
#  ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
#  RangeClause = ExpressionList ( "=" | ":=" ) "range" Expression .

#  Condition = Expression .
#  ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
InitStmt = SimpleStmt 
PostStmt = SimpleStmt 
Condition = Expression 

ForClause = Optional(InitStmt) + SEMI + Optional( Condition ) + SEMI + Optional( PostStmt ) 
RangeClause = ExpressionList + Group( Literal("=") | Literal(":=") ) + Literal("range") + Expression 
ForStmt << ( Literal("for") + Optional( Condition | ForClause | RangeClause ) + Block )

#-------------------------------------------------------------------------
#  GoStmt = "go" Expression .
GoStmt << Literal("go") + Expression

#-------------------------------------------------------------------------
#  SelectStmt = "select" "{" { CommClause } "}" .
#  CommClause = CommCase ":" { Statement ";" } .
#  CommCase = "case" ( SendExpr | RecvExpr) | "default" .
#  SendExpr =  Expression "<-" Expression .
#  RecvExpr =  [ Expression ( "=" | ":=" ) ] "<-" Expression .

RecvExpr =  Optional( Expression + Optional( EQUAL | Literal(":="))) + LARROW + Expression
SendExpr =  Expression + LARROW + Expression 
CommCase = Literal("case") + Group( SendExpr | RecvExpr) | Literal("default")
CommClause = CommCase + COLON + ZeroOrMore( Statement + SEMI )
SelectStmt << ( Literal("select") + LBRACE + ZeroOrMore( CommClause ) + RBRACE )

ReturnStmt << Literal("return") + Optional( ExpressionList )
BreakStmt << Literal("break") + Optional( Label )
ContinueStmt << Literal("continue") + Optional( Label )
GotoStmt << Literal("goto") + Label
FallthroughStmt << Literal("fallthrough")
DeferStmt << Literal("defer") + Expression

#-------------------------------------------------------------------------
#  GoLiteral    = BasicLit | CompositeLit | FunctionLit .
#  Operand    = GoLiteral | QualifiedIdent | MethodExpr | "(" Expression ")" .
GoLiteral  = BasicLit | CompositeLit | FunctionLit 
Operand    = GoLiteral | QualifiedIdent | MethodExpr | LPAREN + Expression + RPAREN

#-------------------------------------------------------------------------
#  BuiltinArgs = Type [ "," ExpressionList ] | ExpressionList .
#  BuiltinCall = identifier "(" [ BuiltinArgs ] ")" .
BuiltinArgs = Type + Optional( COMMA + ExpressionList) | ExpressionList
BuiltinCall = identifier + LPAREN + Optional( BuiltinArgs ) + RPAREN

#-------------------------------------------------------------------------
#  PrimaryExpr =         
#          Operand |
#          Conversion |
#          BuiltinCall |
#          PrimaryExpr Selector |
#          PrimaryExpr Index |
#          PrimaryExpr Slice |
#          PrimaryExpr TypeAssertion |
#          PrimaryExpr Call .

PrimaryExpr << ( Operand |
                Conversion |
                BuiltinCall |
                PrimaryExpr + Selector |
                PrimaryExpr + Index |
                PrimaryExpr + Slice |
                PrimaryExpr + TypeAssertion |
                PrimaryExpr + Call )


#-------------------------------------------------------------------------
#  ImportPath       = string_lit .
#  ImportSpec       = [ "." | PackageName ] ImportPath .
#  ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
ImportPath       = string_lit 
ImportSpec       = Optional(DOT + PackageName) + ImportPath 
ImportDecl       = Literal("import") + Group( ImportSpec | (LPAREN + ZeroOrMore( ImportSpec + SEMI ) + RPAREN) )

#testParse("ImportSpec", ['. "asdf"'])
#testParse("ImportDecl", ['import . "asdf"'])

#-------------------------------------------------------------------------
#  PackageName    = identifier .
#  PackageClause  = "package" PackageName .
#  SourceFile     = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
PackageName    = identifier 
PackageClause  = Literal("package") + PackageName 
SourceFile     = PackageClause + SEMI + ZeroOrMore( ImportDecl + SEMI ) + ZeroOrMore( TopLevelDecl + SEMI )


testParse("PackageClause", ["package asdf"])


example ="""
package main 

import . "fmt";

func main(){
    Printf("ASDF");
}
"""
print SourceFile.parseString(example)
