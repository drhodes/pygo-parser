from pyparsing import *


#==================================================================
# some utility functions

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
STAR = Literal( "*" )

#-------------------------------------------------------------------------
#  unicode_char   = /* an arbitrary Unicode code point */ .
#  unicode_letter = /* a Unicode code point classified as "Letter" */ .
#  unicode_digit  = /* a Unicode code point classified as "Digit" */ .

def literals_(xs): return Or(map(Literal, xs))

unicode_char   = alphanums
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

#-------------------------------------------------------------------------
log_op     = literals_(["||", "&&"])
com_op     = Literal("<-")
rel_op     = literals_([ "==", "!=", "<", "<=", ">", ">=" ])
add_op     = literals_([ "+", "-", "|", "^" ])
mul_op     = literals_([ "*", "/", "%", "<<", ">>", "&", "&^" ])
binary_op  = log_op | com_op | rel_op | add_op | mul_op 
unary_op   = literals_([ "+", "-", "!", "^", "*", "&", "<-" ])



#-------------------------------------------------------------------------
BasicLit   = int_lit | float_lit | char_lit | string_lit

#-------------------------------------------------------------------------
#  LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
#                  SliceType | MapType | TypeName | "(" LiteralType ")" .


#-------------------------------------------------------------------------
#  CompositeLit  = LiteralType "{" [ ElementList [ "," ] ] "}" .

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

UnaryExpr  = PrimaryExpr | unary_op + UnaryExpr 
Expression = UnaryExpr | Expression + binary_op + UnaryExpr 

#-------------------------------------------------------------------------
#  ExpressionList = Expression { "," Expression } .
ExpressionList = delimitedList( Expression )

#-------------------------------------------------------------------------
#  TypeName  = QualifiedIdent.
TypeName = QualifiedIdent

#  TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
#              SliceType | MapType | ChannelType .
#  Type      = TypeName | TypeLit | "(" Type ")" .

TypeLit   = Forward()
Type      = Forward()
Type     << (TypeName | TypeLit | LPAREN + Type + RPAREN)


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
ArrayType, StructType, PointerType, FunctionType, \
    InterfaceType, SliceType, MapType, ChannelType  = [Forward() for x in range(8)]

#-------------------------------------------------------------------------
#  ArrayType   = "[" ArrayLength "]" ElementType .
#  ArrayLength = Expression .
#  ElementType = Type .
ElementType = Type 
ArrayLength = Expression 
ArrayType   = LBRACKET + ArrayLength + RBRACKET + ElementType



#-------------------------------------------------------------------------
#SliceType = "[" "]" ElementType .
SliceType = LBRACKET + RBRACKET + ElementType


"""
#  AnonymousField = [ "*" ] TypeName .
#  FieldDecl      = (IdentifierList Type | AnonymousField) [ Tag ] .
#  StructType     = "struct" "{" { FieldDecl ";" } "}" .
#  Tag            = string_lit .



#  PointerType = "*" BaseType .
#  BaseType = Type .

#  FunctionType   = "func" Signature .
#  Signature      = Parameters [ Result ] .
#  Result         = Parameters | Type .
#  Parameters     = "(" [ ParameterList [ "," ] ] ")" .
#  ParameterList  = ParameterDecl { "," ParameterDecl } .
#  ParameterDecl  = [ IdentifierList ] ( Type | "..." ) .

#  InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
#  MethodSpec         = MethodName Signature | InterfaceTypeName .
#  InterfaceTypeName  = TypeName .

#  MapType     = "map" "[" KeyType "]" ElementType .
#  KeyType     = Type .

#  ChannelType   = Channel | SendChannel | RecvChannel .
#  Channel       = "chan" ElementType .
#  SendChannel   = "chan" "<-" ElementType .
#  RecvChannel   = "<-" "chan" ElementType .

#  Block = "{" { Statement ";" } "}" .

#  Declaration   = ConstDecl | TypeDecl | VarDecl .
#  TopLevelDecl  = Declaration | FunctionDecl | MethodDecl .


#  IdentifierList = identifier { "," identifier } .

#  ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
#  ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .

#  TypeDecl     = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
#  TypeSpec     = identifier Type .

#  VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
#  VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .

#  ShortVarDecl = IdentifierList ":=" ExpressionList .

#  FunctionDecl = "func" identifier Signature [ Body ] .
#  Body         = Block.

#  MethodDecl   = "func" Receiver MethodName Signature [ Body ] .
#  Receiver     = "(" [ identifier ] [ "*" ] BaseTypeName ")" .
#  BaseTypeName = identifier .





#  ElementList   = Element { "," Element } .
#  Element       = [ Key ":" ] Value .
#  Key           = FieldName | ElementIndex .
#  FieldName     = identifier .
#  ElementIndex  = Expression .
#  Value         = Expression .

#  FunctionLit = FunctionType Body .





#  Conversion = LiteralType "(" Expression ")" .

#  Statement =
#          Declaration | LabeledStmt | SimpleStmt |
#          GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
#          FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
#          DeferStmt .

#  SimpleStmt = EmptyStmt | ExpressionStmt | IncDecStmt | Assignment | ShortVarDecl .

#  EmptyStmt = .

#  LabeledStmt = Label ":" Statement .
#  Label       = identifier .

#  ExpressionStmt = Expression .

#  IncDecStmt = Expression ( "++" | "--" ) .

#  Assignment = ExpressionList assign_op ExpressionList .

#  assign_op = [ add_op | mul_op ] "=" .

#  IfStmt    = "if" [ SimpleStmt ";" ] [ Expression ] Block [ "else" Statement ] .

#  SwitchStmt = ExprSwitchStmt | TypeSwitchStmt .

#  ExprSwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
#  ExprCaseClause = ExprSwitchCase ":" { Statement ";" } .
#  ExprSwitchCase = "case" ExpressionList | "default" .

#  TypeSwitchStmt  = "switch" [ SimpleStmt ";" ] TypeSwitchGuard "{" { TypeCaseClause } "}" .
#  TypeSwitchGuard = [ identifier ":=" ] Expression "." "(" "type" ")" .
#  TypeCaseClause  = TypeSwitchCase ":" { Statement ";" } .
#  TypeSwitchCase  = "case" TypeList | "default" .
#  TypeList        = Type { "," Type } .

#  ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .
#  Condition = Expression .

#  ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] .
#  InitStmt = SimpleStmt .
#  PostStmt = SimpleStmt .

#  RangeClause = ExpressionList ( "=" | ":=" ) "range" Expression .

#  GoStmt = "go" Expression .

#  SelectStmt = "select" "{" { CommClause } "}" .
#  CommClause = CommCase ":" { Statement ";" } .
#  CommCase = "case" ( SendExpr | RecvExpr) | "default" .
#  SendExpr =  Expression "<-" Expression .
#  RecvExpr =  [ Expression ( "=" | ":=" ) ] "<-" Expression .

#  ReturnStmt = "return" [ ExpressionList ] .

#  BreakStmt = "break" [ Label ] .

#  ContinueStmt = "continue" [ Label ] .

#  GotoStmt = "goto" Label .

#  FallthroughStmt = "fallthrough" .

#  DeferStmt = "defer" Expression .

#  BuiltinCall = identifier "(" [ BuiltinArgs ] ")" .
#  BuiltinArgs = Type [ "," ExpressionList ] | ExpressionList .

#  SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .

#  PackageClause  = "package" PackageName .
#  PackageName    = identifier .

#  ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
#  ImportSpec       = [ "." | PackageName ] ImportPath .
#  ImportPath       = string_lit .
"""
