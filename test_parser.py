from go_parser import *

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

#  unicode_char   = /* an arbitrary Unicode code point */ .
#  unicode_letter = /* a Unicode code point classified as "Letter" */ .
#  unicode_digit  = /* a Unicode code point classified as "Digit" */ .

#  letter        = unicode_letter | "_" .
#  decimal_digit = "0" ... "9" .
#  octal_digit   = "0" ... "7" .
#  hex_digit     = "0" ... "9" | "A" ... "F" | "a" ... "f" .

#  identifier = letter { letter | unicode_digit } .

#-------------------------------------------------------------------------------
#  hex_lit     = "0" ( "x" | "X" ) hex_digit { hex_digit } .
#  decimal_lit = ( "1" ... "9" ) { decimal_digit } .
#  octal_lit   = "0" { octal_digit } .
#  int_lit     = decimal_lit | octal_lit | hex_lit .

testParse("int_lit", ["0x345", "0345", "345"])

#-------------------------------------------------------------------------------
#  float_lit = decimals "." [ decimals ] [ exponent ] |
#              decimals exponent |
#              "." decimals [ exponent ] .
testParse("float_lit", ["2.34e34",
                        "0234e-34",
                        "0234e+34"])


#  decimals  = decimal_digit { decimal_digit } .
#  exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals .








#-------------------------------------------------------------------------------
#  octal_byte_value = `\` octal_digit octal_digit octal_digit .
testParse('octal_byte_value', ["""\\123"""])

#-------------------------------------------------------------------------------
#  hex_byte_value   = `\` "x" hex_digit hex_digit .
testParse('hex_byte_value', ["""\\x00"""])

#-------------------------------------------------------------------------------
#  little_u_value   = `\` "u" hex_digit hex_digit hex_digit hex_digit .
testParse('little_u_value', ["""\\u1234"""])

#-------------------------------------------------------------------------------
#  big_u_value      = `\` "U" hex_digit hex_digit hex_digit hex_digit
#                             hex_digit hex_digit hex_digit hex_digit .
testParse('big_u_value', ["""\\U12345678"""])

#-------------------------------------------------------------------------------
#  byte_value       = octal_byte_value | hex_byte_value .
testParse("byte_value", ["""\\111""",
                         """\\x12"""
                         ])

#-------------------------------------------------------------------------------
#  unicode_value    = unicode_char | little_u_value | big_u_value | escaped_char .
testParse("unicode_value", ["\\u1111",
                            "\\U11112222",
                            ])

#-------------------------------------------------------------------------------
#  char_lit         = "'" ( unicode_value | byte_value ) "'" .
testParse("char_lit", [""" '\\x12' """,
                       """ '\\x11' """,
                       """ '\\x12' """,
                       """ '\\u1111' """,
                       """ '\\U11112222' """,                  
                       """ 'j' """,
                       ])


#-------------------------------------------------------------------------------
#  escaped_char     = `\` ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | `\` | "'" | `"` ) .
testParse("escaped_char", [r'\n',
                           r'\a',
                           r'\"',
                           r"\'",
                           r'\\',
                           ])

#-------------------------------------------------------------------------------
#  raw_string_lit         = "`" { unicode_char } "`" .
testParse("raw_string_lit", [" `a` ",
                             " `asd123f` ",
                             ])

#  interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
testParse("interpreted_string_lit", [' "000000" ' ])

#  string_lit             = raw_string_lit | interpreted_string_lit .
testParse("string_lit", [' "000000" ',
                         ' `_123_as` ',
                         ])



#  TypeName  = QualifiedIdent.
#  TypeLit   = ArrayType | StructType | PointerType | FunctionType | InterfaceType |
#              SliceType | MapType | ChannelType .


#  Type      = TypeName | TypeLit | "(" Type ")" .
testParse("Type", [' int  ',
                   ' (float) ',
                   ])

#  ArrayType   = "[" ArrayLength "]" ElementType .
#  RuntimeError: maximum recursion depth exceeded
#testParse("ArrayType", [" [2]int "]) 


#  ArrayLength = Expression .
#  ElementType = Type .

#  SliceType = "[" "]" ElementType .

#  StructType     = "struct" "{" { FieldDecl ";" } "}" .
#  FieldDecl      = (IdentifierList Type | AnonymousField) [ Tag ] .
#  AnonymousField = [ "*" ] TypeName .
#  Tag            = string_lit .

#  PointerType = "*" BaseType .
testParse( "PointerType", ["*int"])

#  BaseType = Type .
testParse( "BaseType", ["int"])

#  FunctionType   = "func" Signature .
#  Signature      = Parameters [ Result ] .
#  Result         = Parameters | Type .
#  Parameters     = "(" [ ParameterList [ "," ] ] ")" .
#  ParameterList  = ParameterDecl { "," ParameterDecl } .
testParse( "ParameterList", [ "as, ba sdf",
                              "x, y int",
                              "as, ba sdf, as, ba sdf ",
                              ])

#  ParameterDecl  = [ IdentifierList ] ( Type | "..." ) .
testParse( "ParameterDecl", ["int, atf sdf",
                             "int ..."
                             ])


#  InterfaceType      = "interface" "{" { MethodSpec ";" } "}" .
#  MethodSpec         = MethodName Signature | InterfaceTypeName .
#  MethodName         = identifier .
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

#  ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
#  ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .

#  IdentifierList = identifier { "," identifier } .
testParse("IdentifierList", ["int, int, int"])


#  ExpressionList = Expression { "," Expression } .

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

#  Operand    = Literal | QualifiedIdent | MethodExpr | "(" Expression ")" .
#  Literal    = BasicLit | CompositeLit | FunctionLit .
#  BasicLit   = int_lit | float_lit | char_lit | string_lit .

#  QualifiedIdent = [ PackageName "." ] identifier .
'''
testParse("QualifiedIdent", ["asdf_asdasdf",
                             "asdf",
                             ])
'''

#  CompositeLit  = LiteralType "{" [ ElementList [ "," ] ] "}" .
#  LiteralType   = StructType | ArrayType | "[" "..." "]" ElementType |
#                  SliceType | MapType | TypeName | "(" LiteralType ")" .
#  ElementList   = Element { "," Element } .
#  Element       = [ Key ":" ] Value .
#  Key           = FieldName | ElementIndex .
#  FieldName     = identifier .
#  ElementIndex  = Expression .
#  Value         = Expression .

#  FunctionLit = FunctionType Body .

#  PrimaryExpr =
#          Operand |
#          Conversion |
#          BuiltinCall |
#          PrimaryExpr Selector |
#          PrimaryExpr Index |
#          PrimaryExpr Slice |
#          PrimaryExpr TypeAssertion |
#          PrimaryExpr Call .

#  Selector       = "." identifier .
#  Index          = "[" Expression "]" .
#  Slice          = "[" Expression ":" [ Expression ] "]" .
#  TypeAssertion  = "." "(" Type ")" .
#  Call           = "(" [ ExpressionList [ "," ] ] ")" .

#  Expression = UnaryExpr | Expression binary_op UnaryExpr .
#  UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .

#  binary_op  = log_op | com_op | rel_op | add_op | mul_op .
#  log_op     = "||" | "&&" .
#  com_op     = "<-" .
#  rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
#  add_op     = "+" | "-" | "|" | "^" .
#  mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .

#  unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .

#  MethodExpr    = ReceiverType "." MethodName .
#  ReceiverType  = TypeName | "(" "*" TypeName ")" .

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
example ="""
package main 

import . "fmt";

func main(){
    Printf("ASDF");
}
"""
#print SourceFile.parseString(example)



#  PackageClause  = "package" PackageName .
testParse("PackageClause", ["package foo"])

#  PackageName    = identifier .

#  ImportDecl       = "import" ( ImportSpec | "(" { ImportSpec ";" } ")" ) .
#  ImportSpec       = [ "." | PackageName ] ImportPath .
#  ImportPath       = string_lit .


