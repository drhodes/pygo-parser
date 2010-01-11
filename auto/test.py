from pyparsing import *

unicode_char = Forward()
unicode_letter = Forward()
unicode_digit = Forward()
letter = Forward()
decimal_digit = Forward()
octal_digit = Forward()
hex_digit = Forward()
identifier = Forward()
int_lit = Forward()
decimal_lit = Forward()
octal_lit = Forward()
hex_lit = Forward()
float_lit = Forward()
decimals = Forward()
exponent = Forward()
char_lit = Forward()
unicode_value = Forward()
byte_value = Forward()
octal_byte_value = Forward()
hex_byte_value = Forward()
little_u_value = Forward()
big_u_value = Forward()
escaped_char = Forward()
string_lit = Forward()
raw_string_lit = Forward()
interpreted_string_lit = Forward()
Type = Forward()
TypeName = Forward()
TypeLit = Forward()
ArrayType = Forward()
ArrayLength = Forward()
ElementType = Forward()
SliceType = Forward()
StructType = Forward()
FieldDecl = Forward()
AnonymousField = Forward()
Tag = Forward()
PointerType = Forward()
BaseType = Forward()
FunctionType = Forward()
Signature = Forward()
Result = Forward()
Parameters = Forward()
ParameterList = Forward()
ParameterDecl = Forward()
InterfaceType = Forward()
MethodSpec = Forward()
MethodName = Forward()
InterfaceTypeName = Forward()
MapType = Forward()
KeyType = Forward()
ChannelType = Forward()
Channel = Forward()
SendChannel = Forward()
RecvChannel = Forward()
Block = Forward()
Declaration = Forward()
TopLevelDecl = Forward()
ConstDecl = Forward()
ConstSpec = Forward()
IdentifierList = Forward()
ExpressionList = Forward()
TypeDecl = Forward()
TypeSpec = Forward()
VarDecl = Forward()
VarSpec = Forward()
ShortVarDecl = Forward()
FunctionDecl = Forward()
Body = Forward()
MethodDecl = Forward()
Receiver = Forward()
BaseTypeName = Forward()
Operand = Forward()
Literal = Forward()
BasicLit = Forward()
QualifiedIdent = Forward()
CompositeLit = Forward()
LiteralType = Forward()
ElementList = Forward()
Element = Forward()
Key = Forward()
FieldName = Forward()
ElementIndex = Forward()
Value = Forward()
FunctionLit = Forward()
PrimaryExpr = Forward()
Selector = Forward()
Index = Forward()
Slice = Forward()
TypeAssertion = Forward()
Call = Forward()
Expression = Forward()
UnaryExpr = Forward()
binary_op = Forward()
log_op = Forward()
com_op = Forward()
rel_op = Forward()
add_op = Forward()
mul_op = Forward()
unary_op = Forward()
MethodExpr = Forward()
ReceiverType = Forward()
Conversion = Forward()
Statement = Forward()
SimpleStmt = Forward()
EmptyStmt = Forward()
LabeledStmt = Forward()
Label = Forward()
ExpressionStmt = Forward()
IncDecStmt = Forward()
Assignment = Forward()
assign_op = Forward()
IfStmt = Forward()
SwitchStmt = Forward()
ExprSwitchStmt = Forward()
ExprCaseClause = Forward()
ExprSwitchCase = Forward()
TypeSwitchStmt = Forward()
TypeSwitchGuard = Forward()
TypeCaseClause = Forward()
TypeSwitchCase = Forward()
TypeList = Forward()
ForStmt = Forward()
Condition = Forward()
ForClause = Forward()
InitStmt = Forward()
PostStmt = Forward()
RangeClause = Forward()
GoStmt = Forward()
SelectStmt = Forward()
CommClause = Forward()
CommCase = Forward()
SendExpr = Forward()
RecvExpr = Forward()
ReturnStmt = Forward()
BreakStmt = Forward()
ContinueStmt = Forward()
GotoStmt = Forward()
FallthroughStmt = Forward()
DeferStmt = Forward()
BuiltinCall = Forward()
BuiltinArgs = Forward()
SourceFile = Forward()
PackageClause = Forward()
PackageName = Forward()
ImportDecl = Forward()
ImportSpec = Forward()
ImportPath = Forward()
#--------------------------------------------------------------------------------
unicode_char << ( Literal("a") | Literal("b") | Literal("c") | Literal("d") | Literal("e") | Literal("f") | Literal("g") | Literal("h") | Literal("i") | Literal("j") | Literal("k") | Literal("l") | Literal("m") | Literal("n") | Literal("o") | Literal("p") | Literal("q") | Literal("r") | Literal("s") | Literal("t") | Literal("u") | Literal("v") | Literal("w") | Literal("x") | Literal("y") | Literal("z") | Literal("A") | Literal("B") | Literal("C") | Literal("D") | Literal("E") | Literal("F") | Literal("G") | Literal("H") | Literal("I") | Literal("J") | Literal("K") | Literal("L") | Literal("M") | Literal("N") | Literal("O") | Literal("P") | Literal("Q") | Literal("R") | Literal("S") | Literal("T") | Literal("U") | Literal("V") | Literal("W") | Literal("X") | Literal("Y") | Literal("Z") | Literal("0") | Literal("1") | Literal("2") | Literal("3") | Literal("4") | Literal("5") | Literal("6") | Literal("7") | Literal("8") | Literal("9") )

#--------------------------------------------------------------------------------
unicode_letter << ( Literal("a") | Literal("b") | Literal("c") | Literal("d") | Literal("e") | Literal("f") | Literal("g") | Literal("h") | Literal("i") | Literal("j") | Literal("k") | Literal("l") | Literal("m") | Literal("n") | Literal("o") | Literal("p") | Literal("q") | Literal("r") | Literal("s") | Literal("t") | Literal("u") | Literal("v") | Literal("w") | Literal("x") | Literal("y") | Literal("z") | Literal("A") | Literal("B") | Literal("C") | Literal("D") | Literal("E") | Literal("F") | Literal("G") | Literal("H") | Literal("I") | Literal("J") | Literal("K") | Literal("L") | Literal("M") | Literal("N") | Literal("O") | Literal("P") | Literal("Q") | Literal("R") | Literal("S") | Literal("T") | Literal("U") | Literal("V") | Literal("W") | Literal("X") | Literal("Y") | Literal("Z") )

#--------------------------------------------------------------------------------
unicode_digit << ( Literal("0") | Literal("1") | Literal("2") | Literal("3") | Literal("4") | Literal("5") | Literal("6") | Literal("7") | Literal("8") | Literal("9") )

#--------------------------------------------------------------------------------
letter << ( unicode_letter | Literal("_") )

#--------------------------------------------------------------------------------
decimal_digit << ( Literal("0") | Literal("1") | Literal("2") | Literal("3") | Literal("4") | Literal("5") | Literal("6") | Literal("7") | Literal("8") | Literal("9") )

#--------------------------------------------------------------------------------
octal_digit << ( Literal("0") | Literal("1") | Literal("2") | Literal("3") | Literal("4") | Literal("5") | Literal("6") | Literal("7") )

#--------------------------------------------------------------------------------
hex_digit << ( Literal("0") | Literal("1") | Literal("2") | Literal("3") | Literal("4") | Literal("5") | Literal("6") | Literal("7") | Literal("8") | Literal("9") | Literal("a") | Literal("b") | Literal("c") | Literal("d") | Literal("e") | Literal("f") | Literal("A") | Literal("B") | Literal("C") | Literal("D") | Literal("E") | Literal("F") )

#--------------------------------------------------------------------------------
identifier << ( letter + ZeroOrMore( letter | unicode_digit ) )

#--------------------------------------------------------------------------------
int_lit << ( decimal_lit | octal_lit | hex_lit )

#--------------------------------------------------------------------------------
decimal_lit << ( ( Literal("1") | Literal("2") | Literal("3") | Literal("4") | Literal("5") | Literal("6") | Literal("7") | Literal("8") | Literal("9") ) + ZeroOrMore( decimal_digit ) )

#--------------------------------------------------------------------------------
octal_lit << ( Literal("0") + ZeroOrMore( octal_digit ) )

#--------------------------------------------------------------------------------
hex_lit << ( Literal("0") + ( Literal("x") | Literal("X") ) + hex_digit + ZeroOrMore( hex_digit ) )

#--------------------------------------------------------------------------------
float_lit << ( decimals + Literal(".") + Optional( decimals ) + Optional( exponent ) | decimals + exponent | Literal(".") + decimals + Optional( exponent ) )

#--------------------------------------------------------------------------------
decimals << ( decimal_digit + ZeroOrMore( decimal_digit ) )

#--------------------------------------------------------------------------------
exponent << ( ( Literal("e") | Literal("E") ) + Optional( Literal("+") | Literal("-") ) + decimals )

#--------------------------------------------------------------------------------
char_lit << ( Literal("\'") + ( unicode_value | byte_value ) + Literal("\'") )

#--------------------------------------------------------------------------------
unicode_value << ( unicode_char | little_u_value | big_u_value | escaped_char )

#--------------------------------------------------------------------------------
byte_value << ( octal_byte_value | hex_byte_value )

#--------------------------------------------------------------------------------
octal_byte_value << ( Literal("\\") + octal_digit + octal_digit + octal_digit )

#--------------------------------------------------------------------------------
hex_byte_value << ( Literal("\\") + Literal("x") + hex_digit + hex_digit )

#--------------------------------------------------------------------------------
little_u_value << ( Literal("\\") + Literal("u") + hex_digit + hex_digit + hex_digit + hex_digit )

#--------------------------------------------------------------------------------
big_u_value << ( Literal("\\") + Literal("U") + hex_digit + hex_digit + hex_digit + hex_digit + hex_digit + hex_digit + hex_digit + hex_digit )

#--------------------------------------------------------------------------------
escaped_char << ( Literal("\\") + ( Literal("a") | Literal("b") | Literal("f") | Literal("n") | Literal("r") | Literal("t") | Literal("v") | Literal("\\") | Literal("\'") | Literal('`"`') ) )

#--------------------------------------------------------------------------------
string_lit << ( raw_string_lit | interpreted_string_lit )

#--------------------------------------------------------------------------------
raw_string_lit << ( Literal("`") + ZeroOrMore( unicode_char ) + Literal("`") )

#--------------------------------------------------------------------------------
interpreted_string_lit << ( Literal(`"`) + ZeroOrMore( unicode_value | byte_value ) + Literal(`"`) )

#--------------------------------------------------------------------------------
Type << ( TypeName | TypeLit | Literal("(") + Type + Literal(")") )

#--------------------------------------------------------------------------------
TypeName << ( QualifiedIdent )

#--------------------------------------------------------------------------------
TypeLit << ( ArrayType | StructType | PointerType | FunctionType | InterfaceType | SliceType | MapType | ChannelType )

#--------------------------------------------------------------------------------
ArrayType << ( Literal("[") + ArrayLength + Literal("]") + ElementType )

#--------------------------------------------------------------------------------
ArrayLength << ( Expression )

#--------------------------------------------------------------------------------
ElementType << ( Type )

#--------------------------------------------------------------------------------
SliceType << ( Literal("[") + Literal("]") + ElementType )

#--------------------------------------------------------------------------------
StructType << ( Literal("struct") + Literal("{") + ZeroOrMore( FieldDecl + Literal(";") ) + Literal("}") )

#--------------------------------------------------------------------------------
FieldDecl << ( ( IdentifierList + Type | AnonymousField ) + Optional( Tag ) )

#--------------------------------------------------------------------------------
AnonymousField << ( Optional( Literal("*") ) + TypeName )

#--------------------------------------------------------------------------------
Tag << ( string_lit )

#--------------------------------------------------------------------------------
PointerType << ( Literal("*") + BaseType )

#--------------------------------------------------------------------------------
BaseType << ( Type )

#--------------------------------------------------------------------------------
FunctionType << ( Literal("func") + Signature )

#--------------------------------------------------------------------------------
Signature << ( Parameters + Optional( Result ) )

#--------------------------------------------------------------------------------
Result << ( Parameters | Type )

#--------------------------------------------------------------------------------
Parameters << ( Literal("(") + Optional( ParameterList + Optional( Literal(",") ) ) + Literal(")") )

#--------------------------------------------------------------------------------
ParameterList << ( ParameterDecl + ZeroOrMore( Literal(",") + ParameterDecl ) )

#--------------------------------------------------------------------------------
ParameterDecl << ( Optional( IdentifierList ) + ( Type | Literal("...") ) )

#--------------------------------------------------------------------------------
InterfaceType << ( Literal("interface") + Literal("{") + ZeroOrMore( MethodSpec + Literal(";") ) + Literal("}") )

#--------------------------------------------------------------------------------
MethodSpec << ( MethodName + Signature | InterfaceTypeName )

#--------------------------------------------------------------------------------
MethodName << ( identifier )

#--------------------------------------------------------------------------------
InterfaceTypeName << ( TypeName )

#--------------------------------------------------------------------------------
MapType << ( Literal("map") + Literal("[") + KeyType + Literal("]") + ElementType )

#--------------------------------------------------------------------------------
KeyType << ( Type )

#--------------------------------------------------------------------------------
ChannelType << ( Channel | SendChannel | RecvChannel )

#--------------------------------------------------------------------------------
Channel << ( Literal("chan") + ElementType )

#--------------------------------------------------------------------------------
SendChannel << ( Literal("chan") + Literal("<-") + ElementType )

#--------------------------------------------------------------------------------
RecvChannel << ( Literal("<-") + Literal("chan") + ElementType )

#--------------------------------------------------------------------------------
Block << ( Literal("{") + ZeroOrMore( Statement + Literal(";") ) + Literal("}") )

#--------------------------------------------------------------------------------
Declaration << ( ConstDecl | TypeDecl | VarDecl )

#--------------------------------------------------------------------------------
TopLevelDecl << ( Declaration | FunctionDecl | MethodDecl )

#--------------------------------------------------------------------------------
ConstDecl << ( Literal("const") + ( ConstSpec | Literal("(") + ZeroOrMore( ConstSpec + Literal(";") ) + Literal(")") ) )

#--------------------------------------------------------------------------------
ConstSpec << ( IdentifierList + Optional( Optional( Type ) + Literal("=") + ExpressionList ) )

#--------------------------------------------------------------------------------
IdentifierList << ( identifier + ZeroOrMore( Literal(",") + identifier ) )

#--------------------------------------------------------------------------------
ExpressionList << ( Expression + ZeroOrMore( Literal(",") + Expression ) )

#--------------------------------------------------------------------------------
TypeDecl << ( Literal("type") + ( TypeSpec | Literal("(") + ZeroOrMore( TypeSpec + Literal(";") ) + Literal(")") ) )

#--------------------------------------------------------------------------------
TypeSpec << ( identifier + Type )

#--------------------------------------------------------------------------------
VarDecl << ( Literal("var") + ( VarSpec | Literal("(") + ZeroOrMore( VarSpec + Literal(";") ) + Literal(")") ) )

#--------------------------------------------------------------------------------
VarSpec << ( IdentifierList + ( Type + Optional( Literal("=") + ExpressionList ) | Literal("=") + ExpressionList ) )

#--------------------------------------------------------------------------------
ShortVarDecl << ( IdentifierList + Literal(":=") + ExpressionList )

#--------------------------------------------------------------------------------
FunctionDecl << ( Literal("func") + identifier + Signature + Optional( Body ) )

#--------------------------------------------------------------------------------
Body << ( Block )

#--------------------------------------------------------------------------------
MethodDecl << ( Literal("func") + Receiver + MethodName + Signature + Optional( Body ) )

#--------------------------------------------------------------------------------
Receiver << ( Literal("(") + Optional( identifier ) + Optional( Literal("*") ) + BaseTypeName + Literal(")") )

#--------------------------------------------------------------------------------
BaseTypeName << ( identifier )

#--------------------------------------------------------------------------------
Operand << ( Literal | QualifiedIdent | MethodExpr | Literal("(") + Expression + Literal(")") )

#--------------------------------------------------------------------------------
Literal << ( BasicLit | CompositeLit | FunctionLit )

#--------------------------------------------------------------------------------
BasicLit << ( int_lit | float_lit | char_lit | string_lit )

#--------------------------------------------------------------------------------
QualifiedIdent << ( Optional( PackageName + Literal(".") ) + identifier )

#--------------------------------------------------------------------------------
CompositeLit << ( LiteralType + Literal("{") + Optional( ElementList + Optional( Literal(",") ) ) + Literal("}") )

#--------------------------------------------------------------------------------
LiteralType << ( StructType | ArrayType | Literal("[") + Literal("...") + Literal("]") + ElementType | SliceType | MapType | TypeName | Literal("(") + LiteralType + Literal(")") )

#--------------------------------------------------------------------------------
ElementList << ( Element + ZeroOrMore( Literal(",") + Element ) )

#--------------------------------------------------------------------------------
Element << ( Optional( Key + Literal(":") ) + Value )

#--------------------------------------------------------------------------------
Key << ( FieldName | ElementIndex )

#--------------------------------------------------------------------------------
FieldName << ( identifier )

#--------------------------------------------------------------------------------
ElementIndex << ( Expression )

#--------------------------------------------------------------------------------
Value << ( Expression )

#--------------------------------------------------------------------------------
FunctionLit << ( FunctionType + Body )

#--------------------------------------------------------------------------------
PrimaryExpr << ( Operand | Conversion | BuiltinCall | PrimaryExpr + Selector | PrimaryExpr + Index | PrimaryExpr + Slice | PrimaryExpr + TypeAssertion | PrimaryExpr + Call )

#--------------------------------------------------------------------------------
Selector << ( Literal(".") + identifier )

#--------------------------------------------------------------------------------
Index << ( Literal("[") + Expression + Literal("]") )

#--------------------------------------------------------------------------------
Slice << ( Literal("[") + Expression + Literal(":") + Optional( Expression ) + Literal("]") )

#--------------------------------------------------------------------------------
TypeAssertion << ( Literal(".") + Literal("(") + Type + Literal(")") )

#--------------------------------------------------------------------------------
Call << ( Literal("(") + Optional( ExpressionList + Optional( Literal(",") ) ) + Literal(")") )

#--------------------------------------------------------------------------------
Expression << ( UnaryExpr | Expression + binary_op + UnaryExpr )

#--------------------------------------------------------------------------------
UnaryExpr << ( PrimaryExpr | unary_op + UnaryExpr )

#--------------------------------------------------------------------------------
binary_op << ( log_op | com_op | rel_op | add_op | mul_op )

#--------------------------------------------------------------------------------
log_op << ( Literal("||") | Literal("&&") )

#--------------------------------------------------------------------------------
com_op << ( Literal("<-") )

#--------------------------------------------------------------------------------
rel_op << ( Literal("==") | Literal("!=") | Literal("<") | Literal("<=") | Literal(">") | Literal(">=") )

#--------------------------------------------------------------------------------
add_op << ( Literal("+") | Literal("-") | Literal("|") | Literal("^") )

#--------------------------------------------------------------------------------
mul_op << ( Literal("*") | Literal("/") | Literal("%") | Literal("<<") | Literal(">>") | Literal("&") | Literal("&^") )

#--------------------------------------------------------------------------------
unary_op << ( Literal("+") | Literal("-") | Literal("!") | Literal("^") | Literal("*") | Literal("&") | Literal("<-") )

#--------------------------------------------------------------------------------
MethodExpr << ( ReceiverType + Literal(".") + MethodName )

#--------------------------------------------------------------------------------
ReceiverType << ( TypeName | Literal("(") + Literal("*") + TypeName + Literal(")") )

#--------------------------------------------------------------------------------
Conversion << ( LiteralType + Literal("(") + Expression + Literal(")") )

#--------------------------------------------------------------------------------
Statement << ( Declaration | LabeledStmt | SimpleStmt | GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt | FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt | DeferStmt )

#--------------------------------------------------------------------------------
SimpleStmt << ( EmptyStmt | ExpressionStmt | IncDecStmt | Assignment | ShortVarDecl )

#--------------------------------------------------------------------------------
EmptyStmt << ( Word(""))

#--------------------------------------------------------------------------------
LabeledStmt << ( Label + Literal(":") + Statement )

#--------------------------------------------------------------------------------
Label << ( identifier )

#--------------------------------------------------------------------------------
ExpressionStmt << ( Expression )

#--------------------------------------------------------------------------------
IncDecStmt << ( Expression + ( Literal("++") | Literal("--") ) )

#--------------------------------------------------------------------------------
Assignment << ( ExpressionList + assign_op + ExpressionList )

#--------------------------------------------------------------------------------
assign_op << ( Optional( add_op | mul_op ) + Literal("=") )

#--------------------------------------------------------------------------------
IfStmt << ( Literal("if") + Optional( SimpleStmt + Literal(";") ) + Optional( Expression ) + Block + Optional( Literal("else") + Statement ) )

#--------------------------------------------------------------------------------
SwitchStmt << ( ExprSwitchStmt | TypeSwitchStmt )

#--------------------------------------------------------------------------------
ExprSwitchStmt << ( Literal("switch") + Optional( SimpleStmt + Literal(";") ) + Optional( Expression ) + Literal("{") + ZeroOrMore( ExprCaseClause ) + Literal("}") )

#--------------------------------------------------------------------------------
ExprCaseClause << ( ExprSwitchCase + Literal(":") + ZeroOrMore( Statement + Literal(";") ) )

#--------------------------------------------------------------------------------
ExprSwitchCase << ( Literal("case") + ExpressionList | Literal("default") )

#--------------------------------------------------------------------------------
TypeSwitchStmt << ( Literal("switch") + Optional( SimpleStmt + Literal(";") ) + TypeSwitchGuard + Literal("{") + ZeroOrMore( TypeCaseClause ) + Literal("}") )

#--------------------------------------------------------------------------------
TypeSwitchGuard << ( Optional( identifier + Literal(":=") ) + Expression + Literal(".") + Literal("(") + Literal("type") + Literal(")") )

#--------------------------------------------------------------------------------
TypeCaseClause << ( TypeSwitchCase + Literal(":") + ZeroOrMore( Statement + Literal(";") ) )

#--------------------------------------------------------------------------------
TypeSwitchCase << ( Literal("case") + TypeList | Literal("default") )

#--------------------------------------------------------------------------------
TypeList << ( Type + ZeroOrMore( Literal(",") + Type ) )

#--------------------------------------------------------------------------------
ForStmt << ( Literal("for") + Optional( Condition | ForClause | RangeClause ) + Block )

#--------------------------------------------------------------------------------
Condition << ( Expression )

#--------------------------------------------------------------------------------
ForClause << ( Optional( InitStmt ) + Literal(";") + Optional( Condition ) + Literal(";") + Optional( PostStmt ) )

#--------------------------------------------------------------------------------
InitStmt << ( SimpleStmt )

#--------------------------------------------------------------------------------
PostStmt << ( SimpleStmt )

#--------------------------------------------------------------------------------
RangeClause << ( ExpressionList + ( Literal("=") | Literal(":=") ) + Literal("range") + Expression )

#--------------------------------------------------------------------------------
GoStmt << ( Literal("go") + Expression )

#--------------------------------------------------------------------------------
SelectStmt << ( Literal("select") + Literal("{") + ZeroOrMore( CommClause ) + Literal("}") )

#--------------------------------------------------------------------------------
CommClause << ( CommCase + Literal(":") + ZeroOrMore( Statement + Literal(";") ) )

#--------------------------------------------------------------------------------
CommCase << ( Literal("case") + ( SendExpr | RecvExpr ) | Literal("default") )

#--------------------------------------------------------------------------------
SendExpr << ( Expression + Literal("<-") + Expression )

#--------------------------------------------------------------------------------
RecvExpr << ( Optional( Expression + ( Literal("=") | Literal(":=") ) ) + Literal("<-") + Expression )

#--------------------------------------------------------------------------------
ReturnStmt << ( Literal("return") + Optional( ExpressionList ) )

#--------------------------------------------------------------------------------
BreakStmt << ( Literal("break") + Optional( Label ) )

#--------------------------------------------------------------------------------
ContinueStmt << ( Literal("continue") + Optional( Label ) )

#--------------------------------------------------------------------------------
GotoStmt << ( Literal("goto") + Label )

#--------------------------------------------------------------------------------
FallthroughStmt << ( Literal("fallthrough") )

#--------------------------------------------------------------------------------
DeferStmt << ( Literal("defer") + Expression )

#--------------------------------------------------------------------------------
BuiltinCall << ( identifier + Literal("(") + Optional( BuiltinArgs ) + Literal(")") )

#--------------------------------------------------------------------------------
BuiltinArgs << ( Type + Optional( Literal(",") + ExpressionList ) | ExpressionList )

#--------------------------------------------------------------------------------
SourceFile << ( PackageClause + Literal(";") + ZeroOrMore( ImportDecl + Literal(";") ) + ZeroOrMore( TopLevelDecl + Literal(";") ) )

#--------------------------------------------------------------------------------
PackageClause << ( Literal("package") + PackageName )

#--------------------------------------------------------------------------------
PackageName << ( identifier )

#--------------------------------------------------------------------------------
ImportDecl << ( Literal("import") + ( ImportSpec | Literal("(") + ZeroOrMore( ImportSpec + Literal(";") ) + Literal(")") ) )

#--------------------------------------------------------------------------------
ImportSpec << ( Optional( Literal(".") | PackageName ) + ImportPath )

#--------------------------------------------------------------------------------
ImportPath << ( string_lit )

import sys
sys.setrecursionlimit(15000)
SourceFile.parseString("package main;")
