ifExample = """
ifStmt ::= ( <token 'if'> <expr> <token 'then'> <stmt> <token 'else'> <stmt>
           | <token 'if'> <expr> <token 'then'> <stmt>)

expr ::= <spaces> <letter>
stmt ::= <ifStmt> | <expr>
"""


