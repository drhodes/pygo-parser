from pyparsing import *

# This is the grammar for the version of ebnf the Go team uses
# this will eventually generate a parser :3  
#------------------------------------------------------------------
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

EQUAL = Suppress("=")
DOT = Literal(".")
PIPE = Literal("|")
DQUOTE = Literal('"')
TICK = Literal("`")
LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET, LANGLE, RANGLE = map(Suppress, "(){}[]<>")


#------------------------------------------------------------------
class TokenP(object):
    def __init__(self, rule):
        pass

token = (DQUOTE + Word(alphanums+"!#$%&'()*+,-./:;<=>@[]^_`{|}") + DQUOTE |
         TICK + Or(map(Literal, alphanums+"""_."'""" + "\\")) + TICK )
         
def tokenAction(result):
    return "Literal(" + ''.join(result) + ")"
token.setParseAction(tokenAction)

production_name = Word(alphas + "_")

#------------------------------------------------------------------
# Production  = production_name "=" Expression "." .
class ProductionP(object):
    def __init__(self, rule):
        self.name = rule.asList()[0]
        self.rule = rule
        print self.name, "= Forward()"
    def process(self):        
        print self.name, "<< (", self.rule[1:],  ")"

    def __repr__(self):
        return str(self.rule.asList())


Production << production_name + EQUAL + Expression + Suppress(DOT)
def productionAction(p):
    return ProductionP(p)
Production.setParseAction(productionAction)


#------------------------------------------------------------------
# Expression  = Alternative { "|" Alternative } .
# Expression << Alternative + ZeroOrMore(PIPE + Alternative) 
Expression << Alternative + ZeroOrMore(PIPE + Alternative) 
class ExpressionP(object):
    def __init__(self, rule):
        self.rule = rule

    def __repr__(self):
        return str(self.rule)
#Expression.setParseAction(ExpressionP)



#------------------------------------------------------------------
# Alternative = Term { Term } .
Alternative <<  ZeroOrMore(Term)
class AlternativeP(object):
    def __init__(self, rule):
        self.rule = rule

    def __repr__(self):
        return ' + '.join(map(str, self.rule.asList()))
Alternative.setParseAction(AlternativeP)



#------------------------------------------------------------------
# Term        = production_name | token [ "..." token ] | Group | Option | Repetition .
Term <<  (production_name |
          (token + Optional(Literal("...") + token)) |          
          Group |
          Option |
          Repetition)
class TermP(object):
    def __init__(self, rule):
        self.rule = rule

    def __repr__(self):
        return str(self.rule)
Term.setParseAction(TermP)



#------------------------------------------------------------------
# Group       = "(" Expression ")" .
Group << (LPAREN + Expression + RPAREN)
class GroupP(object):
    def __init__(self, rule):
        self.rule = rule

    def __repr__(self):
        return "( %s )" % (str(self.rule))
Group.setParseAction(GroupP)

#------------------------------------------------------------------
# Option      = "[" Expression "]" .
class OptionP(object):
    def __init__(self, rule):
        self.rule = rule

    def __repr__(self):
        return "Optional( %s )" % (str(self.rule))

Option << (LBRACKET + Expression + RBRACKET)
Option.setParseAction(OptionP)


#------------------------------------------------------------------
# Repetition  = "{" Expression "}" .
class RepetitionP(object):
    def __init__(self, rule):
        self.rule = rule

    def __repr__(self):
        return "ZeroOrMore( %s )" % (str(self.rule))

Repetition << (LBRACE + Expression + RBRACE)
Repetition.setParseAction(RepetitionP)



Grammar = ZeroOrMore(Production)

