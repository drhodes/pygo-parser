from autoparse import Grammar

temp = open('./go.ebnf').read()

print "from pyparsing import *"
print

G = Grammar.parseString(temp)



for prod in G.asList():
    print "#"+"-"*80
    print prod.process()

