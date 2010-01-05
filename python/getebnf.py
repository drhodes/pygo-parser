text = open("./go_spec.html").read()


record = False
for line in text.split('\n'):

    if line.startswith('<pre class="ebnf">'):
        record = True
    if line.startswith('</pre>'):
        if record == True:
            print "</pre>"
        record = False
        

    if record == True:
        print line
                       
