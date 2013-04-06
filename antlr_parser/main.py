#!/usr/bin/python
import antlr
import sys
import sl_parser
import sl_lexer

def visit1(node, i=""):
    if not node:
	print i, "nil"
	return

    c = node.getType()
    c = sl_parser._tokenNames[c]
    
    t = node.getText()
    k = node.getFirstChild()
    s = node.getNextSibling()
    print i, "<", c, "> ",
    if t:
	print " `", t, "' ", 
    print ""
    if k: visit1(k, i+"  ")
    if s: visit1(s, i)
#    print ")", 

def visit(node):
    visit1(node);
    print ""

try:
    l = sl_lexer.Lexer()
    p = sl_parser.Parser(l)
    # p.setFilename('<stdin>')
    p.program()
    ast = p.getAST()
    
    print "V:"
    visit(ast)
    
    print "N: ", ast.toString()
    print "L: ", ast.toStringList()
    print "T: ", ast.toStringTree()
except antlr.TokenStreamException, e:
    sys.stderr.write('exception: ' + str(e) + '\n')
except antlr.RecognitionException, e:
    sys.stderr.write('exception: ' + str(e) + '\n')
