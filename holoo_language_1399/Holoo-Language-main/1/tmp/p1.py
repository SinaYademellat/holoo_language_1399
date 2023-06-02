# (0) import 
import ply.lex as lex
import re

# (1) List of token Names. 

tokens = (
    'testTime',
    'keyword',
    'type',
    'Boolean',
    'MultiLineComments',
    'SingleLineComment',
    'const_real',
    'Scientific_notation',
    'const_char',
    'const_string',
    'const_long',
    'const_dec',
    'const_hex',
    'const_oct',
    'void',
    'id',
    'Equal',
    'NotEqual',
    'LessorEqual',
    'Lessthan',
    'Biggerthan',
    'Biggerorequal',
    'Assignment',
    'Not',
    'BitwiseNegation',            
    'ArithmeticAnd',
    'Logicaland',
    'ArithmeticOr',
    'LogicalOr',
    'Logical_Arithmetic_Xor',            
    'Production',
    'Add',
    'Increment',
    'Decrement',
    'SubAndUnaryMinus',            
    'Div',
    'Mod',
    'O_CurlyBrace',    
    'C_CurlyBrace',
    'O_Parenthesis',    
    'C_Parenthesis',
    'Dot',
    'Comma',
    'Colon',    
    'Semi_Colon',
    'O_Brace',    
    'C_Brace',
   
)
###########################################

t_Equal         =   r'==' 
t_NotEqual      =   r'!='
t_LessorEqual   =   r'<='
t_Lessthan      =   r'<'
t_Biggerthan    =   r'>'
t_Biggerorequal =   r'>='
t_Assignment    =   r'='
t_Not           =   r'!'
t_BitwiseNegation = r'~' 
t_ArithmeticAnd =   r'&'
t_Logicaland    =   r'&&'
t_ArithmeticOr  =   r'\|'
t_LogicalOr     =   r'\|\|'
t_Logical_Arithmetic_Xor = r'\^'
t_Production    =   r'\*'
t_Add           =   r'\+'
t_Increment     =   r'\+\+'
t_Decrement     =   r'--'
t_SubAndUnaryMinus = r'-'
t_Div           =   r'/'
t_Mod           =   r'%'
t_O_CurlyBrace  =   r'{'
t_C_CurlyBrace  =   r'}'
t_O_Parenthesis =   r'\('
t_C_Parenthesis =   r'\)'
t_Dot           =   r'\.'
t_Comma         =   r',' 
t_Colon         =   r':'
t_Semi_Colon    =   r';'        
t_O_Brace       =   r'\['
t_C_Brace       =   r'\]'
t_testTime      = r'(testTime)'
##########################################
def t_keyword(t):
    #r'(bool)|(break)|(case)|(char)|(const)|(continue)|(default)|(double)|(else)|(false)|(function)|(float)|(for)|(if)|(input)|(int)|(long)|(output)|(return)|(sizeof)|(string)|(switch)|(true)'
    r'(break)|(case)|(const)|(continue)|(default)|(else)|(function)|(for)|(if)|(input)|(output)|(return)|(sizeof)|(switch)'
    return t

def t_type(t):
    r'(bool)|(int)|(long)|(float)(char)|(double)|(string)'
    return t

def t_void(t):
    r'(void)'
    return t

def t_Boolean(t):

    r'(true)|(false)'

    if(t.value == 'true'):  #the value can be assigned to any Python object.
        t.value =(t.value,True)
    else:
         t.value =(t.value,False)
    return t 

def t_MultiLineComments(t):
    r'[/][@](.|[\n])*[@][/]'
    # No return value --> ignor :)
    #print("t_MultiLineComments")
    pass
    
def t_SingleLineComment(t):
    r'(@@).*'
    # No return value --> ignor :)
    #print("t_SingleLineComment")
    pass

def t_const_real(t):
    #1.
    #.1
    #1.1
    #           1. ?????      |  ????? . 1     |   1.1  \s :)     
    r'(\+|-)?(([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+)|([0-9]+\.[0-9]+))'
    #r'(\+|-)?(([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+)|([0-9]+\.[0-9]+))\s'   
    #t.value =(t.value,float(t.value))
    try:
        t.value =(t.value,float(t.value))
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = -1400    
    return t

def t_Scientific_notation(t):
    r'(\+|-)?[1-9]+[0-9]*[e|E](\+|-)?[1-9]\s'
    #Convert scientific notation to decimals :)
    return t

def t_const_char(t):
    r'\'.+\'\s'
    return t

def t_const_string(t):
    r'".+"'
    return t

def t_const_long(t):
    r'[\+|-]?[0-9]*(LL|ll)\s'
    return t

# for hexadecimal, they are preceded by the characters 0x (zero, x)
def t_const_hex(t):
    r'[\+|-]?[0][x]([1-9]|[ABCDEF])+\s'
    try:
       t.value = (t.value,int(t.value,16)) #the value can be assigned to any Python object.
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = -1400
    return t

# For octal literals, the digits are preceded with a 0 (zero) character
def t_const_oct(t):
    r'[\+|-]?[0][1-9]+\s'      
    try:
       t.value = (t.value,int(t.value,8))  #the value can be assigned to any Python object.
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = -1400
    return t

def t_const_dec(t):
    r'[\+|-]?[0-9]+\s'
    try:
        t.value = (t.value,int(t.value))
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = -1400
    return t

table={}
def lookupTable(t):
    val = table.get(t,0)
    if val == 0:
        table[t] = 1
        return 1
    else:
        table[t]+=1
    return val+1

def t_id(t):
    r'([a-z]|[A-Z]|[_])([a-z]|[A-Z]|[_]|[0-9])*'
    t.value =( t.value, lookupTable(t.value))
    return t

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (4) track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
# (5) A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'
# (6) Error handling rule
def t_error(t):    
    print("\n~~~~~~~~ ERROR ~~~~~~~~~~~~\n")
    print("Illegal character '%s'" % t.value[0] )     
    print("\nLine: {L}".format(L=t.lexer.lineno))    
    print("\n~~~~~~~~ ERROR ~~~~~~~~~~~~\n")    
    t.lexer.skip(1)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

def p_statement_expr(t):
    # '''statement : program
    # '''
    '''statement : fun_dcl
                 | fun_dcl statement
                 | field_dcl fun_dcl
                 | field_dcl statement fun_dcl
                 | field_dcl statement fun_dcl statement
    '''
    print(t[1],type(t[1]))    
    print('accept')

def p_fun_dcl(t):
    '''fun_dcl : type field_dcl_cnt Semi_Colon
    '''
    t[0]='fun_dcl'

def p_field_dcl_cnt(t):
    '''field_dcl_cnt : id
                     | id int_const  
     '''

def p_int_const(t):
    '''
    int_const : const_dec
                 | const_hex
                 | const_oct
        '''

def p_field_dcl(t):
    '''field_dcl : testTime'''


def p_fun(t):
    """fun : id O_Parenthesis parameters C_Parenthesis
           | id O_Parenthesis C_Parenthesis
    """
    #t[0]=t[1]
    if t[3]==')':
        t[0]='2'
    else:
        t[0]='1'

def p_parameters(t):
    """parameters : id"""




def p_error(t):
     if t:
          print("Syntax 'P'error at token", t.type)
          # Just discard the token and tell the parser it's okay.
          parser.errok()
     else:
          print("Syntax 'P'error at EOF")


import ply.yacc as yacc
import ply.lex as lex
lexer = lex.lex()
# data=input(">> ")
# lexer.input(data)
# while True:
#         tok = lexer.token()       
#         if not tok:
#              break
#         else:
#             print(f"<{tok.type}> , {tok.value}")

parser = yacc.yacc()
while True:
    try:
        s = input('calc > ')   # Use raw_input on Python 2
    except EOFError:
        break
    parser.parse(s)
