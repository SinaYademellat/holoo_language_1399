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
    'const',
   
)
###################################################################################################################################################################################################################

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
t_const         = r'(const)'
########################################################################################################################################################################
def t_keyword(t):
    #r'(bool)|(break)|(case)|(char)|(const)|(continue)|(default)|(double)|(else)|(false)|(function)|(float)|(for)|(if)|(input)|(int)|(long)|(output)|(return)|(sizeof)|(string)|(switch)|(true)'
    r'(break)|(case)|(continue)|(default)|(else)|(function)|(for)|(if)|(input)|(output)|(return)|(sizeof)|(switch)'
    return t

def t_type(t):
    r'(bool)|(int)|(long)|(float)(char)|(double)|(string)'
    print('type->>',t)
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
        table[t] = 0
        return 0
    else:
        table[t]+= val
    return val

def t_id(t):
    r'([a-z]|[A-Z]|[_])([a-z]|[A-Z]|[_]|[0-9])*'
    t.value =( t.value, 'id')#lookupTable(t.value)
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
    
    # '''statement : assignment
    # '''

    # '''statement : fun_dcl
    #              | fun_dcl statement
    #              | field_dcl fun_dcl
    #              | field_dcl statement fun_dcl
    #              | field_dcl statement fun_dcl statement
    # '''

    # day 2
    ''' statement : fun_dcl
    '''

    print('accept')
    print(t[1],type(t[1]),'\n')    


# dictionary of function_name
function_name = { }
tmp_block = []

#4
    # <
    # <func_dcl> --→ {void | <tuple>} <id> ( [ <type> <id>{, <type> <id> }* ]) <block> 
def p_fun_dcl_1(t): # void fii () {....}
   
    '''fun_dcl : void id O_Parenthesis C_Parenthesis  block
    '''
    
    print('t[5]==>',t[5])
   
    t[0]='p_fun_dcl_1'
    function_name[t[2][0]] =('void',tmp_block) #['type',['list of code (Block )']]
    #tmp_block.clear()

#6
    #<block> --→ ‘{‘ <var_dcl>* <statementH>* ‘}’
def p_block1(t):
    '''block : O_CurlyBrace   C_CurlyBrace '''
    tmp_block.clear()
    t[0]='block'

def p_block2(t):
    '''block : O_CurlyBrace var_dcl  C_CurlyBrace '''
    global tmp_block
    t[0]='block1111'
    print('~~~~~~~',t[2])
    tmp_block.append(t[2])

def p_block2inf(t):
    '''block : O_CurlyBrace var_dcl var_dclinf C_CurlyBrace '''
    global tmp_block
    t[0]='p_block2inf'
    print('~~~~~~~p_block2inf~~~~~~~~',t[2])
    tmp_block.append(t[2])
    tmp_block.append(t[3])
#Block2==> need inf ...
def p_var_dclinf(t):
    '''var_dclinf : var_dcl 
                  | var_dcl var_dclinf'''
    print("p_var_dclinf-->")
    print(t[1])
    t[0]=t[1]

def p_block3(t):
    '''block : O_CurlyBrace statementH  C_CurlyBrace '''
    t[0]='block3'

#def p_block3inf ??


def p_block2_3(t):
    ''' block : O_CurlyBrace var_dcl statementH  C_CurlyBrace  '''
    t[0]='block2-3'

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ statementH ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    #10
        # <statementH> --→ <assignment>;
        # | <func_call>;
        # | <cond_stmt>; ??
        # | <loop_stmt>; ??
        # | return;
        # | <expr>;
        # | break;
        # | continue;
        # | sizeof(<type>)

def p_statementH_expr(t):
    ''' statementH : expr Semi_Colon '''
    t[0]='statementH -> expr ;'

######################################### ???

# def p_statementH_func_call (t):
#     ''' statementH : func_call Semi_Colon '''
#     t[0]='statementH -> func_call ;'

# def p_statementH_cond_stmt(t):
#     '''statementH : cond_stmt Semi_Colon'''
#     t[0]='statementH : cond_stmt Semi_Colon'

# # def cond_stmt ?????

# def p_statementH_loop_stmt(t):
#     '''statementH : loop_stmt Semi_Colon'''
#     t[0]='loop_stmt ;'

# #def loop_stmt ????? 

#7 
    #<var_dcl> --→ [const] <type> <var_dcl_cnt> [,<var_dcl_cnt>]*;
def p_var_dcl1(t):
    '''var_dcl : type var_dcl_cnt  Semi_Colon  '''
    print('t[1]-->',t[1])
    print('t[2]-->',t[2])
    t[0]=(t[1],t[2])

#8
    # <var_dcl_cnt> --→ <id> [= <expr>]
def p_var_dcl_cnt1(t):
    '''var_dcl_cnt  : id '''
    t[0]=(t[1][0],0)

def p_var_dcl_cnt2(t):
    '''var_dcl_cnt  : id Assignment  expr'''
    t[0]=(t[1][0],t[3])

def p_field_dcl_cnt(t):
    '''field_dcl_cnt : id
                     | id int_const  
     '''

def p_int_const(t):
    '''
    int_const    : const_dec
                 | const_hex
                 | const_oct
        '''
    #print('int-const',t,t[1],'int-const')
    t[0]= t[1][1]

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

######################################################################################################################

# dictionary of names
names = { }

# #11 
    # <assignment> --→ <variable> = <expr>
                    # | ′(′ <id>[, <id>]∗ ′)′ = <func_call>

def p_assignmentVAE(t):
    '''assignment : variable Assignment expr '''
    names[t[1]] = t[3]

def p_assignmentOiCAfunc_call(t):
    '''assignment : O_Parenthesis id C_Parenthesis Assignment func_call '''
    names[t[2]] = 'return func_call'#t[3]

def p_assignmentOicommaCAfunc_call(t):
    '''assignment : O_Parenthesis id Comma_id C_Parenthesis  Assignment func_call '''
    names[t[2]] = 'return func_call'#t[3]

# #11.0
def p_Comma_id(t):
    '''Comma_id  : Comma id 
                 | Comma id Comma_id
    '''

# #12
    # <variable> --→ <id> [{ ′[′ <expr> ′]′ }+]
            # | ++<variable>
            # | --<variable>
            # | <variable>++
            # | <variable>--
def p_variable(t):
    '''variable : id 
                | id variable_1_2
                | Increment variable
                | Decrement variable
                | variable  Increment
                | variable  Decrement
                '''

    # print("\n!!!!!!!!!!!")
    # print(t)
    # print(t[1])
    # print("\n!!!!!!!!!!!")
    
    t[0]=t[1]

    # t[0]=('variable',t[1])

# #12.0
def p_variable_1_2(t):
    '''variable_1_2 : O_Brace expr C_Brace
                    | O_Brace expr C_Brace  variable_1_2'''

# #17
 # <expr> --→ <expr> <binary_op> <expr>
            # | ′(′ <expr> ′)′ 
            # | <func_call>
            # | <variable>
            # | <const_val>
            # | −<expr>
            # | ! <expr>
def p_expr(t):
    '''expr : O_Parenthesis expr C_Parenthesis
            | func_call
            | SubAndUnaryMinus expr
            | Not expr
    '''

def p_expr_binary_op(t):
    '''expr :  expr binary_op expr '''
    
    ## test 
        # print("line 343")
        # print(t)
        # print(t[2])
        # print("~~~~~~~")

    if(t[2][0]=='arithmatic'):
        print('arithmatic',"--> t[2][1]",t[2][1])

        if(t[2][1] == '+') :

            print('+',t[1],t[3])
            t[0]=t[1]+t[3]
        
        elif(t[2][1] == '-'):
             print('-',t[1],t[3])
             t[0]=t[1]-t[3]

        elif(t[2][1] =='*'):
             print('*',t[1],t[3])
             t[0]=t[1]*t[3]
    
        
        elif(t[2][1] == '/'):
             print('/',t[1],t[3])
             if t[3]!=0:                    
                t[0]=t[1]/t[3]
             else:
                 print("x/0 -- > ???!!")
                 t[0]=t[1]

        elif(t[2][1]=='%'):
             print('%',t[1],t[3])
             t[0]=t[1]%t[3]
    
    else:
        print('conditional')
   
def p_expr_variable(t):
    ''' expr : variable '''
    #fill it

def p_expr_const_val(t):
    '''expr : const_val'''
    t[0] = t[1]

def p_binary_op(t):
    '''binary_op : arithmatic
                 | conditional
                 '''
    #print('-->?',t[1])
    t[0] = t[1]

def p_arithmatic(t):
    '''arithmatic : Add
                  | SubAndUnaryMinus
                  | Production
                  | Div
                  | Mod
                  | ArithmeticAnd
                  | ArithmeticOr
                  | Logical_Arithmetic_Xor
                  | LogicalOr
                  | Logicaland
                  '''
    #print('-->',t[1])
    t[0]=('arithmatic',t[1])

def p_conditional(t):
    '''conditional : Biggerorequal
                   | NotEqual 
                   | Equal
                   | LessorEqual
                   | Lessthan
                   | Biggerthan
                    '''
    t[0]=('conditional',t[1])

# #21 
 # const_val> --→ <int_const>
    # | <real_const>
    # | <char_const>
    # | <bool_const> --?
    # | <string_const>
    # | <long_const>
def p_const_val(t):
    '''const_val : int_const
                 | const_real
                 | const_char
                 | Boolean
                 | const_string
                 | const_long
                 '''
   # test
    print("\n!!!!!!!!!!\n")
    print(t[1],type(t[1]))
    print("\n!!!!!!!!!!\n")

    t[0] = t[1]
    

def p_func_call(t):
    '''func_call : id  O_Parenthesis C_Parenthesis
                 | id  O_Parenthesis parameters C_Parenthesis'''


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

# testlex
    # data=input(">> ")
    # lexer.input(data)
    # while True:
    #         tok = lexer.token()
    #         if not tok:
    #              break
    #         else:
    #             print(f"<{tok.type}> , {tok.value}")

parser = yacc.yacc()


#Test yacc
data__='''void sina(){
    /@
    test function :)
    @/
    int x = 0x12 + 0x13 ;
    int z ;
}
'''
print (data__)
parser.parse(data__)
print('global var -> ',names) 
print('func--->',function_name)

# exit(0)

if __name__=='__main__':        
    while True:
        try: 
            tmp_block.clear()  
            s = input('calc > ')      
        except EOFError:
            break
        parser.parse(s)
        print('global var -> ',names) 
        print('func--->',function_name)


#@@@@@@@@@@@@@@@@@@@@ TEST case @@@@@@@@@@@@@@@@@@@@@@@@
    ########################################################
    #    sina = 12 + 0x12 :)                               #
    #    id   = ? - ? ==> :)                               #
    #   ......                                             #
    ########################################################

#: Ctrl + K Ctrl + 0 ==> ... all methods in vscode


#@@@@@@@@@@@@@@@@@@@@ TEST case @@@@@@@@@@@@@@@@@@@@@@@@
    ########################################################
    # #data__='''void sina(){                              #
    #     0x12  ;                                          #
    # }                                                    #
    #                                                      #
    # vodi ali(){                                          #
    #   int x ;                                            #
    # }                                                    #
    #                                                      #
    ########################################################



#@@@@@@@@@@@@@@@@@@@@ TEST case @@@@@@@@@@@@@@@@@@@@@@@@
    ########################################################
    # # #Test yacc                                        #
    # data__='''void sina(){                              #
    #     /@                                              #
    #     test function :)                                #
    #     @/                                              #
    #     int x = 0x12 + 0x13 ;                           #
    #     int z ;                                         #
    # }                                                   #
    ########################################################



