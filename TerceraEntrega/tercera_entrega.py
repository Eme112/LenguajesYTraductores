# Autor: Emerico Pedraza Gomez
# Matricula: A01382216
# Fecha: 25 de abril de 2022
# Tercera entrega de proyecto

### NOTAS ###
# 1. Siempre marca error en la última línea
# R: 
# 2. Aun no esta terminado variables dimensionadas
# R: 
# 3. Como manejar los prints/inputs
# R: 

from lzma import MODE_NORMAL
from tkinter import E
from numpy import char
import ply.lex as lex
import ply.yacc as yacc
import sys

###         TOKENS Y PALABRAS RESERVADAS         ###

# Definicion de palabras reservadas
reserved = {
    'begin' : 'BEGIN',
    'end' : 'END',
    'loop' : 'LOOP',
    'if' : 'IF',
    'else' : 'ELSE',
    'elsif' : 'ELSIF',
    'while' : 'WHILE',
    'do' : 'DO',
    'for' : 'FOR',

    'function' : 'FUNCTION',
    'procedure' : 'PROCEDURE',
    'return' : 'RETURN',
    'main' : 'MAIN',

    'print' : 'PRINT',
    'input' : 'INPUT',

    'int' : 'INT',
    'float' : 'FLOAT',
}

# Lista de tokens
tokens = [
	'MENOR', 'MAYOR', 'MENOR_IGUAL', 'MAYOR_IGUAL', 'IGUAL', 'DIFERENTE',  
    'AND', 'OR', 'NOT',
    'MAS', 'MENOS', 'POR', 'ENTRE', 'MODULO',
    'COMILLAS',
    'PARENTESIS_IZQUIERDO', 'PARENTESIS_DERECHO', 'CORCHETE_IZQUIERDO', 'CORCHETE_DERECHO',
    'DOS_PUNTOS', 'PUNTO_COMA',
    'VALOR_INT', 'VALOR_FLOAT',
    'ID', 'TEXTO',
    'ASIGNACION',
] + list(reserved.values())

# Definicion de tokens simples
t_MENOR = r'<'
t_MAYOR = r'>'
t_MENOR_IGUAL = r'<='
t_MAYOR_IGUAL = r'>='
t_IGUAL = r'=='
t_DIFERENTE = r'\'='

t_AND = r'&'
t_OR = r'\|'
t_NOT = r'\''

t_MAS = r'\+'
t_MENOS = r'-'
t_POR = r'\*'
t_ENTRE = r'/'
t_MODULO = r'%'

t_COMILLAS = r'\"'

t_PARENTESIS_IZQUIERDO = r'\('
t_PARENTESIS_DERECHO = r'\)'
t_CORCHETE_IZQUIERDO = r'\['
t_CORCHETE_DERECHO = r'\]'

t_DOS_PUNTOS = r'\:'
t_PUNTO_COMA = r'\;'

t_ASIGNACION = r'=>'

# Definicion de otros tokens
def t_VALOR_INT(t):
    r'\d+'
    t.value = int(t.value)    
    return t

def t_VALOR_FLOAT(t):
    r'\d+'
    t.value = float(t.value)    
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')    # Revisar palabras reservadas
    return t

def t_TEXTO(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')    # Revisar palabras reservadas
    return t
 
# Regla para pasar de lineas
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
 
# Strings que se ignoran (espacios y tabs)
t_ignore  = ' \t'

# Regla para manejar errores
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Construir el lexer
lexer = lex.lex()

###         GRAMATICA         ###

# UTILIZANDO SINTAXIS SENCILLA
# Vacío
def p_empty(p):
  '''
  empty :
  '''
  pass

# Comparadores
def p_C(p):
    '''C : MENOR
         | MAYOR
         | MENOR_IGUAL
         | MAYOR_IGUAL
         | IGUAL
         | DIFERENTE'''

# Expresiones
def p_E(p):
    '''E : E2
         | E2 C E2'''

# Expresiones 2
def p_E2(p):
    '''E2 : E3
          | E2 OR E3
          | E2 MAS E3
          | E2 MENOS E3'''

# Expresiones 3
def p_E3(p):
    '''E3 : E4
          | E3 AND E4
          | E3 POR E4
          | E3 ENTRE E4
          | E3 MODULO E4'''

# Expresiones 4
def p_E4(p):
    '''E4 : T
          | NOT T'''

# Terminos
def p_T(p):
    '''T : PARENTESIS_IZQUIERDO E PARENTESIS_DERECHO
         | FUNCTION
         | ID_COMPLETO
         | VALOR_INT
         | VALOR_FLOAT'''

# Nombre de variables
def p_ID_COMPLETO(p):
    '''ID_COMPLETO : ID RANGOS'''
def p_RANGOS(p):
    '''RANGOS : empty
              | CORCHETE_IZQUIERDO VALOR_INT CORCHETE_DERECHO RANGOS'''

# Definicion de variables (incluye dimensionadas)
def p_V(p):
    '''V : TIPO DOS_PUNTOS A PUNTO_COMA
         | TIPO DOS_PUNTOS ID_COMPLETO PUNTO_COMA'''
def p_TIPO(p):
    '''TIPO : INT
            | FLOAT'''
def p_V_M(p):
    '''V_M : empty
           | V V_M'''

# Asignacion
def p_A(p):
    '''A : ID_COMPLETO ASIGNACION VALOR_INT
         | ID_COMPLETO ASIGNACION VALOR_FLOAT
         | ID_COMPLETO ASIGNACION E
         | ID_COMPLETO ASIGNACION ID_COMPLETO
         | ID_COMPLETO ASIGNACION ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO
         | ID_COMPLETO ASIGNACION INPUT PARENTESIS_IZQUIERDO PARENTESIS_DERECHO''' # Penúltima línea para el llamado de funciones con valor de retorno

# Estatutos
def p_EST(p):
    '''EST : empty
           | LOOP_ END LOOP PUNTO_COMA EST
           | IF_ END IF PUNTO_COMA EST
           | A PUNTO_COMA EST
           | PROCEDURE PUNTO_COMA EST
           | FUNCTION PUNTO_COMA EST
           | PRINT PARENTESIS_IZQUIERDO COMILLAS COMILLAS PARENTESIS_DERECHO PUNTO_COMA EST
           | INPUT PARENTESIS_IZQUIERDO PARENTESIS_DERECHO PUNTO_COMA EST
           | ID PARENTESIS_IZQUIERDO  PARENTESIS_DERECHO PUNTO_COMA EST'''          # TODO: Corregir el PRINT, no me deja imprimir texto

# Ciclos
def p_LOOP(p):      # TODO: Tiene problemas para identificar el cierre con el end loop
    '''LOOP_ : DO_WHILE
             | WHILE_
             | FOR_'''
def p_DO_WHILE(p):
    '''DO_WHILE : DO DOS_PUNTOS EST WHILE E PUNTO_COMA'''
def p_WHILE_(p):    
    '''WHILE_ : WHILE E DOS_PUNTOS EST'''
def p_FOR_(p):      # TODO: Marca error de sintaxis el for
    '''FOR_ : FOR EST E EST DOS_PUNTOS EST'''

# If
def p_IF_(p):
    '''IF_ : IF E DOS_PUNTOS EST ELSIF_'''
def p_ELSIF_(p):
    '''ELSIF_ : END IF PUNTO_COMA
              | ELSE DOS_PUNTOS EST END IF PUNTO_COMA
              | ELSIF DOS_PUNTOS E EST ELSIF_'''

# Procedimientos
def p_P(p):
    '''P : PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA
         | PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMA'''

# Funciones
def p_F(p):
    '''F : TIPO ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN ID PUNTO_COMA END FUNCTION PUNTO_COMA'''

# Procedimiento principal
def p_MP(p):
    '''MP : PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA
          | PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMA'''

# Programa
def p_PROGRAMA(p):
    '''PROGRAMA : MP PROGRAMA_H'''
    print("\tSINTAXIS CORRECTA!")
def p_PROGRAMA_H(p):
    '''PROGRAMA_H : empty
                  | P PROGRAMA_H
                  | F PROGRAMA_H '''

# Starter rule
start = 'PROGRAMA'

# Regla del error para errores de sintaxis
def p_error(p):
    print("Syntax error in input!, line: %i" % p.lineno)

parser = yacc.yacc()










inputString = '''
procedure main():
    int : num1[3][2];
begin;
    num1 => suma();
    num2 => 3;
    do:
        i => i + 1;
        print("");
    while i < 10;
    end loop;
end procedure;
'''
"""
procedure suma():
	int : num1;
    int : num2;
    int : res;	
begin;
    res[2] => num1 + num2;
    a => b & c;
    print("");
    input();
end procedure;
'''
"""

parser.parse(inputString)
# Give the lexer some input
lexer.input(inputString)


# Tokenize
while True:
    tok = lexer.token()
    if not tok: 
        break      # No more input
    print(tok) 