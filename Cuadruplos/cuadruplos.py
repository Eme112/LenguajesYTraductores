# Autor: Emerico Pedraza Gomez
# Matricula: A01382216
# Nombre del Lenguaje: MP4

####################         NOTAS         ####################
# 1. Siempre marca error en la última línea (cuando hay mas de un procedure) o al finalizar un loop
# R: 
# 2. Como manejar la impresión de texto en strings?
# R: 
# 3. No detecto valores float, me marca error de sintaxis
# R:
# 4. Problemas para imprimir la linea al tener un error en la tabla de simbolos
# R: 

# Quitar fors y añadir los temporales a la pila de operandos

from inspect import stack
from turtle import st
import ply.lex as lex
import ply.yacc as yacc


####################         TOKENS Y PALABRAS RESERVADAS         ####################
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
    'PUNTO', 'DOS_PUNTOS', 'PUNTO_COMA',
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
t_PUNTO = r'\.'
t_DOS_PUNTOS = r'\:'
t_PUNTO_COMA = r'\;'
t_ASIGNACION = r'=>'

# Definicion de otros tokens
def t_VALOR_INT(t):
    r'\d+'
    t.value = int(t.value)    
    return t

def t_VALOR_FLOAT(t):
    r'\d+\.\d+'
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


####################         TABLA DE SÍMBOLOS         ####################

# Funcion para agregar IDs a la tabla de símbolos
def agregarATablaSimbolos(p, id_index, type_index):
    global index_tabla              # Variable global para el índice de la tabla de símbolos
    global tabla_simbolos           # Variable global para la tabla de símbolos
    identificador = p[id_index]     # Se extrae el identificador
    tipo = p[type_index]            # Se extrae el tipo
    if identificador in tabla_simbolos:
        print("Error[linea]: ID -->", identificador, "<-- definido con anterioridad")
    else:
        tabla_simbolos[identificador] = [tipo, index_tabla] # Se agrega el identificador y su tipo a la tabla de símbolos
        index_tabla += 1                                    # Se incrementa el índice de la tabla de símbolos

index_tabla = 0     # Variable para el índice de la tabla de símbolos
tabla_simbolos = {} # Lista para guardar nombres de variables y funciones/procedimientos


####################         CUADRUPLOS         ####################

stack_operandos = []
cuadruplos = []
cuadruplo_actual = 0
lista_temporales = ['T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10', 'T11', 'T12', 'T13', 'T14', 'T15', 'T16', 'T17', 'T18', 'T19', 'T20', 'T21', 'T22', 'T23', 'T24', 'T25']
jerarquia4 = ['<', '>', '<=', '>=', '==', '\'=']
jerarquia3 = ['|', '+', '-']
jerarquia2 = ['&', '*', '/', '%']
jerarquia1 = ['\'']

def actualizarListas(simbolo):
    op2 = stack_operandos.pop()             # Obtener operando 2
    op1 = stack_operandos.pop()             # Obtener operando 1
    res = lista_temporales.pop()            # Obtener Temp que almacena resultado
    stack_operandos.append(res)             # Añadir Temp al stack de operandos
    guardarCuadruplo(simbolo, op1, op2, res)

def guardarCuadruplo(operador, op1, op2, res):
    global cuadruplos
    global cuadruplo_actual
    cuadruplo_actual += 1
    cuadruplos.append([operador, op1, op2, res])
    print("Cuadruplo añadido -->", cuadruplo_actual, ": [", operador, op1, op2, res, "]")

def reiniciarListaTemporales():
    global lista_temporales
    lista_temporales = ['T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10', 'T11', 'T12', 'T13', 'T14', 'T15', 'T16', 'T17', 'T18', 'T19', 'T20', 'T21', 'T22', 'T23', 'T24', 'T25']


####################         GRAMATICA         ####################

# UTILIZANDO SINTAXIS SENCILLA
# Vacío
def p_empty(p):
  '''
  empty :
  '''
  pass

# Expresiones
def p_E(p):
    '''E : E2
         | E2 MENOR E2 AUX_MENOR
         | E2 MAYOR E2 AUX_MAYOR
         | E2 MENOR_IGUAL E2 AUX_MENOR_IGUAL
         | E2 MAYOR_IGUAL E2 AUX_MAYOR_IGUAL
         | E2 IGUAL E2 AUX_IGUAL
         | E2 DIFERENTE E2 AUX_DIFERENTE'''
def p_AUX_MENOR(p):
    '''AUX_MENOR : empty'''
    actualizarListas('<')
def p_AUX_MAYOR(p):
    '''AUX_MAYOR : empty'''
    actualizarListas('>')
def p_AUX_MENOR_IGUAL(p):
    '''AUX_MENOR_IGUAL : empty'''
    actualizarListas('<=')
def p_AUX_MAYOR_IGUAL(p):
    '''AUX_MAYOR_IGUAL : empty'''
    actualizarListas('>=')
def p_AUX_IGUAL(p):
    '''AUX_IGUAL : empty'''
    actualizarListas('==')
def p_AUX_DIFERENTE(p):
    '''AUX_DIFERENTE : empty'''
    actualizarListas('\'=')
            
# Expresiones 2
def p_E2(p):
    '''E2 : E3
          | E2 OR E3 AUX_OR
          | E2 MAS E3 AUX_MAS
          | E2 MENOS E3 AUX_MENOS'''    
def p_AUX_OR(p):
    '''AUX_OR : empty'''
    actualizarListas('|')
def p_AUX_MAS(p):
    '''AUX_MAS : empty'''
    actualizarListas('+')
def p_AUX_MENOS(p):
    '''AUX_MENOS : empty'''
    actualizarListas('-')

# Expresiones 3
def p_E3(p):
    '''E3 : E4
          | E3 AND E4 AUX_AND
          | E3 POR E4 AUX_POR
          | E3 ENTRE E4 AUX_ENTRE
          | E3 MODULO E4 AUX_MODULO'''
def p_AUX_AND(p):
    '''AUX_AND : empty'''
    actualizarListas('&')
def p_AUX_POR(p):
    '''AUX_POR : empty'''
    actualizarListas('*')
def p_AUX_ENTRE(p):
    '''AUX_ENTRE : empty'''
    actualizarListas('/')
def p_AUX_MODULO(p):
    '''AUX_MODULO : empty'''
    actualizarListas('%')

# Expresiones 4
def p_E4(p):
    '''E4 : T
          | NOT T AUX_NOT'''
def p_AUX_NOT(p):
    '''AUX_NOT : empty'''
    op1 = stack_operandos.pop()             # Obtener operando 1
    res = lista_temporales.pop()            # Obtener Temp que almacena resultado
    stack_operandos.append(res)             # Añadir Temp al stack de operandos
    guardarCuadruplo('\'', op1, None, res)

# Terminos
def p_T(p):
    '''T : PARENTESIS_IZQUIERDO E PARENTESIS_DERECHO
         | FUNCTION
         | ID_COMPLETO
         | VALOR_INT
         | VALOR_FLOAT'''
    # verificar que, si es un ID (no es nada de lo demas), este exista en la tabla de simbolos
    if p[1] != '(':
        if str(p[1])[0] >= 'a' and str(p[1])[0] <= 'z':     # Asegurarse de que no sea un numero
            if p[1] not in tabla_simbolos:
                print("Error[linea]: ID -->", p[1], "<-- no definido")
    # Añadir terminales a la pila de operandos
    if p[1] != '(':
        if str(p[1])[0] >= 'a' and str(p[1])[0] <= 'z':     # Si no es un numero, revisar si es ID
            if p[1] in tabla_simbolos:
                stack_operandos.append(p[1])
        elif type(p[1]) == int or type(p[1]) == float:      # Si es un numero, añadirlo a la pila de operandos
            stack_operandos.append(p[1])

# Nombre de variables
def p_ID_COMPLETO(p):
    '''ID_COMPLETO : ID RANGOS'''
    p[0] = p[1]             # Linea para identificar y retornar el ID de la variable/función/procedimiento

def p_RANGOS(p):
    '''RANGOS : empty
              | CORCHETE_IZQUIERDO VALOR_INT CORCHETE_DERECHO RANGOS'''

# Definicion de variables (incluye dimensionadas)
def p_V(p):
    '''V : TIPO DOS_PUNTOS A PUNTO_COMA
         | TIPO DOS_PUNTOS ID_COMPLETO PUNTO_COMA'''
    # agregarATablaSimbolos(p, id_index, type_index, identificador, tipo)
    agregarATablaSimbolos(p, 3, 1)
def p_TIPO(p):
    '''TIPO : INT
            | FLOAT'''
    p[0] = p[1]             # Linea para identificar y retornar el tipo de la variable
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
    # Verificar que la primer variable (a la que se le actualizara su valor) exista
    if not (p[1] in tabla_simbolos):
        print("Error[linea]: ID -->", p[1], "<-- no ha sido definido con anterioridad")
    # Si ya existe, añadir a la pila de operandos
    else:
        stack_operandos.append(p[1])
    # Si hay otro ID despues de la asignacion, verificar que este tambien exista
    for i in range(2, len(p)):
        if p[i] == '=>':
            if str(p[i+1])[0] >= 'a' and str(p[i+1])[0] <= 'z':     # Asegurarse de que pueda ser un ID
                if not (p[i+1] in reserved):                           # Asegurarse de que no sea una palabra reservada
                    if p[i+1] not in tabla_simbolos:                        # Verificar si el ID no existe
                        print("Error[linea]: ID -->", p[i+1], "<-- no definido")
    # Añadir terminales a la pila de operandos
    if type(p[1]) == int or type(p[1]) == float:      # Si es un numero, añadirlo a la pila de operandos
        stack_operandos.append(p[1])
    # Cuadruplos
    op1 = stack_operandos.pop()
    op2 = stack_operandos.pop()
    res = lista_temporales.pop()
    guardarCuadruplo('=>', op1, op2, res)
    print("")
    reiniciarListaTemporales()


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
    # Verificar que la variable/función/procedimiento exista
    if not (p[1] in reserved):
        if p[1] != None:
            if not (p[1] in tabla_simbolos):
                print("Error[linea]: ID -->", p[1], "<-- no definido")

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
    # Revisar si el ID ya existe
    if p[2] in tabla_simbolos:
        print("Error[linea]: ID -->", p[2], "<-- definido con anterioridad")
    # agregarATablaSimbolos(p, id_index, type_index)
    agregarATablaSimbolos(p, 2, 1)

# Funciones
def p_F(p):
    '''F : TIPO ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN ID PUNTO_COMA END FUNCTION PUNTO_COMA'''
    # Revisar si el ID ya existe
    if p[2] in tabla_simbolos:
        print("Error[linea]: ID -->", p[2], "<-- definido con anterioridad")
    # agregarATablaSimbolos(p, id_index, type_index)
    agregarATablaSimbolos(p, 2, 1)

# Procedimiento principal
def p_MP(p):
    '''MP : PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA
          | PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMA'''
    # agregarATablaSimbolos(p, id_index, type_index)
    agregarATablaSimbolos(p, 2, 1)

# Programa
def p_PROGRAMA(p):
    '''PROGRAMA : MP PROGRAMA_H'''
    print("\n\tSINTAXIS CORRECTA!\n")
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

# Abrir y seleccionar archivo para texto de entrada
try:
    fp = open("programaPrueba2.txt", "r")
    inputString = fp.read()
    fp.close()
except FileNotFoundError:
    print("Revisar PATH.")

parser.parse(inputString)
# Give the lexer some input
lexer.input(inputString)

#print("\nTabla de simbolos:\n",tabla_simbolos)
print("\nStack de operandos:\n", stack_operandos)
print("\nLista de cuádruplos:\n", cuadruplos)