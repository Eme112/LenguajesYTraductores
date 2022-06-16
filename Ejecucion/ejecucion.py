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
# 6. Actualizar los cuadruplos de saltos de procedimientos y llamado

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

tabla_simbolos = {} # Lista para guardar nombres de variables y funciones/procedimientos

# Funcion para agregar IDs a la tabla de símbolos
def agregarATablaSimbolos(p, id_index, type_index):
    global tabla_simbolos           # Variable global para la tabla de símbolos
    identificador = p[id_index]     # Se extrae el identificador
    tipo = p[type_index]            # Se extrae el tipo
    if identificador in tabla_simbolos:
        print("Error[linea]: ID -->", identificador, "<-- definido con anterioridad")
    else:
        tabla_simbolos[identificador] = [tipo, 0] # Se agrega el identificador y su tipo a la tabla de símbolos


####################         CUADRUPLOS         ####################

stack_operandos = []
cuadruplos = []
cuadruplo_actual = 0
lista_temporales = ['T25', 'T24', 'T23', 'T22', 'T21', 'T20', 'T19', 'T18', 'T17', 'T16', 'T15', 'T14', 'T13', 'T12', 'T11', 'T10', 'T9', 'T8', 'T7', 'T6', 'T5', 'T4', 'T3', 'T2', 'T1']
valores_temporales = [0]*25

const_lista_temporales = ['T25', 'T24', 'T23', 'T22', 'T21', 'T20', 'T19', 'T18', 'T17', 'T16', 'T15', 'T14', 'T13', 'T12', 'T11', 'T10', 'T9', 'T8', 'T7', 'T6', 'T5', 'T4', 'T3', 'T2', 'T1']
jerarquia4 = ['<', '>', '<=', '>=', '==', '\'=']
jerarquia3 = ['|', '+', '-']
jerarquia2 = ['&', '*', '/', '%']
jerarquia1 = ['\'']

def actualizarListaCuadruplos(simbolo, numOperandos):
    if numOperandos == 0:
        res = lista_temporales.pop()            # Obtener Temp que almacena resultado
        stack_operandos.append(res)             # Añadir Temp al stack de operandos
        guardarCuadruplo('\'', op1, None, res)
    elif numOperandos == 1:
        op1 = stack_operandos.pop()             # Obtener operando 1
        res = lista_temporales.pop()            # Obtener Temp que almacena resultado
        stack_operandos.append(res)             # Añadir Temp al stack de operandos
        guardarCuadruplo('\'', op1, None, res)
    elif numOperandos == 2:
        op2 = stack_operandos.pop()             # Obtener operando 2
        op1 = stack_operandos.pop()             # Obtener operando 1
        if op1 in const_lista_temporales:
            lista_temporales.append(op1)
        if op2 in const_lista_temporales:
            lista_temporales.append(op2)
        res = lista_temporales.pop()            # Obtener Temp que almacena resultado
        stack_operandos.append(res)             # Añadir Temp al stack de operandos
        guardarCuadruplo(simbolo, op1, op2, res)

def guardarCuadruplo(operador, op1, op2, res):
    global cuadruplos
    global cuadruplo_actual
    cuadruplo_actual += 1
    cuadruplos.append([operador, op1, op2, res])
    print("Cuadruplo añadido -->", cuadruplo_actual, ": [", operador, op1, op2, res, "]")


####################         SALTOS         ####################

pila_saltos = []
pila_cuadruplos_pendientes = []
nivel = -1

def agregarSaltoCuadruplo(saltoCondicional):
    if saltoCondicional:
        op1 = stack_operandos.pop()             # Obtener expr booleana
        guardarCuadruplo("GotoF", op1, None, None)
    else:
        guardarCuadruplo("Goto", None, None, None)

####################         EJECUCION         ####################

inicio_ejecucion = 1

def ejecucionCuadruplos():
    global inicio_ejecucion
    print("Inicio ejecucion", inicio_ejecucion)
    pc = inicio_ejecucion
    while pc <= len(cuadruplos):
        sumar1 = True
        guardar_valor = True
        cuadruplo = cuadruplos[pc-1]
        print("Ejecutando cuadruplo -->", pc, ": [", cuadruplo[0], cuadruplo[1], cuadruplo[2], cuadruplo[3], "]")
        
        operacion = cuadruplo[0]

        if cuadruplo[1] in const_lista_temporales:
            op1 = valores_temporales[const_lista_temporales.index(cuadruplo[1])]
        elif cuadruplo[1] in tabla_simbolos:
            op1 = tabla_simbolos[cuadruplo[1]][1]
        else:
            op1 = cuadruplo[1]

        if cuadruplo[2] in const_lista_temporales:
            op2 = valores_temporales[const_lista_temporales.index(cuadruplo[2])]
        elif cuadruplo[2] in tabla_simbolos:
            op2 = tabla_simbolos[cuadruplo[2]][1]
        else:
            op2 = cuadruplo[2]

        if operacion == '=>':
            valor = op1
        elif operacion == '<':
            valor = op1 < op2
        elif operacion == '>':
            valor = op1 > op2
        elif operacion == '<=':
            valor = op1 <= op2
        elif operacion == '>=':
            valor = op1 >= op2
        elif operacion == '==':
            valor = op1 == op2
        elif operacion == '\'=':
            valor = op1 != op2
        elif operacion == '|':
            valor = op1 or op2
        elif operacion == '+':
            valor = op1 + op2
        elif operacion == '-':
            valor = op1 - op2
        elif operacion == '&':
            valor = op1 and op2
        elif operacion == '*':
            valor = op1 * op2
        elif operacion == '/':
            valor = op1 / op2
        elif operacion == '%':
            valor = op1 % op2
        elif operacion == '\'':
            if op1 == 0:
                valor = 1
            else:
                valor = 0
        elif operacion == 'Goto':
            pc = cuadruplo[3]
            guardar_valor = False
            sumar1 = False
        elif operacion == 'GotoF':
            guardar_valor = False
            if op1 == 0:
                pc = cuadruplo[3]
                sumar1 = False

        if sumar1:
            pc += 1

        if guardar_valor:
            if cuadruplo[3] in const_lista_temporales:
                valores_temporales[const_lista_temporales.index(cuadruplo[3])] = valor
            else:
                tabla_simbolos[cuadruplo[3]][1] = valor


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
    actualizarListaCuadruplos('<', 2)
def p_AUX_MAYOR(p):
    '''AUX_MAYOR : empty'''
    actualizarListaCuadruplos('>', 2)
def p_AUX_MENOR_IGUAL(p):
    '''AUX_MENOR_IGUAL : empty'''
    actualizarListaCuadruplos('<=', 2)
def p_AUX_MAYOR_IGUAL(p):
    '''AUX_MAYOR_IGUAL : empty'''
    actualizarListaCuadruplos('>=', 2)
def p_AUX_IGUAL(p):
    '''AUX_IGUAL : empty'''
    actualizarListaCuadruplos('==', 2)
def p_AUX_DIFERENTE(p):
    '''AUX_DIFERENTE : empty'''
    actualizarListaCuadruplos('\'=', 2)
            
# Expresiones 2
def p_E2(p):
    '''E2 : E3
          | E2 OR E3 AUX_OR
          | E2 MAS E3 AUX_MAS
          | E2 MENOS E3 AUX_MENOS'''    
def p_AUX_OR(p):
    '''AUX_OR : empty'''
    actualizarListaCuadruplos('|', 2)
def p_AUX_MAS(p):
    '''AUX_MAS : empty'''
    actualizarListaCuadruplos('+', 2)
def p_AUX_MENOS(p):
    '''AUX_MENOS : empty'''
    actualizarListaCuadruplos('-', 2)

# Expresiones 3
def p_E3(p):
    '''E3 : E4
          | E3 AND E4 AUX_AND
          | E3 POR E4 AUX_POR
          | E3 ENTRE E4 AUX_ENTRE
          | E3 MODULO E4 AUX_MODULO'''
def p_AUX_AND(p):
    '''AUX_AND : empty'''
    actualizarListaCuadruplos('&', 2)
def p_AUX_POR(p):
    '''AUX_POR : empty'''
    actualizarListaCuadruplos('*', 2)
def p_AUX_ENTRE(p):
    '''AUX_ENTRE : empty'''
    actualizarListaCuadruplos('/', 2)
def p_AUX_MODULO(p):
    '''AUX_MODULO : empty'''
    actualizarListaCuadruplos('%', 2)

# Expresiones 4
def p_E4(p):
    '''E4 : T
          | NOT T AUX_NOT'''
def p_AUX_NOT(p):
    '''AUX_NOT : empty'''
    actualizarListaCuadruplos('\'', 1)

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
    res = stack_operandos.pop()
    op1 = stack_operandos.pop()
    guardarCuadruplo('=>', op1, None, res)

# Estatutos
def p_EST(p):
    '''EST : empty
           | LOOP_ EST
           | IF_ EST
           | A PUNTO_COMA EST
           | PROCEDURE PUNTO_COMA EST
           | FUNCTION PUNTO_COMA EST
           | PRINT PARENTESIS_IZQUIERDO COMILLAS COMILLAS PARENTESIS_DERECHO PUNTO_COMA EST
           | INPUT PARENTESIS_IZQUIERDO PARENTESIS_DERECHO PUNTO_COMA EST
           | ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO PUNTO_COMA EST'''          # TODO: Corregir el PRINT, no me deja imprimir texto
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
    '''DO_WHILE : DO WHILE E DOS_PUNTOS EST END LOOP PUNTO_COMA'''

def p_WHILE_(p):    
    '''WHILE_ : WHILE AUX_WHILE1 E AUX_WHILE2 DOS_PUNTOS EST END LOOP PUNTO_COMA AUX_ENDWHILE'''
def p_AUX_WHILE1(p):
    '''AUX_WHILE1 : empty'''
    global nivel
    nivel += 1
    pila_saltos.append([])      # Agregamos nuevo nivel a la pila de saltos
    pila_saltos[nivel].append(cuadruplo_actual+1)
def p_AUX_WHILE2(p):
    '''AUX_WHILE2 : empty'''
    global nivel
    agregarSaltoCuadruplo(True)
    pila_saltos[nivel].append(cuadruplo_actual)
def p_AUX_ENDWHILE(p):
    '''AUX_ENDWHILE : empty'''
    global nivel
    agregarSaltoCuadruplo(False)
    cuadruplos[pila_saltos[nivel].pop()-1][3] = cuadruplo_actual+1
    cuadruplos[cuadruplo_actual-1][3] = pila_saltos[nivel].pop()
    nivel -= 1
    
def p_FOR_(p):      # TODO: Marca error de sintaxis el for
    '''FOR_ : FOR A PUNTO_COMA AUX_FOR1 E AUX_FOR2 PUNTO_COMA A DOS_PUNTOS EST END LOOP PUNTO_COMA AUX_ENDFOR'''
def p_AUX_FOR1(p):
    '''AUX_FOR1 : empty'''
    global nivel
    nivel += 1
    pila_saltos.append([])      # Agregamos nuevo nivel a la pila de saltos
    pila_saltos[nivel].append(cuadruplo_actual+1)
def p_AUX_FOR2(p):
    '''AUX_FOR2 : empty'''
    global nivel
    agregarSaltoCuadruplo(True)
    pila_saltos[nivel].append(cuadruplo_actual)
def p_AUX_ENDFOR(p):
    '''AUX_ENDFOR : empty'''
    global nivel
    agregarSaltoCuadruplo(False)
    cuadruplos[pila_saltos[nivel].pop()-1][3] = cuadruplo_actual+1
    cuadruplos[cuadruplo_actual-1][3] = pila_saltos[nivel].pop()
    nivel -= 1

# If
def p_IF_(p):
    '''IF_ : IF E AUX_IF DOS_PUNTOS EST AUX_SALIRIF ELSIF_'''
def p_ELSIF_(p):
    '''ELSIF_ : END IF AUX_ENDIF PUNTO_COMA
              | ELSE AUX_ELSE DOS_PUNTOS EST AUX_SALIRIF END IF AUX_ENDIF PUNTO_COMA
              | ELSIF AUX_ELSIF E AUX_ELSIF2 DOS_PUNTOS EST AUX_SALIRIF ELSIF_'''
def p_AUX_IF(P):
    '''AUX_IF : empty'''
    global nivel
    nivel += 2
    pila_saltos.append([])      # Agregamos nuevo nivel a la pila de saltos (para saltos del if)
    pila_saltos.append([])      # Agregamos nuevo nivel a la pila de saltos (para saltos del if)
    agregarSaltoCuadruplo(True)
    pila_saltos[nivel].append(cuadruplo_actual)
def p_AUX_ELSIF(p):
    '''AUX_ELSIF : empty'''
    global nivel
    print(pila_saltos)
    cuadruplos[pila_saltos[nivel].pop()-1][3] = cuadruplo_actual+1
    pila_saltos[nivel-1].append(cuadruplo_actual)
def p_AUX_ELSIF2(p):
    '''AUX_ELSIF2 : empty'''
    global nivel
    agregarSaltoCuadruplo(True)
    pila_saltos[nivel].append(cuadruplo_actual)
def p_AUX_ELSE(p):
    '''AUX_ELSE : empty'''
    global nivel
    agregarSaltoCuadruplo(False)
    cuadruplos[pila_saltos[nivel].pop()-1][3] = cuadruplo_actual+1
    pila_saltos[nivel].append(cuadruplo_actual)
def p_AUX_SALIRIF(p):
    '''AUX_SALIRIF : empty'''
    global nivel
    agregarSaltoCuadruplo(False)
    pila_saltos[nivel-1].append(cuadruplo_actual)
def p_AUX_ENDIF(p):
    '''AUX_ENDIF : empty'''
    global nivel
    while len(pila_saltos[nivel-1]) > 0:
        cuadruplos[pila_saltos[nivel-1].pop()-1][3] = cuadruplo_actual+1
    nivel -= 2


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
    '''MP : PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS MP_AUX V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA
          | PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS MP_AUX V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMA'''
    # agregarATablaSimbolos(p, id_index, type_index)
    agregarATablaSimbolos(p, 2, 1)
def p_MP_AUX(p):
    '''MP_AUX : empty'''
    global inicio_ejecucion
    inicio_ejecucion = cuadruplo_actual

# Programa
def p_PROGRAMA(p):
    '''PROGRAMA : PROGRAMA_H MP'''
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
    fp = open("PruebaFor.txt", "r")
    inputString = fp.read()
    fp.close()
except FileNotFoundError:
    print("Revisar PATH.")

parser.parse(inputString)
# Give the lexer some input
lexer.input(inputString)

cont = 1
for i in cuadruplos:
    print(cont, i)
    cont+=1

print("\n\n\n")

ejecucionCuadruplos()

print("\n\n\n")

for i in tabla_simbolos:
    print(i, tabla_simbolos[i])