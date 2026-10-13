# -*- coding: utf-8 -*-

import ply.lex as lex
import ply.yacc as yacc

# ============================================================
# 1. ANALIZADOR LÉXICO (LEXER)
# ============================================================

tokens = (
    'ENTERO',
    'ID',
    'ZUWEISEN', # palabra de cambio para el = o Asignar como estaba definido
    'ZUSATZ', # palabra de cambio para el + o Suma como estaba definido
    'SUBTRAHIEREN', # palabra de cambio para el - o Resta como estaba definido
    'VON', # palabra de cambio para el * o Multiplicacion como estaba definido
    'TEILT', # palabra de cambio para el / o Division como estaba definido
    'HOCH', # palabra de cambio para el ^ o Potencia como estaba definido
    'FREI', # palabra de cambio para el XOR o Excluyente como estaba definido
    'PARIZQ',
    'PARDER',
)

# Palabras reservadas
palabras_reservadas = {
    'ZUWEISEN': 'ZUWEISEN',
    'ZUSATZ': 'ZUSATZ',
    'SUBTRAHIEREN': 'SUBTRAHIEREN',
    'VON': 'VON',
    'TEILT': 'TEILT',
    'HOCH': 'HOCH',
    'FREI': 'FREI',
}


t_ZUWEISEN   = r'ZUWEISEN'
t_ZUSATZ     = r'ZUSATZ'
t_SUBTRAHIEREN     = r'SUBTRAHIEREN'
t_VON       = r'VON'
t_TEILT  = r'TEILT'
t_HOCH  = r'HOCH'
t_PARIZQ    = r'\('
t_PARDER    = r'\)'

t_ignore = " \t"

# ---- Reglas ----
def t_FREI(t):
    r'FREI'
    return t

def t_ENTERO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[A-Za-z_][A-Za-z_0-9]*'

    t.type = palabras_reservadas.get(t.value, 'ID')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Carácter ilegal '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

# ============================================================
# 2. ANALIZADOR SINTÁCTICO
# ============================================================
# Prioridad de menor a mayor
precedence = (
    ('left', 'ZUSATZ', 'SUBTRAHIEREN'),
    ('left', 'VON', 'TEILT'),
    ('left', 'FREI'),
    ('right', 'HOCH'),
    ('right', 'UMENOS'),
)

# Estructura de nodos para imprimir el arbol sintactico
class NodoSintactico:
    def __init__(self, simbolo, hijos=None, valor=None):
        self.simbolo = simbolo      # nombre del símbolo gramatical (no terminal o token)
        self.hijos = hijos or []    # lista de NodoSintactico (vacío si es terminal)
        self.valor = valor          # valor literal si es terminal (token)

    def es_terminal(self):
        return len(self.hijos) == 0


# Estructura de nodos para imprimir el arbol de expresion
class NodoExpresion:
    def __init__(self, tipo, valor=None, izq=None, der=None):
        self.tipo = tipo    # 'num', 'var', 'asignacion' o el operador '+','-','*','/','^','XOR','uminus'
        self.valor = valor  # valor de literal / nombre de variable
        self.izq = izq
        self.der = der


tabla_simbolos = {}

# ---------------- Reglas de la gramática ----------------

def p_instruccion_asignacion(p):
    'instruccion : ID ZUWEISEN expresion'
    valor = evaluar(p[3])
    tabla_simbolos[p[1]] = valor

    ast = NodoExpresion('asignacion', valor=p[1], der=p[3][0])
    parse_node = NodoSintactico('instruccion', [
        NodoSintactico('ID', valor=p[1]),
        NodoSintactico('ZUWEISEN', valor='ZUWEISEN'),
        p[3][1],
    ])
    p[0] = (ast, parse_node)
    print(f"[Resultado] {p[1]} = {valor}")


def p_instruccion_expresion(p):
    'instruccion : expresion'
    valor = evaluar(p[1])
    ast = p[1][0]
    parse_node = NodoSintactico('instruccion', [p[1][1]])
    p[0] = (ast, parse_node)
    print(f"[Resultado] {valor}")


def p_expresion_binaria(p):
    '''expresion : expresion ZUSATZ expresion
                  | expresion SUBTRAHIEREN expresion
                  | expresion VON expresion
                  | expresion TEILT expresion
                  | expresion HOCH expresion
                  | expresion FREI expresion'''

    operadores = {
        'ZUSATZ': '+',
        'SUBTRAHIEREN': '-',
        'VON': '*',
        'TEILT': '/',
        'HOCH': '^',
        'FREI': 'XOR'
    }

    operador = operadores[p.slice[2].type]

    ast = NodoExpresion(
        operador,
        izq=p[1][0],
        der=p[3][0]
    )

    parse_node = NodoSintactico('expresion', [
        p[1][1],
        NodoSintactico(p.slice[2].type, valor=p[2]),
        p[3][1],
    ])

    p[0] = (ast, parse_node)


def p_expresion_unaria(p):
    'expresion : SUBTRAHIEREN expresion %prec UMENOS'
    ast = NodoExpresion('uminus', der=p[2][0])
    parse_node = NodoSintactico('expresion', [
        NodoSintactico('SUBTRAHIEREN', valor='-'),
        p[2][1],
    ])
    p[0] = (ast, parse_node)


def p_expresion_agrupacion(p):
    'expresion : PARIZQ expresion PARDER'
    ast = p[2][0]
    parse_node = NodoSintactico('expresion', [
        NodoSintactico('PARIZQ', valor='('),
        p[2][1],
        NodoSintactico('PARDER', valor=')'),
    ])
    p[0] = (ast, parse_node)


def p_expresion_entero(p):
    'expresion : ENTERO'
    ast = NodoExpresion('num', valor=p[1])
    parse_node = NodoSintactico('expresion', [
        NodoSintactico('ENTERO', valor=p[1])
    ])
    p[0] = (ast, parse_node)


def p_expresion_id(p):
    'expresion : ID'
    ast = NodoExpresion('var', valor=p[1])
    parse_node = NodoSintactico('expresion', [
        NodoSintactico('ID', valor=p[1])
    ])
    p[0] = (ast, parse_node)


def p_error(p):
    if p:
        print("Error sintáctico en '%s'" % p.value)
    else:
        print("Error sintáctico: fin de entrada inesperado")


parser = yacc.yacc()

# ============================================================
# 3. EVALUADOR DEL ÁRBOL DE EXPRESIÓN
# ============================================================
def evaluar(nodo_tuple):
    nodo = nodo_tuple[0] if isinstance(nodo_tuple, tuple) else nodo_tuple

    if nodo.tipo == 'num':
        return nodo.valor
    if nodo.tipo == 'var':
        return tabla_simbolos.get(nodo.valor, 0)
    if nodo.tipo == 'uminus':
        return -evaluar(nodo.der)
    if nodo.tipo == 'asignacion':
        return evaluar(nodo.der)

    izq = evaluar(nodo.izq)
    der = evaluar(nodo.der)
    if nodo.tipo == '+':   return izq + der
    if nodo.tipo == '-':   return izq - der
    if nodo.tipo == '*':   return izq * der
    if nodo.tipo == '/':   return izq / der
    if nodo.tipo == '^':   return izq ** der
    if nodo.tipo == 'XOR': return izq ^ der
    raise ValueError(f"Operador desconocido: {nodo.tipo}")


# ============================================================
# 4. IMPRESIÓN DE ÁRBOLES 
# ============================================================
def etiqueta_expresion(nodo):
    if nodo.tipo == 'num':
        return str(nodo.valor)

    if nodo.tipo == 'var':
        return nodo.valor

    if nodo.tipo == 'asignacion':
        return f"= ({nodo.valor})"

    if nodo.tipo == 'uminus':
        return "-"

    return nodo.tipo


def imprimir_arbol_expresion(nodo, prefijo="", es_ultimo=True, es_raiz=True):
    etiqueta = etiqueta_expresion(nodo)
    conector = "" if es_raiz else ("└── " if es_ultimo else "├── ")
    print(prefijo + conector + etiqueta)

    hijos = []
    if nodo.tipo == 'asignacion':
        hijos = [nodo.der]
    elif nodo.tipo == 'uminus':
        hijos = [nodo.der]
    elif nodo.izq is not None and nodo.der is not None:
        hijos = [nodo.izq, nodo.der]

    nuevo_prefijo = prefijo if es_raiz else prefijo + ("    " if es_ultimo else "│   ")
    for i, h in enumerate(hijos):
        imprimir_arbol_expresion(h, nuevo_prefijo, i == len(hijos) - 1, False)


def etiqueta_sintactica(nodo):
    if nodo.es_terminal():
        return f"{nodo.simbolo} ('{nodo.valor}')"
    return f"<{nodo.simbolo}>"


def imprimir_arbol_sintactico(nodo, prefijo="", es_ultimo=True, es_raiz=True):
    etiqueta = etiqueta_sintactica(nodo)
    conector = "" if es_raiz else ("└── " if es_ultimo else "├── ")
    print(prefijo + conector + etiqueta)

    nuevo_prefijo = prefijo if es_raiz else prefijo + ("    " if es_ultimo else "│   ")
    for i, h in enumerate(nodo.hijos):
        imprimir_arbol_sintactico(h, nuevo_prefijo, i == len(nodo.hijos) - 1, False)


# ============================================================
# Ciclo principal
# ============================================================
def procesar(entrada, titulo):
    print("=" * 70)
    print(f"Entrada:  {titulo}")
    print("=" * 70)
    tabla_simbolos.clear()
    lexer.lineno = 1
    resultado = parser.parse(entrada, lexer=lexer)

    if resultado is None:
        print("No se pudo procesar la entrada, revisa la sintaxis.\n")
        return

    ast, parse_tree = resultado

    print("\nNOTACIONES")
    

    print("\nÁRBOL DE EXPRESIÓN")
    imprimir_arbol_expresion(ast)

    print("\nÁRBOL SINTÁCTICO")
    imprimir_arbol_sintactico(parse_tree)
    print()


# ============================================================
# MENÚ
# ============================================================
def ejecutar_ejercicios_practica():
    procesar(
        "X ZUWEISEN ((10 ZUSATZ 6) TEILT (4 VON 2)) HOCH 2 SUBTRAHIEREN (15 TEILT 3)",
        "X ZUWEISEN ((10 ZUSATZ 6) TEILT (4 VON 2)) HOCH 2 SUBTRAHIEREN (15 TEILT 3)"
    )
    procesar(
        "10 FREI 12",
        "10 FREI 12"
    )


def ingresar_ecuacion_manual():
    print("\nEscribe tu ecuación usando las palabras reservadas del lenguaje:")
    print("  ZUWEISEN = asignar (=)      ZUSATZ = suma (+)")
    print("  SUBTRAHIEREN = resta (-)    VON = multiplicación (*)")
    print("  TEILT = división (/)        HOCH = potencia (^)")
    print("  FREI = XOR                  ( ) = paréntesis")
    print("Ejemplo: X ZUWEISEN (10 ZUSATZ 6) HOCH 2\n")

    entrada = input("Ecuación: ").strip()
    if not entrada:
        print("[Aviso] No ingresaste ninguna ecuación.\n")
        return

    try:
        procesar(entrada, entrada)
    except Exception as e:
        print(f"No se pudo procesar la ecuación: {e}\n")


def mostrar_menu():
    print("\n" + "=" * 70)
    print("MENÚ - Analizador Léxico y SintácticO")
    print("=" * 70)
    print("1. Mostrar ejercicios de la práctica")
    print("2. Ingresar ecuación manual")
    print("3. Salir del programa")


def main():
    while True:
        mostrar_menu()
        opcion = input("Selecciona una opción (1-3): ").strip()

        if opcion == "1":
            ejecutar_ejercicios_practica()
        elif opcion == "2":
            ingresar_ecuacion_manual()
        elif opcion == "3":
            print("Programa terminado")
            break
        else:
            print("Opción inválida, intenta de nuevo.\n")


if __name__ == "__main__":
    main()
