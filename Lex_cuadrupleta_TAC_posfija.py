import ply.lex as lex
import ply.yacc as yacc


# ============================================================
# ANALIZADOR LÉXICO
# ============================================================
tokens = (
    'ENTERO',
    'VARIABLE',
    'MAS',
    'MENOS',
    'POR',
    'DIV',
    'POTENCIA',
    'IGUAL',
    'PARIZQ',
    'PARDER',
)

t_MAS = r'\+'
t_MENOS = r'-'
t_POR = r'\*'
t_DIV = r'/'
t_POTENCIA = r'\^'
t_IGUAL = r'='
t_PARIZQ = r'\('
t_PARDER = r'\)'

t_ignore = ' \t'

def t_ENTERO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_VARIABLE(t):
    r'[A-Za-z_][A-Za-z_0-9]*'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Carácter ilegal: '{t.value[0]}'")
    t.lexer.skip(1)


lexer = lex.lex()


# ============================================================
# ANALIZADOR SINTÁCTICO
# ============================================================
precedence = (
    ('left', 'MAS', 'MENOS'),
    ('left', 'POR', 'DIV'),
    ('right', 'POTENCIA'),
    ('right', 'UMINUS'),
)



    #   NODO DEL ÁRBOL
class NodoExpresion:
    def __init__(self, tipo, valor=None, izq=None, der=None):
        self.tipo = tipo
        self.valor = valor
        self.izq = izq
        self.der = der


    #   GRAMÁTICA
def p_instruccion_asignacion(p):
    'instruccion : VARIABLE IGUAL expresion'
    p[0] = NodoExpresion(
        'asignacion',
        valor=p[1],
        der=p[3]
    )

def p_instruccion_expresion(p):
    'instruccion : expresion'
    p[0] = p[1]


def p_expresion_binaria(p):
    '''expresion : expresion MAS expresion
                 | expresion MENOS expresion
                 | expresion POR expresion
                 | expresion DIV expresion
                 | expresion POTENCIA expresion'''

    operadores = {
        'MAS': '+',
        'MENOS': '-',
        'POR': '*',
        'DIV': '/',
        'POTENCIA': '^'
    }
    operador = operadores[p.slice[2].type]
    p[0] = NodoExpresion(
        operador,
        izq=p[1],
        der=p[3]
    )


def p_expresion_unaria(p):
    'expresion : MENOS expresion %prec UMINUS'
    p[0] = NodoExpresion(
        'uminus',
        der=p[2]
    )


def p_expresion_parentesis(p):
    'expresion : PARIZQ expresion PARDER'
    p[0] = p[2]


def p_expresion_entero(p):
    'expresion : ENTERO'
    p[0] = NodoExpresion(
        'num',
        valor=p[1]
    )


def p_expresion_variable(p):
    'expresion : VARIABLE'
    p[0] = NodoExpresion(
        'var',
        valor=p[1]
    )

def p_error(p):
    if p:
        print(
            f"Error sintáctico cerca de '{p.value}'"
        )
    else:
        print(
            "Error sintáctico: fin de entrada inesperado"
        )


parser = yacc.yacc()


# ============================================================
# POLACA INVERSA
# ============================================================

def generar_polaca_inversa(nodo):
    resultado = []
    def recorrer(n):
        if n.tipo == 'num':
            resultado.append(str(n.valor))
        elif n.tipo == 'var':
            resultado.append(n.valor)
        elif n.tipo == 'uminus':
            recorrer(n.der)
            resultado.append('UMINUS')
        elif n.tipo == 'asignacion':
            recorrer(n.der)
            resultado.append(n.valor)
            resultado.append('=')
        else:
            recorrer(n.izq)
            recorrer(n.der)
            resultado.append(n.tipo)
            
    recorrer(nodo)
    return resultado


# ============================================================
# TRES DIRECCIONES
# ============================================================

def generar_tres_direcciones(nodo):
    instrucciones = []
    contador = 0

    def nuevo_temporal():
        nonlocal contador
        contador += 1
        return f"T{contador}"

    def recorrer(n):
        if n.tipo == 'num':
            return str(n.valor)
        if n.tipo == 'var':
            return n.valor
        if n.tipo == 'uminus':
            operando = recorrer(n.der)
            temporal = nuevo_temporal()
            instrucciones.append(
                f"{temporal} = -{operando}"
            )
            return temporal
        if n.tipo == 'asignacion':
            resultado = recorrer(n.der)
            instrucciones.append(
                f"{n.valor} = {resultado}"
            )
            return n.valor
        izq = recorrer(n.izq)
        der = recorrer(n.der)
        temporal = nuevo_temporal()
        instrucciones.append(
            f"{temporal} = {izq} {n.tipo} {der}"
        )
        return temporal
    recorrer(nodo)
    return instrucciones


# ============================================================
# CUADRUPLETAS
# ============================================================

def generar_cuadrupletas(nodo):
    cuadrupletas = []
    contador = 0
    def nuevo_temporal():
        nonlocal contador
        contador += 1
        return f"T{contador}"

    def recorrer(n):
        if n.tipo == 'num':
            return str(n.valor)
        if n.tipo == 'var':
            return n.valor
        if n.tipo == 'uminus':
            operando = recorrer(n.der)
            temporal = nuevo_temporal()
            cuadrupletas.append(
                ('UMINUS', operando, '', temporal)
            )
            return temporal
        if n.tipo == 'asignacion':
            resultado = recorrer(n.der)
            cuadrupletas.append(
                ('=', resultado, '', n.valor)
            )
            return n.valor
        izq = recorrer(n.izq)
        der = recorrer(n.der)
        temporal = nuevo_temporal()
        cuadrupletas.append(
            (n.tipo, izq, der, temporal)
        )
        return temporal
    recorrer(nodo)
    return cuadrupletas



def procesar(entrada):
    print("\n" + "=" * 60)
    print(f"EXPRESIÓN: {entrada}")
    print("=" * 60)
    resultado = parser.parse(
        entrada,
        lexer=lexer
    )
    if resultado is None:
        print("\nNo se pudo procesar la expresión.")
        return
    
    polaca = generar_polaca_inversa(resultado)
    print("\nPOLACA INVERSA")
    print("".join(polaca))


    tres_direcciones = generar_tres_direcciones(
        resultado
    )
    print("\nTRES DIRECCIONES")
    for linea in tres_direcciones:
        print(linea)
        
    cuadrupletas = generar_cuadrupletas(
        resultado
    )
    print("\nCUADRUPLETAS")
    for cuadrupla in cuadrupletas:
        print(cuadrupla)
        

def mostrar_menu():
    print("\n" + "=" * 60)
    print("1. Probar ejemplo")
    print("2. Ingresar expresión")
    print("3. Salir")

def main():
    
    while True:
        mostrar_menu()
        opcion = input(
            "\nOpción: "
        ).strip()
        if opcion == '1':
            procesar("a+b*c+d")
        elif opcion == '2':
            entrada = input(
                "\nEscribe la expresión: "
            ).strip()
            if entrada:
                procesar(entrada)
            else:
                print("No ingresaste una expresión.")
        elif opcion == '3':
            print("Programa terminado.")
            break
        else:
            print("Opción inválida.")

if __name__ == '__main__':
    main()