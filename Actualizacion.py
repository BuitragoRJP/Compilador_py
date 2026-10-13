import ply.lex as lex
import ply.yacc as yacc
import os

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
    # Tokens agregados para factorial.py
    'DEF',
    'IF',
    'ELIF',
    'RETURN',
    'OR',
    'MAYOR',
    'IGUALDAD',
    'DOS_PUNTOS',
    'COMA',
    'NEWLINE',
    'INDENT',
    'DEDENT',
)
reservadas = {
    'def': 'DEF',
    'if': 'IF',
    'elif': 'ELIF',
    'return': 'RETURN',
    'or': 'OR',
}
t_MAS = r'\+'
t_MENOS = r'-'
t_POR = r'\*'
t_DIV = r'/'
t_POTENCIA = r'\^'
t_IGUALDAD = r'=='
t_IGUAL = r'='
t_MAYOR = r'>'
t_PARIZQ = r'\('
t_PARDER = r'\)'
t_DOS_PUNTOS = r':'
t_COMA = r','
t_ignore = ' \t'

def t_ENTERO(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_VARIABLE(t):
    r'[A-Za-z_][A-Za-z_0-9]*'
    t.type = reservadas.get(t.value, 'VARIABLE')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Carácter ilegal: '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# ============================================================
# INDENTACIÓN
# ============================================================
class LexerIndentacion:
    def __init__(self, lexer_base):
        self.lexer_base = lexer_base
        self.lineas = []
        self.indentaciones = [0]
        self.cola = []
        self.posicion = 0
        self.fin = False
        self.numero_linea = 0

    def input(self, texto):
        self.lineas = texto.splitlines()
        self.indentaciones = [0]
        self.cola = []
        self.posicion = 0
        self.fin = False
        self.numero_linea = 0

    def token(self):
        while not self.cola:

            if self.fin:
                return None

            if self.posicion >= len(self.lineas):
                while len(self.indentaciones) > 1:
                    self.indentaciones.pop()

                    token = lex.LexToken()
                    token.type = 'DEDENT'
                    token.value = ''
                    token.lineno = self.numero_linea + 1
                    token.lexpos = 0
                    self.cola.append(token)

                self.fin = True

                if not self.cola:
                    return None

                break

            linea = self.lineas[self.posicion]
            self.posicion += 1
            self.numero_linea += 1

            if not linea.strip():
                continue

            espacios = len(linea) - len(linea.lstrip(' '))
            contenido = linea.lstrip(' ')

            # Reconocer INDENT y DEDENT
            if espacios > self.indentaciones[-1]:
                self.indentaciones.append(espacios)

                token = lex.LexToken()
                token.type = 'INDENT'
                token.value = ''
                token.lineno = self.numero_linea
                token.lexpos = 0
                self.cola.append(token)

            elif espacios < self.indentaciones[-1]:
                while (
                    len(self.indentaciones) > 1
                    and espacios < self.indentaciones[-1]
                ):
                    self.indentaciones.pop()

                    token = lex.LexToken()
                    token.type = 'DEDENT'
                    token.value = ''
                    token.lineno = self.numero_linea
                    token.lexpos = 0
                    self.cola.append(token)

                if espacios != self.indentaciones[-1]:
                    raise SyntaxError(
                        f"Indentación inválida en línea "
                        f"{self.numero_linea}"
                    )

            # Tokenizar el contenido de la línea mediante PLY
            self.lexer_base.lineno = self.numero_linea
            self.lexer_base.input(contenido)

            while True:
                token = self.lexer_base.token()

                if token is None:
                    break

                token.lineno = self.numero_linea
                self.cola.append(token)

            # Generar NEWLINE para cada línea no vacía
            token = lex.LexToken()
            token.type = 'NEWLINE'
            token.value = '\n'
            token.lineno = self.numero_linea
            token.lexpos = len(linea)
            self.cola.append(token)

        return self.cola.pop(0)


# ============================================================
# ANALIZADOR SINTÁCTICO
# ============================================================

precedence = (
    ('left', 'OR'),
    ('left', 'IGUALDAD', 'MAYOR'),
    ('left', 'MAS', 'MENOS'),
    ('left', 'POR', 'DIV'),
    ('right', 'POTENCIA'),
    ('right', 'UMINUS'),
)


# NODO DEL ÁRBOL
class NodoExpresion:
    def __init__(self, tipo, valor=None, izq=None, der=None):
        self.tipo = tipo
        self.valor = valor
        self.izq = izq
        self.der = der


# Nodos adicionales para factorial.py
class NodoFuncion:
    def __init__(self, nombre, parametros, cuerpo):
        self.nombre = nombre
        self.parametros = parametros
        self.cuerpo = cuerpo


class NodoIf:
    def __init__(self, condicion, cuerpo, alternativo=None):
        self.condicion = condicion
        self.cuerpo = cuerpo
        self.alternativo = alternativo


class NodoReturn:
    def __init__(self, expresion):
        self.expresion = expresion


class NodoLlamada:
    def __init__(self, nombre, argumentos):
        self.nombre = nombre
        self.argumentos = argumentos


class NodoPrint:
    def __init__(self, expresion):
        self.expresion = expresion


class NodoPrograma:
    def __init__(self, sentencias):
        self.sentencias = sentencias


# GRAMÁTICA

# GRAMÁTICA

def p_programa(p):
    'programa : lista_sentencias'
    p[0] = NodoPrograma(p[1])


def p_lista_sentencias_unica(p):
    'lista_sentencias : sentencia'
    p[0] = [p[1]]


def p_lista_sentencias_multiple(p):
    'lista_sentencias : lista_sentencias sentencia'
    p[0] = p[1] + [p[2]]


def p_sentencia_instruccion(p):
    'sentencia : instruccion NEWLINE'
    p[0] = p[1]


def p_sentencia_funcion(p):
    'sentencia : funcion'
    p[0] = p[1]


def p_sentencia_if(p):
    'sentencia : sentencia_if'
    p[0] = p[1]


def p_sentencia_return(p):
    'sentencia : RETURN expresion NEWLINE'
    p[0] = NodoReturn(p[2])


def p_funcion(p):
    'funcion : DEF VARIABLE PARIZQ parametros PARDER DOS_PUNTOS NEWLINE INDENT lista_sentencias DEDENT'
    p[0] = NodoFuncion(p[2], p[4], p[9])


def p_parametros_vacios(p):
    'parametros : empty'
    p[0] = []


def p_parametros_uno(p):
    'parametros : VARIABLE'
    p[0] = [p[1]]


def p_parametros_multiple(p):
    'parametros : parametros COMA VARIABLE'
    p[0] = p[1] + [p[3]]


def p_sentencia_if_regla(p):
    'sentencia_if : IF expresion DOS_PUNTOS NEWLINE INDENT lista_sentencias DEDENT elif_lista'
    p[0] = NodoIf(p[2], p[6], p[8])


def p_elif_lista_vacia(p):
    'elif_lista : empty'
    p[0] = None


def p_elif_lista(p):
    'elif_lista : ELIF expresion DOS_PUNTOS NEWLINE INDENT lista_sentencias DEDENT elif_lista'
    p[0] = NodoIf(p[2], p[6], p[8])


def p_instruccion_asignacion(p):
    'instruccion : VARIABLE IGUAL expresion'
    p[0] = NodoExpresion(
        'asignacion',
        valor=p[1],
        der=p[3]
    )


def p_instruccion_expresion(p):
    'instruccion : expresion'
    if isinstance(p[1], NodoLlamada) and p[1].nombre == 'print':
        p[0] = NodoPrint(p[1].argumentos[0])
    else:
        p[0] = p[1]


def p_expresion_binaria(p):
    '''expresion : expresion MAS expresion
                 | expresion MENOS expresion
                 | expresion POR expresion
                 | expresion DIV expresion
                 | expresion POTENCIA expresion
                 | expresion IGUALDAD expresion
                 | expresion MAYOR expresion
                 | expresion OR expresion'''

    operadores = {
        'MAS': '+',
        'MENOS': '-',
        'POR': '*',
        'DIV': '/',
        'POTENCIA': '^',
        'IGUALDAD': '==',
        'MAYOR': '>',
        'OR': 'or',
    }

    operador = operadores[p.slice[2].type]

    p[0] = NodoExpresion(
        operador,
        izq=p[1],
        der=p[3]
    )


def p_expresion_unaria(p):
    'expresion : MENOS expresion %prec UMINUS'
    p[0] = NodoExpresion('uminus', der=p[2])


def p_expresion_parentesis(p):
    'expresion : PARIZQ expresion PARDER'
    p[0] = p[2]


def p_expresion_entero(p):
    'expresion : ENTERO'
    p[0] = NodoExpresion('num', valor=p[1])


def p_expresion_variable(p):
    'expresion : VARIABLE'
    p[0] = NodoExpresion('var', valor=p[1])


def p_expresion_llamada(p):
    'expresion : VARIABLE PARIZQ argumentos_llamada PARDER'
    p[0] = NodoLlamada(p[1], p[3])


def p_argumentos_llamada_vacios(p):
    'argumentos_llamada : empty'
    p[0] = []


def p_argumentos_llamada_uno(p):
    'argumentos_llamada : expresion'
    p[0] = [p[1]]


def p_argumentos_llamada_multiple(p):
    'argumentos_llamada : argumentos_llamada COMA expresion'
    p[0] = p[1] + [p[3]]


def p_empty(p):
    'empty :'
    p[0] = None


def p_error(p):
    if p:
        print(
            f"Error sintáctico cerca de '{p.value}' "
            f"en la línea {p.lineno}"
        )
    else:
        print("Error sintáctico: fin de entrada inesperado")



# Parser para expresiones y parser para el archivo
parser = yacc.yacc(start='instruccion', debug=False)
parser_factorial = yacc.yacc(
    start='programa',
    debug=False,
    tabmodule='parsetab_factorial'
)


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


# ============================================================
# GENERADOR DINAMICO DE TRES DIRECCIONES PARA factorial.py
# ============================================================

class GeneradorFactorial:
    def __init__(self):
        self.instrucciones = []
        self.contador_temporal = 0
        self.contador_etiqueta = 0

    def nuevo_temporal(self):
        self.contador_temporal += 1
        return f"T{self.contador_temporal}"

    def nueva_etiqueta(self):
        self.contador_etiqueta += 1
        return f"L{self.contador_etiqueta}"

    def emitir(self, instruccion):
        self.instrucciones.append(instruccion)

   
    def expresion(self, nodo):
        # Llamadas a funciones, incluida la recursion
        if isinstance(nodo, NodoLlamada):
            argumentos = [
                self.expresion(arg)
                for arg in nodo.argumentos
            ]

            for argumento in argumentos:
                self.emitir(f"param {argumento}")

            temporal = self.nuevo_temporal()
            self.emitir(
                f"{temporal} = call {nodo.nombre}, {len(argumentos)}"
            )
            return temporal

        if nodo.tipo == 'num':
            return str(nodo.valor)

        if nodo.tipo == 'var':
            return nodo.valor

        if nodo.tipo == 'uminus':
            operando = self.expresion(nodo.der)
            temporal = self.nuevo_temporal()
            self.emitir(f"{temporal} = -{operando}")
            return temporal

        if nodo.tipo == 'asignacion':
            valor = self.expresion(nodo.der)
            self.emitir(f"{nodo.valor} = {valor}")
            return nodo.valor

        if nodo.tipo in ('+', '-', '*', '/', '^', '==', '>', 'or'):
            izq = self.expresion(nodo.izq)
            der = self.expresion(nodo.der)

            temporal = self.nuevo_temporal()
            self.emitir(
                f"{temporal} = {izq} {nodo.tipo} {der}"
            )
            return temporal

        raise ValueError(
            f"Expresión no soportada: {type(nodo).__name__}"
        )


    def sentencias(self, nodos):
        for nodo in nodos:

            if isinstance(nodo, NodoExpresion):
                self.expresion(nodo)

            elif isinstance(nodo, NodoReturn):
                valor = self.expresion(nodo.expresion)
                self.emitir(f"return {valor}")

            elif isinstance(nodo, NodoPrint):
                valor = self.expresion(nodo.expresion)
                self.emitir(f"print {valor}")

            elif isinstance(nodo, NodoIf):
                condicion = self.expresion(nodo.condicion)

                etiqueta_siguiente = self.nueva_etiqueta()
                etiqueta_fin = self.nueva_etiqueta()

                self.emitir(
                    f"ifFalse {condicion} goto {etiqueta_siguiente}"
                )

                self.sentencias(nodo.cuerpo)
                self.emitir(f"goto {etiqueta_fin}")
                self.emitir(f"{etiqueta_siguiente}:")

                if nodo.alternativo is not None:
                    if isinstance(nodo.alternativo, NodoIf):
                        self.sentencias([nodo.alternativo])
                    else:
                        self.sentencias(nodo.alternativo)

                self.emitir(f"{etiqueta_fin}:")

            elif isinstance(nodo, NodoFuncion):
                self.emitir(f"function {nodo.nombre}:")

                self.sentencias(nodo.cuerpo)

                self.emitir(f"end function {nodo.nombre}")

            else:
                raise ValueError(
                    f"Instrucción no soportada: {type(nodo).__name__}"
                )

    def generar(self, arbol):
        self.sentencias(arbol.sentencias)
        return self.instrucciones


# ============================================================
# EXPORTAR ARCHIVO TXT
# ============================================================

def exportar_codigo(codigo):
    nombre = "factorial_py.txt"
    contador = 1

    while os.path.exists(nombre):
        nombre = f"factorial_py({contador}).txt"
        contador += 1

    with open(nombre, "w", encoding="utf-8") as archivo:
        archivo.write("\n".join(codigo) + "\n")

    print(f"\nArchivo exportado correctamente: {nombre}")


# ============================================================
# PROCESAR ARCHIVO factorial.py
# ============================================================

def procesar_archivo_factorial():
    nombre_archivo = "factorial.py"

    if not os.path.isfile(nombre_archivo):
        print(f"No se encontró el archivo: {nombre_archivo}")
        print("Coloca factorial.py en el directorio de ejecución.")
        return

    try:
        with open(nombre_archivo, "r", encoding="utf-8") as archivo:
            fuente = archivo.read()

        print("\nANALISIS LEXICO")

        lexer_archivo = LexerIndentacion(lexer)
        lexer_archivo.input(fuente)

        # Mostrar los tokens sin consumir el analizador
        # que se utilizará posteriormente.
        while True:
            token = lexer_archivo.token()

            if token is None:
                break

            print(
                f"{token.type:<12} "
                f"{str(token.value):<20} "
                f"línea {token.lineno}"
            )

        # Analisis sintactico con PLY
        lexer_archivo.input(fuente)
        resultado = parser_factorial.parse(
            lexer=lexer_archivo
        )

        if resultado is None:
            print("\nNo se pudo analizar factorial.py.")
            return

        # Generacion dinamica de tres direcciones
        generador = GeneradorFactorial()
        codigo = generador.generar(resultado)

        print("\nTRES DIRECCIONES")
        for linea in codigo:
            print(linea)

        exportar_codigo(codigo)

    except (SyntaxError, ValueError, OSError) as error:
        print(f"\nError al procesar el archivo: {error}")


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
    print(" ".join(polaca))

    tres_direcciones = generar_tres_direcciones(resultado)
    print("\nTRES DIRECCIONES")
    for linea in tres_direcciones:
        print(linea)

    cuadrupletas = generar_cuadrupletas(resultado)
    print("\nCUADRUPLETAS")
    for cuadrupla in cuadrupletas:
        print(cuadrupla)


def mostrar_menu():
    print("\n" + "=" * 60)
    print("1. Probar ejemplo")
    print("2. Ingresar expresión")
    print("3. Procesar archivo factorial.py")
    print("4. Salir")


def main():
    while True:
        mostrar_menu()

        opcion = input("\nOpción: ").strip()

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
            procesar_archivo_factorial()

        elif opcion == '4':
            print("Programa terminado.")
            break

        else:
            print("Opción inválida.")


if __name__ == '__main__':
    main()
