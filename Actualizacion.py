
import os
import ply.lex as lex
import ply.yacc as yacc


# ============================================================
# 1. ANALISIS LEXICO CON PLY
# ============================================================

reserved = {
    "def": "DEF",
    "if": "IF",
    "elif": "ELIF",
    "return": "RETURN",
    "print": "PRINT",
    "or": "OR",
    "and": "AND",
    "not": "NOT",
}

tokens = [
    "NAME", "NUMBER",
    "PLUS", "MINUS", "TIMES", "DIVIDE",
    "EQUALS", "EQ", "NE", "GT", "LT", "GE", "LE",
    "LPAREN", "RPAREN", "COLON", "COMMA",
] + list(reserved.values())

# Los operadores mas largos deben reconocerse antes que los cortos.
t_GE = r">="
t_LE = r"<="
t_EQ = r"=="
t_NE = r"!="
t_PLUS = r"\+"
t_MINUS = r"-"
t_TIMES = r"\*"
t_DIVIDE = r"/"
t_EQUALS = r"="
t_GT = r">"
t_LT = r"<"
t_LPAREN = r"\("
t_RPAREN = r"\)"
t_COLON = r":"
t_COMMA = r","

# Para esta practica se ignoran espacios, tabuladores y saltos
# de linea. La gramatica controla el orden de las instrucciones.
t_ignore = " \t\r\n"


def t_COMMENT(t):
    r"\#[^\n]*"
    pass


def t_NUMBER(t):
    r"\d+"
    t.value = int(t.value)
    return t


def t_NAME(t):
    r"[a-zA-Z_][a-zA-Z_0-9]*"
    t.type = reserved.get(t.value, "NAME")
    return t


def t_error(t):
    raise SyntaxError(
        f"Caracter no reconocido: {t.value[0]!r}"
    )


lexer = lex.lex()


# ============================================================
# 2. GRAMATICA DE EXPRESIONES
# ============================================================

precedence = (
    ("left", "OR"),
    ("left", "AND"),
    ("left", "EQ", "NE", "GT", "LT", "GE", "LE"),
    ("left", "PLUS", "MINUS"),
    ("left", "TIMES", "DIVIDE"),
    ("right", "NOT", "UMINUS"),
)


def p_expresion_binaria(p):
    """expresion : expresion PLUS expresion
                 | expresion MINUS expresion
                 | expresion TIMES expresion
                 | expresion DIVIDE expresion
                 | expresion EQ expresion
                 | expresion NE expresion
                 | expresion GT expresion
                 | expresion LT expresion
                 | expresion GE expresion
                 | expresion LE expresion
                 | expresion OR expresion
                 | expresion AND expresion"""
    p[0] = ("op", p[2], p[1], p[3])


def p_expresion_not(p):
    "expresion : NOT expresion"
    p[0] = ("op", "not", p[2], None)


def p_expresion_menos_unario(p):
    "expresion : MINUS expresion %prec UMINUS"
    p[0] = ("op", "u-", p[2], None)


def p_expresion_parentesis(p):
    "expresion : LPAREN expresion RPAREN"
    p[0] = p[2]


def p_expresion_numero(p):
    "expresion : NUMBER"
    p[0] = ("num", p[1])


def p_expresion_nombre(p):
    "expresion : NAME"
    p[0] = ("id", p[1])


def p_expresion_llamada(p):
    "expresion : NAME LPAREN argumentos_opt RPAREN"
    p[0] = ("call", p[1], p[3])


def p_argumentos_opt(p):
    """argumentos_opt : argumentos
                      | empty"""
    p[0] = p[1]


def p_argumentos_uno(p):
    "argumentos : expresion"
    p[0] = [p[1]]


def p_argumentos_varios(p):
    "argumentos : argumentos COMMA expresion"
    p[0] = p[1] + [p[3]]


def p_empty(p):
    "empty :"
    p[0] = []


# ============================================================
# 3. GRAMATICA DEL PROGRAMA FACTORIAL
# ============================================================


def p_programa(p):
    """programa : DEF NAME LPAREN NAME RPAREN COLON IF expresion COLON NAME EQUALS expresion ELIF expresion COLON NAME EQUALS expresion RETURN expresion NAME EQUALS NAME LPAREN NUMBER RPAREN PRINT LPAREN NAME RPAREN"""

    p[0] = {
        "funcion": p[2],
        "parametro": p[4],
        "condicion_if": p[8],
        "asignacion_if": (p[10], p[12]),
        "condicion_elif": p[14],
        "asignacion_elif": (p[16], p[18]),
        "retorno": p[20],
        "variable_final": p[21],
        "funcion_llamada": p[23],
        "argumento_final": p[25],
        "variable_print": p[29],
    }



def p_error(p):
    if p:
        raise SyntaxError(
            f"Error sintactico cerca de {p.value!r}"
        )
    raise SyntaxError("Fin inesperado de la entrada")


# Los dos parsers utilizan las mismas reglas PLY, pero distintos
# simbolos iniciales.
parser_expresiones = yacc.yacc(
    start="expresion",
    debug=False,
    write_tables=False,
)

parser_factorial = yacc.yacc(
    start="programa",
    debug=False,
    write_tables=False,
)


def analizar_expresion(texto):
    return parser_expresiones.parse(
        texto, lexer=lexer, tracking=True
    )


def analizar_factorial(texto):
    return parser_factorial.parse(
        texto, lexer=lexer, tracking=True
    )


# ============================================================
# 4. REPRESENTACIONES DE EXPRESIONES
# ============================================================

def a_texto(nodo):
    tipo = nodo[0]

    if tipo == "num":
        return str(nodo[1])

    if tipo == "id":
        return nodo[1]

    if tipo == "call":
        args = ", ".join(a_texto(x) for x in nodo[2])
        return f"{nodo[1]}({args})"

    if tipo == "op":
        operador, izquierda, derecha = nodo[1:]

        if operador == "not":
            return f"not {a_texto(izquierda)}"

        if operador == "u-":
            return f"-{a_texto(izquierda)}"

        return (
            f"({a_texto(izquierda)} "
            f"{operador} {a_texto(derecha)})"
        )

    raise ValueError(f"Nodo desconocido: {nodo}")


def polaca(nodo):
    tipo = nodo[0]

    if tipo in ("num", "id"):
        return [str(nodo[1])]

    if tipo == "call":
        salida = []
        for argumento in nodo[2]:
            salida.extend(polaca(argumento))
        salida.append(f"CALL {nodo[1]}")
        return salida

    if tipo == "op":
        operador, izquierda, derecha = nodo[1:]

        if operador in ("not", "u-"):
            return polaca(izquierda) + [operador]

        return (
            polaca(izquierda)
            + polaca(derecha)
            + [operador]
        )

    raise ValueError(f"Nodo desconocido: {nodo}")


def generar_tac_expresion(nodo):
    lineas = []
    contador = [0]

    def temporal():
        contador[0] += 1
        return f"t{contador[0]}"

    def recorrer(actual):
        tipo = actual[0]

        if tipo == "num":
            return str(actual[1])

        if tipo == "id":
            return actual[1]

        if tipo == "call":
            argumentos = [
                recorrer(x) for x in actual[2]
            ]

            for argumento in argumentos:
                lineas.append(f"param {argumento}")

            t = temporal()
            lineas.append(
                f"{t} = call {actual[1]}, {len(argumentos)}"
            )
            return t

        if tipo == "op":
            operador, izquierda, derecha = actual[1:]
            izq = recorrer(izquierda)

            if operador in ("not", "u-"):
                t = temporal()
                lineas.append(f"{t} = {operador} {izq}")
                return t

            der = recorrer(derecha)
            t = temporal()
            lineas.append(f"{t} = {izq} {operador} {der}")
            return t

        raise ValueError(f"Nodo desconocido: {actual}")

    resultado = recorrer(nodo)
    lineas.append(f"resultado = {resultado}")
    return lineas


def generar_cuadruplas(nodo):
    lineas = []
    contador = [0]

    def temporal():
        contador[0] += 1
        return f"t{contador[0]}"

    def recorrer(actual):
        tipo = actual[0]

        if tipo == "num":
            return str(actual[1])

        if tipo == "id":
            return actual[1]

        if tipo == "call":
            argumentos = [
                recorrer(x) for x in actual[2]
            ]

            for argumento in argumentos:
                lineas.append(("PARAM", argumento, "-", "-"))

            t = temporal()
            lineas.append(
                ("CALL", actual[1], str(len(argumentos)), t)
            )
            return t

        if tipo == "op":
            operador, izquierda, derecha = actual[1:]
            izq = recorrer(izquierda)

            if operador in ("not", "u-"):
                t = temporal()
                lineas.append((operador, izq, "-", t))
                return t

            der = recorrer(derecha)
            t = temporal()
            lineas.append((operador, izq, der, t))
            return t

        raise ValueError(f"Nodo desconocido: {actual}")

    resultado = recorrer(nodo)
    lineas.append(("=", resultado, "-", "resultado"))
    return lineas


def mostrar_resultados(nodo):
    print("\nExpresion:")
    print(a_texto(nodo))

    print("\nNotacion polaca inversa:")
    print(" ".join(polaca(nodo)))

    print("\nCodigo de tres direcciones:")
    for linea in generar_tac_expresion(nodo):
        print(linea)

    print("\nCuadruplas:")
    print(f"{'Operador':<12}{'Arg1':<12}{'Arg2':<12}{'Resultado':<12}")

    for op, arg1, arg2, res in generar_cuadruplas(nodo):
        print(f"{op:<12}{arg1:<12}{arg2:<12}{res:<12}")


# ============================================================
# 5. GENERADOR DINAMICO DE TAC PARA FACTORIAL
# ============================================================

class GeneradorFactorial:
    def __init__(self, programa):
        self.programa = programa
        self.lineas = []
        self.contador = 0
        self.etiqueta_contador = 0

    def emitir(self, linea):
        self.lineas.append(linea)

    def temporal(self):
        self.contador += 1
        return f"t{self.contador}"

    def etiqueta(self):
        self.etiqueta_contador += 1
        return f"L{self.etiqueta_contador}"

    def expresion(self, nodo):
        tipo = nodo[0]

        if tipo == "num":
            return str(nodo[1])

        if tipo == "id":
            return nodo[1]

        if tipo == "call":
            argumentos = [
                self.expresion(x) for x in nodo[2]
            ]

            for argumento in argumentos:
                self.emitir(f"param {argumento}")

            t = self.temporal()
            self.emitir(
                f"{t} = call {nodo[1]}, {len(argumentos)}"
            )
            return t

        if tipo == "op":
            operador, izquierda, derecha = nodo[1:]
            izq = self.expresion(izquierda)

            if operador in ("not", "u-"):
                t = self.temporal()
                self.emitir(f"{t} = {operador} {izq}")
                return t

            der = self.expresion(derecha)
            t = self.temporal()
            self.emitir(f"{t} = {izq} {operador} {der}")
            return t

        raise ValueError(f"Expresion no soportada: {nodo}")

    def condicion(self, nodo, verdadera, falsa):
        # Genera saltos para las condiciones logicas sin
        # evaluar innecesariamente ambos lados de OR/AND.
        if nodo[0] == "op" and nodo[1] == "or":
            siguiente = self.etiqueta()
            self.condicion(nodo[2], verdadera, siguiente)
            self.emitir(f"{siguiente}:")
            self.condicion(nodo[3], verdadera, falsa)
            return

        if nodo[0] == "op" and nodo[1] == "and":
            siguiente = self.etiqueta()
            self.condicion(nodo[2], siguiente, falsa)
            self.emitir(f"{siguiente}:")
            self.condicion(nodo[3], verdadera, falsa)
            return

        if nodo[0] == "op" and nodo[1] == "not":
            self.condicion(nodo[2], falsa, verdadera)
            return

        if nodo[0] == "op" and nodo[1] in (
            "==", "!=", ">", "<", ">=", "<="
        ):
            operador, izquierda, derecha = nodo[1:]
            izq = self.expresion(izquierda)
            der = self.expresion(derecha)
            self.emitir(
                f"if {izq} {operador} {der} goto {verdadera}"
            )
            self.emitir(f"goto {falsa}")
            return

        valor = self.expresion(nodo)
        self.emitir(f"if {valor} goto {verdadera}")
        self.emitir(f"goto {falsa}")

    def generar(self):
        p = self.programa

        funcion = p["funcion"]
        parametro = p["parametro"]
        nombre_resultado_if, valor_if = p["asignacion_if"]
        nombre_resultado_elif, valor_elif = p["asignacion_elif"]

        if nombre_resultado_if != nombre_resultado_elif:
            raise ValueError(
                "Las dos ramas deben asignar a la misma variable."
            )

        variable_resultado = nombre_resultado_if

        etiqueta_if = self.etiqueta()
        etiqueta_elif = self.etiqueta()
        etiqueta_fin = self.etiqueta()

        self.emitir(f"func {funcion}({parametro}):")

        # IF: n == 0 or n == 1
        self.condicion(
            p["condicion_if"],
            etiqueta_if,
            etiqueta_elif
        )

        self.emitir(f"{etiqueta_if}:")
        valor = self.expresion(valor_if)
        self.emitir(f"{variable_resultado} = {valor}")
        self.emitir(f"goto {etiqueta_fin}")

        # ELIF: n > 1
        self.emitir(f"{etiqueta_elif}:")
        etiqueta_cuerpo_elif = self.etiqueta()
        self.condicion(
            p["condicion_elif"],
            etiqueta_cuerpo_elif,
            etiqueta_fin
        )

        self.emitir(f"{etiqueta_cuerpo_elif}:")
        valor = self.expresion(valor_elif)
        self.emitir(f"{variable_resultado} = {valor}")
        self.emitir(f"goto {etiqueta_fin}")

        self.emitir(f"{etiqueta_fin}:")
        retorno = self.expresion(p["retorno"])
        self.emitir(f"return {retorno}")
        self.emitir("end func")

        # Llamada principal y salida
        self.emitir("")
        argumento = p["argumento_final"]
        self.emitir(f"param {argumento}")

        llamada = self.temporal()
        self.emitir(
            f"{llamada} = call {p['funcion_llamada']}, 1"
        )

        self.emitir(f"{p['variable_final']} = {llamada}")
        self.emitir(f"print {p['variable_print']}")

        return "\n".join(self.lineas)


# ============================================================
# 6. ARCHIVO DE SALIDA
# ============================================================

def siguiente_nombre():
    numero = 1

    while os.path.exists(f"factorial-py({numero}).txt"):
        numero += 1

    return f"factorial-py({numero}).txt"


def opcion_factorial():
    archivo_entrada = "C:\\Users\\Juan2\\OneDrive\\Escritorio\\ProyectoJVCUP\\factorial.py"

    if not os.path.isfile(archivo_entrada):
        print(f"No se encontro {archivo_entrada}.")
        print("Coloca el archivo junto al programa principal.")
        return

    try:
        with open(archivo_entrada, "r", encoding="utf-8") as archivo:
            codigo = archivo.read()

        programa = analizar_factorial(codigo)
        generador = GeneradorFactorial(programa)
        tac = generador.generar()

        salida = siguiente_nombre()
        
        try:
            with open(salida, "w", encoding="utf-8") as archivo:
                        archivo.write("CODIGO FUENTE:\n")
                        archivo.write(codigo)
                        archivo.write("\n\nCODIGO DE TRES DIRECCIONES:\n")
                        archivo.write(tac)
                        archivo.write("\n")
        except IOError as e:
            print(f"\nError al escribir el archivo de salida: {e}")
            return

        print("\nAnalisis PLY completado.")
        print("\nCodigo de tres direcciones generado:\n")
        print(tac)
        print(f"\nResultado guardado en: {salida}")

    except (SyntaxError, ValueError, TypeError) as error:
        print(f"\nError durante el analisis: {error}")


# ============================================================
# 7. MENU PRINCIPAL
# ============================================================

def main():
    while True:
        print("\n======================================")
        print(" PRACTICA DE COMPILADORES CON PLY")
        print("======================================")
        print("1. Analizar expresion de ejemplo")
        print("2. Analizar expresion personalizada")
        print("3. Analizar factorial.py")
        print("4. Salir")

        opcion = input("\nSelecciona una opcion: ").strip()

        try:
            if opcion == "1":
                expresion = "a + b * c - 10"
                print(f"\nExpresion: {expresion}")
                mostrar_resultados(analizar_expresion(expresion))

            elif opcion == "2":
                expresion = input("\nEscribe una expresion: ")
                mostrar_resultados(analizar_expresion(expresion))

            elif opcion == "3":
                opcion_factorial()

            elif opcion == "4":
                print("Programa finalizado.")
                break

            else:
                print("Opcion no valida.")

        except (SyntaxError, ValueError, TypeError) as error:
            print(f"\nError: {error}")


if __name__ == "__main__":
    main()
