import re
from sympy import symbols, Eq, solve, sympify

x, y = symbols('x y')

def parsear_ecuacion(ecuacion_str):
    ecuacion_str = ecuacion_str.replace(" ", "")
    match = re.match(r'(.+)=([\+\-]?\d+)', ecuacion_str)
    if match:
        izquierda = sympify(match.group(1))
        derecha = sympify(match.group(2))
        return Eq(izquierda, derecha)
    else:
        raise ValueError("La ecuación no tiene el formato esperado")

def ingreso_ecuaciones():
    print("Ingresa:")
    try:
        eq1_input = input(" 1: ")
        eq2_input = input(" 2: ")
        eq1 = parsear_ecuacion(eq1_input)
        eq2 = parsear_ecuacion(eq2_input)
        return eq1, eq2
    except Exception as e:
        print(" Error al interpretar las ecuaciones:", e)
        return None, None

def metodo_sustitucion(eq1, eq2):
    print("\n Método de Sustitución:")
    try:
        despeje = solve(eq1, y)
        if not despeje:
            despeje = solve(eq1, x)
            sustituida = eq2.subs(x, despeje[0])
            solucion = solve(sustituida, y)
            x_valor = despeje[0].subs(y, solucion[0])
        else:
            sustituida = eq2.subs(y, despeje[0])
            solucion = solve(sustituida, x)
            y_valor = despeje[0].subs(x, solucion[0])
            x_valor = solucion[0]
        print(" Resultado:")
        print("x =", x_valor)
        print("y =", y_valor if 'y_valor' in locals() else solucion[0])
    except Exception as e:
        print(" No se pudo resolver por sustitución:", e)

def metodo_igualacion(eq1, eq2):
    print("\n Método de Igualación:")
    try:
        despeje1 = solve(eq1, y)
        despeje2 = solve(eq2, y)
        igualado = Eq(despeje1[0], despeje2[0])
        x_valor = solve(igualado, x)[0]
        y_valor = despeje1[0].subs(x, x_valor)
        print(" Resultado:")
        print("x =", x_valor)
        print("y =", y_valor)
    except Exception as e:
        print(" No se pudo resolver por igualación:", e)

def metodo_reduccion(eq1, eq2):
    print("\n Método de Reducción:")
    try:
        sol = solve((eq1, eq2), (x, y))
        print(" Resultado:")
        print("x =", sol[x])
        print("y =", sol[y])
    except Exception as e:
        print(" No se pudo resolver por reducción:", e)

def main():
    print(" SISTEMA DE ECUACIONES \n")
    eq1, eq2 = ingreso_ecuaciones()
    if eq1 is None or eq2 is None:
        return

    while True:
        print("\n Elige el método para resolver:")
        print("1. Sustitución")
        print("2. Igualación")
        print("3. Reducción")
        print("4. Salir")
        opcion = input("Opción: ")

        if opcion == '1':
            metodo_sustitucion(eq1, eq2)
        elif opcion == '2':
            metodo_igualacion(eq1, eq2)
        elif opcion == '3':
            metodo_reduccion(eq1, eq2)
        elif opcion == '4':
            print(" Hasta luego. Vuelve pronto :) ")
            break
        else:
            print(" Opción inválida.")

if __name__ == "__main__":
    main()
