# Extensión del lenguaje LIS: LIS-E
===================================


## Información General del Proyecto
-----------------------------------
**Propósito:** Definición formal e implementación de una extensión del Lenguaje Imperativo Simple (LIS).
**Lenguaje de implementación:** Haskell, utilizado como lenguaje subyacente de especificación y ejecución.
**Asignatura:** Teoría de Lenguajes – AUS, UNR-IPS (Rosario, Argentina).


## Instrucciones rápidas
------------------------
1. LIS-E es un lenguaje interpretado cuya ejecución requiere como argumento un archivo `<nombre>.lis`. (Opcional compilar el intérprete Haskell con `ghc`)
2. Clonar el repositorio:
    `git clone https://github.com/Andrenon/aus-2025-teoria-de-lenguajes`
    `cd aus-2025-teoria-de-lenguajes`
3. Para pruebas:
    `ghci Main.hs`
4. Ejecutar desde ghci:
    `run "test/test01.lis"`
    `run "test/test02.lis"`
    `...`


## Descripción
-------------
En este trabajo se presenta una Extensión del Lenguaje Imperativo Simple (LIS), denominada LIS-E, cuyo objetivo es incorporar:
- Soporte nativo para strings
- Operadores de conversión entre enteros y strings
- Operador de concatenación para strings (++)
- Operador de flujo |> definido en dos niveles sintácticos:
    - A nivel de expresiones, como azúcar sintáctico para la composición funcional.
    - A nivel de comandos, como un constructor sintáctico que modela la aplicación de acciones con efectos sobre un flujo.
- Comandos con _efectos_ observables sobre valores intermedios
- Capturar _errores_ en tiempo de ejecución

Extensión del _dominio_ de valores:
$$Val ::= Z ∪ String$$

La semántica completa de LIS-E puede formalizarse como:
$$(c,σ,τ)⇓(res,σ′,τ′)$$
donde:
- c: Comando a ejecutar
- σ, σ′: Estado inicial y final, respectivamente
- τ, τ′: Log inicial y final, respectivamente
- res: Resultado de la ejecución. El resultado puede ser:
    - Error abortivo
    - Ejecución normal sin flujo
    - Ejecución normal con flujo
Ahora bien como el Log modela un efecto interno del programa (traza observable para depuración), se considera que si dos programas producen: 
- mismo estado final, 
- mismo resultado,
pero distinto log, eso no cambia el comportamiento interno del programa. Por lo tanto el log no forma parte del núcleo semántico y las reglas semánticas se especifican:
$$(c,σ)⇓(res,σ′)$$


Se mantiene que:
> Todo programa válido en LIS es válido en LIS-E y tiene la misma semántica. $(LIS ⊆ LIS-E)$


## Diseño del lenguaje LIS-E
----------------------------
**Nivel 1 — Sintaxis**
- `AST.hs` → Sintaxis abstracta
- `Parser.hs` → Sintaxis concreta
Esto define el **lenguaje**, no su ejecución.
**Nivel 2 — Semántica**
- `Eval2.hs` → Semántica operacional big-step (evaluador monádico)
Especificación ejecutable del lenguaje: `⟨c,σ⟩⇓(σ′,out,err)`.
El evaluador se construye mediante la composición de mónadas:
  - `ExceptT` → modela manejo de errores dinámicos
  - `StateT` → modela el estado (store)
  - `Writer` → modela los efectos observables (log)
**Nivel 3 — IO**
- `Main.hs` → Interfaz interactiva
Conecta: `archivo → parser → evaluador → print resultado`



## Sintaxis abstracta
---------------------
```
Value ::= IntVal Int | StrVal String

intexp ::= ...
         | len strexp
         | toInt strexp

strexp ::= STRING
         | svar
         | strexp ++ strexp
         | strexp |> strfilter
         | toStr intexp

strfilter ::= upper | lower | reverse | trim

comm ::= ...
       | svar ::= strexp
       | step strexp
       | comm |> pipe_action

pipe_action ::= strfilter | print | sleep intexp
```

## Sintaxis concreta
--------------------
```
intexp ::= ...
         | 'len(' strexp ')'
         | 'toInt('strexp ')'

strexp ::= strexp_no_pipe
         | strexp '|>' strfilter

strexp_no_pipe ::= strexp_value
                 | strexp_no_pipe '++' strexp_value

strexp_value ::= STRING
               | svar
               | 'toStr(' intexp ')'

strfilter ::= 'upper' | 'lower' | 'reverse' | 'trim'

comm ::= ...
       | svar '::=' strexp
       | 'step(' strexp ')'
       | comm '|>' pipe_action

pipe_action ::= strfilter
              | comm_effect

comm_effect ::= 'print'
              | 'sleep(' intexp ')'
```

## Semántica formal
-------------------
Se respeta:
- Expresiones → big-step (⇓)
- Comandos → small-step (⇝)

Se introduce un nuevo comando intermedio:
	`pipe(w,k)`
Este constructor:
- no es visible al usuario
- es interno de la semántica
- representa pipeline en ejecución (w = valor actual, k = número de pasos ejecutados)

### Expresiones enteras nuevas
#### E-Len
$$\frac {⟨s,σ⟩⇓_{strexp}w​}{⟨len(s),σ⟩⇓_{intexp}∣w∣}$$

#### E-ToInt
$$\frac {⟨s,σ⟩⇓_{strexp}w\ \ \ \ parse(w)=n​}{⟨toInt(s),σ⟩⇓_{intexp}n}$$

### Expresiones string
#### E-String
$$\frac {}{⟨^"c^",σ⟩⇓_{strexp}\ ^"c^"​}$$

#### E-SVar
$$\frac {σ(s)=w​}{⟨s,σ⟩⇓_{strexp}w}$$

#### E-ToStr
$$\frac {⟨e,σ⟩⇓_{intexp}n\ \ \ \ str(n)=w​}{⟨toStr(e),σ⟩⇓_{strexp}w}$$

#### E-Concat
$$\frac {​⟨s1​,σ⟩⇓_{strexp}w1​\ \ \ \ ⟨s2​,σ⟩⇓_{strexp}w2​​}{⟨s1​++s2​,σ⟩⇓_{strexp}w1​⋅w2}$$

### Pipe a nivel de expresiones
Azúcar sintáctica: `s∣>f ≡ f(s)`
#### E-Pipe
$$\frac {⟨s,σ⟩⇓w​​​}{⟨s∣>f,σ⟩⇓_{strexp}Ff​(w)}$$
Donde:
|filtro|función matemática|
|---|---|
|upper|uppercase(w)|
|lower|lowercase(w)|
|reverse|reverse(w)|
|trim|removeSpaces(w)|

### Comandos
#### C-SAssign
$$\frac {⟨s,σ⟩⇓_{strexp}w​​​}{⟨x::=s,σ⟩⇝⟨skip,σ[x↦w]⟩}$$

#### C-Step
$$\frac {⟨s,σ⟩⇓_{strexp}w​​​}{⟨step(s),σ⟩⇝⟨pipe(w,0),σ⟩}$$
Inicia un flujo observable.

### Pipe a nivel de comandos
Constructor semántico: `c |> a` introduce una acción con efectos sobre un flujo activo.

#### C-Pipe-Filter
$$\frac { ​​​}{⟨pipe(w,k)∣>f,σ⟩⇝⟨pipe(Ff​(w),k+1),σ⟩}$$

#### C-Print
$$\frac { ​​​}{⟨pipe(w,k)∣>print,σ⟩⇝⟨pipe(w,k+1),σ⟩}$$

#### C-Sleep
$$\frac {⟨e,σ⟩⇓_{intexp}n​}{⟨pipe(w,k)∣>sleep(e),σ⟩⇝⟨pipe(w,k+1),σ⟩}$$

#### C-Pipe-End
$$\frac{} {⟨pipe(w,k),σ⟩ ⇝ ⟨skip,σ⟩}$$

### Errores
#### E-Var-Undefined
$$\frac{x\notin dom(σ)}{⟨x,σ⟩⇓error}$$

#### E-Var-Type-Int
$$\frac{σ(x) = StrVal(w)} {⟨x,σ⟩ ⇓_{intexp} error}$$
“Se esperaba entero”

#### E-SVar-Type-Err
$$\frac{σ(x) = IntVal(n)} {⟨x,σ⟩ ⇓_{strexp} error}$$“Se esperaba string”

#### (E-Div-Zero)
$$\frac{ ⟨a,σ⟩⇓ n_1 \quad ⟨b,σ⟩⇓ 0 } {⟨a / b,σ⟩ ⇓ error}$$

#### E-ToInt-Err
$$\frac{⟨s,σ⟩ ⇓_{strexp} w \quad parse(w)\;falla} {⟨toInt(s),σ⟩ ⇓ error}$$

#### C-Pipe-NoFlow
$$\frac{ (c,σ) ⇓ (Nothing,σ') } { ⟨c |> a,σ⟩ ⇝ ⟨error,σ'⟩ }$$
"Pipe aplicado a comando sin flujo"

#### C-Pipe-Err
Si una acción del pipeline produce error:
$$\frac{⟨a(w),σ⟩ ⇓ error} {⟨pipe(w,k) |> a,σ⟩ ⇝ ⟨error,σ⟩}$$



## Explicación coloquial de las extensiones
-------------------------------------------

**`len(s)`**
Devuelve la longitud del string evaluado.

**`toInt(s)`**
Convierte un string numérico a entero.
Produce error si el string no representa un entero válido.

**`toStr(e)`**
Convierte un entero en string.

**`++`**
Concatena dos strings.

**`step(s)`**
Inicia un pipeline.
Permite observar transformaciones intermedias.

**`|>`**
Aplica una transformación o efecto al flujo actual.

**`upper / lower / reverse / trim`**
Transformaciones puras sobre strings.

**`print`**
Imprime el valor del pipeline. Operador de efecto lateral no terminal.

**`sleep(n)`**
Suspende la ejecución n segundos sin modificar el valor.


## Conclusión
-------------

LIS-E constituye una extensión conservativa del Lenguaje Imperativo Simple (LIS), incorporando soporte nativo para strings y un mecanismo explícito de flujo mediante pipelines. 

La extensión no altera la semántica de los programas válidos en LIS, preservando así la compatibilidad hacia atrás (LIS ⊆ LIS-E). No obstante, amplía el dominio semántico del lenguaje al incorporar efectos observables y manejo explícito de errores como parte del resultado de evaluación.

El diseño mantiene una separación clara entre sintaxis abstracta, sintaxis concreta, semántica operacional e implementación del evaluador, asegurando coherencia formal y modularidad en su construcción.