(ns pl0.core-test
  (:require [clojure.test :refer :all]
            [pl0.core :refer :all]))

(deftest test-a-mayusculas-salvo-strings
  (testing "Prueba de funcion a-mayusculas-salvo-strings"
    (is (= "  CONST Y = 2;" (a-mayusculas-salvo-strings "  const Y = 2;")))
    (is (= "  WRITELN ('Se ingresa un valor, se muestra su doble.');" (a-mayusculas-salvo-strings "  writeln ('Se ingresa un valor, se muestra su doble.');")))
    ))

(deftest test-palabras-reservadas?
  (testing "Prueba de funcion palabra-reservada?"
    (is (= true (palabra-reservada? 'CALL)))
    (is (= true (palabra-reservada? "CALL")))
    (is (= true (palabra-reservada? 'IF)))
    (is (= true (palabra-reservada? "IF")))
    (is (= false (palabra-reservada? "ASIGNAR")))
    (is (= false (palabra-reservada? 'ASIGNAR)))
    ))    


(deftest test-identificador?
  (testing "Prueba de funcion identificador?"
    (is (= false (identificador? 'CALL)))
    (is (= false (identificador? "CALL")))
    (is (= true (identificador? 'V2)))
    (is (= true (identificador? "V2")))
    (is (= false (identificador? 2)))
    (is (= false (identificador? "2")))
    (is (= true (identificador? "V7R30K")))
    (is (= false (identificador? "V7-45")))
    ))

(deftest test-cadena?
  (testing "Prueba de funcion cadena?"
    (is (= true (cadena? "'Hola'")))
    (is (= false (cadena? "Hola")))
    (is (= false (cadena? "'Hola")))
    (is (= false (cadena? 'Hola)))
    ))

(deftest test-ya-declarado-localmente?
  (testing "Prueba de funcion ya-declarado-localmente?"
    (is (= true (ya-declarado-localmente? 'Y '[[0] [[X VAR 0] [Y VAR 1]]])))
    (is (= false (ya-declarado-localmente? 'Z '[[0] [[X VAR 0] [Y VAR 1]]])))
    (is (= false (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]])))
    (is (= true (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]])))
    ))

(deftest test-cargar-var-en-tabla
  (testing "Prueba de funcion cargar-var-en-tabla"
    (is (= '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 
           (cargar-var-en-tabla '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]])))

    (is (= '[nil () [VAR X] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]] 
           (cargar-var-en-tabla '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]])))

    (is (= '[nil () [VAR X Y] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]] 
           (cargar-var-en-tabla '[nil () [VAR X , Y] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]])))
    ))    

(deftest test-inicializar-contexto-local
  (testing "Prueba de funcion inicializar-contexto-local"
    (is (= '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
           (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))

    (is (= '[nil () [] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]]
           (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
    ))    

(deftest test-declaracion-var
  (testing "Prueba de funcion declaracion-var"
    (is (= ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]]
           (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]])))

    (is (= ['BEGIN (list 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";")] :sin-errores [[0] '[[X VAR 0] [Y VAR 1]]] 2 '[[JMP ?]]]
           (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]]) ))
    (is (= ['BEGIN (list 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ",") 'Z (symbol ";")] :sin-errores [[0] '[[X VAR 0] [Y VAR 1] [Z VAR 2]]] 3 '[[JMP ?]]]
           (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ",")  'Z (symbol ";")'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]])))  
  )
)       

(deftest test-signo-unario
  (testing "Prueba de funcion procesar-signo-unario"
    (is (= ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
           (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))

    (is (= [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
           (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    (is (= [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") (symbol "+")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
           (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))) 
    (is (= [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") (symbol "-")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]
           (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []]))) 
  
  )
) 

(deftest test-aplicar-aritmetico
  (testing "Prueba de funcion aplicar-aritmetico"

    (is (= [3] (aplicar-aritmetico + [1 2])))

    (is (= [1 3] (aplicar-aritmetico - [1 4 1])))

    (is (= [1 8] (aplicar-aritmetico * [1 2 4])))
    
    (is (= [1 0] (aplicar-aritmetico / [1 2 4])))

    (is (= nil (aplicar-aritmetico + nil)))

    (is (= [] (aplicar-aritmetico + [])))

    (is (= [1] (aplicar-aritmetico + [1])))

    (is (= [1 2 4] (aplicar-aritmetico 'hola [1 2 4])))

    (is (= [1 2 4] (aplicar-aritmetico count [1 2 4])))

    (is (= ['a 'b 'c] (aplicar-aritmetico + '[a b c])))
  
  )
)