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