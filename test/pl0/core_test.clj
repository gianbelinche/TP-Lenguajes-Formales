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