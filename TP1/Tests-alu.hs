-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Solucion
import Test.HUnit
import Data.List

mucho = 1000000

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
 	"split" ~: testsSplit,
  "longPromedioPalabras" ~: testsLongitudPromedioPalabras,
 	"cuentas" ~: testsCuentas,
  "frecuenciaTokens" ~: testsFrecuenciaTokens
  ]

testsSplit = test [
 	split ',' ",PLP," ~?= ["PLP"],
 	split ',' " ,PLP, " ~?= [" ","PLP"," "]
  ]

testsLongitudPromedioPalabras = test [
  longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4,
  longitudPromedioPalabras "1" ~?= 1,
  longitudPromedioPalabras "Hola a todo" ~?= 3,
  longitudPromedioPalabras "3 tristes tigres" ~?= 4.6666665,
  longitudPromedioPalabras "Mi mama me mima" ~?= 3,
  longitudPromedioPalabras [if x `mod` 2 == 0 then ' ' else 'a' | x <- [1 .. mucho]] ~?= 1
  ]

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")]
  ]

testsFrecuenciaTokens = test [
  buscarExtractor '_' "use_snake_case !" ~?= 0.125,
  buscarExtractor '!' "use_snake_case !" ~?= 0.0625,
  buscarExtractor 's' "use_snake_case !" ~?= 0.1875,
  buscarExtractor 'h' "use_snake_case !" ~?= 0.0,
  buscarExtractor 'h' "" ~?= 0.0,
  buscarExtractor 'a' ['a' | x <- [1 .. mucho]] ~?= 1
  ]

buscarExtractor = \tok ->
        let posToken = (\(Just i) -> i) (elemIndex tok tokens)
        in frecuenciaTokens !! posToken
