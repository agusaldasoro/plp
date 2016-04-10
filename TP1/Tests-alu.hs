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
  "frecuenciaTokens" ~: testsFrecuenciaTokens,
  "distEuclediana" ~: testsDistEuclediana,
  "distCoseno" ~: testsDistCoseno,
  "acurracy" ~: testsAcurracy
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
  buscarExtractor 'a' (replicate mucho 'a') ~?= 1
  ]

testsDistEuclediana = test [
  distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464,
  distEuclideana [1.0] [1.0] ~?= 0,
  distEuclideana [14.0, 9.0] [9.0, 14.0] ~?= 7.071068,
  distEuclideana (replicate mucho 1.0) (replicate mucho 0.0) ~?= sqrt (fromIntegral mucho)
  ]

testsDistCoseno = test [
  distCoseno [1.0, 2.0, 3.0] [1.0, 2.0, 3.0] ~?= 0.99999994,
  distCoseno [1.0] [2.0] ~?= 1,
  distCoseno (replicate mucho 1.0) (replicate mucho 2.0) ~?= 1.0
  ]

testsAcurracy = test [
  accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ~?= 0.6,
  accuracy ["f", "i"] ["f", "f"] ~?= 0.5,
  accuracy ["f", "f", "i", "i", "f"] ["f", "f", "i", "i", "f"] ~?= 1.0,
  accuracy (replicate mucho "f") [if x `mod` 2 == 0 then "f" else "i" | x <- [1 .. mucho]] ~?= 0.5
  ]

buscarExtractor = \tok ->
        let posToken = (\(Just i) -> i) (elemIndex tok tokens)
        in frecuenciaTokens !! posToken
