{- Classes de tipos em Haskell são uma maneira de usar polimorfismo de
sobrecarga, como existe em linguagens como Java. Pense com cuidado e
explique quais s̃ão as vantagens e desvantagens de usar essa abordagem para
polimorfismo de sobrecarga, em comparacao com a abordagem usada por Java.

R: Classes de tipos são como interfaces que definem um comportamento. Se temos um tipo que faz parte da classe 
estamos assumindo que ele suporta e implementa o comportamento oferecido pela classe.
O polimorfismo de sobrecarga é utilizado quando temos funções com o mesmo nome, mas definições distintas.
Em Java podemos definir metodos com o mesmo nome, porém com quantidade/tipo de parametros distintos.
Em Haskell as classes de tipo permitem definir diferentes tipos para uma função, facilitando sua definição (o uso da sobrecarga). -}


{- Implemente uma função que devolva o N-ésimo
número da sequência look-and-say. -}

-- Given a string, "say" what's in the string.
say :: [Char] -> [Char]
say [] = []
say str@(x:xs) = show (length (first)) ++ [x] ++ say second
 where
 	(first, second) = (span (== x) str)

-- Given a number (expressed as a string),
-- returns an infinite list in its look-and-say
-- sequence.
lookAndSayList :: [Char] -> [[Char]]
lookAndSayList n = [said] ++ lookAndSayList said
 where
 	said = say n

-- Retrieves the nth element in the look-and-say sequence
-- specified in the question.
lookAndSay :: Int -> [Char]
lookAndSay 1 = "1"
lookAndSay n = (lookAndSayList "1")!!(n - 2)

{---------------------------------------- EXERCÍCIOS ----------------------------------------}
{- Implemente a função somatorioHexadecimal. Essa função recebe uma lista de Strings
onde cada elemento representa um numero na base hexadecimal e retorna uma String
contendo o resultado em hexadecimal do somatório da primeira lista. -}

hexToDec :: Char -> Int
hexToDec ch
 | hex >= 65 && hex <= 70 = hex - 55
 | hex >= 48 && hex <= 57 = hex - 48
 where
 	hex = fromEnum ch

decToHex :: Int -> String
decToHex 0 = ""
decToHex n
 | modulus == 10 = decToHex (div n 16) ++ ['A']
 | modulus == 11 = decToHex (div n 16) ++ ['B']
 | modulus == 12 = decToHex (div n 16) ++ ['C']
 | modulus == 13 = decToHex (div n 16) ++ ['D']
 | modulus == 14 = decToHex (div n 16) ++ ['E']
 | modulus == 15 = decToHex (div n 16) ++ ['F']
 | otherwise = decToHex (div n 16) ++ show modulus
 where
 	modulus = mod n 16

somatorioDecimal :: [String] -> Int
somatorioDecimal [] = 0
somatorioDecimal (x:xs) = hexToDec (head x) + somatorioDecimal xs

somatorioHexadecimal :: [String] -> String
somatorioHexadecimal xs = decToHex (somatorioDecimal xs)