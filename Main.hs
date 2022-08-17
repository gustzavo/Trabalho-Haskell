-- Gustavo Rodrigues Guimarães



--Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.
soma1 ::  Int -> Int
soma1 x =  succ x
-- *******************************************************************
--Escreva uma função chamada sempre que, não importando o valor de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo
sempre :: any -> Int
sempre x = 0
-- *******************************************************************
--Escreva uma função chamada treco que receba três valores em ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
treco :: Double -> Double -> Double -> Double
treco x y z = (x+y)*z
-- *******************************************************************
--Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.
resto :: Int->Int->Int
resto x y = x `mod` y
-- *******************************************************************
--Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários
precoMaior :: Float -> Float -> Float -> Float -> Float
precoMaior a b c d = maximum[a,b,c,d]
-- *******************************************************************
--Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.
impar :: Int -> Int -> Bool
impar x y = (x*y) `mod` 2 == 1
-- *******************************************************************
--Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
par :: (Int,Int) -> Int
par (x,y) = if even x && even y then x+y else 0
-- *******************************************************************
--Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação
questao8 :: Double -> Double -> Double -> Double
questao8 x y z = x**2 + y/2 ** z
-- *******************************************************************
--Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link: LINK Observe que este diagnóstico é meramente estatístico e não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.
diagnostico :: Double -> String
diagnostico x
  |x<17      = "Muito abaixo do peso"
  |x<18.5    = "Peso normal"
  |x<30      = "Sobrepeso" 
  |x<35      = "Obesidade Ieve" 
  |x<40      = "Obesidade severa"
  |otherwise = "Obesidade mórbida"
-- *******************************************************************
--Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra: 
{-
  -𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4
  -𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100
  -𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400
  -1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
-}
bissexto :: Int -> Bool
bissexto x = ((x `mod` 4 == 0 && x `mod` 100 /= 0) || x`mod`400 == 0)




main = do
  print ("soma1: entrada: 5; resultado: " ++ show (soma1 5))
  print ("soma1: entrada: -6; resultado: " ++ show (soma1 (-6)))

  print ("sempre: entrada: 0; resultado: " ++ show (sempre 0))
  print ("sempre: entrada: 'a'; resultado: " ++ show (sempre ('a')))

  print ("treco: entrada: 5 6 7; resultado: " ++ show (treco 5 6 7))
  print ("treco: entrada: -6 -1 7; resultado: " ++ show (treco (-6) (-1) 7))

  print ("resto: entrada: 5 6; resultado: " ++ show (resto 5 6))
  print ("resto: entrada: -4 2; resultado: " ++ show (resto (-4) 2))

  print ("precoMaior: entrada: 5 6 6.1 6.10009; resultado: " ++ show (precoMaior 5 6 6.1 6.10009))
  print ("precoMaior: entrada: -4 2 5 6; resultado: " ++ show (precoMaior (-4) 2 5 6))

  print ("impar: entrada: 5 5; resultado: " ++ show (impar 5 5))
  print ("impar: entrada: -4 2; resultado: " ++ show (impar (-4) 2))

  print ("par: entrada: 5 6; resultado: " ++ show (par (5,6)))
  print ("par: entrada: -4 2; resultado: " ++ show (par ((-4),2)))

  print ("questao8: entrada: 1 2 3; resultado: " ++ show (questao8 1 2 3))
  print ("questao8: entrada: -4 2 1; resultado: " ++ show (questao8 (-4) 2 1))

  print ("diagnostico: entrada: 16; resultado: " ++ show (diagnostico 16))
  print ("diagnostico: entrada: 16.99999; resultado:" ++ show (diagnostico 16.99999))

  print ("bissexto: entrada: 2000; resultado: " ++ show (bissexto 2000))
  print ("bissexto: entrada: 2022; resultado: " ++ show (bissexto 2022))

