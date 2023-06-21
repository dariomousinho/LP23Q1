import Data.List (delete)
import Data.List (stripPrefix)
import Text.Parsec
import Text.Parsec.String (Parser)

type Vertice = String
type Arestas = (String, String, String)
type Grafo = ([Vertice], [Arestas], Vertice)

-- Função para adicionar uma aresta a um grafo
addAresta :: Grafo -> Arestas -> Grafo
addAresta (vs, as, p) a = (vs, a : as, p)

-- Função para formar o grafo a partir das listas de vértices e relações
buildGrafo :: [Vertice] -> [Arestas] -> Grafo
buildGrafo (v:vs) as = foldl addAresta (v:vs, [], v) as

-- Função para exibir o grafo
showGrafo :: Grafo -> IO ()
showGrafo (vs, as, p) = do
  putStrLn "Vértices:"
  mapM_ print vs
  putStrLn "Arestas:"
  mapM_ print as

  
extractParentheses :: Parser [String]
extractParentheses = many (between (char '(') (char ')') (many (noneOf ")")))


controlaParentheses :: String -> [String] -> [String]
controlaParentheses [] s = s 
controlaParentheses (x:xs) s
  |  x == '(' = case parse extractParentheses "" (x:xs) of
    Left err -> error (show err)
    Right result -> controlaParentheses xs (s ++ result)
  | otherwise = controlaParentheses xs s 

compArestas :: Arestas -> Arestas -> Bool
compArestas (a, _, b) (c, _, d)
  | a == c && b == d = True
  | otherwise = False




atualizaGrafo :: Grafo -> String -> Grafo
atualizaGrafo (v:vs, a:as, p) (x:xs)
  | compArestas a (p,p,x:[]) && xs /= [] = atualizaGrafo (v:vs, a:as, pegarNovaPosicao a) xs
  | compArestas a (p,p,x:[]) && xs == [] = (v:vs, a:as, pegarNovaPosicao a)
  | otherwise = atualizaGrafo (v:vs, as, p) (x:xs)
    where
      pegarNovaPosicao :: Arestas -> Vertice
      pegarNovaPosicao (_,v,_) = v 


parseAninhado :: String -> String -> Grafo -> [String] -> Bool
parseAninhado [] [] _ _ = True
parseAninhado (x:xs) (y:ys) gra [] = parseAninhado (x:xs) (y:ys) gra (controlaParentheses (x:xs) [])
parseAninhado [] (y:ys) gra _ = rodaGrafo (y:ys) gra   
  where
    rodaGrafo :: String -> Grafo -> Bool
    rodaGrafo _ (_, [], _) = False
    rodaGrafo (x:xs) (v:vs, a:as, p)
      | compArestas a (p,p,x:[]) = True
      | otherwise = rodaGrafo (x:xs) (v:vs, as, p)
parseAninhado (x:xs) (y:ys) gra (z:zs)
  | x == ';' = rodaGrafo (y:ys) gra (z:zs)
  | x == 'U' = rodaGrafoU (x:xs) gra (z:zs) 1
  | x == '*' = rodaGrafo3 (x:xs) gra (z:zs)
  | otherwise  = parseAninhado xs (x:xs) gra (z:zs)
  where
    rodaGrafo3 :: String -> Grafo -> [String] -> Bool
    rodaGrafo3 (x:xs) gra (z:zs)
      | parseAninhado z z gra zs = rodaGrafo3 (x:xs) (atualizaGrafo gra z) (z:zs) 
      | otherwise = parseAninhado xs xs (v:vs, a:as, p) zs
    rodaGrafoU :: String -> Grafo -> [String] -> Int -> Bool
    rodaGrafoU (x:xs) (v:vs, a:as, p) (z:zs) count
      | parseAninhado z z (v:vs, a:as, p) zs && count == 1 = parseAninhado xs xs (atualizaGrafo (v:vs, a:as, p) z) (drop 2 (z:zs))
      | parseAninhado z z (v:vs, a:as, p) zs && count == 0 = parseAninhado xs xs (atualizaGrafo (v:vs, a:as, p) z) (drop 1 (z:zs))
      | count == 1 = rodaGrafoU (x:xs) (v:vs, a:as, p) zs 0
      | otherwise = False
    rodaGrafo :: String -> Grafo -> [String] -> Bool
    rodaGrafo _ (_, [], _) _ = False
    rodaGrafo (x:xs) (v:vs, a:as, p) s
      | compArestas a (p,p,x:[]) = parseAninhado (drop 2 (x:xs)) (drop 2 (x:xs)) (v:vs, a:as, pegarNovaPosicao a) s
      | otherwise = rodaGrafo (x:xs) (v:vs, as, p) s
      where
        pegarNovaPosicao :: Arestas -> Vertice
        pegarNovaPosicao (_,v,_) = v
 


removeParenteses :: String -> String -> String
removeParenteses [] _ = []
removeParenteses (x:xs) aux
 | x == '(' = removeParenteses (drop 1 (dropWhile (/= ')') xs)) aux
 | otherwise = removeParenteses xs aux++[x]

-- Remover "?" e o que tiver sendo operado por isto
remove :: String -> String -> String
remove [] _  = []  
remove (x:y:xs) saida
  | x == '?' && y /= ')'  = remove (drop 3 (x:y:xs)) saida -- Exclui "?p"
  | x == '?' && y == ')' = removeAninhado ((xs)) 1 saida
  | otherwise = remove (y:xs) (saida++[x])
    
removeAninhado :: String -> Int -> String -> String
removeAninhado [] 0 saida = [] 
removeAninhado (x:xs) y saida 
      | x == ')' = removeAninhado (drop 1 (x:xs)) (y+1) saida 
      | x == '(' && y /= 1 = removeAninhado (drop 1 (x:xs)) (y-1) saida  
      | x == '(' && y == 1 = (saida ++(drop 2 (x:xs))) 
      | y >= 1 = removeAninhado (drop 1 (x:xs))  y saida 


-- Verifica teste
verificaTeste :: String -> String -> String
verificaTeste [] s = s
verificaTeste (x:xs) stringOri 
 |x == '?' = remove stringOri []
 | otherwise = verificaTeste xs stringOri  


-- Entrada arestas
transformaTupla :: [String] -> (String,String,String)
transformaTupla [x,y,z] = (x,y,z)

entraAresta :: IO [(String,String,String)]
entraAresta = do
  entrada <- getLine
  if null entrada
    then return []
    else do
      entradaRecursiva <- entraAresta
      return ((transformaTupla (words entrada)) : entradaRecursiva)
      

--Exemplo de utilização
main :: IO ()
main = do
  
  -- Vértices
  putStrLn "Quais são os estados? Digite separados por \" \"(espaço):"
  estadosEntrada <- getLine
  let estados = words estadosEntrada


  -- Arestas
  putStrLn "Quais são as arestas (relações) existentes no grafo? Digite cada uma por linha, seprando os vértices por \" \"(espaço), colocando no final o label da aresta (peso):"
  relacoes <- entraAresta

  -- PDL
  let input = "(a;b;b)U(a;a)"
      reverseInput =  reverse input
  let pdlEntrada = reverse (verificaTeste reverseInput reverseInput)

  let grafo = buildGrafo estados relacoes
  
  let resposta = parseAninhado pdlEntrada pdlEntrada grafo []
  showGrafo grafo
  if resposta  
    then print "Executou"
    else print "Falha ao executar"
  -- let input = "a;b;(p->q)?"
  --     output = removeAffected input
  -- putStrLn output

  
-- Exemplo de uso
-- Exemplo de uso
-- main :: IO ()
-- main = do
--   let input = "(a)*(c)U(d)a;b;(y)*"
--   let resposta = controlaParentheses input []
--   print resposta


