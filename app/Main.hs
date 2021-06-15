module Main where
import System.Random
import Text.Printf
import Data.List

type Point     = (Float,Float)
type Triang    = (Point, Point, Point)
type Quadri    = (Point, Point, Point, Point)
type Circle    = (Point, Float)


-------------------------------------------------------------------------------
-- funcoes random, geram listas de IO Int para ser "traduzidas" na main
-------------------------------------------------------------------------------

randPalette :: Int -> [IO Int]
randPalette n = [(randomRIO(0, 255::Int)) | x <- [1..n*3]]

randTriangs :: Float -> Float -> Int -> [IO Float]
randTriangs w h n = concat [[randomRIO (0.0, w), randomRIO (0.0, h)] | x <- [1..(n*3)]]

randQuadri :: Float -> Float -> Int -> [IO Float]
randQuadri w h n = concat [[randomRIO (0.0, w), randomRIO (0.0, h)] | x <- [1..(n*4)]]

randCirc :: Float -> Float -> Int -> [IO Float]
randCirc w h n = concat [[randomRIO (mw, w-m), randomRIO (mh, h-m), randomRIO (0.0, m)] | x <- [1..(n)]]
  where mw = w / 3.0
        mh = h / 3.0
        m = min mw mh  


-------------------------------------------------------------------------------
-- funcoes de clonagem, feitas com os objetos ainda em forma de lista de
-- floats pois assim eh possivel juntar algumas funcoes
-------------------------------------------------------------------------------

cloneTQ :: Float -> Float -> [Float] -> Int ->[Float] -- clona triangulos e poligonos quadrilateros
cloneTQ w h t m
  | m == 1 = t
  | m == 2 = t2
  | m == 4 = t2 ++ (cloneYTQ h t2)
  where t2 = t ++ (cloneXTQ w t)

cloneYTQ :: Float -> [Float] -> [Float]
cloneYTQ h list = [if x `rem` 2 == 1 then h - (list !! x) else (list !! x) | x <- [0..(length list)-1]]

cloneXTQ :: Float -> [Float] -> [Float]
cloneXTQ w list = [if x `rem` 2 == 0 then w - (list !! x) else (list !! x) | x <- [0..(length list)-1]]

cloneCirc :: Float -> Float -> [Float] -> Int ->[Float] -- clona circulos
cloneCirc w h t m
  | m == 1 = t
  | m == 2 = t2
  | m == 4 = t2 ++ (cloneYCirc h t2)
  where t2 = t ++ (cloneXCirc w t)

cloneYCirc :: Float -> [Float] -> [Float]
cloneYCirc h list = [if x `rem` 3 == 1 then h - (list !! x) else (list !! x) | x <- [0..(length list)-1]]

cloneXCirc :: Float -> [Float] -> [Float]
cloneXCirc w list = [if x `rem` 3 == 0 then w - (list !! x) else (list !! x) | x <- [0..(length list)-1]]


-------------------------------------------------------------------------------
-- Geracao de formas a partir de listas, meio longas e confusas, pegam
-- de n em n elementos da lista e colocam em suas estruturas corretas
-------------------------------------------------------------------------------

listToTriangles :: [Float] -> [Triang]
listToTriangles list = [((list !! x, list !! (x+1)),(list !! (x+2), list !! (x+3)),(list !! (x+4), list !! (x+5))) | x <- [0, 6 .. (length list)-1]]

listToQuadri :: [Float] -> [Quadri]
listToQuadri list = [((list !! x, list !! (x+1)),(list !! (x+2), list !! (x+3)),(list !! (x+4), list !! (x+5)),(list !! (x+6), list !! (x+7))) | x <- [0, 8 .. (length list)-1]]

listToCircle :: [Float] -> [Circle]
listToCircle list = [((list !! x, list !! (x+1)),list !! (x+2)) | x <- [0, 3 .. (length list)-1]]


-------------------------------------------------------------------------------
-- Gera strings svg recebendo a forma e uma string de style
-------------------------------------------------------------------------------

svgTriangle :: Triang -> String -> String
svgTriangle ((x1,y1), (x2,y2), (x3,y3)) style =
  printf "<polygon points='%.2f,%.2f %.2f,%.2f %.2f,%.2f' style='fill:none;stroke:%s;stroke-width:3' />" x1 y1 x2 y2 x3 y3 style

svgQuadri :: Quadri -> String -> String
svgQuadri ((x1,y1), (x2,y2), (x3,y3), (x4,y4)) style =
  printf "<polygon points='%.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f' style='fill:none;stroke:%s;stroke-width:3' />" x1 y1 x2 y2 x3 y3 x4 y4 style

svgCircle :: Circle -> String -> String
svgCircle ((x,y),r) style =
  printf "<circle cx='%.2f' cy='%.2f' r='%.2f' style='fill:none;stroke:%s;stroke-width:3' />" x y r style



svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

svgEnd :: String
svgEnd = "</svg>"

-- mesma ideia das funcoes de transformacao de lista para forma
svgStyle :: [Int] -> [String]
svgStyle list = [(printf "rgb(%d,%d,%d); mix-blend-mode: screen; " (list !! x) (list !! (x+1)) (list !! (x+2))) | x <- [0, 3..(length list)-1]]

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- leitura dos parametros
  putStrLn "Insira a largura da imagem:"
  winput <- getLine
  putStrLn "Insira a altura da imagem:"
  hinput <- getLine
  putStrLn "Insira o numero de formas desejado:"
  formas <- getLine
  putStrLn "Insira se deseja 1, 2 ou 4 imagens (qualquer numero diferente sera considerado como 2):"
  mirrors <- getLine

  -- transforma dados lidos em tipos necessarios
  let w = (read winput::Float)
  let h = (read hinput::Float)
  let shapes = (read formas::Int)
  
  -- define divisas dos eixos X e Y e declara numero total de divisas
  let mx = if mirrors == "1" then 1.0 else 2.0
  let my = if mirrors == "4" then 2.0 else 1.0
  let imgs = round (mx * my)

  -- define numero aleatorio de formas
  qCirc <- randomRIO(0, shapes)
  qTri <- randomRIO(0, shapes-qCirc)
  let qQuadri = shapes-qCirc-qTri -- o numero que falta para completar o total de formas desejadas

  -- cria listas de floats aleatorios baseado nas formas desejadas, recebe como lista de [IO Float] e depois traduz
  quadris <- sequence $ randQuadri (w/mx) (h/my) qQuadri
  triangs <- sequence $ randTriangs (w/mx) (h/my) qTri
  circs <- sequence $ randCirc (w/mx) (h/my) qCirc

  -- cria lista de cores para cada lista de forma para facilitar o processo de duplicacao
  paletteQ <- sequence $ randPalette qQuadri
  paletteT <- sequence $ randPalette qTri
  paletteC <- sequence $ randPalette qCirc

  -- clona as listas de formas e transforma as todas em suas estruturas corretas
  let quadris2 = listToQuadri (cloneTQ w h quadris imgs) 
  let triangs2 = listToTriangles (cloneTQ w h triangs imgs) 
  let circs2 = listToCircle (cloneCirc w h circs imgs) 

  -- clona as listar de cores baseado em quantas copias suas formas respectivas possuem
  let svgPaletteQ = take (qQuadri*imgs) $ cycle (svgStyle paletteQ)
  let svgPaletteT = take (qTri*imgs) $ cycle (svgStyle paletteT)
  let svgPaletteC = take (qCirc*imgs) $ cycle (svgStyle paletteC)

  -- junta tudo em uma grande string utilizando intercalate para separar e svgElements para montar cada parte
  let svgForms = intercalate " " [(svgElements svgTriangle triangs2 svgPaletteT), (svgElements svgQuadri quadris2 svgPaletteQ), (svgElements svgCircle circs2 svgPaletteC)]
  let svgstrs = svgBegin w h ++ svgForms ++ svgEnd

  -- cria a imagem :^)
  writeFile "espelhos.svg" svgstrs
