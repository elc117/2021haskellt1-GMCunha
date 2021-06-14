module Main where
import System.Random
import Text.Printf
import Data.List

type Point     = (Float,Float)
type Triang    = (Point, Point, Point)
type Quadri    = (Point, Point, Point, Point)
type Circle    = (Point, Float)


-------------------------------------------------------------------------------
-- funcoes random
-------------------------------------------------------------------------------

randPalette :: Int -> [IO Int]
randPalette n = [(randomRIO(0, 255::Int)) | x <- [1..(n)*3]]

randTriangs :: Float -> Float -> Int -> [IO Float]
randTriangs w h n = concat [concat (transpose [replicate 3 (randomRIO (0.0, w)), replicate 3 (randomRIO (0.0, h))]) | x <- [1..(n)]]

randQuadri :: Float -> Float -> Int -> [IO Float]
randQuadri w h n = concat [concat (transpose [replicate 4 (randomRIO (0.0, w)), replicate 4 (randomRIO (0.0, h))]) | x <- [1..(n)]]

randCirc :: Float -> Float -> Int -> [IO Float]
randCirc w h n = concat [[randomRIO (mw, w-m), randomRIO (mh, h-m), randomRIO (0.0, m)] | x <- [1..(n)]]
  where mw = w / 3.0
        mh = h / 3.0
        m = min mw mh  


-------------------------------------------------------------------------------
-- funcoes de clonagem
-------------------------------------------------------------------------------

cloneYTQ :: Float -> [Float] -> [Float]
cloneYTQ _ [] = []
cloneYTQ h (x:y:ls) = [x, (h-y)] ++ cloneYTQ h ls

cloneXTQ :: Float -> [Float] -> [Float]
cloneXTQ _ [] = []
cloneXTQ w (x:y:ls) = [(w-x), y] ++ cloneXTQ w ls

cloneTQ :: Float -> Float -> [Float] -> Int ->[Float]
cloneTQ w h t m
  | m == 1 = t
  | m == 2 = t2
  | m == 4 = t2 ++ (cloneYTQ h t2)
  where t2 = t ++ (cloneXTQ w t)

cloneYCirc :: Float -> [Float] -> [Float]
cloneYCirc _ [] = []
cloneYCirc h (x:y:r:ls) = [x, (h-y), r] ++ cloneYCirc h ls

cloneXCirc :: Float -> [Float] -> [Float]
cloneXCirc _ [] = []
cloneXCirc w (x:y:r:ls) = [(w-x), y, r] ++ cloneXCirc w ls

cloneCirc :: Float -> Float -> [Float] -> Int ->[Float]
cloneCirc w h t m
  | m == 1 = t
  | m == 2 = t2
  | m == 4 = t2 ++ (cloneYCirc h t2)
  where t2 = t ++ (cloneXCirc w t)


-------------------------------------------------------------------------------
-- Geração de formas a partir de listas
-------------------------------------------------------------------------------

listToTriangles :: [Float] -> [Triang]
listToTriangles [] = []
listToTriangles (x1:y1:x2:y2:x3:y3:xs) = ((x1,y1), (x2,y2), (x3,y3)) : listToTriangles xs

listToQuadri :: [Float] -> [Quadri]
listToQuadri [] = []
listToQuadri (x1:y1:x2:y2:x3:y3:x4:y4:xs) = ((x1,y1), (x2,y2), (x3,y3), (x4, y4)) : listToQuadri xs

listToCircle :: [Float] -> [Circle]
listToCircle [] = []
listToCircle (x:y:r:xs) = ((x,y),r) : listToCircle xs


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera strings representando formas SVG 
-- dadas coordenadas e detalhes das formas e uma string com atributos de estilo
svgTriangle :: Triang -> String -> String
svgTriangle ((x1,y1), (x2,y2), (x3,y3)) style =
  printf "<polygon points='%.2f,%.2f %.2f,%.2f %.2f,%.2f' style='fill:none;stroke:%s;stroke-width:3' />" x1 y1 x2 y2 x3 y3 style

svgQuadri :: Quadri -> String -> String
svgQuadri ((x1,y1), (x2,y2), (x3,y3), (x4,y4)) style =
  printf "<polygon points='%.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f' style='fill:none;stroke:%s;stroke-width:3' />" x1 y1 x2 y2 x3 y3 x4 y4 style

svgCircle :: Circle -> String -> String
svgCircle ((x,y),r) style =
  printf "<circle cx='%.2f' cy='%.2f' r='%.2f' style='fill:none;stroke:%s;stroke-width:3' />" x y r style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

 
-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores

svgStyle :: [Int] -> [String]
svgStyle [] = []
svgStyle (r:g:b:xs) = (printf "rgb(%d,%d,%d); mix-blend-mode: screen;" r g b) : svgStyle xs

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Insira a largura da imagem:"
  winput <- getLine
  putStrLn "Insira a altura da imagem:"
  hinput <- getLine
  putStrLn "Insira o numero de formas desejado:"
  formas <- getLine
  putStrLn "Insira se deseja 1, 2 ou 4 imagens (qualquer numero diferente de 1 ou 2 sera considerado como 4):"
  mirrors <- getLine

  let w = (read winput::Float)
  let h = (read hinput::Float)
  let shapes = (read formas::Int)
  
  let mx = if mirrors == "1" then 1.0 else 2.0
  let my = if mirrors == "4" then 2.0 else 1.0
  let imgs = round (mx * my)

  qCirc <- randomRIO(0, shapes)
  qTri <- randomRIO(0, shapes-qCirc)
  let qQuadri = shapes-qCirc-qTri

  quadris <- sequence $ randQuadri (w/mx) (h/my) qQuadri
  triangs <- sequence $ randTriangs (w/mx) (h/my) qTri
  circs <- sequence $ randCirc (w/mx) (h/my) qCirc

  paletteQ <- sequence $ randPalette qQuadri
  paletteT <- sequence $ randPalette qTri
  paletteC <- sequence $ randPalette qCirc

  let quadris2 = listToQuadri (cloneTQ w h quadris imgs) 
  let triangs2 = listToTriangles (cloneTQ w h triangs imgs) 
  let circs2 = listToCircle (cloneCirc w h circs imgs) 

  let svgPaletteQ = take (qQuadri*imgs) $ cycle (svgStyle paletteQ)
  let svgPaletteT = take (qTri*imgs) $ cycle (svgStyle paletteT)
  let svgPaletteC = take (qCirc*imgs) $ cycle (svgStyle paletteC)

  let svgForms = intercalate " " [(svgElements svgTriangle triangs2 svgPaletteT), (svgElements svgQuadri quadris2 svgPaletteQ), (svgElements svgCircle circs2 svgPaletteC)]
  let svgstrs = svgBegin w h ++ svgForms ++ svgEnd

  writeFile "espelhos.svg" svgstrs
