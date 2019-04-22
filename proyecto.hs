import Data.List
import Data.Ix
import Data.Array

data Grafo v p = G (Array v [(v,p)]) deriving (Eq, Show) -- 'Grafo v p' es un TDA grafo con vértices de tipo v y pesos de tipo p.

nodos :: (Ix v,Num p) => (Grafo v p) -> [v] -- Son los nodos del grafo
nodos (G g) = indices g

aristas :: (Ix v,Num p) => (Grafo v p) -> [(v,v,p)] -- Son las aristas del grafo
aristas (G g) = [(v1,v2,w) | v1 <- nodos (G g) , (v2,w) <- g!v1] 

creaGrafo :: (Ix v, Num p) => (v,v) -> [(v,v,p)] -> Grafo v p -- Crea un grafo a partir de una lista de tuplas
creaGrafo cs vs = -- 'creaGrafo' es un grafo, con el par de cotas cs y listas de aristas as (cada arista es un trío formado por los dos vértices y su peso)
    G (accumArray (\xs x -> xs++[x]) [] cs (([(x2,(x1,p))|(x1,x2,p) <- vs, x1 /= x2])++[(x1,(x2,p)) | (x1,x2,p) <- vs])) -- Se encarga de orientar las aristas

prim :: (Ix v, Num p, Ord p) => Grafo v p -> [(p,v,v)] -- Algoritmo de Prim, Devuelve el arbol recubridor minimo
prim g = prim' [n] ns [] (aristas g) -- Se llama a "prim'" pasandole por parametro los nodos colocados, los nodos por colocar, el arbol de expansion y las aristas del grafo
    where (n:ns) = nodos g -- Evaluacion perezosa, n y ns son los nodos del grafo

prim' t [] ae as = ae -- "prim'" es llamando por 'prim' y ambas funciones son las encargadas de crear el arbol recubridor minimo
prim' t r  ae as = prim' (v':t) (delete v' r) (e:ae) as
    where e@(c,u', v') = minimum [(c,u,v)| (u,v,c) <- as, elem u t, elem v r] -- Evaluacion perezosa que crea una nueva lista por comprension

crearListaConexiones :: [String] -> [[String]] -> [[String]] --'crearListaConexiones' se encarga de transformar una lista de String y en una lista de listas de String
crearListaConexiones [] nuevaListaConexiones = nuevaListaConexiones -- En el caso de que los datos de entrada sean una lista vacia devuelve lo que contenga 'nuevaListaConexiones' (Condicion de parada)
crearListaConexiones (x:xs) nuevaListaConexiones = crearListaConexiones xs nuevaListaConexiones++[agregado] -- En las siguientes recursiones el String tomado se procesa en 'agregado' y luego se agrega a dato, ademas a la recursion siguente se le pasa el resto del lista no procesada
    where agregado = words ([y | y<-x, y/='[', y/=']']) -- where se utiliza para hacer una evaluacion perezosa, 'agregado' transforma el String recibido en una lista de String

crearListaTuplas :: [String] -> [String] -> [[String]] -> [(Char,Char,Int)] -> [(Char,Char,Int)] -- 'crearListaTuplas' recarga de recibir los planetas y las conexiones y devolver un arreglo de duplas que sera usado por el algoritmo para el ARM
crearListaTuplas [] _ _ nuevaMatriz = nuevaMatriz -- En el caso de que las plantas de entrada sean una lista vacia devuelve lo que contenga 'nuevaMatris' (Condicion de parada)
crearListaTuplas (x:xs) plantasTotales (y:ys) nuevaMatriz = crearListaTuplas xs plantasTotales ys (nuevaMatriz++(crearTuplas x plantasTotales y 0 [])) -- En las siguientes recursiones a 'nuevaMatriz' se le concatena los datos devueltos por la funcion 'crearTuplas'

crearTuplas :: String -> [String] -> [String] -> Int -> [(Char,Char,Int)] ->[(Char,Char,Int)] -- 'tranformarConexiones' se encarga de unir las plantas electricas y sus respectivas conexiones en duplas
crearTuplas _ _ [] _ nuevaMatriz = nuevaMatriz -- En el caso de que las conexiones de entrada sean una lista vacia devuelve lo que contenga 'nuevaMatriz'(Condicion de parada)
crearTuplas planta plantasTotales (x:xs) n nuevaMatriz = crearTuplas planta plantasTotales xs (n+1) (nuevaMatriz++[(readChar planta,readChar $ plantasTotales!!n,readInt x)]) -- Si la planta es diferente de vacio se realiza una recurson a la que se le concatena la dupla entre planta electrica y conexion creada

rutaMasCorta :: [Char] -> Char -> [(Int,Char,Char)] -> (Int,Char,Char) -> Int ->[(Int,Char,Char)] -> [(Int,Char,Char)] -- Busca la ruta mas corta entre dos puntos
rutaMasCorta _ _ [] _ _ rmc = rmc -- Si el arm esta vacio devuelve lo que tenga rmc
rutaMasCorta ruta nodo arm elemento opcion rmc 
    | coincidencia == [] = if (opcion == 0) then -- Si la tupla de coincidencia es vacia devuelve lo que tenga rmc
                        rutaMasCorta ruta (ruta!!0) armModificado (0,' ',' ') 1 [] -- Llama 'rutaMasCorta' eliminando el valor que crea inestabilidad
                    else
                        rutaMasCorta ruta (segundo tuplaInversa) armInverso tuplaInversa 0 rmc -- Llama 'rutaMasCorta' pasandole la tupla con los nodos inversos
    | segundo (coincidencia!!0) == nodo = if (tercero (coincidencia!!0) == ruta!!1) then -- Si existe coinicidencia
                                       rutaMasCorta ruta (tercero (coincidencia!!0)) [] (coincidencia!!0) 0 (rmc++[(coincidencia!!0)]) -- Llama 'rutaMasCorta' y le agrega a rmc la coincidencia, arm es igual a vacio para detener la recursividad
                                   else
                                       rutaMasCorta ruta (tercero (coincidencia!!0)) arm (coincidencia!!0) 0 (rmc++[(coincidencia!!0)]) -- Llama 'rutaMasCorta' y le agrega a rmc la coincidencia
    where coincidencia = [x | x <- arm, segundo x == nodo]
          armModificado = [x | x<-arm, x/=elemento]
          tuplaNueva = ([x | x<-arm, tercero x == nodo])!!0
          tuplaInversa = ((primero tuplaNueva), (tercero tuplaNueva), (segundo tuplaNueva))
          armInverso = replaceAtIndex (buscarIndice tuplaNueva arm 0) tuplaInversa arm

buscarIndice :: (Int,Char,Char) -> [(Int,Char,Char)] -> Int -> Int  -- 'buscarIndice' devuelve el numero de la posicion del elemento buscado
buscarIndice _ [] n = n
buscarIndice nodo (x:xs) n = if (nodo == x) then -- Si el elemento x es igual al nodo quiere decir que hay coincidencia
                                buscarIndice nodo [] n
                             else
                                buscarIndice nodo xs (n+1) -- Si el el elemento no es encontrado se llama 'buscarInidice' con el resto de la lista

replaceAtIndex n item ls = a++(item:b) where  (a, (_:b)) = splitAt n ls -- 'replaceAtIndex' reemplaza un elemento en la posicion n de la lista ls   

primero:: (a,b,c) -> a -- primero se encarga de obtener el 1ero elemento de la tupla
primero (a,_,_) = a -- Ignora los dos primeros elementos y devuelve el 1ero

segundo:: (a,b,c) -> b -- segundo se encarga de obtener el 2do elemento de la tupla
segundo (_,b,_) = b -- Ignora los dos primeros elementos y devuelve el 2do

tercero:: (a,b,c) -> c -- tercero se encarga de obtener el 3er elemento de la tupla
tercero (_,_,c) = c -- Ignora los dos primeros elementos y devuelve el 3ero

readInt :: String -> Int -- Convierte un String en Int
readInt = read

readChar :: String -> Char -- Convierte un String en Char
readChar caracter = caracter!!0

main = do -- Funcion principal
    archivo <- readFile "ADY.txt" -- readFile lee el archivo de texto                                             
    let lineas = lines archivo -- lines crea un lista con las lineas del achivo leido   
        plantas = words([x | x<- head lineas, x/='(', x/=')']) -- 'plantas' formatea la entrada de la primera linea del archivo volviendolo una lista de las plantas electricas
        conexiones = reverse $ crearListaConexiones (init $ tail lineas) [] -- 'conexiones' toma todas las lineas excepto la primera y la ultima, estas serian las conexiones de las plantas electricas, esta seria una lista con las lineas del archivo. Luego dichas lineas son transformadas en un arreglo de arreglos de conexiones
        matrizTuplas = [x | x<-(crearListaTuplas plantas plantas conexiones []), (tercero x)/=0] -- 'matrizFormateada' llama a 'crearListaTuplas' pasandole por parametro las plantas y las conexiones ya formateadas en listas de listas, luego de esto se crea una nueva lista sin las conexiones iguales a cero
        cotas = (readChar (head plantas), readChar (last plantas)) -- Son las cotas necesarias para la creacion del grafo, estas son el primer y ultimo nodo del grafo
        grafo = creaGrafo cotas matrizTuplas -- Es el grafo ya creado con la funcion creaGrafo, se le pasan por parametro el par de cotas y la lista con tuplas que representan la matriz
        arbolRecubridorMinimo = prim grafo -- 'arbolRecubridorMinimo' guarda la salida del algoritmo prim pasandole por parametro el grafo creado
        ruta = [readChar ((words $ init $ tail $ last lineas)!!0)]++[readChar ((words $ init $ tail $ last lineas)!!1)] -- Toma la ruta del archivo, le quita los corchetes, lo vuelve una lista, luego los elementos de la lista son convertidos a caracter
        rmc = rutaMasCorta ruta (ruta!!0) arbolRecubridorMinimo (0,' ',' ') 0 []
    print "------------------ ARBOL RECUBRIDOR MINIMO ------------------"
    print arbolRecubridorMinimo -- Imprime el arbol recubridor minimo en pantalla
    print "---------------------- RUTA MAS CORTA -----------------------"
    print rmc
    return ()

-- Realizado por:
-- Juan Villarroel
-- C.I: 25.108.239