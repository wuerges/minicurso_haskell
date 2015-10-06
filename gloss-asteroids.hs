{-
  Jogo "Asteroids" usando a biblioteca Gloss

  Pedro Vasconcelos <pbv@dcc.fc.up.pt> 2014
  Departamento de Ciência de Computadores
  Faculdade de Ciências, Universidade do Porto
-}

module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- representação do "mundo" do jogo
-- lista de objetos com os seguintes invariantes:
-- * nunca é vazia;
-- * o primeiro elemento é *sempre* a nave do jogador.
type World = [Object]

-- um objeto no espaço
-- um par de forma e movimento
type Object = (Shape, Movement)

-- formas de diferentes objetos
data Shape = Asteroid Float   -- asteroides (tamanho)
           | Laser Float     -- "lasers" (decaimento)
           | Ship             -- nave do jogador


-- representação do movimento de um objeto
-- posição, velocidade linear, orientação e velocidade angular
type Movement = (Point,      -- posição do centro
                 Vector,     -- velocidade linear
                 Float,      -- orientação (graus)
                 Float       -- velocidade angular (graus/s)
                 )


-- testar se um objeto é um asteroid
isAsteroid :: Object -> Bool
isAsteroid (Asteroid _, _) = True
isAsteroid _               =  False

-- testar se é laser
isLaser :: Object -> Bool
isLaser (Laser _, _) = True
isLaser _            = False


-- | desenhar todos os objetos
drawWorld :: World -> Picture
drawWorld objs = pictures (map drawObj objs)


-- | desenhar um objeto na posição e orientação corretas
drawObj :: Object -> Picture
drawObj (shape, ((x,y), _, ang, _))
  = translate x y $ rotate ang $ drawShape shape


-- | desenhar a forma de um objeto (centradas na origem)
drawShape :: Shape -> Picture
drawShape Ship            = color green ship
drawShape (Laser time)   = color yellow laser
drawShape (Asteroid size) = color red (scale size size asteroid)

-- | figuras básicas
ship, laser, asteroid :: Picture
ship = polygon [(20,0),(-10,10),
                (-5,0),(-10,-10),(20,0)]
laser = line [(0,0),(10,0)]
asteroid = polygon [(-10,5),(-5, 10),(10,5),
                    (5, -5),(-5,-10),(-10,5)]


-- | proximo estado de movimento após intervalo `dt'
move :: Float -> Movement -> Movement
move dt ((x,y), (dx,dy), ang, angV)
  = ((x',y'), (dx,dy), ang', angV)
    where x' = wrap (x+dt*dx) maxWidth
          y' = wrap (y+dt*dy) maxHeight
          ang' = ang + dt*angV
          wrap h max | h > max = h-2*max
                     | h < -max= h+2*max
                     | otherwise = h

-- | avançar um objeto por um intervalo `dt'
moveObj :: Float -> Object -> Object
moveObj dt (shape, mov) = (shape, move dt mov)


-- | avançar a simulação do mundo um intervalo `dt'
updateWorld :: Float -> World -> World
updateWorld dt = collisions . decay dt . map (moveObj dt)



-- | decaimento dos projéteis "laser" no jogo
decay :: Float -> World -> World
decay dt (ship:objs) = ship : filter remain (map decr objs)
  where
    -- diminuir o tempo de decaimento dos "lasers"
    decr (Laser t, mov)   = (Laser (t-dt), mov)
    decr obj               = obj
    -- testar se um objeto ainda não desapareceu
    -- (i.e. deve permanecer em jogo)
    remain (Laser t, mov) = t>0   -- "lasers" só com tempo positivo
    remain _              = True  -- tudo o resto permanece


-- | detetar e tratar colisões
collisions :: World -> World
collisions (ship:objs) =  ship : (frags ++ objs' ++ objs'')
  where rocks  = filter isAsteroid objs  -- todos os asteroids
        lasers = filter isLaser objs     -- todos os lasers
        frags = concat [fragment rock | rock<-rocks,
                        any (`hits`rock) lasers]
        objs' = [obj | obj<-rocks,
                 not (any (`hits`obj) lasers)]
        objs''= [obj | obj<-lasers,
                 not (any (obj`hits`) rocks)]


-- verificar se há colisão entre dois objetos
-- apenas entre "lasers" e asteroides
hits :: Object -> Object -> Bool
hits (Laser _, ((x,y), _, _, _)) (Asteroid sz, ((x',y'), _, _, _))
  = (x-x')**2 + (y-y')**2 <= (sz*10)**2
hits _ _ = False


-- fragmentar um asteróide em partes mais pequenas
-- elimina os fragmentos demasiado pequenos
fragment :: Object -> [Object]
fragment (Asteroid sz, (pt, (dx,dy), ang, angV))
  | sz'>= 1  = [(Asteroid sz', (pt, vel', ang, angV)) | vel'<- vels]
  | otherwise= []
  where sz'  = 0.5*sz
        vels = [(dy,dx),(-dx,dy),(dx,-dy),(-dy,-dx)]


-- | reagir a eventos de teclas desencadeados pelo jogador
react :: Event -> World -> World
-- * rodar a nave (esquerda/direita)
react (EventKey (SpecialKey KeyLeft) keystate _ _) (ship:objs)
  = (ship':objs)
  where  (Ship, (pos, vel, ang, angV)) = ship
         angV' = if keystate==Down then (-180) else 0
         ship' = (Ship, (pos, vel, ang, angV'))

react (EventKey (SpecialKey KeyRight) keystate _ _) (ship:objs)
  = (ship':objs)
  where  (Ship, (pos, vel, ang, angV)) = ship
         angV'= if keystate==Down then 180 else 0
         ship' = (Ship, (pos, vel, ang, angV'))


-- * acelerar a nave para a frente
react (EventKey (SpecialKey KeyUp) Down _ _) (ship:objs)
  = (ship':objs)
  where (Ship, (pos, (dx,dy), ang, angV)) = ship
        dx' = dx + 10*cos (-ang/180*pi)
        dy' = dy + 10*sin (-ang/180*pi)
        ship' = (Ship, (pos, (dx',dy'), ang, angV))


-- * disparar um novo projétil
react (EventKey (SpecialKey KeySpace) Down _ _) (ship:objs)
  = (ship:proj:objs)
  where (Ship, (pos, _, ang, _)) = ship
        vel = (400*cos (-ang/180*pi), 400*sin (-ang/180*pi))
        proj = (Laser 1, (pos, vel, ang, 0))  -- decai após 1 seg.

-- * todos os outros eventos: ignorar
react _ world = world

-- | criar um asteroide aleatório
randRock :: IO Object
randRock = do pos <- randPoint
              vel <- randVel
              angV <- randomRIO (-90,90)
              ang <- randomRIO (-180,180)
              sz <- randomRIO (1,4)
              return (Asteroid sz, (pos,vel,ang,angV))

randVel :: IO Vector
randVel = do dx <- randomRIO (-50,50)
             dy <- randomRIO (-50,50)
             return (dx,dy)

randPoint :: IO Point
randPoint = do x <- randomRIO (-maxWidth,maxWidth)
               y <- randomRIO (-maxHeight,maxHeight)
               return (x,y)


-- constantes
maxWidth, maxHeight :: Float
maxWidth = 300
maxHeight = 300

fps :: Int
fps = 60

main :: IO ()
main = do
  rocks <- sequence [randRock | _<-[1..10]]
  play window black fps (ship:rocks) drawWorld react updateWorld
  where
    ship = (Ship, ((0,0), (0,0), 0, 0))  -- posição inicial da nave

window = InWindow "Asteroids" (2*round maxWidth,2*round maxHeight) (0,0)


