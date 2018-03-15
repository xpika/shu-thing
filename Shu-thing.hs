module Main (main)
    where

import Control.Exception (catch, throwIO,SomeException)
import Control.Monad (zipWithM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Maybe (isJust, fromJust)
import Graphics.UI.GLUT (DisplayMode(RGBMode, DoubleBuffered),
                        getArgsAndInitialize,
                        GLdouble, initialDisplayMode, initialWindowSize, mainLoop,
                        createWindow, destroyWindow, swapBuffers, Menu(..), MenuItem(MenuEntry),
                        attachMenu, Font(renderString), StrokeFont(Roman), Flavour(Wireframe),
                        Object(Tetrahedron, Teapot, Octahedron, Icosahedron, Dodecahedron),
                        renderObject, MouseButton(LeftButton,RightButton), Key(SpecialKey, Char), KeyState(..),
                        SpecialKey(KeyUp, KeyRight, KeyLeft, KeyDown), displayCallback,
                        keyboardMouseCallback, TimerCallback, addTimerCallback, MatrixComponent(..),
                        MatrixMode(Projection, Modelview), Position(..), Size(..), Vector3(..),
                        loadIdentity, matrixMode, preservingMatrix, viewport,
                        ClearBuffer(DepthBuffer, ColorBuffer), clear, HasSetter(..), lookAt,
                        perspective, Color(color), Color3(..), Vertex3(..))
import Prelude hiding (catch)
import System.Exit (ExitCode(ExitSuccess),exitWith)

import qualified Foreign.C.Types

type NDouble = GLdouble

main :: IO ()
main = do
  keystate <- newIORef []
  cp       <- newIORef $ openingProc keystate
  initialWindowSize $= Size 1200 800
  initialDisplayMode $= [RGBMode,DoubleBuffered]
  getArgsAndInitialize
  wnd <- createWindow "Shu-thing"

  displayCallback $= dispProc cp
  keyboardMouseCallback $= Just (keyProc keystate)

  addTimerCallback 24 $ timerProc $ dispProc cp

  attachMenu LeftButton (Menu [
    MenuEntry "&Exit" exitLoop])
  attachMenu RightButton (Menu [
    MenuEntry "&Exit" exitLoop])

  initMatrix
  mainLoop
  destroyWindow wnd

  `catch` (\e -> return (const () (e::SomeException)) )

exitLoop :: IO a
exitLoop = exitWith ExitSuccess

initMatrix :: IO ()
initMatrix = do
  viewport $= (Position 0 0,Size 1200 800)
  matrixMode $= Projection
  loadIdentity
  perspective 30.0 (4/3) 600 1400
  lookAt (Vertex3 0 0 (927 :: NDouble)) (Vertex3 0 0 (0 :: NDouble)) (Vector3 0 1 (0 :: NDouble))

dispProc :: IORef (IO Scene) -> IO ()
dispProc cp = do
  m <- readIORef cp
  Scene next <- m
  writeIORef cp next

newtype Scene = Scene (IO Scene)

openingProc :: IORef [Key] -> IO Scene
openingProc ks = do
  keystate <- readIORef ks

  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  color $ Color3 (1.0 :: NDouble) 1.0 1.0
  preservingMatrix $ do
    translate $ Vector3 (-250 :: NDouble) 0 0
    scale (0.8 :: NDouble) 0.8 0.8
    renderString Roman "shu-thing"
  preservingMatrix $ do
    translate $ Vector3 (-180 :: NDouble) (-100) 0
    scale (0.4 :: NDouble) 0.4 0.4
    renderString Roman "Press Z key"

  swapBuffers

  if Char 'z' `elem` keystate then do
      gs <- newIORef initialGameState
      return $ Scene $ mainProc gs ks
   else return $ Scene $ openingProc ks

endingProc :: IORef [Key] -> IORef NDouble -> IO Scene
endingProc ks ctr= do
  keystate <- readIORef ks
  counter <- readIORef ctr
  modifyIORef ctr $ min 2420 . (+1.5)
  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  color $ Color3 (1.0 :: NDouble) 1.0 1.0
  zipWithM_ (\str pos -> preservingMatrix $ do
    translate $ Vector3 (-180 :: NDouble) (-240+counter-pos) 0
    scale (0.3 :: NDouble) 0.3 0.3
    renderString Roman str)
    stuffRoll [0,60..]

  swapBuffers

  if Char 'x' `elem` keystate then do
      return $ Scene $ openingProc ks
   else return $ Scene $ endingProc ks ctr

  where
    stuffRoll = [
     "",
     "Game Design",
     "     T. Muranushi",
     "",
     "Main Programmer",
     "     H. Tanaka",
     "",
     "Enemy Algorithm",
     "     M. Takayuki",
     "",
     "Graphics Designer",
     "     Euclid",
     "",
     "Monad Designer",
     "     tanakh",
     "",
     "Lazy Evaluator",
     "     GHC 6.8",
     "",
     "Cast",
     "  Player Dodecahedron",
     "  Bullet Tetrahedron",
     "  Enemy  Octahedron",
     "  Boss   Teapot",
     "",
     "Special thanks to",
     "     Simon Marlow",
     "     Haskell B. Curry",
     "",
     "Presented by",
     "     team combat",
     "",
     "WE LOVE HASKELL!",
     "",
     "    press x key"]

mainProc :: IORef GameState -> IORef [Key] -> IO Scene
mainProc gs ks = do
  keystate <- readIORef ks
  modifyIORef gs $ updateGameState keystate
  gamestate <- readIORef gs

  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  renderGameState gamestate
  swapBuffers
  if (isGameover gamestate) then return $ Scene $ openingProc ks else
   if (isClear gamestate) then do
      counter <- newIORef (0.0 :: NDouble)
      return $ Scene $ endingProc ks counter else
    return $ Scene $ mainProc gs ks

timerProc :: IO a -> TimerCallback
timerProc m = m >> addTimerCallback 16 (timerProc m)

keyProc :: IORef [Key] -> Key -> KeyState -> t -> t1 -> IO ()
keyProc keystate key ks _ _ =
  case (key,ks) of
    (Char 'q',_) -> exitLoop
    (Char 'c',_) -> exitLoop
    (_,Down) -> modifyIORef keystate $ nub . (++[key])
    (_,Up) -> modifyIORef keystate $ filter (/=key)

bosstime, bosstime2 :: Int
bosstime = 6600
bosstime2 = 7200

data GameObject = Player {position :: Point,shotEnergy :: NDouble,hp :: NDouble}|
                  Bullet {position :: Point} |
                  EnemyMaker {timer :: Int,deathtimer :: Int}|
                  Enemy {position :: Point,hp :: NDouble,anime :: Int,enemySpec :: EnemySpec} |
                  Explosion {position :: Point,hp :: NDouble,size :: NDouble}|
                  EnemyBullet {position :: Point,velocity :: Point} |
                  GameoverSignal |
                  ClearSignal
                  deriving (Eq)

data EnemySpec = EnemySpec {ways :: Int,spread :: NDouble,speed :: NDouble,freq :: Int,endurance :: NDouble,boss :: Bool}
                 deriving (Eq)

updateObject :: GameState -> [Key] -> GameObject -> [GameObject]
updateObject _ ks (Player{position=pos,shotEnergy=sen,hp=oldhp})
 = [(Player{position=newPos,shotEnergy=nsen,hp=newhp})] ++ shots
 where
  newPos :: Point
  newPos = if (oldhp > 0) then (nx,ny) +++ v
           else (nx,ny)
  newhp = oldhp
  (x,y) = pos
  nx = if (x < (-310)) then -310 else if (x > 310) then 310 else x
  ny = if (y < (-230)) then -230 else if (y > 200) then 200 else y
  v = (vx,vy) *++ (5.0 :: NDouble)
  shots = replicate shotn $ Bullet pos
  nsen = if (shotn /= 0) then (-1.0) else
    if (shotmode == 1 && shotn == 0) then (sen+0.25) else
    if(shotmode == 0) then 0.0 else
    sen
  vx :: NDouble
  vx =
   if ((SpecialKey KeyLeft) `elem` ks) then -1 else 0 +
   if ((SpecialKey KeyRight) `elem` ks) then 1 else 0
  vy =
   if ((SpecialKey KeyUp) `elem` ks) then 1 else 0 +
   if ((SpecialKey KeyDown) `elem` ks) then -1 else 0
  shotmode :: Int
  shotmode =
   if ((Char 'z') `elem` ks) then 1 else 0
  shotn :: Int
  shotn = if (oldhp <= 0) then 0 else if (shotmode == 0) then 0 else
    if (sen >= 0) then 1 else 0
updateObject _ _ (Bullet{position=pos}) = replicate n (Bullet newpos) where
  newpos = pos +++ (0.0,15.0)
  n = if( (\(_,y) -> y > 250) pos)then 0 else 1
updateObject gs _ (EnemyMaker{timer=t,deathtimer=dtime}) =
 [EnemyMaker{timer=t+1,deathtimer=newdtime}] ++ enemies ++ deatheffects where
  enemies = replicate n $ Enemy{position = (320*sin(dt*dt),240),hp=1.0,anime=0,enemySpec = spec}
  dt :: NDouble
  dt = fromIntegral t
  newdtime = dtime + if (hp p<=0 || (bossExist&&hp b<=0)) then 1 else 0
  n = if((t`mod`120==0 && t<=bosstime) || t==bosstime2) then 1 else 0
  deatheffects = if(dtime==0) then [] else
    if(dtime==120) then [Explosion{position=position deadone,hp=1.0,size=3.0*deathradius}] else
    if(dtime==130) then [Explosion{position=position deadone,hp=1.0,size=3.0*deathradius}] else
    if(dtime==140) then [Explosion{position=position deadone,hp=1.0,size=3.0*deathradius}] else
    if(dtime==240) then [if(hp p<=0) then GameoverSignal else ClearSignal] else
    if(dtime>120) then [] else
    if(dtime`mod`15/=0)then [] else
    [Explosion{position=position deadone +++ ((sin(dt),cos(dt))*++ (16*deathradius)),hp=1.0,size=0.3*deathradius}]
  p = findplayer gs
  b = fromJust mayb
  deadone :: GameObject
  deadone = if(hp p<=0) then p else b
  deathradius = if(hp p<=0) then 1 else 3
  bossExist = isJust mayb
  mayb = findBoss gs
  spec = if(t==bosstime2) then (EnemySpec{ways=0,spread=0.1,speed=3.0,freq=10,endurance=300.0,boss=True})
    else speclist !! (t `div` 600)
  speclist = [
     EnemySpec {ways=0,spread=0.1,speed=3.0,freq=30,endurance=2.0,boss=False},
     EnemySpec {ways=1,spread=0.3,speed=5.0,freq=60,endurance=4.0,boss=False},
     EnemySpec {ways=3,spread=0.7,speed=0.2,freq=90,endurance=8.0,boss=False},
     EnemySpec {ways=45,spread=0.069,speed=8.0,freq=450,endurance=1.0,boss=False},
     EnemySpec {ways=0,spread=0.1,speed=1.0,freq=10,endurance=10.0,boss=False},
     EnemySpec {ways=0,spread=0.1,speed=1.0,freq=10,endurance=10.0,boss=False},
     EnemySpec {ways=3,spread=0.1,speed=3.0,freq=60,endurance=6.0,boss=False},
     EnemySpec {ways=1,spread=0.5,speed=7.0,freq=45,endurance=3.0,boss=False},
     EnemySpec {ways=(10),spread=0.3,speed=15.0,freq=115,endurance=5.0,boss=False}
    ] ++
     map (\o -> EnemySpec {ways=o,spread=0.1,speed=4.0,freq=20,endurance=3.0,boss=False}) [0,1 ..]

updateObject gs _ oldenemy@(Enemy{position=pos,hp=oldhp,anime=oldanime,enemySpec=spec})  =
  replicate n (oldenemy{position=newpos,hp=newhp,anime=newanime,enemySpec=newspec})
   ++ shots ++ explosions where
    newpos = if isBoss then (200 * sin(danime/100),200 + 40 * cos(danime/80))
      else pos +++ (0.0,-1.0)
    newhp = oldhp
    newanime = oldanime + 1
    newspec
        | (not isBoss) = spec
        | (oldhp > 0.75) = EnemySpec{ways=0,spread=0.1,speed=5.0,freq=10,endurance=300.0,boss=True}
        | (oldhp > 0.50) = EnemySpec{ways=8,spread=0.15,speed=3.0,freq=30,endurance=300.0,boss=True}
        | (oldhp > 0.25) = EnemySpec{ways=2,spread=1.2,speed=15.0,freq=10,endurance=300.0,boss=True}
        | (oldhp > 0.05) = EnemySpec{ways=40,spread=0.075,speed=3.0,freq=60,endurance=400.0,boss=True}
        | (oldhp > 0.00) = EnemySpec{ways=15,spread=0.2,speed=16.0,freq=20,endurance=900.0,boss=True}
        | otherwise = EnemySpec{ways=(-1),spread=0.1,speed=3.0,freq=10,endurance=300.0,boss=True}
    danime :: NDouble
    danime = fromIntegral oldanime
    explosions = if(oldhp<=0 && not isBoss) then [Explosion{position=pos,hp=1.0,size=1.0}] else []
    shots = if(oldanime`mod` frq /=(frq-1)) then [] else
      map (\v -> EnemyBullet{position=pos,velocity=v}) vs
    vs = (take (wa+1) $ iterate (vdistr***) centerv) ++
         (take wa $ tail $ iterate (vdistrc***) centerv)
    centerv = (pp -+- pos) *++ (spd / (distance pp pos))
    vdistr :: Point
    vdistr = (cos(sprd),sin(sprd))
    vdistrc :: Point
    vdistrc = (cos(sprd),-sin(sprd))
    pp = playerpos gs
    n = if( (\(_,y) -> y<(-250)) pos || (not isBoss && oldhp<=0))then 0 else 1
    wa = ways spec
    spd= speed spec
    frq = freq spec
    sprd= spread spec
    isBoss = boss spec
updateObject _ _ e@(Explosion{}) = if(hp e>0) then [e{hp=hp e - (0.024/(size e))}] else []
updateObject _ _ eb@(EnemyBullet{}) = if(outofmap (position eb)) then [] else
  [eb{position=position eb +++ velocity eb}]
updateObject _ _ go = [go]

watcher :: [GameObject] -> [GameObject]
watcher os = np ++ ne ++ nb ++ neb ++ others
    where
      ne  = foldr ($) enemies $ map enemyDamager bullets
      np  = foldr ($) players $ map playerDamager ebullets
      nb  = foldr ($) bullets $ map bulletEraser enemies
      neb = foldr ($) ebullets $ map ebEraser players
      bulletEraser :: GameObject -> [GameObject] -> [GameObject]
      bulletEraser e = filter (\b -> (distance2 (position b) (position e)) > hitr(e))
      enemyDamager :: GameObject -> [GameObject] -> [GameObject]
      enemyDamager b = map (\e  ->
                                if ((distance2 (position b) (position e)) > hitr(e))
                                then e
                                 else (\d ->  d{hp=hp d-(1.0 / (endurance (enemySpec d)))}) e)
      hitr e = if(boss $ enemySpec e) then sq 100 else sq 32
      playerDamager :: GameObject -> [GameObject] -> [GameObject]
      playerDamager eb = map (\p  ->
                              if((distance2 (position p) (position eb)) > 70)
                              then p
                              else (\q -> q{hp=hp q-0.3}) p)
      ebEraser p = filter (\eb -> (distance2 (position eb) (position p)) > 70)
      (enemies,bullets,ebullets,players,others) = foldl f ([],[],[],[],[]) os
      f (e,b,eb,p,x) o = case o of
                           Enemy{}       -> (o:e,b,eb,p,x)
                           Bullet{}      -> (e,o:b,eb,p,x)
                           EnemyBullet{} -> (e,b,o:eb,p,x)
                           Player{}      -> (e,b,eb,o:p,x)
                           _             -> (e,b,eb,p,o:x)

renderGameObject :: GameObject -> IO ()
renderGameObject Player{position=pos,hp=h} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (1.0 :: NDouble) h h)
  translate (Vector3 x y 0)
  scale (10 :: NDouble) 10 10
  rotate (x) (Vector3 0 1 0)
  rotate (30 :: NDouble) (Vector3 0 0 1)
  renderObject Wireframe Dodecahedron
renderGameObject Bullet{position=pos} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (0.6 :: NDouble) 0.6 1.0)
  translate (Vector3 x y 0)
  scale (4 :: NDouble) 18 8
  rotate (45 :: NDouble) (Vector3 0 1 0)
  rotate (90 :: NDouble) (Vector3 1 0 0)
  renderObject Wireframe Tetrahedron
renderGameObject Enemy{position=pos,anime=a,hp=h,enemySpec=EnemySpec{boss=False}} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (cos rho) (sin rho) (0.0 :: NDouble))
  translate (Vector3 x y 0)
  rotate (2*(theta :: NDouble)) (Vector3 0 1.0 0)
  scale (32 :: NDouble) 32 8
  renderObject Wireframe Octahedron where
    theta = fromIntegral a
    rho = h * 3.14 / 2
renderGameObject Enemy{position=pos,anime=a,hp=h,enemySpec=EnemySpec{boss=True}} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (cos rho) (sin rho) (0.0 :: NDouble))
  translate (Vector3 x y 0)
  rotate (2*(theta :: NDouble)) (Vector3 0 1.0 0)
  scale (120 :: NDouble) 120 120
  renderObject Wireframe (Teapot 1.0) where
    theta = fromIntegral a
    rho = h * 3.14 / 2
renderGameObject Explosion{position=pos,hp=h,size=s}= preservingMatrix $ do
  let (x,y) = pos
  color (Color3 h 0.0 0.0)
  translate (Vector3 x y 0)
  rotate (720*h) (Vector3 0 1.0 0)
  rotate (540*h) (Vector3 1.0 0 0)
  scale r r r
  renderObject Wireframe Icosahedron where
    r = s*(100 - h*h*80)
renderGameObject EnemyBullet{position=pos} = preservingMatrix $ do
  let (x,y) = pos
  color (Color3 (1.0 :: NDouble) 1.0 1.0)
  translate (Vector3 x y 0)
  scale (5 :: NDouble) 5 5
  rotate (45 :: NDouble) (Vector3 0 0 (1.0 :: NDouble))
  renderObject Wireframe  Tetrahedron
renderGameObject _ = return ()

data GameState = GameState {objects :: [GameObject]}

initialGameState :: GameState
initialGameState = GameState{objects=
  [(Player{position=(0.0,0.0),shotEnergy=0.0,hp=1.0}),(EnemyMaker{timer=0,deathtimer=0})]}

renderGameState :: GameState -> IO ()
renderGameState GameState{objects=os} = mapM_ renderGameObject os

updateGameState :: [Key] -> GameState -> GameState
updateGameState ks gs@(GameState { objects=os }) = newgs where
  newgs = GameState{objects = watcher $ concatMap (updateObject gs ks) os}

playerpos :: GameState -> Point
playerpos = position . findplayer
findplayer :: GameState -> GameObject
findplayer GameState{objects=os} = player where
  [player] = filter (\o -> case o of
    Player{} -> True
    _        -> False) os
findBoss :: GameState -> Maybe GameObject
findBoss GameState{objects=os} = if (length bosses==0) then Nothing
                                  else Just (head bosses) where
  bosses = filter(\o -> case o of
    Enemy{} -> boss( enemySpec o)
    _        -> False) os

isGameover :: GameState -> Bool
isGameover GameState{objects=os} = (GameoverSignal `elem` os)

isClear :: GameState -> Bool
isClear GameState{objects=os} = (ClearSignal `elem` os)

type Point = (NDouble,NDouble)

(+++) :: (NDouble, NDouble) -> (NDouble, NDouble) -> (NDouble, NDouble)
(ax,ay) +++ (bx,by) = (ax+bx, ay+by)

(-+-) :: (Num t1, Num t) => (t, t1) -> (t, t1) -> (t, t1)
(ax,ay) -+- (bx,by) = (ax-bx,ay-by)

(*++) ::  (Num t) => (t, t) -> t -> (t, t)
(ax,ay) *++ (s) = (ax*s,ay*s)

(***) :: (Num t) => (t, t) -> (t, t) -> (t, t)
(ax,ay) *** (bx,by) = (ax*bx-ay*by,ay*bx+ax*by)

sq :: NDouble -> NDouble
sq x = x*x

distance, distance2 :: Point -> Point -> NDouble
distance a b= sqrt $ distance2 a b
distance2 (ax,ay) (bx,by) = sq(ax-bx) + sq(ay-by)

outofmap :: Point -> Bool
outofmap (x,y) = (not $ abs x < 320) || (not $ abs y < 240)
