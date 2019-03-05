-- this or type signature for getProductMenge
-- {-# LANGUAGE FlexibleContexts #-}
import Data.Map as Map
import qualified Data.Map.Strict (Map)
import Control.Monad.Reader as Reader
import qualified Control.Monad.Reader (Reader)
import Control.Monad.State.Lazy as State
import qualified Control.Monad.State.Lazy (State)

newtype Farbe = Farbe String deriving (Eq, Show, Ord)
newtype PH = PH Double deriving (Eq, Show, Ord)

data Haartyp = Fettig | Trocken | Normal | Schuppen
  deriving (Eq, Show, Ord)

data Grundbestandteil =
    Tensid Farbe PH
  | Pflegestoff Farbe Haartyp
  deriving (Eq, Show, Ord)
  
data ReinigungsProdukt =
    Einfach Grundbestandteil
  | Gemisch Double ReinigungsProdukt Double ReinigungsProdukt
  deriving (Eq, Show, Ord)

tensidAnteil :: ReinigungsProdukt -> Double
tensidAnteil (Einfach (Tensid _ _)) = 1.0
tensidAnteil (Einfach (Pflegestoff _ _)) = 0.0
tensidAnteil (Gemisch p1 sp1 p2 sp2) =
  p1 * (tensidAnteil sp1) + p2 * (tensidAnteil sp2)

reinigungsProduktBestandteile :: ReinigungsProdukt -> Map Grundbestandteil Double
reinigungsProduktBestandteile (Einfach s) = Map.singleton s 1.0
reinigungsProduktBestandteile (Gemisch p1 sp1 p2 sp2) =
  let d1 = fmap (\ p -> p*p1) (reinigungsProduktBestandteile sp1)
      d2 = fmap (\ p -> p*p2) (reinigungsProduktBestandteile sp2)
  in Map.unionWith (+) d1 d2

newtype Id = Id Int deriving Eq

data Entitaet state = Entitaet Id state

instance Eq (Entitaet a) where
  (Entitaet id1 _) == (Entitaet id2 _) = id1 == id2
  
newtype Menge = Menge Double deriving (Eq, Show, Ord)

zero = Menge 0

type Vorrat = Map Grundbestandteil Menge


grundbestandteilVorrat :: Vorrat -> Grundbestandteil -> Menge
grundbestandteilVorrat st ssp =
     case Map.lookup ssp st of
       Nothing -> zero
       Just a -> a

newtype ProduktName = ProduktName String
  deriving (Eq, Show, Ord)
type Bestellung = Entitaet (ProduktName, Menge)

type Katalog = Map ProduktName ReinigungsProdukt

type ProduktErmittlung a = Reader Katalog a

-- die benötigten Mengen für eine Bestellung
benoetigteMengen :: Bestellung -> Katalog -> Map Grundbestandteil Menge
benoetigteMengen (Entitaet _ (n, Menge a)) cat =
  case Map.lookup n cat of
    Nothing -> Map.empty
    Just sp ->
      fmap (\ p -> Menge (a * p)) (reinigungsProduktBestandteile sp)

-- die benötigten Mengen für eine Bestellung (ohne Kenntnis des Katalogs)
ermittleBenoetigteMengen :: Bestellung -> ProduktErmittlung (Map Grundbestandteil Menge)
ermittleBenoetigteMengen ord =
  do cat <- Reader.ask
     return (benoetigteMengen ord cat)

-- sind die angegebenen Mengen bevorratet?
sindGrundbestandteileBevorratet :: Vorrat -> Map Grundbestandteil Menge -> Bool
sindGrundbestandteileBevorratet st ams =
  Map.foldrWithKey (\ ssp am av -> av && (grundbestandteilVorrat st ssp >= am)) True ams

-- sind die für die Bestellung benötigten Mengen bevorratet?
sindGrundbestandteileFuerBestellungBevorratet :: Vorrat -> Bestellung -> Katalog -> Bool
sindGrundbestandteileFuerBestellungBevorratet st o cat =
  sindGrundbestandteileBevorratet st (benoetigteMengen o cat)

-- Invariante für den Vorrat
istVorratKorrekt :: Vorrat -> Bool
istVorratKorrekt st =
  Map.foldr (\ (Menge a) v  -> v && (a >= 0)) True st

-- ein Produkt aus dem Vorrat entnehmen
entnehmeGrundbestandteil :: Vorrat -> Grundbestandteil -> Menge -> Vorrat
entnehmeGrundbestandteil st ssp (Menge a) =
  Map.alter (\am0 -> case am0 of
                 Nothing -> Just (Menge (- a))
                 Just (Menge a0) -> Just (Menge (a0 - a)))
    ssp st

-- mehrere Produkte aus dem Vorrat entnehmen
entnehmeGrundbestandteile :: Vorrat -> Map Grundbestandteil Menge -> Vorrat
entnehmeGrundbestandteile st ams =
  Map.foldrWithKey (\ ssp am st -> entnehmeGrundbestandteil st ssp am) st ams

-- not sure this is the right thing wrt. DDD
type VorratsErmittlung a = State Vorrat a

-- wie viel eines Grundbestandteils ist in unserem Vorrat?
getGrundbestandteilMenge :: Grundbestandteil -> VorratsErmittlung Menge
getGrundbestandteilMenge ssp =
  do stock <- State.get
     return (grundbestandteilVorrat stock ssp)

data Event =
    BestellungEingegangen Bestellung
  | GrundbestandteilEntnommen Grundbestandteil Menge
  | BestellungVersandt Bestellung

-- eine Bestellung verarbeiten
verarbeiteBestellung :: Bestellung -> ProduktErmittlung [Event]
verarbeiteBestellung ord =
  do ams <- ermittleBenoetigteMengen ord
     let evs = fmap (uncurry GrundbestandteilEntnommen) (toList ams)
     return ([BestellungEingegangen ord] ++ evs ++ [BestellungVersandt ord])

-- Repository / Monad for contextual information

-- Aggregate

-- commands: orders

-- process events

-- contextual information: amounts of soap, shampoo won't work -
-- different soaps and different shampoos => map?

-- Aggregate validation?

-- 
