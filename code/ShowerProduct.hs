{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Map as Map
import qualified Data.Map.Strict (Map)
import Control.Monad.Identity

import Control.Monad.Reader

import Control.Monad.Writer

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

type ProduktErmittlungT m = ReaderT Katalog m
type ProduktErmittlung m = MonadReader Katalog m

-- die benötigten Mengen für eine Bestellung
benoetigteMengen :: Bestellung -> Katalog -> Map Grundbestandteil Menge
benoetigteMengen (Entitaet _ (n, Menge a)) cat =
  case Map.lookup n cat of
    Nothing -> Map.empty
    Just sp ->
      fmap (\ p -> Menge (a * p)) (reinigungsProduktBestandteile sp)

-- die benötigten Mengen für eine Bestellung (ohne Kenntnis des Katalogs)
ermittleBenoetigteMengen :: ProduktErmittlung m => Bestellung -> m (Map Grundbestandteil Menge)
ermittleBenoetigteMengen ord =
  do cat <- ask
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


entnehmeGrundbestandteileFuerBestellung :: Map Grundbestandteil Menge -> [Event]
entnehmeGrundbestandteileFuerBestellung bestandteile =
    fmap (uncurry GrundbestandteilEntnommen) (toList bestandteile)

-- REPL usability
-- examples

verarbeiteBestellung :: ProduktErmittlung m => Bestellung -> m [Event]
verarbeiteBestellung best =
  do teile <- ermittleBenoetigteMengen best
     return ([BestellungEingegangen best] ++ (entnehmeGrundbestandteileFuerBestellung teile) ++ [BestellungVersandt best])

-- commands: orders
data Befehl =
    SendeBestellung ProduktName Menge

type EventAggregatorT m = WriterT [Event] m
type EventAggregator m = MonadWriter [Event] m

meldeEvent :: EventAggregator m => Event -> m ()
meldeEvent ev = tell [ev]

meldeEvents :: EventAggregator m => [Event] -> m ()
meldeEvents evs = tell evs

type EntitaetGeneratorT m = StateT Int m
type EntitaetGenerator m = MonadState Int m

neueId :: EntitaetGenerator m => m Id
neueId =
  do s <- get
     put (s + 1)
     return (Id s)
     
neueEntitaet :: EntitaetGenerator m => a -> m (Entitaet a)
neueEntitaet st =
   do id <- neueId
      return (Entitaet id st)

type BefehlVerarbeitung a = EntitaetGeneratorT (EventAggregatorT (ProduktErmittlungT Identity)) a

verarbeiteBefehl :: (EntitaetGenerator m, EventAggregator m, ProduktErmittlung m) => Befehl -> m ()
verarbeiteBefehl (SendeBestellung pn m) =
   do b <- neueEntitaet (pn, m)
      evs <- verarbeiteBestellung b
      meldeEvents evs

laufVerarbeiteBefehl :: Katalog -> BefehlVerarbeitung a -> Id -> (a, Id, [Event])
laufVerarbeiteBefehl kat bv (Id id) =
  let ((ret, id), evs) = runReader (runWriterT (runStateT bv id)) kat
  in (ret, Id id, evs)

