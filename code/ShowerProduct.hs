{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Map as Map
import qualified Data.Map.Strict (Map)
import Control.Monad.Identity

import qualified Control.Monad.Reader (ReaderT)
import Control.Monad.Reader as Reader
  
import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer

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
grundbestandteilVorrat vorrat grundbestandteil =
     case Map.lookup grundbestandteil vorrat of
       Nothing -> zero
       Just menge -> menge

newtype ProduktName = ProduktName String
  deriving (Eq, Show, Ord)
type Bestellung = Entitaet (ProduktName, Menge)

type Katalog = Map ProduktName ReinigungsProdukt

type ProduktErmittlungT m = ReaderT Katalog m
type ProduktErmittlung m = MonadReader Katalog m

-- die benötigten Mengen für eine Bestellung
benoetigteMengen :: Bestellung -> Katalog -> Map Grundbestandteil Menge
benoetigteMengen (Entitaet _ (produktname, Menge menge)) katalog =
  case Map.lookup produktname katalog of
    Nothing -> Map.empty
    Just reinigungsprodukt ->
      fmap (\ anteil -> Menge (menge * anteil)) (reinigungsProduktBestandteile reinigungsprodukt)

-- die benötigten Mengen für eine Bestellung (ohne Kenntnis des Katalogs)
ermittleBenoetigteMengen :: ProduktErmittlung m => Bestellung -> m (Map Grundbestandteil Menge)
ermittleBenoetigteMengen bestellung =
  do katalog <- Reader.ask
     return (benoetigteMengen bestellung katalog)

-- sind die angegebenen Mengen bevorratet?
sindGrundbestandteileBevorratet :: Vorrat -> Map Grundbestandteil Menge -> Bool
sindGrundbestandteileBevorratet vorrat mengen =
  Map.foldrWithKey (\ grundbestandteil menge istBevorratet ->
                      istBevorratet && (grundbestandteilVorrat vorrat grundbestandteil >= menge))
    True mengen

-- sind die für die Bestellung benötigten Mengen bevorratet?
sindGrundbestandteileFuerBestellungBevorratet :: Vorrat -> Bestellung -> Katalog -> Bool
sindGrundbestandteileFuerBestellungBevorratet vorrat bestellung katalog =
  sindGrundbestandteileBevorratet vorrat (benoetigteMengen bestellung katalog)

-- Invariante für den Vorrat
istVorratKorrekt :: Vorrat -> Bool
istVorratKorrekt vorrat =
  Map.foldr (\ (Menge menge) istKorrekt  -> istKorrekt && (menge >= 0)) True vorrat

-- ein Produkt aus dem Vorrat entnehmen
entnehmeGrundbestandteil :: Vorrat -> Grundbestandteil -> Menge -> Vorrat
entnehmeGrundbestandteil vorrat grundbestandteil (Menge menge) =
  Map.alter (\mengeVorrat -> case mengeVorrat of
                 Nothing -> Just (Menge (- menge))
                 Just (Menge mengeVorrat) -> Just (Menge (mengeVorrat - menge)))
    grundbestandteil vorrat

-- mehrere Produkte aus dem Vorrat entnehmen
entnehmeGrundbestandteile :: Vorrat -> Map Grundbestandteil Menge -> Vorrat
entnehmeGrundbestandteile vorrat mengen =
  Map.foldrWithKey (\ grundbestandteil menge vorrat ->
                      entnehmeGrundbestandteil vorrat grundbestandteil menge)
    vorrat mengen

-- not sure this is the right thing wrt. DDD
type VorratsErmittlung a = State Vorrat a

-- wie viel eines Grundbestandteils ist in unserem Vorrat?
getGrundbestandteilMenge :: Grundbestandteil -> VorratsErmittlung Menge
getGrundbestandteilMenge grundbestandteil =
  do stock <- State.get
     return (grundbestandteilVorrat stock grundbestandteil)

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
verarbeiteBestellung bestellung =
  do teile <- ermittleBenoetigteMengen bestellung
     return ([BestellungEingegangen bestellung]
             ++ (entnehmeGrundbestandteileFuerBestellung teile)
             ++ [BestellungVersandt bestellung])

-- commands: orders
data Command =
    SendeBestellung ProduktName Menge

type EventAggregatorT m = WriterT [Event] m
type EventAggregator m = MonadWriter [Event] m

meldeEvent :: EventAggregator m => Event -> m ()
meldeEvent event = Writer.tell [event]

meldeEvents :: EventAggregator m => [Event] -> m ()
meldeEvents events = Writer.tell events

type EntitaetGeneratorT m = StateT Int m
type EntitaetGenerator m = MonadState Int m

neueId :: EntitaetGenerator m => m Id
neueId =
  do s <- State.get
     State.put (s + 1)
     return (Id s)
     
neueEntitaet :: EntitaetGenerator m => a -> m (Entitaet a)
neueEntitaet zustand =
   do id <- neueId
      return (Entitaet id zustand)

type CommandVerarbeitung a = EntitaetGeneratorT (EventAggregatorT (ProduktErmittlungT Identity)) a

verarbeiteCommand :: (EntitaetGenerator m, EventAggregator m, ProduktErmittlung m) => Command -> m ()
verarbeiteCommand (SendeBestellung produktname menge) =
   do bestellung <- neueEntitaet (produktname, menge)
      events <- verarbeiteBestellung bestellung
      meldeEvents events

laufVerarbeiteCommand :: Katalog -> CommandVerarbeitung a -> Id -> (a, Id, [Event])
laufVerarbeiteCommand katalog befehlverarbeitung (Id id) =
  let ((ret, id), events) = runReader (runWriterT (runStateT befehlverarbeitung id)) katalog
  in (ret, Id id, events)

