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
  
data WaschProdukt =
    Einfach Grundbestandteil
  | Gemisch Double WaschProdukt Double WaschProdukt
  deriving (Eq, Show, Ord)

tensidAnteil :: WaschProdukt -> Double
tensidAnteil (Einfach (Tensid _ _)) = 1.0
tensidAnteil (Einfach (Pflegestoff _ _)) = 0.0
tensidAnteil (Gemisch menge1 produkt1 menge2 produkt2) =
  menge1 * (tensidAnteil produkt1) + menge2 * (tensidAnteil produkt2)

reinigungsProduktBestandteile :: WaschProdukt -> Map Grundbestandteil Double
reinigungsProduktBestandteile (Einfach bestandteil) = Map.singleton bestandteil 1.0
reinigungsProduktBestandteile (Gemisch menge1 produkt1 menge2 produkt2) =
  let bestandteil1 = fmap (\ p -> p * menge1) (reinigungsProduktBestandteile produkt1)
      bestandteil2 = fmap (\ p -> p * menge2) (reinigungsProduktBestandteile produkt2)
  in Map.unionWith (+) bestandteil1 bestandteil2

newtype Id = Id Int deriving Eq

data Entitaet daten = Entitaet Id daten

instance Eq (Entitaet a) where
  (Entitaet id1 _) == (Entitaet id2 _) = id1 == id2
  
newtype Menge = Menge Double deriving (Eq, Show, Ord)

leer = Menge 0

newtype Vorrat = Vorrat (Map Grundbestandteil Menge)


grundbestandteilVorrat :: Vorrat -> Grundbestandteil -> Menge
grundbestandteilVorrat (Vorrat vorrat) grundbestandteil =
     case Map.lookup grundbestandteil vorrat of
       Nothing -> leer
       Just menge -> menge

newtype ProduktName = ProduktName String
  deriving (Eq, Show, Ord)
type Bestellung = Entitaet (ProduktName, Menge)

type Katalog = Map ProduktName WaschProdukt

type ProduktErmittlungT m = ReaderT Katalog m
type ProduktErmittlung m = MonadReader Katalog m

findeProdukt :: ProduktErmittlung m => ProduktName -> m (Maybe WaschProdukt)
findeProdukt produktname =
  do katalog <- Reader.ask
     return (Map.lookup produktname katalog)

-- die benötigten Mengen für eine Bestellung
benoetigteMengen :: ProduktErmittlung m => Bestellung -> m (Map Grundbestandteil Menge)
benoetigteMengen (Entitaet _ (produktname, Menge menge)) =
  do katalog <- Reader.ask
     case Map.lookup produktname katalog of
       Nothing -> return Map.empty
       Just reinigungsprodukt ->
         return (fmap (\ anteil -> Menge (menge * anteil)) (reinigungsProduktBestandteile reinigungsprodukt))

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
istVorratKorrekt (Vorrat vorrat) =
  Map.foldr (\ (Menge menge) istKorrekt -> istKorrekt && (menge >= 0)) True vorrat

-- ein Produkt aus dem Vorrat entnehmen
entnehmeGrundbestandteil :: Vorrat -> Grundbestandteil -> Menge -> Vorrat
entnehmeGrundbestandteil (Vorrat vorrat) grundbestandteil (Menge menge) =
  Vorrat
    (Map.alter (\ mengeVorrat -> case mengeVorrat of
                    Nothing -> Just (Menge (- menge))
                    Just (Menge mengeVorrat) -> Just (Menge (mengeVorrat - menge)))
       grundbestandteil vorrat)

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
  do vorrat <- State.get
     return (grundbestandteilVorrat vorrat grundbestandteil)

data Event =
    BestellungAkzeptiert Bestellung
  | ProduktNichtGefunden Bestellung
  | BestellungStorniert Bestellung
  | BestellungBestaetigt Bestellung
  | GrundbestandteilEntnommen Bestellung Grundbestandteil Menge
  | ProduktGemischt Bestellung
  | BestellungVersandt Bestellung

entnehmeGrundbestandteileFuerBestellung :: Bestellung -> Map Grundbestandteil Menge -> [Event]
entnehmeGrundbestandteileFuerBestellung bestellung bestandteile =
    fmap (uncurry (GrundbestandteilEntnommen bestellung)) (toList bestandteile)

-- REPL usability
-- examples

{-
verarbeiteBestellung :: ProduktErmittlung m => Bestellung -> m [Event]
verarbeiteBestellung bestellung =
  do bestandteile <- ermittleBenoetigteMengen bestellung
     return ([BestellungEingegangen bestellung]
             ++ (entnehmeGrundbestandteileFuerBestellung bestandteile)
             ++ [BestellungVersandt bestellung])
-}

data Command =
   AkzeptiereBestellung ProduktName Menge
 | BestaetigeBestellung Bestellung
 | StorniereBestellung Bestellung
 | BearbeiteBestellung Bestellung
 | MeldeProduktNichtGefunden ProduktName
 | MischeProdkt Bestellung
 | SendeBestellung Bestellung

type EventAggregatorT m = WriterT [Event] m
type EventAggregator m = MonadWriter [Event] m

meldeEvent :: EventAggregator m => Event -> m ()
meldeEvent event = Writer.tell [event]

meldeEvents :: EventAggregator m => [Event] -> m ()
meldeEvents events = Writer.tell events

type CommandAggregatorT m = WriterT [Command] m
type CommandAggregator m = MonadWriter [Command] m

registriereCommands :: CommandAggregator m => [Command] -> m ()
registriereCommands commands = Writer.tell commands

registriereCommand :: CommandAggregator m => Command -> m ()
registriereCommand command = registriereCommands [command]

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

verarbeiteCommand :: (ProduktErmittlung m, EntitaetGenerator m, EventAggregator m) => Command -> m ()
verarbeiteCommand (AkzeptiereBestellung produktname menge) =
  do bestellung <- neueEntitaet (produktname, menge)
     meldeEvent (BestellungAkzeptiert bestellung)
verarbeiteCommand (BestaetigeBestellung (bestellung@(Entitaet _ (produktname, menge)))) =
  do maybeProdukt <- findeProdukt produktname
     case maybeProdukt of
       Nothing -> meldeEvent (ProduktNichtGefunden bestellung)
       Just produkt -> meldeEvent (BestellungBestaetigt bestellung)
verarbeiteCommand (StorniereBestellung bestellung) =
  meldeEvent (BestellungStorniert bestellung)

  
             
{-
type CommandVerarbeitung a = EntitaetGeneratorT (EventAggregatorT (ProduktErmittlungT Identity)) a

bereiteCommandVerarbeitungVor :: (EntitaetGenerator m, EventAggregator m, ProduktErmittlung m) => Command -> m ()
bereiteCommandVerarbeitungVor (SendeBestellung produktname menge) =
   do bestellung <- neueEntitaet (produktname, menge)
      events <- verarbeiteBestellung bestellung
      meldeEvents events

verarbeiteCommand :: Katalog -> CommandVerarbeitung a -> Id -> (a, Id, [Event])
verarbeiteCommand katalog commandverarbeitung (Id id) =
  let ((ret, id), events) = runReader (runWriterT (runStateT commandverarbeitung id)) katalog
  in (ret, Id id, events)
-}
