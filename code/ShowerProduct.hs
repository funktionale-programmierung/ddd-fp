{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Map as Map
import qualified Data.Map.Strict (Map)
import Control.Monad.Identity

import qualified Control.Monad.Reader (ReaderT)
import Control.Monad.Reader as Reader
  
import Control.Monad.State.Lazy as State
import qualified Control.Monad.State.Lazy (State)

import qualified Data.Monoid (Monoid)
import Data.Monoid as Monoid

import qualified Data.Group (Group)
import Data.Group as Group

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

ersteId :: Id
ersteId = Id 0

naechsteId :: Id -> Id
naechsteId (Id id) = Id (id + 1)

class Monad m => MonadIdGenerator m where
  newId :: m Id

newtype IdGeneratorT m a = IdGeneratorT { runIdGeneratorT :: Int -> m (a, Int) }

instance Functor m => Functor (IdGeneratorT m) where
    fmap f m = IdGeneratorT (\ n ->
        fmap (\ (a, n') -> (f a, n')) (runIdGeneratorT m n))

instance (Functor m, Monad m) => Applicative (IdGeneratorT m) where
    pure a = IdGeneratorT (\ n -> return (a, n))

    IdGeneratorT mf <*> IdGeneratorT mx =
      IdGeneratorT (\ n ->
        do (f, n') <- mf n
           (x, n'') <- mx n'
           return (f x, n''))

instance (Monad m) => Monad (IdGeneratorT m) where
    return a = IdGeneratorT (\ n -> return (a, n))
    m >>= k  =
      IdGeneratorT (\ n ->
        do (a, n') <- runIdGeneratorT m n
           runIdGeneratorT (k a) n')
    fail str = IdGeneratorT (\ _ -> fail str)

instance Monad m => MonadIdGenerator (IdGeneratorT m) where
  newId = IdGeneratorT (\ n -> return (Id n, n+1))

newtype IdGenerator a = IdGenerator { runIdGenerator :: Int -> (a, Int) }

instance Functor IdGenerator where
  fmap f m =
    IdGenerator (\ n ->
                   let (x, n') = runIdGenerator m n
                   in (f x, n'))
                            

instance Applicative IdGenerator where
  pure x = IdGenerator (\ n -> (x, n))

  gf <*> gx =
    IdGenerator (\ n ->
                   let (f, n') = runIdGenerator gf n
                       (x, n'') = runIdGenerator gx n'
                   in (f x, n''))

instance Monad IdGenerator where
  return x = IdGenerator (\ n -> (x, n))
  m >>= k  =
    IdGenerator (\ n ->
                   let (x, n') = runIdGenerator m n
                   in runIdGenerator (k x) n')

instance MonadIdGenerator IdGenerator where
  newId = IdGenerator (\ n -> (Id n, n+1))
    
instance MonadIdGenerator m => MonadIdGenerator (StateT s m) where
  newId = lift newId

instance MonadIdGenerator m => MonadIdGenerator (ReaderT s m) where
  newId = lift newId

data Entitaet daten = Entitaet Id daten

instance Eq (Entitaet a) where
  (Entitaet id1 _) == (Entitaet id2 _) = id1 == id2
  
newtype Menge = Menge Double deriving (Eq, Show, Ord)

leer = Menge 0

instance Semigroup Menge where
  (Menge menge1) <> (Menge menge2) = Menge (menge1 + menge2)

instance Monoid Menge where
  mempty = leer

instance Group Menge where
  invert (Menge menge) = Menge (-menge)

newtype Vorrat = Vorrat (Map Grundbestandteil Menge)
  deriving (Eq, Show)

leererVorrat = Vorrat Map.empty

instance Semigroup Vorrat where
  (Vorrat vorrat1) <> (Vorrat vorrat2) = Vorrat (vorrat1 <> vorrat2)

instance Monoid Vorrat where
  mempty = leererVorrat

instance Group Vorrat where
  invert (Vorrat vorrat) = Vorrat (fmap invert vorrat)
  
grundbestandteilVorrat :: Vorrat -> Grundbestandteil -> Menge
grundbestandteilVorrat (Vorrat vorrat) grundbestandteil =
     case Map.lookup grundbestandteil vorrat of
       Nothing -> leer
       Just menge -> menge

newtype ProduktName = ProduktName String
  deriving (Eq, Show, Ord)
type Bestellung = Entitaet (ProduktName, Menge)

type Katalog = Map ProduktName WaschProdukt

class Monad m => ProduktErmittlung m where
  aktuellerKatalog :: m Katalog

findeProdukt :: ProduktErmittlung m => ProduktName -> m (Maybe WaschProdukt)
findeProdukt produktname =
  do katalog <- aktuellerKatalog
     return (Map.lookup produktname katalog)

-- die benötigten Mengen für eine Bestellung
benoetigteMengen :: ProduktErmittlung m => Bestellung -> m (Map Grundbestandteil Menge)
benoetigteMengen (Entitaet _ (produktname, Menge menge)) =
  do katalog <- aktuellerKatalog
     case Map.lookup produktname katalog of
       Nothing -> return Map.empty
       Just reinigungsprodukt ->
         return (fmap (\ anteil -> Menge (menge * anteil)) (reinigungsProduktBestandteile reinigungsprodukt))

-- der benötigte Vorrat für eine Bestellung
benoetigterVorrat :: ProduktErmittlung m => Bestellung -> m Vorrat
benoetigterVorrat (Entitaet _ (produktname, Menge menge)) =
  do katalog <- aktuellerKatalog
     case Map.lookup produktname katalog of
       Nothing -> return leererVorrat
       Just reinigungsprodukt ->
         return (Vorrat (fmap (\ anteil -> Menge (menge * anteil)) (reinigungsProduktBestandteile reinigungsprodukt)))

-- sind die angegebenen Mengen bevorratet?
sindGrundbestandteileBevorratet :: Vorrat -> Map Grundbestandteil Menge -> Bool
sindGrundbestandteileBevorratet vorrat mengen =
  Map.foldrWithKey (\ grundbestandteil menge istBevorratet ->
                      istBevorratet && (grundbestandteilVorrat vorrat grundbestandteil >= menge))
    True mengen

-- sind die für die Bestellung benötigten Mengen bevorratet?
sindGrundbestandteileFuerBestellungBevorratet :: ProduktErmittlung m => Vorrat -> Bestellung -> m Bool
sindGrundbestandteileFuerBestellungBevorratet vorrat bestellung =
  do mengen <- benoetigteMengen bestellung
     return (sindGrundbestandteileBevorratet vorrat mengen)

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

-- dito, refaktorisiert
vorratMinus :: Vorrat -> Vorrat -> Vorrat
vorratMinus vorrat1 vorrat2 = vorrat1 <> (invert vorrat2)

-- REPL usability
-- examples

data Command =
   AkzeptiereBestellung ProduktName Menge
 | BestaetigeBestellung Bestellung
 | StorniereBestellung Bestellung
 | BearbeiteBestellung Bestellung
 | MeldeProduktNichtGefunden ProduktName
 | MischeProdkt Bestellung
 | SendeBestellung Bestellung

data Event =
    BestellungAkzeptiert Bestellung
  | ProduktNichtGefunden Bestellung
  | BestellungStorniert Bestellung
  | BestellungBestaetigt Bestellung
  | GrundbestandteilEntnommen Bestellung Grundbestandteil Menge
  | NichtGenugVorrat Bestellung
  | ProduktGemischt Bestellung
  | BestellungVersandt Bestellung

type VorratAggregatorT m = StateT Vorrat m
type VorratAggregator m = MonadState Vorrat m

entnehmeGrundbestandteileFuerBestellung :: Bestellung -> Map Grundbestandteil Menge -> [Event]
entnehmeGrundbestandteileFuerBestellung bestellung bestandteile =
    fmap (uncurry (GrundbestandteilEntnommen bestellung)) (toList bestandteile)

bestellungsVorratsEntnahme :: Bestellung -> Vorrat -> [Event]
bestellungsVorratsEntnahme bestellung (Vorrat vorrat) =
    fmap (uncurry (GrundbestandteilEntnommen bestellung)) (toList vorrat)

type EntitaetGenerator = IdGenerator
type MonadEntitaetGenerator m = MonadIdGenerator m

neueEntitaet :: MonadEntitaetGenerator m => a -> m (Entitaet a)
neueEntitaet zustand =
   do id <- newId
      return (Entitaet id zustand)

aktuellerVorrat :: VorratAggregator m => m Vorrat
aktuellerVorrat = State.get

entnimmVorrat :: VorratAggregator m => Vorrat -> m ()
entnimmVorrat vorrat =
  do lagerVorrat <- State.get
     State.put (vorratMinus lagerVorrat vorrat)

verarbeiteCommand :: (MonadEntitaetGenerator m, ProduktErmittlung m, VorratAggregator m) => Command -> m [Event]
verarbeiteCommand (AkzeptiereBestellung produktname menge) =
  do bestellung <- neueEntitaet (produktname, menge)
     return [BestellungAkzeptiert bestellung]
verarbeiteCommand (BestaetigeBestellung (bestellung@(Entitaet _ (produktname, menge)))) =
  do maybeProdukt <- findeProdukt produktname
     case maybeProdukt of
       Nothing -> return [ProduktNichtGefunden bestellung]
       Just produkt -> return [BestellungBestaetigt bestellung]
verarbeiteCommand (BearbeiteBestellung bestellung)  =
  do bestellungVorrat <- benoetigterVorrat bestellung
     aktuellerVorrat <- aktuellerVorrat
     if (istVorratKorrekt (vorratMinus aktuellerVorrat bestellungVorrat)) then
       do entnimmVorrat bestellungVorrat
          return (bestellungsVorratsEntnahme bestellung aktuellerVorrat)
     else
       return [NichtGenugVorrat bestellung]

type ProduktErmittlungT m = ReaderT Katalog m

instance Monad m => ProduktErmittlung (ProduktErmittlungT m) where
  aktuellerKatalog = Reader.ask

type Bearbeitung a = (ProduktErmittlungT (VorratAggregatorT EntitaetGenerator)) a

bearbeite :: Katalog -> Id -> Vorrat -> Bearbeitung a -> (a, Id, Vorrat)
bearbeite katalog (Id n) vorrat bearbeitung =
  let ((x, vorrat'), n') = runIdGenerator (runStateT (runReaderT bearbeitung katalog) vorrat) n
  in (x, Id n', vorrat')

bearbeiteAlles :: Katalog -> Bearbeitung a -> a
bearbeiteAlles katalog bearbeitung =
  let (x, _, _) = bearbeite katalog ersteId leererVorrat bearbeitung
  in x

