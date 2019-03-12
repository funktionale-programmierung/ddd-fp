{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Map as Map
import qualified Data.Map.Strict (Map)

import qualified Data.Monoid (Monoid)
import Data.Monoid as Monoid

-- Domänenmodellierung / Value Objects

data Grundbestandteil =
    Tensid PH
  | Pflegestoff Haartyp
  deriving (Eq, Show, Ord)

data Haartyp = Fettig | Trocken | Normal | Schuppen
  deriving (Eq, Show, Ord)

data PH = PH Double deriving (Eq, Show, Ord)

data WaschProdukt =
    Einfach Grundbestandteil
  | Mixtur Double WaschProdukt Double WaschProdukt
  deriving (Eq, Show, Ord)

tensid = Tensid (PH 5.5)
schuppenmittel = Pflegestoff Schuppen

-- Bestellung

type Bestellung = Entitaet BestellDaten

data BestellDaten = BestellDaten ProduktName Menge
  deriving Show

-- Entität

data Entitaet daten = Entitaet Id daten
  deriving (Show)

instance Eq (Entitaet a) where
  (Entitaet id1 _) == (Entitaet id2 _) = id1 == id2

data Id = Id Int deriving (Eq, Show)

-- zurück zur Bestellung

data ProduktName = ProduktName String
  deriving (Eq, Show, Ord)

data Menge = Menge Double deriving (Eq, Show, Ord)

leereMenge = Menge 0

bestellungDuschgel = Entitaet (Id 1) (BestellDaten (ProduktName "Duschgel") (Menge 1))
bestellungShampoo = Entitaet (Id 2) (BestellDaten (ProduktName "Schuppenshampoo") (Menge 1))
bestellungShampooZuViel = Entitaet (Id 3) (BestellDaten (ProduktName "Schuppenshampoo") (Menge 20))
bestellungUnbekannt = Entitaet (Id 4) (BestellDaten (ProduktName "Erdbeermilch") (Menge 1))

-- Katalog

type Katalog = Map ProduktName WaschProdukt

derKatalog = Map.fromList [
    (ProduktName "Duschgel", Einfach tensid),
    (ProduktName "Schuppenshampoo", Mixtur 0.9 (Einfach tensid) 0.1 (Einfach schuppenmittel))
  ]

-- Vorrat

data Vorrat = Vorrat (Map Grundbestandteil Menge)
  deriving (Eq, Show)

leererVorrat = Vorrat Map.empty

derVorrat = Vorrat (Map.fromList [
    (tensid, Menge 10), (schuppenmittel, Menge 1)
  ])

vorratAus :: Grundbestandteil -> Menge -> Vorrat
vorratAus grundbestandteil menge =
  Vorrat (Map.fromList [(grundbestandteil, menge)])

-- Events

data Event =
    BestellungAkzeptiert Bestellung
  | ProduktNichtGefunden Bestellung
  | BestellungStorniert Bestellung
  | BestellungBestaetigt Bestellung
  | GrundbestandteilEntnommen Bestellung Grundbestandteil Menge
  | NichtGenugVorrat Bestellung
  | ProduktMixturt Bestellung
  | BestellungVersandt Bestellung
  deriving (Show)

-- Aggregat

bestelle :: Bestellung -> Vorrat -> Katalog -> [Event]
bestelle bestellung aktuellerVorrat katalog =
  [BestellungAkzeptiert bestellung] ++ (verarbeiteBestellung bestellung aktuellerVorrat katalog)

verarbeiteBestellung :: Bestellung -> Vorrat -> Katalog -> [Event]
verarbeiteBestellung bestellung@((Entitaet _ (BestellDaten produktname _))) gesamtVorrat katalog =
  let gewuenschtesProdukt = findeProdukt produktname katalog
  in
  case gewuenschtesProdukt of
    Nothing -> [ProduktNichtGefunden bestellung, BestellungStorniert bestellung]
    Just waschProdukt -> [BestellungBestaetigt bestellung] ++ (liefereBestellung bestellung waschProdukt gesamtVorrat)

findeProdukt :: ProduktName -> Katalog -> (Maybe WaschProdukt)
findeProdukt produktname katalog = Map.lookup produktname katalog

liefereBestellung :: Bestellung -> WaschProdukt -> Vorrat -> [Event]
liefereBestellung bestellung@((Entitaet _ (BestellDaten _ menge))) waschProdukt gesamtVorrat =
  let benoetigterVorrat@(Vorrat bestandteile) = benoetigterVorratFuer waschProdukt menge
  in
  if (vorratIstAusreichendFuer benoetigterVorrat gesamtVorrat) then
    (fmap (uncurry (GrundbestandteilEntnommen bestellung)) (Map.toList bestandteile)) ++ [ProduktMixturt bestellung, BestellungVersandt bestellung]
  else
    [NichtGenugVorrat bestellung, BestellungStorniert bestellung]

benoetigterVorratFuer :: WaschProdukt -> Menge -> Vorrat
benoetigterVorratFuer reinigungsprodukt (Menge menge) =
         (Vorrat (fmap (\ anteil -> Menge (menge * anteil)) (reinigungsProduktBestandteile reinigungsprodukt)))

reinigungsProduktBestandteile :: WaschProdukt -> Map Grundbestandteil Double
reinigungsProduktBestandteile (Einfach bestandteil) = Map.singleton bestandteil 1.0
reinigungsProduktBestandteile (Mixtur menge1 produkt1 menge2 produkt2) =
  let bestandteil1 = fmap (\ p -> p * menge1) (reinigungsProduktBestandteile produkt1)
      bestandteil2 = fmap (\ p -> p * menge2) (reinigungsProduktBestandteile produkt2)
  in Map.unionWith (+) bestandteil1 bestandteil2

-- ein Produkt aus dem Vorrat entnehmen
entnehmeGrundbestandteil :: Vorrat -> Grundbestandteil -> Menge -> Vorrat
entnehmeGrundbestandteil (Vorrat vorrat) grundbestandteil (Menge menge) =
  Vorrat
    (Map.alter (\ mengeVorrat -> case mengeVorrat of
                    Nothing -> Just (Menge (- menge))
                    Just (Menge mengeVorrat) -> Just (Menge (mengeVorrat - menge)))
       grundbestandteil vorrat)

vorratIstAusreichendFuer :: Vorrat -> Vorrat -> Bool
vorratIstAusreichendFuer benoetigt gesamt =
  istVorratKorrekt (entnehmeVorrat gesamt benoetigt)

-- Invariante für den Vorrat
istVorratKorrekt :: Vorrat -> Bool
istVorratKorrekt (Vorrat vorrat) =
  Map.foldr (\ (Menge menge) istKorrekt -> istKorrekt && (menge >= 0)) True vorrat

entnehmeVorrat' :: Vorrat -> Vorrat -> Vorrat
entnehmeVorrat' gesamt (Vorrat benoetigt) =
  Map.foldrWithKey (\ grundbestandteil menge vorrat ->
                      entnehmeGrundbestandteil vorrat grundbestandteil menge)
    gesamt benoetigt

-- alternative Version entnehmeVorrat mit Gruppen

entnehmeVorrat :: Vorrat -> Vorrat -> Vorrat
entnehmeVorrat gesamt benoetigt = gesamt <> (invert benoetigt)

class Monoid g => Group g where
  invert :: g -> g

instance Group Vorrat where
  invert (Vorrat bestandteile) = Vorrat (fmap invert bestandteile)

instance Group Menge where
  invert (Menge menge) = Menge (- menge)

instance Monoid Menge where
  mempty = leereMenge

instance Semigroup Menge where
  (Menge m1) <> (Menge m2) = Menge (m1 + m2)

instance Semigroup Vorrat where
  (Vorrat map1) <> (Vorrat map2) =
    Vorrat (unionWith (<>) map1 map2)

instance Monoid Vorrat where
  mempty = leererVorrat

bestelldatenUnbekannt = BestellDaten (ProduktName "Erdbeermilch") (Menge 1)
bestelldatenShampoo = BestellDaten (ProduktName "Schuppenshampoo") (Menge 1)

-- Fold über Events

eventEffektAufVorrat :: Vorrat -> Event -> Vorrat
eventEffektAufVorrat vorrat (GrundbestandteilEntnommen bestellung grundbestandteil menge) =
  entnehmeVorrat vorrat (vorratAus grundbestandteil menge)
eventEffektAufVorrat vorrat _ = vorrat

eventsEffektAufVorrat :: Vorrat -> [Event] -> Vorrat
eventsEffektAufVorrat vorrat events =
  Prelude.foldr (\ event vorrat ->
           eventEffektAufVorrat vorrat event)
    vorrat events

-- Version mit Repository / Monade

bestelle' :: InterfaceIdGenerator m => BestellDaten -> Vorrat -> Katalog -> m [Event]
bestelle' bestelldaten aktuellerVorrat katalog =
  do bestellung <- baueEntitaet bestelldaten
     return ([BestellungAkzeptiert bestellung] ++ (verarbeiteBestellung bestellung aktuellerVorrat katalog))

class Monad m => InterfaceIdGenerator m where
  newId :: m Id

------ Application Layer

baueEntitaet :: InterfaceIdGenerator m => d -> m (Entitaet d)
baueEntitaet daten =
  do id <- newId
     return (Entitaet id daten)

data IdGenerator a = IdGenerator (Int -> (Int, a))

runIdGenerator (IdGenerator idgf) n = idgf n

instance InterfaceIdGenerator IdGenerator where
  newId :: IdGenerator Id
  newId = IdGenerator (\ n -> (n+1, Id n))

instance Monad IdGenerator where
  (>>=)  :: IdGenerator a -> (  a -> IdGenerator b) -> IdGenerator b
  (IdGenerator idgf) >>= f =
    IdGenerator (\ n ->
                   let (n', aelem) = idgf n
                       (IdGenerator idgf') = f aelem
                   in idgf' n')
  return ::  a -> IdGenerator a
  return x = IdGenerator (\ n -> (n, x))

instance Applicative IdGenerator where
  pure :: a -> IdGenerator a
  pure x = IdGenerator (\ n -> (n, x))
  (<*>) :: IdGenerator (a -> b) -> IdGenerator a -> IdGenerator b
  (IdGenerator f) <*> (IdGenerator idgf) =
    IdGenerator (\ n ->
                   let (n', f') = f n
                       (n'', aelem) = idgf n'
                   in (n'', f' aelem))

instance Functor IdGenerator where
  fmap :: (a -> b) -> IdGenerator a -> IdGenerator b
  fmap f (IdGenerator idgf) =
    IdGenerator (\ n ->
                   let (n', aelem) = idgf n
                   in (n', f aelem))

