{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Data.Map as Map
import qualified Data.Map.Strict (Map)

import qualified Data.Monoid (Monoid)
import Data.Monoid as Monoid

-- Domänenmodellierung / Value Objects

data Haartyp = Fettig | Trocken | Normal | Schuppen
  deriving (Eq, Show, Ord)

data Grundbestandteil =
    Tensid PH
  | Pflegestoff Haartyp
  deriving (Eq, Show, Ord)

data PH = PH Double deriving (Eq, Show, Ord)

data WaschProdukt =
    Einfach Grundbestandteil
  | Mixtur Double WaschProdukt WaschProdukt
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

{-
class Eq a where
   (==) :: a -> a -> Bool
-}

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
    (ProduktName "Schuppenshampoo", Mixtur 0.9 (Einfach tensid) (Einfach schuppenmittel))
  ]

-- Vorrat

data Vorrat = Vorrat (Map Grundbestandteil Menge)
  deriving (Eq, Show)

leererVorrat = Vorrat Map.empty

derVorrat = Vorrat (Map.fromList [
    (tensid, Menge 10), (schuppenmittel, Menge 1)
  ])

-- Events

data Event =
    BestellungAkzeptiert Bestellung
  | ProduktNichtGefunden Bestellung
  | BestellungStorniert Bestellung
  | BestellungBestaetigt Bestellung
  | GrundbestandteilEntnommen Bestellung Grundbestandteil Menge
  | NichtGenugVorrat Bestellung
  | ProduktGemischt Bestellung
  | BestellungVersandt Bestellung
  deriving (Show)

-- Aggregat

bestelle :: Bestellung -> Vorrat -> Katalog -> [Event]
bestelle bestellung aktuellerVorrat katalog = undefined

verarbeiteBestellung :: Bestellung -> Vorrat -> Katalog -> [Event]
verarbeiteBestellung bestellung gesamtVorrat katalog = undefined

findeProdukt :: ProduktName -> Katalog -> (Maybe WaschProdukt)
findeProdukt produktname katalog = Map.lookup produktname katalog

liefereBestellung :: Bestellung -> WaschProdukt -> Vorrat -> [Event]
liefereBestellung bestellung@((Entitaet _ (BestellDaten _ menge))) waschProdukt gesamtVorrat =
  let benoetigterVorrat@(Vorrat bestandteile) = benoetigterVorratFuer waschProdukt menge
  in
  if (vorratIstAusreichendFuer benoetigterVorrat gesamtVorrat) then
    undefined -- GrundbestandteilEntnommen für jeden Grundbestandteil
  else
    [NichtGenugVorrat bestellung, BestellungStorniert bestellung]

benoetigterVorratFuer :: WaschProdukt -> Menge -> Vorrat
benoetigterVorratFuer reinigungsprodukt (Menge menge) =
 (Vorrat (fmap (\ anteil -> Menge (menge * anteil))
               (reinigungsProduktBestandteile reinigungsprodukt)))

reinigungsProduktBestandteile :: WaschProdukt -> Map Grundbestandteil Double
reinigungsProduktBestandteile (Einfach bestandteil) = Map.singleton bestandteil 1.0
reinigungsProduktBestandteile (Mixtur menge1 produkt1 produkt2) =
  let menge2 = 1 - menge1
      bestandteil1 = fmap (\ p -> p * menge1) (reinigungsProduktBestandteile produkt1)
      bestandteil2 = fmap (\ p -> p * menge2) (reinigungsProduktBestandteile produkt2)
  in Map.unionWith (+) bestandteil1 bestandteil2

vorratIstAusreichendFuer :: Vorrat -> Vorrat -> Bool
vorratIstAusreichendFuer benoetigt gesamt =
  istVorratKorrekt (entnehmeVorrat gesamt benoetigt)

-- Invariante für den Vorrat
-- :info Map.foldr
istVorratKorrekt :: Vorrat -> Bool
istVorratKorrekt (Vorrat vorrat) =
  undefined

entnehmeVorrat :: Vorrat -> Vorrat -> Vorrat
entnehmeVorrat gesamt (Vorrat benoetigt) =
  Map.foldrWithKey (\ grundbestandteil menge vorrat ->
                      entnehmeGrundbestandteil vorrat grundbestandteil menge)
    gesamt benoetigt

-- ein Produkt aus dem Vorrat entnehmen
entnehmeGrundbestandteil :: Vorrat -> Grundbestandteil -> Menge -> Vorrat
entnehmeGrundbestandteil (Vorrat vorrat) grundbestandteil (Menge menge) =
  Vorrat
    (Map.alter (\ mengeVorrat -> case mengeVorrat of
                    Nothing -> Just (Menge (- menge))
                    Just (Menge mengeVorrat) -> Just (Menge (mengeVorrat - menge)))
       grundbestandteil vorrat)

-- Fold über Events

eventsEffektAufVorrat :: Vorrat -> [Event] -> Vorrat
eventsEffektAufVorrat vorrat events =
  Prelude.foldr (\ event vorrat ->
           eventEffektAufVorrat vorrat event)
    vorrat events

eventEffektAufVorrat :: Vorrat -> Event -> Vorrat
eventEffektAufVorrat vorrat (GrundbestandteilEntnommen bestellung grundbestandteil menge) =
  entnehmeVorrat vorrat (vorratAus grundbestandteil menge)
eventEffektAufVorrat vorrat _ = vorrat

vorratAus :: Grundbestandteil -> Menge -> Vorrat
vorratAus grundbestandteil menge =
  Vorrat (Map.fromList [(grundbestandteil, menge)])

-- Version mit Repository / Monade

bestelldatenUnbekannt = BestellDaten (ProduktName "Erdbeermilch") (Menge 1)
bestelldatenShampoo = BestellDaten (ProduktName "Schuppenshampoo") (Menge 1)


