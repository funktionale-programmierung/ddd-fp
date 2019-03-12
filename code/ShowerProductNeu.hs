{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Map as Map
import qualified Data.Map.Strict (Map)

import Control.Monad.State.Lazy as State
import qualified Control.Monad.State.Lazy (State)

import qualified Data.Monoid (Monoid)
import Data.Monoid as Monoid

--import qualified Data.Group (Group)
--import Data.Group as Group

class Monoid g => Group g where
  invert :: g -> g

verarbeiteBestellung :: Bestellung -> Vorrat -> Katalog -> [Event]
verarbeiteBestellung bestellung aktuellerVorrat katalog =
  [BestellungAkzeptiert bestellung] ++ (findeProduktMischeUndVersendeBestellung bestellung aktuellerVorrat katalog)

findeProduktMischeUndVersendeBestellung :: Bestellung -> Vorrat -> Katalog -> [Event]
findeProduktMischeUndVersendeBestellung bestellung@((Entitaet _ (produktname, _))) gesamtVorrat katalog =
  let gewuenschtesProdukt = findeProdukt produktname katalog
  in
  case gewuenschtesProdukt of
    Nothing -> [ProduktNichtGefunden bestellung, BestellungStorniert bestellung]
    Just waschProdukt -> [BestellungBestaetigt bestellung] ++ (mischeUndVersendeBestellung bestellung waschProdukt gesamtVorrat)

findeProdukt :: ProduktName -> Katalog -> (Maybe WaschProdukt)
findeProdukt produktname katalog = Map.lookup produktname katalog


mischeUndVersendeBestellung :: Bestellung -> WaschProdukt -> Vorrat -> [Event]
mischeUndVersendeBestellung bestellung@((Entitaet _ (_, menge))) waschProdukt gesamtVorrat =
  let benoetigterVorrat@(Vorrat bestandteile) = benoetigterVorratFuer waschProdukt menge
  in
  if (vorratIstAusreichendFuer benoetigterVorrat gesamtVorrat) then
    (fmap (uncurry (GrundbestandteilEntnommen bestellung)) (Map.toList bestandteile)) ++ [ProduktGemischt bestellung, BestellungVersandt bestellung]
  else
    [NichtGenugVorrat bestellung, BestellungStorniert bestellung]


benoetigterVorratFuer :: WaschProdukt -> Menge -> Vorrat
benoetigterVorratFuer reinigungsprodukt (Menge menge) =
         (Vorrat (fmap (\ anteil -> Menge (menge * anteil)) (reinigungsProduktBestandteile reinigungsprodukt)))

reinigungsProduktBestandteile :: WaschProdukt -> Map Grundbestandteil Double
reinigungsProduktBestandteile (Einfach bestandteil) = Map.singleton bestandteil 1.0
reinigungsProduktBestandteile (Gemisch menge1 produkt1 menge2 produkt2) =
  let bestandteil1 = fmap (\ p -> p * menge1) (reinigungsProduktBestandteile produkt1)
      bestandteil2 = fmap (\ p -> p * menge2) (reinigungsProduktBestandteile produkt2)
  in Map.unionWith (+) bestandteil1 bestandteil2

vorratIstAusreichendFuer :: Vorrat -> Vorrat -> Bool
vorratIstAusreichendFuer benoetigt gesamt =
  istVorratKorrekt (gesamt <> (invert benoetigt))


instance Group Vorrat where
  invert (Vorrat bestandteile) = Vorrat (fmap invert bestandteile)

instance Group Menge where
  invert (Menge menge) = Menge (- menge)

instance Monoid Menge where
  mempty = leereMenge

instance Semigroup Menge where
  (Menge m1) <> (Menge m2) = Menge (m1 + m2)


-- Invariante für den Vorrat
istVorratKorrekt :: Vorrat -> Bool
istVorratKorrekt (Vorrat vorrat) =
  Map.foldr (\ (Menge menge) istKorrekt -> istKorrekt && (menge >= 0)) True vorrat

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

newtype Id = Id Int deriving (Eq, Show)

data Entitaet daten = Entitaet Id daten
  deriving (Show)

instance Eq (Entitaet a) where
  (Entitaet id1 _) == (Entitaet id2 _) = id1 == id2

newtype Menge = Menge Double deriving (Eq, Show, Ord)

leereMenge = Menge 0

newtype ProduktName = ProduktName String
  deriving (Eq, Show, Ord)

type Bestellung = Entitaet (ProduktName, Menge)

type Katalog = Map ProduktName WaschProdukt

newtype Vorrat = Vorrat (Map Grundbestandteil Menge)
  deriving (Eq, Show, Monoid, Semigroup)

leererVorrat = Vorrat Map.empty


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



tensid = Tensid (Farbe "rot") (PH 5.5)
schuppenmittel = Pflegestoff (Farbe "gelb") Schuppen

derKatalog = Map.fromList [
    (ProduktName "Duschgel", Einfach tensid),
    (ProduktName "Schuppenshampoo", Gemisch 0.9 (Einfach tensid) 0.1 (Einfach schuppenmittel))
  ]

derVorrat = Vorrat (Map.fromList [
    (tensid, Menge 10), (schuppenmittel, Menge 1)
  ])

bestellungDuschgel = Entitaet (Id 1) (ProduktName "Duschgel", Menge 1)
bestellungShampoo = Entitaet (Id 2) (ProduktName "Schuppenshampoo", Menge 1)
bestellungShampooZuViel = Entitaet (Id 3) (ProduktName "Schuppenshampoo", Menge 20)
bestellungUnbekannt = Entitaet (Id 4) (ProduktName "Erdbeermilch", Menge 1)


