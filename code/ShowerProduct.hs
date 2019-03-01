newtype Color = Color String deriving Eq
newtype PH = PH Double deriving Eq

data HairType = Oily | Dry | Normal | Dandruff
  deriving Eq

data ShowerProduct =
    Soap Color PH
  | Shampoo Color HairType
  | Mixture Double ShowerProduct Double ShowerProduct
  deriving Eq

soapProportion :: ShowerProduct -> Double
soapProportion (Soap _ _)    = 1.0
soapProportion (Shampoo _ _) = 0.0
soapProportion (Mixture p1 sp1 p2 sp2) =
  p1 * (soapProportion sp1) + p2 * (soapProportion sp2)

newtype Id = Id Int deriving Eq

data Entity state = Entity Id state

instance Eq (Entity a) where
  (Entity id1 _) == (Entity id2 _) = id1 == id2
  
newtype Amount = Amount Double

type Stash = Entity (ShowerProduct, Amount)

