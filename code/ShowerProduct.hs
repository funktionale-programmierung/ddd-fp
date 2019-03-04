-- this or type signature for getProductAmount
-- {-# LANGUAGE FlexibleContexts #-}
import Data.Map as Map
import qualified Data.Map.Strict (Map)
import Control.Monad.Reader as Reader
import qualified Control.Monad.Reader (Reader)
import Control.Monad.State.Lazy as State
import qualified Control.Monad.State.Lazy (State)

newtype Color = Color String deriving (Eq, Show, Ord)
newtype PH = PH Double deriving (Eq, Show, Ord)

data HairType = Oily | Dry | Normal | Dandruff
  deriving (Eq, Show, Ord)

data SimpleShowerProduct =
    Soap Color PH
  | Shampoo Color HairType
  deriving (Eq, Show, Ord)
  
data ShowerProduct =
    Simple SimpleShowerProduct
  | Mixture Double ShowerProduct Double ShowerProduct
  deriving (Eq, Show, Ord)

soapProportion :: ShowerProduct -> Double
soapProportion (Simple (Soap _ _)) = 1.0
soapProportion (Simple (Shampoo _ _)) = 0.0
soapProportion (Mixture p1 sp1 p2 sp2) =
  p1 * (soapProportion sp1) + p2 * (soapProportion sp2)

decomposeShowerProduct :: ShowerProduct -> Map SimpleShowerProduct Double
decomposeShowerProduct (Simple s) = Map.singleton s 1.0
decomposeShowerProduct (Mixture p1 sp1 p2 sp2) =
  let d1 = fmap (\ p -> p*p1) (decomposeShowerProduct sp1)
      d2 = fmap (\ p -> p*p2) (decomposeShowerProduct sp2)
  in Map.unionWith (+) d1 d2

newtype Id = Id Int deriving Eq

data Entity state = Entity Id state

instance Eq (Entity a) where
  (Entity id1 _) == (Entity id2 _) = id1 == id2
  
newtype Amount = Amount Double deriving (Eq, Show, Ord)

zero = Amount 0

type Stock = Map SimpleShowerProduct Amount


-- figure out how much of a simple product is in explicit stock
productAmount :: Stock -> SimpleShowerProduct -> Amount
productAmount st ssp =
     case Map.lookup ssp st of
       Nothing -> zero
       Just a -> a

newtype ProductName = ProductName String
  deriving (Eq, Show, Ord)
type Order = Entity (ProductName, Amount)

type Catalog = Map ProductName ShowerProduct

type ProductComputation a = Reader Catalog a

-- figure out map of required amounts in an order
orderAmounts :: Order -> Catalog -> Map SimpleShowerProduct Amount
orderAmounts (Entity _ (n, Amount a)) cat =
  case Map.lookup n cat of
    Nothing -> Map.empty
    Just sp ->
      fmap (\ p -> Amount (a * p)) (decomposeShowerProduct sp)

-- figure out map of required amounts in an order, monadic
askOrderAmounts :: Order -> ProductComputation (Map SimpleShowerProduct Amount)
askOrderAmounts ord =
  do cat <- Reader.ask
     return (orderAmounts ord cat)

-- figure out whether a bunch of product amounts are available in explicit stock
areAmountsInStock :: Stock -> Map SimpleShowerProduct Amount -> Bool
areAmountsInStock st ams =
  Map.foldrWithKey (\ ssp am av -> av && (productAmount st ssp >= am)) True ams

-- figure out if amounts in an order are available in explicit stock
areOrderAmountsInStock :: Stock -> Order -> Catalog -> Bool
areOrderAmountsInStock st o cat =
  areAmountsInStock st (orderAmounts o cat)

-- check is stock is valid - i.e. no negative amounts
isStockValid :: Stock -> Bool
isStockValid st =
  Map.foldr (\ (Amount a) v  -> v && (a >= 0)) True st

-- remove one product's amount from the stock
stockRemoveAmount :: Stock -> SimpleShowerProduct -> Amount -> Stock
stockRemoveAmount st ssp (Amount a) =
  Map.alter (\am0 -> case am0 of
                 Nothing -> Just (Amount (- a))
                 Just (Amount a0) -> Just (Amount (a0 - a)))
    ssp st

-- remove a bunch of product amounts from the stock
stockRemoveAmounts :: Stock -> Map SimpleShowerProduct Amount -> Stock
stockRemoveAmounts st ams =
  Map.foldrWithKey (\ ssp am st -> stockRemoveAmount st ssp am) st ams

-- not sure this is the right thing wrt. DDD
type StockComputation a = State Stock a

-- figure out how much of a product is in implicit stock
getProductAmount :: SimpleShowerProduct -> StockComputation Amount
getProductAmount ssp =
  do stock <- State.get
     return (productAmount stock ssp)

data Event =
    OrderReceived Order
  | SimpleProductRemoved SimpleShowerProduct Amount
  | OrderShipped Order

-- process an order into events
processOrder :: Order -> ProductComputation [Event]
processOrder ord =
  do ams <- askOrderAmounts ord
     let evs = fmap (uncurry SimpleProductRemoved) (toList ams)
     return ([OrderReceived ord] ++ evs ++ [OrderShipped ord])

-- Repository / Monad for contextual information

-- Aggregate

-- commands: orders

-- process events

-- contextual information: amounts of soap, shampoo won't work -
-- different soaps and different shampoos => map?

-- Aggregate validation?

-- 
