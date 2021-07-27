{-# LANGUAGE NoImplicitPrelude #-}

module Types.Free where

-- import           Application.Database  hiding (logReview)
-- import qualified Application.Database  as AD
-- import qualified Application.Scheduler as AS
-- import           Control.Monad.Free
-- import           RIO
-- import           RIO.Time              (NominalDiffTime)
-- import           Types
-- import           Types.Review


-- -- | Babel free monad for sandboxing Lua scripts.
-- data BabelF a =
--   BabelRetrieveAllCards DeckId ([Entity Card] -> a)
--   | BabelRetrieveNextCard DeckId (Maybe (Entity Card) -> a)
--   | BabelRescheduleCard DeckId CardId QueueIndex Bool ReviewEase a
--   | BabelLogReview DeckId CardId Int NominalDiffTime ReviewEase QueueIndex a

-- type BabelFree = Free BabelF

-- instance Functor BabelF where
--   fmap f (BabelRetrieveAllCards did next) =
--     BabelRetrieveAllCards did (f . next)
--   fmap f (BabelRetrieveNextCard did next) =
--     BabelRetrieveNextCard did (f . next)
--   fmap f (BabelRescheduleCard did cid qi crct re x) =
--     BabelRescheduleCard did cid qi crct re (f x)
--   fmap f (BabelLogReview did cid dist dur re qi x) =
--     BabelLogReview did cid dist dur re qi (f x)

-- runBabel :: BabelFree x -> BabelQuery x
-- runBabel (Pure x) = return x
-- runBabel (Free (BabelRetrieveAllCards deckId' next)) =
--   AD.retrieveDeckCards deckId' >>= (runBabel . next)
-- runBabel (Free (BabelRetrieveNextCard deckId' next)) =
--   AD.retrieveNextCard deckId' >>= (runBabel . next)
-- runBabel (Free (BabelRescheduleCard deckId' cardId' qi crct re x)) =
--   AS.rescheduleCard deckId' cardId' qi crct re >> runBabel x
-- runBabel (Free (BabelLogReview deckId' cardId' dist dur re qi x)) =
--   AD.logReview deckId' cardId' dist dur re qi >> runBabel x

-- getAllCards :: DeckId -> BabelFree [Entity Card]
-- getAllCards deckId' = liftF $ BabelRetrieveAllCards deckId' id

-- getNextCard :: DeckId -> BabelFree (Maybe (Entity Card))
-- getNextCard deckId' = liftF $ BabelRetrieveNextCard deckId' id

-- rescheduleCard :: DeckId -> CardId -> QueueIndex -> Bool -> ReviewEase -> BabelFree ()
-- rescheduleCard deckId' cardId' qi crct re =
--   liftF $ BabelRescheduleCard deckId' cardId' qi crct re ()

-- logReview :: DeckId -> CardId -> Int -> NominalDiffTime -> ReviewEase -> QueueIndex -> BabelFree ()
-- logReview deckId' cardId' dist dur re qi =
--   liftF $ BabelLogReview deckId' cardId' dist dur re qi ()

-- val :: x -> BabelFree x
-- val = Pure
