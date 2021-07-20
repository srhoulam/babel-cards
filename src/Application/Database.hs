{-# LANGUAGE NoImplicitPrelude #-}
module Application.Database (module Application.Database) where

import qualified Database.Esqueleto        as E
import           Database.Persist          as Application.Database
-- import           Database.Persist.Class as Application.Database
import           Database.Persist.Sql      (SqlBackend, runSqlPool)
import           Model                     as Application.Database
import           RIO
import           RIO.List                  (headMaybe)
import           RIO.Time                  (UTCTime, getCurrentTime, NominalDiffTime)
import           Types
import           Types.Review

type BabelQuery a = ReaderT SqlBackend (ReaderT Babel IO) a

runDB :: BabelQuery a -> RIO Babel a
runDB action = RIO $ do
  connPool <- asks bConnPool
  runSqlPool action connPool

-- TODO: move constants to their own module
queueIdxNew, queueIdxLearning, queueIdxReview :: Int
queueIdxNew      = 0
queueIdxLearning = 1
queueIdxReview   = 2

-- * Babel database procedures

addNewCard :: DeckId -> Card -> [Text] -> BabelQuery CardId
addNewCard deckId card tagNames = do
  tagsToApply <- for tagNames retrieveOrCreateTag
  cardId <- insert card
  insert_ $ DeckMember deckId cardId 0.0
  for_ tagsToApply $ \tagId -> insert $ TagMember tagId cardId
  enqueueCardNew deckId cardId
  return cardId

enqueueCard :: Int -> UTCTime -> DeckId -> CardId -> BabelQuery ()
enqueueCard index dueDate deckId cardId = do
  now <- getCurrentTime
  insert_ $ QueueItem deckId index cardId dueDate now

enqueueCardNew
  :: DeckId -> CardId -> BabelQuery ()
enqueueCardNew deckId cardId = do
  now <- getCurrentTime
  enqueueCard queueIdxNew now deckId cardId
enqueueCardLearning, enqueueCardReview
  :: UTCTime -> DeckId -> CardId -> BabelQuery ()
enqueueCardLearning = enqueueCard queueIdxLearning
enqueueCardReview = enqueueCard queueIdxReview

logReview :: DeckId
          -> CardId
          -> Int
          -> NominalDiffTime
          -> ReviewEase
          -> (Entity QueueItem)
          -> BabelQuery ()
logReview deckId cardId distance duration ease (Entity _ qi) = do
  let durationSecs = round duration :: Int
      queueIdx = queueItemIndex qi
  now <- getCurrentTime
  insert_ $ ReviewLog deckId cardId distance durationSecs ease queueIdx now

retrieveCardEase :: DeckId -> CardId -> BabelQuery (Maybe Double)
retrieveCardEase deckId = fmap (fmap (deckMemberEase . entityVal))
  . getBy
  . UniqueDeckCard deckId

-- retrieveDeckMember :: DeckId -> CardId -> BabelQuery (Maybe (Entity DeckMember))
-- retrieveDeckMember deckId = getBy . UniqueDeckCard deckId

retrieveNextCard :: DeckId -> BabelQuery (Maybe (Entity Card))
retrieveNextCard deckId = do
  now <- getCurrentTime

  -- TODO: refactor out unnecessary retrievals
  mayLearningCard <- headMaybe <$> retrieveNextCardFromQueue queueIdxLearning now deckId
  mayNewCard <- headMaybe <$> retrieveNextCardFromQueue queueIdxNew now deckId
  mayReviewCard <- headMaybe <$> retrieveNextCardFromQueue queueIdxReview now deckId

  return $ msum [mayLearningCard, mayNewCard, mayReviewCard]

retrieveNextCardFromQueue :: Int
                     -> UTCTime
                     -> DeckId
                     -> BabelQuery [Entity Card]
retrieveNextCardFromQueue queueIndex now deckId =
  E.select $ E.from $ \(card `E.InnerJoin` qi) -> do
    E.on $ card E.^. CardId E.==. qi E.^. QueueItemCardId
    E.where_ $ card E.^. CardEnabled
      E.&&. qi E.^. QueueItemDeckId E.==. E.val deckId
      E.&&. qi E.^. QueueItemDue E.<. E.val now
      E.&&. qi E.^. QueueItemIndex E.==. E.val queueIndex
    E.orderBy [ E.asc (qi E.^. QueueItemDue) ]
    E.limit 1
    return card

retrieveOrCreateTag :: Text -> BabelQuery TagId
retrieveOrCreateTag tagName' = do
  mayTag <- getBy $ UniqueTag tagName'
  case mayTag of
    Just (Entity tid _) -> return tid
    Nothing             -> insert $ Tag tagName'

updateCardEase :: DeckId -> CardId -> ReviewEase -> BabelQuery ()
updateCardEase deckId cardId ReviewAgain = updateWhere
  [ DeckMemberDeckId ==. deckId
  , DeckMemberCardId ==. cardId
  ]
  [ DeckMemberEase =. 0.99 ]
updateCardEase deckId cardId ReviewHard = updateWhere
  [ DeckMemberDeckId ==. deckId
  , DeckMemberCardId ==. cardId
  ]
  [ DeckMemberEase *=. 0.85 ]
updateCardEase _ _ ReviewGood = return ()
updateCardEase deckId cardId ReviewEasy = updateWhere
  [ DeckMemberDeckId ==. deckId
  , DeckMemberCardId ==. cardId
  ]
  [ DeckMemberEase *=. 1.15 ]
