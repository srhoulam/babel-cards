{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Application.Database (module Application.Database) where

import Control.Monad.Trans.Maybe
import qualified Database.Esqueleto        as E
import           Database.Persist          as Application.Database
-- import           Database.Persist.Class as Application.Database
import           Database.Persist.Sql      (SqlBackend, runSqlPool)
-- import Lens.Micro
import           Model                     as Application.Database
import           RIO
import           RIO.List                  (headMaybe)
import           RIO.Time                  (UTCTime, getCurrentTime, NominalDiffTime, addUTCTime)
import           Types
import           Types.Review

type BabelQuery a = ReaderT SqlBackend (ReaderT Babel IO) a

runDB :: BabelQuery a -> RIO Babel a
runDB action = RIO $ do
  connPool <- asks bConnPool
  runSqlPool action connPool

-- * Babel database procedures

addNewCard :: DeckId -> Card -> [Text] -> BabelQuery CardId
addNewCard deckId card tagNames = do
  tagsToApply <- for tagNames retrieveOrCreateTag
  cardId <- insert card
  insert_ $ DeckMember deckId cardId 0.0
  for_ tagsToApply $ \tagId -> insert $ TagMember tagId cardId
  enqueueCardNew deckId cardId
  return cardId

enqueueCard :: QueueIndex -> UTCTime -> DeckId -> CardId -> BabelQuery ()
enqueueCard index dueDate deckId cardId = do
  now <- getCurrentTime
  insert_ $ QueueItem deckId index cardId dueDate now

enqueueCardNew
  :: DeckId -> CardId -> BabelQuery ()
enqueueCardNew deckId cardId = do
  now <- getCurrentTime
  let dueDate = addUTCTime 600 now
  enqueueCard NewQueue dueDate deckId cardId
enqueueCardLearning, enqueueCardReview
  :: UTCTime -> DeckId -> CardId -> BabelQuery ()
enqueueCardLearning = enqueueCard LearningQueue
enqueueCardReview = enqueueCard ReviewQueue

logReview :: DeckId
          -> CardId
          -> Int
          -> NominalDiffTime
          -> ReviewEase
          -> Entity QueueItem
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

retrieveCards :: BabelQuery [Entity Card]
retrieveCards =
  E.select $ E.from $ \(card `E.LeftOuterJoin` tm `E.LeftOuterJoin` dm) -> do
    E.on $ card E.^. CardId           E.==. dm   E.^. DeckMemberCardId
    E.on $ card E.^. CardId           E.==. tm   E.^. TagMemberCardId

    E.groupBy $ card E.^. CardId
    let numDecks = E.countDistinct $ dm E.^. DeckMemberId :: E.SqlExpr (E.Value Int)
        numTags  = E.countDistinct $ tm E.^. TagMemberId  :: E.SqlExpr (E.Value Int)
    E.orderBy [ E.asc numDecks
              , E.asc numTags
              , E.desc $ card E.^. CardEnabled
              ]
    return card

retrieveDeckSummaries :: BabelQuery [DeckMetadata]
retrieveDeckSummaries = do
  summaries <- E.select $ E.from $ \(deck `E.LeftOuterJoin` dm `E.LeftOuterJoin` rl) -> do
    E.on $ deck E.^. DeckId E.==. rl E.^. ReviewLogDeckId
    E.on $ deck E.^. DeckId E.==. dm E.^. DeckMemberDeckId
    E.groupBy $ deck E.^. DeckId
    let latestActivity = E.max_ $ rl E.^. ReviewLogTimestamp
    E.orderBy [ E.desc latestActivity ]
    return ( deck
           , latestActivity
           , E.countDistinct $ dm E.^. DeckMemberId :: E.SqlExpr (E.Value Int)
           )

  return $ mkDeckMetadata <$> summaries
  where mkDeckMetadata (deckMetadataDeckEntity, E.Value deckMetadataLastStudied, E.Value deckMetadataCardCount) =
          DeckMetadata {..}

retrieveNextCard :: DeckId -> BabelQuery (Maybe (Entity Card))
retrieveNextCard deckId = do
  now <- getCurrentTime

  runMaybeT
    $ (MaybeT $ retrieveNextCardFromQueue LearningQueue now deckId)
    <|> (MaybeT $ retrieveNextCardFromQueue NewQueue now deckId)
    <|> (MaybeT $ retrieveNextCardFromQueue ReviewQueue now deckId)

retrieveNextCardFromQueue :: QueueIndex
                          -> UTCTime
                          -> DeckId
                          -> BabelQuery (Maybe (Entity Card))
retrieveNextCardFromQueue queueIndex now deckId = do
  cards <- E.select $ E.from $ \(card `E.InnerJoin` qi) -> do
    E.on $ card E.^. CardId E.==. qi E.^. QueueItemCardId
    E.where_ $ card E.^. CardEnabled
      E.&&. qi E.^. QueueItemDeckId E.==. E.val deckId
      E.&&. qi E.^. QueueItemDue E.<. E.val now
      E.&&. qi E.^. QueueItemIndex E.==. E.val queueIndex
    E.orderBy [ E.asc (qi E.^. QueueItemDue) ]
    E.limit 1
    return card

  return $ headMaybe cards

retrieveOrCreateTag :: Text -> BabelQuery TagId
retrieveOrCreateTag tagName' = do
  mayTag <- getBy $ UniqueTag tagName'
  case mayTag of
    Just (Entity tid _) -> return tid
    Nothing             -> insert $ Tag tagName'

retrieveTags :: BabelQuery [Entity Tag]
retrieveTags = selectList [] [ Asc TagName ]

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
