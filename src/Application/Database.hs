{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Application.Database (module Application.Database) where

import           Control.Monad.Trans.Maybe
import qualified Data.Text                 as Text (lines)
import qualified Database.Esqueleto        as E
import           Database.Persist          as Application.Database
import           Database.Persist.Sql      (SqlBackend, runSqlPool)
import Lens.Micro.Platform
import           Model                     as Application.Database
import           RIO
import           RIO.List                  (headMaybe, sort, (\\))
import           RIO.Time                  (NominalDiffTime, UTCTime,
                                            addUTCTime, getCurrentTime)
import           Types
import           Types.Review
import           Types.TUI

type BabelQuery a = ReaderT SqlBackend (ReaderT Babel IO) a

runDB :: BabelQuery a -> RIO Babel a
runDB action = RIO $ do
  connPool <- asks bConnPool
  runSqlPool action connPool

-- * Babel database procedures

assignCardDeck :: DeckId -> CardId -> BabelQuery ()
assignCardDeck deckId' cardId' = do
  _ <- runMaybeT
    $ void (MaybeT $ getBy $ UniqueDeckCard deckId' cardId')
    <|> lift (do
                 insert_ $ DeckMember deckId' cardId' 0.0
                 enqueueCardNew deckId' cardId')

  return ()

assignCardTag :: TagId -> CardId -> BabelQuery ()
assignCardTag tagId' cardId' = do
  _ <- runMaybeT
    $ void (MaybeT $ getBy $ UniqueTagCard tagId' cardId')
    <|> lift (insert_ $ TagMember tagId' cardId')

  return ()

unassignCardDeck :: DeckId -> CardId -> BabelQuery ()
unassignCardDeck deckId' cardId' = do
  for_ [minBound..maxBound] $ \qi -> runMaybeT $ do
    Entity qiid _ <- MaybeT $ getBy $ UniqueQueueCard deckId' qi cardId'
    lift $ delete qiid
  _ <- runMaybeT $ do
    Entity dmid _ <- MaybeT $ getBy $ UniqueDeckCard deckId' cardId'
    lift $ delete dmid
  return ()

unassignCardTag :: TagId -> CardId -> BabelQuery ()
unassignCardTag tagId' cardId' = do
  _ <- runMaybeT $ do
    Entity dmid _ <- MaybeT $ getBy $ UniqueTagCard tagId' cardId'
    lift $ delete dmid
  return ()

createCard :: NewCard -> BabelQuery CardId
createCard NewCard {..} = do
  newCardId <- insert $ Card newCardObverse newCardReverse True

  let tagNames = sort $ Text.lines newCardTags
  existingTags <- selectList [ TagName <-. tagNames] [ Asc TagName ]
  let nonExistingTags = tagNames \\ existingTags ^.. each . val . name
      existingTagIds = existingTags ^.. each . key
  newTagIds <- insertMany $ Tag <$> nonExistingTags
  _ <- insertMany $ flip TagMember newCardId <$> (existingTagIds <> newTagIds)

  return newCardId

enqueueCard :: QueueIndex -> UTCTime -> DeckId -> CardId -> BabelQuery ()
enqueueCard index dueDate deckId' cardId' = do
  now <- getCurrentTime
  insert_ $ QueueItem deckId' index cardId' dueDate now

enqueueCardNew
  :: DeckId -> CardId -> BabelQuery ()
enqueueCardNew deckId' cardId' = do
  now <- getCurrentTime
  let dueDate = addUTCTime 600 now
  enqueueCard NewQueue dueDate deckId' cardId'
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
logReview deckId' cardId' distance duration ease' (Entity _ qi) = do
  let durationSecs = round duration :: Int
      queueIdx = queueItemIndex qi
  now <- getCurrentTime
  insert_ $ ReviewLog deckId' cardId' distance durationSecs ease' queueIdx now

retrieveCardEase :: DeckId -> CardId -> BabelQuery (Maybe Double)
retrieveCardEase deckId' = fmap (fmap (deckMemberEase . entityVal))
  . getBy
  . UniqueDeckCard deckId'

retrieveCardDecks :: CardId -> BabelQuery [DeckId]
retrieveCardDecks cardId' = fmap (^. val . deckId)
  <$> selectList [ DeckMemberCardId ==. cardId' ] [ Asc DeckMemberDeckId ]

retrieveCardTags :: CardId -> BabelQuery [TagId]
retrieveCardTags cardId' = fmap (^. val . tagId)
  <$> selectList [ TagMemberCardId ==. cardId' ] [ Asc TagMemberTagId ]

retrieveCardsDisabled :: BabelQuery [Entity Card]
retrieveCardsDisabled =
  E.select $ E.from $ \(card `E.LeftOuterJoin` tm `E.LeftOuterJoin` dm) -> do
    E.on $ card E.^. CardId           E.==. dm   E.^. DeckMemberCardId
    E.on $ card E.^. CardId           E.==. tm   E.^. TagMemberCardId

    E.groupBy $ card E.^. CardId
    let numDecks = E.countDistinct $ dm E.^. DeckMemberId :: E.SqlExpr (E.Value Int)
        numTags  = E.countDistinct $ tm E.^. TagMemberId  :: E.SqlExpr (E.Value Int)
    E.where_ $ E.not_ $ card E.^. CardEnabled
    E.orderBy [ E.asc numDecks
              , E.asc numTags
              , E.asc $ card E.^. CardObverse
              ]
    return card

retrieveCardsEnabled :: BabelQuery [Entity Card]
retrieveCardsEnabled =
  E.select $ E.from $ \(card `E.LeftOuterJoin` tm `E.LeftOuterJoin` dm) -> do
    E.on $ card E.^. CardId           E.==. dm   E.^. DeckMemberCardId
    E.on $ card E.^. CardId           E.==. tm   E.^. TagMemberCardId

    E.groupBy $ card E.^. CardId
    let numDecks = E.countDistinct $ dm E.^. DeckMemberId :: E.SqlExpr (E.Value Int)
        numTags  = E.countDistinct $ tm E.^. TagMemberId  :: E.SqlExpr (E.Value Int)
    E.where_ $ card E.^. CardEnabled
    E.orderBy [ E.asc numDecks
              , E.asc numTags
              , E.asc $ card E.^. CardObverse
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
retrieveNextCard deckId' = do
  now <- getCurrentTime

  runMaybeT
    $ (MaybeT $ retrieveNextCardFromQueue LearningQueue now deckId')
    <|> (MaybeT $ retrieveNextCardFromQueue NewQueue now deckId')
    <|> (MaybeT $ retrieveNextCardFromQueue ReviewQueue now deckId')

retrieveNextCardFromQueue :: QueueIndex
                          -> UTCTime
                          -> DeckId
                          -> BabelQuery (Maybe (Entity Card))
retrieveNextCardFromQueue queueIndex now deckId' = do
  cards <- E.select $ E.from $ \(card `E.InnerJoin` qi) -> do
    E.on $ card E.^. CardId E.==. qi E.^. QueueItemCardId
    E.where_ $ card E.^. CardEnabled
      E.&&. qi E.^. QueueItemDeckId E.==. E.val deckId'
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
updateCardEase deckId' cardId' ReviewAgain = updateWhere
  [ DeckMemberDeckId ==. deckId'
  , DeckMemberCardId ==. cardId'
  ]
  [ DeckMemberEase =. 0.99 ]
updateCardEase deckId' cardId' ReviewHard = updateWhere
  [ DeckMemberDeckId ==. deckId'
  , DeckMemberCardId ==. cardId'
  ]
  [ DeckMemberEase *=. 0.85 ]
updateCardEase _ _ ReviewGood = return ()
updateCardEase deckId' cardId' ReviewEasy = updateWhere
  [ DeckMemberDeckId ==. deckId'
  , DeckMemberCardId ==. cardId'
  ]
  [ DeckMemberEase *=. 1.15 ]
