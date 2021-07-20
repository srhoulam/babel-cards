{-# LANGUAGE NoImplicitPrelude #-}
module Application.Scheduler where

import Application.Database
import RIO
import RIO.Time (addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Types.Review

rescheduleCard :: DeckId
               -> CardId
               -> Entity QueueItem
               -> Bool
               -- ^ Correct answer?
               -> ReviewEase
               -> BabelQuery ()
rescheduleCard deckId cardId (Entity qiid qi) True reviewEase = do
  case (queueItemIndex qi, reviewEase) of
    -- NOTE: ease doesn't go down on new/learning cards
    (0, ReviewEasy) -> updateCardEase deckId cardId reviewEase
    (0, _) -> return ()
    (1, ReviewEasy) -> updateCardEase deckId cardId reviewEase
    (1, _) -> return ()
    (2, _) -> updateCardEase deckId cardId reviewEase
    (_, _) -> error "rescheduleCard: out-of-bounds queue index"

  newEase <- fromMaybe
    (error "rescheduleCard: DeckMember doesn't exist??")
    <$> retrieveCardEase deckId cardId
  now <- getCurrentTime
  let newQueueIndex =
        case (queueItemIndex qi, newEase < 1.0, newEase >= 2.0) of
          (0, _, _) -> 1
          (1, _, False) -> 1
          (1, _, True) -> 2
          (2, True, _) -> 1
          (2, _, _) -> 2
          (_, _, _) -> error "rescheduleCard: out-of-bounds queue index"
      oldInterval = diffUTCTime now (queueItemCreated qi)
      newInterval = realToFrac newEase * oldInterval
      newDueDate = addUTCTime newInterval now

  delete qiid
  enqueueCard newQueueIndex newDueDate deckId cardId

rescheduleCard deckId cardId (Entity qiid qi) False reviewEase = do
  case reviewEase of
    ReviewAgain -> updateCardEase deckId cardId reviewEase
    _ -> updateCardEase deckId cardId ReviewHard

  newEase <- fromMaybe
    (error "rescheduleCard: DeckMember doesn't exist??")
    <$> retrieveCardEase deckId cardId
  now <- getCurrentTime
  let newQueueIndex =
        case (queueItemIndex qi, newEase < 1.0, newEase >= 2.0) of
          (0, _, _) -> 1
          (1, _, False) -> 1
          (1, _, True) -> 2
          (2, True, _) -> 1
          (2, _, _) -> 2
          (_, _, _) -> error "rescheduleCard: out-of-bounds queue index"
      newDueDate = addUTCTime nominalDay now

  delete qiid
  enqueueCard newQueueIndex newDueDate deckId cardId
