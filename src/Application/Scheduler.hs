{-# LANGUAGE NoImplicitPrelude #-}
module Application.Scheduler where

import           Application.Database
import           RIO
import           RIO.Time             (addUTCTime, diffUTCTime, getCurrentTime,
                                       nominalDay)
import           System.Random
import           Types
import           Types.Review

rescheduleCard :: DeckId
               -> CardId
               -> Bool
               -- ^ Correct answer?
               -> ReviewEase
               -> BabelQuery ()
rescheduleCard deckId' cardId' True reviewEase = do
  Entity qiid qi <- fromMaybe
    (error "rescheduleCard: QueueItem doesn't exist??")
    <$> getBy (UniqueQueueCard deckId' cardId')

  let qidx = queueItemIndex qi
  case (qidx, reviewEase) of
    -- NOTE: ease doesn't go down on new/learning cards
    (NewQueue, ReviewEasy)      -> updateCardEase deckId' cardId' reviewEase
    (NewQueue, _)               -> return ()
    (LearningQueue, ReviewEasy) -> updateCardEase deckId' cardId' reviewEase
    (LearningQueue, _)          -> return ()
    (ReviewQueue, _)            -> updateCardEase deckId' cardId' reviewEase

  newEase <- fromMaybe
    (error "rescheduleCard: DeckMember doesn't exist??")
    <$> retrieveCardEase deckId' cardId'
  env <- lift ask
  fuzzFactor <- runRIO env generateFuzz
  now <- getCurrentTime

  let newQueueIndex =
        case (qidx, newEase < 1.0, newEase >= 2.0) of
          (NewQueue, _, _)          -> LearningQueue
          (LearningQueue, _, False) -> LearningQueue
          (LearningQueue, _, True)  -> ReviewQueue
          (ReviewQueue, True, _)    -> LearningQueue
          (ReviewQueue, _, _)       -> ReviewQueue
      oldInterval = diffUTCTime now (queueItemCreated qi)
      maxInterval = bcMaxInterval $ bConfig env
      minInterval = bcMinInterval $ bConfig env
      newInterval = max minInterval
        $ min maxInterval
        $ product
        [ realToFrac (fuzzFactor + 1.0)
        , realToFrac newEase
        , oldInterval
        ]
      newDueDate = addUTCTime newInterval now

  delete qiid
  enqueueCard newQueueIndex newDueDate deckId' cardId'

rescheduleCard deckId' cardId' False reviewEase = do
  Entity qiid qi <- fromMaybe
    (error "rescheduleCard: QueueItem doesn't exist??")
    <$> getBy (UniqueQueueCard deckId' cardId')

  case (queueItemIndex qi, reviewEase) of
    -- NOTE: ease doesn't go down on new/learning cards
    (NewQueue, _)              -> return ()
    (LearningQueue, _)         -> return ()
    (ReviewQueue, ReviewAgain) -> updateCardEase deckId' cardId' reviewEase
    (ReviewQueue, _)           -> updateCardEase deckId' cardId' ReviewHard

  newEase <- fromMaybe
    (error "rescheduleCard: DeckMember doesn't exist??")
    <$> retrieveCardEase deckId' cardId'
  env <- lift ask
  fuzzFactor <- runRIO env generateFuzz
  now <- getCurrentTime
  let newQueueIndex =
        case (queueItemIndex qi, newEase < 1.0, newEase >= 2.0) of
          (NewQueue, _, _)          -> LearningQueue
          (LearningQueue, _, False) -> LearningQueue
          (LearningQueue, _, True)  -> ReviewQueue
          (ReviewQueue, True, _)    -> LearningQueue
          (ReviewQueue, _, _)       -> ReviewQueue
      oldInterval = diffUTCTime now (queueItemCreated qi)
      maxInterval = bcMaxInterval $ bConfig env
      minInterval = bcMinInterval $ bConfig env
      newInterval = max minInterval
        $ min maxInterval
        $ product
        [ realToFrac (fuzzFactor + 1.0)
        , realToFrac newEase
        , oldInterval
        ]
      newDueDate = addUTCTime (min newInterval nominalDay) now

  delete qiid
  enqueueCard newQueueIndex newDueDate deckId' cardId'

generateFuzz :: RIO Babel Double
generateFuzz = do
  rngTV <- asks bRNG
  fuzzInt <- atomically $ do
    rng <- readTVar rngTV
    let (fuzz, newRNG) = random rng
    writeTVar rngTV newRNG
    return fuzz

  let fuzzFactor = 0.05
        * fromIntegral (fuzzInt :: Int)
        / fromIntegral (maxBound :: Int)

  return fuzzFactor
