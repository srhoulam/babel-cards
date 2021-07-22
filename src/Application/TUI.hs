{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Application.TUI where

import           Application.Database
import           Brick
import           Brick.BChan
import           Brick.Forms                (checkboxField, editTextField, (@@=),
                                             formState, handleFormEvent,
                                             newForm, renderForm)
import           Brick.Main
import           Brick.Widgets.Border       (border, borderWithLabel, hBorder,
                                             hBorderWithLabel)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (hCenter, vCenter)
import           Brick.Widgets.Dialog       (dialog)
import           Brick.Widgets.Edit         (Editor, editor, handleEditorEvent,
                                             renderEditor)
import           Brick.Widgets.List
                                            -- listSelectedElement, renderList)
import qualified Data.Sequence              as Seq (fromList, singleton)
import qualified Data.Text                  as Text (unpack)
import qualified Graphics.Vty               (defaultConfig, mkVty)
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Model
import           RIO
import           RIO.List                   (headMaybe)
import           RIO.Time
import           Types
import           Types.TUI

lifecycle :: RIO Babel ()
lifecycle = do
  env <- ask
  eventChan <- liftIO $ newBChan 10
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- liftIO $ buildVty
  _ <- liftIO
    $ customMain initialVty buildVty (Just eventChan) lifecycleApp
    $ initialState env eventChan
  return ()
  where lifecycleApp = App {..} :: App BabelTUI BabelEvent String
        appAttrMap _ = attrMap defAttr
          [ (listAttr, withStyle currentAttr dim)
          , (listSelectedAttr, withStyle currentAttr bold)
          , (listSelectedFocusedAttr, withStyle currentAttr $ bold + standout)
          ]
        appStartEvent st@BabelTUI {..} = do
          availDecks <- runRIO btBabel $ runDB $ retrieveDeckSummaries
          -- TODO: load modes, when lua scripting is implemented
          return st
            { btAvailableDecks =
                list "availableDecks" (Seq.fromList availDecks) 1
            , btAvailableModes =
                list "availableModes" (Seq.singleton Standard) 1
            }

        appChooseCursor = showFirstCursor

        appHandleEvent st@BabelTUI {..} evt = do
          case evt of
            AppEvent appEvent -> case appEvent of
              CreateDeck deck -> error "create deck"
                -- TODO: insert deck into db!
                -- TODO: add new deck to available decks
            _ -> return ()

          case btView of
            Start -> case evt of
              VtyEvent event -> do
                updatedList <- handleListEvent event btStartOptions
                let newState = st { btStartOptions = updatedList }
                case event of
                  EvKey (KChar 'q') [] -> halt newState
                  EvKey KEnter [] -> continue newState
                    { btView = maybe btView (fst . snd)
                      $ listSelectedElement updatedList
                    }
                  _ -> continue newState
              _ -> continue st

            DeckSelect -> case evt of
              _ -> continue st

            ModeSelect -> case evt of
              _ -> continue st

            AddNewDeck -> do
              updatedForm <- handleFormEvent evt btDeckForm
              case evt of
                VtyEvent event -> do
                  let newState = st { btDeckForm = updatedForm }
                  case event of
                    EvKey KEsc [] -> continue newState
                      { btView = DecksOverview }
                    EvKey (KChar 'd') [MCtrl] -> do
                      liftIO $ writeBChan btChan $ CreateDeck $ formState updatedForm
                      continue newState { btView = DeckManagement }
                    _ -> continue newState
                _ -> continue st

            DecksOverview -> case evt of
              VtyEvent event -> do
                updatedList <- handleListEvent event btAvailableDecks
                let newState = st { btAvailableDecks = updatedList }
                case event of
                  EvKey KEsc [] -> continue newState
                    { btView = Start
                    }
                  EvKey KEnter [] -> continue newState
                    { btView = DeckManagement
                    , btActiveDeck = dmDeckEntity . snd <$> listSelectedElement updatedList
                    }
                  EvKey (KChar 'a') [] -> continue newState
                    { btView = AddNewDeck
                    }
                  _ -> continue newState
              _ -> continue st

        appDraw BabelTUI {..} = catMaybes
          [ Just $ case btView of
              Start -> applicationTitle
                $ vBox
                [ hCenter $ strWrap "A flash-cards memorization tool."
                , hBorder
                , vCenter
                  $ vBox
                  [ renderList renderStartOption True btStartOptions
                  , hCenter (strWrap "Press ENTER to make a selection.")
                  , hCenter (strWrap "Press Q to quit.")
                  ]
                , hBorder
                , copyrightNotice
                ]
              DeckSelect ->
                error "deck select"
              ModeSelect ->
                error "mode select"
              Playing ->
                error "playing"

              AddNewCard ->
                error "AddNewCard"
              CardsOverview ->
                error "CardsOverview"
              CardManagement ->
                error "CardManagement"

              AddNewDeck -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Deck")
                , vCenter $ renderForm btDeckForm
                , hCenter $ strWrap "Press TAB to proceed to the next field."
                , hCenter $ strWrap "Press Shift+TAB to return to a previous field."
                , hCenter $ strWrap "Press Ctrl+D when finished."
                ]
              DecksOverview -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Decks")
                , vCenter
                  $ vBox
                  [ renderList renderDeckMenuOption True btAvailableDecks
                  , hCenter (strWrap "Press ENTER to make a selection.")
                  , hCenter (strWrap "Press A to add a new deck.")
                  , hCenter (strWrap "Press ESC to return.")
                  ]
                ]
              DeckManagement ->
                error "DeckManagement"
                -- TODO: deck form for editing the active deck's
                --       name and desc
                -- TODO: assign cards to this deck. sort unassigned
                --       cards first!

              Credits -> error "Credits"
          ]

        renderDeckMenuOption _ DeckMetadata {..} =
          let Entity _ deck = dmDeckEntity
          in hBox
             [ str (Text.unpack $ deckName deck)
             , str $ show dmCardCount
             , str $ maybe "Never" (formatTime defaultTimeLocale "%_Y-%m-%d %T") dmLastStudied
             ]
             <=> strWrap (Text.unpack $ deckDescription deck)

        renderStartOption _ (_, label) =
          hCenter $ strWrap label

        applicationTitle = borderWithLabel (str "BabelCards")

        initialState env chan = BabelTUI
          { btBabel = env
          , btView = Start
          , btChan = chan

          , btActiveCard = Nothing
          , btActiveDeck = Nothing

          , btAnswerForm = answerForm
          , btCardForm = cardForm
          , btDeckForm = deckForm

          , btAvailableDecks = list "availableDecks" mempty 1
          , btAvailableModes = list "availableModes" mempty 1
          , btStartOptions = list "startOptions"
              (Seq.fromList
               [ (DeckSelect, "Study a Deck")
               , (DecksOverview, "Decks")
               , (CardsOverview, "Cards")
               , (Credits, "About")
               ])
              1
          }

        answerForm = newForm
          [ editTextField id "userEntry" (Just 1) ]
          mempty

        cardForm = newForm
          [ (str "Obverse: " <+>)
            @@= editTextField obverse "cardObverse" (Just 1)
          , (str "Reverse: " <+>)
            @@= editTextField Model.reverse "cardReverse" (Just 1)
          -- , checkboxField enabled "cardEnabled" "Enabled"
          ]
          (Card "" "" True)

        deckForm = newForm
          [ (str "Name:        " <+>)
            @@= editTextField name "deckName" (Just 1)
          , (str "Description: " <+>)
            @@= editTextField description "deckDescription" Nothing
          ]
          (Deck "" "")

        copyrightNotice = vBox
          $ hCenter <$>
          [ strWrap "BabelCards v0.1.0"
          , strWrap "Copyright (c) 2021 Saad Rhoulam"
          , strWrap "BabelCards comes with ABSOLUTELY NO WARRANTY."
          , strWrap "BabelCards is distributed under the GPLv2 license."
          ]
