{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Application.TUI where

import           Application.Database
import           Brick
import           Brick.Forms                (editTextField, newForm)
import           Brick.Main
import           Brick.Widgets.Border       (border, borderWithLabel, hBorder,
                                             hBorderWithLabel)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (hCenter, vCenter)
import           Brick.Widgets.Dialog       (dialog)
import           Brick.Widgets.Edit         (Editor, editor, handleEditorEvent,
                                             renderEditor)
import           Brick.Widgets.List         (GenericList, handleListEvent, list,
                                             listSelectedElement, renderList)
import qualified Data.Sequence              as Seq (fromList, singleton)
import           Graphics.Vty.Attributes    (defAttr)
import           Graphics.Vty.Input.Events
import           RIO
import           RIO.List                   (headMaybe)
import           Types
import           Types.TUI

lifecycle :: RIO Babel ()
lifecycle = do
  env <- ask
  _ <- liftIO $ defaultMain lifecycleApp $ initialState env
  return ()
  where lifecycleApp = App {..} :: App BabelTUI BabelEvent String
        appAttrMap _ = attrMap defAttr []
        appStartEvent st@BabelTUI {..} = do
          availDecks <- runRIO btBabel $ runDB $ retrieveDeckSummaries
          -- TODO: load modes, when lua scripting is implemented
          return st
            { btAvailableDecks =
                list "availableDecks" (Seq.fromList availDecks) 1
            , btAvailableModes =
                list "availableModes" (Seq.singleton Standard) 1
            }

        appChooseCursor BabelTUI {..} cursors = headMaybe cursors
          -- TODO: more sophisticated cursor choice?

        appHandleEvent st@BabelTUI {..} evt =
          case btView of
            Start -> case evt of
              VtyEvent (EvKey (KChar 'q') []) -> halt st
              VtyEvent (EvKey KEsc []) -> halt st
              VtyEvent (EvKey _ _) -> continue st { btView = DeckSelect }
              _ -> continue st

            DeckSelect -> case evt of
              _ -> continue st

            ModeSelect -> case evt of
              _ -> continue st

            _ -> case evt of
              AppEvent NoOp -> continue st
              _             -> error "define handle event"

        appDraw BabelTUI {..} = catMaybes
          [ Just $ case btView of
              Start          -> borderWithLabel (str "BabelCards")
                $ vBox
                [ hCenter $ strWrap "A flash-cards memorization tool."
                , hBorder
                , vCenter
                  (hCenter (strWrap "Press ESC or Q to exit.")
                    <=> hCenter (strWrap "Press any other key to continue."))
                , hBorder
                , copyrightNotice
                ]
              DeckSelect     ->
                error "deck select"
              ModeSelect     ->
                error "mode select"
              Playing        ->
                error "playing"

              AddNewCard     ->
                error "AddNewCard"
              CardsOverview  ->
                error "CardsOverview"
              CardManagement ->
                error "CardManagement"

              AddNewDeck     ->
                error "AddNewDeck"
              DecksOverview  ->
                error "DecksOverview"
              DeckManagement ->
                error "DeckManagement"
          ]

        initialState env = BabelTUI
          { btBabel = env
          , btView = Start
          , btActiveCard = Nothing
          , btActiveDeck = Nothing
          , btAnswerForm = answerForm
          , btAvailableDecks =
              list "availableDecks" mempty 1
          , btAvailableModes =
              list "availableModes" mempty 1
          }

        answerForm = newForm
          [ editTextField id "userEntry" (Just 1) ]
          mempty

        copyrightNotice = vBox
          $ hCenter <$>
          [ strWrap "BabelCards v0.1.0"
          , strWrap "Copyright (c) 2021 Saad Rhoulam"
          , strWrap "BabelCards comes with ABSOLUTELY NO WARRANTY."
          , strWrap "BabelCards is distributed under the GPLv2 license."
          ]
