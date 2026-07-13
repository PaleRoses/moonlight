-- | CSV, markdown-table, and benchmark-card rendering.
module Moonlight.EGraph.Bench.Harness.Report
  ( Align (..),
    Column (..),
    Table,
    renderCsv,
    renderCsvRow,
    renderCardTable,
    formatMillis,
    Card (..),
    renderCard,
  ) where

import Data.List qualified as List
import Data.Word (Word64)
import Text.Printf (printf)

data Align
  = AlignLeft
  | AlignLeftMarked
  | AlignRight

data Column row = Column
  { columnHeader :: !String,
    columnAlign :: !Align,
    columnCell :: row -> String
  }

type Table row = [Column row]

renderCsv :: Table row -> [row] -> String
renderCsv table rows =
  unlines
    ( List.intercalate "," (fmap columnHeader table)
        : fmap (renderCsvRow table) rows
    )

renderCsvRow :: Table row -> row -> String
renderCsvRow table row =
  List.intercalate "," (fmap (\column -> columnCell column row) table)

renderCardTable :: Table row -> [row] -> [String]
renderCardTable table rows =
  headerLine : alignLine : fmap bodyLine rows
  where
    headerLine =
      "| " <> List.intercalate " | " (fmap columnHeader table) <> " |"
    alignLine =
      "|" <> concatMap (alignCell . columnAlign) table
    alignCell =
      \case
        AlignLeft -> "---|"
        AlignLeftMarked -> ":---|"
        AlignRight -> "---:|"
    bodyLine row =
      "| "
        <> List.intercalate " | " (fmap (\column -> columnCell column row) table)
        <> " |"

formatMillis :: Word64 -> String
formatMillis nanoseconds =
  printf "%.3f" (fromIntegral nanoseconds / (1000000 :: Double) :: Double)

data Card row summary = Card
  { cardVerdict :: [row] -> String,
    cardSummarize :: [row] -> [summary],
    cardTable :: Table summary,
    cardNotes :: [row] -> [String],
    cardMissing :: [row] -> String,
    cardNext :: [row] -> Maybe String
  }

renderCard :: String -> Card row summary -> [row] -> String
renderCard reproCommand card rows =
  unlines
    ( [ cardVerdict card rows,
        "",
        "## REPRO",
        "",
        reproCommand,
        "",
        "## TABLE",
        ""
      ]
        <> renderCardTable (cardTable card) (cardSummarize card rows)
        <> notesSection
        <> [ "",
             "## MISSING",
             "",
             cardMissing card rows
           ]
        <> nextSection
    )
  where
    notesSection =
      case cardNotes card rows of
        [] ->
          []
        notes ->
          ["", "## NOTES"] <> concatMap (\note -> ["", note]) notes
    nextSection =
      case cardNext card rows of
        Nothing ->
          []
        Just next ->
          ["", "## NEXT", "", next]
