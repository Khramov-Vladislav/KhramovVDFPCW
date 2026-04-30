module Core.Common
  ( AppError (..),
    renderAppError
  )
where

data AppError
  = FileNotFound FilePath
  | ParseError String
  | ValidationError String
  deriving (Eq, Show)

renderAppError :: AppError -> String
renderAppError err =
  case err of
    FileNotFound fp -> "File not found: " <> fp
    ParseError msg -> "Parse error: " <> msg
    ValidationError msg -> "Validation error: " <> msg
