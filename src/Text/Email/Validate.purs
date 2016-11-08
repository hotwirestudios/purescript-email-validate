module Text.Email.Validate
    ( isValid
    , validate
    , emailAddress
    , canonicalizeEmail
    , runEmailParser
    , module Text.Email.Parser
    )
where

import Prelude
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Text.Email.Parser (EmailAddress(..), addrSpec, domainPart, localPart, toString)
import Text.Parsing.StringParser (ParseError(..), Pos, PosString, unParser)

-- | Smart constructor for an email address
emailAddress :: String -> Maybe EmailAddress
emailAddress emailString =
    case validate emailString of
         Left _ -> Nothing
         Right email -> Just email

-- | Checks that an email is valid and returns a version of it
--   where comments and whitespace have been removed.
canonicalizeEmail :: String -> Maybe String
canonicalizeEmail = map toString <<< emailAddress

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValid :: String -> Boolean
isValid emailString =
    case validate emailString of
         Left _ -> false
         Right _ -> true

-- | If you want to find out *why* a particular string is not
--   an email address, use this.
validate :: String -> Either String EmailAddress
validate = lmap show <<< runEmailParser

-- | Run a parser for an input string, returning either an error or a result.
runEmailParser :: String -> Either ParseError EmailAddress
runEmailParser s = handleResult $ unParser addrSpec { str: s, pos: 0 }
    where
        handleResult :: Either { pos :: Pos, error :: ParseError } { result :: EmailAddress, suffix :: PosString } -> Either ParseError EmailAddress
        handleResult (Left r) = Left r.error
        handleResult (Right r) = do
            if length r.suffix.str == r.suffix.pos
                then Right r.result
                else Left $ ParseError "leftover characters at end of email string"
