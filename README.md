# haskell-fio

# [![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/sorki/haskell-fio/ci.yaml?branch=master)](https://github.com/sorki/haskell-fio/actions/workflows/ci.yaml)

Fio bank API.

* [API docs](https://www.fio.cz/docs/cz/API_Bankovnictvi.pdf)

Payment parser based on [rememberportal](https://github.com/base48/rememberportal) (WTFPL).

## Usage

Generate read-only token in your account administration, write the token
to a file, provide to application via `FIO_TOKEN_PATH` environment variable:

```haskell
import System.Environment
import qualified API.Fio

main :: IO ()
main = do
  tokPath <- getEnv "FIO_TOKEN_PATH"
  tok <- tokenFromFile tokPath
  lastTransactionsStatement <- API.Fio.getLast tok
  print $ lastTransctionsStatement
```

## Demo example

Sample [application](./app/Main.hs) for testing functionallity is provided:

```
cabal configure -fexample
FIO_TOKEN_PATH=~/.fio.token cabal run fio-exe
```
