hs-server-starter
=============

## Description

Provides a utility to write server program which can be called via Perl's
[https://github.com/kazuho/p5-Server-Starter](start_server) program using
Haskell.

This module does not provide a Haskell implementation of `start_server`,
so you need to use the original Perl version or use
[a version ported to golang](https://github.com/lestrrat/go-server-starter).

## SINOPSIS

Since the `listenAll` function returns a listened `Network.Socket`, please call
`accept` on it.

```haskell
import qualified Network.ServerStarter.Socket as Starter
import qualified Network.Socket               as Socket
import qualified Network.Wai                  as Wai
import qualified Network.Wai.Handler.Warp     as Warp

main :: IO ()
main = do
  (socket:_) <- Starter.listenAll
  Socket.SockAddrInet port _ <- Socket.getSocketName socket
  let setting = Warp.setPort (fromEnum port)
              $ Warp.defaultSettings
  Warp.runSettingsSocket setting socket app

app :: Wai.Application
app = ...
```

Then run `start_server` and access to `http://localhost:12345` .

```
$ start_server --port 12345 -- stack exec server-starter-warp-example
```

## Author

Masahiro Honma (<hiratara@cpan.org>)
