import Dev.Prelude

import Dev.Project

main = project $ do
    require_package [ "base"
                    , "blaze-builder"
                    , "blaze-html"
                    , "bytestring"
                    , "containers"
                    , "directory"
                    , "filepath"
                    , "filestore"
                    , "http-types"
                    , "monads-tf"
                    , "pandoc"
                    , "aeson"
                    , "text"
                    , "time"
                    , "yesod"
                    , "yesod-core"
                    , "template-haskell"
                    ]
    ghc_option "-XTemplateHaskell"
    ghc_option "-XQuasiQuotes"
    ghc_option "-XTypeFamilies"
    ghc_option "-XMultiParamTypeClasses"
    ghc_option "-XFlexibleContexts"
    ghc_option "-XFlexibleInstances"
    ghc_option "-XNoMonomorphismRestriction"
    ghc_option "-XOverloadedStrings"
    ghc_option "-XRankNTypes"
    ghc_option "-XScopedTypeVariables"
    ghc_option "-XUndecidableInstances"
    ghc_option "-XGADTs"

    executable "corebot-bliki" "DefaultMain"

    library "corebot-bliki" [ "Yesod.CoreBot.Bliki" ]

    return ()

