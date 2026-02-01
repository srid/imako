-- Pipeline configuration for Vira <https://vira.nixos.asia/>

\ctx pipeline ->
  let
    isMain = ctx.branch == "master"
  in
  pipeline
    { build.systems =
        [ "x86_64-linux"
        , "aarch64-darwin"
        ]
    , build.flakes =
        [ "."
        , "./nix/home-manager/example" { overrideInputs = [("imako", ".")] }
        ]
    , cache.url = if
        | isMain -> Just "https://cache.nixos.asia/oss"
        | otherwise -> Nothing
    , signoff.enable = True
    }