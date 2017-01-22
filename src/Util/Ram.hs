module Util.Ram where

asyncRam0 :: ∀ n a wdom wgated rdom rgated .
             (KnownNat n) =>
             Clock wdom wgated -> 
             Clock rdom rgated ->
             a ->
             Signal rdom (Unsigned n) ->
             Signal wdom (Unsigned n, a) ->
             Signal rdom a
asyncRam0 wclk rclk a rx wx =
    let rx' = complement <$> rx
        wx' = (\ (k, a) -> (complement k, a) <$ guard (k /= 0)) <$> wx
    in (\ case 0 -> pure a
               _ -> id) <$> rx
                        <*> asyncRamPow2 wclk rclk rx' wx'
