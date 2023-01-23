
-- {-# LANGUAGE EmptyDataDecls             #-}
-- {-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE GADTs                      #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE QuasiQuotes                #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- 01/20/23
-- Database.hs

import SafeSync.Backend.Database.Models

import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

main = do
    print (Person 0 "rudy" 10)
