module Afstanden where

import Data.Vector

data AfstandenTabel = Afstand (Vector (Vector Float))

data LedigingsduurTabel = Ledigingsduur (Vector Float)

--Hieronder moet nog komen te staan hoe we deze tabel gaan inlezen.