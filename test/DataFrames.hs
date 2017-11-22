module DataFrames (hwDF
                  , hwDF_pos
                  , hwDF_neg
                  , hwDF_so) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Model.DataSet (DataFrame
                     , Rows
                     , getAStar
                     , getDStar
                     , stripColumn
                     , showDF
                     )


hwrs :: Rows
hwrs = Map.fromList [ (0, (["big",    "yellow", "soft", "low"], "positive"))
                    , (1, (["big",    "yellow", "hard", "high"], "negative"))
                    , (2, (["medium", "yellow", "soft", "high"], "positive"))
                    , (3, (["medium", "blue", "hard", "high"], "positive"))
                    , (4, (["medium", "blue", "hard", "high"], "positive"))
                    , (5, (["medium", "blue", "soft", "low"], "negative"))
                    , (6, (["big",    "blue", "hard", "low"], "so-so"))
                    , (7, (["big",    "blue", "hard", "high"], "so-so"))
                    ]

hwDF :: DataFrame
hwDF = (["Size", "Color", "Feel", "Temperature"], "Attitude", hwrs)


hwrs_pos :: Rows
hwrs_pos = Map.fromList [ (0, (["big",    "yellow", "soft", "low"], "positive"))
                        , (1, (["big",    "yellow", "hard", "high"], "SP"))
                        , (2, (["medium", "yellow", "soft", "high"], "positive"))
                        , (3, (["medium", "blue", "hard", "high"], "positive"))
                        , (4, (["medium", "blue", "hard", "high"], "positive"))
                        , (5, (["medium", "blue", "soft", "low"], "SP"))
                        , (6, (["big",    "blue", "hard", "low"], "SP"))
                        , (7, (["big",    "blue", "hard", "high"], "SP"))
                        ]

hwDF_pos :: DataFrame
hwDF_pos = (["Size", "Color", "Feel", "Temperature"], "Attitude", hwrs_pos)


hwrs_neg :: Rows
hwrs_neg = Map.fromList [ (0, (["big",    "yellow", "soft", "low"], "SP"))
                        , (1, (["big",    "yellow", "hard", "high"], "negative"))
                        , (2, (["medium", "yellow", "soft", "high"], "SP"))
                        , (3, (["medium", "blue", "hard", "high"], "SP"))
                        , (4, (["medium", "blue", "hard", "high"], "SP"))
                        , (5, (["medium", "blue", "soft", "low"], "negative"))
                        , (6, (["big",    "blue", "hard", "low"], "SP"))
                        , (7, (["big",    "blue", "hard", "high"], "SP"))
                        ]

hwDF_neg :: DataFrame
hwDF_neg = (["Size", "Color", "Feel", "Temperature"], "Attitude", hwrs_neg)


hwrs_so :: Rows
hwrs_so = Map.fromList [ (0, (["big",    "yellow", "soft", "low"], "SP"))
                        , (1, (["big",    "yellow", "hard", "high"], "SP"))
                        , (2, (["medium", "yellow", "soft", "high"], "SP"))
                        , (3, (["medium", "blue", "hard", "high"], "SP"))
                        , (4, (["medium", "blue", "hard", "high"], "SP"))
                        , (5, (["medium", "blue", "soft", "low"], "SP"))
                        , (6, (["big",    "blue", "hard", "low"], "so-so"))
                        , (7, (["big",    "blue", "hard", "high"], "so-so"))
                        ]

hwDF_so :: DataFrame
hwDF_so = (["Size", "Color", "Feel", "Temperature"], "Attitude", hwrs_so)
