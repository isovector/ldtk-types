{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

module LDtk.Types where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Maybe (listToMaybe)
import Numeric (readHex)
import Data.Aeson.Types (Parser)

ldtkOpts :: Options
ldtkOpts = defaultOptions
  { fieldLabelModifier = \case
      -- Names that are too attrocious to allow
      "data'"     -> "data"
      "enumid"    -> "id"
      "tile_flip" -> "f"
      x           -> x
  , allNullaryToStringTag = True
  , unwrapUnaryRecords = True
  }

data Color = Color
  { c_r :: Word8
  , c_g :: Word8
  , c_b :: Word8
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

instance FromJSON Color where
  parseJSON v = do
    ('#' : r1 : r2 : g1 : g2 : b1 : b2 : []) <- parseJSON v
    let safe_read = pure . listToMaybe . fmap fst
    Just r <- safe_read $ readHex [r1, r2]
    Just g <- safe_read $ readHex [g1, g2]
    Just b <- safe_read $ readHex [b1, b2]
    pure $ Color r g b


data EntityDef = EntityDef
  { color :: Color
  , height :: Int
  , identifier :: Text
  , nineSliceBorders :: [Int]
  , pivotX :: Float
  , pivotY :: Float
  , tileRect :: Maybe TilesetRect
  , tileRenderMode :: TileRenderMode
  , tilesetId :: Maybe Int
  , uid :: Int
  , width :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data EmbedAtlas = LdtkIcons
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance FromJSON EmbedAtlas where
  parseJSON v = do
    "LdtkIcons" <- parseJSON @String v
    pure LdtkIcons

data CustomData = CustomData
  { data' :: Text
  , tileId :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data EnumTag = EnumTag
  { enumValueId :: Text
  , tileIds :: [Int]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data TilesetDef = TilesetDef
  { __cHei :: Int
  , __cWid :: Int
  , customData :: [CustomData]
  , embedAtlas :: Maybe EmbedAtlas
  , enumTags :: [EnumTag]
  , identifier :: Text
  , padding :: Int
  , pxHei :: Int
  , pxWid :: Int
  , relPath :: Maybe FilePath
  , spacing :: Int
  , tags :: [Text]
  , tagsSourceEnumUid :: Maybe Int
  , tileGridSize :: Int
  , uid :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Definitions = Definitions
  { entities :: [EntityDef]
  , enums :: [EnumDef]
  , externalEnums :: [EnumDef]
  , layers :: [LayerDef]
  -- , LevelFields :: [FieldDef]
  , tilesets :: [TilesetDef]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data EnumValueDef = EnumValueDef
  { __tileSrcRect :: Rect Int
  , color :: Int
  , enumid :: Text
  , tileId :: Maybe Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data EnumDef = EnumDef
  { externalRelPath :: Maybe FilePath
  , iconTilesetUid :: Maybe Int
  , identifier :: Text
  , tags :: [Text]
  , uid :: Int
  , values :: [EnumValueDef]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data GridValue = GridValue
  { color :: Color
  , identifier :: Maybe Text
  , value :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data LayerDef = LayerDef
  { __type :: LayerType
  , autoSourceLayerDefUid :: Maybe Int
  , displayOpacity :: Float
  , gridSize :: Int
  , identifier :: Text
  , intGridValues :: [GridValue]
  , parallaxFactorX :: Float
  , parallaxFactorY :: Float
  , parallaxScaling :: Bool
  , pxOffsetX :: Int
  , pxOffsetY :: Int
  , tilesetDefUid :: Maybe Int
  , uid :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data LDtkRoot = LDtkRoot
  { bgColor :: Color
  , defs :: Definitions
  , externalLevels :: Bool
  , iid :: Text
  , jsonVersion :: Text
  , levels :: [Level]
  , worldGridHeight :: Maybe Int
  , worldGridWidth :: Maybe Int
  , worldLayout :: WorldLayout
  , worlds :: [World]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data TileRenderMode = Cover | FitInside | Repeat | Stretch | FullSizeCropped | FillSizeUncropped | NineSlice
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (FromJSON)

data WorldLayout = Free | GridVania | LinearHorizontal | LinearVertical
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (FromJSON)

data LayerType = IntGrid | Entities | Tiles | AutoLayer
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (FromJSON)

data World = World
  { identifier :: Text
  , iid :: Text
  , levels :: [Level]
  , worldGridHeight :: Maybe Int
  , worldGridWidth :: Maybe Int
  , worldLayout :: WorldLayout
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Rect a = Rect
  { r_x :: a
  , r_y :: a
  , r_width :: a
  , r_height :: a
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data BgPos = BgPos
  { cropRect :: Rect Float
  , scale :: Pair Float
  , topLeftPx :: Pair Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Direction = North | South | East | West
  deriving stock (Eq, Ord, Show, Read, Generic)

instance FromJSON Direction where
  parseJSON v = do
    parseJSON @String v >>= \case
      "n" -> pure North
      "s" -> pure South
      "e" -> pure East
      "w" -> pure West
      x -> fail $ "not a direction: " <> show x

data Neighbour = Neighbour
  { dir :: Direction
  , levelIid :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Level = Level
  { __bgColor :: Color
  , __bgPos :: Maybe BgPos
  , __neighbours :: [Neighbour]
  , bgRelPath :: Maybe Text
  , externalRelPath :: Maybe Text
  , fieldInstances :: [Field]
  , identifier :: Text
  , iid :: Text
  , layerInstances :: [Layer]
  , pxHei :: Int
  , pxWid :: Int
  , uid :: Int
  , worldDepth :: Int
  , worldX :: Int
  , worldY :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Layer = Layer
  { __cHei :: Int
  , __cWid :: Int
  , __gridSize :: Int
  , __identifier :: Text
  , __opacity :: Float
  , __pxTotalOffsetX :: Int
  , __pxTotalOffsetY :: Int
  , __tilesetDefUid :: Maybe Int
  , __tilesetRelPath:: Maybe Text
  , __type :: LayerType
  , autoLayerTiles :: [Tile]
  , entityInstances :: [Entity]
  , gridTiles :: [Tile]
  , iid :: Text
  , intGridCsv :: [Int]
  , layerDefUid :: Maybe Int
  , tilesetRelPath :: Maybe Text
  , levelId :: Int
  , overrideTilesetUid :: Maybe Int
  , visible :: Bool
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data GridPoint = GridPoint
  { cx :: Int
  , cy :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Pair a = Pair
  { p_x :: a
  , p_y :: a
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

instance FromJSON a => FromJSON (Pair a) where
  parseJSON v = do
    [x, y] <- parseJSON v
    pure $ Pair x y

instance FromJSON a => FromJSON (Rect a) where
  parseJSON v = do
    [x, y, w, h] <- parseJSON v
    pure $ Rect x y w h


data Flip = NoFlip | FlipX | FlipY | FlipXY
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance FromJSON Flip where
  parseJSON = fmap toEnum . parseJSON

data EntityReferenceInfos = EntityReferenceInfos
  { entityIid :: Text
  , layerIid :: Text
  , levelIid :: Text
  , worldIid :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data FieldValue
  = IntegerValue Integer
  | FloatValue Float
  | BooleanValue Bool
  | StringValue Text
  | FilePathValue FilePath
  | ColorValue Color
  | EnumValue Text
  | PointValue GridPoint
  | TileValue TilesetRect
  | EntityRefValue EntityReferenceInfos
  | ArrayValue [FieldValue]
  deriving stock (Eq, Ord, Show, Read, Generic)

data TilesetRect = TilesetRect
  { h :: Int
  , tilesetUid :: Int
  , w :: Int
  , x :: Int
  , y :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Tile = Tile
  { tile_flip :: Flip
  , px :: Pair Int
  , src :: Pair Int
  , t :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Entity = Entity
  { __grid :: Pair Int
  , __identifier :: Text
  , __pivot :: Pair Float
  , __smartColor :: Color
  , __tags :: [Text]
  , __tile :: Maybe TilesetRect
  , defUid :: Int
  , fieldInstances :: [Field]
  , height :: Int
  , iid :: Text
  , px :: Pair Int
  , width :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Field = Field
  { __identifier :: Text
  , __tile :: Maybe TilesetRect
  , __type :: Text
  , __value :: FieldValue
  , defUid :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

parseFieldValue :: Text -> Value -> Parser FieldValue
parseFieldValue ty v = do
  let (super, T.dropEnd 1 -> T.drop 1 -> sub) = T.break (\c -> c == '(' || c == '<' || c == '.') ty
  case super of
    "Int"    -> IntegerValue   <$> parseJSON v
    "Integer"    -> IntegerValue   <$> parseJSON v
    "Float"      -> FloatValue     <$> parseJSON v
    "Bool"    -> BooleanValue   <$> parseJSON v
    "Boolean"    -> BooleanValue   <$> parseJSON v
    "String"     -> StringValue    <$> parseJSON v
    "Multilines" -> StringValue    <$> parseJSON v
    "Text"       -> StringValue    <$> parseJSON v
    "FilePath"   -> FilePathValue  <$> parseJSON v
    "Color"      -> ColorValue     <$> parseJSON v
    "Enum"       -> EnumValue      <$> parseJSON v
    "LocalEnum"       -> EnumValue      <$> parseJSON v
    "Point"      -> PointValue     <$> parseJSON v
    "Tile"       -> TileValue      <$> parseJSON v
    "EntityRef"  -> EntityRefValue <$> parseJSON v
    "Array"      -> do
      arr <- parseJSON v
      ArrayValue <$> traverse (parseFieldValue sub) arr
    x -> fail $ "unknown type " <> show x

instance FromJSON Field where
  parseJSON = withObject "Field" $ \obj -> do
    __identifier <- obj .: "__identifier"
    __tile <- obj .: "__tile"
    __type <- obj .: "__type"
    defUid <- obj .: "defUid"
    mv <- obj .: "__value"
    __value <-
      case mv of
        Just v -> parseFieldValue __type v
        Nothing -> pure $ ArrayValue []
    pure $ Field {..}

instance FromJSON LDtkRoot where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON BgPos where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON Neighbour where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON GridPoint where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON EntityReferenceInfos where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON TilesetRect where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON Tile where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON Entity where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON Layer where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON Level where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON World where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON Definitions where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON EntityDef where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON EnumDef where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON EnumValueDef where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON LayerDef where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON GridValue where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON TilesetDef where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON CustomData where
  parseJSON = genericParseJSON ldtkOpts

instance FromJSON EnumTag where
  parseJSON = genericParseJSON ldtkOpts


test :: World
test = World "ok" "hi" [] (Just 5) (Just 10) Free
