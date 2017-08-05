module Regl where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import DOM.HTML.Types (HTMLCanvasElement, HTMLElement)
import Data.ArrayBuffer.Types (ArrayView)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class Encode, encode)
import Data.Foreign.Generic (genericEncode, defaultOptions)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

foreign import data REGL :: Effect


foreign import data Context :: Type
foreign import data WebGLContext :: Type

instance encodeWebGLContext :: Encode WebGLContext where
  encode = toForeign


newtype Canvas = Canvas HTMLCanvasElement

instance encodeCanvas :: Encode Canvas where
  encode = toForeign

newtype Element = Element HTMLElement

instance encodeElement :: Encode Element where
  encode = toForeign


data WebGLPowerPreference
  = PowerDefault 
  | PowerLow
  | PowerHigh

encOptions :: Options
encOptions = defaultOptions { unwrapSingleConstructors = true }

instance encodeWebGLPowerPreference :: Encode WebGLPowerPreference where
  encode = encode <<< f where
    f PowerDefault = "default"
    f PowerLow = "low-power"
    f PowerHigh = "high-performance"
  
newtype WebGLContextAttributes =
  WebGLContextAttributes
  { alpha :: Boolean
  , depth :: Boolean
  , stencil :: Boolean
  , antialias :: Boolean
  , premultipliedAlpha :: Boolean
  , preserveDrawingBuffer :: Boolean
  , powerPreference :: WebGLPowerPreference
  , failIfMajorPerformanceCaveat :: Boolean
  }

derive instance genericWebGLContextAttributes :: Generic WebGLContextAttributes _

instance encodeWebGLContextAttributes :: Encode WebGLContextAttributes where
  encode = genericEncode encOptions

defaultWebGLContextAttributes :: WebGLContextAttributes
defaultWebGLContextAttributes =
  WebGLContextAttributes
  { alpha : true
  , depth : true
  , stencil : false
  , antialias : true
  , premultipliedAlpha : true
  , preserveDrawingBuffer : false
  , powerPreference : PowerDefault
  , failIfMajorPerformanceCaveat : false
  }


type InitOptions =
  { gl :: NullOrUndefined WebGLContext
  , canvas :: NullOrUndefined Canvas
  , container :: NullOrUndefined Element
  , attributes :: WebGLContextAttributes
  , pixelRatio :: Number
  , extensions :: Array String
  , optionalExtensions :: Array String
  , profile :: Boolean
  }


newtype WInitOptions = WInitOptions InitOptions
derive instance genericInitOptions :: Generic WInitOptions _

defaultInitOptions :: InitOptions
defaultInitOptions =
  { gl : NullOrUndefined Nothing
  , canvas : NullOrUndefined Nothing
  , container : NullOrUndefined Nothing
  , attributes : defaultWebGLContextAttributes
  , pixelRatio : 1.0
  , extensions : []
  , optionalExtensions : []
  , profile : false
  }

foreign import _init :: forall e a. a -> Eff (regl :: REGL | e) Context -- XXX onDone + Either 

init :: forall e. InitOptions -> Eff (regl :: REGL | e) Context
init = _init <<< genericEncode encOptions <<< WInitOptions


type ContextState =
  { tick :: Int
  , time :: Number
  , viewportWidth :: Number
  , viewportHeight :: Number
  , framebufferWidth :: Int
  , framebufferHeight :: Int
  , drawingBufferWidth :: Int
  , drawingBufferHeight :: Int
  , pixelRatio :: Number
  }


foreign import frame :: forall e e2. Context -> (ContextState -> Eff (regl :: REGL | e) Unit) -> Eff (regl :: REGL | e2) Unit


type ClearOptions = 
  { color :: Array Number
  , depth :: Number
  , stencil :: Number
  }

foreign import clear :: forall e e2. Context -> Eff (regl :: REGL | e) (ClearOptions -> Eff (regl :: REGL | e2) Unit)


data DepthFunc 
  = DepthNever
  | DepthAlways
  | DepthLT
  | DepthLE
  | DepthGT
  | DepthGE
  | DepthEQ
  | DepthNE

instance encodeDepthFunc :: Encode DepthFunc where
  encode = encode <<< f where
    f DepthNever = "never"
    f DepthAlways = "always"
    f DepthLT = "less"
    f DepthLE = "lequal"
    f DepthGT = "greater"
    f DepthGE = "gequal"
    f DepthEQ = "equal"
    f DepthNE = "notequal"

type DepthOptions = 
  { enable :: Boolean
  , mask :: Boolean
  , min :: Number
  , max :: Number
  , func :: DepthFunc
  }


encodeDepthOptions :: DepthOptions -> Foreign
encodeDepthOptions o = toForeign { enable: o.enable
                                 , mask: o.mask
                                 , range: [o.min, o.max]
                                 , func: encode o.func
                                 }

type DrawOptions su du sa da = 
  { frag :: String
  , vert :: String
  , staticUniforms :: su
  , dynamicUniforms :: du
  , staticAttributes :: sa
  , dynamicAttributes :: da
  , elements :: Array Int
  , count :: Int
  , depth :: DepthOptions
  }

encodeDrawOptions :: forall su du sa da. DrawOptions su du sa da -> Foreign
encodeDrawOptions o = toForeign { frag: o.frag
                                , vert: o.vert
                                , staticUniforms: o.staticUniforms
                                , dynamicUniforms: o.dynamicUniforms
                                , staticAttributes: o.staticAttributes
                                , dynamicAttributes: o.dynamicAttributes
                                , elements: o.elements
                                , count: o.count
                                , depth: encodeDepthOptions o.depth
                                }

type RenderOptions du da =
  { uniforms :: du
  , attributes :: da
  }

encodeRenderOptions :: forall du da. RenderOptions du da -> Foreign
encodeRenderOptions = toForeign 

foreign import _draw :: forall du da e e2. (RenderOptions du da -> Foreign) -> Context -> Foreign -> Eff (regl :: REGL | e) (RenderOptions du da -> Eff (regl :: REGL | e2) Unit)


draw :: forall su du sa da e e2. Context -> DrawOptions su du sa da -> Eff (regl :: REGL | e) (RenderOptions du da -> Eff (regl :: REGL | e2) Unit)
draw ctx = _draw encodeRenderOptions ctx <<< encodeDrawOptions

foreign import data Buffer :: Type

foreign import buffer :: forall e. Context -> Array Number -> Eff (regl :: REGL | e) Buffer

data MagFilter 
  = MagNearest
  | MagLinear


instance encodeMagFilter :: Encode MagFilter where
  encode = encode <<< f where
    f MagNearest = "nearest"
    f MagLinear = "linear"
    

data MinFilter
  = MinNearest
  | MinLinear
  | MinLinearMipmapLinear
  | MinNearestMipmapLinear
  | MinLinearMipmapNearest
  | MinNearestMipmapNearest

instance encodeMinFilter :: Encode MinFilter where
  encode = encode <<< f where
    f MinNearest = "nearest"
    f MinLinear = "linear"
    f MinLinearMipmapLinear = "linear mipmap linear"
    f MinNearestMipmapLinear = "nearest mipmap linear"
    f MinLinearMipmapNearest = "linear mipmap nearest"
    f MinNearestMipmapNearest = "nearest mipmap nearest"
    

data WrapMode
  = Repeat
  | Clamp
  | Mirror

instance encodeWrapMode :: Encode WrapMode where
  encode = encode <<< f where
    f Repeat = "repeat"
    f Clamp = "clamp"
    f Mirror = "mirror"
    

data TexFormat
  = Alpha
  | Luminance
  | LuminanceAlpha
  | RGB
  | RGBA
  | RGBA4
  | RGB5A1
  | RGB565
  | SRGB
  | SRGBA
  | Depth
  | DepthStencil
  | RGBDXT1
  | RGBADXT1
  | RGBADXT3
  | RGBADXT5
  | RGBATC
  | RGBAATCExplicitAlpha
  | RGBAATCInterpolatedAlpha
  | RGBPVRTC4BPPv1
  | RGBPVRTC2BPPv1
  | RGBAPVRTC4BPPv1
  | RGBAPVRTC2BPPv1
  | RGBETC1

instance encodeTexFormat :: Encode TexFormat where
  encode = encode <<< f where
    f Alpha = "alpha"
    f Luminance = "luminance"
    f LuminanceAlpha = "luminance alpha"
    f RGB = "rgb"
    f RGBA = "rgba"
    f RGBA4 = "rgba4"
    f RGB5A1 = "rgba5 a1"
    f RGB565 = "rgb565"
    f SRGB = "srgb"
    f SRGBA = "srgba"
    f Depth = "depth"
    f DepthStencil = "depth stencil"
    f RGBDXT1 = "rgb s3tc dxt1"
    f RGBADXT1 = "rgba s3tc dxt1"
    f RGBADXT3 = "rgba s3tc dxt3"
    f RGBADXT5 = "rgba s3tc dxt5"
    f RGBATC = "rgba atc"
    f RGBAATCExplicitAlpha = "rgba atc explicit alpha"
    f RGBAATCInterpolatedAlpha = "rgba atc interpolated alpha"
    f RGBPVRTC4BPPv1 = "rgb pvrtc 4bppv1"
    f RGBPVRTC2BPPv1 = "rgb pvrtc 2bppv1"
    f RGBAPVRTC4BPPv1 = "rgba pvrtc 4bppv1"
    f RGBAPVRTC2BPPv1 = "rgba pvrtc 2bppv1"
    f RGBETC1 = "rgb etc1"
    

    
data ColorSpace
  = ColorSpaceNone
  | ColorSpaceBrowser

instance encodeColorSpace :: Encode ColorSpace where
  encode = encode <<< f where
    f ColorSpaceNone = "none"
    f ColorSpaceBrowser = "browser"
  

data MipmapMode
  = MipmapDontCare
  | MipmapNice
  | MipmapFast

instance encodeMipmapMode :: Encode MipmapMode where
  encode = encode <<< f where
    f MipmapDontCare = "dont care"
    f MipmapNice = "nice"
    f MipmapFast = "fast"

data Alignment
  = AlignmentOne
  | AlignmentTwo
  | AlignmentFour
  | AlignmentEight

instance encodeAlignment :: Encode Alignment where
  encode = encode <<< f where
    f AlignmentOne = 1
    f AlignmentTwo = 2
    f AlignmentFour = 4
    f AlignmentEight = 8

data TexType
  = TexTypeUInt8
  | TexTypeUInt16
  | TexTypeUInt32
  | TexTypeFloat16
  | TexTypeFloat32    


instance encodeTexType :: Encode TexType where
  encode = encode <<< f where
    f TexTypeUInt8 = "uint8"
    f TexTypeUInt16 = "uint16"
    f TexTypeUInt32 = "uint32"
    f TexTypeFloat16 = "float16"
    f TexTypeFloat32 = "float32"


newtype WrappedData a = WrappedData (ArrayView a)

instance encodeArrayBuffer :: Encode (WrappedData a) where
  encode (WrappedData x) = toForeign x


type BitmapOptions a =
  { width :: Int
  , height :: Int
  , data :: WrappedData a
  , type :: TexType
  , format :: TexFormat
  , alignment :: Alignment
  , premultiplyAlpha :: Boolean
  , colorSpace :: ColorSpace
  , channels :: Int
  , flipY :: Boolean
  }

newtype WBitmapOptions a = WBitmapOptions (BitmapOptions a)
derive instance genericBitmapOptions :: Generic (WBitmapOptions a) _

type SamplerOptions =   
  { mag :: MagFilter
  , min :: MinFilter
  , wrapS :: WrapMode
  , wrapT :: WrapMode    
  , aniso :: Int
  , mipmap :: MipmapMode
  }

newtype WSamplerOptions = WSamplerOptions SamplerOptions
derive instance genericSamplerOptions :: Generic WSamplerOptions _


foreign import data Texture :: Type

foreign import _texture :: forall e. Context -> Foreign -> Foreign -> Eff (regl :: REGL | e) Texture

texture :: forall a e. Context -> SamplerOptions -> BitmapOptions a -> Eff (regl :: REGL | e) Texture
texture ctx so bo = _texture ctx (genericEncode encOptions $ WSamplerOptions so) (genericEncode encOptions $ WBitmapOptions bo)

type CubeOptions a = 
  { posX :: BitmapOptions a
  , negX :: BitmapOptions a
  , posY :: BitmapOptions a
  , negY :: BitmapOptions a
  , posZ :: BitmapOptions a
  , negZ :: BitmapOptions a
  }

newtype WCubeOptions a = WCubeOptions (CubeOptions a)
derive instance genericCubeOptions :: Generic (WCubeOptions a) _

instance encodeWCubeOptions :: Encode (WCubeOptions a) where
   encode (WCubeOptions o) = toForeign { posX: enc o.posX
                                       , negX: enc o.negX
                                       , posY: enc o.posY
                                       , negY: enc o.negY
                                       , posZ: enc o.posZ
                                       , negZ: enc o.negZ
                                       }
                             where enc = genericEncode encOptions <<< WBitmapOptions

foreign import data CubeMap :: Type

foreign import _cube :: forall e. Context -> Foreign -> Foreign -> Eff (regl :: REGL | e) CubeMap

cube :: forall a e. Context -> SamplerOptions -> CubeOptions a -> Eff (regl :: REGL | e) CubeMap
cube ctx so co = _cube ctx eso eco
  where eso = genericEncode encOptions $ WSamplerOptions so
        eco = encode $ WCubeOptions co
