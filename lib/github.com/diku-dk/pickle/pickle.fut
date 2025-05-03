-- Serialisation and deserialisation of Futhark values to byte
-- sequences ("pickling").

-- | This module contains picklers for primitive Futhark values, as
-- well as combinators for creating array- and tuple-picklers.  It can
-- be used directly, or as a building block for application-specific
-- picklers.  Trying to unpickle an invalid byte sequence may crash
-- the program.
module type pickle = {
  -- | A pickler that describes both how to pickle and unpickle a
  -- value of type `a` taking s bytes.
  type^ pu 'a [s]

  -- | A sequence of bytes.
  type bytes [n] = [n]u8

  -- | Convert a value to a byte sequence.
  val pickle 'a [s]: pu a [s] -> a -> bytes [s]
  -- | Recover a value from a byte sequence.
  val unpickle 'a [s] : pu a [s] -> bytes [s] -> a

  val i8 : pu i8 [1]
  val i16 : pu i16 [2]
  val i32 : pu i32 [4]
  val i64 : pu i64 [8]

  val u8 : pu u8 [1]
  val u16 : pu u16 [2]
  val u32 : pu u32 [4]
  val u64 : pu u64 [8]

  val f32 : pu f32 [4]
  val f64 : pu f64 [8]

  val bool : pu bool [1]
  val pair 'a 'b [s1] [s2]: pu a [s1] -> pu b [s2] -> pu (a,b) [s1+s2]

  -- | Row major array pickler.
  val array 'a [s]: (k: i64) -> pu a [s] -> pu ([k]a) [k*s]

  -- | Column major array pickler.
  val array' 'a [s]: (k: i64) -> pu a [s] -> pu ([k]a) [s*k]

  -- | Given an isomorphism between types `a` and `b`, as well as a
  -- pickler for `a`, produce a pickler for `b`.  This is particularly
  -- handy for pickling records, as you can simply describe how they
  -- can be converted into nested pairs and back again, and then use
  -- the `pair`@term combinator.
  val iso 'a 'b [s]: (a->b) -> (b->a) -> pu a [s] -> pu b [s]

  val cst 'a: a -> pu a [0]
}

module pickle : pickle = {
  type bytes [n] = [n]u8

  type^ pu 'a [s] = { pickler : a -> bytes [s]
                , unpickler : bytes [s] -> a
                , witness: [s]()}

  let pickle 'a [s] (pu: pu a [s]) = pu.pickler

  let unpickle 'a [s] (pu: pu a [s]) = pu.unpickler

  let iso 'a 'b [s] (f: a->b) (g: b->a) (pu: pu a [s]) : pu b [s] =
    { pickler = pu.pickler <-< g
    , unpickler = pu.unpickler >-> f
    , witness = replicate s ()
    }

  module math = {
    module bool = bool
    module i8 = i8
    module i16 = i16
    module i32 = i32
    module i64 = i64
    module u8 = u8
    module u16 = u16
    module u32 = u32
    module u64 = u64
  }

  let i8 : pu i8 [1] =
    { pickler = \x -> [u8.i8 x]
    , unpickler = \bs -> i8.u8 bs[0]
    , witness = replicate 1 ()
    }

  let i16 : pu i16 [2] =
    { pickler = \x -> [u8.i16 (x>>8),
                       u8.i16 (x>>0)]
    , unpickler = \bs -> i16.u8 bs[0] << 8 |
                         i16.u8 bs[1] << 0
    , witness = replicate 2 ()
    }

  let i32 : pu i32 [4] =
    { pickler = \x -> [u8.i32 (x>>24),
                       u8.i32 (x>>16),
                       u8.i32 (x>>8),
                       u8.i32 (x>>0)]
    , unpickler = \bs -> i32.u8 bs[0] << 24 |
                         i32.u8 bs[1] << 16 |
                         i32.u8 bs[2] << 8 |
                         i32.u8 bs[3] << 0
    , witness = replicate 4 ()
    }

  let i64 : pu i64 [8] =
    { pickler = \x -> [u8.i64 (x>>56),
                       u8.i64 (x>>48),
                       u8.i64 (x>>40),
                       u8.i64 (x>>32),
                       u8.i64 (x>>24),
                       u8.i64 (x>>16),
                       u8.i64 (x>>8),
                       u8.i64 (x>>0)]
    , unpickler = \bs -> i64.u8 bs[0] << 56 |
                         i64.u8 bs[1] << 48 |
                         i64.u8 bs[2] << 40 |
                         i64.u8 bs[3] << 32 |
                         i64.u8 bs[4] << 24 |
                         i64.u8 bs[5] << 16 |
                         i64.u8 bs[6] << 8 |
                         i64.u8 bs[7] << 0
    , witness = replicate 8 ()
    }

  let u64 = iso math.u64.i64 math.i64.u64 i64
  let u32 = iso math.u32.i32 math.i32.u32 i32
  let u16 = iso math.u16.i16 math.i16.u16 i16
  let u8  = iso math.u8.i8   math.i8.u8   i8

  let f32 = iso f32.from_bits f32.to_bits u32
  let f64 = iso f64.from_bits f64.to_bits u64

  let bool : pu bool [1] = iso math.bool.i8 math.i8.bool i8

  let pair 'a 'b [s1] [s2] (pu_a: pu a [s1]) (pu_b: pu b [s2]): pu (a,b) [s1+s2] =
    { pickler = \(a, b) -> pu_a.pickler a ++ pu_b.pickler b
    , unpickler = \bs -> (pu_a.unpickler (take s1 bs)
                         ,pu_b.unpickler ((drop s1 bs) :> [s2]u8))
    , witness = replicate (s1+s2) ()
    }

  --| Col major array pickler
  let array' 'a [s] k (pu: pu a [s]): pu ([k]a) [s*k] =
    { pickler = \(arr: [k]a) -> arr |> map pu.pickler |> transpose |> flatten
    , unpickler = \bs ->
                    take (s*k) bs
                      |> unflatten
                      |> transpose
                      |> map pu.unpickler
    , witness = replicate (s*k) ()
    }

  --| Row major array pickler
  let array 'a [s] k (pu: pu a [s]): pu ([k]a) [k*s] =
    { pickler = \(arr: [k]a) -> arr |> map pu.pickler |> flatten
    , unpickler = \bs ->
                    take (k*s) bs
                      |> unflatten
                      |> map pu.unpickler
    , witness = replicate (k*s) ()
    }

  let cst 'a (x: a): pu a [0] =
    { pickler = \_ -> []
    , unpickler = \_ -> x
    , witness = replicate 0 ()
    }
}
