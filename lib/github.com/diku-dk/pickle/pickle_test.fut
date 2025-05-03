-- | ignore

import "pickle"

module P = pickle

let test 'a [s] (pu: P.pu a [s]) (p: a->bool) (v:a) =
  p(P.unpickle pu (P.pickle pu v))

-- ==
-- entry: test_i8
-- input { 3i64 } output { true }
-- input { -1i64 } output { true }
-- input { 0x7Fi64 } output { true }
-- input { -0x80i64 } output { true }
-- input { -0x81i64 } output { false }
-- input { 0xFFi64 } output { false }
-- input { 0i64 } output { true }

entry test_i8 (x:i64) : bool =
  let y = i8.i64 x
  in test P.i8 (\z -> x == i64.i8 z) y

-- ==
-- entry: test_i16
-- input { 3i64 } output { true }
-- input { -1i64 } output { true }
-- input { 0x7FFFi64 } output { true }
-- input { -0x8000i64 } output { true }
-- input { -0x8001i64 } output { false }
-- input { 0xFFFFi64 } output { false }
-- input { 0i64 } output { true }

entry test_i16 (x:i64) : bool =
  let y = i16.i64 x
  in test P.i16 (\z -> x == i64.i16 z) y

-- ==
-- entry: test_i32
-- input { 3i64 } output { true }
-- input { -1i64 } output { true }
-- input { 0x7FFF_FFFFi64 } output { true }
-- input { -0x8000_0000i64 } output { true }
-- input { -0x8000_0001i64 } output { false }
-- input { 0xFFFF_FFFFi64 } output { false }
-- input { 0i64 } output { true }

entry test_i32 (x:i64) : bool =
  let y = i32.i64 x
  in test P.i32 (\z -> x == i64.i32 z) y

-- ==
-- entry: test_i64
-- input { 3i64 } output { true }
-- input { -1i64 } output { true }
-- input { 0x7FFF_FFFF_FFFF_FFFFi64 } output { true }
-- input { -0x8000_0000_0000_0000i64 } output { true }
-- input { 0i64 } output { true }

entry test_i64 (x:i64) : bool =
  let y = x
  in test P.i64 (\z -> x == z) y

-- ==
-- entry: test_u8
-- input { 1 } output { true }
-- input { 255 } output { true }
-- input { 0x100i32 } output { false }
-- input { 0 } output { true }

entry test_u8 (x:i32) : bool =
  let y = u8.i32 x
  in test P.u8 (\z -> x == i32.u8 z) y

-- ==
-- entry: test_u16
-- input { 1i64 } output { true }
-- input { 0xFFFFi64 } output { true }
-- input { 0x10000i64 } output { false }
-- input { 0i64 } output { true }

entry test_u16 (x:i64) : bool =
  let y = u16.i64 x
  in test P.u16 (\z -> x == i64.u16 z) y

-- ==
-- entry: test_u32
-- input { 1i64 } output { true }
-- input { 0xFFFFFFFFi64 } output { true }
-- input { 0x100000000i64 } output { false }
-- input { 0i64 } output { true }

entry test_u32 (x:i64) : bool =
  let y = u32.i64 x
  in test P.u32 (\z -> x == i64.u32 z) y

-- ==
-- entry: test_u64
-- input { 1u64 } output { true }
-- input { 0xFFFF_FFFF_FFFF_FFFFu64 } output { true }
-- input { 0u64 } output { true }

entry test_u64 (x:u64) : bool =
  let y = x
  in test P.u64 (\z -> x == z) y

-- ==
-- entry: test_f32
-- input { 1f32 } output { true }
-- input { -1f32 } output { true }
-- input { f32.inf } output { true }

entry test_f32 (x:f32) : bool =
  test P.f32 (==x) x

-- ==
-- entry: test_f64
-- input { 1f64 } output { true }
-- input { -1f64 } output { true }
-- input { f64.inf } output { true }

entry test_f64 (x:f64) : bool =
  test P.f64 (==x) x

-- ==
-- entry: test_bool
-- input { true } output { true }
-- input { false } output { true }

entry test_bool (x:bool) : bool =
  test P.bool (\z -> x == z) x

-- ==
-- entry: pair
-- input { 1i8 2i16 } output { 1i8 2i16 }
-- input { 3i8 4i16 } output { 3i8 4i16 }

entry pair (x1: i8) (x2: i16): (i8, i16) = 
  let pu = P.pair P.i8 P.i16
  let x = (x1, x2)
  in P.unpickle pu (P.pickle pu x)

-- ==
-- entry: pair2
-- input { 0 } output { true }

entry pair2 (_:i32): bool = 
  let pu = P.pair P.i32 (P.pair P.i8 P.i16)
  let v1 = (3,(1,2))
  let v2 = (6,(3,7))
  in test pu (v1 ==) v1
     && test pu (v2 ==) v2

-- ==
-- entry: many
-- input { 0 } output { true }

entry many (_:i32) : bool =
  let pu = P.array 2 (P.array 2 (P.pair P.i32 (P.pair P.i8 P.i16)))
  let pu2 = P.iso (\(a,b) -> {a,b}) (\{a,b} -> (a,b)) (P.pair P.i32 P.i8)
  let v = [[(3,(1,2)),(6,(3,7))],
           [(4,(1,1)),(6,(4,2))]]
  let v2 = {a=23,b=12}
  in test pu (v ==) v
     && test pu2 (v2 ==) v2
     && test P.i64 (0xFFFFFFFFFFFFFFF ==) 0xFFFFFFFFFFFFFFF
     && test P.i32 (\v -> i64.i32 v == 0x7FFFFFFF) 0x7FFFFFFF
     && test P.i16 (\v -> i64.i16 v == 0x7FFF) 0x7FFF
     && test P.u16 (\v -> i64.u16 v == 0x8000) 0x8000
     && test P.i8 (100 ==) 100

-- ==
-- entry: test_row_major
-- input { [[1u8, 2u8], [3u8, 4u8]] } output { [1u8, 2u8, 3u8, 4u8] }

entry test_row_major (v: [2][2]u8) =
  let pu1 = P.array 2 (P.array 2 P.u8)
  in P.pickle pu1 v

-- ==
-- entry: test_col_major
-- input { [[1u8, 2u8], [3u8, 4u8]] } output { [1u8, 3u8, 2u8, 4u8] }

entry test_col_major (v: [2][2]u8) =
  let pu1 = P.array' 2 (P.array' 2 P.u8)
  in P.pickle pu1 v

-- ==
-- entry: test_csti32
-- input { 42 } output { true }

entry test_csti32 (x: i32): bool =
  let pu = P.cst x
  in test pu (x ==) x

-- ==
-- entry: test_cst_unit
-- input { } output { true }

entry test_cst_unit: bool =
  let pu = P.cst ()
  let s = P.pickle pu ()
  let v = P.unpickle pu []
  in null s && v == ()
