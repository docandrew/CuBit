with Interfaces;
package body Heap_Classes with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   -- Each entry describes a 16-byte request bucket. A small read-only table
   -- avoids several unpredictable comparisons on mixed-size request traces.
   type Bucket is range 1 .. 256;
   Classes : constant array (Bucket) of Size_Class :=
     [1 => Bytes_16, 2 => Bytes_32, 3 => Bytes_48, 4 => Bytes_64,
      5 => Bytes_80, 6 => Bytes_96, 7 => Bytes_112, 8 => Bytes_128,
      9 .. 10 => Bytes_160, 11 .. 12 => Bytes_192, 13 .. 14 => Bytes_224, 15 .. 16 => Bytes_256,
      17 .. 20 => Bytes_320, 21 .. 24 => Bytes_384, 25 .. 28 => Bytes_448, 29 .. 32 => Bytes_512,
      33 .. 40 => Bytes_640, 41 .. 48 => Bytes_768, 49 .. 56 => Bytes_896, 57 .. 64 => Bytes_1024,
      65 .. 80 => Bytes_1280, 81 .. 96 => Bytes_1536, 97 .. 112 => Bytes_1792, 113 .. 128 => Bytes_2048,
      129 .. 160 => Bytes_2560, 161 .. 192 => Bytes_3072, 193 .. 224 => Bytes_3584, 225 .. 256 => Bytes_4096];
   function Class_For (Size : Request_Size) return Size_Class is
     (Classes (Bucket ((Size + 15) / 16)));

   procedure Prove_Stride (Class : Size_Class) is
   begin
      case Class is
         when Bytes_16 => pragma Assert (Stride (Class) = 16 and then 16 mod 16 = 0);
         when Bytes_32 => pragma Assert (Stride (Class) = 32 and then 32 mod 16 = 0);
         when Bytes_48 => pragma Assert (Stride (Class) = 48 and then 48 mod 16 = 0);
         when Bytes_64 => pragma Assert (Stride (Class) = 64 and then 64 mod 16 = 0);
         when Bytes_80 => pragma Assert (Stride (Class) = 80 and then 80 mod 16 = 0);
         when Bytes_96 => pragma Assert (Stride (Class) = 96 and then 96 mod 16 = 0);
         when Bytes_112 => pragma Assert (Stride (Class) = 112 and then 112 mod 16 = 0);
         when Bytes_128 => pragma Assert (Stride (Class) = 128 and then 128 mod 16 = 0);
         when Bytes_160 => pragma Assert (Stride (Class) = 160 and then 160 mod 16 = 0);
         when Bytes_192 => pragma Assert (Stride (Class) = 192 and then 192 mod 16 = 0);
         when Bytes_224 => pragma Assert (Stride (Class) = 224 and then 224 mod 16 = 0);
         when Bytes_256 => pragma Assert (Stride (Class) = 256 and then 256 mod 16 = 0);
         when Bytes_320 => pragma Assert (Stride (Class) = 320 and then 320 mod 16 = 0);
         when Bytes_384 => pragma Assert (Stride (Class) = 384 and then 384 mod 16 = 0);
         when Bytes_448 => pragma Assert (Stride (Class) = 448 and then 448 mod 16 = 0);
         when Bytes_512 => pragma Assert (Stride (Class) = 512 and then 512 mod 16 = 0);
         when Bytes_640 => pragma Assert (Stride (Class) = 640 and then 640 mod 16 = 0);
         when Bytes_768 => pragma Assert (Stride (Class) = 768 and then 768 mod 16 = 0);
         when Bytes_896 => pragma Assert (Stride (Class) = 896 and then 896 mod 16 = 0);
         when Bytes_1024 => pragma Assert (Stride (Class) = 1_024 and then 1_024 mod 16 = 0);
         when Bytes_1280 => pragma Assert (Stride (Class) = 1_280 and then 1_280 mod 16 = 0);
         when Bytes_1536 => pragma Assert (Stride (Class) = 1_536 and then 1_536 mod 16 = 0);
         when Bytes_1792 => pragma Assert (Stride (Class) = 1_792 and then 1_792 mod 16 = 0);
         when Bytes_2048 => pragma Assert (Stride (Class) = 2_048 and then 2_048 mod 16 = 0);
         when Bytes_2560 => pragma Assert (Stride (Class) = 2_560 and then 2_560 mod 16 = 0);
         when Bytes_3072 => pragma Assert (Stride (Class) = 3_072 and then 3_072 mod 16 = 0);
         when Bytes_3584 => pragma Assert (Stride (Class) = 3_584 and then 3_584 mod 16 = 0);
         when Bytes_4096 => pragma Assert (Stride (Class) = 4_096 and then 4_096 mod 16 = 0);
      end case;
   end Prove_Stride;

   function Slots (Class : Size_Class) return Slot_Total is
   begin
      -- Proof only (no code): each table entry is its stride's quotient.
      case Class is
         when Bytes_16 => pragma Assert (Slot_Counts (Class) = 65_536 / 16 and then Stride (Class) = 16);
         when Bytes_32 => pragma Assert (Slot_Counts (Class) = 65_536 / 32 and then Stride (Class) = 32);
         when Bytes_48 => pragma Assert (Slot_Counts (Class) = 65_536 / 48 and then Stride (Class) = 48);
         when Bytes_64 => pragma Assert (Slot_Counts (Class) = 65_536 / 64 and then Stride (Class) = 64);
         when Bytes_80 => pragma Assert (Slot_Counts (Class) = 65_536 / 80 and then Stride (Class) = 80);
         when Bytes_96 => pragma Assert (Slot_Counts (Class) = 65_536 / 96 and then Stride (Class) = 96);
         when Bytes_112 => pragma Assert (Slot_Counts (Class) = 65_536 / 112 and then Stride (Class) = 112);
         when Bytes_128 => pragma Assert (Slot_Counts (Class) = 65_536 / 128 and then Stride (Class) = 128);
         when Bytes_160 => pragma Assert (Slot_Counts (Class) = 65_536 / 160 and then Stride (Class) = 160);
         when Bytes_192 => pragma Assert (Slot_Counts (Class) = 65_536 / 192 and then Stride (Class) = 192);
         when Bytes_224 => pragma Assert (Slot_Counts (Class) = 65_536 / 224 and then Stride (Class) = 224);
         when Bytes_256 => pragma Assert (Slot_Counts (Class) = 65_536 / 256 and then Stride (Class) = 256);
         when Bytes_320 => pragma Assert (Slot_Counts (Class) = 65_536 / 320 and then Stride (Class) = 320);
         when Bytes_384 => pragma Assert (Slot_Counts (Class) = 65_536 / 384 and then Stride (Class) = 384);
         when Bytes_448 => pragma Assert (Slot_Counts (Class) = 65_536 / 448 and then Stride (Class) = 448);
         when Bytes_512 => pragma Assert (Slot_Counts (Class) = 65_536 / 512 and then Stride (Class) = 512);
         when Bytes_640 => pragma Assert (Slot_Counts (Class) = 65_536 / 640 and then Stride (Class) = 640);
         when Bytes_768 => pragma Assert (Slot_Counts (Class) = 65_536 / 768 and then Stride (Class) = 768);
         when Bytes_896 => pragma Assert (Slot_Counts (Class) = 65_536 / 896 and then Stride (Class) = 896);
         when Bytes_1024 => pragma Assert (Slot_Counts (Class) = 65_536 / 1_024 and then Stride (Class) = 1_024);
         when Bytes_1280 => pragma Assert (Slot_Counts (Class) = 65_536 / 1_280 and then Stride (Class) = 1_280);
         when Bytes_1536 => pragma Assert (Slot_Counts (Class) = 65_536 / 1_536 and then Stride (Class) = 1_536);
         when Bytes_1792 => pragma Assert (Slot_Counts (Class) = 65_536 / 1_792 and then Stride (Class) = 1_792);
         when Bytes_2048 => pragma Assert (Slot_Counts (Class) = 65_536 / 2_048 and then Stride (Class) = 2_048);
         when Bytes_2560 => pragma Assert (Slot_Counts (Class) = 65_536 / 2_560 and then Stride (Class) = 2_560);
         when Bytes_3072 => pragma Assert (Slot_Counts (Class) = 65_536 / 3_072 and then Stride (Class) = 3_072);
         when Bytes_3584 => pragma Assert (Slot_Counts (Class) = 65_536 / 3_584 and then Stride (Class) = 3_584);
         when Bytes_4096 => pragma Assert (Slot_Counts (Class) = 65_536 / 4_096 and then Stride (Class) = 4_096);
      end case;
      return Slot_Counts (Class);
   end Slots;

   -- The product of an offset and a reciprocal: below 2 ** 45.
   type Wide is range 0 .. 2 ** 45;
   procedure Divide (Relative : Geometry_Offset; Class : Size_Class; Quotient : out Natural;
                     Exact : out Boolean) is
      M : constant Wide := Wide (Magics (Class));
      P : constant Wide := Wide (Relative) * M;
      -- P = Q * 2 ** 32 + L, L < 2 ** 32: one multiplication, then the
      -- high half is the quotient and a low half below the reciprocal
      -- says the division was exact. Unsigned halves: no sign correction
      -- in the code (P is not negative).
      U : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (P);
      Q : constant Wide := Wide (Interfaces.Shift_Right (U, Reciprocal_Shift));
      L : constant Wide := P - Q * 2 ** Reciprocal_Shift;
   begin
      -- The shift is the division (the one bit-vector fact, for every class).
      pragma Assert (Q = P / 2 ** Reciprocal_Shift);
      -- Proof only (no code): with M * stride = 2 ** 32 + E (E < stride),
      -- P = (R / stride) * 2 ** 32 + ((R / stride) * E + (R mod stride) * M),
      -- the second term below 2 ** 32, and below M exactly when
      -- R mod stride = 0. Linear in each class.
      case Class is
         when Bytes_16 =>
            pragma Assert (Stride (Class) = 16 and then M = 268_435_456);
            pragma Assert (P = Wide (Relative / 16) * 4_294_967_296 + (Wide (Relative / 16) * 0 + Wide (Relative mod 16) * 268_435_456));
            pragma Assert (Wide (Relative / 16) * 0 + Wide (Relative mod 16) * 268_435_456 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 16));
            pragma Assert (L = Wide (Relative / 16) * 0 + Wide (Relative mod 16) * 268_435_456);
            pragma Assert ((L < M) = (Relative mod 16 = 0));
         when Bytes_32 =>
            pragma Assert (Stride (Class) = 32 and then M = 134_217_728);
            pragma Assert (P = Wide (Relative / 32) * 4_294_967_296 + (Wide (Relative / 32) * 0 + Wide (Relative mod 32) * 134_217_728));
            pragma Assert (Wide (Relative / 32) * 0 + Wide (Relative mod 32) * 134_217_728 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 32));
            pragma Assert (L = Wide (Relative / 32) * 0 + Wide (Relative mod 32) * 134_217_728);
            pragma Assert ((L < M) = (Relative mod 32 = 0));
         when Bytes_48 =>
            pragma Assert (Stride (Class) = 48 and then M = 89_478_486);
            pragma Assert (P = Wide (Relative / 48) * 4_294_967_296 + (Wide (Relative / 48) * 32 + Wide (Relative mod 48) * 89_478_486));
            pragma Assert (Wide (Relative / 48) * 32 + Wide (Relative mod 48) * 89_478_486 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 48));
            pragma Assert (L = Wide (Relative / 48) * 32 + Wide (Relative mod 48) * 89_478_486);
            pragma Assert ((L < M) = (Relative mod 48 = 0));
         when Bytes_64 =>
            pragma Assert (Stride (Class) = 64 and then M = 67_108_864);
            pragma Assert (P = Wide (Relative / 64) * 4_294_967_296 + (Wide (Relative / 64) * 0 + Wide (Relative mod 64) * 67_108_864));
            pragma Assert (Wide (Relative / 64) * 0 + Wide (Relative mod 64) * 67_108_864 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 64));
            pragma Assert (L = Wide (Relative / 64) * 0 + Wide (Relative mod 64) * 67_108_864);
            pragma Assert ((L < M) = (Relative mod 64 = 0));
         when Bytes_80 =>
            pragma Assert (Stride (Class) = 80 and then M = 53_687_092);
            pragma Assert (P = Wide (Relative / 80) * 4_294_967_296 + (Wide (Relative / 80) * 64 + Wide (Relative mod 80) * 53_687_092));
            pragma Assert (Wide (Relative / 80) * 64 + Wide (Relative mod 80) * 53_687_092 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 80));
            pragma Assert (L = Wide (Relative / 80) * 64 + Wide (Relative mod 80) * 53_687_092);
            pragma Assert ((L < M) = (Relative mod 80 = 0));
         when Bytes_96 =>
            pragma Assert (Stride (Class) = 96 and then M = 44_739_243);
            pragma Assert (P = Wide (Relative / 96) * 4_294_967_296 + (Wide (Relative / 96) * 32 + Wide (Relative mod 96) * 44_739_243));
            pragma Assert (Wide (Relative / 96) * 32 + Wide (Relative mod 96) * 44_739_243 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 96));
            pragma Assert (L = Wide (Relative / 96) * 32 + Wide (Relative mod 96) * 44_739_243);
            pragma Assert ((L < M) = (Relative mod 96 = 0));
         when Bytes_112 =>
            pragma Assert (Stride (Class) = 112 and then M = 38_347_923);
            pragma Assert (P = Wide (Relative / 112) * 4_294_967_296 + (Wide (Relative / 112) * 80 + Wide (Relative mod 112) * 38_347_923));
            pragma Assert (Wide (Relative / 112) * 80 + Wide (Relative mod 112) * 38_347_923 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 112));
            pragma Assert (L = Wide (Relative / 112) * 80 + Wide (Relative mod 112) * 38_347_923);
            pragma Assert ((L < M) = (Relative mod 112 = 0));
         when Bytes_128 =>
            pragma Assert (Stride (Class) = 128 and then M = 33_554_432);
            pragma Assert (P = Wide (Relative / 128) * 4_294_967_296 + (Wide (Relative / 128) * 0 + Wide (Relative mod 128) * 33_554_432));
            pragma Assert (Wide (Relative / 128) * 0 + Wide (Relative mod 128) * 33_554_432 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 128));
            pragma Assert (L = Wide (Relative / 128) * 0 + Wide (Relative mod 128) * 33_554_432);
            pragma Assert ((L < M) = (Relative mod 128 = 0));
         when Bytes_160 =>
            pragma Assert (Stride (Class) = 160 and then M = 26_843_546);
            pragma Assert (P = Wide (Relative / 160) * 4_294_967_296 + (Wide (Relative / 160) * 64 + Wide (Relative mod 160) * 26_843_546));
            pragma Assert (Wide (Relative / 160) * 64 + Wide (Relative mod 160) * 26_843_546 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 160));
            pragma Assert (L = Wide (Relative / 160) * 64 + Wide (Relative mod 160) * 26_843_546);
            pragma Assert ((L < M) = (Relative mod 160 = 0));
         when Bytes_192 =>
            pragma Assert (Stride (Class) = 192 and then M = 22_369_622);
            pragma Assert (P = Wide (Relative / 192) * 4_294_967_296 + (Wide (Relative / 192) * 128 + Wide (Relative mod 192) * 22_369_622));
            pragma Assert (Wide (Relative / 192) * 128 + Wide (Relative mod 192) * 22_369_622 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 192));
            pragma Assert (L = Wide (Relative / 192) * 128 + Wide (Relative mod 192) * 22_369_622);
            pragma Assert ((L < M) = (Relative mod 192 = 0));
         when Bytes_224 =>
            pragma Assert (Stride (Class) = 224 and then M = 19_173_962);
            pragma Assert (P = Wide (Relative / 224) * 4_294_967_296 + (Wide (Relative / 224) * 192 + Wide (Relative mod 224) * 19_173_962));
            pragma Assert (Wide (Relative / 224) * 192 + Wide (Relative mod 224) * 19_173_962 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 224));
            pragma Assert (L = Wide (Relative / 224) * 192 + Wide (Relative mod 224) * 19_173_962);
            pragma Assert ((L < M) = (Relative mod 224 = 0));
         when Bytes_256 =>
            pragma Assert (Stride (Class) = 256 and then M = 16_777_216);
            pragma Assert (P = Wide (Relative / 256) * 4_294_967_296 + (Wide (Relative / 256) * 0 + Wide (Relative mod 256) * 16_777_216));
            pragma Assert (Wide (Relative / 256) * 0 + Wide (Relative mod 256) * 16_777_216 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 256));
            pragma Assert (L = Wide (Relative / 256) * 0 + Wide (Relative mod 256) * 16_777_216);
            pragma Assert ((L < M) = (Relative mod 256 = 0));
         when Bytes_320 =>
            pragma Assert (Stride (Class) = 320 and then M = 13_421_773);
            pragma Assert (P = Wide (Relative / 320) * 4_294_967_296 + (Wide (Relative / 320) * 64 + Wide (Relative mod 320) * 13_421_773));
            pragma Assert (Wide (Relative / 320) * 64 + Wide (Relative mod 320) * 13_421_773 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 320));
            pragma Assert (L = Wide (Relative / 320) * 64 + Wide (Relative mod 320) * 13_421_773);
            pragma Assert ((L < M) = (Relative mod 320 = 0));
         when Bytes_384 =>
            pragma Assert (Stride (Class) = 384 and then M = 11_184_811);
            pragma Assert (P = Wide (Relative / 384) * 4_294_967_296 + (Wide (Relative / 384) * 128 + Wide (Relative mod 384) * 11_184_811));
            pragma Assert (Wide (Relative / 384) * 128 + Wide (Relative mod 384) * 11_184_811 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 384));
            pragma Assert (L = Wide (Relative / 384) * 128 + Wide (Relative mod 384) * 11_184_811);
            pragma Assert ((L < M) = (Relative mod 384 = 0));
         when Bytes_448 =>
            pragma Assert (Stride (Class) = 448 and then M = 9_586_981);
            pragma Assert (P = Wide (Relative / 448) * 4_294_967_296 + (Wide (Relative / 448) * 192 + Wide (Relative mod 448) * 9_586_981));
            pragma Assert (Wide (Relative / 448) * 192 + Wide (Relative mod 448) * 9_586_981 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 448));
            pragma Assert (L = Wide (Relative / 448) * 192 + Wide (Relative mod 448) * 9_586_981);
            pragma Assert ((L < M) = (Relative mod 448 = 0));
         when Bytes_512 =>
            pragma Assert (Stride (Class) = 512 and then M = 8_388_608);
            pragma Assert (P = Wide (Relative / 512) * 4_294_967_296 + (Wide (Relative / 512) * 0 + Wide (Relative mod 512) * 8_388_608));
            pragma Assert (Wide (Relative / 512) * 0 + Wide (Relative mod 512) * 8_388_608 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 512));
            pragma Assert (L = Wide (Relative / 512) * 0 + Wide (Relative mod 512) * 8_388_608);
            pragma Assert ((L < M) = (Relative mod 512 = 0));
         when Bytes_640 =>
            pragma Assert (Stride (Class) = 640 and then M = 6_710_887);
            pragma Assert (P = Wide (Relative / 640) * 4_294_967_296 + (Wide (Relative / 640) * 384 + Wide (Relative mod 640) * 6_710_887));
            pragma Assert (Wide (Relative / 640) * 384 + Wide (Relative mod 640) * 6_710_887 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 640));
            pragma Assert (L = Wide (Relative / 640) * 384 + Wide (Relative mod 640) * 6_710_887);
            pragma Assert ((L < M) = (Relative mod 640 = 0));
         when Bytes_768 =>
            pragma Assert (Stride (Class) = 768 and then M = 5_592_406);
            pragma Assert (P = Wide (Relative / 768) * 4_294_967_296 + (Wide (Relative / 768) * 512 + Wide (Relative mod 768) * 5_592_406));
            pragma Assert (Wide (Relative / 768) * 512 + Wide (Relative mod 768) * 5_592_406 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 768));
            pragma Assert (L = Wide (Relative / 768) * 512 + Wide (Relative mod 768) * 5_592_406);
            pragma Assert ((L < M) = (Relative mod 768 = 0));
         when Bytes_896 =>
            pragma Assert (Stride (Class) = 896 and then M = 4_793_491);
            pragma Assert (P = Wide (Relative / 896) * 4_294_967_296 + (Wide (Relative / 896) * 640 + Wide (Relative mod 896) * 4_793_491));
            pragma Assert (Wide (Relative / 896) * 640 + Wide (Relative mod 896) * 4_793_491 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 896));
            pragma Assert (L = Wide (Relative / 896) * 640 + Wide (Relative mod 896) * 4_793_491);
            pragma Assert ((L < M) = (Relative mod 896 = 0));
         when Bytes_1024 =>
            pragma Assert (Stride (Class) = 1_024 and then M = 4_194_304);
            pragma Assert (P = Wide (Relative / 1_024) * 4_294_967_296 + (Wide (Relative / 1_024) * 0 + Wide (Relative mod 1_024) * 4_194_304));
            pragma Assert (Wide (Relative / 1_024) * 0 + Wide (Relative mod 1_024) * 4_194_304 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 1_024));
            pragma Assert (L = Wide (Relative / 1_024) * 0 + Wide (Relative mod 1_024) * 4_194_304);
            pragma Assert ((L < M) = (Relative mod 1_024 = 0));
         when Bytes_1280 =>
            pragma Assert (Stride (Class) = 1_280 and then M = 3_355_444);
            pragma Assert (P = Wide (Relative / 1_280) * 4_294_967_296 + (Wide (Relative / 1_280) * 1_024 + Wide (Relative mod 1_280) * 3_355_444));
            pragma Assert (Wide (Relative / 1_280) * 1_024 + Wide (Relative mod 1_280) * 3_355_444 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 1_280));
            pragma Assert (L = Wide (Relative / 1_280) * 1_024 + Wide (Relative mod 1_280) * 3_355_444);
            pragma Assert ((L < M) = (Relative mod 1_280 = 0));
         when Bytes_1536 =>
            pragma Assert (Stride (Class) = 1_536 and then M = 2_796_203);
            pragma Assert (P = Wide (Relative / 1_536) * 4_294_967_296 + (Wide (Relative / 1_536) * 512 + Wide (Relative mod 1_536) * 2_796_203));
            pragma Assert (Wide (Relative / 1_536) * 512 + Wide (Relative mod 1_536) * 2_796_203 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 1_536));
            pragma Assert (L = Wide (Relative / 1_536) * 512 + Wide (Relative mod 1_536) * 2_796_203);
            pragma Assert ((L < M) = (Relative mod 1_536 = 0));
         when Bytes_1792 =>
            pragma Assert (Stride (Class) = 1_792 and then M = 2_396_746);
            pragma Assert (P = Wide (Relative / 1_792) * 4_294_967_296 + (Wide (Relative / 1_792) * 1_536 + Wide (Relative mod 1_792) * 2_396_746));
            pragma Assert (Wide (Relative / 1_792) * 1_536 + Wide (Relative mod 1_792) * 2_396_746 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 1_792));
            pragma Assert (L = Wide (Relative / 1_792) * 1_536 + Wide (Relative mod 1_792) * 2_396_746);
            pragma Assert ((L < M) = (Relative mod 1_792 = 0));
         when Bytes_2048 =>
            pragma Assert (Stride (Class) = 2_048 and then M = 2_097_152);
            pragma Assert (P = Wide (Relative / 2_048) * 4_294_967_296 + (Wide (Relative / 2_048) * 0 + Wide (Relative mod 2_048) * 2_097_152));
            pragma Assert (Wide (Relative / 2_048) * 0 + Wide (Relative mod 2_048) * 2_097_152 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 2_048));
            pragma Assert (L = Wide (Relative / 2_048) * 0 + Wide (Relative mod 2_048) * 2_097_152);
            pragma Assert ((L < M) = (Relative mod 2_048 = 0));
         when Bytes_2560 =>
            pragma Assert (Stride (Class) = 2_560 and then M = 1_677_722);
            pragma Assert (P = Wide (Relative / 2_560) * 4_294_967_296 + (Wide (Relative / 2_560) * 1_024 + Wide (Relative mod 2_560) * 1_677_722));
            pragma Assert (Wide (Relative / 2_560) * 1_024 + Wide (Relative mod 2_560) * 1_677_722 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 2_560));
            pragma Assert (L = Wide (Relative / 2_560) * 1_024 + Wide (Relative mod 2_560) * 1_677_722);
            pragma Assert ((L < M) = (Relative mod 2_560 = 0));
         when Bytes_3072 =>
            pragma Assert (Stride (Class) = 3_072 and then M = 1_398_102);
            pragma Assert (P = Wide (Relative / 3_072) * 4_294_967_296 + (Wide (Relative / 3_072) * 2_048 + Wide (Relative mod 3_072) * 1_398_102));
            pragma Assert (Wide (Relative / 3_072) * 2_048 + Wide (Relative mod 3_072) * 1_398_102 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 3_072));
            pragma Assert (L = Wide (Relative / 3_072) * 2_048 + Wide (Relative mod 3_072) * 1_398_102);
            pragma Assert ((L < M) = (Relative mod 3_072 = 0));
         when Bytes_3584 =>
            pragma Assert (Stride (Class) = 3_584 and then M = 1_198_373);
            pragma Assert (P = Wide (Relative / 3_584) * 4_294_967_296 + (Wide (Relative / 3_584) * 1_536 + Wide (Relative mod 3_584) * 1_198_373));
            pragma Assert (Wide (Relative / 3_584) * 1_536 + Wide (Relative mod 3_584) * 1_198_373 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 3_584));
            pragma Assert (L = Wide (Relative / 3_584) * 1_536 + Wide (Relative mod 3_584) * 1_198_373);
            pragma Assert ((L < M) = (Relative mod 3_584 = 0));
         when Bytes_4096 =>
            pragma Assert (Stride (Class) = 4_096 and then M = 1_048_576);
            pragma Assert (P = Wide (Relative / 4_096) * 4_294_967_296 + (Wide (Relative / 4_096) * 0 + Wide (Relative mod 4_096) * 1_048_576));
            pragma Assert (Wide (Relative / 4_096) * 0 + Wide (Relative mod 4_096) * 1_048_576 < 4_294_967_296);
            pragma Assert (Q = Wide (Relative / 4_096));
            pragma Assert (L = Wide (Relative / 4_096) * 0 + Wide (Relative mod 4_096) * 1_048_576);
            pragma Assert ((L < M) = (Relative mod 4_096 = 0));
      end case;
      Quotient := Natural (Q);
      Exact := L < M;
   end Divide;

   function Scale (Item : Natural; Class : Size_Class) return Natural is
      V : constant Natural := Item * Stride (Class);
   begin
      -- Proof only (no code): in each class the product ends in the slab.
      case Class is
         when Bytes_16 => pragma Assert (Stride (Class) = 16 and then Slots (Class) = 4_096 and then V = Item * 16 and then V mod 16 = 0 and then V + 16 <= 65_536);
         when Bytes_32 => pragma Assert (Stride (Class) = 32 and then Slots (Class) = 2_048 and then V = Item * 32 and then V mod 16 = 0 and then V + 32 <= 65_536);
         when Bytes_48 => pragma Assert (Stride (Class) = 48 and then Slots (Class) = 1_365 and then V = Item * 48 and then V mod 16 = 0 and then V + 48 <= 65_536);
         when Bytes_64 => pragma Assert (Stride (Class) = 64 and then Slots (Class) = 1_024 and then V = Item * 64 and then V mod 16 = 0 and then V + 64 <= 65_536);
         when Bytes_80 => pragma Assert (Stride (Class) = 80 and then Slots (Class) = 819 and then V = Item * 80 and then V mod 16 = 0 and then V + 80 <= 65_536);
         when Bytes_96 => pragma Assert (Stride (Class) = 96 and then Slots (Class) = 682 and then V = Item * 96 and then V mod 16 = 0 and then V + 96 <= 65_536);
         when Bytes_112 => pragma Assert (Stride (Class) = 112 and then Slots (Class) = 585 and then V = Item * 112 and then V mod 16 = 0 and then V + 112 <= 65_536);
         when Bytes_128 => pragma Assert (Stride (Class) = 128 and then Slots (Class) = 512 and then V = Item * 128 and then V mod 16 = 0 and then V + 128 <= 65_536);
         when Bytes_160 => pragma Assert (Stride (Class) = 160 and then Slots (Class) = 409 and then V = Item * 160 and then V mod 16 = 0 and then V + 160 <= 65_536);
         when Bytes_192 => pragma Assert (Stride (Class) = 192 and then Slots (Class) = 341 and then V = Item * 192 and then V mod 16 = 0 and then V + 192 <= 65_536);
         when Bytes_224 => pragma Assert (Stride (Class) = 224 and then Slots (Class) = 292 and then V = Item * 224 and then V mod 16 = 0 and then V + 224 <= 65_536);
         when Bytes_256 => pragma Assert (Stride (Class) = 256 and then Slots (Class) = 256 and then V = Item * 256 and then V mod 16 = 0 and then V + 256 <= 65_536);
         when Bytes_320 => pragma Assert (Stride (Class) = 320 and then Slots (Class) = 204 and then V = Item * 320 and then V mod 16 = 0 and then V + 320 <= 65_536);
         when Bytes_384 => pragma Assert (Stride (Class) = 384 and then Slots (Class) = 170 and then V = Item * 384 and then V mod 16 = 0 and then V + 384 <= 65_536);
         when Bytes_448 => pragma Assert (Stride (Class) = 448 and then Slots (Class) = 146 and then V = Item * 448 and then V mod 16 = 0 and then V + 448 <= 65_536);
         when Bytes_512 => pragma Assert (Stride (Class) = 512 and then Slots (Class) = 128 and then V = Item * 512 and then V mod 16 = 0 and then V + 512 <= 65_536);
         when Bytes_640 => pragma Assert (Stride (Class) = 640 and then Slots (Class) = 102 and then V = Item * 640 and then V mod 16 = 0 and then V + 640 <= 65_536);
         when Bytes_768 => pragma Assert (Stride (Class) = 768 and then Slots (Class) = 85 and then V = Item * 768 and then V mod 16 = 0 and then V + 768 <= 65_536);
         when Bytes_896 => pragma Assert (Stride (Class) = 896 and then Slots (Class) = 73 and then V = Item * 896 and then V mod 16 = 0 and then V + 896 <= 65_536);
         when Bytes_1024 => pragma Assert (Stride (Class) = 1_024 and then Slots (Class) = 64 and then V = Item * 1_024 and then V mod 16 = 0 and then V + 1_024 <= 65_536);
         when Bytes_1280 => pragma Assert (Stride (Class) = 1_280 and then Slots (Class) = 51 and then V = Item * 1_280 and then V mod 16 = 0 and then V + 1_280 <= 65_536);
         when Bytes_1536 => pragma Assert (Stride (Class) = 1_536 and then Slots (Class) = 42 and then V = Item * 1_536 and then V mod 16 = 0 and then V + 1_536 <= 65_536);
         when Bytes_1792 => pragma Assert (Stride (Class) = 1_792 and then Slots (Class) = 36 and then V = Item * 1_792 and then V mod 16 = 0 and then V + 1_792 <= 65_536);
         when Bytes_2048 => pragma Assert (Stride (Class) = 2_048 and then Slots (Class) = 32 and then V = Item * 2_048 and then V mod 16 = 0 and then V + 2_048 <= 65_536);
         when Bytes_2560 => pragma Assert (Stride (Class) = 2_560 and then Slots (Class) = 25 and then V = Item * 2_560 and then V mod 16 = 0 and then V + 2_560 <= 65_536);
         when Bytes_3072 => pragma Assert (Stride (Class) = 3_072 and then Slots (Class) = 21 and then V = Item * 3_072 and then V mod 16 = 0 and then V + 3_072 <= 65_536);
         when Bytes_3584 => pragma Assert (Stride (Class) = 3_584 and then Slots (Class) = 18 and then V = Item * 3_584 and then V mod 16 = 0 and then V + 3_584 <= 65_536);
         when Bytes_4096 => pragma Assert (Stride (Class) = 4_096 and then Slots (Class) = 16 and then V = Item * 4_096 and then V mod 16 = 0 and then V + 4_096 <= 65_536);
      end case;
      return V;
   end Scale;

   procedure Prove_Scale (Item : Natural; Class : Size_Class) is
   begin
      case Class is
         when Bytes_16 => pragma Assert (Stride (Class) = 16 and then Scale (Item, Class) = Item * 16);
         when Bytes_32 => pragma Assert (Stride (Class) = 32 and then Scale (Item, Class) = Item * 32);
         when Bytes_48 => pragma Assert (Stride (Class) = 48 and then Scale (Item, Class) = Item * 48);
         when Bytes_64 => pragma Assert (Stride (Class) = 64 and then Scale (Item, Class) = Item * 64);
         when Bytes_80 => pragma Assert (Stride (Class) = 80 and then Scale (Item, Class) = Item * 80);
         when Bytes_96 => pragma Assert (Stride (Class) = 96 and then Scale (Item, Class) = Item * 96);
         when Bytes_112 => pragma Assert (Stride (Class) = 112 and then Scale (Item, Class) = Item * 112);
         when Bytes_128 => pragma Assert (Stride (Class) = 128 and then Scale (Item, Class) = Item * 128);
         when Bytes_160 => pragma Assert (Stride (Class) = 160 and then Scale (Item, Class) = Item * 160);
         when Bytes_192 => pragma Assert (Stride (Class) = 192 and then Scale (Item, Class) = Item * 192);
         when Bytes_224 => pragma Assert (Stride (Class) = 224 and then Scale (Item, Class) = Item * 224);
         when Bytes_256 => pragma Assert (Stride (Class) = 256 and then Scale (Item, Class) = Item * 256);
         when Bytes_320 => pragma Assert (Stride (Class) = 320 and then Scale (Item, Class) = Item * 320);
         when Bytes_384 => pragma Assert (Stride (Class) = 384 and then Scale (Item, Class) = Item * 384);
         when Bytes_448 => pragma Assert (Stride (Class) = 448 and then Scale (Item, Class) = Item * 448);
         when Bytes_512 => pragma Assert (Stride (Class) = 512 and then Scale (Item, Class) = Item * 512);
         when Bytes_640 => pragma Assert (Stride (Class) = 640 and then Scale (Item, Class) = Item * 640);
         when Bytes_768 => pragma Assert (Stride (Class) = 768 and then Scale (Item, Class) = Item * 768);
         when Bytes_896 => pragma Assert (Stride (Class) = 896 and then Scale (Item, Class) = Item * 896);
         when Bytes_1024 => pragma Assert (Stride (Class) = 1_024 and then Scale (Item, Class) = Item * 1_024);
         when Bytes_1280 => pragma Assert (Stride (Class) = 1_280 and then Scale (Item, Class) = Item * 1_280);
         when Bytes_1536 => pragma Assert (Stride (Class) = 1_536 and then Scale (Item, Class) = Item * 1_536);
         when Bytes_1792 => pragma Assert (Stride (Class) = 1_792 and then Scale (Item, Class) = Item * 1_792);
         when Bytes_2048 => pragma Assert (Stride (Class) = 2_048 and then Scale (Item, Class) = Item * 2_048);
         when Bytes_2560 => pragma Assert (Stride (Class) = 2_560 and then Scale (Item, Class) = Item * 2_560);
         when Bytes_3072 => pragma Assert (Stride (Class) = 3_072 and then Scale (Item, Class) = Item * 3_072);
         when Bytes_3584 => pragma Assert (Stride (Class) = 3_584 and then Scale (Item, Class) = Item * 3_584);
         when Bytes_4096 => pragma Assert (Stride (Class) = 4_096 and then Scale (Item, Class) = Item * 4_096);
      end case;
   end Prove_Scale;

   procedure Prove_Start (Relative : Geometry_Offset; Class : Size_Class) is
   begin
      case Class is
         when Bytes_16 => pragma Assert (Stride (Class) = 16 and then Slots (Class) = 4_096 and then Relative = (Relative / 16) * 16);
         when Bytes_32 => pragma Assert (Stride (Class) = 32 and then Slots (Class) = 2_048 and then Relative = (Relative / 32) * 32);
         when Bytes_48 => pragma Assert (Stride (Class) = 48 and then Slots (Class) = 1_365 and then Relative = (Relative / 48) * 48);
         when Bytes_64 => pragma Assert (Stride (Class) = 64 and then Slots (Class) = 1_024 and then Relative = (Relative / 64) * 64);
         when Bytes_80 => pragma Assert (Stride (Class) = 80 and then Slots (Class) = 819 and then Relative = (Relative / 80) * 80);
         when Bytes_96 => pragma Assert (Stride (Class) = 96 and then Slots (Class) = 682 and then Relative = (Relative / 96) * 96);
         when Bytes_112 => pragma Assert (Stride (Class) = 112 and then Slots (Class) = 585 and then Relative = (Relative / 112) * 112);
         when Bytes_128 => pragma Assert (Stride (Class) = 128 and then Slots (Class) = 512 and then Relative = (Relative / 128) * 128);
         when Bytes_160 => pragma Assert (Stride (Class) = 160 and then Slots (Class) = 409 and then Relative = (Relative / 160) * 160);
         when Bytes_192 => pragma Assert (Stride (Class) = 192 and then Slots (Class) = 341 and then Relative = (Relative / 192) * 192);
         when Bytes_224 => pragma Assert (Stride (Class) = 224 and then Slots (Class) = 292 and then Relative = (Relative / 224) * 224);
         when Bytes_256 => pragma Assert (Stride (Class) = 256 and then Slots (Class) = 256 and then Relative = (Relative / 256) * 256);
         when Bytes_320 => pragma Assert (Stride (Class) = 320 and then Slots (Class) = 204 and then Relative = (Relative / 320) * 320);
         when Bytes_384 => pragma Assert (Stride (Class) = 384 and then Slots (Class) = 170 and then Relative = (Relative / 384) * 384);
         when Bytes_448 => pragma Assert (Stride (Class) = 448 and then Slots (Class) = 146 and then Relative = (Relative / 448) * 448);
         when Bytes_512 => pragma Assert (Stride (Class) = 512 and then Slots (Class) = 128 and then Relative = (Relative / 512) * 512);
         when Bytes_640 => pragma Assert (Stride (Class) = 640 and then Slots (Class) = 102 and then Relative = (Relative / 640) * 640);
         when Bytes_768 => pragma Assert (Stride (Class) = 768 and then Slots (Class) = 85 and then Relative = (Relative / 768) * 768);
         when Bytes_896 => pragma Assert (Stride (Class) = 896 and then Slots (Class) = 73 and then Relative = (Relative / 896) * 896);
         when Bytes_1024 => pragma Assert (Stride (Class) = 1_024 and then Slots (Class) = 64 and then Relative = (Relative / 1_024) * 1_024);
         when Bytes_1280 => pragma Assert (Stride (Class) = 1_280 and then Slots (Class) = 51 and then Relative = (Relative / 1_280) * 1_280);
         when Bytes_1536 => pragma Assert (Stride (Class) = 1_536 and then Slots (Class) = 42 and then Relative = (Relative / 1_536) * 1_536);
         when Bytes_1792 => pragma Assert (Stride (Class) = 1_792 and then Slots (Class) = 36 and then Relative = (Relative / 1_792) * 1_792);
         when Bytes_2048 => pragma Assert (Stride (Class) = 2_048 and then Slots (Class) = 32 and then Relative = (Relative / 2_048) * 2_048);
         when Bytes_2560 => pragma Assert (Stride (Class) = 2_560 and then Slots (Class) = 25 and then Relative = (Relative / 2_560) * 2_560);
         when Bytes_3072 => pragma Assert (Stride (Class) = 3_072 and then Slots (Class) = 21 and then Relative = (Relative / 3_072) * 3_072);
         when Bytes_3584 => pragma Assert (Stride (Class) = 3_584 and then Slots (Class) = 18 and then Relative = (Relative / 3_584) * 3_584);
         when Bytes_4096 => pragma Assert (Stride (Class) = 4_096 and then Slots (Class) = 16 and then Relative = (Relative / 4_096) * 4_096);
      end case;
   end Prove_Start;

   procedure Prove_Order (I, J : Natural; Class : Size_Class) is
   begin
      case Class is
         when Bytes_16 => pragma Assert (Stride (Class) = 16 and then I * 16 + 16 <= J * 16);
         when Bytes_32 => pragma Assert (Stride (Class) = 32 and then I * 32 + 32 <= J * 32);
         when Bytes_48 => pragma Assert (Stride (Class) = 48 and then I * 48 + 48 <= J * 48);
         when Bytes_64 => pragma Assert (Stride (Class) = 64 and then I * 64 + 64 <= J * 64);
         when Bytes_80 => pragma Assert (Stride (Class) = 80 and then I * 80 + 80 <= J * 80);
         when Bytes_96 => pragma Assert (Stride (Class) = 96 and then I * 96 + 96 <= J * 96);
         when Bytes_112 => pragma Assert (Stride (Class) = 112 and then I * 112 + 112 <= J * 112);
         when Bytes_128 => pragma Assert (Stride (Class) = 128 and then I * 128 + 128 <= J * 128);
         when Bytes_160 => pragma Assert (Stride (Class) = 160 and then I * 160 + 160 <= J * 160);
         when Bytes_192 => pragma Assert (Stride (Class) = 192 and then I * 192 + 192 <= J * 192);
         when Bytes_224 => pragma Assert (Stride (Class) = 224 and then I * 224 + 224 <= J * 224);
         when Bytes_256 => pragma Assert (Stride (Class) = 256 and then I * 256 + 256 <= J * 256);
         when Bytes_320 => pragma Assert (Stride (Class) = 320 and then I * 320 + 320 <= J * 320);
         when Bytes_384 => pragma Assert (Stride (Class) = 384 and then I * 384 + 384 <= J * 384);
         when Bytes_448 => pragma Assert (Stride (Class) = 448 and then I * 448 + 448 <= J * 448);
         when Bytes_512 => pragma Assert (Stride (Class) = 512 and then I * 512 + 512 <= J * 512);
         when Bytes_640 => pragma Assert (Stride (Class) = 640 and then I * 640 + 640 <= J * 640);
         when Bytes_768 => pragma Assert (Stride (Class) = 768 and then I * 768 + 768 <= J * 768);
         when Bytes_896 => pragma Assert (Stride (Class) = 896 and then I * 896 + 896 <= J * 896);
         when Bytes_1024 => pragma Assert (Stride (Class) = 1_024 and then I * 1_024 + 1_024 <= J * 1_024);
         when Bytes_1280 => pragma Assert (Stride (Class) = 1_280 and then I * 1_280 + 1_280 <= J * 1_280);
         when Bytes_1536 => pragma Assert (Stride (Class) = 1_536 and then I * 1_536 + 1_536 <= J * 1_536);
         when Bytes_1792 => pragma Assert (Stride (Class) = 1_792 and then I * 1_792 + 1_792 <= J * 1_792);
         when Bytes_2048 => pragma Assert (Stride (Class) = 2_048 and then I * 2_048 + 2_048 <= J * 2_048);
         when Bytes_2560 => pragma Assert (Stride (Class) = 2_560 and then I * 2_560 + 2_560 <= J * 2_560);
         when Bytes_3072 => pragma Assert (Stride (Class) = 3_072 and then I * 3_072 + 3_072 <= J * 3_072);
         when Bytes_3584 => pragma Assert (Stride (Class) = 3_584 and then I * 3_584 + 3_584 <= J * 3_584);
         when Bytes_4096 => pragma Assert (Stride (Class) = 4_096 and then I * 4_096 + 4_096 <= J * 4_096);
      end case;
   end Prove_Order;
end Heap_Classes;
