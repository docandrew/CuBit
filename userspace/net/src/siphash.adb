------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body SipHash with SPARK_Mode is

   --  SipHash-c-d with c = 2 compression and d = 4 finalization rounds.
   Compression_Rounds  : constant := 2;
   Finalization_Rounds : constant := 4;
   Word_Bytes          : constant := 8;
   --  Initial state: "somepseudorandomlygeneratedbytes" (the paper, 2.2).
   Init_V0 : constant := 16#736f6d6570736575#;
   Init_V1 : constant := 16#646f72616e646f6d#;
   Init_V2 : constant := 16#6c7967656e657261#;
   Init_V3 : constant := 16#7465646279746573#;
   Finalization_Mark : constant := 16#ff#;
   --  The last word carries the message length's low byte on top.
   Length_Shift   : constant := 8 * (Word_Bytes - 1);
   Length_Modulus : constant := 2 ** 8;

   type State is record
      V0, V1, V2, V3 : Unsigned_64;
   end record;

   --  SipRound; its rotation amounts are the algorithm's definition.
   procedure Round (S : in out State) is
   begin
      S.V0 := S.V0 + S.V1; S.V1 := Rotate_Left (S.V1, 13); S.V1 := S.V1 xor S.V0;
      S.V0 := Rotate_Left (S.V0, 32);
      S.V2 := S.V2 + S.V3; S.V3 := Rotate_Left (S.V3, 16); S.V3 := S.V3 xor S.V2;
      S.V0 := S.V0 + S.V3; S.V3 := Rotate_Left (S.V3, 21); S.V3 := S.V3 xor S.V0;
      S.V2 := S.V2 + S.V1; S.V1 := Rotate_Left (S.V1, 17); S.V1 := S.V1 xor S.V2;
      S.V2 := Rotate_Left (S.V2, 32);
   end Round;

   --  Little-endian word from Count (<= Word_Bytes) bytes starting at First.
   function Word (B : Byte_Array; First : Natural; Count : Natural) return Unsigned_64
   with Pre => Count <= Word_Bytes and then
               (Count = 0 or else
                  (First >= B'First and then First <= B'Last and then
                   B'Last - First >= Count - 1))
   is
      W : Unsigned_64 := 0;
   begin
      for I in 0 .. Count - 1 loop
         W := W or Shift_Left (Unsigned_64 (B (First + I)), 8 * I);
      end loop;
      return W;
   end Word;

   procedure Compress (S : in out State; M : Unsigned_64) is
   begin
      S.V3 := S.V3 xor M;
      for R in 1 .. Compression_Rounds loop
         Round (S);
      end loop;
      S.V0 := S.V0 xor M;
   end Compress;

   function Hash (K : Key; Message : Byte_Array) return Unsigned_64 is
      S : State :=
        (V0 => K.K0 xor Init_V0,
         V1 => K.K1 xor Init_V1,
         V2 => K.K0 xor Init_V2,
         V3 => K.K1 xor Init_V3);
      Blocks : constant Natural := Message'Length / Word_Bytes;
      Tail   : constant Natural := Message'Length mod Word_Bytes;
   begin
      for I in 0 .. Blocks - 1 loop
         Compress (S, Word (Message, Message'First + Word_Bytes * I, Word_Bytes));
      end loop;
      Compress (S, Shift_Left (Unsigned_64 (Message'Length mod Length_Modulus), Length_Shift) or
                   Word (Message, Message'First + Word_Bytes * Blocks, Tail));
      S.V2 := S.V2 xor Finalization_Mark;
      for R in 1 .. Finalization_Rounds loop
         Round (S);
      end loop;
      return S.V0 xor S.V1 xor S.V2 xor S.V3;
   end Hash;

   function To_Key (Bytes : Byte_Array) return Key is
     ((K0 => Word (Bytes, Bytes'First, Word_Bytes),
       K1 => Word (Bytes, Bytes'First + Word_Bytes, Word_Bytes)));
end SipHash;
