------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Bounded little-endian reads over an untrusted byte image (a debug file,
--  bytes read from the target): every read checks its bounds and reports
--  failure instead of reading past the end.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Debug_Target;

package CuBit.Debug.Bytes with SPARK_Mode is

   subtype Offset is Unsigned_64;
   subtype Byte_Buffer is CuBit.Debug_Target.Byte_Buffer;
   subtype Buffer_Access is CuBit.Debug_Target.Buffer_Access;

   --  A position in Data, and whether every read so far stayed in bounds.
   type Cursor is record
      At_Byte : Offset := 0;
      Limit   : Offset := 0;   --  one past the last readable byte
      Ok      : Boolean := True;
   end record;

   function Make (First, Limit : Offset) return Cursor is
     ((At_Byte => First, Limit => Limit, Ok => First <= Limit));

   function Remaining (C : Cursor) return Offset is
     (if C.Ok and then C.At_Byte <= C.Limit then C.Limit - C.At_Byte else 0);

   --  Each read: on overrun C.Ok becomes False and the result is 0.
   procedure U8  (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64);
   procedure U16 (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64);
   procedure U32 (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64);
   procedure U64 (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64);
   procedure Sized (Data : Byte_Buffer; C : in out Cursor; Size : Natural; V : out Unsigned_64);
   procedure ULEB (Data : Byte_Buffer; C : in out Cursor; V : out Unsigned_64);
   procedure SLEB (Data : Byte_Buffer; C : in out Cursor; V : out Integer_64);
   procedure Skip (C : in out Cursor; Count : Offset);
   --  Past a NUL-terminated string.
   procedure Skip_String (Data : Byte_Buffer; C : in out Cursor);

   --  The NUL-terminated string at At_Byte (at most Max bytes), as text.
   function C_String (Data : Byte_Buffer; At_Byte : Offset; Max : Positive) return String;

   function Sign_Extend (V : Unsigned_64; Bits : Positive) return Integer_64;
   function To_Signed (V : Unsigned_64) return Integer_64;
   function To_Unsigned (V : Integer_64) return Unsigned_64;

end CuBit.Debug.Bytes;
