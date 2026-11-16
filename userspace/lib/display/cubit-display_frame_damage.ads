------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A pool frame's damage boxes (LAT-002 multi-rect submit).
--
--  @description
--  An IPC message has four words (CuBit.Messages), all used by a
--  Submit_Frame: the session, the frame, the boxes' bounding area, and the
--  plan epoch of a plane frame (CuBit.Display_Pool_Protocol,
--  CuBit.Display_Plane_Protocol). The boxes themselves are a record
--  in the frame's own buffer, in the page after its pixel rows: written by
--  the compositor before it submits the buffer and immutable, like the
--  pixels, until display.svc releases it (no shared mutable list). The
--  record names its frame, so a stale record from an earlier frame of the
--  same buffer is refused. display.svc copies only these boxes.
--
--  Decode validates everything the service relies on: the magic, the
--  frame, a count of 1 .. Maximum_Boxes, and every box nonempty and inside
--  the frame's announced area. A refused record means the bounding area is
--  copied instead, which is always correct (the area is validated
--  against the buffer by the service).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol;
package CuBit.Display_Frame_Damage with Pure, SPARK_Mode is
   package DP renames CuBit.Desktop_Protocol;
   use type DP.Pixel_Coordinate, DP.Pixel_Extent;

   --  At most this many boxes per frame: the compositor's damage ceiling
   --  (Compositor_Damage.Capacity; Desktop checks they agree).
   Maximum_Boxes : constant := 8;
   subtype Box_Count is Natural range 0 .. Maximum_Boxes;
   subtype Box_Index is Positive range 1 .. Maximum_Boxes;
   --  The record's room: one page after the pixel rows.
   Record_Bytes : constant := 4096;
   --  "DMGBOXES" in ASCII: the record's first word.
   Magic : constant Unsigned_64 := 16#5345_584F_4247_4D44#;
   --  Magic, frame, count, then one packed box per word.
   Header_Words : constant := 3;
   Record_Words : constant := Header_Words + Maximum_Boxes;
   pragma Compile_Time_Error (Record_Words * 8 > Record_Bytes, "the record must fit its page");
   subtype Word_Index is Natural range 0 .. Record_Words - 1;
   type Word_Array is array (Word_Index) of Unsigned_64;

   type Box_Array is array (Box_Index) of DP.Rectangle;
   type Damage is record
      Count : Box_Count := 0;
      Boxes : Box_Array := [others => (others => <>)];
   end record;

   function Nonempty (B : DP.Rectangle) return Boolean is
     (B.Width > 0 and then B.Height > 0);
   --  B lies inside Area.
   function Inside (B, Area : DP.Rectangle) return Boolean is
     (Natural (B.X) >= Natural (Area.X) and then Natural (B.Y) >= Natural (Area.Y) and then
      Natural (B.X) + Natural (B.Width) <= Natural (Area.X) + Natural (Area.Width) and then
      Natural (B.Y) + Natural (B.Height) <= Natural (Area.Y) + Natural (Area.Height));

   function Pack (B : DP.Rectangle) return Unsigned_64 is
     (Unsigned_64 (B.X) + Unsigned_64 (B.Y) * 2 ** 16 +
      Unsigned_64 (B.Width) * 2 ** 32 + Unsigned_64 (B.Height) * 2 ** 48);
   function Unpack (W : Unsigned_64) return DP.Rectangle is
     ((DP.Pixel_Coordinate (W mod 2 ** 16), DP.Pixel_Coordinate (W / 2 ** 16 mod 2 ** 16),
       DP.Pixel_Extent (W / 2 ** 32 mod 2 ** 16), DP.Pixel_Extent (W / 2 ** 48)));

   function Encode (Frame : Unsigned_64; Item : Damage) return Word_Array
     with Post => Encode'Result (0) = Magic and Encode'Result (1) = Frame and
       Encode'Result (2) = Unsigned_64 (Item.Count) and
       (for all I in 1 .. Item.Count =>
          Encode'Result (Header_Words + I - 1) = Pack (Item.Boxes (I)));

   type Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Damage;
         when False => null;
      end case;
   end record;
   --  Words is a value: never pass a record another process can still
   --  write (an overlay of its page); decode a snapshot of it (Shared).
   function Decode (Words : Word_Array; Frame : Unsigned_64; Area : DP.Rectangle) return Decoding
     with Post => (if Decode'Result.Valid then
       Decode'Result.Value.Count >= 1 and
       Unsigned_64 (Decode'Result.Value.Count) = Words (2) and
       (for all I in 1 .. Decode'Result.Value.Count =>
          Nonempty (Decode'Result.Value.Boxes (I)) and
          Inside (Decode'Result.Value.Boxes (I), Area)));
end CuBit.Display_Frame_Damage;
