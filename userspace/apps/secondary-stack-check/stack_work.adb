------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
pragma Warnings (Off, "internal GNAT unit");
with System.Secondary_Stack;
pragma Warnings (On, "internal GNAT unit");
with CuBit.Secondary_Stack_Layout;
with CuBit.Thread_Areas;

package body Stack_Work is

   package SS renames System.Secondary_Stack;

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   Word_Bytes : constant := 8;
   type Frame_Words is array (1 .. Frame_Bytes / Word_Bytes) of Unsigned_64;

   --  Distinct per seed and position; never all one value.
   Byte_Prime : constant := 251;
   Seed_Step  : constant := 131;
   Index_Step : constant := 7;
   Word_Mix   : constant := 16#9E37_79B9_7F4A_7C15#;

   function Value_Of (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Where)));

   function Byte_At (Seed : Unsigned_64; Index : Positive) return Unsigned_8 is
     (Unsigned_8 ((Seed * Seed_Step + Unsigned_64 (Index) * Index_Step) mod Byte_Prime));

   function Word_At (Seed : Unsigned_64; Index : Positive) return Unsigned_64 is
     ((Seed + Unsigned_64 (Index)) * Word_Mix);

   --  An unconstrained result: the callee builds it on the secondary stack.
   function Pattern (Seed : Unsigned_64; Length : Natural) return Byte_Array
   with No_Inline;
   function Pattern (Seed : Unsigned_64; Length : Natural) return Byte_Array is
   begin
      return Result : Byte_Array (1 .. Length) do
         for I in Result'Range loop
            Result (I) := Byte_At (Seed, I);
         end loop;
      end return;
   end Pattern;

   --  Two results, joined into a third on the secondary stack.
   function Join (Left, Right : Byte_Array) return Byte_Array with No_Inline;
   function Join (Left, Right : Byte_Array) return Byte_Array is
   begin
      return Left & Right;
   end Join;

   function Matches (Item : Byte_Array; Seed : Unsigned_64) return Boolean is
     (for all I in Item'Range => Item (I) = Byte_At (Seed, I - Item'First + 1));

   --  At the deepest frame: results on the secondary stack, checked, and
   --  where they and the thread's stack region are.
   function Secondary (Seed : Unsigned_64; Found : in out Outcome) return Boolean
   with No_Inline;
   function Secondary (Seed : Unsigned_64; Found : in out Outcome) return Boolean is
      Region : constant Unsigned_64 :=
        Value_Of (SS.SS_Region (CuBit.Thread_Areas.Current_Stack));
      First : Byte_Array renames Pattern (Seed, Result_Bytes);
      Both : Byte_Array renames
        Join (Pattern (Seed + 1, Result_Bytes), Pattern (Seed + 2, Result_Bytes));
   begin
      Found.Result_Address := Value_Of (Both'Address);
      Found.Region := Region;
      return First'Length = Result_Bytes
        and then Both'Length = 2 * Result_Bytes
        and then Matches (First, Seed)
        and then Matches (Both (Both'First .. Both'First + Result_Bytes - 1), Seed + 1)
        and then Matches (Both (Both'First + Result_Bytes .. Both'Last), Seed + 2)
        and then Inside (Value_Of (First'Address), Region,
                         CuBit.Secondary_Stack_Layout.Reservation_Bytes)
        and then Inside (Found.Result_Address, Region,
                         CuBit.Secondary_Stack_Layout.Reservation_Bytes);
   end Secondary;

   --  One primary-stack frame of Frame_Bytes, filled, then checked after
   --  the deeper frames and the secondary-stack work have run. Volatile so
   --  the frame really is written and read.
   function Deep (Level : Natural; Seed : Unsigned_64; Found : in out Outcome) return Boolean
   with No_Inline;
   function Deep (Level : Natural; Seed : Unsigned_64; Found : in out Outcome) return Boolean is
      Frame : Frame_Words with Volatile;
      Frame_Seed : constant Unsigned_64 := Seed + Unsigned_64 (Level) * Frame_Bytes;
      Intact : Boolean;
   begin
      for I in Frame'Range loop
         Frame (I) := Word_At (Frame_Seed, I);
      end loop;
      Intact := (if Level = 0 then Secondary (Seed, Found) else Deep (Level - 1, Seed, Found));
      for I in Frame'Range loop
         if Frame (I) /= Word_At (Frame_Seed, I) then
            Intact := False;
         end if;
      end loop;
      return Intact;
   end Deep;

   function Run (Seed : Unsigned_64) return Outcome is
      Found : Outcome;
   begin
      Found.Passed := Deep (Frames - 1, Seed, Found);
      return Found;
   end Run;

end Stack_Work;
