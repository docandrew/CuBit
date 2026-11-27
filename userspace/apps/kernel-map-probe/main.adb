with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Owned_Reservations;
with CuBit.Process_Calls;

--  Kernel-map probe (KERN-014, docs/kernel-memory-map.md): writes a marker
--  into its own heap page, prints it, and asks a probe kernel
--  (KERN014_PROBE=1) to read that word through the kernel's linear map, as
--  a kernel read bug or a speculative gadget would. The headless test
--  kernel-map-probe compares the two: before KERN-014 unmaps process
--  memory the kernel reads the marker ("readable"); after, the read faults
--  ("unmapped").
procedure Main is
   use ASCII;
   Marker : constant Unsigned_64 := 16#4B45_524E_3031_3421#;
   --  Volatile: the marker must be in memory before the kernel looks.
   type Word_Array is array (Natural range <>) of Unsigned_64
     with Volatile_Components;
   type Word_Access is access Word_Array;
   --  Several pages, so the probed word is well inside one of them.
   Buffer : constant Word_Access := new Word_Array (0 .. 4 * 512 - 1);
   Probed : constant Natural := 2 * 512 + 8;

   function Hex (Value : Unsigned_64) return String is
      Digit_Text : constant String := "0123456789abcdef";
      Result : String (1 .. 16);
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digit_Text (Natural (V and 15) + 1);
         V := Shift_Right (V, 4);
      end loop;
      return Result;
   end Hex;
   procedure Request (Text : String) renames CuBit.Process_Calls.Write_Console;

   --  KERN-014 slice 2: the kernel stores into this process's memory
   --  through its own mapping; the hardware decides. Into a writable page
   --  (stored), a page made read-only (refused, unchanged), a reserved page
   --  with no frame (refused), the kernel half (refused before any access).
   --  The last two faults are recovered, and counted.
   procedure Kernel_Stores is
      package Owned renames CuBit.Owned_Reservations;
      Page    : constant Unsigned_64 := Owned.Page_Bytes;
      Written : constant Unsigned_64 := 16#5752_4954_5445_4E21#;
      Base    : constant Unsigned_64 := Owned.Reserve (2 * Page);
      Word : Unsigned_64 with Import, Volatile, Address => To_Address (Integer_Address (Base));
   begin
      if Base = 0 or else not Owned.Change (Base, Page, Owned.Commit) then
         debugPrint ("kernel-map-probe: FAIL owned memory" & LF);
         return;
      end if;
      Word := 0;
      Request ("KERN014-PROBE-WRITE " & Hex (Base));
      debugPrint ("kernel-map-probe: writable page " &
                  (if Word = Written then "holds the store" else "FAIL lacks the store") & LF);
      Word := 0;
      if not Owned.Change (Base, Page, Owned.Make_Read_Only) then
         debugPrint ("kernel-map-probe: FAIL read-only" & LF);
         return;
      end if;
      Request ("KERN014-PROBE-WRITE " & Hex (Base));
      debugPrint ("kernel-map-probe: read-only page " &
                  (if Word = 0 then "unchanged" else "FAIL changed") & LF);
      Request ("KERN014-PROBE-WRITE " & Hex (Base + Page));
      Request ("KERN014-PROBE-WRITE " & Hex (16#FFFF_8000_0010_0000#));
      Request ("KERN014-PROBE-FAULTS");
   end Kernel_Stores;
begin
   Buffer.all := (others => 0);
   Buffer (Probed) := Marker;
   debugPrint ("kernel-map-probe: marker 0x" & Hex (Marker) & LF);
   --  Straight to the kernel's console write, not debugPrint: a console
   --  sink would forward the text, and the kernel would read the
   --  forwarder's memory at this address.
   Request ("KERN014-PROBE-READ " & Hex (Unsigned_64 (To_Integer (Buffer (Probed)'Address))));
   if Buffer (Probed) /= Marker then
      debugPrint ("kernel-map-probe: FAIL marker changed" & LF);
   end if;
   Kernel_Stores;
   debugPrint ("kernel-map-probe: done" & LF);
end Main;
