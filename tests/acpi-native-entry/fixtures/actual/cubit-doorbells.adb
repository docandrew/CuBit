------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages;

package body CuBit.Doorbells is

   package KC renames CuBit.Kernel_Calls;
   package KA renames CuBit.Kernel_ABI;

   function Tag_Word is new Ada.Unchecked_Conversion (CuBit.Messages.MessageTag, Unsigned_64);

   --  SEND_EVENT's destination and authority are not used by a wake.
   No_Destination : constant Unsigned_64 := 0;

   Wake_Tag : constant CuBit.Messages.MessageTag :=
     (label => ABI.Wake_Label, length => ABI.Wake_Length, protocol => 0, ipc => <>, reserved => 0);

   Sync_Tag : constant CuBit.Messages.MessageTag :=
     (label => ABI.Wake_Label, length => ABI.Sync_Length, protocol => 0, ipc => <>, reserved => 0);

   function Ring (Grant : GR.Reference; Sync : Boolean) return Boolean is
     (KC.Call (KA.Send_Event, No_Destination, Tag_Word (if Sync then Sync_Tag else Wake_Tag),
               GR.Encode (Grant), (if Sync then ABI.Sync_Hint else 0))
      = ABI.Accepted_Word);

   procedure Decode
     (Label : Unsigned_32; Length : Unsigned_8; Words : Event_Words;
      Is_Wake : out Boolean; Into : out Bell_Array; Count : out Wake_Count)
   is
      Kept : Natural := 0;
   begin
      Into := [others => (slot => 0, generation => 1)];
      Count := 0;
      Is_Wake := Label = ABI.Wake_Label and then Natural (Length) in 1 .. ABI.Wakes_Per_Event;
      if not Is_Wake then
         return;
      end if;
      for I in 0 .. Natural (Length) - 1 loop
         if GR.Valid_Wire (Words (I)) then
            Kept := Kept + 1;
            Into (Kept) := GR.Decode (Words (I));
         end if;
      end loop;
      Count := Kept;
   end Decode;

end CuBit.Doorbells;
