with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Capabilities; use XHCI_Capabilities;

procedure Capability_Tests is
begin
   --  Literal register fixtures expose swapped significance independently
   --  of the encoder used for exhaustive tests below.
   pragma Assert (Scratchpad_Count (16#0000_0000#) = 0);
   pragma Assert (Scratchpad_Count (16#0800_0000#) = 1);
   pragma Assert (Scratchpad_Count (16#8000_0000#) = 16);
   pragma Assert (Scratchpad_Count (16#8800_0000#) = 17);
   pragma Assert (Scratchpad_Count (16#F800_0000#) = 31);
   pragma Assert (Scratchpad_Count (16#0020_0000#) = 32);
   pragma Assert (Scratchpad_Count (16#0820_0000#) = 33);
   pragma Assert (Scratchpad_Count (16#FFFF_FFFF#) = 1023);

   for Count in Scratchpad_Buffer_Count loop
      declare
         Encoded : constant Unsigned_32 :=
           Unsigned_32 (Count mod 32) * 2**27 +
           Unsigned_32 (Count / 32) * 2**21;
      begin
         pragma Assert (Scratchpad_Count (Encoded) = Count);
         --  Restore flag, other capability fields, and reserved bits must
         --  not contribute to the scratchpad buffer count.
         pragma Assert (Scratchpad_Count (Encoded or 16#041F_FFFF#) = Count);
         for Bit in 0 .. 20 loop
            pragma Assert
              (Scratchpad_Count (Encoded or Shift_Left (1, Bit)) = Count);
         end loop;
         pragma Assert
           (Scratchpad_Count (Encoded or 16#0400_0000#) = Count);
      end;
   end loop;
   Ada.Text_IO.Put_Line
     ("XHCI-CAPABILITIES: PASS all 1024 scratchpad counts and unrelated bits");
end Capability_Tests;
