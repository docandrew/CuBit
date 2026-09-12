with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Legacy; use XHCI_Legacy;

procedure Legacy_Tests is
begin
   pragma Assert (First_Offset (16#0020_0001#) = 16#80#);
   pragma Assert (First_Offset (16#FFFF_FFFF#) = 16#3FFFC#);
   pragma Assert (First_Offset (0) = 0);
   pragma Assert (Next_Distance (16#0101_0801#) = 32);
   pragma Assert (Next_Distance (16#0101_0001#) = 0);
   pragma Assert (Next_Distance (16#FFFF_FFFF#) = 1020);
   pragma Assert (Fits (16#80#, 8, 4096));
   pragma Assert (Fits (4088, 8, 4096));
   pragma Assert (Fits (4092, 4, 4096));
   pragma Assert (not Fits (4092, 8, 4096));
   pragma Assert (not Fits (16#81#, 8, 4096));
   pragma Assert (not Fits (0, 8, 4096));
   pragma Assert (not Fits (32, 8, 4));
   pragma Assert (not Fits (Unsigned_64'Last, 8, 4096));
   pragma Assert (Disable_SMIs (0) = 16#E000_0000#);
   pragma Assert (Disable_SMIs (16#FFFF_FFFF#) = 16#E00E_1FEE#);
   for Bit in 0 .. 31 loop
      declare
         Input : constant Unsigned_32 := Shift_Left (1, Bit);
         Output : constant Unsigned_32 := Disable_SMIs (Input);
      begin
         --  No SMI enable or reserved-zero status bits are echoed.
         pragma Assert ((Output and 16#1FF1_E011#) = 0);
         pragma Assert ((Output and 16#000E_1FEE#) = (Input and 16#000E_1FEE#));
         pragma Assert ((Output and 16#E000_0000#) = 16#E000_0000#);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("XHCI-LEGACY: PASS capability bounds and SMI controls");
end Legacy_Tests;
