with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_L3; use Intel_GPU_ADLN_L3;
procedure L3_Inventory_Tests is
   function Raw is new Ada.Unchecked_Conversion (Allocation, Unsigned_32);
   function Raw is new Ada.Unchecked_Conversion (Parameters, Unsigned_32);
   function Raw is new Ada.Unchecked_Conversion (Fuse_Control, Unsigned_32);
   F : Fuse_Control := Decode_Fuse (16#F0#);
   P : Parameters := (Tagged_Ways => 88, Untagged_Ways => 32, others => <>);
   A : Allocation := Render_Allocation;
begin
   for Bit in 0 .. 31 loop
      declare V : constant Unsigned_32 := Shift_Left (1, Bit); begin
         pragma Assert (Encode (Decode_Allocation (V)) = V and Raw (Decode_Allocation (V)) = V);
         pragma Assert (Encode (Decode_Parameters (V)) = V and Raw (Decode_Parameters (V)) = V);
         pragma Assert (Encode (Decode_Fuse (V)) = V and Raw (Decode_Fuse (V)) = V);
      end;
   end loop;
   pragma Assert (Probe_URB_KiB (True, F, P, A) = 512);
   pragma Assert (Probe_URB_KiB (False, F, P, A) = 0);
   for Mask in 0 .. 255 loop
      F.Disabled_Banks := B8 (Mask);
      pragma Assert (Enabled_Banks (F) <= 8);
      pragma Assert (Probe_URB_KiB (True, F, P, A) = (if Mask = 16#F0# then 512 else 0));
   end loop;
   F := Decode_Fuse (16#F0#);
   F.Reserved_8 := B16'Last; F.Reserved_26 := B2'Last; F.Reserved_29 := B3'Last;
   P.Reserved_16 := B16'Last; A.Reserved_8 := B3'Last;
   pragma Assert (Probe_URB_KiB (True, F, P, A) = 512);
   A.Error_Status := 1; pragma Assert (Probe_URB_KiB (True, F, P, A) = 0);
   A.Error_Status := 0; F.Hash_Mode := 1;
   pragma Assert (Probe_URB_KiB (True, F, P, A) = 0);
   F.Hash_Mode := 0; F.WGBox_Configuration := 1;
   pragma Assert (Probe_URB_KiB (True, F, P, A) = 0);
   Ada.Text_IO.Put_Line ("L3 inventory PASS: representation fields, bank inventory, ownership/configuration denial, reserved-bit tolerance (encoding only)");
end L3_Inventory_Tests;
