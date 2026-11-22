with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADS_Engines;
with Intel_GPU_ADS_System_Info; use Intel_GPU_ADS_System_Info;
procedure ADS_System_Info_Tests is
   Description : Inventory;
   Topology : Intel_GPU_ADLN_Steering.Topology := Intel_GPU_ADLN_Steering.Decode (1, 4, 0);
   Value : System_Info;
   Prefix : Intel_GPU_ADS_Engines.Encoding;
   Raw : Unsigned_32;
   Expected : Info_Bytes;
begin
   for Disabled in Unsigned_32 range 0 .. 7 loop
      Description := Decode (16#8086#, 16#46D2#,
        (Disabled and 1) or Shift_Left (Disabled and 2, 1) or Shift_Left (Disabled and 4, 14));
      Prefix := Intel_GPU_ADS_Engines.Encode (Description);
      for Encoded_Count in Unsigned_32 range 0 .. 255 loop
         Raw := Encoded_Count * 65536 + 16#A5000001#;
         Value := Build (Description, Topology, Raw, Raw);
         Expected := [others => 0];
         for I in Prefix.Bytes'Range loop Expected (I) := Prefix.Bytes (I); end loop;
         Expected (576) := 1;
         Expected (580) := (if (Disabled and 1) = 0 then 1 else 0) +
           (if (Disabled and 2) = 0 then 4 else 0);
         Expected (584) := Unsigned_8 ((Encoded_Count + 1) mod 256);
         Expected (585) := Unsigned_8 ((Encoded_Count + 1) / 256);
         pragma Assert (Value.Valid and Value.Bytes = Expected);
         pragma Assert (not Build (Description, Topology, Raw, Raw xor 1).Valid);
      end loop;
   end loop;
   pragma Assert (not Build (Description, Topology, Unsigned_32'Last, Unsigned_32'Last).Valid);
   Topology.Valid := False;
   pragma Assert (not Build (Description, Topology, 0, 0).Valid);
   Ada.Text_IO.Put_Line ("ADS system info: 2048 inventory/doorbell combinations and rejected samples PASS");
end ADS_System_Info_Tests;
