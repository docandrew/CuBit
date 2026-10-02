with Interfaces; use Interfaces;
with Intel_GPU_ADS_Layout; use Intel_GPU_ADS_Layout;
with Ada.Text_IO;
with Intel_GPU_ADS_Backing;
procedure ADS_Layout_Tests is
   Value : Layout;
   Cursor : Unsigned_64;
   function Align (N : Unsigned_64) return Unsigned_64 is
     (((N + 4095) / 4096) * 4096);
begin
   pragma Assert (Intel_GPU_ADS_Backing.Capacity = 16_777_216);
   for Low in Unsigned_64 range 0 .. 4095 loop
      pragma Assert (Intel_GPU_ADS_Backing.Valid_Physical (4096 + Low) = (Low = 0));
      pragma Assert (Intel_GPU_ADS_Backing.Valid_Physical
        (2 ** 32 - Intel_GPU_ADS_Backing.Capacity + Low) = (Low = 0));
   end loop;
   pragma Assert (not Intel_GPU_ADS_Backing.Valid_Physical (0));
   pragma Assert (not Intel_GPU_ADS_Backing.Valid_Physical (Unsigned_64'Last));
   pragma Assert (Plan (4096, 262144, 4096, 65536, 16#801000#,
                        Intel_GPU_ADS_Backing.Capacity).Valid);
   for R in 0 .. 32 loop
      for G in 0 .. 32 loop
         Value := Plan (Unsigned_64 (R * 16), Unsigned_64 (G * 1023),
                        12, 4096, 16#801000#, Limit);
         pragma Assert (Value.Valid and Sound (Value));
         pragma Assert (Value.Offset (Policies) = 4572 and
                        Value.Offset (System_Info) = 4668 and
                        Value.Offset (Usage) = 5308 and
                        Value.Offset (Registers) = 21692);
         Cursor := Align (21692 + Unsigned_64 (R * 16));
         pragma Assert (Value.Offset (Golden_Contexts) = Cursor);
         Cursor := Align (Cursor + Unsigned_64 (G * 1023));
         pragma Assert (Value.Offset (Workarounds) = Cursor);
         Cursor := Align (Cursor + 12);
         pragma Assert (Value.Offset (Capture) = Cursor);
         Cursor := Align (Cursor + 4096);
         pragma Assert (Value.Offset (Private_Data) = Cursor);
         Cursor := Align (Cursor + 16#801000#);
         pragma Assert (Value.Total = Cursor);
         pragma Assert (Plan (Unsigned_64 (R * 16), Unsigned_64 (G * 1023),
                              12, 4096, 16#801000#, Cursor).Valid);
         pragma Assert (not Plan (Unsigned_64 (R * 16), Unsigned_64 (G * 1023),
                                  12, 4096, 16#801000#, Cursor - 1).Valid);
      end loop;
   end loop;
   for S in Section range Registers .. Private_Data loop
      declare
         Sizes : Extents := [others => 0];
      begin
         Sizes (S) := Unsigned_64'Last - 15;
         pragma Assert (not Plan (Sizes (Registers), Sizes (Golden_Contexts),
           Sizes (Workarounds), Sizes (Capture), Sizes (Private_Data), Limit).Valid);
      end;
   end loop;
   pragma Assert (not Plan (1, 0, 0, 0, 0, Limit).Valid);
   pragma Assert (not Plan (0, 0, 1, 0, 0, Limit).Valid);
   pragma Assert (not Plan (16, 4096, 4096, 4096, 16#801000#, 1024 * 1024).Valid);
   Ada.Text_IO.Put_Line ("PASS: 1089 ADS layouts/exact capacity boundaries and hostile sizes");
end ADS_Layout_Tests;
