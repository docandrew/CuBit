with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
procedure Inventory_Tests is
   Result : Inventory;
begin
   for Bits in Unsigned_32 range 0 .. 4095 loop
      Result := Decode (16#8086#, 16#46D2#,
        (Bits and 255) or Shift_Left (Shift_Right (Bits, 8), 16));
      pragma Assert (Result.Valid);
      pragma Assert (Result.Engines (Render) and Result.Engines (Copy));
      pragma Assert (Result.Engines (Video_0) = ((Bits and 1) = 0));
      pragma Assert (Result.Engines (Video_2) = ((Bits and 4) = 0));
      pragma Assert (Result.Engines (Enhance_0) = ((Bits and 256) = 0));
      pragma Assert (Result.Domains (GT) and Result.Domains (Render_Domain));
      pragma Assert (Result.Domains (VDBOX_0) = Result.Engines (Video_0));
      pragma Assert (Result.Domains (VDBOX_2) = Result.Engines (Video_2));
      pragma Assert (Result.Domains (VEBOX_0) = Result.Engines (Enhance_0));
      for E in Engine loop
         pragma Assert (Engine_Base (E) mod 4096 = 0 and Engine_Base (E) < 16#200000#);
         pragma Assert (Pending_Register (E) mod 4 = 0 and Pending_Register (E) / 4096 = 8);
         pragma Assert (Engine_Write_Allowed (Result, Engine_Base (E) + 16#9C#, 16#01000100#) = Result.Engines (E));
         pragma Assert (Engine_Write_Allowed (Result, Engine_Base (E) + 16#29C#, 16#04000400#) = Result.Engines (E));
         pragma Assert (Engine_Write_Allowed (Result, Engine_Base (E) + 16#D0#, 16#10001#) = Result.Engines (E));
         pragma Assert (Engine_Write_Allowed (Result, Engine_Base (E) + 16#D0#, 16#40004#) = Result.Engines (E));
         pragma Assert (Engine_Write_Allowed (Result, Engine_Base (E) + 16#D0#, 16#10000#) = Result.Engines (E));
         pragma Assert (not Engine_Write_Allowed (Result, Engine_Base (E) + 16#9C#, 16#01000000#));
         pragma Assert (not Engine_Write_Allowed (Result, Engine_Base (E) + 16#D0#, Unsigned_32'Last));
      end loop;
      pragma Assert (not Engine_Write_Allowed (Result, 16#941C#, 1));
   end loop;
   for ID in Unsigned_16 loop
      Result := Decode (16#8086#, ID, 0);
      pragma Assert (Result.Valid = (ID = 16#46D2#));
      if not Result.Valid then
         pragma Assert (Result.Engines = Engine_Set'[others => False]);
         pragma Assert (Result.Domains = Domain_Set'[others => False]);
      end if;
   end loop;
   Result := Decode (0, 16#46D2#, 0);
   pragma Assert (not Result.Valid);
   Result := Decode (16#8086#, 16#46D2#, Unsigned_32'Last);
   pragma Assert (not Result.Valid and Result.Domains = Domain_Set'[others => False]);
   for D in Domain loop
      pragma Assert (Request_Register (D) / 4096 = 10);
      pragma Assert (Request_Register (D) mod 4 = 0 and Ack_Register (D) mod 4 = 0);
      for Other in Domain loop
         if D /= Other then
            pragma Assert (Request_Register (D) /= Request_Register (Other));
            pragma Assert (Ack_Register (D) /= Ack_Register (Other));
         end if;
      end loop;
   end loop;
end Inventory_Tests;
