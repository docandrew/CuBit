with Interfaces; use Interfaces;
with Intel_GPU_PCI_Power;
with Intel_GPU_PCI_Interrupts; use Intel_GPU_PCI_Interrupts;
procedure PCI_Interrupt_Tests is
   Data : Intel_GPU_PCI_Power.Configuration;
   Value : Snapshot;
begin
   for Flags in Unsigned_8 loop
      pragma Assert (Encoding_Valid (Flags) =
        (Flags = 0 or else
          ((Flags and 16#81#) = 1 and
           (Flags and 12) /= 8 and
           ((Flags and 16#70#) = 0 or (Flags and 16#10#) /= 0))));
      if Encoding_Valid (Flags) then
         pragma Assert (Pack (Unpack (Flags)) = Flags);
      end if;
      Data := [others => 0];
      Data (5) := Flags;
      Value := Decode (Data);
      pragma Assert (Value.Valid and (Value.INTx_Disabled = ((Flags and 4) /= 0)));
      Data (6) := 16#10#;
      Data (16#34#) := 16#40#;
      Data (16#40#) := 5;
      Data (16#41#) := 16#60#;
      Data (16#42#) := Flags;
      Data (16#43#) := Flags;
      Data (16#60#) := 16#11#;
      Data (16#63#) := Flags;
      Value := Decode (Data);
      pragma Assert (Value.Valid and Value.MSI_Present and Value.MSIX_Present);
      pragma Assert (Value.MSI_Enabled = ((Flags and 1) /= 0));
      pragma Assert (Value.MSIX_Enabled = ((Flags and 16#80#) /= 0));
      pragma Assert (Value.MSIX_Masked = ((Flags and 16#40#) /= 0));
      pragma Assert (Unpack (Pack (Value)) = Value);
      Data (16#61#) := 16#40#;
      pragma Assert (not Decode (Data).Valid);
      Data (16#61#) := 0;
      Data (16#60#) := 5;
      pragma Assert (not Decode (Data).Valid);
   end loop;
   for Position in Unsigned_8 loop
      Data := [others => 0];
      Data (6) := 16#10#;
      Data (16#34#) := Position;
      pragma Assert (Decode (Data).Valid =
        (Position = 0 or else (Position >= 16#40# and Position mod 4 = 0)));
      if Position >= 16#40# and Position mod 4 = 0 then
         Data (Natural (Position)) := 16#11#;
         pragma Assert (Decode (Data).Valid = (Position <= 16#F4#));
         Data (Natural (Position)) := 5;
         for Wide in 0 .. 1 loop
            for Masked in 0 .. 1 loop
               Data (Natural (Position) + 2) := Unsigned_8 (Wide * 128);
               Data (Natural (Position) + 3) := Unsigned_8 (Masked);
               pragma Assert (Decode (Data).Valid =
                 (Natural (Position) + 10 + Wide * 4 + Masked * 10 <= 256));
            end loop;
         end loop;
      end if;
   end loop;
   -- Forward and backward links into a recognized capability's payload.
   Data := [others => 0];
   Data (6) := 16#10#;
   Data (16#34#) := 16#40#;
   Data (16#40#) := 16#11#;
   Data (16#41#) := 16#44#;
   pragma Assert (not Decode (Data).Valid);
   Data (16#34#) := 16#44#;
   Data (16#45#) := 16#40#;
   Data (16#41#) := 0;
   pragma Assert (not Decode (Data).Valid);
   Data := [others => 16#FF#];
   pragma Assert (not Decode (Data).Valid);
end PCI_Interrupt_Tests;
