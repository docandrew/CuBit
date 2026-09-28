with Interfaces; use Interfaces;
with Intel_GPU_PCI_Power; use Intel_GPU_PCI_Power;
procedure PCI_Power_Tests is
   Data : Configuration := [others => 0];
   Expected : constant array (Unsigned_8 range 0 .. 3) of Power_Status :=
     [D0, D1, D2, D3_Hot];
begin
   pragma Assert (Decode (Data) = Unavailable);
   Data (6) := 16#10#;
   for Position in Unsigned_8 loop
      Data (16#34#) := Position;
      pragma Assert (Decode (Data) =
        (if Position = 0 or else (Position >= 16#40# and Position mod 4 = 0)
         then Unavailable else Malformed));
   end loop;
   Data (16#34#) := 16#40#;
   Data (16#40#) := 1;
   Data (16#42#) := 3;
   for Value in Unsigned_8 loop
      Data (16#44#) := Value;
      pragma Assert (Decode (Data) = Expected (Value and 3));
   end loop;
   Data (16#41#) := 16#40#;
   pragma Assert (Decode (Data) = Malformed);
   Data (16#44#) := 0;
   Data (16#41#) := 16#44#;
   pragma Assert (Decode (Data) = Malformed);
   Data (16#41#) := 16#48#;
   Data (16#48#) := 1;
   pragma Assert (Decode (Data) = Malformed);
   Data := [others => 0];
   Data (6) := 16#10#;
   Data (16#34#) := 16#FC#;
   Data (16#FC#) := 1;
   pragma Assert (Decode (Data) = Malformed);
   Data := [others => 0];
   Data (6) := 16#10#;
   Data (16#34#) := 16#40#;
   for Position in 16#40# .. 16#F8# loop
      if Position mod 4 = 0 then
         Data (Position + 1) := Unsigned_8 (Position + 4);
      end if;
   end loop;
   pragma Assert (Decode (Data) = Unavailable);
   Data (16#FD#) := 16#40#;
   pragma Assert (Decode (Data) = Malformed);
end PCI_Power_Tests;
