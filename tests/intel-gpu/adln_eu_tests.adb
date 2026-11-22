with Interfaces; use Interfaces;
with Intel_GPU_ADLN_EU;
procedure ADLN_EU_Tests is
   package EU renames Intel_GPU_ADLN_EU;
begin
   for DSS in Unsigned_32 range 0 .. 63 loop
      for Fuse in Unsigned_32 range 0 .. 255 loop
         declare
            R : constant EU.Topology := EU.Decode (1, DSS, Fuse);
            Count_DSS, Count_EU : Natural := 0;
            Expected : Unsigned_16 := 0;
         begin
            for I in 0 .. 5 loop
               if (DSS / 2 ** I) mod 2 = 1 then Count_DSS := Count_DSS + 1; end if;
            end loop;
            for I in 0 .. 15 loop
               if (Fuse / 2 ** (I / 2)) mod 2 = 0 then
                  Expected := Expected + 2 ** I;
                  Count_EU := Count_EU + 1;
               end if;
            end loop;
            pragma Assert (R.EU_Mask = Expected and R.DSS_Mask = Unsigned_8 (DSS));
            pragma Assert (R.Total_EUs = Count_DSS * Count_EU);
            pragma Assert (R.Valid = (Count_DSS > 0 and Count_EU > 0));
         end;
      end loop;
   end loop;
   pragma Assert (not EU.Decode (0, 1, 0).Valid);
   pragma Assert (not EU.Decode (2, 1, 0).Valid);
   pragma Assert (not EU.Decode (Unsigned_32'Last, 1, 0).Valid);
   pragma Assert (not EU.Decode (1, Unsigned_32'Last, 0).Valid);
   pragma Assert (not EU.Decode (1, 1, Unsigned_32'Last).Valid);
   pragma Assert (EU.Decode (16#10001#, 16#10001#, 16#10000#).Total_EUs = 16);
end ADLN_EU_Tests;
