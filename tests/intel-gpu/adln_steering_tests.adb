with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Steering; use Intel_GPU_ADLN_Steering;
procedure ADLN_Steering_Tests is
   Value : Topology;
   First_DSS, First_L3, Expected : Natural;
   First, Second : Fuse_Snapshot;
begin
   for DSS in Unsigned_32 range 0 .. 63 loop
      for Banks in Unsigned_32 range 0 .. 15 loop
         Value := Decode (1, DSS, 15 - Banks);
         pragma Assert (Value.Valid = (DSS /= 0 and Banks /= 0));
         if Value.Valid then
            First_DSS := 0;
            while (DSS / 2 ** First_DSS) mod 2 = 0 loop
               First_DSS := First_DSS + 1;
            end loop;
            First_L3 := 0;
            while (Banks / 2 ** First_L3) mod 2 = 0 loop
               First_L3 := First_L3 + 1;
            end loop;
            Expected := (if (Banks / 2 ** First_DSS) mod 2 /= 0 then
                           First_DSS else First_L3);
            pragma Assert (Value.Default_Instance = First_DSS);
            pragma Assert (Instance (Value, 16#B0FF#) = First_DSS);
            pragma Assert (Instance (Value, 16#B100#) = Expected);
            pragma Assert (Instance (Value, 16#B3FF#) = Expected);
            pragma Assert (Instance (Value, 16#B400#) = First_DSS);
         end if;
      end loop;
   end loop;
   for Slice in Unsigned_32 range 0 .. 255 loop
      pragma Assert (Decode (Slice, 1, 0).Valid = (Slice = 1));
   end loop;
   pragma Assert (not Decode (Unsigned_32'Last, 1, 0).Valid);
   pragma Assert (not Decode (1, Unsigned_32'Last, 0).Valid);
   pragma Assert (not Decode (1, 1, Unsigned_32'Last).Valid);
   pragma Assert (Decode (16#10001#, 16#10001#, 16#10000#).Valid);
   First := (1, 5, 2);
   pragma Assert (Decode_Stable (First, First).Valid);
   for Changed in 0 .. 2 loop
      Second := First;
      case Changed is
         when 0 => Second.Slice_Enable := 3;
         when 1 => Second.DSS_Enable := 4;
         when 2 => Second.L3_Disable := 3;
      end case;
      pragma Assert (not Decode_Stable (First, Second).Valid);
   end loop;
   First := (others => Unsigned_32'Last);
   pragma Assert (not Decode_Stable (First, First).Valid);
   Ada.Text_IO.Put_Line ("ADL-N steering: 1024 topology pairs, 256 slice masks and hostile reads PASS");
end ADLN_Steering_Tests;
