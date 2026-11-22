package body Intel_GPU_Plane_Collect is
   use Interfaces;
   procedure Inspect
     (Table_Bytes : Unsigned_64; Result : out Observation)
   is
      OK, End_OK : Boolean;
      Value : Unsigned_32;
      Values : array (1 .. 2) of Intel_GPU_Plane_Decode.Sample;
   begin
      Result := (others => <>);
      Begin_Access (OK);
      if not OK then return; end if;
      Result.State := Read_Failed;
      for Pass in Values'Range loop
         for Field in 0 .. 5 loop
            Read_Field (Field, Value, OK);
            if not OK or else Value = Unsigned_32'Last then
               End_Access (End_OK);
               if not End_OK then Result.State := Access_End_Failed; end if;
               return;
            end if;
            Result.Reads := Result.Reads + 1;
            case Field is
               when 0 => Values (Pass).Control := Value;
               when 1 => Values (Pass).Stride := Value;
               when 2 => Values (Pass).Size := Value;
               when 3 => Values (Pass).Offset := Value;
               when 4 => Values (Pass).Surface := Value;
               when others => Values (Pass).Live_Surface := Value;
            end case;
         end loop;
      end loop;
      End_Access (End_OK);
      if not End_OK then Result.State := Access_End_Failed; return; end if;
      Result.Before := Values (1);
      Result.After := Values (2);
      Result.Decoded := Intel_GPU_Plane_Decode.Decode
        (Result.Before, Result.After, Table_Bytes);
      Result.State := Collected;
   end Inspect;
end Intel_GPU_Plane_Collect;
