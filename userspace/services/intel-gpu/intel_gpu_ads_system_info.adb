with Intel_GPU_ADS_Engines;
package body Intel_GPU_ADS_System_Info with SPARK_Mode is
   use Intel_GPU_ADLN_Inventory;
   function Build (Description : Inventory;
                   Topology : Intel_GPU_ADLN_Steering.Topology;
                   Doorbell_First, Doorbell_Second : Unsigned_32) return System_Info is
      Prefix : constant Intel_GPU_ADS_Engines.Encoding := Intel_GPU_ADS_Engines.Encode (Description);
      Result : System_Info;
      Count : Unsigned_32;
   begin
      if not Prefix.Valid or else not Topology.Valid or else
        Doorbell_First /= Doorbell_Second or else Doorbell_First = Unsigned_32'Last
      then return Result; end if;
      for I in Prefix.Bytes'Range loop Result.Bytes (I) := Prefix.Bytes (I); end loop;
      Result.Bytes (576) := 1; -- admitted physical slice count, not DSS count
      if Description.Engines (Video_0) then Result.Bytes (580) := 1; end if;
      if Description.Engines (Video_2) then Result.Bytes (580) := Result.Bytes (580) or 4; end if;
      Count := (Shift_Right (Doorbell_First, 16) and 255) + 1;
      Result.Bytes (584) := Unsigned_8 (Count mod 256);
      Result.Bytes (585) := Unsigned_8 (Count / 256);
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADS_System_Info;
