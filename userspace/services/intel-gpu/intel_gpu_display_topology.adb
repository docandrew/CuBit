package body Intel_GPU_Display_Topology with SPARK_Mode is
   function Pipe_Request_State_Mask (Item : Pipe) return Unsigned_32 is
      Selection : constant Unsigned_64 := Required (Item);
      Result : Unsigned_32 := 0;
   begin
      for W in Well loop
         if W /= DC_Off and then (Selection and Bit (W)) /= 0 then
            Result := Result or Request_Mask (W) or State_Mask (W);
         end if;
      end loop;
      return Result;
   end Pipe_Request_State_Mask;
end Intel_GPU_Display_Topology;
