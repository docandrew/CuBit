package body Intel_GPU_Observation with SPARK_Mode is
   procedure Capture
     (Hardware : Intel_GPU_Probe.Platform; D0_Confirmed : Boolean;
      Mapping_Base, Mapping_Bytes : Unsigned_64; Result : out Snapshot)
   is
   begin
      Result := (Captured => False, Values => [others => 0]);
      if not Can_Observe
        (Hardware, D0_Confirmed, Mapping_Base, Mapping_Bytes)
      then
         return;
      end if;
      for Name in Register_Name loop
         Result.Values (Name) := Read_32
           (Intel_GPU_Probe.Register_Address
              (Mapping_Base, Mapping_Bytes, Offset (Name)));
      end loop;
      Result.Captured := True;
   end Capture;
end Intel_GPU_Observation;
