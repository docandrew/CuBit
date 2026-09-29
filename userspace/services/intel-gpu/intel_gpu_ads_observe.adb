with Interfaces; use Interfaces;
package body Intel_GPU_ADS_Observe is
   function Capture
     (Description : Intel_GPU_ADLN_Inventory.Inventory;
      Power_Held : Boolean) return Observation
   is
      use Intel_GPU_ADLN_Steering;
      First, Second : Fuse_Snapshot;
      Doorbell_First, Doorbell_Second : Unsigned_32;
      EU_First, EU_Second : Unsigned_32;
      Result : Observation;
      procedure Sample (Fuses : out Fuse_Snapshot; Doorbell, EU : out Unsigned_32) is
      begin
         Fuses.Slice_Enable := Read_32 (Slice_Register);
         Fuses.DSS_Enable := Read_32 (DSS_Register);
         Fuses.L3_Disable := Read_32 (L3_Register);
         Doorbell := Read_32 (Intel_GPU_ADS_System_Info.Doorbell_Register);
         EU := Read_32 (Intel_GPU_ADLN_EU.EU_Disable_Register);
      end Sample;
   begin
      if not Description.Valid or else not Power_Held then return Result; end if;
      Sample (First, Doorbell_First, EU_First);
      Sample (Second, Doorbell_Second, EU_Second);
      if EU_First /= EU_Second then return Result; end if;
      Result.Execution_Units := Intel_GPU_ADLN_EU.Decode
        (First.Slice_Enable, First.DSS_Enable, EU_First);
      if not Result.Execution_Units.Valid then return (others => <>); end if;
      Result.Topology := Decode_Stable (First, Second);
      Result.System_Info := Intel_GPU_ADS_System_Info.Build
        (Description, Result.Topology, Doorbell_First, Doorbell_Second);
      if not Result.System_Info.Valid then return (others => <>); end if;
      Result.Doorbell := Doorbell_First;
      Result.Valid := True;
      return Result;
   end Capture;
end Intel_GPU_ADS_Observe;
