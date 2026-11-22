with Intel_GPU_ADLN_Golden;
package body Intel_GPU_ADS_Initialization with SPARK_Mode is
   function Prepare
     (Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Topology : Intel_GPU_ADLN_Steering.Topology;
      Doorbell_First, Doorbell_Second, MMIO_Bytes : Unsigned_32;
      GPU_Base, Backing_Bytes, Firmware_Private_Bytes : Unsigned_64)
      return Prepared_Image
   is
      use Intel_GPU_ADS_Layout;
      Result : Prepared_Image;
      Golden : Intel_GPU_ADLN_Golden.Reservation;
      Register_Table : Intel_GPU_ADS_Header.Register_Descriptors;
      Capture_Table : Intel_GPU_ADS_Header.Capture_Pointers;
      Golden_Table, Size_Table : Intel_GPU_ADS_Header.Class_Values;
      Golden_Bytes : constant Unsigned_64 :=
        Intel_GPU_ADLN_Golden.Required_Bytes (Inventory);
   begin
      if Firmware_Private_Bytes /= 16#801000# or else Golden_Bytes = 0 then
         return Result;
      end if;
      Result.Layout := Plan
        (Intel_GPU_ADS_Register_Image.Capacity, Golden_Bytes, 0,
         Intel_GPU_ADS_Capture_Image.Capacity, Firmware_Private_Bytes, Backing_Bytes);
      if not Result.Layout.Valid or else GPU_Base = 0 or else
        GPU_Base mod 4096 /= 0 or else GPU_Base >= Limit or else
        Result.Layout.Total > Limit - GPU_Base
      then return (others => <>); end if;
      Result.System_Info := Intel_GPU_ADS_System_Info.Build
        (Inventory, Topology, Doorbell_First, Doorbell_Second);
      Result.Registers := Intel_GPU_ADS_Register_Image.Build
        (Inventory, Topology, MMIO_Bytes, GPU_Base + Result.Layout.Offset (Registers));
      Result.Capture := Intel_GPU_ADS_Capture_Image.Build
        (Inventory, Topology, GPU_Base + Result.Layout.Offset (Capture),
         Result.Layout.Bytes (Capture));
      Golden := Intel_GPU_ADLN_Golden.Plan
        (Inventory, GPU_Base + Result.Layout.Offset (Golden_Contexts), Golden_Bytes);
      if not Result.System_Info.Valid or else not Result.Registers.Valid or else
        not Result.Capture.Valid or else not Golden.Valid
      then return (others => <>); end if;
      Result.Policies := Intel_GPU_ADS_Policies.Encode (Allow_Engine_Reset => False);
      for I in Register_Table'Range loop
         Register_Table (I) := Result.Registers.Descriptors (I);
      end loop;
      for I in Capture_Table'Range loop
         Capture_Table (I) := Result.Capture.Pointers (I);
      end loop;
      for I in Golden_Table'Range loop
         Golden_Table (I) := Golden.Addresses (I);
         Size_Table (I) := Golden.State_Bytes (I);
      end loop;
      Result.Header := Intel_GPU_ADS_Header.Encode
        (Register_Table,
         Unsigned_32 (GPU_Base + Result.Layout.Offset (Policies)),
         Unsigned_32 (GPU_Base + Result.Layout.Offset (System_Info)),
         Unsigned_32 (GPU_Base + Result.Layout.Offset (Private_Data)),
         Golden_Table, Size_Table, Capture_Table);
      Result.Valid := True;
      return Result;
   end Prepare;
end Intel_GPU_ADS_Initialization;
