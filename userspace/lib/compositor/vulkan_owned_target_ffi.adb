package body Vulkan_Owned_Target_FFI with SPARK_Mode => Off is
   function Native_Bind (Description, A, B, C, Submission : System.Address) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_owned_targets_bind";
   procedure Bind (Description, A, B, C, Submission : System.Address; Result : out Interfaces.Unsigned_32) is
   begin Result := Native_Bind (Description, A, B, C, Submission); end Bind;
   function Native_Prepare (Description, Submission : System.Address;
      Slot, Width, Height, Discard : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_owned_targets_prepare_frame";
   procedure Prepare_Frame (Description, Submission : System.Address;
      Slot, Width, Height : Interfaces.Unsigned_32; Discard : Boolean;
      Result : out Interfaces.Unsigned_32) is
   begin
      Result := Native_Prepare (Description, Submission, Slot, Width, Height,
                                (if Discard then 1 else 0));
   end Prepare_Frame;
   function Native_Readback (Description, Submission, Staging : System.Address;
      Slot : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_owned_targets_record_readback";
   procedure Record_Readback (Description, Submission, Staging : System.Address;
      Slot : Interfaces.Unsigned_32; Result : out Interfaces.Unsigned_32) is
   begin Result := Native_Readback (Description, Submission, Staging, Slot); end Record_Readback;
   type Native_Region is record
      Left, Top, Right, Bottom : Interfaces.Unsigned_32;
   end record with Convention => C;
   type Native_Regions is array (Compositor_Damage.Index) of aliased Native_Region
     with Convention => C;
   function Native_Regions_Readback (Description, Submission, Staging : System.Address;
      Slot, Count : Interfaces.Unsigned_32; Regions : access constant Native_Region)
      return Interfaces.Unsigned_32 with Import, Convention => C,
        External_Name => "cubit_vulkan_owned_targets_record_readback_regions";
   procedure Record_Readback_Regions (Description, Submission, Staging : System.Address;
      Slot : Interfaces.Unsigned_32; Repair : Compositor_Damage.State;
      Result : out Interfaces.Unsigned_32)
   is
      Values : Native_Regions := (others => (others => 0));
      B : Compositor_Damage.Box;
   begin
      pragma Assert (Native_Region'Size = 16 * 8);
      for I in 1 .. Compositor_Damage.Count (Repair) loop
         B := Compositor_Damage.Item (Repair, I);
         Values (I) := (Interfaces.Unsigned_32 (B.Left), Interfaces.Unsigned_32 (B.Top),
           Interfaces.Unsigned_32 (B.Right), Interfaces.Unsigned_32 (B.Bottom));
      end loop;
      Result := Native_Regions_Readback (Description, Submission, Staging, Slot,
        Interfaces.Unsigned_32 (Compositor_Damage.Count (Repair)), Values (1)'Access);
   end Record_Readback_Regions;
end Vulkan_Owned_Target_FFI;
