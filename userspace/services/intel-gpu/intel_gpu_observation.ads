with Interfaces;
with Intel_GPU_Probe;

--  Admission for the initial ADLN power-control snapshot, not for arbitrary
--  register access. D0 must come from trusted PCI discovery, not an IPC claim.
package Intel_GPU_Observation with SPARK_Mode is
   use Interfaces;
   use type Intel_GPU_Probe.Platform;
   type Register_Name is (Firmware_Power_Control, Driver_Power_Control);
   function Offset (Name : Register_Name) return Unsigned_64 is
     (case Name is
        when Firmware_Power_Control => 16#45400#,
        when Driver_Power_Control => 16#45404#);

   function Can_Observe
     (Hardware : Intel_GPU_Probe.Platform; D0_Confirmed : Boolean;
      Mapping_Base, Mapping_Bytes : Unsigned_64) return Boolean
   is
     (Hardware = Intel_GPU_Probe.Alder_Lake_N and then D0_Confirmed
      and then (for all Name in Register_Name =>
        Intel_GPU_Probe.Contains_Register
          (Mapping_Base, Mapping_Bytes, Offset (Name))));

   type Register_Values is array (Register_Name) of Unsigned_32;
   --  Raw evidence only: even plausible values neither hold power references
   --  nor authorize access to a dependent register or claim scanout ownership.
   type Snapshot is record
      Captured : Boolean := False;
      Values : Register_Values := [others => 0];
   end record;

   generic
      with function Read_32 (Address : Unsigned_64) return Unsigned_32;
   procedure Capture
     (Hardware : Intel_GPU_Probe.Platform; D0_Confirmed : Boolean;
      Mapping_Base, Mapping_Bytes : Unsigned_64; Result : out Snapshot)
   with Post =>
     (Result.Captured = Can_Observe
        (Hardware, D0_Confirmed, Mapping_Base, Mapping_Bytes)
      and then (if not Result.Captured then Result.Values = [0, 0]));
   --  No reads on rejection. Exactly one read per named register on admission.
   --  The native adapter must retain the live uncached read-only mapping and
   --  D0 device lifetime throughout; this routine cannot enforce those facts.
end Intel_GPU_Observation;
