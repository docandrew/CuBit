with Interfaces;
with Intel_GPU_Probe;
with Intel_GPU_Display_Topology;

--  Admission for the initial ADLN power-control snapshot, not for arbitrary
--  register access. D0 must come from trusted PCI discovery, not an IPC claim.
package Intel_GPU_Observation with SPARK_Mode is
   use Interfaces;
   use type Intel_GPU_Probe.Platform;
   type Register_Name is
     (Firmware_Power_Control, Driver_Power_Control, Display_DC_Control, Display_Fuse_Status);
   function Offset (Name : Register_Name) return Unsigned_64 is
     (case Name is
        when Firmware_Power_Control => 16#45400#,
        when Driver_Power_Control => 16#45404#,
        when Display_DC_Control => 16#45504#,
        when Display_Fuse_Status => 16#42000#);

   function Can_Observe
     (Hardware : Intel_GPU_Probe.Platform; D0_Confirmed : Boolean;
      Mapping_Base, Mapping_Bytes : Unsigned_64) return Boolean
   is
     (Hardware = Intel_GPU_Probe.Alder_Lake_N and then D0_Confirmed
      and then (for all Name in Register_Name =>
        Intel_GPU_Probe.Contains_Register
          (Mapping_Base, Mapping_Bytes, Offset (Name))));

   type Register_Values is array (Register_Name) of Unsigned_32;
   type Display_Pipe is (Pipe_A, Pipe_B, Pipe_C, Pipe_D);
   -- Xe-LPD hierarchy: PW1 -> PWA, or PW1 -> PW2 -> PWB/PWC/PWD.
   -- Each pair is request+state, not just observed power-on. This decodes
   -- a snapshot ONLY: it holds no reference and does not cover DC state,
   -- fuse readiness, access serialization or ownership of inherited requests.
   function Pipe_Request_State_Mask (Pipe : Display_Pipe) return Unsigned_32 is
     (Intel_GPU_Display_Topology.Pipe_Request_State_Mask
        (case Pipe is
           when Pipe_A => Intel_GPU_Display_Topology.A,
           when Pipe_B => Intel_GPU_Display_Topology.B,
           when Pipe_C => Intel_GPU_Display_Topology.C,
           when Pipe_D => Intel_GPU_Display_Topology.D));
   function Pipe_Request_State_Set
     (Driver_Control : Unsigned_32; Pipe : Display_Pipe) return Boolean is
     (Driver_Control /= Unsigned_32'Last and then
      (Driver_Control and Pipe_Request_State_Mask (Pipe)) =
        Pipe_Request_State_Mask (Pipe));
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
      and then (if not Result.Captured then Result.Values = Register_Values'[others => 0]));
   --  No reads on rejection. Exactly one read per named register on admission.
   --  The native adapter must retain the live uncached read-only mapping and
   --  D0 device lifetime throughout; this routine cannot enforce those facts.
end Intel_GPU_Observation;
