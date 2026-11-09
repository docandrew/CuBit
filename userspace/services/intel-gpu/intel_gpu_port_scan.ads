with Interfaces; use Interfaces;
with CuBit.Monitor_EDID.Catalog;
with Intel_GPU_Port_Detection;
--  DISP-009/010 step 1, the sequence: read the hot-plug words (reads
--  only), decode each port, read the base EDID block of each attached
--  unlit port whose channel is its DDC pin, and decide its state. The
--  register reads and the DDC read are formals so a register model can
--  drive it (tests/display-modeset/detection_tests.adb).
generic
   --  Read-only display MMIO (register BAR offset), all ones on failure.
   with function Read_MMIO (Offset : Unsigned_32) return Unsigned_32;
   --  The port the firmware lit (it is never probed).
   with function Is_Lit (Item : Intel_GPU_Port_Detection.Port) return Boolean;
   --  The base EDID block over Item's DDC pin (GMBUS, retries inside).
   with procedure Read_DDC_Base
     (Item : Intel_GPU_Port_Detection.Port; Base : out CuBit.Monitor_EDID.Catalog.Block;
      Outcome : out Intel_GPU_Port_Detection.EDID_Outcome);
package Intel_GPU_Port_Scan with SPARK_Mode is
   package PD renames Intel_GPU_Port_Detection;
   package C renames CuBit.Monitor_EDID.Catalog;
   use type PD.EDID_Channel, PD.EDID_Outcome, PD.Port_Sense, PD.Port_State;

   type Port_Result is record
      Sense : PD.Port_Sense;
      Channel : PD.EDID_Channel := PD.No_Channel;
      Outcome : PD.EDID_Outcome := PD.Not_Read;
      State : PD.Port_State := PD.Disconnected;
   end record;
   type Results is array (PD.Port) of Port_Result;
   type Bases is array (PD.Port) of C.Block;

   procedure Capture (Words : out PD.Hotplug_Words);

   --  Every port: Result (P) follows Words, Is_Lit and the read; Base (P)
   --  holds the block when Result (P).Outcome = Read_Valid.
   procedure Scan (Words : PD.Hotplug_Words; Result : out Results; Base : out Bases)
     with Post =>
       (for all P in PD.Port =>
          Result (P).Sense = PD.Decode_Port (Words, P) and then
          Result (P).Channel = PD.Channel_For (Is_Lit (P), Result (P).Sense) and then
          (if Result (P).Channel /= PD.DDC_Channel then
             Result (P).Outcome = (if Result (P).Channel = PD.AUX_Channel then PD.Needs_AUX
                                   else PD.Not_Read)) and then
          Result (P).State = PD.Decide (Is_Lit (P), Result (P).Sense, Result (P).Outcome));
end Intel_GPU_Port_Scan;
