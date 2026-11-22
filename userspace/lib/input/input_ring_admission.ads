------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Whether Desktop admits an input source ring (IPC-009): only through the
--  endpoint devmgr badged for input sources (CuBit.Input.Source_Ring_Tag),
--  the opener producing, within the driver's and the set's limits. No
--  system call: Desktop_Input_Sources asks it and tests/input-rings checks it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Input;
with CuBit.Input_Rings;

package Input_Ring_Admission with SPARK_Mode is

   type Verdict is (Admitted, Not_An_Input_Endpoint, Not_Producing, Source_Limit);

   --  Badge: the authority the open came through (kernel-stamped).
   --  Driver_Rings: the rings this driver has open; Free_Slot: the set's.
   function Decide
     (Badge : Unsigned_64; Producing : Boolean; Driver_Rings, Free_Slot : Natural) return Verdict is
     (if Badge /= CuBit.Input.Source_Ring_Tag then Not_An_Input_Endpoint
      elsif not Producing then Not_Producing
      elsif Free_Slot = 0 or else Driver_Rings >= CuBit.Input_Rings.Sources_Per_Driver then Source_Limit
      else Admitted);

   function Reason (V : Verdict) return String is
     (case V is
        when Admitted => "admitted",
        when Not_An_Input_Endpoint => "not opened through an input source's endpoint",
        when Not_Producing => "the opener does not produce",
        when Source_Limit => "source limit reached");

end Input_Ring_Admission;
