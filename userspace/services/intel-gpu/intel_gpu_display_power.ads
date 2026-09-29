with Interfaces;
with Intel_GPU_Display_Topology;
generic
   with function Read_32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write_32 (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
   with function Now_Us return Interfaces.Unsigned_64;
   with procedure Pause;
   with procedure Post_Enable
     (Item : Intel_GPU_Display_Topology.Request_Well; Success : out Boolean);
   with procedure Pre_Disable
     (Item : Intel_GPU_Display_Topology.Request_Well; Success : out Boolean);
   -- These implement a complete DC-off reference, not just DC register write
   -- verification. Drop must retire software bookkeeping for inherited refs
   -- without undoing the firmware baseline. All callbacks must be bounded,
   -- non-raising and serialized under the same designated device owner.
   with procedure Hold_DC_Off (Added, Success : out Boolean);
   with procedure Drop_DC_Off (Added : Boolean; Success : out Boolean);
package Intel_GPU_Display_Power is
   type Ownership_State is (Idle, Held, Faulted);
   function State return Ownership_State;
   function Retained return Interfaces.Unsigned_64;
   function Uncertain return Interfaces.Unsigned_64;
   -- Authority_Ready is trusted caller evidence of owner designation, live
   -- mapping/D0 identity, exclusive access and stable inherited baseline. It
   -- must never be copied from an untrusted client's request.
   procedure Acquire
     (Item : Intel_GPU_Display_Topology.Pipe; Authority_Ready : Boolean;
      Poll_Limit : Positive; Success : out Boolean);
   procedure Release (Success : out Boolean);
end Intel_GPU_Display_Power;
