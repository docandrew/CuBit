with Interfaces;
with Intel_GPU_GuC_CT_Setup;
generic
   -- Serialized, non-reentrant, bounded callbacks. Owner includes running
   -- authenticated firmware, runtime mailbox authority and retained forcewake.
   with function Owner_Ready return Boolean;
   with function Read_Word (Index : Natural) return Interfaces.Unsigned_32;
   with procedure Write_Word (Index : Natural; Value : Interfaces.Unsigned_32;
                              Success : out Boolean);
   with procedure Notify (Success : out Boolean);
   with function Now return Interfaces.Unsigned_64; -- microseconds; Last=invalid
   with procedure Pause;
package Intel_GPU_GuC_MMIO is
   type Channel is limited private;
   function Broken (Object : Channel) return Boolean;
   type Result is (Rejected, Access_Failed, Invalid_Clock, Timed_Out,
                   Invalid_Reply, Firmware_Failed, Retry_Exhausted, Complete);
   -- Short CT-setup requests, single-word replies. No arbitrary memory access.
   -- Retry only on the firmware's explicit DROPPED/RETRY response, max3 times.
   -- Any other failure after admission breaks the channel; never silently
   -- reuse a mailbox which might still contain an in-flight command.
   procedure Exchange (Object : in out Channel;
     Request : Intel_GPU_GuC_CT_Setup.Request; Poll_Limit : Positive;
     Reply : out Interfaces.Unsigned_32; Status : out Result);
private
   type Channel is limited record
      Failed : Boolean := False;
   end record;
end Intel_GPU_GuC_MMIO;
