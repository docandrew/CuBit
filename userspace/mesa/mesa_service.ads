with Interfaces;
with System;

--  Trusted in-process adapter for service-device.h, NOT GPU authority or a
--  general Vulkan binding. Foreign calls and hardware observations are not
--  SPARK-proved. Serialize all operations and uses of the borrowed queue.
package Mesa_Service with SPARK_Mode => Off is
   use type Interfaces.Integer_32;
   subtype Result is Interfaces.Integer_32;
   Success : constant Result := 0;
   Initialization_Failed : constant Result := -3;
   type Retirement is (Retired, Pending, Unsafe);
   type Owner is limited private;

   --  Native CuBit x86-64 ABI. These are opaque, same-process borrowed Vulkan
   --  handles/function pointer, NEVER capability slots or IPC payloads.
   type Device_View is record
      Instance, Physical, Device, Queue : System.Address := System.Null_Address;
      Family : Interfaces.Unsigned_32 := 0;
      Instance_Proc : System.Address := System.Null_Address;
   end record with Convention => C;
   for Device_View use record
      Instance at 0 range 0 .. 63;
      Physical at 8 range 0 .. 63;
      Device at 16 range 0 .. 63;
      Queue at 24 range 0 .. 63;
      Family at 32 range 0 .. 31;
      Instance_Proc at 40 range 0 .. 63;
   end record;
   for Device_View'Size use 384;
   for Device_View'Alignment use 8;
   pragma Compile_Time_Error (System.Address'Size /= 64, "Mesa bridge needs x86-64 addresses");

   --  One attempt per Owner AND one accepted owner per process in the C
   --  implementation. Slot must already be admitted and remain unchanged.
   --  A failed Result with Accepted=True STILL requires retaining the slot
   --  and calling Close after all consumers retire. No reset/retry API.
   procedure Start (Object : in out Owner; Slot : Interfaces.Unsigned_64;
                    Status : out Result);
   function Accepted (Object : Owner) return Boolean;
   procedure Borrow (Object : Owner; View : out Device_View; Ready : out Boolean);
   function Health (Object : Owner) return Result;

   --  Consumers_Retired is a trusted caller observation, not a fence/proof.
   --  False returns Pending without teardown. True permits one destruction
   --  pass and subsequent polling. Pending/Unsafe retain all owner storage
   --  and endpoint authority. Retired still does not permit owner/slot reuse.
   function Close (Object : in out Owner; Consumers_Retired : Boolean)
                   return Retirement;
private
   type Owner is limited record
      Handle : System.Address := System.Null_Address;
      Attempted, Closing : Boolean := False;
   end record;
end Mesa_Service;
