pragma Ada_2022;
with Interfaces;

--  The manifest request for the platform power object (Request.Platform_Power,
--  KERN-004, Platform_Power_ABI): a request, never a grant. procmgr installs a
--  copy with Write only when the program's startup entry approves it,
--  (platform-power approve-declared), and only for a program the trusted
--  startup plan started. Its startup entry has kind Platform_Power; the
--  program reads it with CuBit.Handles.Startup.Platform_Power_Named and calls
--  CuBit.Handles.Power.Enter_State.
package CuBit.Platform_Power_Authority with Pure, SPARK_Mode is
   --  Its .cubit.caps request kind (after the resource kinds' 16 .. 20).
   Manifest_Request : constant Interfaces.Unsigned_8 := 21;
   --  Its rights field in .cubit.caps: Write_Only (the encoding's 2).
   Manifest_Rights : constant Interfaces.Unsigned_8 := 2;
end CuBit.Platform_Power_Authority;
