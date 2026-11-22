pragma Ada_2022;
with Interfaces;

--  The manifest request for the test console object (Kernel_Object
--  Test_Console, Test_Console_ABI): a request, never a grant. procmgr
--  installs a copy with Write only when the program's startup entry
--  approves it, (test-console approve-declared), and only for a program the
--  trusted startup plan started. Only the test variant of logstore's
--  manifest declares it and only test plans approve it (tests/test-console
--  checks that nothing shipped does). Its startup entry has kind
--  Test_Console; logstore reads it with
--  CuBit.Handles.Startup.Test_Console_Named and calls
--  CuBit.Handles.Test_Consoles.
package CuBit.Test_Console_Authority with Pure, SPARK_Mode is
   --  Its .cubit.caps request kind (after the trace object's 22).
   Manifest_Request : constant Interfaces.Unsigned_8 := 23;
   --  Its rights field in .cubit.caps: Write_Only (the encoding's 2).
   Manifest_Rights : constant Interfaces.Unsigned_8 := 2;
   --  The startup name logstore looks for.
   Binding : constant String := "test-console";
end CuBit.Test_Console_Authority;
