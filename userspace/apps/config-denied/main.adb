with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Config_Inspection;
with CuBit.Settings.Inspection;
with Startup;
--  A program without config-all: Config must refuse it inspection outside
--  its "test.denied" scope. Its manifest gives it no Inspector, so it makes
--  one from its Config endpoint on purpose; Config, not the Inspector,
--  decides.
procedure Main is
   use CuBit.Config_Inspection;
   Config : constant CuBit.Settings.Inspection.Inspector := CuBit.Settings.Inspection.Using (Startup.Config);
   procedure Query (Op : Operation; Key : String; Value : out Text; Result : out Status) is
   begin
      CuBit.Settings.Inspection.Inspect (Config, Op, Key, Machine_Context, Value, Result);
   end Query;
   Value : Text;
   Result : Status;
   Ignore : Unsigned_64;
   Good : Boolean;
begin
   Query (Probe, "", Value, Result);
   Good := Result = Denied;
   Query (List_Keys, "", Value, Result);
   Good := Good and Result = Denied;
   Query (Read_Value, "test.config.value", Value, Result);
   Good := Good and Result = Denied;
   Query (Read_Value, "test.deniedness", Value, Result);
   Good := Good and Result = Denied;
   Query (Read_Value, "test.denied.absent", Value, Result);
   Good := Good and Result = Missing;
   debugPrint ((if Good then "TEST: PASS config-denied" else "TEST: FAIL config-denied") & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
end Main;
