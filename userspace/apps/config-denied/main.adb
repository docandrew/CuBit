with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Config_Reader;
with CuBit.Config_Inspection;
with CuBit.Startup_State;
with Startup;
procedure Main is
   use CuBit.Config_Inspection;
   Value : Text;
   Result : Status;
   Ignore : Unsigned_64;
   Good : Boolean;
begin
   Good := CuBit.Startup_State.Ready (Startup.Verify);
   CuBit.Config_Reader.Query (Probe, "", Value, Result);
   Good := Good and Result = Denied;
   CuBit.Config_Reader.Query (List_Keys, "", Value, Result);
   Good := Good and Result = Denied;
   CuBit.Config_Reader.Query (Read_Value, "test.config.value", Value, Result);
   Good := Good and Result = Denied;
   CuBit.Config_Reader.Query (Read_Value, "test.deniedness", Value, Result);
   Good := Good and Result = Denied;
   CuBit.Config_Reader.Query (Read_Value, "test.denied.absent", Value, Result);
   Good := Good and Result = Missing;
   debugPrint ((if Good then "TEST: PASS config-denied" else "TEST: FAIL config-denied") & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
end Main;
