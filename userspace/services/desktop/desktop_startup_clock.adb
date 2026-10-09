with CuBit.Messages;
package body Desktop_Startup_Clock with SPARK_Mode => Off, Refined_State => (Clock => (First, Started)) is
   use type Interfaces.Unsigned_64;
   First   : Interfaces.Unsigned_64 := 0;
   Started : Boolean := False;

   function Elapsed_Ms return Interfaces.Unsigned_64 is
      Now : constant Interfaces.Unsigned_64 :=
        CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
   begin
      if not Started then
         First := Now;
         Started := True;
      end if;
      return (if Now >= First then Now - First else 0);
   end Elapsed_Ms;
end Desktop_Startup_Clock;
