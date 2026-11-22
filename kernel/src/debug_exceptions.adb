pragma Ada_2022;
with Build;
with x86;
with Debug_Register_Probe;

package body Debug_Exceptions is
   procedure Handle_Kernel is
   begin
      Last_Kernel_DR6 := x86.getDR6;
      x86.setDR6 (x86.DR6_CLEAR);
      if Build.Debug_Register_Probe then
         Debug_Register_Probe.Disarm_Local;
      end if;
      Kernel_Count := Kernel_Count + 1;
   end Handle_Kernel;
end Debug_Exceptions;
