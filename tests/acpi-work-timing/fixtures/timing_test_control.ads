pragma Ada_2022;
with Interfaces;
package Timing_Test_Control is
   Now : Interfaces.Unsigned_64 := 0;
   Known, Complete, Found_Module : Boolean := True;
   Lines : Natural := 0;
   Found_Zero_Allocation : Boolean := False;
   Found_Allocation,Found_Lookup,Found_Gap,Found_Drop,Incomplete : Boolean := False;
   Found_FACS, Found_Deferred, Found_Signal : Boolean := False;
end Timing_Test_Control;
