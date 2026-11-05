pragma Ada_2022;
package body AML_Allocation_Retry with SPARK_Mode is
   procedure Run (Result : out Report) is
   begin
      Result := (1,False);
      Attempt (False);
      if Capacity_Refused then
         Result := (2,True); Attempt (True);
      end if;
   end Run;
end AML_Allocation_Retry;
