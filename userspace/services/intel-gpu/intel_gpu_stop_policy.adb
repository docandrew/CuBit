pragma Ada_2022;
package body Intel_GPU_Stop_Policy with SPARK_Mode is

   procedure Note_Stop (S : in out State; Now : Unsigned_64) is
   begin
      if not S.Requested then
         S := (Requested => True, Since => Now);
      end if;
   end Note_Stop;

end Intel_GPU_Stop_Policy;
