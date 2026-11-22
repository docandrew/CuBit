with Intel_GPU_Combo_PHY;
package body Intel_GPU_PHY_Pages with SPARK_Mode is
   function Write_Address (Register_Offset : Unsigned_32) return Unsigned_64 is
      use Intel_GPU_Combo_PHY;
      Page : Page_Index;
   begin
      if Register_Offset mod 4 /= 0 then return 0; end if;
      for Port in PHY loop
         for F in Misc .. CL_5 loop
            -- Only A is a compensation master on ADL-N.
            if (Port = A or F /= Comp_8) and then
              Register_Offset = Write_Offset (Port, F)
            then
               Page := (if F = Misc then 0 elsif Port = A then 1 else 2);
               return Virtual_Base + Unsigned_64 (Page) * 4096 +
                 Unsigned_64 (Register_Offset mod 4096);
            end if;
         end loop;
      end loop;
      return 0;
   end Write_Address;
end Intel_GPU_PHY_Pages;
