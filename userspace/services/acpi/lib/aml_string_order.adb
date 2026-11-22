pragma Ada_2022;
package body AML_String_Order with SPARK_Mode is
   use type AML_Decode.Byte;
   function Compare (Left, Right : AML_Decode.Bytes) return Ordering is
      Common : constant Natural := Natural'Min (Left'Length, Right'Length);
   begin
      for I in 0 .. Common - 1 loop
         if Left (Left'First + I) < Right (Right'First + I) then return Less;
         elsif Left (Left'First + I) > Right (Right'First + I) then return Greater;
         end if;
      end loop;
      if Left'Length < Right'Length then return Less;
      elsif Left'Length > Right'Length then return Greater;
      else return Equal; end if;
   end Compare;
end AML_String_Order;
