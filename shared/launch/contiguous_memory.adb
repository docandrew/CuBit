pragma Ada_2022;

package body Contiguous_Memory with SPARK_Mode is

   function Order_For (Pages : Page_Count) return Order is
      Result : Order := 0;
   begin
      while 2 ** Result < Pages loop
         pragma Loop_Invariant (2 ** Result < Pages and then Result < Maximum_Order);
         pragma Loop_Invariant (Result = 0 or else 2 ** (Result - 1) < Pages);
         pragma Loop_Variant (Increases => Result);
         Result := Result + 1;
      end loop;
      return Result;
   end Order_For;

end Contiguous_Memory;
