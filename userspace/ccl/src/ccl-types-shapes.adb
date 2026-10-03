package body CCL.Types.Shapes with SPARK_Mode is
   function Shape_Of (Item : Registry; Ref : Type_Reference) return Row_Shape is
      Result : Row_Shape;
      Row : Type_Reference := Ref;
   begin
      if Is_List (Item, Ref) then
         Row := Element_Of (Item, Ref);
         Result.Many := True;
      end if;
      if not Known (Item, Row) or else Describe (Item, Row).Form /= Product then
         return (others => <>);
      end if;
      declare
         D : constant Description := Describe (Item, Row);
      begin
         Result.Row_Type := D.Identifier;
         Result.Count := D.Count;
         for I in 1 .. D.Count loop
            Result.Fields (I) :=
              (Identifier => D.Parts (I).Identifier,
               Type_Name => Describe (Item, D.Parts (I).Payload).Identifier,
               Numeric => Base_Of (Item, D.Parts (I).Payload) = Integer_Type);
         end loop;
      end;
      return Result;
   end Shape_Of;
end CCL.Types.Shapes;
