package body CCL.Types.Shapes with SPARK_Mode is
   function Is_Record (Item : Registry; Ref : Type_Reference) return Boolean is
     (Known (Item, Ref) and then Describe (Item, Ref).Form = Product);

   --  Ref's own rows: a record's fields as columns.
   function Record_Shape (Item : Registry; Ref : Type_Reference; Many : Boolean) return Row_Shape
   with Pre => Is_Record (Item, Ref)
   is
      D : constant Description := Describe (Item, Ref);
      Result : Row_Shape;
   begin
      Result.Many := Many;
      Result.Row_Type := D.Identifier;
      Result.Count := D.Count;
      for I in 1 .. D.Count loop
         Result.Fields (I) :=
           (Identifier => D.Parts (I).Identifier,
            Type_Name => Describe (Item, D.Parts (I).Payload).Identifier,
            Numeric => Base_Of (Item, D.Parts (I).Payload) = Integer_Type);
      end loop;
      return Result;
   end Record_Shape;

   function Shape_Of (Item : Registry; Ref : Type_Reference) return Row_Shape is
      Row : Type_Reference := Ref;
      Many : Boolean := False;
   begin
      if Is_List (Item, Ref) then
         Row := Element_Of (Item, Ref);
         Many := True;
      end if;
      if Is_Record (Item, Row) then
         return Record_Shape (Item, Row, Many);
      elsif not Many or else not Known (Item, Row) or else Describe (Item, Row).Form /= Sum then
         return (others => <>);
      end if;
      declare
         V : constant Description := Describe (Item, Row);
         Result : Row_Shape;
      begin
         if V.Count = 0 or else not Is_Record (Item, V.Parts (1).Payload) then
            return (others => <>);
         end if;
         Result := Record_Shape (Item, V.Parts (1).Payload, Many => True);
         Result.Variant_Rows := True;
         Result.Member_Count := V.Count;
         for M in 1 .. V.Count loop
            Result.Members (M).Member := V.Parts (M).Identifier;
            if Is_Record (Item, V.Parts (M).Payload) then
               declare
                  P : constant Description := Describe (Item, V.Parts (M).Payload);
               begin
                  Result.Members (M).Fields := P.Count;
                  for F in 1 .. P.Count loop
                     for C in 1 .. Result.Count loop
                        if Same (P.Parts (F).Identifier, Result.Fields (C).Identifier) then
                           Result.Members (M).Column_Of (F) := C;
                        end if;
                     end loop;
                  end loop;
               end;
            end if;
         end loop;
         return Result;
      end;
   end Shape_Of;
end CCL.Types.Shapes;
