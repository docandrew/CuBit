package body CCL.Bounded_Stacks with
   SPARK_Mode => On
is
   procedure Initialize (Item : out Stack) is
   begin
      Item :=
        (Elements => [others => Default_Value],
         Next     => Index_Type'First,
         Count    => 0);
   end Initialize;

   procedure Push
     (Item   : in out Stack;
      Value  : Element_Type;
      Result : out Operation_Result)
   is
      Current_Count : constant Unsigned_32 := Item.Count;
   begin
      if Current_Count > Capacity then
         Result := Stack_Invalid;
      elsif Current_Count = Capacity then
         Result := Stack_Full;
      else
         Item.Elements (Item.Next) := Value;
         Item.Next := Item.Next + 1;
         Item.Count := Current_Count + 1;
         Result := Stack_Ok;
      end if;
   end Push;

   procedure Pop
     (Item   : in out Stack;
      Value  : out Element_Type;
      Result : out Operation_Result)
   is
      Current_Count : constant Unsigned_32 := Item.Count;
   begin
      if Current_Count > Capacity then
         Value := Item.Elements (Index_Type'First);
         Result := Stack_Invalid;
      elsif Current_Count = 0 then
         Value := Item.Elements (Index_Type'First);
         Result := Stack_Empty;
      else
         Item.Next := Item.Next - 1;
         Item.Count := Current_Count - 1;
         Value := Item.Elements (Item.Next);
         Item.Elements (Item.Next) := Default_Value;
         Result := Stack_Ok;
      end if;
   end Pop;

   procedure Peek_Top
     (Item   : Stack;
      Value  : out Element_Type;
      Result : out Operation_Result)
   is
      Current_Count : constant Unsigned_32 := Item.Count;
      Top           : Index_Type;
   begin
      if Current_Count > Capacity then
         Value := Item.Elements (Index_Type'First);
         Result := Stack_Invalid;
      elsif Current_Count = 0 then
         Value := Item.Elements (Index_Type'First);
         Result := Stack_Empty;
      else
         Top := Item.Next - 1;
         Value := Item.Elements (Top);
         Result := Stack_Ok;
      end if;
   end Peek_Top;

   function "=" (Left, Right : Stack) return Boolean is
      Left_Count  : constant Unsigned_32 := Left.Count;
      Right_Count : constant Unsigned_32 := Right.Count;
   begin
      if Left_Count > Capacity then
         return False;
      elsif Right_Count > Capacity then
         return False;
      elsif Left_Count /= Right_Count or else Left.Next /= Right.Next then
         return False;
      else
         for Position in Index_Type loop
            if Left.Elements (Position) /= Right.Elements (Position) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";
end CCL.Bounded_Stacks;
