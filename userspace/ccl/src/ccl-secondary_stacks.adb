package body CCL.Secondary_Stacks with
   SPARK_Mode => On
is
   procedure Initialize (Item : out Stack) is
   begin
      Item := (others => <>);
   end Initialize;

   function Mark (Item : Stack) return Stack_Mark is
     ((Bytes => Item.Used,
       Values => Item.Count,
       Boundary_Generation =>
         (if Item.Count = 0 then 0
          else Item.Allocations (Item.Count - 1).Generation_Number)));

   procedure Allocate_String
     (Item      : in out Stack;
      Text      : String;
      Value     : out String_Value;
      Result    : out Operation_Result;
      First     : String_Index := 1;
      Sensitive : Boolean := False)
   is
      Text_Length : constant Natural := Text'Length;
      Slot        : Value_Slot;
      Offset      : Storage_Offset := 0;
   begin
      Value := (others => <>);
      if Text_Length > Capacity or else Text_Length > Capacity - Item.Used then
         Result := Storage_Full;
      elsif Item.Count = Max_Values then
         Result := Value_Table_Full;
      elsif Item.Next_Generation = Generation'Last then
         Result := Generation_Exhausted;
      else
         Slot := Value_Slot (Item.Count);
         if Text_Length > 0 then
            Offset := Storage_Offset (Item.Used);
            for Position in 0 .. Text_Length - 1 loop
               Item.Data (Offset + Position) :=
                 Text (Text'First + Position);
            end loop;
         end if;
         Item.Allocations (Slot) :=
           (Offset => Offset,
            Count => Storage_Count (Text_Length),
            Generation_Number => Item.Next_Generation,
            Sensitive => Sensitive,
            Active => True);
         Value :=
           (Slot => Slot,
            Generation_Number => Item.Next_Generation,
            First => First,
            Count => Storage_Count (Text_Length));
         Item.Used := Item.Used + Text_Length;
         Item.Count := Item.Count + 1;
         Item.Next_Generation := Item.Next_Generation + 1;
         Result := Operation_Ok;
      end if;
   end Allocate_String;

   function Boundary_Is_Valid
     (Item : Stack; Boundary : Stack_Mark) return Boolean is
     (Boundary.Values <= Item.Count and then Boundary.Bytes <= Item.Used and then
      (if Boundary.Values = 0 then
          Boundary.Bytes = 0 and then Boundary.Boundary_Generation = 0
       else
          Item.Allocations (Boundary.Values - 1).Active and then
          Item.Allocations (Boundary.Values - 1).Generation_Number =
            Boundary.Boundary_Generation and then
          (if Item.Allocations (Boundary.Values - 1).Count = 0 then
              True
           else
              Item.Allocations (Boundary.Values - 1).Count <=
                Boundary.Bytes and then
              Item.Allocations (Boundary.Values - 1).Offset =
                Boundary.Bytes -
                  Item.Allocations (Boundary.Values - 1).Count)));

   procedure Release
     (Item     : in out Stack;
      Boundary : Stack_Mark;
      Result   : out Operation_Result)
   is
      Allocation_Item : Allocation;
      Scrub_Released_Bytes : Boolean := False;
   begin
      if not Boundary_Is_Valid (Item, Boundary) then
         Result := Invalid_Mark;
         return;
      end if;

      if Boundary.Values < Item.Count then
         for Slot in Boundary.Values .. Item.Count - 1 loop
            Allocation_Item := Item.Allocations (Slot);
            Scrub_Released_Bytes :=
              Scrub_Released_Bytes or Allocation_Item.Sensitive;
            Item.Allocations (Slot).Active := False;
         end loop;
      end if;
      --  One contiguous wipe is both cheaper and easier to establish than
      --  reconstructing each released allocation's bounds.  If the scope held
      --  any sensitive value, all bytes in that released scope are secret.
      if Scrub_Released_Bytes and then Boundary.Bytes < Item.Used then
         for Position in Boundary.Bytes .. Item.Used - 1 loop
            Item.Data (Position) := Character'Val (0);
         end loop;
      end if;
      Item.Used := Boundary.Bytes;
      Item.Count := Boundary.Values;
      Result := Operation_Ok;
   end Release;

   procedure Clear (Item : in out Stack) is
   begin
      if Item.Used > 0 then
         for Position in 0 .. Item.Used - 1 loop
            Item.Data (Position) := Character'Val (0);
         end loop;
      end if;
      if Item.Count > 0 then
         for Slot in 0 .. Item.Count - 1 loop
            Item.Allocations (Slot).Active := False;
         end loop;
      end if;
      Item.Used := 0;
      Item.Count := 0;
   end Clear;

   procedure Read
     (Item    : Stack;
      Value   : String_Value;
      Index   : String_Index;
      Element : out Character;
      Result  : out Operation_Result)
   is
      Relative : Natural;
   begin
      Element := Character'Val (0);
      if not Is_Valid (Item, Value) then
         Result := Invalid_Value;
      elsif Index < Value.First or else
        Index - Value.First >= Value.Count
      then
         Result := Invalid_Bounds;
      else
         Relative := Index - Value.First;
         Element := Item.Data
           (Item.Allocations (Value.Slot).Offset + Relative);
         Result := Operation_Ok;
      end if;
   end Read;

   procedure Copy_To
     (Item   : Stack;
      Value  : String_Value;
      Target : out String;
      Result : out Operation_Result)
   is
   begin
      Target := [others => Character'Val (0)];
      if not Is_Valid (Item, Value) then
         Result := Invalid_Value;
      elsif Target'Length /= Value.Count then
         Result := Length_Mismatch;
      else
         if Value.Count > 0 then
            for Position in 0 .. Value.Count - 1 loop
               Target (Target'First + Position) := Item.Data
                 (Item.Allocations (Value.Slot).Offset + Position);
            end loop;
         end if;
         Result := Operation_Ok;
      end if;
   end Copy_To;
end CCL.Secondary_Stacks;
