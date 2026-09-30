package body CCL.Secondary_Arrays with
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

   procedure Allocate
     (Item      : in out Stack;
      Items     : Element_Array;
      Value     : out Array_Value;
      Result    : out Operation_Result;
      First     : Array_Index := 1;
      Sensitive : Boolean := False)
   is
      Item_Count : constant Natural := Items'Length;
      Slot        : Value_Slot;
      Offset      : Storage_Offset := 0;
   begin
      Value := (others => <>);
      if Item_Count > Max_Array_Length or else
        Item_Count > Capacity or else Item_Count > Capacity - Item.Used
      then
         Result := Storage_Full;
      elsif Item.Count = Max_Values then
         Result := Value_Table_Full;
      elsif Item.Next_Generation = Generation'Last then
         Result := Generation_Exhausted;
      else
         Slot := Value_Slot (Item.Count);
         if Item_Count > 0 then
            Offset := Storage_Offset (Item.Used);
            for Position in 0 .. Item_Count - 1 loop
               Item.Data (Offset + Position) :=
                 Items (Items'First + Position);
            end loop;
         end if;
         Item.Allocations (Slot) :=
           (Offset => Offset,
            Count => Storage_Count (Item_Count),
            Generation_Number => Item.Next_Generation,
            Sensitive => Sensitive,
            Active => True);
         Value :=
           (Slot => Slot,
            Generation_Number => Item.Next_Generation,
            First => First,
            Count => Storage_Count (Item_Count));
         Item.Used := Item.Used + Item_Count;
         Item.Count := Item.Count + 1;
         Item.Next_Generation := Item.Next_Generation + 1;
         Result := Operation_Ok;
      end if;
   end Allocate;

   procedure Reserve
     (Item   : in out Stack;
      Count  : Natural;
      Value  : out Array_Value;
      Result : out Operation_Result)
   is
      Slot   : Value_Slot;
      Offset : Storage_Offset := 0;
   begin
      Value := (others => <>);
      if Count > Max_Array_Length or else
        Count > Capacity or else Count > Capacity - Item.Used
      then
         Result := Storage_Full;
      elsif Item.Count = Max_Values then
         Result := Value_Table_Full;
      elsif Item.Next_Generation = Generation'Last then
         Result := Generation_Exhausted;
      else
         Slot := Value_Slot (Item.Count);
         if Count > 0 then
            Offset := Storage_Offset (Item.Used);
            --  Released storage may still hold older elements.
            for Position in 0 .. Count - 1 loop
               Item.Data (Offset + Position) := Null_Element;
            end loop;
         end if;
         Item.Allocations (Slot) :=
           (Offset => Offset,
            Count => Storage_Count (Count),
            Generation_Number => Item.Next_Generation,
            Sensitive => False,
            Active => True);
         Value :=
           (Slot => Slot,
            Generation_Number => Item.Next_Generation,
            First => 1,
            Count => Storage_Count (Count));
         Item.Used := Item.Used + Count;
         Item.Count := Item.Count + 1;
         Item.Next_Generation := Item.Next_Generation + 1;
         Result := Operation_Ok;
      end if;
   end Reserve;

   procedure Write
     (Item    : in out Stack;
      Value   : Array_Value;
      Index   : Array_Index;
      Element : Element_Type;
      Result  : out Operation_Result)
   is
   begin
      if not Is_Valid (Item, Value) then
         Result := Invalid_Value;
      elsif Index < Value.First or else
        Index - Value.First >= Value.Count
      then
         Result := Invalid_Bounds;
      else
         Item.Data
           (Item.Allocations (Value.Slot).Offset + (Index - Value.First)) :=
           Element;
         Result := Operation_Ok;
      end if;
   end Write;

   procedure Shrink
     (Item   : in out Stack;
      Value  : in out Array_Value;
      Count  : Natural;
      Result : out Operation_Result)
   is
      Source_Offset : Storage_Offset;
      Copy          : Array_Value;
   begin
      if not Is_Valid (Item, Value) then
         Result := Invalid_Value;
         return;
      elsif Count > Value.Count then
         Result := Invalid_Bounds;
         return;
      elsif Count = Value.Count then
         Result := Operation_Ok;
         return;
      end if;
      Source_Offset := Item.Allocations (Value.Slot).Offset;
      if Natural (Value.Slot) = Item.Count - 1 and then
        Source_Offset + Value.Count = Item.Used
      then
         --  The newest value: give back its tail, scrubbed.
         for Position in Source_Offset + Count .. Item.Used - 1 loop
            Item.Data (Position) := Null_Element;
         end loop;
         Item.Allocations (Value.Slot).Count := Storage_Count (Count);
         Item.Used := Source_Offset + Count;
         Value.Count := Storage_Count (Count);
         Result := Operation_Ok;
      else
         Reserve (Item, Count, Copy, Result);
         if Result /= Operation_Ok then
            return;
         end if;
         if Count > 0 then
            for Position in 0 .. Count - 1 loop
               pragma Loop_Invariant (Is_Valid (Item, Copy));
               pragma Loop_Invariant (Length (Copy) = Count);
               Item.Data (Item.Allocations (Copy.Slot).Offset + Position) :=
                 Item.Data (Source_Offset + Position);
            end loop;
         end if;
         Copy.First := Value.First;
         Value := Copy;
      end if;
   end Shrink;

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
            Item.Data (Position) := Null_Element;
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
            Item.Data (Position) := Null_Element;
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
      Value   : Array_Value;
      Index   : Array_Index;
      Element : out Element_Type;
      Result  : out Operation_Result)
   is
      Relative : Natural;
   begin
      Element := Null_Element;
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
      Value  : Array_Value;
      Target : out Element_Array;
      Result : out Operation_Result)
   is
   begin
      Target := [others => Null_Element];
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
end CCL.Secondary_Arrays;
