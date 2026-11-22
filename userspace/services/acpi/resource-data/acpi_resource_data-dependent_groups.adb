pragma Ada_2022;
package body ACPI_Resource_Data.Dependent_Groups with SPARK_Mode is
   use type Firmware_Tables.Byte;
   Start_Name : constant Item_Name := 6;
   End_Name : constant Item_Name := 7;
   End_Tag_Name : constant Item_Name := 15;
   Reserved_Mask : constant Firmware_Tables.Byte := 16#F0#;
   Priority_Mask : constant Firmware_Tables.Byte := 3;
   Performance_Divisor : constant Firmware_Tables.Byte := 4;
   function Is_Item (Item : Frame; Name : Item_Name) return Boolean is
     (Item.Status = Decoded and then Item.Encoding = Small_Item and then Item.Name = Name);
   procedure Analyze (Data : Firmware_Tables.Bytes; Alternatives : out Alternative_Array;
      Result : out Analysis_Result; Maximum_Items : Item_Count := Default_Item_Limit) is
      Framing : constant Scan_Result := Scan (Data, Maximum_Items);
      Cursor : Byte_Offset := 0;
      Open_Group, End_Seen : Boolean := False;
      Shape : Layout;
      subtype Grammar_Error is Analysis_Status range Bad_Dependent_Length .. Alternative_Capacity;
      procedure Reject (Status : Grammar_Error; Position : Byte_Offset) with
        Pre => not Result'Constrained,
        Post => Result.Status = Status and then Result.Position = Position
          and then (for all Slot in Alternatives'Range =>
            Alternatives (Slot) = Alternative'(others => <>));
      procedure Reject (Status : Grammar_Error; Position : Byte_Offset) is
      begin
         Alternatives := [others => <>];
         Result := (Status => Status, Position => Position);
      end Reject;
   begin
      Alternatives := [others => <>];
      if Framing.Status /= Complete then
         Result := (Status => Framing_Error, Position => Framing.Position, Cause => Framing.Status);
         return;
      end if;
      -- One grammar pass after the complete framing scan. No callback exposes
      -- partial slots; every error resets the entire caller-sized output.
      loop
         pragma Loop_Invariant (Cursor <= Byte_Offset (Data'Length));
         pragma Loop_Invariant (Shape.Alternatives <= Item_Count (Alternatives'Length));
         pragma Loop_Invariant (Shape.Alternatives <= Item_Count (Cursor));
         pragma Loop_Invariant (Shape.Common_Before <= Item_Count (Cursor));
         pragma Loop_Invariant (Shape.Common_After <= Item_Count (Cursor));
         pragma Loop_Invariant (Shape.Group_First <= Cursor and then Shape.Group_Limit <= Cursor);
         pragma Loop_Invariant (if not Open_Group then Shape.Group_First <= Shape.Group_Limit);
         pragma Loop_Invariant (if Open_Group then Shape.Alternatives > 0 and then not End_Seen);
         pragma Loop_Invariant (for all Slot in Alternatives'Range =>
           (if Item_Count (Slot - Alternatives'First) < Shape.Alternatives then
              Valid_Span (Alternatives (Slot), Cursor)
            else Alternatives (Slot) = Alternative'(others => <>)));
         pragma Loop_Invariant (if Open_Group then
           Alternatives (Alternatives'First + (Natural (Shape.Alternatives) - 1)).Limit = Cursor);
         pragma Loop_Variant (Decreases => Byte_Offset (Data'Length) - Cursor);
         declare
            Item : constant Frame := Read_Frame (Data, Cursor);
         begin
            if Item.Status /= Decoded then
               Alternatives := [others => <>];
               Result := (Status => Framing_Error, Position => Cursor, Cause => Malformed_Frame);
               return;
            end if;
            if Is_Item (Item, End_Tag_Name) then
               if Open_Group then Reject (Missing_End, Cursor); return; end if;
               Shape.End_Tag := Cursor;
               exit;
            elsif Is_Item (Item, Start_Name) then
               if Item.Length not in 0 .. 1 then Reject (Bad_Dependent_Length, Cursor); return; end if;
               if End_Seen then Reject (Start_After_End, Cursor); return; end if;
               declare
                  Info : Firmware_Tables.Byte := 0;
               begin
                  if Item.Length = 1 then
                     Info := Data (Data'First + Natural (Item.Payload));
                     if (Info and Reserved_Mask) /= 0 then Reject (Reserved_Priority_Bits, Cursor); return; end if;
                     if (Info and Priority_Mask) = Priority_Mask
                       or else ((Info / Performance_Divisor) and Priority_Mask) = Priority_Mask then
                        Reject (Reserved_Priority, Cursor); return;
                     end if;
                  end if;
                  if Shape.Alternatives >= Item_Count (Alternatives'Length) then
                     Reject (Alternative_Capacity, Cursor); return;
                  end if;
                  if Shape.Alternatives = 0 then Shape.Group_First := Cursor; end if;
                  Shape.Alternatives := Shape.Alternatives + 1;
                  declare
                     Slot : constant Positive := Alternatives'First + (Natural (Shape.Alternatives) - 1);
                  begin
                     Alternatives (Slot) := (Marker => Cursor, First | Limit => Item.Next_Offset,
                       Items => 0, Information_Present => Item.Length = 1, Information => Info,
                       Compatibility | Performance => Acceptable);
                     if Item.Length = 1 then
                        Alternatives (Slot).Compatibility := Priority'Val (Natural (Info and Priority_Mask));
                        Alternatives (Slot).Performance := Priority'Val (Natural ((Info / Performance_Divisor) and Priority_Mask));
                     end if;
                  end;
               end;
               Open_Group := True;
            elsif Is_Item (Item, End_Name) then
               if Item.Length /= 0 then Reject (Bad_Dependent_Length, Cursor); return; end if;
               if End_Seen then Reject (Repeated_End, Cursor); return; end if;
               if not Open_Group then Reject (End_Without_Start, Cursor); return; end if;
               End_Seen := True;
               Open_Group := False;
               Shape.Group_Limit := Item.Next_Offset;
            elsif Open_Group then
               declare
                  Slot : constant Positive := Alternatives'First + (Natural (Shape.Alternatives) - 1);
               begin
                  Alternatives (Slot).Items := Alternatives (Slot).Items + 1;
                  Alternatives (Slot).Limit := Item.Next_Offset;
               end;
            elsif End_Seen then
               Shape.Common_After := Shape.Common_After + 1;
            else
               Shape.Common_Before := Shape.Common_Before + 1;
            end if;
            Cursor := Item.Next_Offset;
         end;
      end loop;
      Result := (Status => Grouped, Position => Shape.End_Tag, Value => Shape);
   end Analyze;
end ACPI_Resource_Data.Dependent_Groups;
