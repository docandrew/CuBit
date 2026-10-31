pragma Ada_2022;
package body HID_Report_Schema with SPARK_Mode is
   use type Byte;
   use type Word;
   use type Wide;
   Usage_Page_Base : constant Word := 2 ** 16;
   Long_Prefix : constant Byte := 16#FE#;
   Variable_Bit : constant Word := 2;
   type Math is range -(2 ** 32) .. 2 ** 34;
   type Global_State is record
      Page, Size, Elements, Unit : Word := 0;
      ID : Report_ID := 0;
      Logical_Min, Physical_Min : Numeric_Value := 0;
      Logical_Max, Physical_Max : Word := 0;
      Logical_Width, Physical_Width : Natural range 0 .. 4 := 0;
      Exponent : Integer range -8 .. 7 := 0;
   end record;
   type Global_Stack is array (Positive range <>) of Global_State;
   type Collection_Stack is array (Positive range <>) of Count;
   function Signed (Value : Word; Width : Natural) return Numeric_Value is
      Limit : Math;
   begin
      if Width = 0 then return 0; end if;
      Limit := 2 ** (Width * 8);
      return Numeric_Value (if Math (Value) >= Limit / 2 then Math (Value) - Limit else Math (Value));
   end Signed;
   function Maximum (Value : Word; Width : Natural; Minimum : Numeric_Value) return Numeric_Value is
     (if Minimum < 0 then Signed (Value, Width) else Numeric_Value (Value));
   procedure Parse
     (Data : Bytes; Fields : out Field_Table; Usages : out Usage_Table;
      Collections : out Collection_Table; Info : out Result)
   is
      State : Global_State;
      Saved : Global_Stack (1 .. Maximum_Global_Depth);
      Parents : Collection_Stack (1 .. Maximum_Collection_Depth) := [others => 0];
      Global_Depth, Collection_Depth : Natural := 0;
      Current_Collection, F_Total, U_Total, C_Total, Local_Count, Range_Index : Count := 0;
      First_Local, Range_Minimum : Word := 0;
      First_Short, Range_Short, Range_Pending : Boolean := False;
      Delimiter_Open, Preferred_Seen, Had_Delimiter : Boolean := False;
      Length, Position, Start : Natural := 0;
      Prefix : Byte;
      Width, Tag, Category : Natural;
      Value, Factor, Complete_Usage : Word;
      Kind : Report_Kind;
      Span, Maximum_Value, Physical_Low, Physical_High : Numeric_Value;
      Failed : Boolean := False;
      function Get (At_Offset : Natural) return Byte is (Data (Data'First + At_Offset));
      function Resolve (Raw : Word; Short : Boolean) return Word is
        (if Short then State.Page * Usage_Page_Base + Raw else Raw);
      procedure Fail (Why : Status) is
      begin Info.Outcome := Why; Info.At_Byte := Count (Start); Failed := True; end Fail;
      procedure Append_Usage (Low, High : Word; Short : Boolean) is
      begin
         U_Total := U_Total + 1; Local_Count := Local_Count + 1;
         if Local_Count = 1 then First_Local := Low; First_Short := Short; end if;
         if Wide (U_Total) <= Wide (Usages'Length) then
            Usages (Usages'First + Natural (U_Total - 1)) := (Low, High, Short);
         end if;
      end Append_Usage;
      procedure Flush_Local is
      begin
         if Local_Count > 0 then
            for N in Count range U_Total - Local_Count + 1 .. U_Total loop
               if Wide (N) <= Wide (Usages'Length) then
                  declare P : constant Positive := Usages'First + Natural (N - 1); begin
                     Usages (P).First := Resolve (Usages (P).First, Usages (P).Uses_Current_Page);
                     Usages (P).Last := Resolve (Usages (P).Last, Usages (P).Uses_Current_Page);
                     Usages (P).Uses_Current_Page := False;
                  end;
               end if;
            end loop;
         end if;
      end Flush_Local;
      procedure Clear_Local is
      begin
         Local_Count := 0; First_Local := 0; First_Short := False;
         Range_Pending := False; Delimiter_Open := False;
         Preferred_Seen := False; Had_Delimiter := False;
      end Clear_Local;
   begin
      Fields := [others => (Kind => Input, ID => 0, Offset => 0, Size => 0,
         Elements => 0, Flags => 0, Collection_ID => 0, Usage_First => 0,
         Usage_Terms => 0, Logical_Minimum => 0, Logical_Maximum => 0,
         Physical_Minimum => 0, Physical_Maximum => 0, Unit => 0,
         Unit_Exponent => 0)];
      Usages := [others => (First => 0, Last => 0, Uses_Current_Page => False)];
      Collections := [others => (Parent => 0, Kind => 0, Usage => 0)];
      Info := (Outcome => Empty_Descriptor, At_Byte => 0, Fields_Required => 0,
         Usages_Required => 0, Collections_Required => 0, Has_Report_IDs => False,
         Bits => [others => [others => 0]], Present => [others => [others => False]]);
      if Wide (Data'Length) = 0 then return; end if;
      if Wide (Data'Length) > Maximum_Descriptor_Bytes then Info.Outcome := Descriptor_Too_Long; return; end if;
      Length := Data'Length;
      while Position < Length loop
         Start := Position; Prefix := Get (Position); Position := Position + 1;
         if Prefix = Long_Prefix then
            if Length - Position < 2 then Fail (Truncated_Item); return; end if;
            if Natural (Get (Position)) > Length - Position - 2 then Fail (Truncated_Item);
            else Fail (Unsupported_Long_Item); end if;
            return;
         end if;
         Width := Natural (Prefix mod 4); if Width = 3 then Width := 4; end if;
         Category := Natural ((Prefix / 4) mod 4); Tag := Natural (Prefix / 16);
         if Width > Length - Position then Fail (Truncated_Item); return; end if;
         Value := 0; Factor := 1;
         for N in Natural range 1 .. Width loop
            Value := Value + Word (Get (Position)) * Factor; Position := Position + 1;
            if N < Width then Factor := Factor * 256; end if;
         end loop;
         case Category is
            when 1 =>
               case Tag is
                  when 0 => if Value >= Usage_Page_Base then Fail (Invalid_Global); else State.Page := Value; end if;
                  when 1 => State.Logical_Min := Signed (Value, Width);
                  when 2 => State.Logical_Max := Value; State.Logical_Width := Width;
                  when 3 => State.Physical_Min := Signed (Value, Width);
                  when 4 => State.Physical_Max := Value; State.Physical_Width := Width;
                  when 5 =>
                     Span := (if Value <= 15 then Numeric_Value (Value) else Signed (Value, Width));
                     if Span in 8 .. 15 then Span := Span - 16; end if;
                     if Span not in -8 .. 7 then Fail (Invalid_Global); else State.Exponent := Integer (Span); end if;
                  when 6 => State.Unit := Value;
                  when 7 => State.Size := Value;
                  when 8 =>
                     if Value not in 1 .. Word (Report_ID'Last) then Fail (Invalid_Report_ID);
                     else
                        for K in Report_Kind loop
                           if Info.Present (K, 0) then Fail (Mixed_Report_IDs); end if;
                        end loop;
                        State.ID := Report_ID (Value); Info.Has_Report_IDs := True;
                     end if;
                  when 9 => State.Elements := Value;
                  when 10 =>
                     if Width /= 0 then Fail (Invalid_Global);
                     elsif Global_Depth = Maximum_Global_Depth then Fail (Depth_Refused);
                     else Global_Depth := Global_Depth + 1; Saved (Global_Depth) := State; end if;
                  when 11 =>
                     if Width /= 0 then Fail (Invalid_Global);
                     elsif Global_Depth = 0 then Fail (Global_Stack_Underflow);
                     else State := Saved (Global_Depth); Global_Depth := Global_Depth - 1; end if;
                  when others => Fail (Unsupported_Item);
               end case;
            when 2 =>
               if Tag = 10 then
                  Had_Delimiter := True;
                  if Value = 1 and then not Delimiter_Open then Delimiter_Open := True; Preferred_Seen := False;
                  elsif Value = 0 and then Delimiter_Open then Delimiter_Open := False;
                  else Fail (Invalid_Delimiter); end if;
               elsif Tag in 0 .. 2 then
                  Complete_Usage := Value;
                  if Width /= 4 and then Value >= Usage_Page_Base then Fail (Invalid_Usage_Range);
                  elsif Tag = 0 then
                     if not Delimiter_Open or else not Preferred_Seen then
                        Append_Usage (Complete_Usage, Complete_Usage, Width /= 4); Preferred_Seen := Delimiter_Open;
                     end if;
                  elsif Delimiter_Open then Fail (Unsupported_Delimiter_Range);
                  elsif Tag = 1 then
                     if Range_Pending then Fail (Invalid_Usage_Range);
                     else
                        Range_Minimum := Value; Range_Short := Width /= 4; Range_Pending := True;
                        Append_Usage (Value, Value, Range_Short); Range_Index := U_Total;
                     end if;
                  elsif not Range_Pending or else Range_Short /= (Width /= 4) or else
                     Value < Range_Minimum or else Value / Usage_Page_Base /= Range_Minimum / Usage_Page_Base
                  then Fail (Invalid_Usage_Range);
                  else
                     if Wide (Range_Index) <= Wide (Usages'Length) then
                        Usages (Usages'First + Natural (Range_Index - 1)).Last := Value;
                     end if;
                     Range_Pending := False;
                  end if;
               elsif Tag not in 3 .. 5 | 7 .. 9 then Fail (Unsupported_Item);
               end if;
            when 0 =>
               if Range_Pending then Fail (Invalid_Usage_Range);
               elsif Delimiter_Open then Fail (Invalid_Delimiter);
               else
                  Flush_Local;
                  case Tag is
                     when 10 =>
                        if Value > 255 then Fail (Invalid_Collection);
                        elsif Had_Delimiter and then Value = 1 then Fail (Invalid_Delimiter);
                        elsif Collection_Depth = Maximum_Collection_Depth then Fail (Depth_Refused);
                        else
                           C_Total := C_Total + 1;
                           if Wide (C_Total) <= Wide (Collections'Length) then
                              Collections (Collections'First + Natural (C_Total - 1)) :=
                                (Current_Collection, Byte (Value), Resolve (First_Local, First_Short));
                           end if;
                           Collection_Depth := Collection_Depth + 1; Parents (Collection_Depth) := Current_Collection;
                           Current_Collection := C_Total;
                        end if;
                     when 12 =>
                        if Width /= 0 or else Collection_Depth = 0 then Fail (Invalid_Collection);
                        else Current_Collection := Parents (Collection_Depth); Collection_Depth := Collection_Depth - 1; end if;
                     when 8 | 9 | 11 =>
                        Kind := (if Tag = 8 then Input elsif Tag = 9 then Output else Feature);
                        if Had_Delimiter and then (Value and Variable_Bit) = 0 then Fail (Invalid_Delimiter);
                        elsif Info.Has_Report_IDs and then State.ID = 0 then Fail (Mixed_Report_IDs);
                        elsif State.Size = 0 or else State.Elements = 0 then Fail (Invalid_Geometry);
                        elsif State.Size > Maximum_Report_Bits or else State.Elements > Maximum_Report_Bits or else
                           Wide (State.Size) * Wide (State.Elements) > Wide (Maximum_Report_Bits - Info.Bits (Kind, State.ID))
                        then Fail (Report_Too_Large);
                        else
                           Maximum_Value := Maximum (State.Logical_Max, State.Logical_Width, State.Logical_Min);
                           if Maximum_Value < State.Logical_Min then Fail (Invalid_Value_Range);
                           else
                              Physical_Low := State.Physical_Min;
                              Physical_High := Maximum (State.Physical_Max, State.Physical_Width, State.Physical_Min);
                              if Physical_Low = 0 and then Physical_High = 0 then
                                 Physical_Low := State.Logical_Min; Physical_High := Maximum_Value;
                              end if;
                              F_Total := F_Total + 1;
                              if Wide (F_Total) <= Wide (Fields'Length) then
                                 Fields (Fields'First + Natural (F_Total - 1)) :=
                                   (Kind, State.ID, Info.Bits (Kind, State.ID), Bit_Count (State.Size),
                                    Bit_Count (State.Elements), Value, Current_Collection,
                                    (if Local_Count = 0 then 0 else U_Total - Local_Count + 1), Local_Count,
                                    State.Logical_Min, Maximum_Value, Physical_Low, Physical_High,
                                    State.Unit, State.Exponent);
                              end if;
                              Info.Bits (Kind, State.ID) := Info.Bits (Kind, State.ID) +
                                Bit_Count (Wide (State.Size) * Wide (State.Elements));
                              Info.Present (Kind, State.ID) := True;
                           end if;
                        end if;
                     when others => Fail (Unsupported_Item);
                  end case;
                  Clear_Local;
               end if;
            when others => Fail (Unsupported_Item);
         end case;
         if Failed then return; end if;
      end loop;
      Start := Position;
      if Collection_Depth /= 0 then Fail (Unbalanced_Collection);
      elsif Global_Depth /= 0 then Fail (Unbalanced_Global_Stack);
      elsif Range_Pending then Fail (Invalid_Usage_Range);
      elsif Delimiter_Open then Fail (Invalid_Delimiter);
      elsif Local_Count /= 0 then Fail (Dangling_Local_Items);
      elsif F_Total = 0 then Fail (Empty_Descriptor);
      else
         Info.Fields_Required := F_Total; Info.Usages_Required := U_Total; Info.Collections_Required := C_Total;
         Info.Outcome := (if Wide (F_Total) > Wide (Fields'Length) or else Wide (U_Total) > Wide (Usages'Length) or else
                            Wide (C_Total) > Wide (Collections'Length) then Need_Capacity else Decoded);
         Info.At_Byte := Count (Position);
      end if;
   end Parse;
end HID_Report_Schema;
