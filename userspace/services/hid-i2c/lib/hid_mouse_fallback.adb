package body HID_Mouse_Fallback with SPARK_Mode is
   use type Schema.Byte, Schema.Count, Schema.Word, Schema.Status, Schema.Report_Kind, Schema.Report_ID;
   use type Schema.Numeric_Value, Schema.Bit_Count, Names.Status, Values.Status;
   procedure Compile
     (Layout : Schema.Result; Fields : Schema.Field_Table; Usages : Schema.Usage_Table;
      Collections : Schema.Collection_Table; Result : out Model)
   is
      Root : Schema.Count := 0;
      Usage : Schema.Word;
      Status : Names.Status;
      Seen_ID, Bad : Boolean := False;
      function In_Mouse (ID : Schema.Count) return Boolean is
         Current : Schema.Count := ID;
      begin
         for Step in 1 .. Collections'Length loop
            if Current = 0 or else Natural(Current) not in Collections'Range then return False; end if;
            if Current = Root then return True; end if;
            Current := Collections(Natural(Current)).Parent;
         end loop;
         return False;
      end In_Mouse;
      procedure Save (Target : in out Slot; F : Schema.Field_Span; Element : Schema.Bit_Count) is
      begin
         if Target.Present then Bad := True;
         else Target := (True,F,Element); end if;
      end Save;
   begin
      Result := (others => <>);
      if Layout.Outcome /= Schema.Decoded or else Fields'First /= 1 or else Collections'First /= 1
        or else Natural(Layout.Fields_Required) > Fields'Length
        or else Natural(Layout.Collections_Required) > Collections'Length then return; end if;
      for Index in 1 .. Natural(Layout.Collections_Required) loop
         if Collections(Index).Parent = 0 and then Collections(Index).Kind = 1
           and then Collections(Index).Usage = 16#0001_0002# then
            if Root /= 0 then return; end if; Root := Schema.Count(Index);
         end if;
      end loop;
      if Root = 0 then return; end if;
      for Index in 1 .. Natural(Layout.Fields_Required) loop
         declare F : constant Schema.Field_Span := Fields(Index); begin
            if F.Kind = Schema.Input and then (F.Flags and 1) = 0 and then In_Mouse(F.Collection_ID) then
               if F.Elements > 512 then return; end if;
               if Seen_ID and then F.ID /= Result.ID then return; end if;
               Result.ID := F.ID; Seen_ID := True;
               for Element in 1 .. Natural(F.Elements) loop
                  Names.Resolve(Layout,F,Usages,Schema.Bit_Count(Element-1),Usage,Status);
                  if Status = Names.Resolved then
                     case Usage is
                        when 16#0001_0030# | 16#0001_0031# =>
                           if (F.Flags and 4) = 0 or else F.Logical_Minimum < -2047 or else F.Logical_Maximum > 2047 then return; end if;
                           if Usage = 16#0001_0030# then Save(Result.X,F,Schema.Bit_Count(Element-1));
                           else Save(Result.Y,F,Schema.Bit_Count(Element-1)); end if;
                        when 16#0001_0038# =>
                           if (F.Flags and 4) = 0 or else F.Logical_Minimum < -128 or else F.Logical_Maximum > 127 then return; end if;
                           Save(Result.Wheel,F,Schema.Bit_Count(Element-1));
                        when 16#0009_0001# .. 16#0009_0005# =>
                           if F.Logical_Minimum /= 0 or else F.Logical_Maximum /= 1 then return; end if;
                           Save(Result.Buttons(Positive(Usage-16#0009_0000#)),F,Schema.Bit_Count(Element-1));
                        when others => null;
                     end case;
                  end if;
               end loop;
            end if;
         end;
      end loop;
      Result.Ready := Seen_ID and then not Bad and then Result.X.Present and then Result.Y.Present;
   end Compile;
   procedure Decode
     (Plan : Model; Layout : Schema.Result; Data : Schema.Bytes;
      Matched, Accepted : out Boolean; Report : out Pointer_Pending.Report)
   is
      Value : Schema.Numeric_Value;
      Status : Values.Status;
      Good : Boolean := True;
      procedure Read (Item : Slot) is
      begin
         Value := 0;
         if Item.Present then
            Values.Read_Element(Data,Layout,Item.Field,Item.Element,Value,Status);
            if Status /= Values.Decoded then Good := False; end if;
         end if;
      end Read;
   begin
      Matched := False; Accepted := False; Report := (Buttons => 0, X => 0, Y => 0, Wheel => 0, Flags => 0);
      if not Plan.Ready or else Data'Length = 0 then return; end if;
      if Layout.Has_Report_IDs and then Schema.Report_ID(Data(Data'First)) /= Plan.ID then return; end if;
      Matched := True;
      Read(Plan.X);if Value not in -2047 .. 2047 then return;end if;Report.X := Pointer_Pending.Displacement(Value);
      Read(Plan.Y);if Value not in -2047 .. 2047 then return;end if;Report.Y := Pointer_Pending.Displacement(-Value);
      Read(Plan.Wheel);if Value not in -128 .. 127 then return;end if;Report.Wheel := Pointer_Pending.Wheel_Steps(Value);
      for Index in Plan.Buttons'Range loop
         Read(Plan.Buttons(Index));
         if Value = 1 then Report.Buttons := Report.Buttons or Pointer_Pending.Button_State(2 ** (Index-1)); end if;
      end loop;
      Accepted := Good;
      if not Accepted then Report := (Buttons => 0, X => 0, Y => 0, Wheel => 0, Flags => 0);end if;
   end Decode;
end HID_Mouse_Fallback;
