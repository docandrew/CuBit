pragma Ada_2022;
package body HID_Report_Metadata with SPARK_Mode is
   use type Schema.Status;
   use type Schema.Count;
   use type Schema.Word;
   use type Schema.Byte;
   use type Schema.Numeric_Value;
   use type Schema.Report_ID;
   use type Bindings.Status;
   use type Bindings.Work_Count;
   use type Values.Status;
   Touchpad_Usage : constant Schema.Word := 16#000D_0005#;
   Finger_Usage : constant Schema.Word := 16#000D_0022#;
   Count_Usage : constant Schema.Word := 16#000D_0054#;
   Time_Usage : constant Schema.Word := 16#000D_0056#;
   Button_Usage_Base : constant Schema.Word := 16#0009_0000#;
   Application_Collection : constant Schema.Byte := 1;
   Logical_Collection : constant Schema.Byte := 2;
   Relative_Flag : constant Schema.Word := 4;
   SI_Seconds : constant Schema.Word := 16#1001#;
   Tick_Exponent : constant := -4;
   Last_Scan_Time : constant := 65_535;
   Maximum_Scalar_Bits : constant := 32;
   procedure Compile
     (Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Touchpad : Schema.Count; Budget : Bindings.Work_Count;
      Item : out Report_Layout; Charged : out Bindings.Work_Count;
      Outcome : out Compile_Status)
   is
      Empty, Candidate : Report_Layout;
      Found : Schema.Count;
      Used : Bindings.Work_Count;
      Status : Bindings.Status;
      Failed : Boolean := False;
      procedure Refuse (Why : Compile_Status) is
      begin Outcome := Why; Failed := True; end Refuse;
      procedure Find_Part (Which : Part_Kind; Usage : Schema.Word; Optional : Boolean := False) is
         B : Bindings.Binding;
         Current : Schema.Count;
         C : Schema.Collection;
      begin
         if Failed then return; end if;
         Bindings.Find (Layout, Fields, Usages, Collections, Touchpad, Usage,
                        Budget - Charged, B, Used, Status); Charged := Charged + Used;
         case Status is
            when Bindings.Matched => null;
            when Bindings.Missing => if not Optional then Refuse (Missing_Field); end if; return;
            when Bindings.Ambiguous => Refuse (Ambiguous_Field); return;
            when Bindings.Budget_Exhausted => Refuse (Budget_Exhausted); return;
            when others => Refuse (Invalid_Tables); return;
         end case;
         Candidate.Parts (Which) := (True, Fields (Fields'First + Natural (B.Field_Number - 1)), B.Element);
         Current := Candidate.Parts (Which).Field.Collection_ID;
         while Current /= Touchpad loop
            if Current = 0 then Refuse (Unsupported_Collection); return; end if;
            if Charged = Budget then Refuse (Budget_Exhausted); return; end if;
            Charged := Charged + 1; C := Collections (Collections'First + Natural (Current - 1));
            if C.Kind = Application_Collection or else
               (C.Kind = Logical_Collection and then C.Usage = Finger_Usage)
            then Refuse (Unsupported_Collection); return; end if;
            Current := C.Parent;
         end loop;
         if Candidate.Parts (Which).Field.Size not in 1 .. Maximum_Scalar_Bits or else
            Candidate.Parts (Which).Field.Logical_Minimum < 0 or else
            (Candidate.Parts (Which).Field.Flags and Relative_Flag) /= 0
         then Refuse (Unsupported_Field); end if;
      end Find_Part;
   begin
      Item := Empty; Charged := 0; Outcome := Invalid_Schema;
      if Layout.Outcome /= Schema.Decoded then return; end if;
      Bindings.Ancestor (Layout, Collections, Touchpad, Touchpad_Usage, Application_Collection,
                         Budget, Found, Used, Status); Charged := Used;
      case Status is
         when Bindings.Matched => if Found /= Touchpad then Outcome := Unsupported_Collection; return; end if;
         when Bindings.Missing => Outcome := Unsupported_Collection; return;
         when Bindings.Budget_Exhausted => Outcome := Budget_Exhausted; return;
         when others => Outcome := Invalid_Tables; return;
      end case;
      if Collections (Collections'First + Natural (Touchpad - 1)).Parent /= 0 then Outcome := Unsupported_Collection; return; end if;
      Find_Part (Count_Part, Count_Usage); Find_Part (Time_Part, Time_Usage);
      Find_Part (Button_1, Button_Usage_Base + 1, True);
      Find_Part (Button_2, Button_Usage_Base + 2, True);
      Find_Part (Button_3, Button_Usage_Base + 3, True);
      if Failed then return; end if;
      declare T : constant Schema.Field_Span := Candidate.Parts (Time_Part).Field; begin
         if T.Unit /= SI_Seconds or else T.Unit_Exponent /= Tick_Exponent or else
            T.Logical_Minimum /= 0 or else T.Logical_Maximum > Last_Scan_Time or else
            T.Physical_Minimum /= T.Logical_Minimum or else T.Physical_Maximum /= T.Logical_Maximum
         then Outcome := Unsupported_Field; return; end if;
      end;
      for P in Part_Kind loop
         if Candidate.Parts (P).Present and then Candidate.Parts (P).Field.ID /= Candidate.Parts (Count_Part).Field.ID
         then Outcome := Split_Report; return; end if;
      end loop;
      Candidate.Report := Layout; Candidate.Valid := True; Item := Candidate; Outcome := Bound;
   end Compile;
   procedure Decode
     (Item : Report_Layout; Data : Schema.Bytes;
      Sample : out Metadata; Outcome : out Decode_Status)
   is
      Empty, Candidate : Metadata;
      Value : Schema.Numeric_Value;
      Failed : Boolean := False;
      procedure Read_Part (Which : Part_Kind) is
         Status : Values.Status;
      begin
         Values.Read_Element (Data, Item.Report, Item.Parts (Which).Field, Item.Parts (Which).Element, Value, Status);
         if Status /= Values.Decoded then
            Outcome := (if Status in Values.Report_ID_Mismatch | Values.Invalid_Length then Wrong_Report else Invalid_Data);
            Failed := True;
         end if;
      end Read_Part;
      Which : Part_Kind;
   begin
      Sample := Empty; Outcome := Unbound;
      if not Item.Valid then return; end if;
      Read_Part (Count_Part); if Failed then return; end if;
      if Value < 0 then Outcome := Invalid_Data; return; end if;
      Candidate.Contact_Count := Interfaces.Unsigned_32 (Value);
      Read_Part (Time_Part); if Failed then return; end if;
      if Value not in 0 .. Last_Scan_Time then Outcome := Invalid_Data; return; end if;
      Candidate.Scan_Time := Interfaces.Unsigned_16 (Value);
      for Button in Button_Index loop
         Which := Part_Kind'Val (Part_Kind'Pos (Button_1) + Integer (Button) - 1);
         if Item.Parts (Which).Present then
            Read_Part (Which); if Failed then return; end if;
            if Value not in 0 .. 1 then Outcome := Invalid_Data; return; end if;
            Candidate.Buttons (Button) := (Present => True, Pressed => Value = 1);
         end if;
      end loop;
      Sample := Candidate; Outcome := Sampled;
   end Decode;
end HID_Report_Metadata;
