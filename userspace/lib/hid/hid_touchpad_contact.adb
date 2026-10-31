pragma Ada_2022;
package body HID_Touchpad_Contact with SPARK_Mode is
   use type Schema.Status;
   use type Schema.Byte;
   use type Schema.Count;
   use type Schema.Word;
   use type Schema.Numeric_Value;
   use type Schema.Report_ID;
   use type Bindings.Status;
   use type Bindings.Work_Count;
   use type Values.Status;
   Finger_Usage : constant Schema.Word := 16#000D_0022#;
   Touchpad_Usage : constant Schema.Word := 16#000D_0005#;
   Tip_Usage : constant Schema.Word := 16#000D_0042#;
   Contact_ID_Usage : constant Schema.Word := 16#000D_0051#;
   X_Usage : constant Schema.Word := 16#0001_0030#;
   Y_Usage : constant Schema.Word := 16#0001_0031#;
   Confidence_Usage : constant Schema.Word := 16#000D_0047#;
   Logical_Collection : constant Schema.Byte := 2;
   Application_Collection : constant Schema.Byte := 1;
   Relative_Flag : constant Schema.Word := 4;
   Maximum_Scalar_Bits : constant := 32;
   procedure Compile
     (Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Finger : Schema.Count; Budget : Bindings.Work_Count;
      Item : out Contact_Layout; Charged : out Bindings.Work_Count;
      Outcome : out Compile_Status)
   is
      Empty, Candidate : Contact_Layout;
      Found : Schema.Count;
      Used : Bindings.Work_Count;
      Status : Bindings.Status;
      Failed : Boolean := False;
      procedure Refuse (Why : Compile_Status) is
      begin Outcome := Why; Failed := True; end Refuse;
      procedure Check_Ancestor (Usage : Schema.Word; Kind : Schema.Byte; Exact : Boolean) is
      begin
         Bindings.Ancestor (Layout, Collections, Finger, Usage, Kind, Budget - Charged, Found, Used, Status);
         Charged := Charged + Used;
         case Status is
            when Bindings.Matched =>
               if Exact and then Found /= Finger then Refuse (Unsupported_Collection); end if;
            when Bindings.Missing => Refuse (Unsupported_Collection);
            when Bindings.Budget_Exhausted => Refuse (Budget_Exhausted);
            when others => Refuse (Invalid_Tables);
         end case;
      end Check_Ancestor;
      procedure Find_Part (Which : Part_Kind; Usage : Schema.Word; Optional : Boolean := False) is
         B : Bindings.Binding;
         Current : Schema.Count;
         C : Schema.Collection;
      begin
         if Failed then return; end if;
         Bindings.Find (Layout, Fields, Usages, Collections, Finger, Usage,
                        Budget - Charged, B, Used, Status);
         Charged := Charged + Used;
         case Status is
            when Bindings.Matched => null;
            when Bindings.Missing => if not Optional then Refuse (Missing_Field); end if; return;
            when Bindings.Ambiguous => Refuse (Ambiguous_Field); return;
            when Bindings.Budget_Exhausted => Refuse (Budget_Exhausted); return;
            when others => Refuse (Invalid_Tables); return;
         end case;
         Candidate.Parts (Which) := (Fields (Fields'First + Natural (B.Field_Number - 1)), B.Element);
         Current := Candidate.Parts (Which).Field.Collection_ID;
         while Current /= Finger loop
            if Current = 0 then Refuse (Unsupported_Collection); return; end if;
            if Charged = Budget then Refuse (Budget_Exhausted); return; end if;
            Charged := Charged + 1;
            C := Collections (Collections'First + Natural (Current - 1));
            if C.Kind = Application_Collection or else
               (C.Kind = Logical_Collection and then C.Usage = Finger_Usage)
            then Refuse (Unsupported_Collection); return; end if;
            Current := C.Parent;
         end loop;
         if Candidate.Parts (Which).Field.Size not in 1 .. Maximum_Scalar_Bits or else
            (Candidate.Parts (Which).Field.Flags and Relative_Flag) /= 0 then Refuse (Unsupported_Field); return; end if;
         if Which = Confidence_Part then Candidate.Has_Confidence := True; end if;
      end Find_Part;
   begin
      Item := Empty; Charged := 0; Outcome := Invalid_Schema;
      if Layout.Outcome /= Schema.Decoded then return; end if;
      Check_Ancestor (Finger_Usage, Logical_Collection, True); if Failed then return; end if;
      Check_Ancestor (Touchpad_Usage, Application_Collection, False); if Failed then return; end if;
      Find_Part (Tip_Part, Tip_Usage); Find_Part (ID_Part, Contact_ID_Usage);
      Find_Part (X_Part, X_Usage); Find_Part (Y_Part, Y_Usage);
      Find_Part (Confidence_Part, Confidence_Usage, True);
      if Failed then return; end if;
      if Candidate.Parts (ID_Part).Field.Logical_Minimum < 0 then Outcome := Unsupported_Field; return; end if;
      for P in Part_Kind loop
         if (P /= Confidence_Part or else Candidate.Has_Confidence) and then
            Candidate.Parts (P).Field.ID /= Candidate.Parts (Tip_Part).Field.ID then Outcome := Split_Report; return; end if;
      end loop;
      Candidate.Report := Layout; Candidate.Valid := True; Item := Candidate; Outcome := Bound;
   end Compile;
   procedure Decode
     (Item : Contact_Layout; Data : Schema.Bytes;
      Sample : out Contact; Outcome : out Decode_Status)
   is
      Empty, Candidate : Contact;
      Value : Schema.Numeric_Value;
      Failed : Boolean := False;
      procedure Read_Part (Which : Part_Kind) is
         Status : Values.Status;
      begin
         Values.Read_Element (Data, Item.Report, Item.Parts (Which).Field,
                              Item.Parts (Which).Element, Value, Status);
         if Status /= Values.Decoded then
            Outcome := (if Status in Values.Report_ID_Mismatch | Values.Invalid_Length then Wrong_Report else Invalid_Data);
            Failed := True;
         end if;
      end Read_Part;
   begin
      Sample := Empty; Outcome := Unbound;
      if not Item.Valid then return; end if;
      Read_Part (Tip_Part); if Failed then return; end if;
      if Value not in 0 .. 1 then Outcome := Invalid_Data; return; end if;
      Candidate.Tip := Value = 1;
      Read_Part (ID_Part); if Failed then return; end if;
      if Value < 0 then Outcome := Invalid_Data; return; end if;
      Candidate.ID := Schema.Word (Value);
      if Candidate.Tip then
         Read_Part (X_Part); if Failed then return; end if; Candidate.X := Value;
         Read_Part (Y_Part); if Failed then return; end if; Candidate.Y := Value;
         Candidate.Has_Position := True;
         if Item.Has_Confidence then
            Read_Part (Confidence_Part); if Failed then return; end if;
            if Value not in 0 .. 1 then Outcome := Invalid_Data; return; end if;
            Candidate.Confidence := (if Value = 1 then Confident else Not_Confident);
         end if;
      end if;
      Sample := Candidate; Outcome := Sampled;
   end Decode;
end HID_Touchpad_Contact;
