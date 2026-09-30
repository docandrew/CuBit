with CCL.Diagnostics;
with CCL.Types;

package body CCL.Sessions with SPARK_Mode is
   use type CCL.Language.Interpretation_Status;
   use type CCL.Language.Diagnostic_Code;
   use type CCL.VM.Value_Kind;

   function Slot (First : History_Index; Offset : History_Count) return History_Index is
     (1 + (First - 1 + Offset) mod Maximum_History);

   procedure Initialize (Item : out Session) is
   begin
      Item := (others => <>);
      CCL.Catalog.Initialize (Item.Catalog);
   end Initialize;

   procedure Initialize
     (Item : out Session; Visible_Interfaces : CCL.Catalog.Interface_Catalog) is
   begin
      Item := (Catalog => Visible_Interfaces, others => <>);
   end Initialize;

   procedure Clear_History (Item : in out Session) is
   begin
      Item.Entries := [others => <>];
      Item.Count := 0;
      Item.Oldest := 1;
   end Clear_History;

   function Length (Item : Session) return History_Count is (Item.Count);

   procedure Complete
     (Item : Session; Prefix : String;
      Matches : out CCL.Catalog.Completion.Match_List) is
   begin
      CCL.Catalog.Completion.Find (Item.Catalog, Prefix, Matches);
   end Complete;

   procedure Describe
     (Item : Session; Name : String;
      Operation : out CCL.Catalog.Resolved_Operation; Found : out Boolean) is
   begin
      CCL.Catalog.Resolve (Item.Catalog, Name, Operation, Found);
   end Describe;

   procedure Recall
     (Item : Session; Index : History_Index; Entry_Value : out Submission;
      Found : out Boolean) is
   begin
      Found := Index <= Item.Count;
      Entry_Value := (others => <>);
      if Found then Entry_Value := Item.Entries (Slot (Item.Oldest, Index - 1)); end if;
   end Recall;

   procedure Record_Submission
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Outcome : CCL.Language.Interpretation_Result)
   is
      Value : Submission;
      Target : History_Index;
   begin
      Value.Fuel := Fuel;
      Value.Source_Length := Natural'Min (Source'Length, Value.Source'Length);
      Value.Source_Truncated := Source'Length > Value.Source'Length;
      if Value.Source_Length > 0 then
         Value.Source (1 .. Value.Source_Length) :=
           Source (Source'First .. Source'First + (Value.Source_Length - 1));
      end if;
      Value.Outcome := Outcome;
      Target := Slot (Item.Oldest, Item.Count);
      Item.Entries (Target) := Value;
      if Item.Count = Maximum_History then
         Item.Oldest := Slot (Item.Oldest, 1);
      else
         Item.Count := Item.Count + 1;
      end if;
   end Record_Submission;

   procedure Submit
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Outcome : out CCL.Language.Interpretation_Result) is
   begin
      CCL.Language.Interpret (Source, Fuel, Item.Catalog, Outcome);
      Record_Submission (Item, Source, Fuel, Outcome);
   end Submit;

   procedure Submit_With_Host
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result)
   is
      procedure Evaluate is new CCL.Language.Interpret_With_Host (Host_Context, Invoke);
   begin
      --  Evaluate the original source, never a truncated history copy.
      Evaluate (Source, Fuel, Item.Catalog, Grants, Context, Outcome);
      Record_Submission (Item, Source, Fuel, Outcome);
   end Submit_With_Host;

   procedure Submit_With_Values
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result)
   is
      procedure Evaluate is new CCL.Language.Interpret_With_Values (Host_Context, Invoke);
   begin
      Evaluate (Source, Fuel, Item.Catalog, Grants, Context, Outcome);
      Record_Submission (Item, Source, Fuel, Outcome);
   end Submit_With_Values;

   function Result_Type (Outcome : CCL.Language.Interpretation_Result)
     return CCL.Language.Static_Type is
   begin
      if Outcome.Status /= CCL.Language.Succeeded or else not Outcome.Has_Value then
         return CCL.Language.Invalid_Type;
      elsif Outcome.Has_List then return Outcome.List_Type;
      elsif Outcome.Has_Function then return CCL.Language.Invalid_Type;
      elsif Outcome.Has_Text then return CCL.Language.String_Type;
      elsif Outcome.Has_Character then return CCL.Language.Character_Type;
      elsif Outcome.Variant_Type in CCL.Types.Declared_Type then return Outcome.Variant_Type;
      elsif Outcome.Result_Value.Kind = CCL.VM.Integer_Value then return CCL.Language.Integer_Type;
      elsif Outcome.Result_Value.Kind = CCL.VM.Boolean_Value then return CCL.Language.Boolean_Type;
      else return CCL.Language.Invalid_Type;
      end if;
   end Result_Type;

   --  One line for a list result: List<Integer>: [10, 20, 30]. Strings are
   --  quoted; enumeration members show their position until results carry
   --  member names.
   function List_Image (Outcome : CCL.Language.Interpretation_Result) return String is
      use type CCL.Language.Static_Type;
      use type Interfaces.Integer_64;
      Maximum : constant := 2 * CCL.Language.MAX_TEXT_BYTES;
      Buffer : String (1 .. Maximum) := [others => ' '];
      Last : Natural range 0 .. Maximum := 0;
      Text_First : Positive := 1;
      Element_Type : constant CCL.Language.Static_Type := Outcome.List_Element_Type;
      procedure Add (Item : String) is
      begin
         if Item'Length <= Maximum - Last then
            Buffer (Last + 1 .. Last + Item'Length) := Item;
            Last := Last + Item'Length;
         end if;
      end Add;
      function Trimmed (Item : String) return String is
        (if Item'Length > 0 and then Item (Item'First) = ' '
         then Item (Item'First + 1 .. Item'Last) else Item);
   begin
      for I in 1 .. Outcome.List_Length loop
         if I > 1 then Add (", "); end if;
         if Element_Type = CCL.Language.String_Type then
            Add ('"' & Outcome.List_Text.Data
                   (Text_First .. Outcome.List_Text_Ends (I)) & '"');
            Text_First := Outcome.List_Text_Ends (I) + 1;
         elsif Element_Type = CCL.Language.Boolean_Type then
            Add ((if Outcome.List_Values (I).Boolean then "true" else "false"));
         elsif Element_Type = CCL.Language.Character_Type then
            Add ("'" & Character'Val (Natural (Outcome.List_Values (I).Integer mod 256)) & "'");
         elsif Element_Type = CCL.Language.Integer_Type then
            Add (Trimmed (Interfaces.Integer_64'Image (Outcome.List_Values (I).Integer)));
         else
            Add ("#" & Trimmed (Interfaces.Integer_64'Image (Outcome.List_Values (I).Integer)));
         end if;
      end loop;
      if Outcome.List_Total > Outcome.List_Length then
         Add ((if Outcome.List_Length > 0 then ", " else "") & "... " &
              Trimmed (Natural'Image (Outcome.List_Total - Outcome.List_Length)) & " more");
      end if;
      return "List<" &
        (if Element_Type = CCL.Language.String_Type then "String"
         elsif Element_Type = CCL.Language.Boolean_Type then "Boolean"
         elsif Element_Type = CCL.Language.Character_Type then "Character"
         elsif Element_Type = CCL.Language.Integer_Type then "Integer"
         else "enumeration") & ">: [" & Buffer (1 .. Last) & "]";
   end List_Image;

   function Result_Image (Outcome : CCL.Language.Interpretation_Result) return String is
   begin
      if Outcome.Status = CCL.Language.Host_Import_Required then
         return CCL.Diagnostics.Message (Outcome.Status);
      elsif Outcome.Status /= CCL.Language.Succeeded then
         return CCL.Diagnostics.Message (Outcome.Status) &
           (if Outcome.Diagnostic = CCL.Language.No_Diagnostic then ""
            else ": " & CCL.Diagnostics.Message (Outcome.Diagnostic)) &
           (if Outcome.Diagnostic_Position = 0 then ""
            else " at character" & Natural'Image (Outcome.Diagnostic_Position));
      end if;
      if Outcome.Has_List then
         return List_Image (Outcome);
      elsif Outcome.Has_Function then
         return "Function: " & CCL.Types.Image (Outcome.Function_Name);
      end if;
      case Result_Type (Outcome) is
         when CCL.Language.Integer_Type =>
            return "Integer:" & Interfaces.Integer_64'Image (Outcome.Result_Value.Integer);
         when CCL.Language.Boolean_Type =>
            return "Boolean: " & (if Outcome.Result_Value.Boolean then "true" else "false");
         when CCL.Language.String_Type =>
            return "String: " & Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length);
         when CCL.Language.Character_Type =>
            return "Character: " & Outcome.Result_Character;
         when CCL.Language.Invalid_Type => return "ok";
         when CCL.Language.Handler_Type => return "Handler";
         when CCL.Language.Unit_Type => return "Unit";
         when CCL.Types.Declared_Type =>
            return CCL.Types.Image (Outcome.Variant_Type_Name) & "." &
              CCL.Types.Image (Outcome.Variant_Member_Name) &
              (case Outcome.Variant_Payload_Type is
                when CCL.Language.Integer_Type => "(" & Interfaces.Integer_64'Image (Outcome.Result_Value.Integer) & ")",
                when CCL.Language.Boolean_Type => (if Outcome.Result_Value.Boolean then "(true)" else "(false)"),
                when others => "");
      end case;
   end Result_Image;
end CCL.Sessions;
