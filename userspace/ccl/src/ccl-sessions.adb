with CCL.VM;
with Interfaces;
with CCL.Diagnostics;

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

   procedure Recall
     (Item : Session; Index : History_Index; Entry_Value : out Submission;
      Found : out Boolean) is
   begin
      Found := Index <= Item.Count;
      Entry_Value := (others => <>);
      if Found then Entry_Value := Item.Entries (Slot (Item.Oldest, Index - 1)); end if;
   end Recall;

   procedure Submit
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Outcome : out CCL.Language.Interpretation_Result)
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
      --  The frontend checks the ORIGINAL length before parsing. Do not pass
      --  the history's bounded copy: a valid prefix must not become a command.
      CCL.Language.Interpret (Source, Fuel, Item.Catalog, Outcome);
      Value.Outcome := Outcome;
      Target := Slot (Item.Oldest, Item.Count);
      Item.Entries (Target) := Value;
      if Item.Count = Maximum_History then
         Item.Oldest := Slot (Item.Oldest, 1);
      else
         Item.Count := Item.Count + 1;
      end if;
   end Submit;

   function Result_Type (Outcome : CCL.Language.Interpretation_Result)
     return CCL.Language.Static_Type is
   begin
      if Outcome.Status /= CCL.Language.Succeeded or else not Outcome.Has_Value then
         return CCL.Language.Invalid_Type;
      elsif Outcome.Has_Text then return CCL.Language.String_Type;
      elsif Outcome.Has_Character then return CCL.Language.Character_Type;
      elsif Outcome.Result_Value.Kind = CCL.VM.Integer_Value then return CCL.Language.Integer_Type;
      elsif Outcome.Result_Value.Kind = CCL.VM.Boolean_Value then return CCL.Language.Boolean_Type;
      else return CCL.Language.Invalid_Type;
      end if;
   end Result_Type;

   function Result_Image (Outcome : CCL.Language.Interpretation_Result) return String is
   begin
      if Outcome.Status = CCL.Language.Host_Import_Required then
         return "Service call needs VM execution; no service was invoked.";
      elsif Outcome.Status /= CCL.Language.Succeeded then
         return CCL.Diagnostics.Message (Outcome.Status) &
           (if Outcome.Diagnostic = CCL.Language.No_Diagnostic then ""
            else ": " & CCL.Diagnostics.Message (Outcome.Diagnostic)) &
           (if Outcome.Diagnostic_Position = 0 then ""
            else " at character" & Natural'Image (Outcome.Diagnostic_Position));
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
      end case;
   end Result_Image;
end CCL.Sessions;
