with CCL.Sessions;
with Clipboard_Text;

package body CCL_Result_Copy is
   use type CCL.Language.Static_Type;

   function Type_Name (Outcome : CCL.Language.Interpretation_Result) return String is
     (CCL.Sessions.Result_Type_Image (Outcome));
   function Text_Form (Outcome : CCL.Language.Interpretation_Result) return String is
     (CCL.Sessions.Result_Value_Image (Outcome));

   function Literal (Outcome : CCL.Language.Interpretation_Result) return String is
      Value : constant String := CCL.Sessions.Result_Value_Image (Outcome);
   begin
      if CCL.Sessions.Result_Type (Outcome) /= CCL.Language.String_Type or else
        Outcome.Has_List or else Outcome.Has_Literal
      then
         return Value;
      end if;
      declare
         --  Every byte escaped at most doubles, plus the two quotes.
         Quoted : Clipboard_Text.Builder (Positive'Min (2 * Value'Length + 2, Clipboard_Text.Maximum_Form));
      begin
         Clipboard_Text.Append_Quoted (Quoted, Value);
         return (if Quoted.Fits then Clipboard_Text.Image (Quoted) else "");
      end;
   end Literal;
end CCL_Result_Copy;
