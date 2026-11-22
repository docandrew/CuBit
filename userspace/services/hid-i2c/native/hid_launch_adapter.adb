pragma Ada_2022;
package body HID_Launch_Adapter with SPARK_Mode is
   package P renames Parameters;
   package Pointer renames App.Source.Pointer;
   use type App.W.Word;
   function Direction (Raw : P.HID_Direction_Value) return Pointer.Direction is
     (case Raw is when P.As_Reported => Pointer.As_Reported, when P.Reversed => Pointer.Reversed);
   function Confidence (Raw : P.HID_Confidence_Value) return Pointer.Confidence_Policy is
     (case Raw is when P.Require_Confident => Pointer.Require_Confident, when P.Accept_Unknown => Pointer.Accept_Unknown);
   function Button (Raw : P.HID_Button_Value) return App.Source.Button_Target is
     (case Raw is when P.Ignore => App.Source.Ignore, when P.Primary_Button => App.Source.Primary_Button,
      when P.Secondary_Button => App.Source.Secondary_Button, when P.Middle_Button => App.Source.Middle_Button,
      when P.Side_Button => App.Source.Side_Button, when P.Extra_Button => App.Source.Extra_Button);
   function Translate (Raw : P.HID_Parameters_Value) return Result is
   begin
      if Long_Long_Integer (Raw.contact_limit) >
        Long_Long_Integer (App.Source.Reports.Frames.Contact_Limit'Last)
      then return (Outcome => Contact_Capacity_Exceeded); end if;
      return (Outcome => Accepted,
         Descriptor_Register => App.D.Register_Number (Raw.descriptor_register), Timing => Raw.timing,
         Settings =>
           (Contact_Limit => App.Source.Reports.Frames.Contact_Limit (Raw.contact_limit),
            Binding_Budget => App.Bindings.Work_Count (Raw.binding_budget),
            -- Unused until report parsing supplies the actual coordinate bounds.
            X_Range => (0, 0), Y_Range => (0, 0),
            X_Gain => (Pointer.Gain_Numerator (Raw.x.numerator), Pointer.Gain_Denominator (Raw.x.denominator)),
            Y_Gain => (Pointer.Gain_Numerator (Raw.y.numerator), Pointer.Gain_Denominator (Raw.y.denominator)),
            X_Direction => Direction (Raw.x.direction), Y_Direction => Direction (Raw.y.direction),
            Confidence => Confidence (Raw.confidence),
            Buttons => [Button (Raw.buttons.button_1), Button (Raw.buttons.button_2), Button (Raw.buttons.button_3)],
            Coordinates => App.Source.Report_Bounds));
   end Translate;
   function Deadline (Now : App.W.Word; Span : P.HID_Milliseconds_Value) return Deadline_Result is
      Window : constant App.W.Word := App.W.Word (Span);
   begin
      if Now > App.W.Millisecond'Last - Window then return (Available => False); end if;
      return (Available => True, Limit => Now + Window);
   end Deadline;
end HID_Launch_Adapter;
