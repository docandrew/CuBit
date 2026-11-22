package body XHCI_Capabilities with SPARK_Mode => On is
   function Scratchpad_Count (Parameters : Unsigned_32)
      return Scratchpad_Buffer_Count
   is
      Low : constant Unsigned_32 := Shift_Right (Parameters, 27) and 16#1F#;
      High : constant Unsigned_32 := Shift_Right (Parameters, 21) and 16#1F#;
   begin
      return Scratchpad_Buffer_Count (Shift_Left (High, 5) or Low);
   end Scratchpad_Count;
end XHCI_Capabilities;
