pragma Ada_2022;
with CuBit.Handles.IPC;
package body ACPI_Runtime_Query_Replies with SPARK_Mode => Off is
   procedure Save (Handle : out CuBit.Handles.Reply; Accepted : out Boolean) is
      Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
   begin
      Accepted := Saved.Saved;
      Handle := (if Saved.Saved then Saved.Handle else CuBit.Handles.No_Reply);
   end Save;
   procedure Answer (Handle : CuBit.Handles.Reply; Value : Details; Delivered : out Boolean) is
   begin Delivered := CuBit.Handles.IPC.Answer (Handle, Encode (Value)); end Answer;
end ACPI_Runtime_Query_Replies;
