pragma Ada_2022;
with CuBit.Log_Records;
package Log_Capture is
   Attempts : Natural := 0;
   Fail : Boolean := False;
   Last_Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
   Length : Natural := 0;
   Text : String (1 .. CuBit.Log_Records.Maximum_Text_Bytes) := [others => ' '];
   procedure Reset (Raise_On_Write : Boolean);
end Log_Capture;
