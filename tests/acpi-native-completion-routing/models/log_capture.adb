pragma Ada_2022;
package body Log_Capture is
   procedure Reset (Raise_On_Write : Boolean) is
   begin
      Attempts := 0; Fail := Raise_On_Write; Length := 0;
      Text := [others => ' ']; Last_Level := CuBit.Log_Records.Trace;
   end Reset;
end Log_Capture;
