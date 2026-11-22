pragma Ada_2022;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Messages;
with Startup;
package body I2C_Log is
   use type Interfaces.Unsigned_64;
   Publisher : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Enabled : Boolean := True;
   procedure Line (Text : String; Failed : Boolean := False) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
        ("i2c-dw: " & Text,
         (if Failed then CuBit.Log_Records.Warning else CuBit.Log_Records.Information));
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint ("i2c-dw: " & Text & ASCII.LF);
      if Enabled and then Made.Success then CuBit.Logging.Emit (Publisher, Made.Value, Submitted); end if;
   end Line;
   procedure Stage (Name : String; OK : Boolean; Reason : String; Details : String := "") is
   begin
      Line ("stage " & Name & (if OK then " ok " else " fail ") & Reason & Details, not OK);
   end Stage;
   procedure Summary (Ready : Boolean) is
   begin
      Line ((if Ready then "bring-up complete" else "bring-up stopped; controller unavailable"), not Ready);
   end Summary;
   procedure Note (Text : String) is
   begin Line (Text); end Note;
   procedure Disable (Reason : String) is
      Done : Boolean;
   begin
      if not Enabled then return; end if;
      -- Close through the existing runtime before any further publication.
      -- Grant notices conservatively end logging: the opaque publisher does
      -- not expose which channel grants it currently owns. Serial remains.
      Enabled := False;
      CuBit.Logging.Disconnect (Publisher, Done);
      CuBit.Messages.debugPrint ("i2c-dw: logging disabled " & Reason &
        (if Done then " closed" else " pending") & ASCII.LF);
   end Disable;
   function Hex (Key : String; Value : Interfaces.Unsigned_64) return String is
      Hex_Alphabet : constant String := "0123456789abcdef";
      Radix : constant Interfaces.Unsigned_64 := 16;
      Text : String (1 .. 16);
      Remaining : Interfaces.Unsigned_64 := Value;
   begin
      for N in reverse Text'Range loop
         Text (N) := Hex_Alphabet (Natural (Remaining mod Radix) + 1);
         Remaining := Remaining / Radix;
      end loop;
      return " " & Key & "=0x" & Text;
   end Hex;
end I2C_Log;
