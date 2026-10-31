pragma Ada_2022;
with CuBit.Log_Records;
with CuBit.Messages;
package body GPIO_Native_Log is
   package R renames GPIO_Native_Resources;
   package T renames R.T;
   use type Interfaces.Unsigned_64, R.Outcome, T.Header_Outcome;
   Publisher : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Target);
   Enabled : Boolean := True;
   Summarized : Boolean := False;
   Success, Failure : array (Bringup_Stage) of Boolean := [others => False];
   Seen_Communities : array (R.G.Memory_Resource_Ordinal) of Boolean := [others => False];
   Repeats : Interfaces.Unsigned_64 := 0;
   Completed : Natural range 0 .. Bringup_Stage'Pos (Bringup_Stage'Last) + 1 := 0;
   Last_Reached : Bringup_Stage := Identify;
   function Name (Item : Bringup_Stage) return String is
     (case Item is when Identify => "identify", when Resources => "resources",
      when Geometry => "geometry", when Pin_Setup => "community-parking", when Parked => "parked",
      when Registration => "registration", when Readiness => "readiness");
   procedure Repeat is
   begin if Repeats < Interfaces.Unsigned_64'Last then Repeats := Repeats + 1; end if; end Repeat;
   procedure Line (Text : String; Failed : Boolean := False) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
        ("gpio: " & Text, (if Failed then CuBit.Log_Records.Warning else CuBit.Log_Records.Information));
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint ("gpio: " & Text & ASCII.LF);
      if Enabled and then Made.Success then CuBit.Logging.Emit (Publisher, Made.Value, Submitted); end if;
   end Line;
   procedure Stage (Item : Bringup_Stage; OK : Boolean; Reason : String; Details : String := "") is
   begin
      if (if OK then Success (Item) else Failure (Item)) then Repeat; return; end if;
      if OK then Success (Item) := True; Completed := Completed + 1; Last_Reached := Item;
      else Failure (Item) := True; end if;
      Line ("stage " & Name (Item) & (if OK then " ok " else " fail ") & Reason & Details, not OK);
   end Stage;
   function Hex (Key : String; Value : Interfaces.Unsigned_64) return String is
      Alphabet : constant String := "0123456789abcdef";
      Radix : constant Interfaces.Unsigned_64 := 16;
      Digits_Per_Word : constant := Interfaces.Unsigned_64'Size / 4;
      Text : String (1 .. Digits_Per_Word);
      Remaining : Interfaces.Unsigned_64 := Value;
      First : Positive := Text'First;
   begin
      for N in reverse Text'Range loop
         Text (N) := Alphabet (Natural (Remaining mod Radix) + 1); Remaining := Remaining / Radix;
      end loop;
      while First < Text'Last and then Text (First) = '0' loop First := First + 1; end loop;
      return " " & Key & "=0x" & Text (First .. Text'Last);
   end Hex;
   procedure Community (Ordinal : R.G.Memory_Resource_Ordinal; Item : R.Resource_Report) is
      Community_Number : constant String := (case Ordinal is when 0 => "0", when 1 => "1", when 2 => "4", when 3 => "5");
      OK : constant Boolean := Item.Status = R.Ready;
   begin
      if Seen_Communities (Ordinal) then Repeat; return; end if;
      Seen_Communities (Ordinal) := True;
      if Item.Status = R.Absent then
         Line ("stage community-" & Community_Number & " ok not-present"); return;
      end if;
      if Item.Header.Status in T.Probed | T.Geometry_Refused then
         Line ("stage community-" & Community_Number & (if OK then " ok " else " fail ") & R.Outcome'Image (Item.Status) &
            Hex ("rev", Interfaces.Unsigned_64 (Item.Header.Revision)) &
            Hex ("padbar", Interfaces.Unsigned_64 (Item.Header.PADBAR)) &
            Hex ("bytes", Interfaces.Unsigned_64 (Item.Description.Bytes)), not OK);
      else
         Line ("stage community-" & Community_Number & (if OK then " ok " else " fail ") & R.Outcome'Image (Item.Status) &
            " map=" & T.Map_Outcome'Image (Item.Mapping.Status) &
            " probe=" & T.Header_Outcome'Image (Item.Header.Status) & " sample=unavailable", not OK);
      end if;
   end Community;
   procedure Summary is
      All_Good : constant Boolean := (for all Item in Bringup_Stage => Success (Item) and not Failure (Item));
   begin
      if Summarized then return; end if;
      Summarized := True;
      if All_Good then Line ("bring-up complete" & Hex ("repeats", Repeats));
      else
         Line ("bring-up reached " & (if Completed = 0 then "none" else Name (Last_Reached)) &
            " (" & Natural'Image (Completed) & "/" &
            Natural'Image (Bringup_Stage'Pos (Bringup_Stage'Last) + 1) & ")" & Hex ("repeats", Repeats), True);
      end if;
   end Summary;
   procedure Disable (Reason : String) is
      Done : Boolean;
   begin
      if not Enabled then return; end if;
      Enabled := False; CuBit.Logging.Disconnect (Publisher, Done);
      CuBit.Messages.debugPrint ("gpio: logging disabled " & Reason & (if Done then " closed" else " pending") & ASCII.LF);
   end Disable;
end GPIO_Native_Log;
