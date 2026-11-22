pragma Ada_2022;
with CuBit.Handles.IPC;
with CuBit.Log_Records;
with CuBit.Messages;
with CuBit.Process_IDs;
with HID_Log_Lifetime;
package body HID_Native_Log is
   package P renames CuBit.Process_IDs;
   package IPC renames CuBit.Handles.IPC;
   use type Interfaces.Unsigned_64, CuBit.Handles.ABI.Kind;
   Publisher : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Target);
   Logstore : P.Process_ID := P.No_Process;
   Initialized, Enabled, Summarized : Boolean := False;
   May_End : Boolean := True;
   function Detached return Boolean is (May_End);
   Success, Failure : array (Bringup_Stage) of Boolean := [others => False];
   Repeats : Interfaces.Unsigned_64 := 0;
   Completed : Natural range 0 .. Bringup_Stage'Pos (Bringup_Stage'Last) + 1 := 0;
   Last_Reached : Bringup_Stage := Identify;
   function Name (Item : Bringup_Stage) return String is
     (case Item is when Identify => "identify", when Bindings => "bindings",
      when Buffers => "buffers", when Reset => "reset", when Descriptor => "descriptor",
      when Report_Parser => "report-parser", when Serving_Pin => "serving-pin",
      when Publication => "publication", when Readiness => "readiness");
   procedure Initialize (Result : out Binding_Result) is
   begin
      Result := Already_Initialized; if Initialized then return; end if;
      Initialized := True;
      Result := Missing; if not Target.Present then return; end if;
      declare
         Description : constant IPC.Description := IPC.Describe
           (CuBit.Logging.Logstore_Handles.Handle (Target.Value));
      begin
         Result := Refused;
         if not Description.Current or else Description.Kind /= CuBit.Handles.ABI.Endpoint then return; end if;
         Logstore := P.From_Word (Description.Object);
         if not P.Is_Process (Logstore) then Logstore := P.No_Process; return; end if;
         Enabled := True; May_End := False; Result := Bound;
      end;
   end Initialize;
   procedure Line (Text : String; Failed : Boolean := False) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make
        ("hid-i2c: " & Text,
         (if Failed then CuBit.Log_Records.Warning else CuBit.Log_Records.Information));
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint ("hid-i2c: " & Text & ASCII.LF);
      if Enabled and then Made.Success then CuBit.Logging.Emit (Publisher, Made.Value, Submitted); end if;
   end Line;
   procedure Stage (Item : Bringup_Stage; OK : Boolean; Why : Reason; Details : String := "") is
   begin
      if (if OK then Success (Item) else Failure (Item)) then
         if Repeats < Interfaces.Unsigned_64'Last then Repeats := Repeats + 1; end if;
         return;
      end if;
      if OK then Success (Item) := True; Completed := Completed + 1; Last_Reached := Item;
      else Failure (Item) := True; end if;
      Line ("stage " & Name (Item) & (if OK then " ok " else " fail ") & Reason'Image (Why) & Details, not OK);
   end Stage;
   function Hex (Key : String; Value : Interfaces.Unsigned_64) return String is
      Alphabet : constant String := "0123456789abcdef";
      Radix : constant Interfaces.Unsigned_64 := 16;
      Text : String (1 .. Interfaces.Unsigned_64'Size / 4);
      Remaining : Interfaces.Unsigned_64 := Value;
      First : Positive := Text'First;
   begin
      for Index in reverse Text'Range loop
         Text (Index) := Alphabet (Natural (Remaining mod Radix) + 1); Remaining := Remaining / Radix;
      end loop;
      while First < Text'Last and then Text (First) = '0' loop First := First + 1; end loop;
      return " " & Key & "=0x" & Text (First .. Text'Last);
   end Hex;
   procedure Summary is
      All_Good : constant Boolean := (for all Item in Bringup_Stage => Success (Item) and not Failure (Item));
   begin
      if Summarized then return; end if; Summarized := True;
      if All_Good then Line ("bring-up complete" & Hex ("repeats", Repeats));
      else Line ("bring-up reached " & (if Completed = 0 then "none" else Name (Last_Reached)) &
         " (" & Natural'Image (Completed) & "/" &
         Natural'Image (Bringup_Stage'Pos (Bringup_Stage'Last) + 1) & ")" & Hex ("repeats", Repeats), True); end if;
   end Summary;
   procedure Disable is
      Done : Boolean;
   begin
      if not Enabled then return; end if;
      Enabled := False; CuBit.Logging.Disconnect (Publisher, Done);
      May_End := Done;
      CuBit.Messages.debugPrint ("hid-i2c: logging disabled" &
         (if Done then " closed" else " pending") & ASCII.LF);
   end Disable;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event) is
   begin
      if HID_Log_Lifetime.Related (Logstore, Item) then Disable; end if;
   end Grant_Notice;
end HID_Native_Log;
