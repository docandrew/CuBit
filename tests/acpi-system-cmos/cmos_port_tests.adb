pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_CMOS_Ports;
with ACPI_CMOS_Transactions;
with ACPI_Backend_Binding_Types;

procedure CMOS_Port_Tests is
   package T renames ACPI_CMOS_Transactions;
   package P renames T.P;
   package I renames T.I;
   package B renames ACPI_Backend_Binding_Types;
   use type Interfaces.Unsigned_16, I.Byte, I.Word, P.Device_Kind, T.Completion;
   type Handle is new Natural;
   type Failure is (None, Deny_Select, Unknown_Select, Deny_Data, Unknown_Data, Bad_Byte, Revoked);
   Mode : Failure := None;
   Description : B.Description;
   Checks, Steps : Natural := 0;
   Selected : I.Byte := 0;
   Select_Port : Interfaces.Unsigned_16 := 0;
   In_Callback : Boolean := False;
   Active_Plan : T.Transfer;
   procedure Check (Good : Boolean) is
   begin
      if not Good then raise Program_Error with "CMOS physical-interface regression"; end if;
      Checks := Checks + 1;
   end Check;
   procedure Inspect (Held : Handle; Item : out B.Description) is
   begin Check (Held = 1); Item := Description; end Inspect;
   procedure Read_Byte (Held : Handle; Port : Interfaces.Unsigned_16;
      Value : out I.Word; Outcome : out T.Completion);
   procedure Write_Byte (Held : Handle; Port : Interfaces.Unsigned_16;
      Value : I.Byte; Outcome : out T.Completion);
   package Ports is new ACPI_CMOS_Ports (Handle, 0, Inspect, Read_Byte, Write_Byte);
   type Owner_Access is access all Ports.Context;
   Active : Owner_Access := null;
   procedure Reenter is
      Value : I.Byte;
      Outcome : T.Completion;
      Before : constant Natural := Steps;
   begin
      Check (not In_Callback); In_Callback := True;
      Ports.Transfer_Byte (Active.all, Active_Plan, Value, Outcome);
      Check (Outcome = T.Refused and Steps = Before);
      In_Callback := False;
   end Reenter;
   procedure Read_Byte (Held : Handle; Port : Interfaces.Unsigned_16;
      Value : out I.Word; Outcome : out T.Completion) is
   begin
      Check (Held = 1 and Steps = 1);
      Check (Port = Select_Port + 1 and Port in 16#71# | 16#73#);
      Steps := Steps + 1; Reenter;
      Value := (if Mode = Bad_Byte then 256 else I.Word (Selected xor 16#5A#));
      Outcome := (if Mode = Unknown_Data then T.Unknown elsif Mode = Deny_Data then T.Refused else T.Completed);
   end Read_Byte;
   procedure Write_Byte (Held : Handle; Port : Interfaces.Unsigned_16;
      Value : I.Byte; Outcome : out T.Completion) is
   begin
      Check (Held = 1 and Steps < 2);
      if Steps = 0 then
         Check (Port in 16#70# | 16#72#);
         Select_Port := Port;
         Selected := (Value and 16#7F#) + (if Port = 16#72# then 128 else 0);
         Outcome := (if Mode = Unknown_Select then T.Unknown elsif Mode = Deny_Select then T.Refused else T.Completed);
      else
         Check (Port = Select_Port + 1 and Value = 16#A5#);
         Outcome := (if Mode = Unknown_Data then T.Unknown elsif Mode = Deny_Data then T.Refused else T.Completed);
      end if;
      Steps := Steps + 1; Reenter;
   end Write_Byte;
begin
   for Kind in P.Device_Kind loop
      for Extended in Boolean loop
         for NMI in Boolean loop
            for Cell in I.Byte loop
               for Writing in Boolean loop
                  for Scenario in Failure loop
                     declare
                        IO : aliased Ports.Context;
                        Good : Boolean;
                        Value : I.Byte;
                        Outcome : T.Completion;
                        Profile : constant P.Profile := (Kind, Extended);
                        Maximum : constant Natural := (if Kind = P.PC_AT_64 then 63 elsif Extended then 255 else 127);
                        Supported : constant Boolean := Kind in P.PC_AT_64 | P.Intel_Compatible;
                        Admitted : constant Boolean := Supported and Natural (Cell) <= Maximum
                          and not (Writing and (Cell <= 13 or Cell = 50));
                        Expected_Steps : Natural;
                     begin
                        Mode := Scenario; Steps := 0; Active := IO'Unchecked_Access;
                        Description := (True, B.Port_Resource, 16#70#,
                          (if Kind = P.Intel_Compatible and Extended then 4 else 2), True, True, False);
                        Ports.Initialize (IO, 1, Profile, 50, True, NMI, Good);
                        Check (Good = Supported);
                        Active_Plan := (Ticket => 1,
                          Pair => (if Cell < 128 then P.Standard_Pair else P.Extended_Pair),
                          Selector => I.Selector (Cell mod 128, NMI), For_Write => Writing, Value => 16#A5#);
                        if Scenario = Revoked then Description.Current := False; end if;
                        Ports.Transfer_Byte (IO, Active_Plan, Value, Outcome);
                        if not Admitted or Scenario = Revoked then
                           Expected_Steps := 0; Check (Outcome = T.Refused);
                        elsif Scenario in Deny_Select | Unknown_Select then
                           Expected_Steps := 1;
                           Check (Outcome = (if Scenario = Unknown_Select then T.Unknown else T.Refused));
                        else
                           Expected_Steps := 2;
                           Check (Outcome = (if Scenario = Unknown_Data then T.Unknown
                             elsif Scenario = Deny_Data or (Scenario = Bad_Byte and not Writing)
                             then T.Refused else T.Completed));
                           if Outcome = T.Completed and not Writing then
                              Check (Value = (Cell xor 16#5A#));
                           end if;
                        end if;
                        Check (Steps = Expected_Steps);
                        if Outcome = T.Unknown then
                           Check (Ports.Pending (IO) and not Ports.Is_Ready (IO));
                           Ports.Transfer_Byte (IO, Active_Plan, Value, Outcome);
                           Check (Outcome = T.Refused and Steps = Expected_Steps);
                           Ports.Initialize (IO, 1, Profile, 50, True, NMI, Good);
                           Check (not Good and Ports.Pending (IO));
                        end if;
                        Active := null;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   -- Invalid startup ranges, rights, history and century cannot lead to I/O.
   for Bad in 1 .. 7 loop
      declare
         IO : Ports.Context;
         Good : Boolean;
      begin
         Description := (True, B.Port_Resource, 16#70#, 2, True, True, False);
         case Bad is
            when 1 => Description.Base := 16#71#;
            when 2 => Description.Length := 4;
            when 3 => Description.Readable := False;
            when 4 => Description.Delegable := True;
            when others => null;
         end case;
         Steps := 0;
         Ports.Initialize (IO, 1, (P.PC_AT_64, False), (if Bad = 6 then 128 else 50), Bad /= 5, False, Good);
         if Bad = 7 then Check (Good); else Check (not Good); end if;
         Check (Steps = 0);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("CMOS physical-interface checks " & Checks'Image & " PASS");
end CMOS_Port_Tests;
