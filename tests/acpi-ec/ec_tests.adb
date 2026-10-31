with Ada.Text_IO;
with ACPI_EC_Types;
with ACPI_EC_Transactions;
procedure EC_Tests is
   use ACPI_EC_Types;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "EC check" & Checks'Image;
      end if;
   end Check;
   procedure Scenario (Kind : Operation; Offset : Cell_Offset;
                       Fault : Natural; Stop_After_Write : Boolean := False) is
      Now : Microseconds := 1;
      Last_Write : Microseconds := 0;
      Pending : Boolean := False;
      Pending_Byte : Byte := 0;
      Command : Byte := 0;
      Stage : Natural := 0;
      Output_Full_Now : Boolean := False;
      Data : Byte := 0;
      Writes : Natural := 0;
      Reads : Natural := 0;
      IO_Count : Natural := 0;
      Selected : Cell_Offset := 0;
      Cell_Value : Byte := 16#A5#;
      Good_Clock : Boolean := True;
      Ownership : Boolean := True;
      Accepted : Boolean;
      Inject : Boolean := False;
      procedure Clock (Time : out Microseconds; Available : out Boolean) is
      begin
         Time := Now;
         Available := Good_Clock;
      end Clock;
      function Owned return Boolean is (Ownership);
      procedure Read (Register : Read_Register; Value : out Byte;
                      Result : out Access_Result) is
      begin
         IO_Count := IO_Count + 1;
         Reads := Reads + 1;
         Value := 0;
         Result := Completed;
         if Fault = 16 and Writes > 0 then
            Result := Indeterminate;
            return;
         end if;
         if Register = Status_Register then
            --  A host write's input-full assertion can be delayed for one us.
            --  The engine must not poll the old clear bit during that window.
            if Pending then
               Check (Now >= Last_Write + 1);
               if Now < Last_Write + 2 then
                  Value := Input_Full;
                  return;
               end if;
               Pending := False;
               if Stage = 1 and Command = Query_Command then
                  Data := 16#42#;
                  Output_Full_Now := True;
               elsif Stage = 2 then
                  Selected := Cell_Offset (Pending_Byte);
                  Check (Selected = Offset);
                  if Command = Read_Command then
                     Data := Cell_Value;
                     Output_Full_Now := True;
                  end if;
               elsif Stage = 3 then
                  Check (Command = Write_Command);
                  Cell_Value := Pending_Byte;
               end if;
            end if;
            if Fault = 6 and Writes = 0 then
               Value := Output_Full;
            elsif Fault = 7 and Writes = 0 then
               Value := Burst_Mode;
            elsif Output_Full_Now then
               Value := Output_Full;
            elsif Fault = 17 then
               Value := Input_Full;
            elsif Fault /= 8 then
               Value := Event_Pending;
            end if;
         else
            Check (Output_Full_Now);
            Output_Full_Now := False;
            Value := Data;
            if Fault = 9 then
               Good_Clock := False;
            end if;
         end if;
      end Read;
      procedure Write (Register : Write_Register; Value : Byte;
                       Result : out Access_Result) is
      begin
         IO_Count := IO_Count + 1;
         Writes := Writes + 1;
         Check (not Pending and not Output_Full_Now);
         Result := Completed;
         if Inject or (Fault = 18 and Writes = 2) then
            Result := Indeterminate;
            return;
         elsif Fault = 10 then
            Result := Refused;
            return;
         end if;
         Pending := True;
         Last_Write := Now;
         Pending_Byte := Value;
         Stage := Stage + 1;
         if Stage = 1 then
            Check (Register = Command_Register);
            Command := Value;
            Check (Command = (case Kind is
                       when Read_Cell => Read_Command,
                       when Write_Cell => Write_Command,
                       when Query_Event => Query_Command));
         else
            Check (Register = Data_Register);
         end if;
      end Write;
      package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
      Before : Natural;
      Terminal_IO : Natural;
   begin
      Engine.Start (Kind, Offset, 16#7C#, 1000,
                    (if Fault = 5 then 0 elsif Fault = 15 then 3 elsif Fault = 17 then 20 else 1000), Accepted);
      Check (Accepted);
      Engine.Start (Kind, Offset, 0, 1000, 1000, Accepted);
      Check (not Accepted);
      if Fault = 1 then Engine.Request_Stop; end if;
      if Fault = 2 then Good_Clock := False; end if;
      if Fault = 3 then Ownership := False; end if;
      if Fault = 4 then Inject := True; end if;
      for Iteration in 1 .. 200 loop
         Before := IO_Count;
         Engine.Step;
         Check (IO_Count <= Before + 1);
         exit when Engine.Status /= Running;
         if Stop_After_Write and Writes > 0 then Engine.Request_Stop; end if;
         if Writes > 0 then
            case Fault is
               when 11 => Now := 1000;
               when 12 => Good_Clock := False;
               when 13 => Ownership := False;
               when 14 => Now := 0;
               when others => null;
            end case;
         end if;
         --  Two steps per microsecond exercises same-tick postwrite guards.
         if Iteration mod 2 = 0 then Now := Now + 1; end if;
      end loop;
      Check (Engine.Status /= Running);
      case Fault is
         when 0 =>
            Check (Engine.Status = Succeeded);
            Check (not Pending and not Output_Full_Now);
            Check (not Engine.Needs_Recovery);
            Check (Engine.Effects_Possible);
            Check (Engine.Stop_Requested = Stop_After_Write);
            case Kind is
               when Read_Cell => Check (Engine.Result_Byte = 16#A5# and Writes = 2);
               when Write_Cell => Check (Cell_Value = 16#7C# and Writes = 3);
               when Query_Event => Check (Engine.Result_Byte = 16#42# and Writes = 1);
            end case;
         when 1 => Check (Engine.Status = Stopped and IO_Count = 0);
         when 2 => Check (Engine.Status = Clock_Unavailable and IO_Count = 0);
         when 3 => Check (Engine.Status = Ownership_Lost and IO_Count = 0);
         when 4 => Check (Engine.Status = Access_Unknown and Engine.Needs_Recovery);
         when 5 => Check (Engine.Status = Budget_Exhausted and IO_Count = 0);
         when 6 => Check (Engine.Status = Unexpected_Output and Writes = 0);
         when 7 => Check (Engine.Status = Burst_Active and Writes = 0);
         when 8 => Check (Engine.Status = No_Event and Writes = 0);
         when 9 => Check (Engine.Status = Clock_Unavailable
                          and Engine.Result_Byte = 0 and Engine.Needs_Recovery);
         when 10 => Check (Engine.Status = Access_Refused and Writes = 1
                           and not Engine.Effects_Possible);
         when 11 => Check (Engine.Status = Deadline_Reached and Engine.Needs_Recovery);
         when 12 => Check (Engine.Status = Clock_Unavailable and Engine.Needs_Recovery);
         when 13 => Check (Engine.Status = Ownership_Lost and Engine.Needs_Recovery);
         when 14 => Check (Engine.Status = Clock_Reversed and Engine.Needs_Recovery);
         when 15 => Check (Engine.Status = Budget_Exhausted and Engine.Needs_Recovery);
         when 16 => Check (Engine.Status = Access_Unknown and Engine.Needs_Recovery);
         when 17 => Check (Engine.Status = Budget_Exhausted
                           and Engine.Needs_Recovery and Writes = 0
                           and not Engine.Effects_Possible);
         when 18 => Check (Engine.Status = Access_Unknown and Engine.Needs_Recovery);
         when others => raise Program_Error;
      end case;
      Terminal_IO := IO_Count;
      Engine.Step;
      Check (IO_Count = Terminal_IO);
      Engine.Acknowledge (Accepted);
      Check (Accepted = not Engine.Needs_Recovery);
      if not Accepted then
         Engine.Start (Read_Cell, Offset, 0, 1000, 1000, Accepted);
         Check (not Accepted);
      end if;
   end Scenario;
begin
   for Kind in Operation loop
      for Offset in Cell_Offset loop
         Scenario (Kind, Offset, 0);
      end loop;
      Scenario (Kind, 255, 0, True);
   end loop;
   for Fault in 1 .. 17 loop
      Scenario (Query_Event, 0, Fault);
   end loop;
   Scenario (Read_Cell, 255, 18);
   Ada.Text_IO.Put_Line ("EC checks:" & Checks'Image);
end EC_Tests;
