with Ada.Text_IO;
with ACPI_EC_Types;
with ACPI_EC_Transactions;
with ACPI_EC_Registers;
with ACPI_EC_Endpoint;
with ACPI_Region_Protocol;
with Interfaces;
procedure EC_Register_Tests is
   use ACPI_EC_Types;
   use type ACPI_Region_Protocol.Outcome;
   Now : Microseconds := 1;
   Last_Write : Microseconds := 0;
   Pending : Boolean := False;
   Output_Ready : Boolean := False;
   Stage : Natural := 0;
   Command : Byte := 0;
   Written : Byte := 0;
   Cells : array (Cell_Offset) of Byte := [others => 16#A5#];
   Selected_Cell : Cell_Offset := 0;
   IO : Natural := 0;
   Checks : Natural := 0;
   Unknown_Write : Boolean := False;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "EC register check" & Checks'Image;
      end if;
   end Check;
   procedure Clock (Time : out Microseconds; Available : out Boolean) is
   begin
      Time := Now;
      Available := True;
   end Clock;
   function Owned return Boolean is (True);
   procedure Read (Register : Read_Register; Value : out Byte;
                   Result : out Access_Result) is
   begin
      IO := IO + 1;
      Result := Completed;
      Value := 0;
      if Register = Status_Register then
         if Pending then
            Check (Now >= Last_Write + 1);
            Pending := False;
            if Stage = 2 then
               Selected_Cell := Cell_Offset (Written);
               if Command = Read_Command then
                  Output_Ready := True;
               end if;
            elsif Stage = 3 then
               Check (Command = Write_Command);
               Cells (Selected_Cell) := Written;
               Stage := 0;
            end if;
         end if;
         if Output_Ready then Value := Output_Full; end if;
      else
         Check (Output_Ready);
         Value := Cells (Selected_Cell);
         Output_Ready := False;
         Stage := 0;
      end if;
   end Read;
   procedure Write (Register : Write_Register; Value : Byte;
                    Result : out Access_Result) is
   begin
      IO := IO + 1;
      Check (not Pending and not Output_Ready);
      if Unknown_Write then
         Result := Indeterminate;
         return;
      end if;
      Result := Completed;
      Pending := True;
      Last_Write := Now;
      Written := Value;
      Stage := Stage + 1;
      if Stage = 1 then
         Check (Register = Command_Register);
         Command := Value;
      else
         Check (Register = Data_Register);
      end if;
   end Write;
   package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
   package Catalog is new ACPI_EC_Registers (Engine, Maximum_Registers => 2);
   use Catalog;
   package Wire is new ACPI_EC_Endpoint (Catalog);
   Owner : constant Owner_ID := 16#ABCD_1234_0000_0001#;
   Other_Owner : constant Owner_ID := 16#1234_1234_0000_0001#;
   Epoch : constant Epoch_ID := 16#1234_0000_0000_0001#;
   A : Admission := (Register => 17, Epoch => Epoch, Owner => Owner,
                    Cell => 255, Readable => True, Writable => True,
                    Exact_Write_Safe => True, Write_Mask => 16#0F#);
   Accepted : Boolean;
   Result : Submission;
   Token : Operation_ID;
   Reply : Completion;
   Last_Token : Operation_ID := 0;
   Fault_Reply : ACPI_Region_Protocol.Packet;
   Before : Natural;
   procedure Reject (Sender : Owner_ID; Register : Register_ID;
                     Version : Epoch_ID; For_Write : Boolean; Value : Byte) is
   begin
      Before := IO;
      Catalog.Submit (Sender, Register, Version, For_Write, Value,
                      Now + 100, 100, Result, Token);
      Check (Result = Denied and Token = 0 and IO = Before);
   end Reject;
   procedure Run (Write_Cell : Boolean; Value : Byte; Expected : Byte) is
      Own_Token : Operation_ID;
   begin
      Catalog.Submit (Owner, 17, A.Epoch, Write_Cell, Value,
                      Now + 100, 100, Result, Own_Token);
      Check (Result = Started and Own_Token > Last_Token);
      Last_Token := Own_Token;
      Check (Catalog.Active);
      Catalog.Revoke (17, A.Epoch, Accepted);
      Check (not Accepted);
      Catalog.Submit (Owner, 17, A.Epoch, False, 0,
                      Now + 100, 100, Result, Token);
      Check (Result = Busy and Token = 0);
      Catalog.Take_Completion (Reply);
      Check (not Reply.Present);
      for I in 1 .. 40 loop
         Before := IO;
         Catalog.Step;
         Check (IO <= Before + 1);
         Catalog.Take_Completion (Reply);
         exit when Reply.Present;
         Now := Now + 1;
      end loop;
      Check (Reply.Present and Reply.Status = Succeeded
             and Reply.Token = Own_Token and Reply.Owner = Owner
             and Reply.Epoch = A.Epoch and Reply.Register = 17
             and not Reply.Needs_Recovery and Reply.Effects_Possible);
      Check (Reply.Value = Expected);
      Check (not Catalog.Active and not Catalog.Retained_For_Recovery);
      Before := IO;
      Catalog.Take_Completion (Reply);
      Catalog.Step;
      Check (not Reply.Present and IO = Before);
   end Run;
begin
   Catalog.Install (A, Accepted);
   Check (Accepted);
   Catalog.Install (A, Accepted);
   Check (not Accepted);
   Reject (Other_Owner, 17, Epoch, False, 0);
   Reject (Owner, 17, Epoch - 1, False, 0);
   Reject (Owner, 18, Epoch, False, 0);
   Reject (Owner, 17, Epoch, False, 1);
   Reject (Owner, 17, Epoch, True, 16#10#);
   for Value in Byte range 0 .. 15 loop
      Run (True, Value, 0);
      Run (False, 0, Value);
      Check (Cells (255) = Value and Cells (0) = 16#A5#);
   end loop;
   Catalog.Revoke (17, Epoch - 1, Accepted);
   Check (not Accepted);
   Catalog.Revoke (17, Epoch, Accepted);
   Check (Accepted);
   Reject (Owner, 17, Epoch, False, 0);
   Catalog.Install (A, Accepted);
   Check (not Accepted);
   A.Epoch := Epoch + 1;
   A.Writable := False;
   A.Write_Mask := 0;
   Catalog.Install (A, Accepted);
   Check (Accepted);
   Reject (Owner, 17, Epoch, False, 0);
   Reject (Owner, 17, A.Epoch, True, 0);
   Run (False, 0, 15);
   declare
      B : Admission := A;
   begin
      B.Register := 18;
      B.Cell := 0;
      B.Readable := False;
      B.Writable := True;
      B.Exact_Write_Safe := False;
      B.Write_Mask := 255;
      Catalog.Install (B, Accepted);
      Check (not Accepted);
      B.Exact_Write_Safe := True;
      Catalog.Install (B, Accepted);
      Check (Accepted);
      Reject (Owner, 18, B.Epoch, False, 0);
      Catalog.Revoke (18, B.Epoch, Accepted);
      Check (Accepted);
      B.Register := 19;
      Catalog.Install (B, Accepted);
      Check (not Accepted); -- retired ID history is not silently recycled
   end;
   Catalog.Submit (Owner, 17, A.Epoch, False, 0,
                   Now + 100, 100, Result, Token);
   Check (Result = Started);
   Catalog.Request_Stop;
   Before := IO;
   Catalog.Step;
   Catalog.Take_Completion (Reply);
   Check (Reply.Present and Reply.Status = Stopped and not Reply.Effects_Possible
          and not Reply.Needs_Recovery and IO = Before and not Catalog.Active);
   declare
      use ACPI_Region_Protocol;
      use type Interfaces.Unsigned_64;
      Packet_In, Packet_Out : Packet;
      Deferred : Boolean;
      Fact : Catalog.Completion;
   begin
      Packet_In := (Label => Read_Operation,
                    Data => [Interfaces.Unsigned_64 (A.Epoch), 17, 0, 0],
                    others => <>);
      for Bad in 0 .. 7 loop
         declare
            Invalid : Packet := Packet_In;
         begin
            case Bad is
               when 0 => Invalid.Length := 3;
               when 1 => Invalid.Flags := 1;
               when 2 => Invalid.Reserved := 1;
               when 3 => Invalid.Label := 8;
               when 4 => Invalid.Data (3) := 1;
               when 5 => Invalid.Data (2) := 1;
               when 6 => Invalid.Label := Write_Operation; Invalid.Data (2) := 256;
               when 7 => Invalid.Data (1) := 0;
            end case;
            Before := IO;
            Wire.Begin_Call (Owner, Invalid, Now + 100, 100,
                             Deferred, Token, Packet_Out);
            Check (not Deferred and Token = 0 and IO = Before);
            Check (Valid_Reply (Packet_Out, False)
                   and Packet_Out.Data (0) /= ACPI_Region_Protocol.Outcome'Pos (Done));
         end;
      end loop;
      Before := IO;
      Wire.Begin_Call (Other_Owner, Packet_In, Now + 100, 100,
                       Deferred, Token, Packet_Out);
      Check (not Deferred and Token = 0 and IO = Before);
      Wire.Begin_Call (Owner, Packet_In, Now + 100, 100,
                       Deferred, Token, Packet_Out);
      Check (Deferred and Token > Last_Token and IO = Before);
      Last_Token := Token;
      Wire.Collect (Fact, Packet_Out);
      Check (not Fact.Present);
      for I in 1 .. 40 loop
         Catalog.Step;
         Wire.Collect (Fact, Packet_Out);
         exit when Fact.Present;
         Now := Now + 1;
      end loop;
      Check (Fact.Present and Fact.Token = Last_Token and Fact.Owner = Owner);
      Check (Valid_Reply (Packet_Out, False)
             and Packet_Out.Data (0) = ACPI_Region_Protocol.Outcome'Pos (Done)
             and Packet_Out.Data (1) = 15);
      Wire.Collect (Fact, Packet_Out);
      Check (not Fact.Present);
   end;
   Unknown_Write := True;
   Catalog.Submit (Owner, 17, A.Epoch, False, 0,
                   Now + 100, 100, Result, Token);
   Check (Result = Started);
   for I in 1 .. 40 loop
      Catalog.Step;
      Wire.Collect (Reply, Fault_Reply);
      exit when Reply.Present;
      Now := Now + 1;
   end loop;
   Check (Reply.Present and Reply.Status = Access_Unknown
          and Reply.Needs_Recovery and Reply.Effects_Possible
          and Reply.Owner = Owner and Reply.Value = 0);
   Check (ACPI_Region_Protocol.Valid_Reply (Fault_Reply, False));
   Check (ACPI_Region_Protocol.Decode (Fault_Reply, False).Status =
          ACPI_Region_Protocol.Indeterminate);
   Check (Catalog.Active and Catalog.Retained_For_Recovery);
   Catalog.Revoke (17, A.Epoch, Accepted);
   Check (not Accepted);
   Before := IO;
   Catalog.Submit (Owner, 17, A.Epoch, False, 0,
                   Now + 100, 100, Result, Token);
   Check (Result = Busy and Token = 0);
   Catalog.Take_Completion (Reply);
   Catalog.Step;
   Check (not Reply.Present and IO = Before);
   declare
      type Times is array (Positive range <>) of Microseconds;
   begin
      for Start of Times'[1, Microseconds'Last - 1] loop
         declare
            Time : Microseconds := Start;
            Accesses : Natural := 0;
            procedure Sample (Stamp : out Microseconds; Available : out Boolean) is
            begin Stamp := Time; Available := True; end Sample;
            procedure Read_Test (Register : Read_Register; Value : out Byte; Result : out Access_Result) is
            begin
               Check (Register = Status_Register);
               Accesses := Accesses + 1; Value := Event_Pending; Result := Completed;
            end Read_Test;
            procedure Write_Test (Register : Write_Register; Value : Byte; Result : out Access_Result) is
            begin
               Check (Register = Command_Register and Value = Query_Command);
               Accesses := Accesses + 1; Result := Completed;
            end Write_Test;
            package E is new ACPI_EC_Transactions (Read_Test, Write_Test, Sample, Owned);
            package C is new ACPI_EC_Registers (E, 1);
            use type C.Progress_Kind, C.Submission;
            Hint : C.Progress;
            Submission : C.Submission;
            Token : C.Operation_ID;
            Fact : C.Query_Completion;
         begin
            Hint := C.Next_Progress (Time); Check (Hint.Kind = C.No_Work and Hint.At_Time = 0);
            C.Submit_Query (Start + 1, 100, Submission, Token); Check (Submission = C.Started);
            Check (C.Next_Progress (Time).Kind = C.Run_Step and Accesses = 0);
            C.Step; C.Step; -- initial event status,then query command
            Check (Accesses = 2 and C.Next_Progress (Time).Kind = C.Run_Step);
            C.Step; -- fresh postwrite sample establishes earliest status poll
            Hint := C.Next_Progress (Time);
            Check (Hint.Kind = C.Wait_Until and Hint.At_Time = Start + 1 and Accesses = 2);
            for Repeat in 1 .. 3 loop
               Check (C.Next_Progress (Time).Kind = C.Wait_Until
                 and C.Next_Progress (Time).At_Time = Hint.At_Time and Accesses = 2);
            end loop;
            Time := Hint.At_Time;
            Check (C.Next_Progress (Time).Kind = C.Run_Step);
            C.Step; -- deadline wins over poll; no third register callback
            Check (Accesses = 2 and C.Next_Progress (Time).Kind = C.Run_Step);
            C.Take_Query (Fact);
            Check (Fact.Present and Fact.Status = Deadline_Reached and Fact.Needs_Recovery);
            Hint := C.Next_Progress (Time);
            Check (Hint.Kind = C.No_Work and Hint.At_Time = 0 and C.Active and C.Retained_For_Recovery);
            C.Step; Check (Accesses = 2);
         end;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("EC register checks:" & Checks'Image);
end EC_Register_Tests;
