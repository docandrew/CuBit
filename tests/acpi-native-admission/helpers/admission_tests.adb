pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with ACPI_Requests;
with ACPI_Native_Endpoint;
with ACPI_Protocol;
with ACPI_Endpoint;
with Admission_Probe;
with CuBit.Messages;
procedure Admission_Tests is
   package P renames Admission_Probe;
   package Q renames ACPI_Requests;
   package E renames ACPI_Native_Endpoint;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Interfaces.Unsigned_8;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Runtime : constant Boolean := Mode = "runtime";
   Selected : constant Boolean := Mode /= "unselected";
   Server : Q.State (1, 64, 64, 1, 0, 16, 64, 16);
   Request, Reply : CuBit.Messages.Message;
   Accepted : Boolean;
   Checks, Cases : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why; end if;
   end Check;
   Expected : P.Gate_Array;
   Reached : Boolean;
begin
   if Runtime then
      E.Select_Runtime (True, Accepted);
      Check (Accepted, "runtime selection");
   elsif Mode = "observer" then
      E.Select_Observer ((others => True), True, Accepted);
      Check (Accepted, "observer selection");
      Check (P.Button_Stops = 1 and P.Entry_Stops = 1, "observer selection stops action owners");
   else
      Check (Mode = "unselected", "known test mode");
   end if;
   for Gate in Boolean loop
      for Quiet in Boolean loop
         for Blocked in Boolean loop
            for Handle_At in 0 .. 4 loop
               for Authorized in Boolean loop
                  Cases := Cases + 1;
                  P.Gates := [others => False]; P.Seen := [others => False];
                  P.Quiet := Quiet; P.Blocked := Blocked; P.Handle_At := Handle_At;
                  P.Diagnostics := 0; P.Warnings := 0; P.Button_Stops := 0;
                  Request := CuBit.Messages.NULL_MESSAGE;
                  Request.tag.label := Q.Read_Metrics; Request.tag.length := 4;
                  Request.words := [E.Metrics_Serving_State_Page, 0, 0, 0];
                  Request.authorityTag := (if Authorized then Interfaces.Unsigned_64 (ACPI_Protocol.Observer_Tag) else 0);
                  E.Dispatch_With_Admission (Server, CuBit.Messages.From_Word (16#0000_0100_0000_0007#), Request, Reply, Gate);
                  Expected := [Gate and Quiet, Gate and Runtime,
                     Gate and Runtime and not Blocked,
                     Gate and Selected and not Blocked and Quiet];
                  for Stage in P.Gates'Range loop
                     Reached := Handle_At = 0 or else Stage <= Handle_At;
                     Check (P.Seen (Stage) = Reached, "handler order and short circuit");
                     if Reached then Check (P.Gates (Stage) = Expected (Stage), "scheduler gate forwarded to action boundary"); end if;
                  end loop;
                  Check (P.Button_Stops = Boolean'Pos (Handle_At = 2 and Blocked), "only blocking entry stops buttons");
                  Check (P.Warnings = Boolean'Pos (Handle_At = 0 and not Authorized and Gate and not Blocked), "refusal log admission");
                  if not Gate then Check (P.Diagnostics = 0, "closed gate short circuits quiet callbacks"); end if;
                  Check (E.Reply_Now = (Handle_At in 0 | 3), "asynchronous handlers preserve reply ownership");
                  if Handle_At = 0 then
                     Check (Reply.tag.length = 4, "observer reply remains available while gate closed");
                     if Authorized then
                        Check (Reply.tag.label = ACPI_Endpoint.Reply_OK, "authorized metrics allowed");
                        Check (Reply.words (3) = Boolean'Pos (Runtime), "serving mode copied faithfully");
                     else
                        Check (Reply.tag.label = ACPI_Endpoint.Reply_Error, "unstamped observer refused");
                        Check (Reply.words (0) = Q.Outcome'Pos (Q.Denied), "denial does not gain authority from PID");
                     end if;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Native admission " & Mode & " cases" & Cases'Image & " checks" & Checks'Image);
end Admission_Tests;
