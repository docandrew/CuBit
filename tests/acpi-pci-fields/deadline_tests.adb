pragma Ada_2022;
with ACPI_PCI_Protocol;
with ACPI_Field_Protocol_Frames;
with Ada.Text_IO;
procedure Deadline_Tests is
   package Q renames ACPI_PCI_Protocol;
   package W renames Q.Wire;
   use type W.Time_Instant, Q.Microsecond_Deadline;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   Encoded : constant Q.Encoded_Request := Q.Read_Request (1, 1, ((0, 0, 0, 0), 0, 256),
     16#60#, Q.P.T.Dword_Access, 10_000);
   Reply : Q.Packet;
   Limit : constant W.Time_Instant := W.Time_Instant ((Q.Microsecond_Deadline'Last - 1) / 1_000);
begin
   for N in Q.Microsecond_Deadline range 0 .. 1_000_000 loop
      Check (Q.IPC_Deadline_For (N) = W.Time_Instant (N / 1_000 + (if N mod 1_000 = 0 then 0 else 1)));
   end loop;
   Check (Q.IPC_Deadline_For (Q.No_Finite_Deadline) = W.No_Finite_Deadline);
   Check (Q.IPC_Deadline_For (Q.No_Finite_Deadline - 1) /= W.No_Finite_Deadline);
   Check (Q.Data_Deadline_For (W.No_Finite_Deadline) = Q.No_Finite_Deadline);
   Check (Q.Data_Deadline_For (Limit) = Q.Microsecond_Deadline (Limit) * 1_000);
   Check (Q.Data_Deadline_For (Limit + 1) = Q.No_Finite_Deadline);
   Check (Q.Data_Deadline_For (0) = 0 and then Q.Data_Deadline_For (10) = 10_000);
   Check (ACPI_Field_Protocol_Frames.Owned_Label (Q.Header_Operation));
   Check (ACPI_Field_Protocol_Frames.Request_Valid (Q.Header_Request));
   Check (ACPI_Field_Protocol_Frames.Reply_Valid (Q.Header_Request, Q.Header_Result (1, 1)));
   Check (ACPI_Field_Protocol_Frames.Request_Valid (Encoded.Item));
   Check (ACPI_Field_Protocol_Frames.Reply_Valid (Encoded.Item, Q.Read_Result (16#E000_0000#)));
   Reply := Q.Read_Result (16#E000_0000#); Reply.Flags := 1;
   Check (not ACPI_Field_Protocol_Frames.Reply_Valid (Encoded.Item, Reply));
   Reply := Q.Error_Reply (Q.Read, Q.Not_Ready);
   Check (ACPI_Field_Protocol_Frames.Reply_Valid (Encoded.Item, Reply));
   Check (not ACPI_Field_Protocol_Frames.Reply_Valid (Q.Header_Request, Reply));
   Ada.Text_IO.Put_Line ("deadline/frame checks" & Checks'Image & " PASS");
end Deadline_Tests;
