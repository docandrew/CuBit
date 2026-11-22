pragma Ada_2022;
with Interfaces; with Ada.Text_IO; with GNAT.Source_Info;
with CuBit.Messages; with ACPI_Native_Lock_Notes; with ACPI_Lock_Trace_Protocol;
with ACPI_Global_Lock_Protocol; with ACPI_Register_Roles; with Timing_Test_Control;
procedure Native_Lock_Notes_Tests is
   package N renames ACPI_Native_Lock_Notes;
   package M renames CuBit.Messages;
   package G renames ACPI_Global_Lock_Protocol;
   package W renames ACPI_Lock_Trace_Protocol;
   use type W.Outcome, Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   Owner : constant M.Process_ID := M.From_Word (16#ABCD_0000_0000_0001#);
   Other_Owner : constant M.Process_ID := M.From_Word (16#1234_0000_0000_0001#);
   Scope : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (ACPI_Register_Roles.Frontend_Tag);
   Request, Reply : M.Message := M.NULL_MESSAGE;
   Handled : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Line : Positive := GNAT.Source_Info.Line) is
   begin Checks := Checks+1; if not Good then raise Program_Error with "check" & Checks'Image & " line" & Line'Image; end if; end Check;
   procedure Dump (Caller : M.Process_ID; Status : W.Outcome) is
   begin
      N.Handle_Dump (Caller,Request,Reply,Handled);
      Check (Handled);
      Check (Reply.tag.label = W.Reply_Label and W.Outcome'Val (Reply.words (1)) = Status);
   end Dump;
begin
   Request.tag := (label => W.Request_Label,length => 4,protocol => 0,ipc => <>,reserved => 0);
   Request.words := M.MessageWords (W.Request.Data); Request.authorityTag := Scope;
   Dump (Owner,W.Unauthorized);
   N.Begin_Request (Owner,Scope,G.Prepare); N.Finish_Request ((True,G.Ready,G.Configured,(1,1)));
   N.Begin_Request (Owner,Scope,G.Acquire); N.Observe_Word (16#CAFE_0000#,True);
   N.Observe_CAS (16#CAFE_0000#,16#CAFE_0002#,16#CAFE_0000#,True,True,True);
   N.Finish_Request ((True,G.Recorded,G.Acquired,(1,1)));
   N.Begin_Request (Owner,Scope,G.Release);
   N.Observe_CAS (16#CAFE_0003#,16#CAFE_0000#,16#CAFE_0003#,True,True,True);
   N.Observe_Signal (N.Trace.Signal_Not_Sent,0,0); N.Finish_Request ((True,G.Recorded,G.Signal_Deferred,(1,1)));
   Check (Timing_Test_Control.Lines = 0);
   Dump (Other_Owner,W.Unauthorized); Request.authorityTag := 0; Dump (Owner,W.Unauthorized);
   Request.authorityTag := Scope; Request.tag.reserved := 1; Dump (Owner,W.Malformed); Request.tag.reserved := 0;
   Dump (Owner,W.Accepted); Check (Reply.words (2) = 3);
   N.Flush (False); Check (Timing_Test_Control.Lines = 0);
   N.Begin_Maintenance; N.Observe_Signal (N.Trace.Signaled,2,2); N.Finish_Maintenance (G.Released);
   N.Flush (True); Check (Timing_Test_Control.Lines = 9);
   Check (Timing_Test_Control.Found_FACS and Timing_Test_Control.Found_Deferred and Timing_Test_Control.Found_Signal);
   Dump (Owner,W.Already_Dumped); N.Flush (True); Check (Timing_Test_Control.Lines = 9);
   Ada.Text_IO.Put_Line ("Native lock notes model checks" & Checks'Image);
end Native_Lock_Notes_Tests;
