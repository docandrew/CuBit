pragma Ada_2022;
with Ada.Command_Line;with Ada.Text_IO;with Interfaces;
with ACPI_Requests;with ACPI_Native_Endpoint;with ACPI_Native_Power_Buttons;
with CuBit.Messages;with CuBit.Handles.IPC;with CuBit.Power_Event_Exchange;
with Admission_Probe;
procedure Gate_Tests is
   package Q renames ACPI_Requests;package Endpoint renames ACPI_Native_Endpoint;
   package Buttons renames ACPI_Native_Power_Buttons;package IPC renames CuBit.Handles.IPC;
   package M renames CuBit.Messages;package E renames CuBit.Power_Event_Exchange;
   use type Interfaces.Unsigned_64;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Runtime : constant Boolean := Mode = "runtime";
   Owner : Q.State (1, 64, 64, 1, 0, 16, 64, 16);
   Input, Reply : M.Message;
   Accepted, Worked : Boolean;
   Checks : Natural := 0;
   procedure Check (Value : Boolean; Why : String) is
   begin Checks := Checks + 1;if not Value then raise Program_Error with Why;end if;end Check;
begin
   Check (Mode in "runtime" | "observer" | "unselected", "known serving mode");
   Buttons.Configure_Fixed (True);IPC.Reset;
   if Runtime then Endpoint.Select_Runtime (True, Accepted);Check (Accepted,"runtime selected");
   elsif Mode = "observer" then Endpoint.Select_Observer ((others => True),True,Accepted);Check (Accepted,"observer selected");end if;
   Admission_Probe.Quiet := False;Admission_Probe.Handle_At := 0;
   Input := M.NULL_MESSAGE;Input.tag.label := E.Button_Watch;Input.tag.length := E.Watch_Request_Words;
   Input.authorityTag := E.Authority_Tag;Input.words (E.Request_Word) := 16#FEDC_BA98_7654_3210#;
   M.Current_Reply_Authority := True;
   Endpoint.Dispatch_With_Admission (Owner,M.From_Word (16#F123_4567_89AB_CDEF#),Input,Reply,False,
     Cached_Button_Watches_Open => True);
   Check (Endpoint.Reply_Now = not Runtime,"cached watch requires selected runtime");
   Check (IPC.Live_Count = Boolean'Pos (Runtime) and IPC.Save_Calls = Boolean'Pos (Runtime),
     "observer and unselected modes acquire no saved authority");
   if not Runtime then Check (Reply.words (E.Watch_Status_Word) = E.Watch_Status'Enum_Rep (E.Watch_Unavailable),
      "closed serving mode has explicit refusal");end if;
   M.Current_Reply_Authority := True;
   Endpoint.Dispatch_With_Admission (Owner,M.From_Word (16#F123_4567_89AB_CDEF#),Input,Reply,False);
   Check (Endpoint.Reply_Now and Reply.words (E.Watch_Status_Word) = E.Watch_Status'Enum_Rep (E.Watch_Unavailable),
     "default drain keeps cached-watch permission closed");
   Check (IPC.Live_Count = Boolean'Pos (Runtime) and IPC.Save_Calls = Boolean'Pos (Runtime),
     "drain refusal preserves any existing watch");
   Input.tag.label := E.Prepare_To_Sleep;
   Admission_Probe.Gates := [others => False];Admission_Probe.Seen := [others => False];
   Endpoint.Dispatch_With_Admission (Owner,M.From_Word (16#F123_4567_89AB_CDEF#),Input,Reply,False,
     Cached_Button_Watches_Open => True);
   for Stage in 2 .. 4 loop
      Check (Admission_Probe.Seen (Stage) and not Admission_Probe.Gates (Stage),
        "cached watch permission never opens power entry/preparation or passive AML");
   end loop;
   Check (IPC.Live_Count = Boolean'Pos (Runtime),"nonwatch request preserves prior authority");
   Buttons.Begin_Stop;Buttons.Advance (True,Worked);
   Check (IPC.Live_Count = 0 and IPC.Answer_Calls = Boolean'Pos (Runtime),"stop consumes only the selected runtime watch");
   Ada.Text_IO.Put_Line ("Watch gate " & Mode & " checks" & Checks'Image);
end Gate_Tests;
