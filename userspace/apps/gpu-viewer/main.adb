with Ada.Command_Line;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with Native_GPU_Presentation;
with Native_GPU_Probe_Protocol;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.C_ABI;
with Startup;

-- Bootstrap diagnostic consumer, NOT an ANV device or a render client.
-- devmgr launches it with two startup endpoints, both badged
-- Native_GPU_Probe_Protocol.Viewer_Tag: "gpu-probe" (intel-gpu) and
-- "device-manager" (devmgr). It asks devmgr for Desktop's endpoint (the reply
-- carries it, CAP-002) before any pixel grant is acquired.
procedure Main is
   package D renames CuBit.Desktop_Protocol;
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   package P renames Native_GPU_Presentation;
   package Q renames Native_GPU_Probe_Protocol;
   use type D.Status_Code;
   use type D.Input_Event_Kind;
   use type D.Surface_Name;
   Driver : constant CuBit.Handles.Endpoint := Startup.Gpu_Probe;
   Desktop : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Label : constant Unsigned_32 := Q.Label;
   Root : Unsigned_64 := 0;
   Child : aliased Unsigned_64 := 0;
   Surface : D.Surface_Name := 0;
   Borrowed : Boolean := False;
   Address : System.Address;
   OK : Boolean;
   Ignored : Unsigned_64;
   Status : Unsigned_32;
   After_Serial : Unsigned_64 := 0;
   --  Why Run ended: the exit code devmgr names in its log line.
   Outcome : Q.Viewer_Outcome := Q.Closed;
   use type Q.Viewer_Outcome;
   --  Desktop binding: devmgr's codes (Q.Valid_Desktop_Reply), how often to
   --  ask while there is no desktop yet, and how long apart.
   Binding_Bound : constant Unsigned_64 := 0;
   Binding_Not_Yet : constant Unsigned_64 := 2;
   Binding_Attempts : constant := 600;
   Binding_Retry_Ms : constant := 100;

   function Send (Request : D.Wire_Message) return D.Wire_Message is
      Msg : Message := CuBit.Desktop_Messages.From_Wire (Request);
      Returned : MessageTag;
   begin
      Returned := capCall (Desktop, Msg, CuBit.Messages.Wait_Forever);
      if Returned /= Msg.tag then return (others => <>); end if;
      return CuBit.Desktop_Messages.To_Wire (Msg);
   end Send;

   function Probe (Retire : Boolean) return MessageWords is
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
      Action : constant Q.Operation :=
        (if Retire then Q.Retire_Target else Q.Read_Target);
   begin
      Msg.tag := (label => Label, length => 4, protocol => 0, ipc => <>, reserved => 0);
      Msg.words := MessageWords (Q.Request (Action, (if Retire then Root else 0)));
      Returned := capCall (Driver, Msg, CuBit.Messages.Wait_Forever);
      if Returned /= (label => Label, length => 4, protocol => 0, ipc => <>, reserved => 0) or else Msg.tag /= Returned or else
        not Q.Valid_Reply (Action, Q.Words (Msg.words))
      then return [5, 1, 0, 0]; end if;
      return Msg.words;
   end Probe;

   procedure Cleanup is
      Result : MessageWords;
   begin
      if Surface /= 0 then
         -- Destroy before retiring the child: an attachment reply was not a
         -- reader-release fence. On uncertainty retain/poll, never reattach.
         declare
            Reply_Wire : constant D.Wire_Message := Send
              (D.Encode_Destroy ((Surface => Surface)));
            pragma Unreferenced (Reply_Wire);
         begin null; end;
      end if;
      while Borrowed loop
         G.Return_Acquisition (R.Decode (Root), OK);
         if OK then Borrowed := False; end if;
         if Borrowed then Ignored := syscall (SYSCALL_SLEEP, 100); end if;
      end loop;
      if Child /= 0 then
         loop
            Status := P.Retire (Child);
            exit when Status = 0;
            -- Keep this process and its capabilities alive on uncertainty.
            Ignored := syscall (SYSCALL_SLEEP, 100);
         end loop;
      end if;
      if Root /= 0 then
         loop
            Result := Probe (True);
            exit when Result (0) = 0;
            -- Only an exact successful retirement reply is a release fence.
            -- Denial, malformed transport and pending retain our endpoint.
            Ignored := syscall (SYSCALL_SLEEP, 100);
         end loop;
      end if;
   end Cleanup;

   procedure Run is
      Created : D.Creation_Result;
      Reply_Words : MessageWords;
   begin
      -- Desktop is launched independently after devmgr. Request only this
      -- pre-authorized binding, not an arbitrary service/PID/capability.
      declare
         Msg : Message;
         Returned : MessageTag;
         Carried : CuBit.Handles.IPC.Carried;
         Bound : Boolean := False;
      begin
         Outcome := Q.Binding_Timed_Out;
         for Attempt in 1 .. Binding_Attempts loop
            Msg := NULL_MESSAGE;
            Msg.tag := (label => Q.Desktop_Binding_Label, length => 4,
                        protocol => 0, ipc => <>, reserved => 0);
            Msg.words := [1, 0, 0, 0];
            --  A bound reply carries Desktop's endpoint (Read+Write).
            Returned := CuBit.Handles.IPC.Call_Accepting
              (Startup.Device_Manager, Msg, CuBit.Messages.Wait_Forever, Carried);
            if Returned.label /= Q.Desktop_Binding_Label or else
              Returned.length /= 4 or else Returned.reserved /= 0 or else
              not Q.Valid_Desktop_Reply (Q.Words (Msg.words))
            then
               Outcome := Q.Binding_Malformed;
               exit;
            end if;
            if Msg.words (0) = Binding_Bound then
               declare
                  Accepted : constant CuBit.Handles.IPC.Accepted_Endpoint :=
                    CuBit.Handles.IPC.Accept_Endpoint (Carried);
               begin
                  if Accepted.OK then
                     Desktop := Accepted.Value;
                     Bound := True;
                  else
                     Outcome := Q.Binding_Malformed;
                  end if;
               end;
               exit;
            end if;
            if Msg.words (0) /= Binding_Not_Yet then
               Outcome := Q.Binding_Refused;
               exit;
            end if;
            Ignored := syscall (SYSCALL_SLEEP, Binding_Retry_Ms);
         end loop;
         if not Bound then
            return;
         end if;
         Outcome := Q.Closed;
      end;
      if D.Decode_Hello_Result (Send (D.Encode_Hello (D.Current_Revision))).Status /= D.Success
      then Outcome := Q.Desktop_Unavailable; return; end if;
      Reply_Words := Probe (False);
      if Reply_Words (0) /= 0 or else Reply_Words (3) /= 16384 or else
        not R.Valid_Wire (Reply_Words (2))
      then Outcome := Q.Target_Unavailable; return; end if;
      Root := Reply_Words (2);
      G.Acquire_Via_Capability (Driver, R.Decode (Root), 0, 16384,
        G.Read_Access, Address, OK);
      if not OK then Outcome := Q.Acquire_Failed; return; end if;
      Borrowed := True;
      Created := D.Decode_Creation_Result
        (Send (D.Encode_Create ((64, 64, D.Window_Surface, D.Desktop_Sized))));
      if Created.Status /= D.Success then Outcome := Q.Surface_Failed; return; end if;
      Surface := Created.Surface;
      Status := P.Forward (CuBit.Handles.C_ABI.To_C (Desktop), Root, 0, 16384, Child'Access);
      if Status /= 0 then Outcome := Q.Forward_Failed; return; end if;
      G.Return_Acquisition (R.Decode (Root), OK);
      if not OK then Outcome := Q.Return_Failed; return; end if;
      Borrowed := False;
      Status := P.Attach_Linear
        (CuBit.Handles.C_ABI.To_C (Desktop), Unsigned_64 (Surface), Child, 64, 64, 256);
      if Status /= 0 then Outcome := Q.Attach_Failed; return; end if;
      if D.Decode_Status (Send (D.Encode_Present ((Surface, (0, 0, 64, 64)))),
        D.Present_Surface) /= D.Success then Outcome := Q.Present_Failed; return; end if;
      debugPrint ("gpu-viewer: completed target attached; viewer copied no pixels" & ASCII.LF);
      -- Any key closes the diagnostic. Poll bounded requests so destroyed
      -- windows also retire their child grant rather than leaving a waiter.
      loop
         declare
            Input : constant D.Input_Result := D.Decode_Input_Result
              (Send (D.Encode_Input_Request ((D.Poll_Input, Surface, After_Serial))), D.Poll_Input);
         begin
            exit when Input.Status /= D.Success;
            After_Serial := Input.Value.Serial;
            exit when Input.Value.Kind = D.Key_Pressed;
         end;
         Ignored := syscall (SYSCALL_SLEEP, 10);
      end loop;
   end Run;
begin
   Run;
   Cleanup;
   if Outcome /= Q.Closed then
      debugPrint ("gpu-viewer: ended: " & Q.Outcome_Name (Outcome) & ASCII.LF);
   end if;
   --  Always set: without it the start code exits with whatever is in RAX.
   Ada.Command_Line.Set_Exit_Status
     (Ada.Command_Line.Exit_Status (Q.Viewer_Outcome'Enum_Rep (Outcome)));
end Main;
