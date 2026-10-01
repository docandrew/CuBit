with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with Native_GPU_Presentation;
with Native_GPU_Probe_Protocol;

-- Bootstrap diagnostic consumer, NOT an ANV device or a render client.
-- Supervisor must install stable endpoints: driver4 and Desktop21. Neither
-- slot may be replaced during grant lifetime. devmgr binds Desktop through
-- the preinstalled supervisor endpoint before any pixel grant is acquired.
procedure Main is
   package D renames CuBit.Desktop_Protocol;
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   package P renames Native_GPU_Presentation;
   package Q renames Native_GPU_Probe_Protocol;
   use type D.Status_Code;
   use type D.Input_Event_Kind;
   use type D.Surface_Name;
   Driver : constant CapabilitySlot := Q.Viewer_Driver_Slot;
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

   function Send (Request : D.Wire_Message) return D.Wire_Message is
      Msg : Message := CuBit.Desktop_Messages.From_Wire (Request);
      Returned : MessageTag;
   begin
      Returned := capCall (CAP_SLOT_DESKTOP, Msg);
      if Returned /= Msg.tag then return (others => <>); end if;
      return CuBit.Desktop_Messages.To_Wire (Msg);
   end Send;

   function Probe (Retire : Boolean) return MessageWords is
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
      Action : constant Q.Operation :=
        (if Retire then Q.Retire_Target else Q.Read_Target);
   begin
      Msg.tag := (Label, 4, 0, 0);
      Msg.words := MessageWords (Q.Request (Action, (if Retire then Root else 0)));
      Returned := capCall (Driver, Msg);
      if Returned /= (Label, 4, 0, 0) or else Msg.tag /= Returned or else
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
         Bound : Boolean := False;
      begin
         for Attempt in 1 .. 600 loop
            Msg := NULL_MESSAGE;
            Msg.tag := (Q.Desktop_Binding_Label, 4, 0, 0);
            Msg.words := [1, 0, 0, 0];
            Returned := capCall (Q.Viewer_Supervisor_Slot, Msg);
            exit when Returned /= (Q.Desktop_Binding_Label, 4, 0, 0) or else
              Msg.tag /= Returned or else not Q.Valid_Desktop_Reply (Q.Words (Msg.words));
            if Msg.words (0) = 0 then Bound := True; exit; end if;
            exit when Msg.words (0) /= 2;
            Ignored := syscall (SYSCALL_SLEEP, 100);
         end loop;
         if not Bound then
            debugPrint ("gpu-viewer: Desktop binding unavailable" & ASCII.LF);
            return;
         end if;
      end;
      if D.Decode_Hello_Result (Send (D.Encode_Hello (D.Current_Revision))).Status /= D.Success
      then debugPrint ("gpu-viewer: Desktop unavailable" & ASCII.LF); return; end if;
      Reply_Words := Probe (False);
      if Reply_Words (0) /= 0 or else Reply_Words (3) /= 16384 or else
        not R.Valid_Wire (Reply_Words (2))
      then debugPrint ("gpu-viewer: completed target unavailable" & ASCII.LF); return; end if;
      Root := Reply_Words (2);
      G.Acquire_Via_Capability (Driver, R.Decode (Root), 0, 16384,
        G.Read_Access, Address, OK);
      if not OK then return; end if;
      Borrowed := True;
      Created := D.Decode_Creation_Result
        (Send (D.Encode_Create ((64, 64, D.Window_Surface))));
      if Created.Status /= D.Success then return; end if;
      Surface := Created.Surface;
      Status := P.Forward (CAP_SLOT_DESKTOP, Root, 0, 16384, Child'Access);
      if Status /= 0 then return; end if;
      G.Return_Acquisition (R.Decode (Root), OK);
      if not OK then return; end if;
      Borrowed := False;
      Status := P.Attach_Linear (CAP_SLOT_DESKTOP, Unsigned_64 (Surface), Child, 64, 64, 256);
      if Status /= 0 then return; end if;
      if D.Decode_Status (Send (D.Encode_Present ((Surface, (0, 0, 64, 64)))),
        D.Present_Surface) /= D.Success then return; end if;
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
end Main;
