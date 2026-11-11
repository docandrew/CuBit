pragma Ada_2022;
with Capability_ABI;
with CuBit.Messages;
with CuBit.Monotonic;
package body ACPI_PCI_Window_Client is
   package IPC renames H.IPC;
   package M renames CuBit.Messages;
   use type Q.Word, Q.Disposition, Q.Access_Mode;
   use type H.ABI.Kind, IPC.Carried_Kind, M.MessageTag;
   function Ready (Run : Context) return Boolean is (Run.Accepted);
   function Memory (Run : Context) return H.Device_Memory is (Run.Value);
   function Description (Run : Context) return Q.Reply is (Run.Info);
   function Requested (Run : Context) return Q.Request is (Run.Request);

   procedure Release (Run : in out Context; Released : out Boolean) is
      Closed : constant IPC.Close_Result := IPC.Close (Run.Owned);
   begin
      Released := Closed.Closed;
      if Released then
         Run.Owned := (Kind => IPC.Absent);
         Run.Value := H.No_Device_Memory;
         Run.Info := (others => <>);
         Run.Request := (others => <>);
         Run.Accepted := False;
      end if;
   end Release;

   procedure Acquire
     (Run : in out Context; Target : H.Endpoint; Expected_Server : P.Process_ID;
      Request : Q.Request; Deadline : Q.Word; Result : out Report)
   is
      Endpoint : IPC.Description;
      Request_Packet : Q.Packet;
      Msg : M.Message := M.NULL_MESSAGE;
      Tag : M.MessageTag;
      Received : IPC.Carried;
      Now : Q.Word;
      procedure Reject (Why : Outcome) is
         Released : Boolean;
      begin
         Result.Status := Why;
         Release (Run, Released);
         if not Released then Result.Status := Cleanup_Pending; end if;
      end Reject;
   begin
      Result := (others => <>);
      if Run.Accepted then
         Result.Status := Already_Held;
         return;
      elsif Run.Owned.Kind = IPC.Present then
         Result.Status := Cleanup_Pending;
         return;
      elsif not Q.Valid (Request) or else Request.Mode /= Q.Read_Only
        or else Deadline = M.Wait_Forever
      then
         return;
      end if;
      Endpoint := IPC.Describe (Target);
      if not P.Is_Process (Expected_Server) or else not Endpoint.Current
        or else Endpoint.Kind /= H.ABI.Endpoint
        or else Endpoint.Object /= P.To_Word (Expected_Server)
        or else Endpoint.Authority_Tag /= Q.Backend_Tag
        or else not Endpoint.Held.Read or else not Endpoint.Held.Write
        or else Endpoint.Held.Grant or else Endpoint.Held.Execute or else Endpoint.Held.Revoke
      then
         Result.Status := Invalid_Provider;
         return;
      end if;
      Now := CuBit.Monotonic.Milliseconds;
      if Deadline <= Now then
         Result.Status := Deadline_Passed;
         return;
      end if;
      Request_Packet := Q.Encode (Request);
      Msg.tag := (label => Request_Packet.Label, length => Request_Packet.Length,
                   protocol => 0, ipc => <>, reserved => 0);
      Msg.words := M.MessageWords (Request_Packet.Words);
      Tag := IPC.Call_Accepting (Target, Msg, Deadline, Received);
      -- Retain the kernel-reported carried handle BEFORE parsing any bytes.
      -- Every rejection path below closes it, even for malformed/denied replies.
      Run.Owned := Received;
      if Tag /= Msg.tag then
         Reject (Invalid_Reply);
         return;
      elsif Received.Kind = IPC.Refused then
         -- This is authenticated receive metadata: the kernel could not
         -- install the offered handle in our table. No handle is owned.
         Result.Reason := Q.Capacity_Refused;
         Reject (Denied);
         return;
      end if;
      declare
         Decoded : constant Q.Decoded_Reply := Q.Decode
           ((Label => Msg.tag.label, Length => Msg.tag.length,
             Flags => Q.Word (Capability_ABI.Flag_Byte
               (0, Capability_ABI.IPC_Flags (Msg.tag.ipc))),
             Protocol => Q.Word (Msg.tag.protocol), Reserved => Q.Word (Msg.tag.reserved),
             Words => Q.Word_Array (Msg.words)), Request.ID,
            Has_Capability => Received.Kind = IPC.Present);
      begin
         if not Decoded.Valid then
            Reject (Invalid_Reply);
         elsif Decoded.Item.Status /= Q.Granted then
            Result.Reason := Decoded.Item.Status;
            Reject (Denied);
         else
            declare
               Accepted : constant IPC.Accepted_Device_Memory :=
                 IPC.Accept_Device_Memory (Received);
            begin
               if not Accepted.OK then
                  Reject (Invalid_Memory);
                  return;
               end if;
               declare
                  D : constant IPC.Description := IPC.Describe (Accepted.Value);
               begin
                  if not D.Current or else D.Kind /= H.ABI.Device_Memory
                    or else D.Authority_Tag /= Capability_ABI.Firmware_PCI_Config_Tag
                    or else not D.Held.Read or else D.Held.Write or else D.Held.Grant
                    or else D.Held.Execute or else D.Held.Revoke
                    or else D.Object /= Decoded.Item.First
                    or else D.Parameter /= Q.Bus_Bytes
                  then
                     Reject (Invalid_Memory);
                     return;
                  end if;
                  Run.Value := Accepted.Value;
                  Run.Info := Decoded.Item;
                  Run.Request := Request;
                  Run.Accepted := True;
                  Result := (Acquired, Q.Granted);
               end;
            end;
         end if;
      end;
   end Acquire;
   procedure Move
     (Run : in out Context; Owned : out IPC.Carried;
      Value : out H.Device_Memory; Info : out Q.Reply;
      Request : out Q.Request; Accepted : out Boolean) is
   begin
      Owned := Run.Owned; Value := Run.Value; Info := Run.Info;
      Request := Run.Request; Accepted := Run.Accepted;
      Run.Owned := (Kind => IPC.Absent);
      Run.Value := H.No_Device_Memory;
      Run.Info := (others => <>); Run.Request := (others => <>);
      Run.Accepted := False;
   end Move;
end ACPI_PCI_Window_Client;
