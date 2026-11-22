pragma Ada_2022;
package body ACPI_NVS_Client with SPARK_Mode is
   use type Word, Tick, P.Wire.Delivery, P.Outcome;
   Page_Bytes : constant Word := 4096;
   Physical_Limit : constant Word := 2 ** 47;
   function Observe (State : Context) return Statistics is
     ((State.Stage, (if State.Stage = Ready then State.Count else 0), State.Working));
   procedure In_Time (State : in out Context; Deadline : Tick; Result : out Outcome) is
      Now : Tick;
      Available : Boolean;
   begin
      if Deadline = P.Wire.No_Finite_Deadline then Result := Invalid_Deadline; return; end if;
      Read_Clock (Now, Available);
      if not Available then Result := Clock_Unavailable;
      elsif State.Seen_Clock and then Now < State.Last_Clock then Result := Clock_Reversed;
      else
         State.Last_Clock := Now; State.Seen_Clock := True;
         Result := (if Now >= Deadline then Expired else Read_Complete);
      end if;
   end In_Time;
   procedure Call
     (State : in out Context; Channel : in out Transport; Deadline : Tick;
      Item : P.Packet; Kind : P.Operation; Width : P.T.Access_Width;
      Reply : out P.Reply; Result : out Outcome) is
      Packet : P.Packet;
      Delivered : P.Wire.Delivery;
   begin
      Reply := (others => <>); In_Time (State, Deadline, Result);
      if Result /= Read_Complete then return; end if;
      Exchange (Channel, State.Destination, Deadline, Item, Packet, Delivered);
      if Delivered = P.Wire.Not_Dispatched then Result := Not_Sent; return;
      elsif Delivered /= P.Wire.Reply_Received then
         State.Stage := Uncertain; Result := Completion_Unknown; return;
      end if;
      Reply := P.Decode_Reply (Packet, Kind, Width);
      if not Reply.Valid then State.Stage := Uncertain; Result := Bad_Reply; return; end if;
      In_Time (State, Deadline, Result);
   end Call;
   procedure Load
     (State : in out Context; Channel : in out Transport; Destination : Peer;
      Deadline : Tick; Result : out Outcome) is
      Reply : P.Reply;
      Count : Record_Count;
      Item : Description;
      procedure Stop is
      begin
         if State.Stage /= Uncertain then State.Stage := Failed; end if;
         State.Working := False;
      end Stop;
   begin
      if State.Working then Result := Reentered; return; end if;
      if State.Stage /= Fresh then Result := Already_Consumed; return; end if;
      State.Stage := Loading; State.Working := True;
      if Destination = No_Peer then Result := Invalid_Peer; Stop; return; end if;
      State.Destination := Destination;
      Call (State, Channel, Deadline, P.Header_Request, P.Header, P.T.Byte_Access, Reply, Result);
      if Result /= Read_Complete then Stop; return; end if;
      if Reply.Status /= P.Ready then Result := Backend_Refused; Stop; return; end if;
      Count := Record_Count (Reply.Count); State.Epoch := Reply.Epoch;
      for I in 1 .. Count loop
         Call (State, Channel, Deadline, P.Describe_Request (State.Epoch, Word (I)),
           P.Describe, P.T.Byte_Access, Reply, Result);
         if Result /= Read_Complete then Stop; return; end if;
         if Reply.Status /= P.Ready then Result := Backend_Refused; Stop; return; end if;
         Item := (Reply.ID, Reply.Base, Reply.Length);
         if Item.Base mod Page_Bytes /= 0 or else Item.Length mod Page_Bytes /= 0
           or else Item.Base >= Physical_Limit or else Item.Length > Physical_Limit - Item.Base
         then Result := Invalid_Metadata; Stop; return; end if;
         for J in 1 .. State.Count loop
            if Item.ID = State.Items (J).ID
              or else (if Item.Base >= State.Items (J).Base
                then Item.Base - State.Items (J).Base < State.Items (J).Length
                else State.Items (J).Base - Item.Base < Item.Length)
            then Result := Invalid_Metadata; Stop; return; end if;
         end loop;
         State.Items (I) := Item; State.Count := I;
      end loop;
      Call (State, Channel, Deadline, P.Header_Request, P.Header, P.T.Byte_Access, Reply, Result);
      if Result /= Read_Complete then Stop; return; end if;
      if Reply.Status /= P.Ready or else Reply.Epoch /= State.Epoch or else Reply.Count /= Word (Count)
      then Result := Metadata_Changed; Stop; return; end if;
      State.Stage := Ready; State.Working := False; Result := Loaded;
   end Load;
   procedure Describe
     (State : Context; Ordinal : Natural; Item : out Description; Available : out Boolean) is
   begin
      Item := (others => <>);
      Available := State.Stage = Ready and then not State.Working and then Ordinal in 1 .. State.Count;
      if Available then Item := State.Items (Ordinal); end if;
   end Describe;
   procedure Read
     (State : in out Context; Channel : in out Transport; ID, Offset : Word;
      Width : P.T.Access_Width; Deadline : Tick; Value : out Word; Result : out Outcome) is
      Reply : P.Reply;
      Bytes : constant Word := Word (P.T.Octets (Width));
   begin
      Value := 0;
      if State.Working then Result := Reentered; return; end if;
      if State.Stage /= Ready then Result := Not_Ready; return; end if;
      for I in 1 .. State.Count loop
         if State.Items (I).ID = ID then
            if Offset >= State.Items (I).Length or else Offset mod Bytes /= 0
              or else Bytes > State.Items (I).Length - Offset
            then Result := Out_Of_Range; return; end if;
            State.Working := True;
            Call (State, Channel, Deadline, P.Read_Request (State.Epoch, ID, Offset, Width), P.Read, Width, Reply, Result);
            if Result = Read_Complete then
               if Reply.Status = P.Done then Value := Reply.Value;
               else State.Stage := Failed; Result := Backend_Refused;
               end if;
            elsif Result not in Not_Sent | Invalid_Deadline | Expired then
               if State.Stage /= Uncertain then State.Stage := Failed; end if;
            end if;
            State.Working := False; return;
         end if;
      end loop;
      Result := Missing_Record;
   end Read;
end ACPI_NVS_Client;
