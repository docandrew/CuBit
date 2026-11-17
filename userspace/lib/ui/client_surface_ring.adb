------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Channel_Contracts;

package body Client_Surface_Ring is

   use type CuBit.Channels.Open_Result;
   use type Wire.Request_Kind;
   use type Token;

   function Address_Of (Base : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Base)));

   function Channel_Contract return CuBit.Channel_Contracts.Contract is
     ((SR.Element with delta Kind => CuBit.Channel_Contracts.Duplex,
                             Policy => CuBit.Channel_Contracts.Lossless,
                             Pages => SR.RC.Client_Pages, Buffers => SR.RC.Server_Pages));

   procedure Open
     (C : in out Connection; Endpoint : CuBit.Handles.Endpoint;
      Deadline : CuBit.Deadlines.Deadline; Opened : out Boolean;
      Result : out CuBit.Channels.Open_Result;
      Refusal : out CuBit.Channel_Protocol.Open_Refusal)
   is
   begin
      Opened := False;
      Result := CuBit.Channels.Failed;
      Refusal := CuBit.Channel_Protocol.Unknown_Type;
      if C.Open then
         return;
      end if;
      C.Table := [others => <>];
      C.Strays := 0;
      CuBit.Channels.Open
        (Endpoint, Channel_Contract, CuBit.Channels.Producing, C.Channel, Result, Refusal,
         Deadline => CuBit.Deadlines.Kernel_Word (Deadline));
      if Result /= CuBit.Channels.Opened then
         return;
      end if;
      SR.RC.Attach_Client (C.Ring, Address_Of (C.Channel.Own_Base), Address_Of (C.Channel.Peer_Base));
      C.Open := True;
      Opened := True;
   end Open;

   function Is_Open (C : Connection) return Boolean is (C.Open);

   procedure Close (C : in out Connection) is
   begin
      if not C.Open then
         return;
      end if;
      SR.RC.Detach_Client (C.Ring);
      CuBit.Channels.Close (C.Channel);
      C.Open := False;
      C.Wake_Owed := False;
      C.Table := [others => <>];
   end Close;

   function Outstanding (C : Connection; Kind : Wire.Request_Kind) return Natural is
      Count : Natural := 0;
   begin
      for E of C.Table loop
         if E.Used and then E.Kind = Kind then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Outstanding;

   function Can_Submit (C : Connection; Kind : Wire.Request_Kind) return Boolean is
     (C.Open and then
      (case Kind is
          when Wire.Present | Wire.Publish =>
            Outstanding (C, Wire.Present) + Outstanding (C, Wire.Publish) < Limit (Kind),
          when others => Outstanding (C, Kind) < Limit (Kind)));

   procedure Submit
     (C : in out Connection; Item : Wire.Request; Tag : out Token; Result : out Submit_Result)
   is
      Free : Natural := 0;
      Submitted, Ring : Boolean;
   begin
      Tag := 0;
      if not C.Open then
         Result := Ended;
         return;
      end if;
      if not Can_Submit (C, Item.Kind) or else not SR.RC.Can_Submit (C.Ring)
        or else C.Next_Tag = Token'Last
      then
         Result := Full;
         return;
      end if;
      for I in C.Table'Range loop
         if not C.Table (I).Used then
            Free := I;
            exit;
         end if;
      end loop;
      if Free = 0 then
         Result := Full;
         return;
      end if;
      C.Next_Tag := C.Next_Tag + 1;
      SR.RC.Submit (C.Ring, C.Next_Tag, Wire.Encode (Item), Submitted, Ring);
      if not Submitted then
         Result := Full;
         return;
      end if;
      C.Table (Free) := (True, C.Next_Tag, Item.Kind);
      Tag := C.Next_Tag;
      --  Desktop sleeps and asked to be woken (once per arming): Flush
      --  pays it, once for everything the frame queued.
      C.Wake_Owed := C.Wake_Owed or else Ring;
      Result := Queued;
   end Submit;

   procedure Flush (C : in out Connection) is
      Ignore : CuBit.Channels.Signal_Result;
   begin
      if C.Open and then C.Wake_Owed then
         CuBit.Channels.Signal (C.Channel, Sync => False, Result => Ignore);
      end if;
      C.Wake_Owed := False;
   end Flush;

   procedure Reap
     (C : in out Connection; Tag : out Token; Answer : out Wire.Result; Got : out Boolean)
   is
      Item : SR.RC.Queues.Completion;
      Found : Boolean;
   begin
      Tag := 0;
      Answer := (Kind => Wire.Present, Status => Wire.DP.Invalid_Request);
      Got := False;
      if not C.Open then
         return;
      end if;
      loop
         SR.RC.Reap (C.Ring, Item, Found);
         exit when not Found;
         for E of C.Table loop
            if E.Used and then E.Tag = Item.Tag then
               E.Used := False;
               declare
                  Decoded : constant Wire.Result_Decoding := Wire.Decode (Item.Answer, E.Kind);
               begin
                  if Decoded.Valid then
                     Tag := Item.Tag;
                     Answer := Decoded.Value;
                     Got := True;
                     return;
                  end if;
               end;
               exit;
            end if;
         end loop;
         --  Malformed, or no request of ours: dropped and counted.
         if C.Strays < Unsigned_64'Last then
            C.Strays := C.Strays + 1;
         end if;
      end loop;
   end Reap;

   function Arm (C : in out Connection) return Boolean is
   begin
      --  Desktop must hear what this side queued before it sleeps.
      Flush (C);
      return not C.Open or else SR.RC.Arm_For_Completions (C.Ring);
   end Arm;

   procedure Disarm (C : Connection) is
   begin
      if C.Open then
         SR.RC.Disarm_For_Completions (C.Ring);
      end if;
   end Disarm;

   procedure Note_Event (C : in out Connection; Event : CuBit.Control_Events.Event; Ends : out Boolean) is
   begin
      Ends := C.Open and then CuBit.Channels.Ended (C.Channel, Event);
      if Ends then
         Close (C);
      end if;
   end Note_Event;

   function Stray_Answers (C : Connection) return Unsigned_64 is (C.Strays);

end Client_Surface_Ring;
