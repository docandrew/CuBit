pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;
with CuBit.Channels;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Host_Bridge_Windows;
with CuBit.Request_Channels;
with ACPI_Host_Bridge_Snapshot;
with ACPI_Host_Bridge_Wire;
with ACPI_PCI_Window_Exchange;
with PCI_BAR_Placement;
with Host_Bridge_Rows;
with Devmgr_Host_Windows;
with Devmgr_Heap;
with Devmgr_Growable;

package body Devmgr_Host_Bridge_Consumer is
   package C renames CuBit.Channels;
   package CC renames CuBit.Channel_Contracts;
   package M renames CuBit.Host_Bridge_Windows;
   package S renames ACPI_Host_Bridge_Snapshot;
   package W renames ACPI_Host_Bridge_Wire;
   package PL renames PCI_BAR_Placement;
   package HW renames Devmgr_Host_Windows;
   package RC is new CuBit.Request_Channels (M.Request, M.Result, M.Queue_Slot_Bits, M.Queue_Slot_Bits);
   use type CC.Contract, C.Side, M.Snapshot_State, HW.Update_Result;

   Page_Bytes : constant := 4_096;
   Channel_Number : constant Unsigned_64 := 1;

   --  The last sequence accepted for each root of the current channel.
   type Root_Sequence is record
      Root, Sequence : Unsigned_64 := 0;
   end record;
   package Sequences is new Devmgr_Growable (Root_Sequence, Devmgr_Heap.Allocate, Devmgr_Heap.Release);
   use type Sequences.Growth_Status;

   Link    : C.Channel;
   Server  : RC.Server;
   Active  : Boolean := False;
   Last    : Sequences.Store;

   procedure Note (Line : String) is
   begin
      Report (Line);
   end Note;

   function Image (V : Unsigned_64) return String is
      T : constant String := Unsigned_64'Image (V);
   begin
      return T (T'First + 1 .. T'Last);
   end Image;

   function Address (Base : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Base)));

   --  The host-bridge contract at some measured page count.
   function Ours (Item : CC.Contract) return Boolean is
     (Item.Pages >= M.Minimum_Client_Pages and then Item = M.Measured_Contract (Item.Pages));

   procedure End_Session (Why : String) is
   begin
      if Active then
         RC.Detach_Server (Server);
         C.Close (Link);
         Sequences.Clear (Last);
         Active := False;
         Note ("devmgr: host-bridge channel ended: " & Why);
      end if;
   end End_Session;

   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Answer : out CuBit.Messages.Message)
   is
      Valid : Boolean;
      Item : CC.Contract;
      Opener_Side : C.Side;
      Connector : Unsigned_16;
   begin
      Answer := CuBit.Messages.NULL_MESSAGE;
      C.Decode_Open (Request, Is_Open, Valid, Item, Opener_Side, Connector);
      if not Is_Open or else not Valid or else not Ours (Item) then
         Is_Open := False;
         return;
      end if;
      if Request.authorityTag /= ACPI_PCI_Window_Exchange.Backend_Tag or else Opener_Side /= C.Producing then
         Note ("devmgr: host-bridge channel refused: not acpi.svc's pci-windows endpoint, or not producing");
         Answer := C.Refusal_Reply (CuBit.Channel_Protocol.Not_Admitted);
         return;
      end if;
      End_Session ("replaced by a new open");
      C.Accept_Open (From, Request, Channel_Number, Link, Answer);
      if not Link.Active or else Link.Own_Base = 0 or else Link.Peer_Base = 0 then
         Note ("devmgr: host-bridge channel refused: its grant was unusable");
         return;
      end if;
      RC.Attach_Server (Server, Address (Link.Own_Base), Address (Link.Peer_Base));
      Active := True;
      Note ("devmgr: host-bridge channel opened (" & Image (Unsigned_64 (Item.Pages)) & " client pages)");
   end Handle_Open;

   procedure Handle_Close
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Closed : out Boolean) is
   begin
      Closed := Active and then C.Closed_By (Link, From, Request);
      if Closed then
         End_Session ("acpi.svc closed it");
      end if;
   end Handle_Close;

   --  Sequence must rise per root; records it when it does.
   function Fresh (Root, Sequence : Unsigned_64) return Boolean is
      Room : Sequences.Capacity_Report;
   begin
      for I in 1 .. Sequences.Length (Last) loop
         declare
            E : constant Root_Sequence := Sequences.Element_At (Last, I);
         begin
            if E.Root = Root then
               if Sequence <= E.Sequence then
                  return False;
               end if;
               Sequences.Set_At (Last, I, (Root, Sequence));
               return True;
            end if;
         end;
      end loop;
      Sequences.Append (Last, (Root, Sequence), Room);
      return Room.Status = Sequences.Grown;
   end Fresh;

   --  Decode one copied packet and apply it. Returns the answer's code.
   procedure Apply
     (Root, Sequence : Unsigned_64; Packet : S.Bytes; Outcome : out M.Acceptance; Published : in out Boolean)
   is
      Row_Capacity : constant Natural :=
        Natural'Max (1, (if Packet'Length > S.Header_Bytes then (Packet'Length - S.Header_Bytes) / S.Row_Bytes else 0));
      Items  : W.Rows (1 .. Row_Capacity);
      Source : S.Bytes (1 .. Natural'Max (1, Packet'Length));
      Info   : S.Header;
      Result : S.Report;
      use type S.Outcome;
   begin
      S.Decode (Packet, Info, Items, Source, Result);
      if Result.Status /= S.Done then
         Outcome := M.Malformed;
         Note ("devmgr: host-bridge snapshot refused: " & S.Outcome'Image (Result.Status));
         return;
      end if;
      if Info.Root_ID /= Root or else Info.Sequence /= Sequence then
         Outcome := M.Identity_Mismatch;
         return;
      end if;
      if not Fresh (Root, Sequence) then
         Outcome := M.Stale_Sequence;
         return;
      end if;
      Outcome := M.Accepted;
      if Info.State /= M.Complete then
         Note ("devmgr: host-bridge root " & Image (Root) & " snapshot " & M.Snapshot_State'Image (Info.State) &
               ": " & M.Failure_Reason'Image (Info.Reason) & "; its windows are unchanged");
         return;
      end if;
      declare
         Rows : W.Rows renames Items (1 .. Natural'Min (Items'Length, Natural (Info.Required_Descriptors)));
         Spans : PL.Span_Array (1 .. Natural'Max (1, Rows'Length));
         Prefetch : Host_Bridge_Rows.Flags (1 .. Spans'Length);
         Count : Natural;
         First_Bus, Last_Bus : Unsigned_8;
         Segment : constant Unsigned_16 :=
           (if Info.Segment.Present and then Info.Segment.Value <= Unsigned_64 (Unsigned_16'Last)
            then Unsigned_16 (Info.Segment.Value) else 0);
         Update : HW.Update_Result;
      begin
         if Rows'Length = 0 then
            Count := 0;
            First_Bus := 0;
            Last_Bus := Host_Bridge_Rows.Last_Bus_Number;
         else
            Host_Bridge_Rows.Windows (Rows, Spans (1 .. Rows'Length), Prefetch (1 .. Rows'Length), Count);
            Host_Bridge_Rows.Buses (Rows, Info.Base_Bus.Present, Info.Base_Bus.Value, First_Bus, Last_Bus);
         end if;
         declare
            Windows : HW.Window_Array (1 .. Natural'Max (1, Count));
         begin
            for I in 1 .. Count loop
               Windows (I) := (Span => Spans (I), Prefetchable => Prefetch (I));
            end loop;
            HW.Replace_Root (Segment, First_Bus, Last_Bus, Windows (1 .. Count), Update);
            if Update = HW.Updated then
               Published := True;
               Note ("devmgr: host-bridge root " & Image (Root) & " (segment " & Image (Unsigned_64 (Segment)) &
                     ", buses " & Image (Unsigned_64 (First_Bus)) & ".." & Image (Unsigned_64 (Last_Bus)) & "): " &
                     Image (Unsigned_64 (Count)) & " memory window(s)");
               for I in 1 .. Count loop
                  Note ("devmgr: host-bridge window " & Image (Spans (I).First * Page_Bytes) & " .. " &
                        Image (Spans (I).Past * Page_Bytes - 1) & (if Prefetch (I) then " prefetchable" else ""));
               end loop;
            else
               Note ("devmgr: host-bridge root " & Image (Root) & " windows not recorded: no memory");
            end if;
         end;
      end;
   end Apply;

   procedure Serve_One (Taken : RC.Queues.Submission; Published : in out Boolean; Ring : out Boolean) is
      Root     : constant Unsigned_64 := Taken.Item (M.Root_ID);
      Sequence : constant Unsigned_64 := Taken.Item (M.Sequence);
      Bytes    : constant Unsigned_64 := Taken.Item (M.Packet_Bytes);
      Region   : constant Unsigned_64 := Unsigned_64 (Link.Item.Pages) * Page_Bytes;
      Outcome  : M.Acceptance := M.Malformed;
   begin
      if Bytes = 0 or else Bytes > Region - Unsigned_64 (RC.Client_Payload_At) then
         Outcome := M.Too_Large;
      else
         --  Copy, then validate: the private copy is read; the peer's
         --  region never is again.
         declare
            Length : constant Natural := Natural (Bytes);
            Block  : constant System.Address := Devmgr_Heap.Allocate (Unsigned_64 (Length), 8);
            use type System.Address;
         begin
            if Block = System.Null_Address then
               Outcome := M.Too_Large;
               Note ("devmgr: host-bridge snapshot of " & Image (Bytes) & " bytes refused: no memory to copy it");
            else
               declare
                  --  SEC-025: single-read the peer's payload, copied once into Copy and never read again.
                  Peer : constant S.Bytes (1 .. Length)
                    with Import, Address => Address (Link.Peer_Base + Unsigned_64 (RC.Client_Payload_At));
                  --  SEC-025: own  devmgr's private heap block, which no peer can write.
                  Copy : S.Bytes (1 .. Length) with Import, Address => Block;
               begin
                  Copy := Peer;
                  Apply (Root, Sequence, Copy, Outcome, Published);
               end;
               Devmgr_Heap.Release (Block);
            end if;
         end;
      end if;
      RC.Complete (Server, Taken.Tag, [M.Root_ID => Root, M.Sequence => Sequence,
                                         M.Disposition => M.Acceptance_Code (Outcome)], Ring);
   end Serve_One;

   procedure Pump (Published : out Boolean) is
      Taken : RC.Queues.Submission;
      Got, Ring_Taken, Ring_Done : Boolean;
      Signalled : C.Signal_Result;
   begin
      Published := False;
      if not Active then
         return;
      end if;
      while RC.Can_Take (Server) loop
         RC.Take (Server, Taken, Got, Ring_Taken);
         exit when not Got;
         Serve_One (Taken, Published, Ring_Done);
         if Ring_Taken or else Ring_Done then
            C.Signal (Link, Sync => False, Result => Signalled);
         end if;
      end loop;
   end Pump;

   function Arm return Boolean is
   begin
      return not Active or else RC.Arm_For_Requests (Server);
   end Arm;
end Devmgr_Host_Bridge_Consumer;
