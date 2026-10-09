------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Named I/O Streams implementation, over CuBit.Stream_Rings
--  (docs/ccl-streams.md, "The ring underneath").
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Arguments;
with CuBit.Program_Descriptions;
with CuBit.Outlet_Rings;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Channel_Rings;
with CuBit.Stream_Regions;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Outlet_Channels;

package body CuBit.Streams is
   use type CuBit.Protocols.Wire_Size_Kind;
   package Rings renames CuBit.Channel_Rings;
   package SR renames CuBit.Stream_Rings;
   package Regions renames CuBit.Stream_Regions;
   use type Rings.Index;

   package Channels renames CuBit.Channels;
   REPLY_OK  : constant Unsigned_32 := 16#F000#;

   ---------------------------------------------------------------------------
   --  Producer state: each stream's region and its writer. Each reader is a
   --  channel (CuBit.Outlet_Channels) holding its own grant of the region.
   ---------------------------------------------------------------------------

   type Subscriber_Array is array (CursorSlot) of Channels.Channel;

   type StreamState is record
      active   : Boolean      := False;
      id       : StreamId     := 0;
      pages    : Natural      := 0;      --  declared pages (Region_Pages of)
      baseAddr : Unsigned_64  := 0;      --  the region: control page, ring
      writer   : Rings.Producer;
      schema   : CuBit.Protocols.Schema_Contract :=
        CuBit.Protocols.NO_SCHEMA_CONTRACT;
      subs     : Subscriber_Array;
      --  A ring a launcher lent (Open_Outlet): its grant, returned when
      --  the launcher revokes it (Return_Revoked).
      lent     : Boolean := False;
      lentGrant : CuBit.Memory_Grants.Grant_Reference := (slot => 0, generation => 1);
   end record;

   subtype StreamIndex is Natural range 0 .. MAX_STREAMS - 1;
   streamTab : array (StreamIndex) of StreamState;

   function Declared (Pages : Natural) return SR.Declared_Pages
     renames Regions.Declared;

   function findStream (id : StreamId) return Integer;
   function findStream (id : StreamId) return Integer is
   begin
      for i in StreamIndex loop
         if streamTab (i).active and then streamTab (i).id = id then
            return i;
         end if;
      end loop;
      return -1;
   end findStream;

   --  The records a stream carries, as a reader's contract names them.
   function Element_Of (S : StreamState) return CuBit.Protocols.Schema_Contract is
     (if CuBit.Protocols.Valid (S.schema) then S.schema
      elsif Regions.Element (S.baseAddr) = Unsigned_16 (TYPE_TEXT_LINE)
      then CuBit.Protocols.TEXT_LINE_CONTRACT
      else CuBit.Protocols.RAW_BYTES_CONTRACT);

   --  A reader's channel number: its stream and slot.
   function Number_Of (Stream : StreamIndex; Slot : CursorSlot) return Unsigned_64 is
     (Unsigned_64 (Stream) * Unsigned_64 (MAX_SUBSCRIBERS) + Unsigned_64 (Slot) + 1);

   ---------------------------------------------------------------------------
   --  streamHandleSubscription: one pending open, close or list. A reader
   --  opens a channel on the outlet's connector (its ring id) and gets its
   --  own read-only grant of the outlet's region (Channels.Accept_Shared).
   ---------------------------------------------------------------------------
   function streamHandleSubscription return Boolean is
      from  : Process_ID;
      msg   : Message;
      found : Boolean;
      Ignore : Unsigned_64;
   begin
      Poll_Service_Request (from, msg, found);
      if not found then
         return False;
      end if;

      if msg.tag.label = CuBit.Channel_Protocol.OP_OPEN_CONSUMING then
         declare
            use type CuBit.Channel_Contracts.Channel_Policy;
            Is_Open, Valid : Boolean;
            Offered : CuBit.Channel_Contracts.Contract;
            Opener_Side : Channels.Side;
            Connector : Unsigned_16;
            idx : Integer;
            Answer : Message;
         begin
            Channels.Decode_Open (msg, Is_Open, Valid, Offered, Opener_Side, Connector);
            idx := findStream (StreamId (Connector));
            if not Valid or else idx < 0
              or else not CuBit.Protocols.Compatible (Element_Of (streamTab (idx)), Offered.Element)
              or else Offered.Policy /= CuBit.Channel_Contracts.Drop_Oldest
            then
               Ignore := reply (from, Channels.Refusal_Reply (CuBit.Channel_Protocol.Unknown_Type));
               return True;
            end if;
            for K in CursorSlot loop
               if not streamTab (idx).subs (K).Active then
                  Channels.Accept_Shared
                    (from, msg, Number_Of (idx, K), streamTab (idx).baseAddr,
                     SR.Ring_Pages (Declared (streamTab (idx).pages)),
                     streamTab (idx).subs (K), Answer);
                  Ignore := reply (from, Answer);
                  return True;
               end if;
            end loop;
            Ignore := reply (from, Channels.Refusal_Reply (CuBit.Channel_Protocol.No_Room));
         end;
         return True;

      elsif msg.tag.label = CuBit.Channel_Protocol.OP_CLOSE then
         --  One-way: a reader let go of its channel.
         declare
            Number : constant Unsigned_64 := Channels.Number_Of (msg);
         begin
            if Number in 1 .. Unsigned_64 (MAX_STREAMS * MAX_SUBSCRIBERS) then
               declare
                  idx : constant StreamIndex := StreamIndex ((Number - 1) / MAX_SUBSCRIBERS);
                  Slot : constant CursorSlot := CursorSlot ((Number - 1) mod MAX_SUBSCRIBERS);
               begin
                  if Channels.Closed_By (streamTab (idx).subs (Slot), from, msg) then
                     Channels.Close (streamTab (idx).subs (Slot));
                  end if;
               end;
            end if;
         end;
         return True;

      elsif msg.tag.label = OP_STREAM_LIST then
         declare
            bitmask     : Unsigned_64 := 0;
            activeCount : Unsigned_32 := 0;
            Answer      : Message := NULL_MESSAGE;
         begin
            for i in StreamIndex loop
               if streamTab (i).active then
                  bitmask := bitmask or
                     Shift_Left (Unsigned_64'(1), Natural (streamTab (i).id));
                  activeCount := activeCount + 1;
               end if;
            end loop;
            Answer.tag.label := REPLY_OK;
            Answer.words (0) := bitmask;
            Answer.words (1) := Unsigned_64 (activeCount);
            Ignore := reply (from, Answer);
         end;
         return True;
      end if;
      --  Anything else (a producing open, an unknown request) is refused
      --  with a reply: a caller is never left waiting for one.
      Ignore := reply (from, Channels.Refusal_Reply (CuBit.Channel_Protocol.Unsupported));
      return True;
   end streamHandleSubscription;

   --  A reader's grant came back (it closed, or died): its slot is free.
   function Forget_Returned (Event : CuBit.Control_Events.Event) return Boolean is
   begin
      for S of streamTab loop
         for Sub of S.subs loop
            if Channels.Ended (Sub, Event) then
               Channels.Close (Sub);
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Forget_Returned;

   ---------------------------------------------------------------------------
   --  Rings
   ---------------------------------------------------------------------------

   --  Take over a ring a launcher lent this process (initialized by it):
   --  produce into it as into one of our own, from where it stands.
   procedure streamAdopt (id : StreamId; pages : Natural; base : Unsigned_64);
   procedure streamAdopt (id : StreamId; pages : Natural; base : Unsigned_64) is
   begin
      for i in StreamIndex loop
         if not streamTab (i).active then
            streamTab (i) := (
               active   => True,
               id       => id,
               pages    => pages,
               baseAddr => base,
               writer   => Regions.Writer_Of (base, pages),
               schema   => CuBit.Protocols.NO_SCHEMA_CONTRACT,
               subs     => (others => <>),
               others   => <>);
            return;
         end if;
      end loop;
   end streamAdopt;

   procedure streamCreate (id        : StreamId;
                            pages     : Natural;
                            entryType : TypeTag) is
      Region : constant Positive := SR.Region_Pages (Declared (pages));
      ret : Unsigned_64;
      idx : Integer := -1;
   begin
      for i in StreamIndex loop
         if not streamTab (i).active then
            idx := i;
            exit;
         end if;
      end loop;
      if idx < 0 then
         return;  -- no free slots
      end if;
      --  A page more than the region: grants are whole pages, and the break
      --  is not page aligned.
      ret := syscall (SYSCALL_SBRK, Unsigned_64 (Region + 1) * SR.PAGE_BYTES);
      if ret = Unsigned_64'Last then
         return;
      end if;
      declare
         base : constant Unsigned_64 :=
           (ret + SR.PAGE_BYTES - 1) / SR.PAGE_BYTES * SR.PAGE_BYTES;
      begin
         Regions.Initialize (base, Unsigned_16 (entryType));
         streamTab (idx) := (
            active   => True,
            id       => id,
            pages    => pages,
            baseAddr => base,
            writer   => SR.New_Writer (SR.Ring_Bytes (Declared (pages))),
            schema   => CuBit.Protocols.NO_SCHEMA_CONTRACT,
            subs     => (others => <>),
            others   => <>);
      end;
   end streamCreate;

   procedure streamCreateTyped
     (id        : StreamId;
      pages     : Natural;
      entryType : TypeTag;
      schema    : CuBit.Protocols.Schema_Contract)
   is
      idx : Integer;
   begin
      if not CuBit.Protocols.Valid (schema) then
         return;
      end if;
      streamCreate (id, pages, entryType);
      idx := findStream (id);
      if idx >= 0 then
         streamTab (idx).schema := schema;
      end if;
   end streamCreateTyped;

   function Open_Outlet (Name : String) return StreamId is
      package LA renames CuBit.Launch_Arguments;
      package PD renames CuBit.Program_Descriptions;
      package PR renames CuBit.Outlet_Rings;
      use type LA.Validation;
      use type PD.Connector_Direction;
      use type PD.Element_Kind;
      Launch_Length : Unsigned_64
      with Import, Convention => C,
           External_Name => "__cubit_launch_arguments_length";
      S : PD.Signature;
      Ring_Table : PR.Table;
      Accepted, Found, Rings_Valid : Boolean := False;
      Index : PD.Connector_Index;
   begin
      if Launch_Length not in LA.Header_Bytes .. LA.Maximum_Block_Bytes then
         return NO_STREAM;
      end if;
      declare
         Item : constant LA.Block (1 .. Natural (Launch_Length))
         with Import, Address => System'To_Address (LA.Block_Address);
      begin
         if LA.Validate (Item) /= LA.Valid then
            return NO_STREAM;
         end if;
         --  After the strings: the ring table, if the launcher lent rings,
         --  then the description.
         declare
            First : constant Positive := LA.Strings_Last (Item) + 1;
            Trailer : PR.Bytes (1 .. Item'Last - First + 1);
            Present : Boolean;
            Ring_Length : PR.Table_Length;
         begin
            for K in Trailer'Range loop
               Trailer (K) := Item (First + K - 1);
            end loop;
            PR.Measure (Trailer, Present, Ring_Length);
            if Present then
               PR.Decode (Trailer (1 .. Ring_Length), Ring_Table, Rings_Valid);
            end if;
            declare
               Description : PD.Bytes (1 .. Trailer'Length - Ring_Length);
            begin
               if Description'Length = 0
                 or else Description'Length > PD.Maximum_Descriptor_Bytes
               then
                  return NO_STREAM;
               end if;
               for K in Description'Range loop
                  Description (K) := Trailer (Ring_Length + K);
               end loop;
               PD.Decode (Description, S, Accepted);
            end;
         end;
      end;
      if not Accepted then
         return NO_STREAM;
      end if;
      PD.Find_Connector (S, Name, Index, Found);
      if not Found or else S.Connectors (Index).Direction /= PD.Outlet then
         return NO_STREAM;
      end if;
      declare
         Ring : constant StreamId := StreamId (PD.Ring_Id (Index));
         Pages : constant Natural := S.Connectors (Index).Pages;
      begin
         --  The launcher's ring, when it lent one.
         if Rings_Valid then
            for E in 1 .. Ring_Table.Count loop
               if Ring_Table.Entries (E).Outlet = Index then
                  declare
                     Mapped : System.Address;
                     Ok : Boolean;
                  begin
                     CuBit.Memory_Grants.Acquire
                       (CuBit.Grant_References.Decode (Ring_Table.Entries (E).Grant),
                        From_Word (Ring_Table.Owner), 0,
                        Regions.Region_Bytes (Pages),
                        CuBit.Memory_Grants.Write_Access, Mapped, Ok);
                     if Ok then
                        streamAdopt (Ring, Pages, Unsigned_64 (To_Integer (Mapped)));
                        declare
                           Adopted : constant Integer := findStream (Ring);
                        begin
                           if Adopted >= 0 then
                              streamTab (Adopted).lent := True;
                              streamTab (Adopted).lentGrant :=
                                CuBit.Grant_References.Decode (Ring_Table.Entries (E).Grant);
                           end if;
                        end;
                        return Ring;
                     end if;
                  end;
               end if;
            end loop;
         end if;
         if S.Connectors (Index).Element = PD.Text_Lines then
            streamCreateTyped (Ring, Pages, TYPE_TEXT_LINE,
                               CuBit.Protocols.TEXT_LINE_CONTRACT);
         else
            streamCreate (Ring, Pages, TYPE_RAW_BYTES);
         end if;
         return Ring;
      end;
   end Open_Outlet;

   ---------------------------------------------------------------------------
   --  streamWrite: one record (Stream_Rings.Publish), the oldest ones
   --  evicted if there is no room; never waits for a reader.
   ---------------------------------------------------------------------------
   function streamWrite (id        : StreamId;
                          data      : System.Address;
                          len       : Unsigned_32;
                          entryType : TypeTag) return Unsigned_32
   is
      pragma Unreferenced (entryType);   --  the stream's, in the region
      idx : constant Integer := findStream (id);
      Ignore_Handled : Boolean;
   begin
      if idx < 0 or len = 0 or len > Unsigned_32 (CuBit.Channel_Rings.Maximum_Size) then
         return 0;
      end if;
      Ignore_Handled := streamHandleSubscription;
      if Regions.Write (streamTab (idx).baseAddr, streamTab (idx).writer, data, Natural (len))
      then
         return len;
      end if;
      return 0;
   end streamWrite;

   function streamWriteTyped
     (id        : StreamId;
      data      : System.Address;
      len       : Unsigned_32;
      entryType : TypeTag;
      schema    : CuBit.Protocols.Schema_Contract) return Unsigned_32
   is
      idx : constant Integer := findStream (id);
   begin
      if idx < 0 or else
        not CuBit.Protocols.Compatible (streamTab (idx).schema, schema)
        or else
        (if schema.Sizing = CuBit.Protocols.Fixed_Size then
            len /= schema.Wire_Size
         else len > schema.Wire_Size)
      then
         return 0;
      end if;
      return streamWrite (id, data, len, entryType);
   end streamWriteTyped;

   procedure streamPrint (id  : StreamId;
                           msg : String)
   is
      ignore : Unsigned_32;
      idx    : constant Integer := findStream (id);
   begin
      if msg'Length = 0 then
         return;
      end if;
      --  As records of at most TEXT_LINE_RECORD_BYTES each.
      declare
         First : Positive := msg'First;
         Last : Natural;
      begin
         while First <= msg'Last loop
            Last := Natural'Min
              (msg'Last, First + CuBit.Protocols.TEXT_LINE_RECORD_BYTES - 1);
            declare
               Piece : constant String := msg (First .. Last);
            begin
               if idx >= 0 and then CuBit.Protocols.Valid (streamTab (idx).schema) then
                  ignore := streamWriteTyped
                    (id, Piece'Address, Unsigned_32 (Piece'Length), TYPE_TEXT_LINE,
                     streamTab (idx).schema);
               else
                  ignore := streamWrite
                    (id, Piece'Address, Unsigned_32 (Piece'Length), TYPE_TEXT_LINE);
               end if;
            end;
            First := Last + 1;
         end loop;
      end;
   end streamPrint;

   function Return_Revoked (Slot, Generation : Unsigned_64) return Boolean is
      Returned : Boolean;
   begin
      for I in StreamIndex loop
         declare
            S : StreamState renames streamTab (I);
         begin
            if S.active and then S.lent
              and then Unsigned_64 (S.lentGrant.slot) = Slot
              and then Unsigned_64 (S.lentGrant.generation) = Generation
            then
               S.active := False;
               CuBit.Memory_Grants.Return_Acquisition (S.lentGrant, Returned);
               return True;
            end if;
         end;
      end loop;
      return False;
   end Return_Revoked;

   ---------------------------------------------------------------------------
   --  Readers
   ---------------------------------------------------------------------------

   procedure Subscribe_Begin
     (Endpoint : CuBit.Messages.CapabilitySlot; Stream : StreamId;
      Element : CuBit.Protocols.Schema_Contract; Token : Unsigned_64;
      Sub : out SubInfo; Submitted : out Boolean) is
   begin
      Channels.Begin_Open
        (Endpoint, CuBit.Outlet_Channels.Offer (Element), Channels.Consuming, Token, Sub.Link,
         Submitted, Connector => Unsigned_16 (Stream));
   end Subscribe_Begin;

   procedure Subscribe_Finish (Sub : in out SubInfo; Reply : Message; Subscribed : out Boolean) is
      use type Channels.Open_Result;
      Result : Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Channels.Finish_Open (Sub.Link, Reply, Result, Ignore_Refusal);
      Subscribed := Result = Channels.Opened;
   end Subscribe_Finish;

   function streamRead (sub       : in out SubInfo;
                         buf       : System.Address;
                         maxLen    : Unsigned_32;
                         entryType : out TypeTag) return Unsigned_32
   is
      use type Channels.Take_Result;
      Length : Natural;
      Result : Channels.Take_Result;
   begin
      entryType := TYPE_RAW_BYTES;
      if not sub.Link.Active then
         return 0;
      end if;
      entryType := TypeTag (Regions.Element (sub.Link.Peer_Base));
      Channels.Take (sub.Link, buf, Natural (maxLen), Length, Result);
      return (if Result = Channels.Taken then Unsigned_32 (Length) else 0);
   end streamRead;

   function streamAvailable (sub : SubInfo) return Unsigned_32 is
   begin
      if not sub.Link.Active then
         return 0;
      end if;
      return Unsigned_32 (Regions.Produced (sub.Link.Peer_Base) - sub.Link.Cursor);
   end streamAvailable;

   procedure Unsubscribe (Sub : in out SubInfo) is
   begin
      Channels.Close (Sub.Link);
   end Unsubscribe;

end CuBit.Streams;
