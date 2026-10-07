------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Datagram_Rings;
with CuBit.Grant_References;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Protocols;
with CuBit.Stream_Regions;

package body CuBit.Channels is

   package Rings renames CuBit.Channel_Rings;
   package DR renames CuBit.Datagram_Rings;
   package KA renames CuBit.Kernel_ABI;
   package KC renames CuBit.Kernel_Calls;
   package MG renames CuBit.Memory_Grants;
   package GR renames CuBit.Grant_References;
   use type CC.Channel_Policy;
   use type DR.Put_Result;
   use type DR.Take_Result;
   use type CuBit.Control_Events.Event_Kind;
   use type CuBit.Protocols.Wire_Size_Kind;

   Page_Bytes : constant := CC.Page_Bytes;
   Word_Bytes : constant := 8;
   Consumer_Words_Offset : constant := 128;
   Waiting : constant := 1;
   Not_Waiting : constant := 0;

   ---------------------------------------------------------------------------
   --  Shared words
   ---------------------------------------------------------------------------

   function At_Byte (Base : Unsigned_64; Offset : Natural) return System.Address is
     (To_Address (Integer_Address (Base + Unsigned_64 (Offset))));

   function Word (Base : Unsigned_64; Offset : Natural) return Unsigned_64;
   function Word (Base : Unsigned_64; Offset : Natural) return Unsigned_64 is
      Value : constant Unsigned_64 with Import, Volatile, Address => At_Byte (Base, Offset);
   begin
      return Value;
   end Word;

   procedure Set_Word (Base : Unsigned_64; Offset : Natural; Value : Unsigned_64);
   procedure Set_Word (Base : Unsigned_64; Offset : Natural; Value : Unsigned_64) is
      Target : Unsigned_64 with Import, Volatile, Address => At_Byte (Base, Offset);
   begin
      Target := Value;
   end Set_Word;

   function Index_Word (Base : Unsigned_64; Offset : Natural) return Rings.Index is
     (Rings.Index (Word (Base, Offset) and 16#FFFF_FFFF#));

   --  x86-64 keeps stores, and loads, in program order; the compiler must too.
   procedure Fence;
   procedure Fence is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
   end Fence;

   --  A store, then a load of the peer's word: x86-64 may let the load pass
   --  the store, so each side could miss the other's write (a lost wake).
   --  Both sides of a wake handshake store their own word, fence, then load
   --  the other's.
   procedure Full_Fence;
   procedure Full_Fence is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Full_Fence;

   procedure Zero (Base : Unsigned_64; Bytes : Unsigned_64);
   procedure Zero (Base : Unsigned_64; Bytes : Unsigned_64) is
   begin
      for Offset in 0 .. Natural (Bytes / Word_Bytes) - 1 loop
         Set_Word (Base, Offset * Word_Bytes, 0);
      end loop;
   end Zero;

   ---------------------------------------------------------------------------
   --  Memory: owned pages, released once their grant has retired
   ---------------------------------------------------------------------------

   Retiring_Slots : constant := 16;
   type Retiring_Region is record
      Used  : Boolean := False;
      Base  : Unsigned_64 := 0;
      Bytes : Unsigned_64 := 0;
      Grant : MG.Grant_Reference := (slot => 0, generation => 1);
   end record;
   Retiring : array (1 .. Retiring_Slots) of Retiring_Region;

   procedure Release (Base, Bytes : Unsigned_64);
   procedure Release (Base, Bytes : Unsigned_64) is
      Ignore : Unsigned_64;
   begin
      Ignore := KC.Call (KA.Release_Owned_Memory, Base, Bytes);
   end Release;

   procedure Release_Retired;
   procedure Release_Retired is
   begin
      for R of Retiring loop
         if R.Used and then MG.Retirement_Confirmed (R.Grant) then
            Release (R.Base, R.Bytes);
            R := (others => <>);
         end if;
      end loop;
   end Release_Retired;

   --  Keep Base until Grant retires (or release it now if it has).
   procedure Retire (Base, Bytes : Unsigned_64; Grant : MG.Grant_Reference);
   procedure Retire (Base, Bytes : Unsigned_64; Grant : MG.Grant_Reference) is
   begin
      if MG.Retirement_Confirmed (Grant) then
         Release (Base, Bytes);
         return;
      end if;
      for R of Retiring loop
         if not R.Used then
            R := (Used => True, Base => Base, Bytes => Bytes, Grant => Grant);
            return;
         end if;
      end loop;
      --  No room to remember it: the pages stay with the process (never
      --  reused while a peer may still map them).
   end Retire;

   function Allocate (Bytes : Unsigned_64) return Unsigned_64;
   function Allocate (Bytes : Unsigned_64) return Unsigned_64 is
      Base : constant Unsigned_64 := KC.Call (KA.Allocate_Owned_Memory, Bytes);
   begin
      return (if Base = KA.Failed then 0 else Base);
   end Allocate;

   ---------------------------------------------------------------------------
   --  Regions
   ---------------------------------------------------------------------------

   function Data_Bytes (Item : CC.Contract) return Unsigned_64 is
     (Unsigned_64 (CP.Region_Pages (Item)) * Page_Bytes)
   with Pre => CC.Valid (Item);

   function Ring_Size (Item : CC.Contract) return Rings.Ring_Size is
     (Item.Pages * Page_Bytes)
   with Pre => CC.Valid (Item) and then Item.Kind = CC.Queue;

   --  This side's region for C: the data region (producers) or the index
   --  page (consumers of a policy that needs one); 0 when none.
   --  An arena belongs to the side that lends it (Producing): the other
   --  side maps it, writable, and has no index page.
   --  A duplex side owns its region whichever side it is: the opener's
   --  (Producing) of Pages, the acceptor's of Buffers.
   function Own_Region_Bytes (Item : CC.Contract; S : Side) return Unsigned_64 is
     (if Item.Kind = CC.Duplex
      then (if S = Producing then Data_Bytes (Item)
            else Unsigned_64 (CP.Acceptor_Region_Pages (Item)) * Page_Bytes)
      else
        (case S is
           when Producing => Data_Bytes (Item),
           when Consuming =>
             (if Item.Kind = CC.Queue and then CP.Needs_Index (Item.Policy)
              then CP.INDEX_BYTES else 0)));

   --  Grants and mappings: an arena is written by both sides.
   function Writable (Item : CC.Contract) return Boolean is (Item.Kind = CC.Arena);
   function Access_For (Item : CC.Contract) return MG.Required_Access is
     (if Writable (Item) then MG.Write_Access else MG.Read_Access);

   function Peer_Region_Bytes (Item : CC.Contract; S : Side) return Unsigned_64 is
     (Own_Region_Bytes (Item, (if S = Producing then Consuming else Producing)));

   --  Make this side's region and grant it, read-only with events, by
   --  Grant (a procedure that creates the grant for the peer).
   generic
      with procedure Grant
        (Base : Unsigned_64; Pages : Natural; Reference : out MG.Grant_Reference;
         Created : out Boolean);
   procedure Make_Own (C : in out Channel; Made : out Boolean);
   procedure Make_Own (C : in out Channel; Made : out Boolean) is
      Bytes : constant Unsigned_64 := Own_Region_Bytes (C.Item, C.This_Side);
      Created : Boolean;
   begin
      Made := True;
      if Bytes = 0 then
         return;
      end if;
      Made := False;
      C.Own_Base := Allocate (Bytes);
      if C.Own_Base = 0 then
         return;
      end if;
      C.Own_Bytes := Bytes;
      Zero (C.Own_Base,
            (if C.This_Side = Producing and then C.Item.Kind /= CC.Duplex
             then CP.CONTROL_BYTES else Bytes));
      if C.This_Side = Producing and then C.Item.Kind /= CC.Duplex then
         Set_Word (C.Own_Base, CP.ELEMENT_OFFSET,
                   Unsigned_64 (C.Item.Element.Identity) and 16#FFFF#);
         if C.Item.Kind = CC.Queue then
            C.Writer := Rings.New_Producer (Ring_Size (C.Item));
         end if;
      end if;
      Grant (C.Own_Base, Natural (Bytes / Page_Bytes), C.Own_Grant, Created);
      if not Created then
         Release (C.Own_Base, Bytes);
         C.Own_Base := 0;
         return;
      end if;
      C.Has_Own := True;
      Made := True;
   end Make_Own;

   procedure Start_Reading (C : in out Channel);
   procedure Start_Reading (C : in out Channel) is
   begin
      if C.This_Side = Consuming and then C.Item.Kind = CC.Queue then
         C.Reader := Rings.New_Consumer (Ring_Size (C.Item), Index_Word (C.Peer_Base, CP.PRODUCED_OFFSET));
         C.Cursor := Index_Word (C.Peer_Base, CP.PRODUCED_OFFSET);
      end if;
   end Start_Reading;

   ---------------------------------------------------------------------------
   --  Opening
   ---------------------------------------------------------------------------

   function Label_For (S : Side) return Unsigned_32 is
     (if S = Producing then CP.OP_OPEN_PRODUCING else CP.OP_OPEN_CONSUMING);

   --  The opener's side, before the request goes out: its own region and
   --  grant, and the request.
   procedure Prepare_Open
     (Endpoint : CuBit.Messages.CapabilitySlot; Item : CC.Contract; This_Side : Side;
      Connector : Unsigned_16; C : out Channel; M : out CuBit.Messages.Message;
      Prepared : out Boolean);
   procedure Prepare_Open
     (Endpoint : CuBit.Messages.CapabilitySlot; Item : CC.Contract; This_Side : Side;
      Connector : Unsigned_16; C : out Channel; M : out CuBit.Messages.Message;
      Prepared : out Boolean)
   is
      procedure Grant_Via
        (Base : Unsigned_64; Pages : Natural; Reference : out MG.Grant_Reference;
         Created : out Boolean);
      procedure Grant_Via
        (Base : Unsigned_64; Pages : Natural; Reference : out MG.Grant_Reference;
         Created : out Boolean) is
      begin
         MG.Create_Via_Capability
           (Endpoint, To_Address (Integer_Address (Base)), Pages, Writable (Item), Reference, Created,
            notify => True);
      end Grant_Via;
      procedure Make is new Make_Own (Grant_Via);
      Words : constant CC.Words := CC.Encode (Item);
   begin
      Release_Retired;
      C := (Active => False, Item => Item, This_Side => This_Side, Opener => True,
            Endpoint => Endpoint, others => <>);
      M := CuBit.Messages.NULL_MESSAGE;
      Make (C, Prepared);
      if not Prepared then
         return;
      end if;
      --  The connector rides in the tag's unauthenticated field.
      M.tag := (label => Label_For (This_Side), length => CP.Open_Words, flags => 0,
                reserved => Connector);
      M.words := [Words (0), Words (1), Words (2),
                  (if C.Has_Own then GR.Encode (C.Own_Grant) else 0)];
   end Prepare_Open;

   procedure Finish_Open
     (C : in out Channel; Reply : CuBit.Messages.Message;
      Result : out Open_Result; Refusal : out CP.Open_Refusal)
   is
      Peer_Bytes : Unsigned_64;
      Mapped : System.Address;
      Acquired : Boolean;
      Accepted_Pages : constant Unsigned_64 := Reply.words (2);
   begin
      Result := Failed;
      Refusal := CP.Unsupported;
      if Reply.tag.label /= KA.Reply_OK then
         if Reply.tag.label = KA.Reply_Error then
            Result := Refused;
            Refusal := (if Reply.words (0) in 1 .. 4
                        then CP.Open_Refusal'Enum_Val (Reply.words (0)) else CP.Unsupported);
         end if;
         Close (C);
         return;
      end if;
      --  The acceptor may name the capacity it has (a broadcast outlet's
      --  ring): a smaller or larger power of two that still holds the
      --  element.
      if C.Item.Kind = CC.Queue and then Accepted_Pages /= 0 then
         if Accepted_Pages > Unsigned_64 (CC.Ring_Pages'Last)
           or else not CC.Valid ((C.Item with delta Pages => Natural (Accepted_Pages)))
           or else (C.This_Side = Producing and then Natural (Accepted_Pages) /= C.Item.Pages)
         then
            Close (C);
            return;
         end if;
         C.Item.Pages := Natural (Accepted_Pages);
      end if;
      C.Peer_Number := Reply.words (0);
      Peer_Bytes := Peer_Region_Bytes (C.Item, C.This_Side);
      if Peer_Bytes > 0 then
         if not GR.Valid_Wire (Reply.words (1)) then
            Close (C);
            return;
         end if;
         C.Peer_Grant := GR.Decode (Reply.words (1));
         MG.Acquire_Via_Capability
           (C.Endpoint, C.Peer_Grant, 0, Peer_Bytes, Access_For (C.Item), Mapped, Acquired);
         if not Acquired then
            Close (C);
            return;
         end if;
         C.Has_Peer := True;
         C.Peer_Base := Unsigned_64 (To_Integer (Mapped));
      end if;
      C.Active := True;
      Start_Reading (C);
      Result := Opened;
   end Finish_Open;

   procedure Open
     (Endpoint : CuBit.Messages.CapabilitySlot; Item : CC.Contract; This_Side : Side;
      C : out Channel; Result : out Open_Result; Refusal : out CP.Open_Refusal;
      Connector : Unsigned_16 := 0)
   is
      M : CuBit.Messages.Message;
      Prepared : Boolean;
      Ignore_Tag : CuBit.Messages.MessageTag;
   begin
      Refusal := CP.Unsupported;
      Prepare_Open (Endpoint, Item, This_Side, Connector, C, M, Prepared);
      if not Prepared then
         Result := Failed;
         return;
      end if;
      Ignore_Tag := CuBit.Messages.capCall (Endpoint, M);
      Finish_Open (C, M, Result, Refusal);
   end Open;

   procedure Begin_Open
     (Endpoint : CuBit.Messages.CapabilitySlot; Item : CC.Contract; This_Side : Side;
      Token : Unsigned_64; C : out Channel; Submitted : out Boolean;
      Connector : Unsigned_16 := 0)
   is
      M : CuBit.Messages.Message;
   begin
      Prepare_Open (Endpoint, Item, This_Side, Connector, C, M, Submitted);
      if Submitted then
         Submitted := CuBit.Messages.capSubmit (Endpoint, M, Token);
         if not Submitted then
            Close (C);
         end if;
      end if;
   end Begin_Open;

   procedure Decode_Open
     (Request : CuBit.Messages.Message; Is_Open : out Boolean; Valid : out Boolean;
      Item : out CC.Contract; Opener_Side : out Side; Connector : out Unsigned_16)
   is
      Accepted : Boolean;
   begin
      Is_Open := Request.tag.label in CP.OP_OPEN_PRODUCING | CP.OP_OPEN_CONSUMING;
      Opener_Side := (if Request.tag.label = CP.OP_OPEN_PRODUCING then Producing else Consuming);
      Connector := Request.tag.reserved;
      Valid := False;
      Item := (others => <>);
      if not Is_Open or else Request.tag.length /= CP.Open_Words then
         return;
      end if;
      CC.Decode ([Request.words (0), Request.words (1), Request.words (2)], Item, Accepted);
      Valid := Accepted;
   end Decode_Open;

   function Refusal_Reply (Why : CP.Open_Refusal) return CuBit.Messages.Message is
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      M.tag := (label => KA.Reply_Error, length => 1, flags => 0, reserved => 0);
      M.words (0) := CP.Open_Refusal'Enum_Rep (Why);
      return M;
   end Refusal_Reply;

   procedure Accept_Open
     (From : CuBit.Messages.ProcessID; Request : CuBit.Messages.Message;
      Number : Unsigned_64; C : out Channel; Reply : out CuBit.Messages.Message)
   is
      procedure Grant_To
        (Base : Unsigned_64; Pages : Natural; Reference : out MG.Grant_Reference;
         Created : out Boolean);
      procedure Grant_To
        (Base : Unsigned_64; Pages : Natural; Reference : out MG.Grant_Reference;
         Created : out Boolean) is
      begin
         MG.Create_For_Process
           (From, To_Address (Integer_Address (Base)), Pages, Writable (C.Item), Reference, Created,
            notify => True);
      end Grant_To;
      procedure Make is new Make_Own (Grant_To);
      Is_Open, Valid, Made, Acquired : Boolean;
      Item : CC.Contract;
      Opener_Side : Side;
      Ignore_Connector : Unsigned_16;
      Peer_Bytes : Unsigned_64;
      Mapped : System.Address;
   begin
      Release_Retired;
      C := (others => <>);
      Decode_Open (Request, Is_Open, Valid, Item, Opener_Side, Ignore_Connector);
      if not Valid then
         Reply := Refusal_Reply (CP.Unsupported);
         return;
      end if;
      C := (Active => False, Item => Item,
            This_Side => (if Opener_Side = Producing then Consuming else Producing),
            Opener => False, Peer => From, others => <>);
      Peer_Bytes := Peer_Region_Bytes (Item, C.This_Side);
      if Peer_Bytes > 0 then
         if not GR.Valid_Wire (Request.words (3)) then
            Reply := Refusal_Reply (CP.Bad_Grant);
            return;
         end if;
         C.Peer_Grant := GR.Decode (Request.words (3));
         MG.Acquire (C.Peer_Grant, From, 0, Peer_Bytes, Access_For (Item), Mapped, Acquired);
         if not Acquired then
            Reply := Refusal_Reply (CP.Bad_Grant);
            return;
         end if;
         C.Has_Peer := True;
         C.Peer_Base := Unsigned_64 (To_Integer (Mapped));
      end if;
      Make (C, Made);
      if not Made then
         Close (C);
         Reply := Refusal_Reply (CP.No_Room);
         return;
      end if;
      C.Active := True;
      Start_Reading (C);
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => KA.Reply_OK, length => CP.Reply_Words,
                    flags => 0, reserved => 0);
      Reply.words (0) := Number;
      Reply.words (1) := (if C.Has_Own then GR.Encode (C.Own_Grant) else 0);
      Reply.words (2) := Unsigned_64 (Item.Pages);
   end Accept_Open;

   procedure Accept_Shared
     (From : CuBit.Messages.ProcessID; Request : CuBit.Messages.Message;
      Number : Unsigned_64; Base : Unsigned_64; Pages : CC.Ring_Pages;
      C : out Channel; Reply : out CuBit.Messages.Message)
   is
      Is_Open, Valid, Created : Boolean;
      Item : CC.Contract;
      Opener_Side : Side;
      Ignore_Connector : Unsigned_16;
   begin
      C := (others => <>);
      Decode_Open (Request, Is_Open, Valid, Item, Opener_Side, Ignore_Connector);
      if not Valid or else Opener_Side /= Consuming or else Item.Policy /= CC.Drop_Oldest then
         Reply := Refusal_Reply (CP.Unsupported);
         return;
      end if;
      Item.Pages := Pages;
      if not CC.Valid (Item) then
         Reply := Refusal_Reply (CP.Unsupported);
         return;
      end if;
      C := (Active => True, Item => Item, This_Side => Producing, Opener => False, Peer => From,
            Own_Base => Base, Own_Bytes => Data_Bytes (Item), Shares_Region => True,
            others => <>);
      MG.Create_For_Process
        (From, To_Address (Integer_Address (Base)), CP.Region_Pages (Item), False, C.Own_Grant,
         Created, notify => True);
      if not Created then
         C := (others => <>);
         Reply := Refusal_Reply (CP.No_Room);
         return;
      end if;
      C.Has_Own := True;
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => KA.Reply_OK, length => CP.Reply_Words, flags => 0, reserved => 0);
      Reply.words (0) := Number;
      Reply.words (1) := GR.Encode (C.Own_Grant);
      Reply.words (2) := Unsigned_64 (Pages);
   end Accept_Shared;

   ---------------------------------------------------------------------------
   --  The data plane
   ---------------------------------------------------------------------------

   procedure Kick (C : Channel) is
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Ignore : Boolean;
   begin
      if C.Opener then
         M.tag := (label => CP.OP_KICK, length => 1, flags => 0, reserved => 0);
         M.words (0) := C.Peer_Number;
         Ignore := CuBit.Messages.capSubmit (C.Endpoint, M, CuBit.Messages.NO_COMPLETION_TOKEN);
      end if;
   end Kick;

   --  The consumer's index, as far as the producer can trust it.
   procedure Learn_Consumed (C : in out Channel);
   procedure Learn_Consumed (C : in out Channel) is
      Ignore_Accepted : Boolean;
   begin
      if C.Has_Peer then
         Rings.Accept_Consumed (C.Writer, Index_Word (C.Peer_Base, CP.CONSUMED_OFFSET),
                                Ignore_Accepted);
      end if;
   end Learn_Consumed;

   function All_Taken (C : in out Channel) return Boolean is
   begin
      if not C.Active or else C.This_Side /= Producing or else not C.Has_Peer then
         return False;
      end if;
      Learn_Consumed (C);
      return C.Writer.Fill = 0;
   end All_Taken;

   function Free_Bytes (C : in out Channel) return Natural is
   begin
      if not C.Active or else C.This_Side /= Producing then
         return 0;
      end if;
      Learn_Consumed (C);
      return Rings.Space (C.Writer);
   end Free_Bytes;

   procedure Put
     (C : in out Channel; Data : System.Address; Length : Natural; Result : out Put_Result)
   is
      Put_Result_Of_Ring : DR.Put_Result;
   begin
      if not C.Active or else C.This_Side /= Producing or else C.Item.Kind /= CC.Queue then
         Result := Closed;
         return;
      end if;
      --  Bounded elements: at most the bound; fixed ones: exactly it.
      if Length > CC.Largest_Element or else Length > Natural (C.Item.Element.Wire_Size)
        or else (C.Item.Element.Sizing = CuBit.Protocols.Fixed_Size
                 and then Length /= Natural (C.Item.Element.Wire_Size))
      then
         Result := Too_Large;
         return;
      end if;
      if C.Item.Policy = CC.Drop_Oldest then
         Result := (if CuBit.Stream_Regions.Write (C.Own_Base, C.Writer, Data, Length)
                    then Put else Too_Large);
         return;
      end if;
      declare
         Ring : Rings.Bytes (0 .. C.Writer.Size - 1)
           with Import, Address => At_Byte (C.Own_Base, CP.CONTROL_BYTES);
         Source : constant Rings.Bytes (0 .. Length - 1) with Import, Address => Data;
      begin
         Learn_Consumed (C);
         DR.Put (C.Writer, Ring, Source, Put_Result_Of_Ring);
         if Put_Result_Of_Ring = DR.No_Room and then C.Item.Policy = CC.Lossless then
            --  Arm, then look once more: the consumer kicks after it moves.
            Set_Word (C.Own_Base, CP.PRODUCER_WAITING_OFFSET, Waiting);
            Full_Fence;
            Learn_Consumed (C);
            DR.Put (C.Writer, Ring, Source, Put_Result_Of_Ring);
         end if;
         case Put_Result_Of_Ring is
            when DR.Put =>
               Set_Word (C.Own_Base, CP.PRODUCER_WAITING_OFFSET, Not_Waiting);
               Fence;
               Set_Word (C.Own_Base, CP.PRODUCED_OFFSET, Unsigned_64 (C.Writer.Produced));
               Full_Fence;
               if Word (C.Peer_Base, CP.CONSUMER_WAITING_OFFSET) /= Not_Waiting then
                  Kick (C);
               end if;
               Result := Put;
            when DR.No_Room =>
               if C.Item.Policy = CC.Shed_Newest then
                  Set_Word (C.Own_Base, CP.SHED_OFFSET, Word (C.Own_Base, CP.SHED_OFFSET) + 1);
                  Result := Shed;
               else
                  Result := Full;
               end if;
            when DR.Too_Large =>
               Result := Too_Large;
         end case;
      end;
   end Put;

   procedure Release (C : in out Channel) is
   begin
      if not C.Active or else C.This_Side /= Consuming or else C.Own_Base = 0 then
         return;
      end if;
      Fence;
      Set_Word (C.Own_Base, CP.CONSUMED_OFFSET, Unsigned_64 (C.Reader.Consumed));
      Full_Fence;
      if Word (C.Peer_Base, CP.PRODUCER_WAITING_OFFSET) /= Not_Waiting then
         Kick (C);
      end if;
   end Release;

   procedure Take
     (C : in out Channel; Into : System.Address; Maximum : Natural;
      Length : out Natural; Result : out Take_Result; Hold : Boolean := False)
   is
      Accepted, Ignore_Truncated : Boolean;
      Taken_From_Ring : DR.Take_Result;
   begin
      Length := 0;
      if not C.Active or else C.This_Side /= Consuming or else not C.Has_Peer
        or else C.Item.Kind /= CC.Queue
      then
         Result := Closed;
         return;
      end if;
      if C.Item.Policy = CC.Drop_Oldest then
         Length := CuBit.Stream_Regions.Read
           (C.Peer_Base, Ring_Size (C.Item), C.Cursor, Into, Maximum);
         Result := (if Length > 0 then Taken else Empty);
         return;
      end if;
      Rings.Accept_Produced (C.Reader, Index_Word (C.Peer_Base, CP.PRODUCED_OFFSET), Accepted);
      if not Accepted then
         Result := Malformed;
         return;
      end if;
      declare
         Ring : constant Rings.Bytes (0 .. C.Reader.Size - 1)
           with Import, Address => At_Byte (C.Peer_Base, CP.CONTROL_BYTES);
         Target : Rings.Bytes (0 .. Maximum - 1) with Import, Address => Into;
      begin
         DR.Take (C.Reader, Ring, Target, Length, Ignore_Truncated, Taken_From_Ring);
      end;
      case Taken_From_Ring is
         when DR.Taken =>
            if not Hold then
               Release (C);
            end if;
            --  A fixed-size element of another size is the producer's
            --  fault: it is consumed and reported, never handed over.
            if C.Item.Element.Sizing = CuBit.Protocols.Fixed_Size
              and then Length /= Natural (C.Item.Element.Wire_Size)
            then
               Length := 0;
               Result := Malformed;
            else
               Result := Taken;
            end if;
         when DR.Empty =>
            Result := Empty;
         when DR.Malformed =>
            Result := Malformed;
      end case;
   end Take;

   function Arm (C : Channel) return Boolean is
      Probe : Rings.Consumer := C.Reader;
      Accepted : Boolean;
   begin
      if not C.Active or else C.This_Side /= Consuming or else C.Own_Base = 0 then
         return True;
      end if;
      Set_Word (C.Own_Base, CP.CONSUMER_WAITING_OFFSET, Waiting);
      Full_Fence;
      Rings.Accept_Produced (Probe, Index_Word (C.Peer_Base, CP.PRODUCED_OFFSET), Accepted);
      if Accepted and then Probe.Available > 0 then
         Disarm (C);
         return False;
      end if;
      return True;
   end Arm;

   procedure Disarm (C : Channel) is
   begin
      if C.Own_Base /= 0 and then C.This_Side = Consuming then
         Set_Word (C.Own_Base, CP.CONSUMER_WAITING_OFFSET, Not_Waiting);
      end if;
   end Disarm;

   function Buffer_Address (C : Channel; Buffer : Natural) return System.Address is
     (At_Byte ((if C.This_Side = Producing then C.Own_Base else C.Peer_Base),
               CP.CONTROL_BYTES + Buffer * C.Item.Pages * Page_Bytes));

   function Number_Of (Request : CuBit.Messages.Message) return Unsigned_64 is
     (Request.words (0));

   function Shed_Count (C : Channel) return Unsigned_64 is
     (if not C.Active then 0
      elsif C.This_Side = Producing then Word (C.Own_Base, CP.SHED_OFFSET)
      elsif C.Has_Peer then Word (C.Peer_Base, CP.SHED_OFFSET)
      else 0);

   procedure Set_Consumer_Word (C : Channel; Index : Consumer_Word_Index; Value : Unsigned_64) is
   begin
      if C.Active and then C.This_Side = Consuming and then C.Own_Base /= 0 then
         Set_Word (C.Own_Base, Consumer_Words_Offset + Index * Word_Bytes, Value);
      end if;
   end Set_Consumer_Word;

   function Consumer_Word (C : Channel; Index : Consumer_Word_Index) return Unsigned_64 is
     (if C.Active and then C.This_Side = Producing and then C.Has_Peer
      then Word (C.Peer_Base, Consumer_Words_Offset + Index * Word_Bytes) else 0);

   function Ended (C : Channel; Event : CuBit.Control_Events.Event) return Boolean is
     (C.Active
      and then ((Event.Kind = CuBit.Control_Events.Grant_Revoked and then C.Has_Peer
                 and then Event.Slot = Unsigned_64 (C.Peer_Grant.slot)
                 and then Event.Generation = Unsigned_64 (C.Peer_Grant.generation))
                or else
                (Event.Kind = CuBit.Control_Events.Grant_Returned and then C.Has_Own
                 and then Event.Slot = Unsigned_64 (C.Own_Grant.slot)
                 and then Event.Generation = Unsigned_64 (C.Own_Grant.generation))));

   procedure Close (C : in out Channel) is
      Done : Boolean;
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      if C.Opener and then C.Active then
         M.tag := (label => CP.OP_CLOSE, length => 1, flags => 0, reserved => 0);
         M.words (0) := C.Peer_Number;
         Done := CuBit.Messages.capSubmit (C.Endpoint, M, CuBit.Messages.NO_COMPLETION_TOKEN);
      end if;
      if C.Has_Peer then
         MG.Return_Acquisition (C.Peer_Grant, Done);
      end if;
      if C.Has_Own then
         MG.Revoke (C.Own_Grant, Done);
         if not C.Shares_Region then
            Retire (C.Own_Base, C.Own_Bytes, C.Own_Grant);
         end if;
      elsif C.Own_Base /= 0 and then not C.Shares_Region then
         Release (C.Own_Base, C.Own_Bytes);
      end if;
      C := (others => <>);
   end Close;

end CuBit.Channels;
