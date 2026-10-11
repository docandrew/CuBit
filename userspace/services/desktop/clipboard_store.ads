------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop clipboard history: typed entries, pins, and paste-once secrets
--
--  Pure state machine (no IPC, no clock, no allocation). Desktop owns the one
--  instance; docs/clipboard.md has the design. Content arrives already copied
--  out of the client's grant and validated (data-plane rule 1).
--
--  Proved (level 2, tests/clipboard):
--  * a sensitive entry is delivered at most once: the paste that delivers it
--    wipes it in the same step, and a later paste cannot return it;
--  * a paste into a sensitive target consumes the entry it delivered;
--  * an entry whose deadline has passed is never delivered;
--  * every slot without live content holds only zero bytes (wiped);
--  * sensitive entries are never pinned, always carry a deadline no later
--    than Sensitive_Lifetime after their copy, and never yield a preview.
------------------------------------------------------------------------------
with Interfaces;

package Clipboard_Store with SPARK_Mode is
   use type Interfaces.Unsigned_8;

   --  History depth (Windows keeps 25). Fixed slots keep the proof simple;
   --  24 x 64 KiB is 1.5 MiB of Desktop data.
   Capacity : constant := 24;
   --  Type name, text and CCL source together. Matches the client transfer
   --  window (Compositor_Clipboard_Protocol.Maximum_Payload).
   Maximum_Payload : constant := 65_536;
   Maximum_Type_Name : constant := 256;
   --  Explicit deadline for sensitive entries: wiped unused after this.
   Sensitive_Lifetime_Ms : constant := 45_000;

   type Millisecond is new Interfaces.Unsigned_64;

   type Payload_Length is range 0 .. Maximum_Payload;
   subtype Payload_Index is Payload_Length range 1 .. Maximum_Payload;
   type Payload_Bytes is array (Payload_Index) of Interfaces.Unsigned_8;
   subtype Type_Name_Length is Payload_Length range 0 .. Maximum_Type_Name;

   --  Representations an entry may carry. Text is UTF-8 for any text field;
   --  CCL_Value is the canonical CCL literal that reads back as the value,
   --  with its type name.
   type Representation is (Text_Form, CCL_Form);
   type Representation_Set is array (Representation) of Boolean;
   No_Representations : constant Representation_Set := [others => False];

   type Sensitivity is (Ordinary, Sensitive);

   --  Kernel-authenticated process identity of the copying app.
   type Source_Identity is new Interfaces.Unsigned_64;

   --  Names are never reused; a larger name is newer.
   type Entry_Name is new Interfaces.Unsigned_64;
   No_Entry : constant Entry_Name := 0;
   subtype Live_Entry_Name is Entry_Name range 1 .. Entry_Name'Last;

   type Slot_Count is range 0 .. Capacity;
   subtype Slot_Index is Slot_Count range 1 .. Capacity;

   --  Available: content present. Consumed / Expired: a sensitive tombstone
   --  kept for the manager (mask icon, no content); its bytes are zero.
   type Entry_Phase is (Vacant, Available, Consumed, Expired);
   subtype Tombstone is Entry_Phase range Consumed .. Expired;

   type Lifetime_Kind is (Until_Evicted, Until_Deadline);
   type Lifetime (Kind : Lifetime_Kind := Until_Evicted) is record
      case Kind is
         when Until_Deadline => Deadline : Millisecond := 0;
         when Until_Evicted => null;
      end case;
   end record;

   --  Lengths of the three regions, stored in this order: type, text, CCL.
   type Layout is record
      Type_Length : Type_Name_Length := 0;
      Text_Length : Payload_Length := 0;
      CCL_Length  : Payload_Length := 0;
   end record;
   function Fits (L : Layout) return Boolean is
     (Natural (L.Type_Length) + Natural (L.Text_Length) +
        Natural (L.CCL_Length) <= Maximum_Payload);
   --  A text form is required: every entry can be pasted as text. A CCL form
   --  needs a type name; a type name without a CCL form is meaningless.
   function Well_Formed (L : Layout) return Boolean is
     (Fits (L) and then L.Text_Length > 0 and then
      (L.CCL_Length > 0) = (L.Type_Length > 0));
   function Forms (L : Layout) return Representation_Set is
     ([Text_Form => L.Text_Length > 0, CCL_Form => L.CCL_Length > 0]);

   --  Everything the manager may show about an entry: never content.
   type Metadata is record
      Name        : Entry_Name := No_Entry;
      Phase       : Entry_Phase := Vacant;
      Source      : Source_Identity := 0;
      Copied_At   : Millisecond := 0;
      Kind        : Sensitivity := Ordinary;
      Life        : Lifetime;
      Pinned      : Boolean := False;
      Shape       : Layout;
   end record;

   type Store is private
     with Default_Initial_Condition => Valid (Store) and then Empty (Store);

   --  Ghost-free invariants, also exported for the hosted test:
   function Valid (S : Store) return Boolean;
   function Info (S : Store; Slot : Slot_Index) return Metadata;
   function Wiped (S : Store; Slot : Slot_Index) return Boolean;
   function Next_Name (S : Store) return Live_Entry_Name;
   function Empty (S : Store) return Boolean;
   function Byte (S : Store; Slot : Slot_Index; Index : Payload_Index)
     return Interfaces.Unsigned_8;

   --  The entry a paste delivers: the newest Available one. 0 = none.
   function Current (S : Store) return Slot_Count
     with Pre => Valid (S),
       Post => (if Current'Result /= 0 then
                  Info (S, Current'Result).Phase = Available and then
                  (for all K in Slot_Index =>
                     (if Info (S, K).Phase = Available then
                        Info (S, K).Name <= Info (S, Current'Result).Name)))
               and then
               (if Current'Result = 0 then
                  (for all K in Slot_Index => Info (S, K).Phase /= Available));

   --  Slot of the newest entry (any non-vacant phase) older than Below;
   --  Below = No_Entry starts from the newest. 0 = none. Manager order.
   function Older (S : Store; Below : Entry_Name) return Slot_Count
     with Pre => Valid (S),
       Post => (if Older'Result /= 0 then
                  Info (S, Older'Result).Phase /= Vacant and then
                  (Below = No_Entry or else Info (S, Older'Result).Name < Below));

   function Find (S : Store; Name : Entry_Name) return Slot_Count
     with Pre => Valid (S),
       Post => (if Find'Result /= 0 then
                  Name /= No_Entry and then
                  Info (S, Find'Result).Name = Name and then
                  Info (S, Find'Result).Phase /= Vacant) and then
               (if Name /= No_Entry and then
                   (for some K in Slot_Index =>
                      Info (S, K).Phase /= Vacant and then
                      Info (S, K).Name = Name)
                then Find'Result /= 0);

   --  Sensitive entries live for Sensitive_Lifetime_Ms at most; ordinary ones
   --  until evicted or deleted.
   function Lifetime_For (Kind : Sensitivity; Now : Millisecond)
     return Lifetime is
     (case Kind is
        when Ordinary => (Kind => Until_Evicted),
        when Sensitive =>
          (Kind => Until_Deadline,
           Deadline =>
             (if Now <= Millisecond'Last - Sensitive_Lifetime_Ms
              then Now + Sensitive_Lifetime_Ms else Millisecond'Last)));

   function Expired_At (Life : Lifetime; Now : Millisecond) return Boolean is
     (Life.Kind = Until_Deadline and then Now >= Life.Deadline);

   type Offer_Outcome is (Stored, History_Full, Names_Exhausted);

   --  Store a validated offer as the newest entry. A full history evicts the
   --  oldest unpinned entry (tombstones first); all pinned is History_Full.
   procedure Offer
     (S       : in out Store;
      Source  : Source_Identity;
      Now     : Millisecond;
      Kind    : Sensitivity;
      Shape   : Layout;
      Data    : Payload_Bytes;
      Name    : out Entry_Name;
      Outcome : out Offer_Outcome)
     with Pre => Valid (S) and then Well_Formed (Shape),
       Post => Valid (S) and then
         (if Outcome = Stored then
            Name /= No_Entry and then Current (S) /= 0 and then
            Info (S, Current (S)).Name = Name and then
            Info (S, Current (S)).Kind = Kind and then
            Info (S, Current (S)).Source = Source and then
            Info (S, Current (S)).Shape = Shape and then
            Info (S, Current (S)).Life = Lifetime_For (Kind, Now) and then
            not Info (S, Current (S)).Pinned
          else Name = No_Entry);

   type Paste_Outcome is (Delivered, Nothing_Available, Format_Refused);

   --  Deliver the current entry in the requested forms (text is always
   --  acceptable). Expired entries are wiped first and never delivered. A
   --  sensitive entry, or any entry pasted into a sensitive target, is wiped
   --  in the same step: paste once.
   procedure Paste
     (S        : in out Store;
      Now      : Millisecond;
      Target   : Sensitivity;
      Accepted : Representation_Set;
      Meta     : out Metadata;
      Data     : out Payload_Bytes;
      Outcome  : out Paste_Outcome)
     with Pre => Valid (S),
       Post => Valid (S) and then
         (if Outcome = Delivered then
            Meta.Phase = Available and then
            Well_Formed (Meta.Shape) and then
            not Expired_At (Meta.Life, Now) and then
            Accepted (Text_Form) and then
            --  Paste once: the delivered secret no longer exists.
            (if Meta.Kind = Sensitive or else Target = Sensitive then
               Find (S, Meta.Name) /= 0 and then
               Info (S, Find (S, Meta.Name)).Phase = Consumed and then
               Wiped (S, Find (S, Meta.Name)))
          else Meta.Phase = Vacant)
         and then
         --  Nothing that has expired survives a paste with content.
         (for all K in Slot_Index =>
            (if Info (S, K).Phase = Available then
               not Expired_At (Info (S, K).Life, Now)));

   --  Wipe every entry whose deadline has passed (periodic housekeeping).
   procedure Expire (S : in out Store; Now : Millisecond)
     with Pre => Valid (S),
       Post => Valid (S) and then Next_Name (S) = Next_Name (S'Old) and then
         (for all K in Slot_Index =>
            (if Info (S, K).Phase = Available then
               not Expired_At (Info (S, K).Life, Now)));

   --  Earliest deadline of a live sensitive entry, or Otherwise.
   function Next_Deadline (S : Store; Otherwise : Millisecond)
     return Millisecond
     with Pre => Valid (S);

   --  Manager actions. Promote makes an entry current (new name). Sensitive
   --  entries cannot be pinned. Delete wipes; Clear wipes all unpinned.
   procedure Promote (S : in out Store; Slot : Slot_Index; Done : out Boolean)
     with Pre => Valid (S),
       Post => Valid (S) and then
         (if Done then Current (S) = Slot and then
            Info (S, Slot).Phase = Available);
   procedure Set_Pinned
     (S : in out Store; Slot : Slot_Index; Pinned : Boolean; Done : out Boolean)
     with Pre => Valid (S),
       Post => Valid (S) and then
         (if Done then Info (S, Slot).Pinned = Pinned and then
            Info (S, Slot).Kind = Ordinary);
   procedure Delete (S : in out Store; Slot : Slot_Index)
     with Pre => Valid (S),
       Post => Valid (S) and then Info (S, Slot).Phase = Vacant and then
         Wiped (S, Slot);
   procedure Clear (S : in out Store)
     with Pre => Valid (S),
       Post => Valid (S) and then
         (for all K in Slot_Index =>
            (if not Info (S, K).Pinned then
               Info (S, K).Phase = Vacant and then Wiped (S, K)));

   --  Up to Limit bytes of an ordinary entry's text form for the manager.
   --  A sensitive entry or a tombstone has no preview.
   function Preview_Length
     (S : Store; Slot : Slot_Index; Limit : Payload_Length)
     return Payload_Length
     with Pre => Valid (S),
       Post => Preview_Length'Result <= Limit and then
         Preview_Length'Result <= Info (S, Slot).Shape.Text_Length and then
         (if Info (S, Slot).Kind = Sensitive or else
             Info (S, Slot).Phase /= Available
          then Preview_Length'Result = 0);
   function Preview_Byte
     (S : Store; Slot : Slot_Index; Index : Payload_Index)
     return Interfaces.Unsigned_8
     with Pre => Valid (S) and then
       Index <= Preview_Length (S, Slot, Maximum_Payload);

private
   type Slot_Entry is record
      Meta : Metadata;
      Data : Payload_Bytes := [others => 0];
   end record;
   type Slot_Array is array (Slot_Index) of Slot_Entry;

   type Store is record
      Slots : Slot_Array;
      Next  : Live_Entry_Name := 1;
   end record;

   function All_Zero (D : Payload_Bytes) return Boolean is
     (for all I in Payload_Index => D (I) = 0);

   function Valid_Slot (E : Slot_Entry; Next : Live_Entry_Name) return Boolean
   is
     ((if E.Meta.Phase = Vacant then
         E.Meta.Name = No_Entry and then not E.Meta.Pinned
       else E.Meta.Name /= No_Entry and then E.Meta.Name < Next)
      and then
      (if E.Meta.Phase /= Available then All_Zero (E.Data))
      and then
      (if E.Meta.Phase = Available then Well_Formed (E.Meta.Shape))
      and then
      (if E.Meta.Phase in Tombstone then E.Meta.Kind = Sensitive)
      and then
      (if E.Meta.Kind = Ordinary then E.Meta.Life.Kind = Until_Evicted)
      and then
      (if E.Meta.Kind = Sensitive then
         not E.Meta.Pinned and then
         E.Meta.Life = Lifetime_For (Sensitive, E.Meta.Copied_At)));

   function Distinct (Slots : Slot_Array) return Boolean is
     (for all A in Slot_Index =>
        (for all B in Slot_Index =>
           (if A /= B and then Slots (A).Meta.Phase /= Vacant and then
               Slots (B).Meta.Phase /= Vacant
            then Slots (A).Meta.Name /= Slots (B).Meta.Name)));

   function Valid (S : Store) return Boolean is
     ((for all K in Slot_Index => Valid_Slot (S.Slots (K), S.Next))
      and then Distinct (S.Slots));

   function Info (S : Store; Slot : Slot_Index) return Metadata is
     (S.Slots (Slot).Meta);
   function Wiped (S : Store; Slot : Slot_Index) return Boolean is
     (All_Zero (S.Slots (Slot).Data));
   function Next_Name (S : Store) return Live_Entry_Name is (S.Next);
   function Empty (S : Store) return Boolean is
     (for all K in Slot_Index => S.Slots (K).Meta.Phase = Vacant);
   function Byte (S : Store; Slot : Slot_Index; Index : Payload_Index)
     return Interfaces.Unsigned_8 is (S.Slots (Slot).Data (Index));
end Clipboard_Store;
