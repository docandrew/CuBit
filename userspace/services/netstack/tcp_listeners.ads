with Interfaces; use Interfaces;
with TCP_Slots;

--  Bounded listener lifetime/ownership model. The caller must perform policy
--  admission before Bind. No packet IO, grants, or privilege decisions here.
--
--  Each connection records the listener it waits on, so a connection is in
--  at most one backlog by construction. Proved (make -C kernel
--  prove-tcp-session, level 1), under Valid, which every operation keeps:
--  every waiting connection's listener is open, so closing a listener
--  leaves none of its connections behind; listener handles are distinct and
--  never reused (a new one is above every earlier one), so a closed handle
--  never names a later listener; Reserve takes only a connection waiting
--  nowhere; Accept_Ready hands out only a ready connection of the caller's
--  own listener and removes it; Close, Close_Owned, Expire and Remove leave
--  no reported connection waiting; and no listener ever has more than
--  Maximum_Backlog connections waiting (its count Held always equals the
--  connections waiting on it).
package TCP_Listeners with SPARK_Mode is
   --  STOPGAP until netstack's startup limits: listeners system-wide, and
   --  connections each may hold before its owner takes them.
   Maximum_Listeners : constant := 16;
   Maximum_Backlog : constant := 16;
   subtype Listener_Index is Positive range 1 .. Maximum_Listeners;
   subtype Connection_Index is Natural range 0 .. TCP_Slots.MAX_TCP_CONNS - 1;
   subtype Owner_Id is Unsigned_64 range 1 .. Unsigned_64'Last;
   subtype Handle is Unsigned_64;
   No_Handle : constant Handle := 0;
   type Bind_Status is (Bound, Invalid_Address, Address_In_Use, Table_Full, Handles_Exhausted);
   type Connection_List is array (Connection_Index) of Boolean;
   type Table is private;

   function Valid (Item : Table) return Boolean with Ghost;
   --  Connection waits in some listener's backlog (handshaking or ready).
   function Pending (Item : Table; Connection : Connection_Index) return Boolean;
   function Is_Ready (Item : Table; Connection : Connection_Index) return Boolean;
   --  The handle of the listener Connection waits on (No_Handle if none).
   function Parent_Id (Item : Table; Connection : Connection_Index) return Handle;
   function Owned (Item : Table; Owner : Owner_Id; Listener : Handle) return Boolean;
   --  Owner holds no listener.
   function Holds_None (Item : Table; Owner : Owner_Id) return Boolean;

   procedure Initialize (Item : out Table)
     with Post => Valid (Item);
   procedure Bind
     (Item : in out Table; Owner : Owner_Id; Address : Unsigned_32;
      Port : Unsigned_16; Listener : out Handle; Status : out Bind_Status)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then
                  (if Status = Bound then
                     Listener /= No_Handle and then Owned (Item, Owner, Listener) and then
                     --  fresh: above every handle given out before
                     not Owned (Item'Old, Owner, Listener)
                   else Listener = No_Handle);
   function Find (Item : Table; Address : Unsigned_32; Port : Unsigned_16) return Handle;
   --  SYN and ready children share a bounded budget; none is exposed
   --  before Mark_Ready.
   procedure Reserve
     (Item : in out Table; Listener : Handle; Connection : Connection_Index;
      Deadline : Unsigned_64; Success : out Boolean)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then
                  (if Success then
                     not Pending (Item'Old, Connection) and then
                     Pending (Item, Connection) and then not Is_Ready (Item, Connection) and then
                     Parent_Id (Item, Connection) = Listener and then Listener /= No_Handle
                   else Pending (Item, Connection) = Pending (Item'Old, Connection));
   procedure Mark_Ready (Item : in out Table; Connection : Connection_Index)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then
                  (for all C in Connection_Index =>
                     Pending (Item, C) = Pending (Item'Old, C) and then
                     Parent_Id (Item, C) = Parent_Id (Item'Old, C));
   --  Owner's listener has an established connection waiting.
   function Has_Ready (Item : Table; Owner : Owner_Id; Listener : Handle) return Boolean
     with Post => (if Has_Ready'Result then Owned (Item, Owner, Listener));
   procedure Accept_Ready
     (Item : in out Table; Owner : Owner_Id; Listener : Handle;
      Connection : out Connection_Index; Found : out Boolean)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then
                  (if Found then
                     Owned (Item'Old, Owner, Listener) and then
                     Is_Ready (Item'Old, Connection) and then
                     Parent_Id (Item'Old, Connection) = Listener and then
                     not Pending (Item, Connection));
   procedure Remove (Item : in out Table; Connection : Connection_Index)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then not Pending (Item, Connection);
   procedure Close
     (Item : in out Table; Owner : Owner_Id; Listener : Handle;
      Children : out Connection_List; Success : out Boolean)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then
                  (if Success then
                     Owned (Item'Old, Owner, Listener) and then
                     not Owned (Item, Owner, Listener) and then
                     (for all C in Connection_Index =>
                        Children (C) =
                          (Pending (Item'Old, C) and then Parent_Id (Item'Old, C) = Listener)
                        and then (if Children (C) then not Pending (Item, C)))
                   else Children = [Connection_Index => False]);
   --  Close every listener Owner holds, returning their children.
   procedure Close_Owned
     (Item : in out Table; Owner : Owner_Id; Children : out Connection_List)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then
                  Holds_None (Item, Owner) and then
                  (for all C in Connection_Index =>
                     (if Children (C) then not Pending (Item, C)));
   procedure Expire
     (Item : in out Table; Now : Unsigned_64; Children : out Connection_List)
     with Pre  => Valid (Item),
          Post => Valid (Item) and then
                  (for all C in Connection_Index =>
                     (if Children (C) then Pending (Item'Old, C) and then not Pending (Item, C)));
   function Next_Deadline (Item : Table) return Unsigned_64;
private
   type Child_State is (Vacant, Handshaking, Ready);
   --  The connection's place in a backlog, if any.
   type Child is record
      State : Child_State := Vacant;
      Parent : Listener_Index := Listener_Index'First;
      Deadline : Unsigned_64 := 0;
   end record;
   type Child_Array is array (Connection_Index) of Child;
   subtype Backlog_Count is Natural range 0 .. Maximum_Backlog;
   type Listener_Record is record
      Id : Handle := No_Handle;
      Owner : Owner_Id := 1;
      Address : Unsigned_32 := 0;
      Port : Unsigned_16 := 0;
      Held : Backlog_Count := 0;   --  connections waiting on it
   end record;
   type Listener_Array is array (Listener_Index) of Listener_Record;
   type Table is record
      Entries : Listener_Array := [others => <>];
      Children : Child_Array := [others => <>];
      Next_Id : Handle := 1;
   end record;

   --  Whether Children (C) waits on listener L, as 0 or 1.
   function Waits (Children : Child_Array; C : Connection_Index; L : Listener_Index)
     return Natural is
     (if Children (C).State /= Vacant and then Children (C).Parent = L then 1 else 0)
   with Ghost;

   --  The connections among the first N that wait on listener L.
   function Waiting_Upto
     (Children : Child_Array; L : Listener_Index; N : Natural) return Natural is
     (if N = 0 then 0
      else Waiting_Upto (Children, L, N - 1) + Waits (Children, N - 1, L))
   with Ghost,
        Pre => N <= Connection_Index'Last + 1,
        Post => Waiting_Upto'Result <= N,
        Subprogram_Variant => (Decreases => N);

   function Waiting (Children : Child_Array; L : Listener_Index) return Natural is
     (Waiting_Upto (Children, L, Connection_Index'Last + 1))
   with Ghost;

   function Valid (Item : Table) return Boolean is
     (Item.Next_Id /= No_Handle and then
      (for all L in Listener_Index =>
         Item.Entries (L).Id < Item.Next_Id) and then
      (for all L1 in Listener_Index =>
         (for all L2 in Listener_Index =>
            (if L1 /= L2 and then Item.Entries (L1).Id /= No_Handle then
               Item.Entries (L1).Id /= Item.Entries (L2).Id))) and then
      (for all C in Connection_Index =>
         (if Item.Children (C).State /= Vacant then
            Item.Entries (Item.Children (C).Parent).Id /= No_Handle)) and then
      (for all L in Listener_Index =>
         Item.Entries (L).Held = Waiting (Item.Children, L)));

   function Pending (Item : Table; Connection : Connection_Index) return Boolean is
     (Item.Children (Connection).State /= Vacant);
   function Is_Ready (Item : Table; Connection : Connection_Index) return Boolean is
     (Item.Children (Connection).State = Ready);
   function Parent_Id (Item : Table; Connection : Connection_Index) return Handle is
     (if Item.Children (Connection).State = Vacant then No_Handle
      else Item.Entries (Item.Children (Connection).Parent).Id);
   function Owned (Item : Table; Owner : Owner_Id; Listener : Handle) return Boolean is
     (Listener /= No_Handle and then
      (for some L in Listener_Index =>
         Item.Entries (L).Id = Listener and then Item.Entries (L).Owner = Owner));
   function Holds_None (Item : Table; Owner : Owner_Id) return Boolean is
     (for all L in Listener_Index =>
        Item.Entries (L).Id = No_Handle or else Item.Entries (L).Owner /= Owner);
end TCP_Listeners;
