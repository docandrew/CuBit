with Interfaces; use Interfaces;
with TCPSession;

--  Bounded listener lifetime/ownership model. The caller must perform policy
--  admission before Bind. No packet IO, grants, or privilege decisions here.
package TCP_Listeners with SPARK_Mode is
   Maximum_Listeners : constant := 4;
   Maximum_Backlog : constant := 2;
   subtype Listener_Index is Positive range 1 .. Maximum_Listeners;
   subtype Connection_Index is Natural range 0 .. TCPSession.MAX_TCP_CONNS - 1;
   subtype Owner_Id is Unsigned_64 range 1 .. Unsigned_64'Last;
   subtype Handle is Unsigned_64;
   No_Handle : constant Handle := 0;
   type Bind_Status is (Bound, Invalid_Address, Address_In_Use, Table_Full, Handles_Exhausted);
   type Connection_List is array (Connection_Index) of Boolean;
   type Table is private;

   procedure Initialize (Item : out Table);
   procedure Bind
     (Item : in out Table; Owner : Owner_Id; Address : Unsigned_32;
      Port : Unsigned_16; Listener : out Handle; Status : out Bind_Status);
   function Find (Item : Table; Address : Unsigned_32; Port : Unsigned_16) return Handle;
   function Owned (Item : Table; Owner : Owner_Id; Listener : Handle) return Boolean;
   --  A connection appears in at most one listener backlog. SYN and ready
   --  children share a bounded budget; none is exposed before Mark_Ready.
   procedure Reserve
     (Item : in out Table; Listener : Handle; Connection : Connection_Index;
      Deadline : Unsigned_64; Success : out Boolean);
   procedure Mark_Ready (Item : in out Table; Connection : Connection_Index);
   procedure Accept_Ready
     (Item : in out Table; Owner : Owner_Id; Listener : Handle;
      Connection : out Connection_Index; Found : out Boolean);
   procedure Remove (Item : in out Table; Connection : Connection_Index);
   procedure Close
     (Item : in out Table; Owner : Owner_Id; Listener : Handle;
      Children : out Connection_List; Success : out Boolean);
   procedure Expire
     (Item : in out Table; Now : Unsigned_64; Children : out Connection_List);
   function Next_Deadline (Item : Table) return Unsigned_64;
private
   type Child_State is (Vacant, Handshaking, Ready);
   type Child is record
      State : Child_State := Vacant;
      Connection : Connection_Index := 0;
      Deadline : Unsigned_64 := 0;
   end record;
   type Backlog is array (Positive range 1 .. Maximum_Backlog) of Child;
   type Listener_Record is record
      Id : Handle := No_Handle;
      Owner : Owner_Id := 1;
      Address : Unsigned_32 := 0;
      Port : Unsigned_16 := 0;
      Pending : Backlog := [others => <>];
   end record;
   type Listener_Array is array (Listener_Index) of Listener_Record;
   type Table is record
      Entries : Listener_Array := [others => <>];
      Next_Id : Handle := 1;
   end record;
end TCP_Listeners;
