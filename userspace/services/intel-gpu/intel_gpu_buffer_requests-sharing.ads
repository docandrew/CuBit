with CuBit.Messages;
with Intel_GPU_Buffer_Views;
generic
   -- Trusted admission lookup, never request words. Must hold the selected
   -- endpoint slot stable through Share (including supervisor edits).
   with procedure Recipient_Of
     (Sender, Stamp : Unsigned_64; Slot : out CuBit.Messages.CapabilitySlot;
      Identity : out Unsigned_64);
package Intel_GPU_Buffer_Requests.Sharing is
   Capacity : constant := 64;
   subtype Mapping_ID is Unsigned_32;
   type Mapping_Table is limited private;
   Map_Label : constant Unsigned_32 := 16#0A23#;
   Map_Read : constant Unsigned_64 := 0;
   Map_Write : constant Unsigned_64 := 1;
   Retire_Map : constant Unsigned_64 := 2;
   Map_Presentation : constant Unsigned_64 := 3;
   -- Read-only owner-opted-in forwarding, never implicit in Map_Read/Write.
   Pending_Retirement : constant Unsigned_64 := 4;
   -- Request [version | operation<<32, BO-handle, offset, bytes]; retirement
   -- uses [version | 2<<32, mapping-ID, 0, 0]. Success response:
   -- [OK, version, mapping-ID, grant-reference] for map, zero trailing words
   -- for retirement. Other statuses also have zero trailing words; status4
   -- means repeat RETIRE later, not repeat MAP. Map is never blindly retried.
   -- A nonzero Created is a dispatcher cleanup ticket: if reply delivery
   -- fails, call Reject_Delivery even though the app never saw the mapping ID.
   procedure Handle
     (Object : Service; Table : in out Mapping_Table;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words; Created : out Mapping_ID);
   -- One table per driver incarnation. IDs never repeat or wrap. Confirmed
   -- retired view slots may be recycled under a fresh ID; uncertain/live views
   -- remain retained. Exhaustion fails without granting more.
   procedure Map
     (Object : Service; Table : in out Mapping_Table;
      Sender, Stamp, ID, Offset, Bytes : Unsigned_64; Writable : Boolean;
      Mapping : out Mapping_ID; Reference : out Unsigned_64;
      Presentation : Boolean := False);
   -- Authenticated client retirement/poll. Accepted=False for foreign/unknown
   -- IDs. Retired means this grant is gone, NOT that GPU work/backing is free.
   procedure Retire
     (Table : in out Mapping_Table; Sender, Stamp : Unsigned_64;
      Mapping : Mapping_ID; Accepted : out Boolean;
      State : out Intel_GPU_Buffer_Views.View_State);
   -- Trusted dispatcher lifecycle hooks, never application-supplied IDs.
   procedure Reject_Delivery (Table : in out Mapping_Table; Mapping : Mapping_ID);
   -- Close admission first so no subsequent request resolves this session.
   -- This hook drains its grants; it does not edit the admission controller.
   procedure Retire_Session (Table : in out Mapping_Table; Session : Unsigned_64);
   procedure Quarantine (Table : in out Mapping_Table);
   procedure Poll (Table : in out Mapping_Table);
   -- Dispatcher-owned view; caller supplies only its BO handle and range.
   -- Authenticate against the same registry that created the handle. No
   -- backing address or mutable handle registry escapes this interface.
   -- Serialized with allocation completion, close and session retirement.
   -- The dispatcher must retire a successful view if reply delivery fails,
   -- and track all live views through session teardown before reclaiming RAM.
   procedure Share
     (Object : Service; Sender, Stamp, ID, Offset, Bytes : Unsigned_64;
      Writable : Boolean; View : in out Intel_GPU_Buffer_Views.View;
      Accepted : out Boolean; Presentation : Boolean := False);
private
   type Mapping_Entry is limited record
      ID : Mapping_ID := 0;
      Session : Unsigned_64 := 0;
      View : Intel_GPU_Buffer_Views.View;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Mapping_Entry;
   type Mapping_Table is limited record
      Used : Natural range 0 .. Capacity := 0;
      Last_ID : Mapping_ID := 0;
      Failed : Boolean := False;
      Items : Entries;
   end record;
end Intel_GPU_Buffer_Requests.Sharing;
