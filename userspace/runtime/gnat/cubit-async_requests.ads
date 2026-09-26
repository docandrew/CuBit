with Interfaces;

--  Single-owner lifecycle for one outstanding asynchronous request.
--  One tracker per slot in a client's bounded request table. No CCL, service,
--  allocator, payload, polling or syscall dependency.
package CuBit.Async_Requests with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   subtype Token is Interfaces.Unsigned_64;
   No_Token : constant Token := 0;
   type Phase is (Idle, Reserved, In_Flight, Completion_Ready);
   type Tracker is private;
   function State (Item : Tracker) return Phase;
   function Last_Token (Item : Tracker) return Token;
   function Pending_Token (Item : Tracker) return Token;
   function Detached (Item : Tracker) return Boolean;
   function Drained (Item : Tracker) return Boolean is (State (Item) = Idle);
   function Can_Resume (Item : Tracker) return Boolean is
     (State (Item) = Completion_Ready and not Detached (Item));
   function Can_Reserve (Item : Tracker; ID : Token) return Boolean is
     (State (Item) = Idle and not Detached (Item) and
      ID > Last_Token (Item) and ID < Token'Last);

   procedure Reserve
     (Item : in out Tracker; ID : Token; Accepted : out Boolean)
     with Global => null, Post =>
       (if Accepted then Can_Reserve (Item'Old, ID) and
          State (Item) = Reserved and Last_Token (Item) = ID and
          Pending_Token (Item) = ID and not Detached (Item)
        else not Can_Reserve (Item'Old, ID) and Item = Item'Old);
   --  Reserve before submission; even queue rejection burns ID.
   --  IDs must also be unique across dispatcher slots/replacement clients.
   --  A token is correlation data, NEVER proof of authority.

   procedure Submitted (Item : in out Tracker; Accepted : Boolean)
     with Global => null, Pre => State (Item) = Reserved,
       Post => State (Item) = (if Accepted then In_Flight else Idle) and
         Pending_Token (Item) =
           (if Accepted then Pending_Token (Item'Old) else No_Token) and
         Last_Token (Item) = Last_Token (Item'Old) and
         Detached (Item) = Detached (Item'Old);

   procedure Capture
     (Item : in out Tracker; ID : Token; Valid : Boolean;
      Accepted : out Boolean)
     with Global => null, Post =>
       (if Accepted then
          State (Item'Old) = In_Flight and Valid and
          ID = Pending_Token (Item'Old) and
          State (Item) = Completion_Ready and
          Pending_Token (Item) = Pending_Token (Item'Old) and
          Last_Token (Item) = Last_Token (Item'Old) and
          Detached (Item) = Detached (Item'Old)
        else (State (Item'Old) /= In_Flight or not Valid or
          ID /= Pending_Token (Item'Old)) and Item = Item'Old);
   --  Caller supplies a kernel-authenticated receipt. Capture correlates only;
   --  service status, transport failure and payload validation belong to the
   --  adapter. An error receipt still needs capture/drain exactly once.

   procedure Release (Item : in out Tracker)
     with Global => null, Pre => State (Item) = Completion_Ready,
       Post => Drained (Item) and Pending_Token (Item) = No_Token and
         Last_Token (Item) = Last_Token (Item'Old) and
         Detached (Item) = Detached (Item'Old);
   --  After consuming or draining the receipt. This establishes neither remote
   --  handle closure nor shared-grant retirement; adapters must check those.

   procedure Stop (Item : in out Tracker)
     with Global => null, Post => Detached (Item) and
       State (Item) = State (Item'Old) and
       Pending_Token (Item) = Pending_Token (Item'Old) and
       Last_Token (Item) = Last_Token (Item'Old);
   --  Irreversible detachment: disallows new work/resumption but still accepts
   --  the outstanding receipt for draining. It requests no cancellation, frees
   --  no memory and claims no rollback. Serialize with submission/dispatch.
private
   type Tracker is record
      Current : Phase := Idle;
      Last, Pending : Token := No_Token;
      Stopped : Boolean := False;
   end record;
   function State (Item : Tracker) return Phase is (Item.Current);
   function Last_Token (Item : Tracker) return Token is (Item.Last);
   function Pending_Token (Item : Tracker) return Token is (Item.Pending);
   function Detached (Item : Tracker) return Boolean is (Item.Stopped);
end CuBit.Async_Requests;
