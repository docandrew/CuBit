with Intel_GPU_Waiter_Ring;

-- Buffer allocations that wait for the allocator, first come first served.
--
-- The buffer service allocates one buffer at a time (one backing request
-- to devmgr in flight, one deferred reply). A Create that arrived while
-- another allocation was in flight used to be answered Unavailable at once
-- (Allocation_Outcome Application_Pending). Mesa reports that as a failed
-- allocation; on the IdeaPad (laptop image 11) a second Vulkan client
-- starting while desktop.svc allocated made desktop.svc's renderer give up
-- and exit. A request is never refused for a transient internal state
-- (no-lost-events), so such a Create now waits here with its saved reply
-- and is dispatched again, oldest first, once the allocator is idle.
--
-- Storage is on the heap and grows by doubling from Initial_Size up to
-- Ceiling, the named limit; at the ceiling (or when the heap refuses) the
-- caller answers the request itself. Serialized by the driver's loop.
generic
   -- What is dispatched again: the sender and its request.
   type Request is private;
   -- A saved reply. Exactly one owner holds it at a time.
   type Reply is limited private;
   with function Held (Item : Reply) return Boolean;
   -- Move the reply From holds into Into, which is empty; From is empty
   -- afterwards. Never copies a reply.
   with procedure Move (From, Into : in out Reply);
   Initial_Size : Intel_GPU_Waiter_Ring.Ring_Size;
   Ceiling : Intel_GPU_Waiter_Ring.Ring_Size;
package Intel_GPU_Allocation_Waiters with SPARK_Mode => Off is
   pragma Compile_Time_Error (Initial_Size > Ceiling, "initial size above the ceiling");
   subtype Waiter_Count is Intel_GPU_Waiter_Ring.Ring_Count;

   type Queue is limited private;
   function Length (Q : Queue) return Waiter_Count;
   -- Most waiters held at once so far (diagnostics).
   function Peak (Q : Queue) return Waiter_Count;

   -- The dispatch rule, shared by the driver and its tests. A Create waits
   -- while the allocator is busy, or while older waiters exist and it is
   -- not the oldest being dispatched again (keeps arrival order).
   function Must_Wait (Q : Queue; Allocator_Busy, Dispatching_Oldest : Boolean) return Boolean is
     (Allocator_Busy or else (Length (Q) > 0 and then not Dispatching_Oldest));
   -- The oldest waiter can be dispatched now.
   function Ready (Q : Queue; Allocator_Busy : Boolean) return Boolean is
     (Length (Q) > 0 and then not Allocator_Busy);

   type Wait_Result is
     (Waiting,       -- Owed moved into the queue.
      At_Ceiling,    -- Ceiling waiters already wait; Owed is untouched.
      No_Memory);    -- The heap refused to grow the queue; Owed is untouched.

   -- Wait behind every current waiter.
   procedure Append (Q : in out Queue; Item : Request; Owed : in out Reply;
                     Result : out Wait_Result)
     with Pre => Held (Owed),
          Post => Length (Q) = Length (Q)'Old + (if Result = Waiting then 1 else 0) and then
                  Held (Owed) = (Result /= Waiting);
   -- Wait again as the oldest: a waiter that was taken but found the
   -- allocator busy keeps its place at the head.
   procedure Restore (Q : in out Queue; Item : Request; Owed : in out Reply;
                      Result : out Wait_Result)
     with Pre => Held (Owed),
          Post => Length (Q) = Length (Q)'Old + (if Result = Waiting then 1 else 0) and then
                  Held (Owed) = (Result /= Waiting);
   -- Remove the oldest waiter; its reply moves into Owed.
   procedure Take (Q : in out Queue; Item : out Request; Owed : in out Reply;
                   Found : out Boolean)
     with Pre => not Held (Owed),
          Post => Found = (Length (Q)'Old > 0) and Held (Owed) = Found and
                  Length (Q) + (if Found then 1 else 0) = Length (Q)'Old;
private
   type Waiter is limited record
      Item : Request;
      Owed : Reply;
   end record;
   type Waiter_Array is array (Intel_GPU_Waiter_Ring.Ring_Slot range <>) of Waiter;
   type Waiter_Storage is access Waiter_Array;
   type Queue is limited record
      Slots : Waiter_Storage := null;
      Head : Intel_GPU_Waiter_Ring.Ring_Slot := 0;
      Count : Waiter_Count := 0;
      Most : Waiter_Count := 0;
   end record;
   function Length (Q : Queue) return Waiter_Count is (Q.Count);
   function Peak (Q : Queue) return Waiter_Count is (Q.Most);
end Intel_GPU_Allocation_Waiters;
