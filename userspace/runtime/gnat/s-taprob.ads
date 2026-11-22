------------------------------------------------------------------------------
--  CuBit userspace runtime: protected objects without entries.
--
--  With one thread per process there is no contention, so Lock only records
--  ownership. Locking an object that is already locked can only be a nested
--  protected call on the same object (a bounded error, RM 9.5.1), and is
--  treated as a program error instead of deadlocking. If CuBit processes gain
--  threads, this must become a real ceiling-priority lock.
------------------------------------------------------------------------------
package System.Tasking.Protected_Objects is
   pragma Preelaborate;

   type Protection is limited private;
   type Protection_Access is access all Protection;
   Null_PO : constant Protection_Access := null;

   function Get_Ceiling
     (Object : Protection_Access) return System.Any_Priority;

   procedure Initialize_Protection
     (Object           : Protection_Access;
      Ceiling_Priority : Integer);

   procedure Lock (Object : Protection_Access);
   procedure Lock_Read_Only (Object : Protection_Access);
   procedure Set_Ceiling
     (Object : Protection_Access;
      Prio   : System.Any_Priority);
   procedure Unlock (Object : Protection_Access);

private
   type Protection is record
      Ceiling : System.Any_Priority := System.Any_Priority'Last;
      Locked : Boolean := False;
   end record;
end System.Tasking.Protected_Objects;
