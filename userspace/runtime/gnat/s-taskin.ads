------------------------------------------------------------------------------
--  CuBit userspace runtime: minimal System.Tasking.
--
--  CuBit processes are single-threaded (pragma Restrictions (No_Tasking)).
--  This declares only what protected objects without entries need, so that
--  thread-safe libraries (for example SPARKTLS.RBG) can be used unchanged.
------------------------------------------------------------------------------
package System.Tasking is
   pragma Preelaborate;

   Null_Entry : constant := 0;
   Max_Entry : constant := Integer'Last;
   type Entry_Index is range Integer'First .. Max_Entry;

   --  Ceiling used when a protected type has no Priority aspect.
   Unspecified_Priority : constant Integer := System.Priority'First - 1;
end System.Tasking;
