with Interfaces; use Interfaces;

--  Request identity within one process generation. Zero is the synchronous
--  sentinel, never an asynchronous request ID. Exhaustion never wraps.
package IPC_Request_Ids with SPARK_Mode, Pure is
   type Sequence is new Unsigned_64;
   Initial_Sequence : constant Sequence := 0;
   subtype Identifier is Unsigned_64 range 1 .. Unsigned_64'Last;
   type Allocation (Available : Boolean := False) is record
      case Available is
         when True => Id : Identifier;
         when False => null;
      end case;
   end record;

   --  The caller commits Id as its new sequence while holding the same lock
   --  that protects request publication. Returning a value cannot overwrite
   --  a caller's constrained variant record through an out parameter.
   function Next (Last_Issued : Sequence) return Allocation
     with Post =>
       (if Last_Issued < Sequence'Last then
           Next'Result.Available and then
           Next'Result.Id = Unsigned_64 (Last_Issued) + 1
        else not Next'Result.Available);
end IPC_Request_Ids;
