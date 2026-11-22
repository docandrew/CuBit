with Interfaces; use Interfaces;
with Intel_GPU_Context_Table;
-- Test adapter: storage for every record of a hosted context table, and the
-- tests' habit of opening contexts in ID order with the table choosing the ID.
generic
   with package Pool is new Intel_GPU_Context_Table (<>);
package Context_Table_Access is
   procedure Prepare (T : in out Pool.Table);
   -- The lowest unused ID (preparing T first when needed).
   procedure Open
     (T : in out Pool.Table; GPU_Start, Pin_Bias : Unsigned_64;
      Quantum_Us, Preemption_Us : Unsigned_32; Preempt_To_Idle : Boolean;
      ID : out Unsigned_32; Accepted : out Boolean; Session : Unsigned_64 := 0);
   function Count (T : Pool.Table) return Natural;
   -- The context opened for Session, whatever its state; No_Context if none.
   function Context_Of (T : Pool.Table; Session : Unsigned_64) return Unsigned_32;
   function Session_Context (T : Pool.Table; Session : Unsigned_64) return Unsigned_32;
   procedure Retire_Session (T : in out Pool.Table; Session : Unsigned_64; ID : out Unsigned_32);
end Context_Table_Access;
