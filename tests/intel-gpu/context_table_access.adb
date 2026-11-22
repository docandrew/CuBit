with Test_Storage;
package body Context_Table_Access is
   procedure Prepare (T : in out Pool.Table) is
      Grown : Boolean;
   begin
      if Pool.Grown (T) /= 0 then return; end if;
      Pool.Attach (T, Test_Storage.Allocate (Pool.Storage_Bytes (Pool.Context_Count'Last)));
      Pool.Grow (T, Pool.Context_Count'Last, Grown);
      pragma Assert (Grown);
   end Prepare;
   procedure Open
     (T : in out Pool.Table; GPU_Start, Pin_Bias : Unsigned_64;
      Quantum_Us, Preemption_Us : Unsigned_32; Preempt_To_Idle : Boolean;
      ID : out Unsigned_32; Accepted : out Boolean; Session : Unsigned_64 := 0) is
      First : constant Unsigned_32 := Pool.Last_ID (T) + 1 - Unsigned_32 (Pool.Grown (T));
   begin
      Prepare (T);
      ID := Pool.No_Context; Accepted := False;
      for Candidate in First .. Pool.Last_ID (T) loop
         if not Pool.In_Use (T, Candidate) then
            Pool.Open (T, Candidate, GPU_Start, Pin_Bias, Quantum_Us, Preemption_Us,
                       Preempt_To_Idle, Accepted, Session);
            if Pool.In_Use (T, Candidate) then ID := Candidate; end if;
            return;
         end if;
      end loop;
   end Open;
   function First_ID (T : Pool.Table) return Unsigned_32 is
     (Pool.Last_ID (T) + 1 - Unsigned_32 (Pool.Grown (T)));
   function Count (T : Pool.Table) return Natural is
      N : Natural := 0;
   begin
      for ID in First_ID (T) .. Pool.Last_ID (T) loop
         if Pool.In_Use (T, ID) then N := N + 1; end if;
      end loop;
      return N;
   end Count;
   function Context_Of (T : Pool.Table; Session : Unsigned_64) return Unsigned_32 is
   begin
      if Session /= 0 then
         for ID in First_ID (T) .. Pool.Last_ID (T) loop
            if Pool.Session_Of (T, ID) = Session then return ID; end if;
         end loop;
      end if;
      return Pool.No_Context;
   end Context_Of;
   function Session_Context (T : Pool.Table; Session : Unsigned_64) return Unsigned_32 is
     (if Context_Of (T, Session) = Pool.No_Context then Pool.No_Context
      else Pool.Session_Context (T, Context_Of (T, Session), Session));
   procedure Retire_Session (T : in out Pool.Table; Session : Unsigned_64; ID : out Unsigned_32) is
   begin
      ID := Context_Of (T, Session);
      if ID /= Pool.No_Context then Pool.Retire_Session (T, ID, Session); end if;
   end Retire_Session;
end Context_Table_Access;
