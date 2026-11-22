with Interfaces;
with CCL.Objects;

--  Single-dispatcher typed publication. The caller authenticates worker
--  attachment/completions and authorizes client keys before entering here.
--  Session/request numbers correlate work; they never confer authority.
package Config_Objects with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   use type CCL.Objects.Image;
   subtype Number is Interfaces.Unsigned_64;
   Maximum_Revision : constant Number := 2 ** 63 - 1;
   type Phase is (Unbound, Detached, Recovering, Loading, Ready, Committing, Recovery_Required);
   type Outcome is
     (Accepted, Published, Rejected, Not_Bound, Busy, Needs_Recovery,
      Revision_Conflict, Revision_Exhausted, Invalid_Value, Invalid_Request, Ignored);
   type Commit_Outcome is (Committed, Definitely_Rejected, Conflicted, Indeterminate);
   type Load_Outcome is (Loaded, Absent, Failed);
   type Read_Result is (Found, Stale, Missing, Unavailable, Schema_Mismatch);
   type State is limited private;
   function Status (Object : State) return Phase;
   function Revision (Object : State) return Number;
   function Session (Object : State) return Number;
   function Pending_Request (Object : State) return Number;
   function Visible (Object : State) return CCL.Objects.Image with Ghost;
   function Candidate (Object : State) return CCL.Objects.Image with Ghost;
   function Valid_Candidate (Object : State) return Boolean with Ghost;
   procedure Initialize
     (Object : in out State; Contract : CCL.Objects.Binding; Accepted : out Boolean);
   --  Authorized replacement sessions increase; attachment always requires a
   --  load before writing. Tokens come from the dispatcher's non-reusing
   --  process-wide allocator. Zero and U64'Last are reserved request tokens.
   procedure Attach (Object : in out State; New_Session : Number; Result : out Outcome)
     with Post => Visible (Object) = Visible (Object)'Old and Revision (Object) = Revision (Object)'Old;
   procedure Begin_Load (Object : in out State; Request : Number; Result : out Outcome)
     with Post => Visible (Object) = Visible (Object)'Old and Revision (Object) = Revision (Object)'Old;
   procedure Finish_Load
     (Object : in out State; From_Session, Request : Number; Completion : Load_Outcome;
      Saved_Revision : Number; Value : CCL.Objects.Image; Result : out Outcome)
     with Post =>
       ((Visible (Object) = Visible (Object)'Old and
          Revision (Object) = Revision (Object)'Old) or else Result = Published) and
       (if Result = Published then Revision (Object) = Saved_Revision);
   procedure Begin_Commit
     (Object : in out State; Value : CCL.Objects.Image;
      Expected_Revision, Request : Number; Result : out Outcome)
     with Post => Visible (Object) = Visible (Object)'Old and
       Revision (Object) = Revision (Object)'Old and
       (if Result = Accepted then Valid_Candidate (Object));
   procedure Export_Pending
     (Object : State; Value : out CCL.Objects.Image; Expected_Revision : out Number;
      Available : out Boolean);
   procedure Finish_Commit
     (Object : in out State; From_Session, Request : Number; Completion : Commit_Outcome;
      Saved_Revision : Number; Result : out Outcome)
     with Post =>
       ((Visible (Object) = Visible (Object)'Old and
          Revision (Object) = Revision (Object)'Old) or else Result = Published) and
       ((Visible (Object) = Candidate (Object)'Old and
          Revision (Object) = Revision (Object)'Old + 1 and
          Completion = Committed and Saved_Revision = Revision (Object)) or else Result /= Published);
   --  Timeout/death preserves the last known value, marked stale. It does not
   --  cancel or roll back a possibly committed database operation.
   procedure Lose_Worker (Object : in out State; From_Session : Number)
     with Post => Visible (Object) = Visible (Object)'Old and Revision (Object) = Revision (Object)'Old;
   procedure Read
     (Object : State; Expected_Schema : CCL.Objects.Schema_Key;
      Value : out CCL.Objects.Image; Saved_Revision : out Number;
      Result : out Read_Result);
private
   type Buffer_Index is (First_Buffer, Second_Buffer);
   type Buffers is array (Buffer_Index) of CCL.Objects.Image;
   type State is limited record
      Contract : CCL.Objects.Binding;
      Data : Buffers;
      Active : Buffer_Index := First_Buffer;
      Current_Revision : Number range 0 .. Maximum_Revision := 0;
      Current_Phase : Phase := Unbound;
      Worker_Session, Last_Request, Active_Request : Number := 0;
   end record;
end Config_Objects;
