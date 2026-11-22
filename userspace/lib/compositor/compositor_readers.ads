-- Ordered retirement of an output lease and its immutable target grants.
-- Callback confirmations are trusted external evidence, not inferred from
-- revocation acceptance or elapsed time. No storage is released here.
generic
   Maximum_Targets : Positive;
   with procedure Release_Lease (Confirmed : out Boolean);
   with procedure Retire_Grant (Index : Positive; Confirmed : out Boolean);
package Compositor_Readers with SPARK_Mode is
   subtype Target_Index is Positive range 1 .. Maximum_Targets;
   type Grant_Set is array (Target_Index) of Boolean;
   type State is private;
   function Lease_Pending (S : State) return Boolean;
   function Grant_Pending (S : State; Index : Target_Index) return Boolean;
   function Uncertain (S : State) return Boolean;
   function Clear (S : State) return Boolean;
   function Open (Leased : Boolean; Grants : Grant_Set) return State
     with Post => Lease_Pending (Open'Result) = Leased and
       not Uncertain (Open'Result) and
       (for all I in Target_Index => Grant_Pending (Open'Result, I) = Grants (I));
   -- One bounded attempt. A failed confirmation quarantines this state;
   -- completed predecessors stay retired and must not be attempted again.
   procedure Retire (S : in out State)
     with Pre => not Uncertain (S),
       Post => (if not Uncertain (S) then Clear (S)) and
         (if not Lease_Pending (S'Old) then not Lease_Pending (S)) and
         (for all I in Target_Index =>
            (if not Grant_Pending (S'Old, I) then not Grant_Pending (S, I)));
private
   type State is record
      Lease : Boolean := False;
      Grants : Grant_Set := (others => False);
      Failed : Boolean := False;
   end record;
   function Lease_Pending (S : State) return Boolean is (S.Lease);
   function Grant_Pending (S : State; Index : Target_Index) return Boolean is
     (S.Grants (Index));
   function Uncertain (S : State) return Boolean is (S.Failed);
   function Clear (S : State) return Boolean is
     (not S.Failed and not S.Lease and (for all I in Target_Index => not S.Grants (I)));
   function Open (Leased : Boolean; Grants : Grant_Set) return State is
     ((Leased, Grants, False));
end Compositor_Readers;
