with Config;

-- Sequential ownership policy. Spinlocks commits these transitions with a
-- hardware compare/exchange; this package does not model concurrent memory.
package Locks with SPARK_Mode => On is
    subtype CPU_ID is Natural range 0 .. Config.MAX_SMP_CPUS - 1;
    type State is private;
    Unowned : constant State;
    type Acquire_Result is (Acquired, Contended, Reentrant);

    function Is_Locked (S : State) return Boolean;
    function Owned_By (S : State; CPU : CPU_ID) return Boolean;

    procedure Acquire (S : in out State; CPU : CPU_ID;
                       Result : out Acquire_Result)
      with Inline_Always, Global => null,
      Post =>
        (if not Is_Locked (S'Old) then
             Result = Acquired and Owned_By (S, CPU)
         elsif Owned_By (S'Old, CPU) then
             Result = Reentrant and S = S'Old
         else Result = Contended and S = S'Old);

    procedure Release (S : in out State; CPU : CPU_ID; Success : out Boolean)
      with Inline_Always, Global => null,
      Post => (Success = Owned_By (S'Old, CPU)) and
        (if Success then S = Unowned else S = S'Old);

    procedure Prove_Exclusive_Owner (Owner, Other : CPU_ID)
      with Ghost, Global => null, Pre => Owner /= Other;

private
    -- One word carries both availability and ownership: no split observation
    -- of a lock flag and separately published CPU metadata.
    type State is range -1 .. Config.MAX_SMP_CPUS - 1 with Size => 32;
    Unowned : constant State := -1;
    function Is_Locked (S : State) return Boolean is (S /= Unowned);
    function Owned_By (S : State; CPU : CPU_ID) return Boolean is
      (S = State (CPU));
end Locks;
