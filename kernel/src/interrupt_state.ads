-- Interrupt exclusion bookkeeping. Hardware IF, GS and lock ownership are
-- observed by PerCPUData's trusted adapter; this package has no hardware access.
package Interrupt_State with SPARK_Mode => On is
    type State is private;
    type Context is private;
    Initial_State : constant State;
    Initial_Context : constant Context;

    type Result is (Success, Interrupts_Enabled_While_Nested,
                    Nesting_Exhausted, No_Critical_Section);
    function Depth (S : State) return Natural;
    function Restores_Interrupts (S : State) return Boolean;
    function Restores_Interrupts (C : Context) return Boolean;

    procedure Enter (S : in out State; Hardware_IF : Boolean; Status : out Result)
      with Global => null,
      Post =>
        (if Depth (S'Old) > 0 and Hardware_IF then
            Status = Interrupts_Enabled_While_Nested and S = S'Old
         elsif Depth (S'Old) = Natural'Last then
            Status = Nesting_Exhausted and S = S'Old
         else
            Status = Success and Depth (S) = Depth (S'Old) + 1 and
            Restores_Interrupts (S) =
              (if Depth (S'Old) = 0 then Hardware_IF
               else Restores_Interrupts (S'Old)));

    procedure Leave (S : in out State; Hardware_IF : Boolean;
                     Enable_Interrupts : out Boolean; Status : out Result)
      with Global => null,
      Post =>
        (if Hardware_IF then
            Status = Interrupts_Enabled_While_Nested and S = S'Old and
            not Enable_Interrupts
         elsif Depth (S'Old) = 0 then
            Status = No_Critical_Section and S = S'Old and
            not Enable_Interrupts
         else
            Status = Success and Depth (S) = Depth (S'Old) - 1 and
            Restores_Interrupts (S) = Restores_Interrupts (S'Old) and
            Enable_Interrupts =
              (Depth (S) = 0 and Restores_Interrupts (S)));

    function Can_Handoff (S : State; Hardware_IF, Owns_Process_Lock : Boolean)
      return Boolean is
        (Depth (S) = 1 and not Hardware_IF and Owns_Process_Lock);

    function Capture (S : State) return Context
      with Global => null, Pre => Depth (S) = 1,
           Post => Restores_Interrupts (Capture'Result) = Restores_Interrupts (S);

    procedure Resume (S : in out State; Saved : Context)
      with Global => null, Pre => Depth (S) = 1,
           Post => Depth (S) = 1 and
                   Restores_Interrupts (S) = Restores_Interrupts (Saved);

    -- The resumed context's final unlock depends only on its saved policy,
    -- regardless of the context that handed over the CPU/Process.lock.
    procedure Prove_Handoff (Suspended, Incoming : State)
      with Ghost, Global => null,
           Pre => Depth (Suspended) = 1 and Depth (Incoming) = 1;

    -- An inner lock cannot overwrite the outer restoration policy or enable
    -- interrupts on release. Only the matching outer release restores IF.
    procedure Prove_Nested_Exclusion (Idle : State; Hardware_IF : Boolean)
      with Ghost, Global => null, Pre => Depth (Idle) = 0;

private
    type Restoration_Policy is (Keep_Masked, Restore_Enabled);
    for Restoration_Policy use (Keep_Masked => 0, Restore_Enabled => 1);
    for Restoration_Policy'Size use 32;

    type State is record
        Policy : Restoration_Policy := Keep_Masked;
        Level : Natural := 0;
    end record;
    -- Preserve the two 32-bit slots in the assembly-visible per-CPU layout.
    for State use record
        Policy at 0 range 0 .. 31;
        Level at 4 range 0 .. 31;
    end record;
    for State'Size use 64;

    type Context is record
        Policy : Restoration_Policy := Keep_Masked;
    end record;
    Initial_State : constant State := (Keep_Masked, 0);
    Initial_Context : constant Context := (Policy => Keep_Masked);
    function Depth (S : State) return Natural is (S.Level);
    function Restores_Interrupts (S : State) return Boolean is
      (S.Policy = Restore_Enabled);
    function Restores_Interrupts (C : Context) return Boolean is
      (C.Policy = Restore_Enabled);
end Interrupt_State;
