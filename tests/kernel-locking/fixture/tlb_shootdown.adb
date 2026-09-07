package body TLB_Shootdown is
    Counts : array (0 .. 7) of Natural := (others => 0)
      with Atomic_Components;
    procedure Service (CPU : Natural) is
    begin
        -- One Linux task per logical CPU; observe that contended loops service
        -- requests. This stub does not emulate CR3 or remote TLBs.
        Counts (CPU) := Counts (CPU) + 1;
    end Service;
    function Calls (CPU : Natural) return Natural is (Counts (CPU));
end TLB_Shootdown;
