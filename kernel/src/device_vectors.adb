-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Device interrupt vector allocator - implementation
-------------------------------------------------------------------------------
pragma Ada_2022;
package body Device_Vectors with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Run_Free: All_Free, scanning only the block.
    ---------------------------------------------------------------------------
    function Run_Free (T : Vector_Table; Head : Vector; Count : Block_Size)
      return Boolean
    with
        Pre  => Fits (Head, Count),
        Post => Run_Free'Result = All_Free (T, Head, Count)
    is
    begin
        for V in Head .. Head + Count - 1 loop
            if T (V).State /= Free then
                return False;
            end if;
            pragma Loop_Invariant
              (for all W in Head .. V => T (W).State = Free);
        end loop;
        return True;
    end Run_Free;

    ---------------------------------------------------------------------------
    -- Allocate
    ---------------------------------------------------------------------------
    procedure Allocate
      (T      : in out Vector_Table;
       Owner  : Owner_Key;
       Count  : Block_Size;
       Head   : out Vector;
       Status : out Allocation_Status)
    is
    begin
        Head := Vector'First;
        if Owner = NO_OWNER then
            Status := Invalid_Owner;
            return;
        end if;

        for H in Vector loop
            pragma Loop_Invariant (T = T'Loop_Entry);
            pragma Loop_Invariant
              (for all G in Vector'First .. H - 1 =>
                 (if Fits (G, Count) then not All_Free (T, G, Count)));

            if Fits (H, Count) and then Run_Free (T, H, Count) then
                for V in H .. H + Count - 1 loop
                    T (V) := (State => Bound, Owner => Owner,
                              Head => H, Count => Count);
                    pragma Loop_Invariant
                      (for all W in Vector =>
                         (if W in H .. V
                          then T (W) = (State => Bound, Owner => Owner,
                                        Head => H, Count => Count)
                          else T (W) = T'Loop_Entry (W)));
                end loop;
                Head := H;
                Status := Allocated;
                return;
            end if;
        end loop;

        Status := Exhausted;
    end Allocate;

    ---------------------------------------------------------------------------
    -- Release
    ---------------------------------------------------------------------------
    procedure Release
      (T      : in out Vector_Table;
       Head   : Vector;
       Status : out Release_Status)
    is
    begin
        if not Is_Head (T, Head) then
            Status := Not_A_Block;
            return;
        end if;

        for V in Vector loop
            if T (V).State /= Free and then T (V).Head = Head then
                T (V) := FREE_ENTRY;
            end if;
            pragma Loop_Invariant
              (for all W in Vector =>
                 (if W <= V then
                    (if Member (T'Loop_Entry, Head, W) then T (W) = FREE_ENTRY
                     else T (W) = T'Loop_Entry (W))
                  else T (W) = T'Loop_Entry (W)));
        end loop;
        Status := Released;
    end Release;

    ---------------------------------------------------------------------------
    -- Retire
    ---------------------------------------------------------------------------
    procedure Retire (T : in out Vector_Table; Owner : Owner_Key) is
    begin
        for V in Vector loop
            if T (V).State = Bound and then T (V).Owner = Owner then
                T (V).State := Retired;
            end if;
            pragma Loop_Invariant
              (for all W in Vector =>
                 (if W <= V
                     and then T'Loop_Entry (W).State = Bound
                     and then T'Loop_Entry (W).Owner = Owner
                  then T (W) = (T'Loop_Entry (W) with delta State => Retired)
                  else T (W) = T'Loop_Entry (W)));
        end loop;
    end Retire;
end Device_Vectors;
