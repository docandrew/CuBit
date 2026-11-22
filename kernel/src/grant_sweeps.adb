-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
package body Grant_Sweeps with SPARK_Mode => Off is

    type Cursor;
    type Cursor_Access is access all Cursor;
    type Cursor is limited record
        Key   : List_Key;
        Ahead : Slot;           -- the next grant to visit
        Outer : Cursor_Access;  -- the walk this one runs inside
    end record;

    Innermost : Cursor_Access := null;

    procedure Walk (Key : List_Key) is
        Here : aliased Cursor :=
          (Key => Key, Ahead => First (Key), Outer => Innermost);
        S : Slot;
    begin
        -- An exception here halts the kernel, so the chain never outlives
        -- this frame.
        Innermost := Here'Unchecked_Access;
        while Here.Ahead /= No_Slot loop
            S := Here.Ahead;
            Here.Ahead := Next (Key, S);
            Visit (S);
        end loop;
        Innermost := Here.Outer;
    end Walk;

    procedure Pass (Key : List_Key; S, Following : Slot) is
        C : Cursor_Access := Innermost;
    begin
        while C /= null loop
            if C.Ahead = S and then C.Key = Key then
                C.Ahead := Following;
            end if;
            C := C.Outer;
        end loop;
    end Pass;

end Grant_Sweeps;
