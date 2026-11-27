-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
pragma Ada_2022;

package body Wait_Deadlines with SPARK_Mode => On is

    -- Every thread keeps its membership and its deadline.
    function Same_Keys (A, B : Heap) return Boolean is
      (for all U in Thread_Ref =>
         Contains (A, U) = Contains (B, U)
         and then (if Contains (B, U) then A.Keys (A.Place (U)) = B.Keys (B.Place (U))))
    with Ghost, Pre => Placed (A) and then Placed (B);

    -- Ordered but for the edge into J (sifting up).
    function Up_Ordered (H : Heap; J : Index) return Boolean is
      ((for all I in 2 .. H.Size => (if I /= J then H.Keys (I / 2) <= H.Keys (I)))
       and then (if J > 1 then
                   (for all C in 2 .. H.Size =>
                      (if C / 2 = J then H.Keys (J / 2) <= H.Keys (C)))))
    with Ghost, Pre => J <= H.Size;

    -- Ordered but for the edges out of J (sifting down).
    function Down_Ordered (H : Heap; J : Index) return Boolean is
      ((for all I in 2 .. H.Size => (if I / 2 /= J then H.Keys (I / 2) <= H.Keys (I)))
       and then (if J > 1 then
                   (for all C in 2 .. H.Size =>
                      (if C / 2 = J then H.Keys (J / 2) <= H.Keys (C)))))
    with Ghost, Pre => J <= H.Size;

    procedure Swap (H : in out Heap; A, B : Index)
    with Pre  => Placed (H) and then A <= H.Size and then B <= H.Size and then A /= B,
         Post => Placed (H) and then H.Size = H'Old.Size and then Same_Keys (H, H'Old)
                 and then H.Keys (A) = H'Old.Keys (B) and then H.Keys (B) = H'Old.Keys (A)
                 and then (for all I in Index =>
                             (if I /= A and then I /= B then H.Keys (I) = H'Old.Keys (I)))
    is
        Key_A  : constant Unsigned_64 := H.Keys (A);
        Item_A : constant Thread_Ref := H.Items (A);
    begin
        H.Keys (A) := H.Keys (B);
        H.Items (A) := H.Items (B);
        H.Keys (B) := Key_A;
        H.Items (B) := Item_A;
        H.Place (H.Items (A)) := A;
        H.Place (H.Items (B)) := B;
    end Swap;

    procedure Sift_Up (H : in out Heap; Start : Index)
    with Pre  => Placed (H) and then Start <= H.Size and then Up_Ordered (H, Start),
         Post => Valid (H) and then H.Size = H'Old.Size and then Same_Keys (H, H'Old)
    is
        J : Index := Start;
    begin
        while J > 1 and then H.Keys (J / 2) > H.Keys (J) loop
            pragma Loop_Invariant (Placed (H) and then J <= H.Size and then H.Size = H'Loop_Entry.Size);
            pragma Loop_Invariant (Up_Ordered (H, J));
            pragma Loop_Invariant (Same_Keys (H, H'Loop_Entry));
            pragma Loop_Variant (Decreases => J);
            Swap (H, J, J / 2);
            J := J / 2;
        end loop;
    end Sift_Up;

    procedure Sift_Down (H : in out Heap; Start : Index)
    with Pre  => Placed (H) and then Start <= H.Size and then Down_Ordered (H, Start),
         Post => Valid (H) and then H.Size = H'Old.Size and then Same_Keys (H, H'Old)
    is
        J : Index := Start;
        Least : Index;
    begin
        loop
            pragma Loop_Invariant (Placed (H) and then J <= H.Size and then H.Size = H'Loop_Entry.Size);
            pragma Loop_Invariant (Down_Ordered (H, J));
            pragma Loop_Invariant (Same_Keys (H, H'Loop_Entry));
            pragma Loop_Variant (Increases => J);
            exit when 2 * J > H.Size;
            Least := 2 * J;
            if 2 * J + 1 <= H.Size and then H.Keys (2 * J + 1) < H.Keys (2 * J) then
                Least := 2 * J + 1;
            end if;
            exit when H.Keys (J) <= H.Keys (Least);
            Swap (H, J, Least);
            J := Least;
        end loop;
    end Sift_Down;

    procedure Insert (H : in out Heap; T : Thread_Ref; Key : Unsigned_64) is
        J : constant Index := H.Size + 1;
    begin
        H.Size := J;
        H.Keys (J) := Key;
        H.Items (J) := T;
        H.Place (T) := J;
        Sift_Up (H, J);
    end Insert;

    procedure Remove (H : in out Heap; T : Thread_Ref) is
        P    : constant Index := H.Place (T);
        Last : constant Index := H.Size;
    begin
        if P /= Last then
            H.Keys (P) := H.Keys (Last);
            H.Items (P) := H.Items (Last);
            H.Place (H.Items (P)) := P;
        end if;
        H.Place (T) := 0;
        H.Items (Last) := No_Thread;
        H.Size := Last - 1;
        if P <= H.Size then
            if P > 1 and then H.Keys (P / 2) > H.Keys (P) then
                Sift_Up (H, P);
            else
                Sift_Down (H, P);
            end if;
        end if;
    end Remove;

    -- The root's deadline is the least (by induction over the edges).
    procedure Root_Is_Least (H : Heap)
    with Ghost, Global => null,
         Pre  => Valid (H) and then H.Size > 0,
         Post => (for all I in 1 .. H.Size => H.Keys (1) <= H.Keys (I))
    is
    begin
        for I in 1 .. H.Size loop
            pragma Loop_Invariant (for all J in 1 .. I - 1 => H.Keys (1) <= H.Keys (J));
            pragma Assert (if I > 1 then H.Keys (1) <= H.Keys (I / 2));
        end loop;
    end Root_Is_Least;

    function Earliest (H : Heap) return Thread_Ref is
    begin
        if H.Size = 0 then
            return No_Thread;
        end if;
        Root_Is_Least (H);
        return H.Items (1);
    end Earliest;

    procedure Pop_Due (H : in out Heap; Now : Unsigned_64; T : out Thread_Ref) is
    begin
        T := No_Thread;
        if H.Size = 0 then
            return;
        end if;
        Root_Is_Least (H);
        if H.Keys (1) <= Now then
            T := H.Items (1);
            Remove (H, T);
        end if;
    end Pop_Due;

end Wait_Deadlines;
