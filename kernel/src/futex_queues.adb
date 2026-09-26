package body Futex_Queues with SPARK_Mode => On is

    procedure Enqueue (B    : in out Bucket;
                       W    : Waiter_Id;
                       K    : Key;
                       Ticket : Unsigned_64;
                       At_Slot : out Slot_Index;
                       Ok   : out Boolean)
    is
    begin
        At_Slot := Slot_Index'First;
        Ok := False;
        for I in Slot_Index loop
            pragma Loop_Invariant (for all J in Slot_Index'First .. I - 1 =>
                                     B.S (J).Used);
            if not B.S (I).Used then
                At_Slot := I;
                Ok := True;
                exit;
            end if;
        end loop;
        if not Ok then
            return;
        end if;
        B.S (At_Slot) := (Used => True, Waiter => W, K => K,
                          Ticket => Ticket);
        B.Next_Ticket := Ticket + 1;
    end Enqueue;

    procedure Remove_At (B       : in out Bucket;
                         At_Slot : Slot_Index;
                         W       : Waiter_Id;
                         Removed : out Boolean)
    is
    begin
        Removed := B.S (At_Slot).Used and then B.S (At_Slot).Waiter = W;
        if Removed then
            B.S (At_Slot) := (Used => False, Waiter => No_Waiter,
                              K => (Owner => 0, Address => 0), Ticket => 0);
        end if;
    end Remove_At;

    procedure Find_Oldest (B : Bucket; K : Key; Found : out Boolean;
                           At_Slot : out Slot_Index)
    is
    begin
        Found := False;
        At_Slot := Slot_Index'First;
        for I in Slot_Index loop
            pragma Loop_Invariant
              (if Found then
                 B.S (At_Slot).Used and then B.S (At_Slot).K = K and then
                 (for all J in Slot_Index'First .. I - 1 =>
                    (if B.S (J).Used and then B.S (J).K = K then
                       B.S (At_Slot).Ticket <= B.S (J).Ticket))
               else
                 (for all J in Slot_Index'First .. I - 1 =>
                    not (B.S (J).Used and then B.S (J).K = K)));
            if B.S (I).Used and then B.S (I).K = K and then
               (not Found or else B.S (I).Ticket < B.S (At_Slot).Ticket)
            then
                At_Slot := I;
                Found := True;
            end if;
        end loop;
    end Find_Oldest;

    procedure Wake_One (B : in out Bucket; K : Key; W : out Waiter_Id;
                        From_Slot : out Slot_Index)
    is
        Found : Boolean := False;
    begin
        W := No_Waiter;
        From_Slot := Slot_Index'First;
        for I in Slot_Index loop
            pragma Loop_Invariant
              (if Found then
                 B.S (From_Slot).Used and then B.S (From_Slot).K = K and then
                 (for all J in Slot_Index'First .. I - 1 =>
                    (if B.S (J).Used and then B.S (J).K = K then
                       B.S (From_Slot).Ticket <= B.S (J).Ticket))
               else
                 (for all J in Slot_Index'First .. I - 1 =>
                    not (B.S (J).Used and then B.S (J).K = K)));
            pragma Loop_Invariant (B = B'Loop_Entry);
            if B.S (I).Used and then B.S (I).K = K and then
               (not Found or else B.S (I).Ticket < B.S (From_Slot).Ticket)
            then
                From_Slot := I;
                Found := True;
            end if;
        end loop;
        if not Found then
            return;
        end if;
        pragma Assert (Oldest (B, K, From_Slot));
        W := B.S (From_Slot).Waiter;
        B.S (From_Slot) := (Used => False, Waiter => No_Waiter,
                            K => (Owner => 0, Address => 0), Ticket => 0);
    end Wake_One;

    procedure Prove_FIFO (B : Bucket; K : Key; W1, W2 : Waiter_Id) is
        C : Bucket := B;
        S1, S2, F : Slot_Index;
        Ok1, Ok2 : Boolean;
        First : Waiter_Id;
    begin
        Enqueue (C, W1, K, C.Next_Ticket, S1, Ok1);
        if not Ok1 then
            return;
        end if;
        pragma Assert (Contains (C, W1));
        Enqueue (C, W2, K, C.Next_Ticket, S2, Ok2);
        if not Ok2 then
            return;
        end if;
        -- W1's ticket precedes W2's, and nothing else waits on K.
        pragma Assert (C.S (S1).Ticket < C.S (S2).Ticket);
        pragma Assert (for all I in Slot_Index =>
                         (if C.S (I).Used and then C.S (I).K = K then
                            I = S1 or else I = S2));
        Wake_One (C, K, First, F);
        pragma Assert (F = S1);
        pragma Assert (First = W1);
    end Prove_FIFO;

    procedure Prove_Key_Isolation (B : Bucket; K1, K2 : Key; W : Waiter_Id) is
        C : Bucket := B;
        S, F : Slot_Index;
        Ok : Boolean;
        Woken : Waiter_Id;
    begin
        Enqueue (C, W, K1, C.Next_Ticket, S, Ok);
        if not Ok then
            return;
        end if;
        Wake_One (C, K2, Woken, F);
        -- The K1 waiter is still waiting, whatever K2 wake did.
        pragma Assert (Woken /= W);
        pragma Assert (Contains (C, W));
    end Prove_Key_Isolation;

end Futex_Queues;
