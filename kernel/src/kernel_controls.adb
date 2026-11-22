-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
package body Kernel_Controls with
    SPARK_Mode => On
is

    procedure Open (T : out Target_State; Generation : Unsigned_64) is
    begin
        T := (Open => True, Generation => Generation, Slots => (others => <>));
    end Open;

    procedure Close (T : in out Target_State) is
    begin
        T := (Open => False, Generation => T.Generation, Slots => (others => <>));
    end Close;

    procedure Send
      (T : in out Target_State; Target_Generation : Unsigned_64;
       Sender : Sender_Id; Sender_Generation : Unsigned_64; Kind : Control_Kind;
       Result : out Send_Result)
    is
        Free : Natural := 0;
    begin
        if not T.Open or else T.Generation /= Target_Generation then
            Result := Not_Open;
            return;
        end if;
        -- The sender's own slot, if it has unread messages here.
        for I in Slot_Index loop
            pragma Loop_Invariant
              (for all J in Slot_Index =>
                 (if J < I then
                    not (In_Use (T.Slots (J)) and then
                         T.Slots (J).Sender = Sender and then
                         T.Slots (J).Generation = Sender_Generation)));
            pragma Loop_Invariant
              (Free = 0 or else
               (Free in Slot_Index and then Free < I and then
                not In_Use (T.Slots (Free))));
            if In_Use (T.Slots (I)) then
                if T.Slots (I).Sender = Sender and then
                   T.Slots (I).Generation = Sender_Generation
                then
                    T.Slots (I).Pending (Kind) := True;
                    Result := Accepted;
                    return;
                end if;
            elsif Free = 0 then
                Free := I;
            end if;
        end loop;
        if Free = 0 then
            Result := Busy;
            return;
        end if;
        T.Slots (Free) :=
          (Sender => Sender, Generation => Sender_Generation, Pending => No_Kinds);
        T.Slots (Free).Pending (Kind) := True;
        Result := Accepted;
    end Send;

    procedure Take
      (T : in out Target_State; Sender : out Sender_Id;
       Sender_Generation : out Unsigned_64; Kind : out Control_Kind;
       Found : out Boolean)
    is
    begin
        Sender := No_Sender;
        Sender_Generation := 0;
        Kind := Control_Kind'First;
        Found := False;
        for I in Slot_Index loop
            pragma Loop_Invariant
              (for all J in Slot_Index => (if J < I then not In_Use (T.Slots (J))));
            pragma Loop_Invariant (T = T'Loop_Entry);
            if In_Use (T.Slots (I)) then
                for K in Control_Kind loop
                    pragma Loop_Invariant
                      (for all L in Control_Kind =>
                         (if L < K then not T.Slots (I).Pending (L)));
                    if T.Slots (I).Pending (K) then
                        Sender := T.Slots (I).Sender;
                        Sender_Generation := T.Slots (I).Generation;
                        Kind := K;
                        T.Slots (I).Pending (K) := False;
                        Found := True;
                        return;
                    end if;
                end loop;
            end if;
        end loop;
    end Take;

end Kernel_Controls;
