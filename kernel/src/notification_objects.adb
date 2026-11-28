-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
pragma Ada_2022;

package body Notification_Objects with SPARK_Mode => On is

    procedure Create (N : out Notification; Owner : Owner_Ref) is
    begin
        N := (Empty with delta Live => True, Owner => Owner);
    end Create;

    procedure Finish (N : in out Notification; Waiter, Bound : out Thread_Ref) is
    begin
        Waiter := N.Waiter;
        Bound := N.Bound;
        N := Empty;
    end Finish;

    procedure Revoke (N : in out Notification; Waiter : out Thread_Ref) is
    begin
        Waiter := N.Waiter;
        N.Waiter := No_Thread;
    end Revoke;

    procedure Signal (N : in out Notification; Badge : Unsigned_64; At_Level : Level;
                      Result : out Signal_Result) is
    begin
        Result := (What => Nothing, Thread => No_Thread, Taken => 0,
                   To => No_Link, To_Badge => 0);
        N.Bits := N.Bits or Badge;
        if At_Level = First and then N.Root.Id /= No_Object then
            Result.What := Forward;
            Result.To := N.Root;
            Result.To_Badge := N.Root_Badge;
        elsif N.Waiter /= No_Thread then
            Result.What := Wake_Waiter;
            Result.Thread := N.Waiter;
            Result.Taken := N.Bits;
            N.Bits := 0;
            N.Waiter := No_Thread;
        elsif N.Bound /= No_Thread then
            Result.What := Ring_Bound;
            Result.Thread := N.Bound;
        end if;
    end Signal;

    procedure Start_Wait (N : in out Notification; Me : Thread_Ref; Block : Boolean;
                          Outcome : out Wait_Outcome; Taken : out Unsigned_64) is
    begin
        Taken := 0;
        if N.Bits /= 0 then
            Outcome := Got;
            Taken := N.Bits;
            N.Bits := 0;
        elsif not Block then
            Outcome := Empty_Now;
        elsif N.Waiter /= No_Thread or else N.Root.Id /= No_Object then
            Outcome := Busy;
        else
            Outcome := Blocked;
            N.Waiter := Me;
        end if;
    end Start_Wait;

    procedure Cancel_Wait (N : in out Notification; Me : Thread_Ref; Removed : out Boolean) is
    begin
        Removed := N.Waiter = Me;
        if Removed then
            N.Waiter := No_Thread;
        end if;
    end Cancel_Wait;

    procedure Bind_Leaf (L, R : in out Notification; L_Id : Object_Id; R_Link : Link;
                         Badge : Unsigned_64; Outcome : out Bind_Outcome;
                         Told : out Signal_Result) is
    begin
        Told := (What => Nothing, Thread => No_Thread, Taken => 0,
                 To => No_Link, To_Badge => 0);
        if L_Id = R_Link.Id or else Badge = 0 or else L.Is_Root or else Is_Leaf (R) then
            Outcome := Not_Allowed;
        elsif Is_Leaf (L) then
            Outcome := Already;
        elsif L.Waiter /= No_Thread then
            Outcome := Busy;
        else
            Outcome := Bound_Now;
            L.Root := R_Link;
            L.Root_Badge := Badge;
            R.Is_Root := True;
            -- Bits that came before the binding reach the root now.
            if L.Bits /= 0 then
                Signal (R, Badge, Second, Told);
            end if;
        end if;
    end Bind_Leaf;

    procedure Unbind_Leaf (L : in out Notification) is
    begin
        L.Root := No_Link;
        L.Root_Badge := 0;
    end Unbind_Leaf;

    procedure Bind_Thread (R : in out Notification; T : Thread_Ref;
                           Outcome : out Bind_Outcome; Told : out Signal_Result) is
    begin
        Told := (What => Nothing, Thread => No_Thread, Taken => 0,
                 To => No_Link, To_Badge => 0);
        if Is_Leaf (R) then
            Outcome := Not_Allowed;
        elsif R.Bound /= No_Thread then
            Outcome := Already;
        else
            Outcome := Bound_Now;
            R.Bound := T;
            R.Is_Root := True;
            if R.Bits /= 0 and then R.Waiter = No_Thread then
                Told.What := Ring_Bound;
                Told.Thread := T;
            end if;
        end if;
    end Bind_Thread;

    procedure Unbind_Thread (R : in out Notification; T : Thread_Ref; Removed : out Boolean) is
    begin
        Removed := R.Bound = T;
        if Removed then
            R.Bound := No_Thread;
        end if;
    end Unbind_Thread;

end Notification_Objects;
