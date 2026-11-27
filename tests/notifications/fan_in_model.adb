pragma Ada_2022;

package body Fan_In_Model with SPARK_Mode => On is

    --  The model does not act on every result the operations return.
    pragma Warnings (GNATprove, Off, "* is set by * but not used after the call");

    procedure Prove_Initial is null;

    procedure Create_Object (S : in out State; I : Id; Owner : Owner_Ref) is
    begin
        Create (S.O (I), Owner);
    end Create_Object;

    procedure End_Object (S : in out State; I : Id) is
        Waiter, Bound : Thread_Ref;
    begin
        Finish (S.O (I), Waiter, Bound);
        S.G (I) := S.G (I) + 1;
    end End_Object;

    procedure Bind (S : in out State; L, R : Id; Badge : Unsigned_64;
                    Outcome : out Bind_Outcome)
    is
        Told : Signal_Result;
        Leaf, Root : Notification;
    begin
        if L = R then
            Outcome := Not_Allowed;
            return;
        end if;
        Leaf := S.O (L);
        Root := S.O (R);
        Bind_Leaf (Leaf, Root, L, (Id => R, Gen => S.G (R)), Badge, Outcome, Told);
        S.O (L) := Leaf;
        S.O (R) := Root;
    end Bind;

    procedure Unbind (S : in out State; L : Id) is
    begin
        Unbind_Leaf (S.O (L));
    end Unbind;

    procedure Bind_To_Thread (S : in out State; R : Id; T : Thread_Ref;
                              Outcome : out Bind_Outcome)
    is
        Told : Signal_Result;
    begin
        Bind_Thread (S.O (R), T, Outcome, Told);
    end Bind_To_Thread;

    procedure Unbind_From_Thread (S : in out State; R : Id; T : Thread_Ref) is
        Removed : Boolean;
    begin
        Unbind_Thread (S.O (R), T, Removed);
    end Unbind_From_Thread;

    procedure Wait (S : in out State; I : Id; Me : Thread_Ref; Block : Boolean;
                    Outcome : out Wait_Outcome; Taken : out Unsigned_64) is
    begin
        if Is_Leaf (S.O (I)) and then not Live_Link (S, S.O (I).Root) then
            Unbind_Leaf (S.O (I));
        end if;
        Start_Wait (S.O (I), Me, Block, Outcome, Taken);
    end Wait;

    procedure Cancel (S : in out State; I : Id; Me : Thread_Ref) is
        Removed : Boolean;
    begin
        Cancel_Wait (S.O (I), Me, Removed);
    end Cancel;

    procedure Wake (S : in out State; I : Id; Badge : Unsigned_64;
                    Woken, Rung : out Thread_Ref; Taken : out Unsigned_64)
    is
        First_Step, Second_Step : Signal_Result;
        Old : constant State := S with Ghost;
    begin
        Woken := No_Thread;
        Rung := No_Thread;
        Taken := 0;
        Signal (S.O (I), Badge, First, First_Step);
        case First_Step.What is
            when Wake_Waiter =>
                Woken := First_Step.Thread;
                Taken := First_Step.Taken;
            when Ring_Bound =>
                Rung := First_Step.Thread;
            when Nothing =>
                null;
            when Forward =>
                pragma Assert (Is_Leaf (Old.O (I)));
                if Live_Link (S, First_Step.To) then
                    pragma Assert (First_Step.To.Id /= I);
                    Signal (S.O (First_Step.To.Id), First_Step.To_Badge, Second, Second_Step);
                    pragma Assert (Second_Step.What /= Forward);
                    case Second_Step.What is
                        when Wake_Waiter =>
                            Woken := Second_Step.Thread;
                            Taken := Second_Step.Taken;
                        when Ring_Bound =>
                            Rung := Second_Step.Thread;
                        when others =>
                            null;
                    end case;
                end if;
        end case;
    end Wake;

    procedure Prove_No_Chain (S : State; I : Id) is null;

end Fan_In_Model;
