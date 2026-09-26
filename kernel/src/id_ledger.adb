package body Id_Ledger with SPARK_Mode => On is

    procedure Initialize (L : out Ledger) is
    begin
        L := (others => (Used => False, Retired => False, Generation => 0));
    end Initialize;

    -- No used entry in I's page other than I itself.
    function Scan_Page_Empty_Except (L : Ledger; I : Valid_Id) return Boolean
    with
        Post => Scan_Page_Empty_Except'Result = Page_Empty_Except (L, I)
    is
    begin
        for J in Valid_Id loop
            pragma Loop_Invariant
              (for all K in Valid_Id range 1 .. J - 1 =>
                 (if K /= I and then Page_Of (K) = Page_Of (I)
                  then not Used (L, K)));
            if J /= I and then Page_Of (J) = Page_Of (I) and then Used (L, J) then
                return False;
            end if;
        end loop;
        return True;
    end Scan_Page_Empty_Except;

    procedure Allocate (L : in out Ledger; I : out Id; First_In_Page : out Boolean) is
    begin
        I := 0;
        First_In_Page := False;
        for J in Valid_Id loop
            pragma Loop_Invariant
              (for all K in Valid_Id range 1 .. J - 1 =>
                 not Free_For_Allocate (L, K));
            if Free_For_Allocate (L, J) then
                First_In_Page := Scan_Page_Empty_Except (L, J);
                L (J).Used := True;
                I := J;
                return;
            end if;
        end loop;
    end Allocate;

    procedure Allocate_Specific (L             : in out Ledger;
                                 I             : Valid_Id;
                                 Success       : out Boolean;
                                 First_In_Page : out Boolean)
    is
    begin
        if L (I).Used or else L (I).Retired then
            Success := False;
            First_In_Page := False;
        else
            First_In_Page := Scan_Page_Empty_Except (L, I);
            L (I).Used := True;
            Success := True;
        end if;
    end Allocate_Specific;

    procedure Release (L            : in out Ledger;
                       I            : Valid_Id;
                       Last_In_Page : out Boolean;
                       Advance      : Boolean := True) is
    begin
        Last_In_Page := Scan_Page_Empty_Except (L, I);
        L (I).Used := False;
        if L (I).Generation >= Generation_Limit then
            L (I).Retired := True;
        elsif Advance then
            L (I).Generation := L (I).Generation + 1;
        end if;
    end Release;

    procedure Invalidate (L : in out Ledger; I : Valid_Id; Saturated : out Boolean) is
    begin
        Saturated := L (I).Generation >= Generation_Limit;
        if not Saturated then
            L (I).Generation := L (I).Generation + 1;
        end if;
    end Invalidate;

    procedure Prove_Release_Invalidates (L : Ledger; I : Valid_Id) is
        G       : constant Generation := Generation_Of (L, I);
        After   : Ledger := L;
        Last    : Boolean;
        Success : Boolean;
        First   : Boolean;
    begin
        Release (After, I, Last);
        pragma Assert (not Current (After, I, G));
        -- Reusing the ID for a new object does not revive the old reference.
        Allocate_Specific (After, I, Success, First);
        pragma Assert (not Current (After, I, G));
    end Prove_Release_Invalidates;

end Id_Ledger;
