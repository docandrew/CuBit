pragma Ada_2022;
package body IOMMU_Domains with SPARK_Mode => On is

    function Find_Domain (L : Ledger; Unit : Unit_Number; Requester : Requester_ID)
      return Domain_Count is
    begin
        for D in 1 .. L.Domain_Total loop
            pragma Loop_Invariant
              (for all E in 1 .. D - 1 =>
                 L.Domains (E).Unit /= Unit or else L.Domains (E).Requester /= Requester);
            if L.Domains (D).Unit = Unit and then L.Domains (D).Requester = Requester then
                return D;
            end if;
        end loop;
        return 0;
    end Find_Domain;

    procedure Admit_Reserved
      (L : in out Ledger; Unit : Unit_Number; Requester : Requester_ID;
       First : Frame_Number; Pages : Page_Count; Status : out Admit_Status) is
    begin
        if L.Reserved_Total = Maximum_Reserved then
            Status := Capacity;
            return;
        end if;
        for N in Mapping_Index loop
            pragma Loop_Invariant
              (for all K in 1 .. N - 1 =>
                 (if Live (L.Mappings (K)) and then L.Mappings (K).Kind = Granted then
                    not Overlap (First, Pages, L.Mappings (K).First, L.Mappings (K).Pages)));
            if Live (L.Mappings (N)) and then L.Mappings (N).Kind = Granted
              and then Overlap (First, Pages, L.Mappings (N).First, L.Mappings (N).Pages)
            then
                Status := Conflicts;
                return;
            end if;
        end loop;
        L.Reserved_Total := L.Reserved_Total + 1;
        L.Reserved (L.Reserved_Total) :=
          (Unit => Unit, Requester => Requester, First => First, Pages => Pages);
        pragma Assert (Covers (L.Reserved (L.Reserved_Total), First, Pages));
        Status := Admitted;
    end Admit_Reserved;

    procedure Create_Domain
      (L : in out Ledger; Unit : Unit_Number; Requester : Requester_ID;
       D : out Domain_Index; Created : out Boolean; Status : out Domain_Status)
    is
        Existing : constant Domain_Count := Find_Domain (L, Unit, Requester);
    begin
        Created := False;
        if Existing /= 0 then
            D := Existing;
            Status := Ready;
            return;
        elsif L.Domain_Total = Maximum_Domains then
            D := Domain_Index'First;
            Status := Capacity;
            return;
        end if;
        L.Domain_Total := L.Domain_Total + 1;
        D := L.Domain_Total;
        L.Domains (D) := (Unit => Unit, Requester => Requester,
                          Owner => No_Owner, Epoch => 0);
        Created := True;
        Status := Ready;
    end Create_Domain;

    procedure Bind_Owner
      (L : in out Ledger; D : Domain_Index; Owner : Owner_Key; Status : out Bind_Status) is
    begin
        if L.Domains (D).Owner = No_Owner or else L.Domains (D).Owner = Owner then
            L.Domains (D).Owner := Owner;
            Status := Bound;
        else
            Status := Busy;
        end if;
    end Bind_Owner;

    procedure Release_Owner (L : in out Ledger; Owner : Owner_Key) is
    begin
        for D in Domain_Index loop
            pragma Loop_Invariant
              (L.Domain_Total = L.Domain_Total'Loop_Entry
               and then L.Mappings = L.Mappings'Loop_Entry
               and then L.Reserved_Total = L.Reserved_Total'Loop_Entry
               and then L.Reserved = L.Reserved'Loop_Entry
               and then (for all E in Domain_Index =>
                           L.Domains (E).Unit = L.Domains'Loop_Entry (E).Unit
                           and then L.Domains (E).Requester = L.Domains'Loop_Entry (E).Requester
                           and then L.Domains (E).Epoch = L.Domains'Loop_Entry (E).Epoch)
               and then (for all E in Domain_Index =>
                           L.Domains (E) =
                             (if E < D and then L.Domains'Loop_Entry (E).Owner = Owner
                              then (L.Domains'Loop_Entry (E) with delta Owner => No_Owner)
                              else L.Domains'Loop_Entry (E))));
            if L.Domains (D).Owner = Owner then
                L.Domains (D).Owner := No_Owner;
            end if;
        end loop;
    end Release_Owner;

    -- A free slot, or 0.
    function Free_Slot (L : Ledger) return Natural
    with Post => (if Free_Slot'Result /= 0 then
                    Free_Slot'Result in Mapping_Index
                    and then not Live (L.Mappings (Free_Slot'Result)))
    is
    begin
        for M in Mapping_Index loop
            if not Live (L.Mappings (M)) then
                return M;
            end if;
        end loop;
        return 0;
    end Free_Slot;

    function Clear_Of_Live (L : Ledger; First : Frame_Number; Pages : Page_Count)
      return Boolean
    with Post => Clear_Of_Live'Result = Reusable (L, First, Pages)
    is
    begin
        for N in Mapping_Index loop
            pragma Loop_Invariant
              (for all K in 1 .. N - 1 =>
                 (if Live (L.Mappings (K)) then
                    not Overlap (First, Pages, L.Mappings (K).First, L.Mappings (K).Pages)));
            if Live (L.Mappings (N))
              and then Overlap (First, Pages, L.Mappings (N).First, L.Mappings (N).Pages)
            then
                return False;
            end if;
        end loop;
        return True;
    end Clear_Of_Live;

    function Clear_Of_Reserved (L : Ledger; First : Frame_Number; Pages : Page_Count)
      return Boolean
    with Post => Clear_Of_Reserved'Result = not Touches_Reserved (L.Reserved, L.Reserved_Total, First, Pages)
    is
    begin
        for R in 1 .. L.Reserved_Total loop
            pragma Loop_Invariant
              (for all K in 1 .. R - 1 =>
                 not Overlap (L.Reserved (K).First, L.Reserved (K).Pages, First, Pages));
            if Overlap (L.Reserved (R).First, L.Reserved (R).Pages, First, Pages) then
                return False;
            end if;
        end loop;
        return True;
    end Clear_Of_Reserved;

    procedure Map_Grant
      (L : in out Ledger; D : Domain_Index; Owner : Owner_Key;
       First : Frame_Number; Pages : Page_Count; Writable : Boolean;
       M : out Mapping_Index; Status : out Map_Status)
    is
        Slot : Natural;
    begin
        M := Mapping_Index'First;
        if Owner = No_Owner or else L.Domains (D).Owner /= Owner then
            Status := Not_Owner;
            return;
        elsif not Clear_Of_Live (L, First, Pages)
          or else not Clear_Of_Reserved (L, First, Pages)
        then
            Status := Overlaps;
            return;
        end if;
        Slot := Free_Slot (L);
        if Slot = 0 then
            Status := Capacity;
            return;
        end if;
        M := Slot;
        L.Mappings (M) :=
          (State => Mapped, Kind => Granted, Domain => D, First => First,
           Pages => Pages, Writable => Writable, Grantee => Owner, Stamp => 0);
        Status := Added;
    end Map_Grant;

    procedure Map_Reserved
      (L : in out Ledger; D : Domain_Index; First : Frame_Number; Pages : Page_Count;
       M : out Mapping_Index; Status : out Map_Status)
    is
        Slot : Natural;
    begin
        M := Mapping_Index'First;
        if not Reserved_For (L.Reserved, L.Reserved_Total, L.Domains (D).Unit, L.Domains (D).Requester, First, Pages) then
            Status := Not_Reserved;
            return;
        end if;
        Slot := Free_Slot (L);
        if Slot = 0 then
            Status := Capacity;
            return;
        end if;
        M := Slot;
        L.Mappings (M) :=
          (State => Mapped, Kind => Reserved_Region, Domain => D, First => First,
           Pages => Pages, Writable => True, Grantee => No_Owner, Stamp => 0);
        Status := Added;
    end Map_Reserved;

    function Find_Grant
      (L : Ledger; D : Domain_Index; First : Frame_Number; Pages : Page_Count)
      return Natural is
    begin
        for M in Mapping_Index loop
            if L.Mappings (M).State = Mapped and then L.Mappings (M).Kind = Granted
              and then L.Mappings (M).Domain = D and then L.Mappings (M).First = First
              and then L.Mappings (M).Pages = Pages
            then
                return M;
            end if;
        end loop;
        return 0;
    end Find_Grant;

    procedure Begin_Unmap (L : in out Ledger; M : Mapping_Index; Stamp : out Unsigned_64) is
        D : constant Domain_Index := L.Mappings (M).Domain;
    begin
        Stamp := L.Domains (D).Epoch + 1;
        L.Domains (D).Epoch := Stamp;
        L.Mappings (M).State := Stale;
        L.Mappings (M).Stamp := Stamp;
    end Begin_Unmap;

    procedure Complete_Flush (L : in out Ledger; D : Domain_Index; Through : Unsigned_64) is
    begin
        for N in Mapping_Index loop
            pragma Loop_Invariant
              (L.Domain_Total = L.Domain_Total'Loop_Entry
               and then L.Domains = L.Domains'Loop_Entry
               and then L.Reserved_Total = L.Reserved_Total'Loop_Entry
               and then L.Reserved = L.Reserved'Loop_Entry
               and then (for all K in Mapping_Index =>
                           L.Mappings (K) =
                             (if K < N and then L.Mappings'Loop_Entry (K).State = Stale
                                and then L.Mappings'Loop_Entry (K).Domain = D
                                and then L.Mappings'Loop_Entry (K).Stamp <= Through
                              then (L.Mappings'Loop_Entry (K) with delta State => Flushed)
                              else L.Mappings'Loop_Entry (K))));
            if L.Mappings (N).State = Stale and then L.Mappings (N).Domain = D
              and then L.Mappings (N).Stamp <= Through
            then
                L.Mappings (N).State := Flushed;
            end if;
        end loop;
    end Complete_Flush;

    procedure Release
      (L : in out Ledger; M : Mapping_Index; Status : out Release_Status) is
    begin
        if L.Mappings (M).State /= Flushed then
            Status := Not_Flushed;
            return;
        end if;
        L.Mappings (M).State := Unused;
        Status := Released;
    end Release;
end IOMMU_Domains;
