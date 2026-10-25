pragma Ada_2022;

package body Firmware_Snapshot_Service with SPARK_Mode is

    use type Firmware_Tables.Snapshots.Metadata_Status;
    use type Transfers.Transfer_Status;

    function Signature_Of (Name : Firmware_Tables.Signature) return ABI.Signature_Bytes is
        ([for I in ABI.Signature_Index => Character'Pos (Name (I))]);

    procedure Serve_Info (Result : out ABI.Outcome)
    with Post => Result in ABI.Ok | ABI.Not_Ready | ABI.Copy_Failed
    is
        Ready : constant Boolean := Snapshot_Ready;
        Count : constant Natural := (if Ready then Table_Count else 0);
        Info  : ABI.Catalog := (Ready => Ready, Table_Count => Unsigned_32 (Count), others => <>);
        Found : Firmware_Tables.Snapshots.Metadata_Result;
        Extent : Unsigned_64;
        Written : Boolean;
    begin
        Result := ABI.Not_Ready;
        for Index in 1 .. Count loop
            pragma Loop_Invariant (Info.Ready = Ready and then Info.Table_Count = Unsigned_32 (Count));
            pragma Loop_Invariant (Info.Largest <= Info.Total);
            Found := Describe (Index);
            -- The snapshot is sealed: a ready one describes every table it
            -- counts. A disagreement is reported as not ready.
            if Found.Status /= Firmware_Tables.Snapshots.Metadata_Available then
                return;
            end if;
            Extent := Unsigned_64 (Found.Info.Extent);
            -- Fewer than 2**31 tables of fewer than 2**31 bytes never reach
            -- the limit; saturating keeps the sum total without assuming so.
            if Extent > Unsigned_64'Last - Info.Total then
                Info.Total := Unsigned_64'Last;
            else
                Info.Total := Info.Total + Extent;
            end if;
            Info.Largest := Unsigned_64'Max (Info.Largest, Extent);
        end loop;
        Write_Reply (ABI.Encode_Info (Info), Written);
        Result := (if Written then ABI.Ok else ABI.Copy_Failed);
    end Serve_Info;

    -- The table's metadata, or why there is none. A table word past
    -- Natural is no table: it is narrowed only once known to fit.
    function Find (Table : ABI.Table_Number) return Firmware_Tables.Snapshots.Metadata_Result is
        (if not Snapshot_Ready
         then (Status => Firmware_Tables.Snapshots.Snapshot_Not_Ready)
         elsif Table = 0 or else Unsigned_64 (Table) > Unsigned_64 (Natural'Last)
         then (Status => Firmware_Tables.Snapshots.Unknown_Table)
         else Describe (Natural (Table)))
    with Post => (if Find'Result.Status = Firmware_Tables.Snapshots.Metadata_Available
                  then Table in 1 .. ABI.Table_Number (Natural'Last));

    procedure Serve_Describe (Table : ABI.Table_Number; Result : out ABI.Outcome)
    with Post => Result in ABI.Ok | ABI.Not_Ready | ABI.Unknown_Table | ABI.Copy_Failed
    is
        Found : constant Firmware_Tables.Snapshots.Metadata_Result := Find (Table);
        Written : Boolean;
    begin
        Result := Metadata_Outcome (Found.Status);
        if Result /= ABI.Ok then
            return;
        end if;
        Write_Reply
          (ABI.Encode_Description
             ((Table     => Table,
               Signature => Signature_Of (Found.Info.Name),
               Revision  => Found.Info.Revision,
               Extent    => Unsigned_64 (Found.Info.Extent))),
           Written);
        Result := (if Written then ABI.Ok else ABI.Copy_Failed);
    end Serve_Describe;

    procedure Serve_Copy (Item : ABI.Request; Result : out ABI.Outcome)
    with Pre  => Item.Op = ABI.Copy_Table_Part and then ABI.Valid_Request (Item),
         Post => Result in ABI.Ok | ABI.Not_Ready | ABI.Unknown_Table | ABI.Beyond_End
                         | ABI.Empty_Before_End | ABI.Copy_Failed
    is
        Found   : constant Firmware_Tables.Snapshots.Metadata_Result := Find (Item.Table);
        Plan    : ABI.Copy_Plan;
        Extent  : Unsigned_64;
        Index   : Natural;
        Done    : Natural := 0;
        Total   : constant Natural := Natural (Item.Capacity);
        Written : Boolean;
    begin
        -- The logical table and offset first; nothing is copied before.
        Result := Metadata_Outcome (Found.Status);
        if Result /= ABI.Ok then
            return;
        end if;
        Index := Natural (Item.Table);
        Extent := Unsigned_64 (Found.Info.Extent);
        Plan := ABI.Plan_Copy (Extent, Item.Offset, Item.Capacity);
        if Plan.Status /= ABI.Ok then
            Result := Plan.Status;
            return;
        end if;
        -- Plan_Copy: Offset <= Extent <= Natural'Last.
        while Done < Total loop
            pragma Loop_Invariant (Done < Total and then Total <= ABI.Chunk_Max);
            pragma Loop_Invariant (Item.Offset <= Extent and then Extent <= Unsigned_64 (Natural'Last));
            declare
                Length : constant Positive := Natural'Min (Piece_Bytes, Total - Done);
                Source : constant Unsigned_64 := Item.Offset + Unsigned_64 (Done);
                Piece  : Firmware_Tables.Bytes (1 .. Length) := [others => 0];
                Moved  : Transfers.Transfer_Result;
            begin
                if Source <= Extent then
                    Copy_Part (Index, Natural (Source), Piece, Moved);
                    -- The sealed snapshot agrees with its own descriptor; a
                    -- disagreement is reported, never papered over.
                    if Moved.Status /= Transfers.Transferred then
                        Result := Transfer_Outcome (Moved.Status);
                        if Result = ABI.Ok then
                            Result := ABI.Copy_Failed;
                        end if;
                        return;
                    elsif Unsigned_64 (Moved.Copied) /=
                          Unsigned_64'Min (Unsigned_64 (Length), Extent - Source)
                    then
                        Result := ABI.Copy_Failed;
                        return;
                    end if;
                end if;
                -- Past the table's end the piece stays zero (the suffix).
                Write_User (Item.Destination + Unsigned_64 (Done), Piece, Written);
                if not Written then
                    Result := ABI.Copy_Failed;
                    return;
                end if;
                Done := Done + Length;
            end;
        end loop;
        Write_Reply (ABI.Encode_Copy (Plan.Copied), Written);
        Result := (if Written then ABI.Ok else ABI.Copy_Failed);
    end Serve_Copy;

    procedure Serve (Words : ABI.Request_Words; Result : out ABI.Outcome) is
        Item  : ABI.Request;
        Valid : Boolean;
    begin
        ABI.Decode (Words, Item, Valid);
        if not Valid then
            Result := ABI.Malformed;
            return;
        end if;
        case Item.Op is
            when ABI.Catalog_Info    => Serve_Info (Result);
            when ABI.Describe_Table  => Serve_Describe (Item.Table, Result);
            when ABI.Copy_Table_Part => Serve_Copy (Item, Result);
        end case;
    end Serve;

end Firmware_Snapshot_Service;
