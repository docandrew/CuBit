-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- Process_Capabilities - implementation
-------------------------------------------------------------------------------
pragma Ada_2022;

with Charged_Metadata_Pages;
with Memory_Accounting;

package body Process_Capabilities with SPARK_Mode => Off is

    procedure Allocate_Page
      (Owner : Unsigned_64; Page : out System.Address; OK : out Boolean) is
    begin
        Charged_Metadata_Pages.Allocate
          (Memory_Accounting.Identity (Owner), Page, OK);
    end Allocate_Page;

    procedure Release_Page (Page : System.Address) is
    begin
        Charged_Metadata_Pages.Release (Page);
    end Release_Page;

    procedure Limit_Of
      (Kind, Count : Unsigned_64;
       Limit       : out Tables.Handle_Limit;
       Valid       : out Boolean) is
    begin
        Limit := Tables.Default_Limit;
        Valid := False;
        if Kind = ABI.Limit_Kind'Enum_Rep (ABI.Bounded) then
            if Count in 1 .. Unsigned_64 (Tables.Max_Slots) then
                Limit := (Kind => Tables.Bounded,
                          Most => Tables.Limit_Count (Count));
                Valid := True;
            end if;
        elsif Kind = ABI.Limit_Kind'Enum_Rep (ABI.Kernel_Limit) then
            if Count = 0 then
                Limit := Tables.Kernel_Limit_Handles;
                Valid := True;
            end if;
        end if;
    end Limit_Of;

    function Find_Any (T : Tables.Table; Word : Unsigned_64) return Found is
        L : constant Tables.Lookup_Result := Tables.Lookup (T, Tables.From_Word (Word));
    begin
        if L.Check /= Tables.Current then
            return (OK => False, Why => Refusal_Of (L.Check));
        end if;
        return (OK => True, Cap => L.Item, Handle => Tables.From_Word (Word));
    end Find_Any;

    function Find
      (T      : Tables.Table;
       Word   : Unsigned_64;
       Kind   : CapabilityType;
       Rights : CapabilityRights) return Found
    is
        L : constant Tables.Lookup_Result := Tables.Lookup (T, Tables.From_Word (Word));
    begin
        if L.Check /= Tables.Current then
            return (OK => False, Why => Refusal_Of (L.Check));
        elsif L.Item.capType /= Kind then
            return (OK => False, Why => ABI.Wrong_Kind);
        elsif not isSubsetOf (Rights, L.Item.rights) then
            return (OK => False, Why => ABI.Insufficient_Rights);
        end if;
        return (OK => True, Cap => L.Item, Handle => Tables.From_Word (Word));
    end Find;

    procedure Insert (T : in out Tables.Table; Cap : Capability; Result : out Unsigned_64) is
        use type Tables.Insert_Status;
        Inserted : Tables.Insert_Result;
    begin
        Tables.Insert (T, Cap, Inserted);
        if Inserted.Status = Tables.Opened then
            Result := Tables.To_Word (Inserted.Opened_Handle);
        else
            Result := ABI.Refusal_Word (Placement.Refusal_Of (Inserted.Status));
        end if;
    end Insert;

    procedure Close
      (T : in out Tables.Table; Word : Unsigned_64;
       Removed : out Capability; Result : out Unsigned_64)
    is
        R : Tables.Remove_Result;
    begin
        Tables.Close (T, Tables.From_Word (Word), R);
        if R.Check = Tables.Current then
            Removed := R.Removed;
            Result := 0;
        else
            Removed := NULL_CAPABILITY;
            Result := ABI.Refusal_Word (Refusal_Of (R.Check));
        end if;
    end Close;

    procedure Revoke_At (T : in out Tables.Table; Slot : Tables.Slot_Index) is
        R : Tables.Remove_Result;
    begin
        if Tables.Is_Live_At (T, Slot) then
            Tables.Revoke (T, Tables.Handle_At (T, Slot), R);
        end if;
    end Revoke_At;

end Process_Capabilities;
