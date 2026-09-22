-------------------------------------------------------------------------------
--  CuBitOS
--  Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
package body Memory_Grants with SPARK_Mode => On is
    procedure Advance_Generation
      (value    : in out Live_Grant_Generation;
       reusable :    out Boolean)
    is
    begin
        if value = Grant_Generation'Last then
            reusable := False;
        else
            value := value + 1;
            reusable := True;
        end if;
    end Advance_Generation;

    function To_Live_Generation
      (value : Unsigned_64) return Live_Grant_Generation
    is
    begin
        return Live_Grant_Generation (value);
    end To_Live_Generation;

    function Overlaps_Received_Region
      (firstByte : Unsigned_64;
       pages     : Page_Count) return Boolean
    is
        byteCount : constant Unsigned_64 := Unsigned_64 (pages) * Page_Size;
    begin
        return
            (firstByte >= Received_Region_First and then
             firstByte < Received_Region_Limit)
            or else
            (firstByte < Received_Region_First and then
             byteCount > Received_Region_First - firstByte);
    end Overlaps_Received_Region;

    function Permission_Attenuates
      (parent, child : Permission) return Boolean
    is
    begin
        return parent = Borrowed_Read_Write or else
            child = Borrowed_Read_Only;
    end Permission_Attenuates;

    function Range_Attenuates
      (parentPages : Page_Count;
       childOffset : Page_Offset;
       childPages  : Page_Count) return Boolean
    is
    begin
        return childOffset < parentPages and then
            childPages <= parentPages - childOffset;
    end Range_Attenuates;

    function Is_Valid (value : Lifecycle) return Boolean is
      ((if value.state = Inactive then
           value.acquisitions = 0 and value.forwarding /= Retained) and
       (if value.state = Revocation_Requested then
            value.acquisitions /= 0 or value.forwarding = Retained
        else True));

    function Is_Active (value : Lifecycle) return Boolean is
      (value.state /= Inactive);

    function Is_Available (value : Lifecycle) return Boolean is
      (value.state = Available);

    function Is_Revocation_Pending (value : Lifecycle) return Boolean is
      (value.state = Revocation_Requested);

    function Acquisition_Total
      (value : Lifecycle) return Acquisition_Count is
      (value.acquisitions);

    function Can_Acquire (value : Lifecycle) return Boolean is
      (value.state = Available and then
       value.acquisitions < Acquisition_Count'Last);

    function Has_Forwarding_Hold (value : Lifecycle) return Boolean is
      (value.forwarding = Retained);

    function Can_Retain_Forwarding_Hold (value : Lifecycle) return Boolean is
      (value.state = Available and value.forwarding = Unused);

    procedure Retain_Forwarding_Hold
      (value : in out Lifecycle; applied : out Boolean) is
    begin
        applied := Can_Retain_Forwarding_Hold (value);
        if applied then
            value.forwarding := Retained;
        end if;
    end Retain_Forwarding_Hold;

    procedure Record_Acquire (value : in out Lifecycle) is
    begin
        value.acquisitions := value.acquisitions + 1;
    end Record_Acquire;

    procedure Request_Revocation
      (value  : in out Lifecycle;
       result : out Revocation_Result)
    is
    begin
        if value.state = Inactive then
            result := Revocation_Rejected;
        elsif value.acquisitions = 0 and value.forwarding /= Retained then
            value := Inactive_Lifecycle;
            result := Revocation_Completed;
        else
            value.state := Revocation_Requested;
            result := Revocation_Pending;
        end if;
    end Request_Revocation;

    procedure Record_Return
      (value  : in out Lifecycle;
       result : out Return_Result)
    is
    begin
        if value.acquisitions = 0 then
            result := Return_Rejected;
        elsif value.state = Revocation_Requested and then
              value.acquisitions = 1 and then value.forwarding /= Retained
        then
            value := Inactive_Lifecycle;
            result := Revocation_Completed_On_Return;
        else
            value.acquisitions := value.acquisitions - 1;
            result := Acquisition_Returned;
        end if;
    end Record_Return;

    procedure Release_Forwarding_Hold
      (value : in out Lifecycle; result : out Hold_Release_Result) is
    begin
        if value.forwarding /= Retained then
            result := Hold_Release_Rejected;
        else
            value.forwarding := Released;
            if value.state = Revocation_Requested and value.acquisitions = 0 then
                value.state := Inactive;
                result := Revocation_Completed_On_Hold_Release;
            else
                result := Forwarding_Hold_Released;
            end if;
        end if;
    end Release_Forwarding_Hold;

    procedure Close_Receiver
      (value            : in out Lifecycle;
       had_acquisitions : out Boolean)
    is
    begin
        had_acquisitions := value.acquisitions /= 0;
        value.acquisitions := 0;
        if value.forwarding = Retained then
            value.state := Revocation_Requested;
        else
            value.state := Inactive;
        end if;
    end Close_Receiver;
end Memory_Grants;
