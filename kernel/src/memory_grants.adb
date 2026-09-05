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
end Memory_Grants;
