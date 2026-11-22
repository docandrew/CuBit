pragma Ada_2022;
package body IO_Page_Walk with SPARK_Mode => On is

    -- The level-1 (leaf) table for Address, or Found = False.
    procedure Leaf_Table
      (Root : Page_Address; W : Address_Width; Address : Unsigned_64;
       Create : Boolean; Table : out Page_Address; Found : out Boolean)
    is
        Current : Page_Address := Root;
    begin
        Table := Root;
        Found := False;
        for L in reverse 2 .. Levels (W) loop
            declare
                Index : constant Table_Index := Index_Of (Address, L);
                E : constant Unsigned_64 := Read (Current, Index);
            begin
                if Present (E) then
                    Current := Entry_Address (E);
                elsif Create then
                    declare
                        Next : Page_Address;
                        Made : Boolean;
                    begin
                        New_Table (Next, Made);
                        if not Made then
                            return;
                        end if;
                        Write (Current, Index, Table_Entry (Next));
                        Current := Next;
                    end;
                else
                    return;
                end if;
            end;
        end loop;
        Table := Current;
        Found := True;
    end Leaf_Table;

    function Lookup (Root : Page_Address; W : Address_Width; Address : Unsigned_64)
      return Unsigned_64
    is
        Current : Page_Address := Root;
    begin
        for L in reverse 2 .. Levels (W) loop
            declare
                E : constant Unsigned_64 := Read (Current, Index_Of (Address, L));
            begin
                if not Present (E) then
                    return Not_Present;
                end if;
                Current := Entry_Address (E);
            end;
        end loop;
        return Read (Current, Index_Of (Address, 1));
    end Lookup;

    procedure Map_Page
      (Root : Page_Address; W : Address_Width; Address : Unsigned_64;
       Frame : Page_Address; Writable : Boolean; Success : out Boolean)
    is
        Table : Page_Address;
        Found : Boolean;
    begin
        Success := False;
        Leaf_Table (Root, W, Address, True, Table, Found);
        if not Found or else Present (Read (Table, Index_Of (Address, 1))) then
            return;
        end if;
        Write (Table, Index_Of (Address, 1), Leaf_Entry (Frame, Writable));
        Success := True;
    end Map_Page;

    procedure Unmap_Page (Root : Page_Address; W : Address_Width; Address : Unsigned_64) is
        Table : Page_Address;
        Found : Boolean;
    begin
        Leaf_Table (Root, W, Address, False, Table, Found);
        if Found then
            Write (Table, Index_Of (Address, 1), Not_Present);
        end if;
    end Unmap_Page;
end IO_Page_Walk;
