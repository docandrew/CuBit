-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Vector for expandable storage. This mirrors a subset
--  of the Ada.Containers.Vectors package, though omits some functionality that
--  doesn't apply to CuBit.
-------------------------------------------------------------------------------
with Ada.Iterator_Interfaces;

generic
    type Index_Type is range <>;
    type Element_Type is private;
    with function "=" (Left, Right: Element_Type) return Boolean is <>;
package Vectors is
    pragma Preelaborate (Vectors);

    function Element (Container: Vector; Index: Index_Type) return Element_Type;
    function Element (Position: Cursor) return Element_Type;
    
    procedure Replace_Element (Container: in out Vector;
                               Index:     in Index_Type;
                               New_Item:  in Element_Type);
    procedure Replace_Element (Container: in out Vector;
                               Position:  in Cursor;
                               New_Item:  in Element_Type);

    function First_Index (Container: Vector) return Index_Type;
    function Last_Index (Container: Vector) return Extended_Index;                               

    function To_Cursor (Container: Vector; Index: Extended_Index) return Cursor;
    function To_Index (Position: Cursor) return Extended_Index;

    procedure Reserve_Capacity (Container : in out Vector;
                                Capacity  : in Count_Type);

    function Capacity (Container : Vector) return Count_Type;
end Vectors;