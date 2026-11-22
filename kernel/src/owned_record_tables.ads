with Interfaces;
with System;
-- Caller serializes every operation and keeps references only until Release.
-- IDs are private metadata handles, never user authority. Generation/ownership
-- validation remains with the mapping layer. Backing failures are fallible.
generic
   type Element is private;
   Maximum_Records : Positive;
   Records_Per_Block : Positive;
   with procedure Allocate_Memory (Bytes : Positive; Address : out System.Address);
   with procedure Free_Memory (Bytes : Positive; Address : System.Address);
package Owned_Record_Tables with SPARK_Mode => Off is
   subtype Id is Natural range 0 .. Maximum_Records;
   subtype Valid_Id is Id range 1 .. Id'Last;
   type Reference (Value : not null access Element) is null record
     with Implicit_Dereference => Value;
   type Table is limited private;
   type Table_Access is access all Table;
   type Cursor is private;
   type View (Object : not null Table_Access) is null record
     with Iterable => (First => Start, Next => Advance,
                       Has_Element => Has_Element, Element => Current);
   function Start (Object : View) return Cursor;
   function Advance (Object : View; Position : Cursor) return Cursor;
   function Has_Element (Object : View; Position : Cursor) return Boolean;
   function Current (Object : View; Position : Cursor) return Id;
   -- Removing the current record is allowed; removing the prefetched successor
   -- during iteration is not. The caller still holds the registry lock.
   function Iterate (Object : not null Table_Access) return View;
   procedure Insert (Object : in out Table; Key : Interfaces.Unsigned_64;
                     Initial : Element; Index : out Id);
   procedure Release (Object : in out Table; Index : Valid_Id);
   function Present (Object : Table; Index : Id) return Boolean;
   function Get (Object : Table; Index : Valid_Id) return Reference
     with Pre => Present (Object, Index);
   -- Ordered iteration, including equal keys. Save Next before releasing Index.
   function First (Object : Table) return Id;
   function Next (Object : Table; Index : Valid_Id) return Id
     with Pre => Present (Object, Index);
   function Count (Object : Table) return Natural;
private
   type Cursor is record
      Index, Following : Id := 0;
   end record;
   type Directory;
   type Directory_Pointer is access all Directory;
   type Table is limited record
      Pages : Directory_Pointer := null;
      Head, Tail : Id := 0;
      Used : Natural range 0 .. Maximum_Records := 0;
      Hint : Natural := 0;
   end record;
end Owned_Record_Tables;
