pragma Ada_2022;
with Interfaces;
with System;
-- Kernel-internal references only. Caller serializes every operation, owns
-- each leased reference exclusively, and detaches readers before Release.
-- Released references may not be dereferenced (their backing may be freed).
generic
   type Element is private;
   Records_Per_Slab : Positive;
   with procedure Allocate_Page
     (Owner : Interfaces.Unsigned_64; Page : out System.Address; OK : out Boolean);
   with procedure Release_Page (Page : System.Address);
package Owner_Record_Slabs is
   type Arena is private;
   No_Arena : constant Arena;
   type Node is private;
   type Reference is access all Node;
   type List is limited private;
   function Empty (Object : Arena) return Boolean;
   function Empty (Object : List) return Boolean;
   procedure Push (Object : in out List; Item : not null Reference);
   procedure Pop (Object : in out List; Item : out Reference);
   procedure Move (Source, Target : in out List);
   function Metadata_Bytes return Interfaces.Unsigned_64;
   procedure Open
     (Owner, Byte_Limit : Interfaces.Unsigned_64; Object : out Arena; OK : out Boolean);
   -- Consumes the arena handle. Outstanding records keep its original owner
   -- and page alive; a new arena for a reused PID is completely independent.
   procedure Close (Object : in out Arena);
   procedure Reserve
     (Object : Arena; Initial : Element; Byte_Limit : Interfaces.Unsigned_64;
      Item : out Reference; OK : out Boolean);
   function Value (Item : not null Reference) return Element;
   procedure Set_Value (Item : not null Reference; Data : Element);
   procedure Release (Item : in out Reference);
private
   type Arena_Data;
   type Arena is access all Arena_Data;
   No_Arena : constant Arena := null;
   type Slab;
   type Slab_Access is access all Slab;
   type Node is record
      Data : Element;
      Next : Reference := null;
      Parent : Slab_Access := null;
      Leased, Linked : Boolean := False;
   end record;
   type List is limited record
      First, Last : Reference := null;
   end record;
   type Nodes is array (1 .. Records_Per_Slab) of aliased Node;
   type Slab is record
      Parent : Arena := null;
      Previous, Following : Slab_Access := null;
      Free : Reference := null;
      Live : Natural := 0;
      Entries : Nodes;
   end record;
   type Arena_Data is record
      Owner : Interfaces.Unsigned_64 := 0;
      Available : Slab_Access := null;
      Live : Interfaces.Unsigned_64 := 0;
      Accepting : Boolean := True;
   end record;
end Owner_Record_Slabs;
