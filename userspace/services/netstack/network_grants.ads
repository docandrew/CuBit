pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Network_Authority; use CuBit.Network_Authority;

package Network_Grants with SPARK_Mode is
   Maximum_Grants : constant := 32;
   type Table is private;
   --  Only the authenticated policy endpoint may call Install/Release.
   procedure Install
     (State : in out Table; Owner : Unsigned_64; Item : Scope;
      Tag : out Unsigned_64; Success : out Boolean);
   procedure Release (State : in out Table; Owner, Tag : Unsigned_64);
   function Owned (State : Table; Owner, Tag : Unsigned_64) return Boolean;
   function May_Resolve
     (State : Table; Owner, Tag : Unsigned_64) return Boolean;
   function Allows
     (State : Table; Owner, Tag : Unsigned_64; Action : Operation;
      Address : Unsigned_32; Port : Unsigned_16) return Boolean;
private
   type Grant_Record is record
      Owner : Unsigned_64 := 0;
      Tag : Unsigned_64 := 0;
      Item : Scope := Denied_Scope;
   end record;
   type Grant_Array is array (1 .. Maximum_Grants) of Grant_Record;
   type Table is record
      Entries : Grant_Array := [others => <>];
      Next_Tag : Unsigned_64 := First_Grant_Tag;
   end record;
end Network_Grants;
