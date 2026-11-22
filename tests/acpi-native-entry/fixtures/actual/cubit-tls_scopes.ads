pragma Ada_2022;
with Interfaces; use Interfaces;

--  TLS client scopes: which host names and ports a process may reach
--  through tls.svc. A scope is written "HOST:PORT" or "HOST:FIRST-LAST":
--
--    tls-test.cubit.internal:443    exactly that name
--    *.example.com:443              any name with at least one more label
--                                   before ".example.com" (not example.com)
--    *:443                          any name (a general-purpose client)
--
--  Names are canonical lowercase DNS names: labels of 1..63 letters, digits
--  or hyphens (not starting or ending with a hyphen), at most 64 characters
--  in total (the manifest access-entry limit). IP literals are not names and
--  are never matched: authority is granted by name. Requests are lowercased
--  before matching; a scope must already be lowercase.
package CuBit.TLS_Scopes with SPARK_Mode is
   Maximum_Name : constant := 64;
   subtype Name_Length is Natural range 0 .. Maximum_Name;

   type Match_Kind is (Exact, Suffix, Any_Name);

   type Scope is record
      Kind : Match_Kind := Exact;
      Name : String (1 .. Maximum_Name) := [others => ' '];
      Length : Name_Length := 0;       --  for Suffix: the part after "*."
      First_Port : Unsigned_16 := 0;
      Last_Port : Unsigned_16 := 0;
   end record;

   --  A usable host name: DNS syntax, not all-numeric (so not an IPv4
   --  literal), at most Maximum_Name characters. Uppercase is accepted
   --  here; Lowercase canonicalizes it.
   function Valid_Name (Text : String) return Boolean
   with
     Pre  => Text'Last < Integer'Last,
     Post => (if Valid_Name'Result then Text'Length in 1 .. Maximum_Name);

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z'
      then Character'Val (Character'Pos (C) + 32) else C);

   procedure Parse (Text : String; Item : out Scope; Success : out Boolean)
   with
     Post =>
       (if Success then
          Item.First_Port >= 1 and then Item.Last_Port >= Item.First_Port
          and then (Item.Kind = Any_Name or else Item.Length >= 1));

   --  Name must be a Valid_Name; comparison is case-insensitive.
   function Allows
     (Item : Scope; Name : String; Port : Unsigned_16) return Boolean
   with Pre => Name'Last < Integer'Last;
end CuBit.TLS_Scopes;
