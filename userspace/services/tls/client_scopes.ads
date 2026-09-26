pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.TLS_Scopes;
with CuBit.TLS_Protocol;

--  Per-client TLS scopes installed by procmgr. A client with no entry has no
--  authority; installing replaces the whole list, never merges.
package Client_Scopes with SPARK_Mode is
   Maximum_Clients : constant := 32;
   subtype Scope_Count is
     Natural range 0 .. CuBit.TLS_Protocol.Maximum_Scopes_Per_Client;
   type Scope_List is array
     (1 .. CuBit.TLS_Protocol.Maximum_Scopes_Per_Client)
     of CuBit.TLS_Scopes.Scope;

   type Table is private;

   --  Fails (installing nothing) when the table is full.
   procedure Install
     (Item : in out Table; Client : Unsigned_64; Scopes : Scope_List;
      Count : Scope_Count; Success : out Boolean)
   with Pre => Client /= 0;

   function Has_Client (Item : Table; Client : Unsigned_64) return Boolean;

   --  Free entries are Client 0, so only nonzero clients can be revoked.
   procedure Revoke (Item : in out Table; Client : Unsigned_64)
   with Pre => Client /= 0, Post => not Has_Client (Item, Client);

   --  Only an installed client can be allowed anything.
   function Allows
     (Item : Table; Client : Unsigned_64; Name : String; Port : Unsigned_16)
      return Boolean
   with
     Pre  => Name'Last < Integer'Last,
     Post => (if Allows'Result then Client /= 0 and then Has_Client (Item, Client));
private
   type Client_Entry is record
      Client : Unsigned_64 := 0;
      Count : Scope_Count := 0;
      Scopes : Scope_List;
   end record;
   type Entries is array (1 .. Maximum_Clients) of Client_Entry;
   type Table is record
      Clients : Entries;
   end record;

   function Has_Client (Item : Table; Client : Unsigned_64) return Boolean is
     (for some E of Item.Clients => E.Client = Client);
end Client_Scopes;
