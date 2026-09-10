with Interfaces; use Interfaces;

--  Service-local channel identity, separate from TCP state and mapped buffers.
--  A handle is neither an array index nor authority on its own: resolution
--  also requires the kernel-supplied caller and authority tag. IDs never wrap
--  or repeat during this service lifetime. A service restart invalidates its
--  endpoint capabilities; these IDs are not persistent/distributed identities.
package Network_Channel_Handles with SPARK_Mode is
   Maximum_Channels : constant := 8;
   subtype Channel_Index is Natural range 0 .. Maximum_Channels - 1;
   subtype Channel_Reference is Integer range -1 .. Channel_Index'Last;
   No_Channel : constant Channel_Reference := -1;
   type Handle is new Unsigned_64;
   No_Handle : constant Handle := 0;
   type Table is private;

   procedure Allocate
     (Item : in out Table; Owner, Authority : Unsigned_64;
      Index : out Channel_Reference);
   function Value (Item : Table; Index : Channel_Index) return Handle;
   function Resolve
     (Item : Table; Owner, Authority : Unsigned_64; Id : Handle)
      return Channel_Reference;
   --  Internal teardown uses a validated slot, never a wire-supplied index.
   procedure Release (Item : in out Table; Index : Channel_Index);
private
   type Channel_Identity is record
      Id : Handle := No_Handle;
      Owner, Authority : Unsigned_64 := 0;
   end record;
   type Entries is array (Channel_Index) of Channel_Identity;
   type Table is record
      Slots : Entries := [others => <>];
      Next_Id : Handle := 1;
   end record;
end Network_Channel_Handles;
