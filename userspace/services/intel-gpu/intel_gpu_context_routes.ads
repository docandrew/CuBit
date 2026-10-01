with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Event;
generic
   Capacity : Positive;
package Intel_GPU_Context_Routes with SPARK_Mode is
   No_Context : constant Unsigned_32 := 65535;
   -- Serialized, retained ownership records for one CT transport lifetime.
   -- No deletion or ID reuse: failed contexts still own their late replies.
   type Registry is limited private;
   function Count (Object : Registry) return Natural;
   function Matches (Object : Registry; Fence : Unsigned_16;
                     ID : Unsigned_32) return Boolean;
   function Owner (Object : Registry; Fence : Unsigned_16) return Unsigned_32
     with Post => Matches (Object, Fence, Owner'Result);
   function Contains (Object : Registry; ID : Unsigned_32) return Boolean;
   type Disposition is (Invalid_Message, Unclaimed, Context_Message);
   type Destination is record
      Kind : Disposition := Invalid_Message;
      ID : Unsigned_32 := No_Context;
   end record;
   -- Only owned, CT-validated payload copies. Scheduling notifications route
   -- by their explicit ID; request failures route by the transport fence.
   -- Unclaimed messages must be retained for the other channel consumers.
   function Select_Destination
     (Object : Registry; Payload : Intel_GPU_GuC_Context_Event.Words;
      Fence : Unsigned_16) return Destination
     with Post =>
       (if Select_Destination'Result.Kind = Context_Message then
          Select_Destination'Result.ID < No_Context and
          Contains (Object, Select_Destination'Result.ID)
        else Select_Destination'Result.ID = No_Context);
   procedure Register
     (Object : in out Registry; ID : Unsigned_32;
      First, Last : Unsigned_16; Accepted : out Boolean)
     with Post => (if Accepted then Contains (Object, ID) and
       Owner (Object, First) = ID and Owner (Object, Last) = ID);
private
   -- Unused entries are excluded by Used; a retained entry can never carry
   -- the sentinel returned for an unowned fence.
   subtype Context_ID is Unsigned_32 range 0 .. No_Context - 1;
   type Route_Record is record
      ID : Context_ID := 0;
      First, Last : Unsigned_16 := 0;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Route_Record;
   type Registry is limited record
      Used : Natural range 0 .. Capacity := 0;
      Items : Entries;
   end record;
end Intel_GPU_Context_Routes;
