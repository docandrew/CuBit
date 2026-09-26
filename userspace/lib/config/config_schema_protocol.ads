with Interfaces;
with CCL.Objects.Schemas;
with Config_Worker_Protocol;

-- Native Config/worker metadata operations. Names/session/tokens are not
-- authority. Authenticated adapters snapshot this frame before validation.
package Config_Schema_Protocol with SPARK_Mode is
   use Interfaces;
   subtype Number is Unsigned_64;
   Maximum_Name : constant := Config_Worker_Protocol.Maximum_Name;
   Version : constant Unsigned_32 := 1;
   type Operation is (Create, Recover);
   for Operation use (Create => 1, Recover => 2);
   type Reply_Kind is
     (Created, Already_Exists, Definition_Conflict, Loaded, Absent,
      Rejected, Uncertain, Load_Failed);
   for Reply_Kind use
     (Created => 1, Already_Exists => 2, Definition_Conflict => 3,
      Loaded => 4, Absent => 5, Rejected => 6, Uncertain => 7, Load_Failed => 8);
   Frame_Bytes : constant := 4096 + CCL.Objects.Schemas.Native_Schema_Bytes;
   type Header_Padding is array (1 .. 3800) of Unsigned_8;
   type Frame is record
      Format, Action, Reply, Reserved : Unsigned_32 := 0;
      Session, Token : Number := 0;
      Name_Length, Context_Length : Unsigned_32 := 0;
      Name, Context : String (1 .. Maximum_Name) := [others => Character'Val (0)];
      Padding : Header_Padding := [others => 0];
      Metadata : CCL.Objects.Schemas.Image;
   end record with Size => Frame_Bytes * 8, Alignment => 4096;
   for Frame use record
      Format at 0 range 0 .. 31;
      Action at 4 range 0 .. 31;
      Reply at 8 range 0 .. 31;
      Reserved at 12 range 0 .. 31;
      Session at 16 range 0 .. 63;
      Token at 24 range 0 .. 63;
      Name_Length at 32 range 0 .. 31;
      Context_Length at 36 range 0 .. 31;
      Name at 40 range 0 .. Maximum_Name * 8 - 1;
      Context at 168 range 0 .. Maximum_Name * 8 - 1;
      Padding at 296 range 0 .. 3800 * 8 - 1;
      Metadata at 4096 range 0 .. CCL.Objects.Schemas.Native_Schema_Bytes * 8 - 1;
   end record;
   function Valid_Request (Item : Frame) return Boolean
     with Post => (if Valid_Request'Result then
       Item.Name_Length in 1 .. Maximum_Name and Item.Context_Length in 1 .. Maximum_Name);
   function Valid_Reply (Item, Request : Frame) return Boolean;
   procedure Make_Request
     (Action : Operation; Session, Token : Number; Name, Context : String;
      Contract : CCL.Objects.Binding; Item : out Frame; Accepted : out Boolean)
     with Post => (if Accepted then Valid_Request (Item));
   -- Only Loaded carries metadata. Create acknowledgments cannot redefine the
   -- requested type, and absence/errors cannot leak previous buffer contents.
   procedure Make_Reply
     (Request : Frame; Kind : Reply_Kind; Contract : CCL.Objects.Binding;
      Item : out Frame; Accepted : out Boolean)
     with Post => (if Accepted then Valid_Reply (Item, Request));
end Config_Schema_Protocol;
