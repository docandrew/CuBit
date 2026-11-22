with Interfaces;
with CCL.Objects;

--  Native Config <-> storage worker data protocol, not a public SQL endpoint.
--  The IPC adapter authenticates peers and owns/snapshots the grant before
--  calling this package. A session, token, name or schema is NOT authority.
package Config_Worker_Protocol with SPARK_Mode is
   use Interfaces;
   use type CCL.Objects.Image;
   subtype Number is Unsigned_64;
   Maximum_Revision : constant Number := 2 ** 63 - 1;
   Maximum_Name : constant := 128;
   Version : constant Unsigned_32 := 1;
   type Operation is (Load, Commit);
   for Operation use (Load => 1, Commit => 2);
   type Reply_Kind is
     (Loaded, Absent, Load_Failed, Committed, Conflict, Rejected, Uncertain);
   for Reply_Kind use
     (Loaded => 1, Absent => 2, Load_Failed => 3, Committed => 4,
      Conflict => 5, Rejected => 6, Uncertain => 7);

   --  One header page followed by the existing four-page native object.
   --  Raw fields accept all bit patterns; validate BEFORE enum conversion or
   --  length narrowing. Explicit padding is checked and always initialized.
   Frame_Bytes : constant := 4096 + CCL.Objects.Native_Image_Bytes;
   type Header_Padding is array (1 .. 3792) of Unsigned_8;
   type Frame is record
      Format, Action, Reply, Reserved : Unsigned_32 := 0;
      Session, Token, Revision : Number := 0;
      Name_Length, Context_Length : Unsigned_32 := 0;
      Name, Context : String (1 .. Maximum_Name) := [others => Character'Val (0)];
      Padding : Header_Padding := [others => 0];
      Value : CCL.Objects.Image;
   end record with Size => Frame_Bytes * 8, Alignment => 4096;
   for Frame use record
      Format at 0 range 0 .. 31;
      Action at 4 range 0 .. 31;
      Reply at 8 range 0 .. 31;
      Reserved at 12 range 0 .. 31;
      Session at 16 range 0 .. 63;
      Token at 24 range 0 .. 63;
      Revision at 32 range 0 .. 63;
      Name_Length at 40 range 0 .. 31;
      Context_Length at 44 range 0 .. 31;
      Name at 48 range 0 .. Maximum_Name * 8 - 1;
      Context at 176 range 0 .. Maximum_Name * 8 - 1;
      Padding at 304 range 0 .. 3792 * 8 - 1;
      Value at 4096 range 0 .. CCL.Objects.Native_Image_Bytes * 8 - 1;
   end record;

   --  Component-separated ASCII names, consistent with the storage adapter.
   --  Names address objects; namespace/context authorization is separate.
   function Valid_Name (Text : String) return Boolean
     with Post => (if Valid_Name'Result then Text'Length in 1 .. Maximum_Name);
   --  Export the bounds already established by validation, so consumers can
   --  safely slice the names without rechecking untrusted lengths themselves.
   function Valid_Request (Item : Frame; Contract : CCL.Objects.Binding) return Boolean
     with Post => (if Valid_Request'Result then
       Item.Name_Length in 1 .. Maximum_Name and Item.Context_Length in 1 .. Maximum_Name);
   function Valid_Reply
     (Item, Request : Frame; Contract : CCL.Objects.Binding) return Boolean;

   procedure Make_Request
     (Action : Operation; Session, Token, Expected_Revision : Number;
      Name, Context : String; Contract : CCL.Objects.Binding;
      Value : CCL.Objects.Image; Item : out Frame; Accepted : out Boolean)
     with Post => (if Accepted then Valid_Request (Item, Contract));
   --  Loaded is the only reply carrying a value. Other outcomes use the
   --  canonical empty image; omitted values cannot leak old loan contents.
   procedure Make_Reply
     (Request : Frame; Kind : Reply_Kind; Saved_Revision : Number;
      Contract : CCL.Objects.Binding; Value : CCL.Objects.Image;
      Item : out Frame; Accepted : out Boolean)
     with Post => (if Accepted then Valid_Reply (Item, Request, Contract));
end Config_Worker_Protocol;
