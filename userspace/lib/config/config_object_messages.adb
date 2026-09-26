with Config_Worker_Protocol;

package body Config_Object_Messages with SPARK_Mode is
   use type CCL.Objects.Schema_Key;
   package Grants renames CuBit.Grant_References;

   function Allowed_Error (Action : Operation; Code : Status) return Boolean is
     (case Action is
        when Open_Collection => Code in Invalid_Request | Denied | Missing | Busy | Unavailable | Schema_Mismatch | Capacity_Exceeded,
        when Create_Collection => Code in Invalid_Request | Denied | Missing | Busy | Unavailable | Schema_Mismatch | Capacity_Exceeded | Uncertain,
        when Get_Object => Code in Invalid_Request | Denied | Missing | Busy | Unavailable | Schema_Mismatch,
        when Set_Object => Code in Invalid_Request | Denied | Busy | Unavailable | Conflict | Rejected | Uncertain,
        when Close_Collection => Code in Invalid_Request | Denied | Unavailable);

   function Valid_Descriptor (Item : Open_Descriptor) return Boolean is
     (Item.Format = Version and then Item.Access_Rights in 1 .. 3 and then
      Item.Reserved = 0 and then Item.Schema /= CCL.Objects.No_Schema and then
      Item.Name_Length in 1 .. Maximum_Name and then
      Config_Worker_Protocol.Valid_Name (Item.Name (1 .. Natural (Item.Name_Length))) and then
      (for all I in Natural (Item.Name_Length) + 1 .. Maximum_Name => Item.Name (I) = Character'Val (0)) and then
      (for all B of Item.Padding => B = 0));

   procedure Describe
     (Name : String; Access_Rights : Access_Mode; Context : Unsigned_64;
      Schema : CCL.Objects.Schema_Key; Item : out Open_Descriptor; Valid : out Boolean) is
   begin
      Item := (others => <>);
      Valid := Config_Worker_Protocol.Valid_Name (Name) and Schema /= CCL.Objects.No_Schema;
      if not Valid then return; end if;
      Item.Access_Rights := Access_Mode'Enum_Rep (Access_Rights);
      Item.Context := Context; Item.Schema := Schema;
      Item.Name_Length := Name'Length; Item.Name (1 .. Name'Length) := Name;
   end Describe;

   function Request
     (Action : Operation; Grant : Grants.Reference;
      Handle : Unsigned_64 := 0; Revision : Unsigned_64 := 0) return CuBit.Messages.Message
   is
      Item : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Item.tag.label := Operation'Enum_Rep (Action);
      case Action is
         when Open_Collection | Create_Collection => Item.tag.length := 1; Item.words (0) := Grants.Encode (Grant);
         when Get_Object => Item.tag.length := 2; Item.words (0) := Handle; Item.words (1) := Grants.Encode (Grant);
         when Set_Object =>
            Item.tag.length := 3; Item.words (0) := Handle;
            Item.words (1) := Grants.Encode (Grant); Item.words (2) := Revision;
         when Close_Collection => Item.tag.length := 1; Item.words (0) := Handle;
      end case;
      return Item;
   end Request;

   function Valid_Request (Item : CuBit.Messages.Message; Action : Operation) return Boolean is
     (Item.tag.label = Operation'Enum_Rep (Action) and then Item.tag.flags = 0 and then Item.tag.reserved = 0 and then
      Item.words (3) = 0 and then
      (case Action is
         when Open_Collection | Create_Collection => Item.tag.length = 1 and then Grants.Valid_Wire (Item.words (0)) and then
           Item.words (1) = 0 and then Item.words (2) = 0,
         when Get_Object => Item.tag.length = 2 and then Item.words (0) /= 0 and then
           Grants.Valid_Wire (Item.words (1)) and then Item.words (2) = 0,
         when Set_Object => Item.tag.length = 3 and then Item.words (0) /= 0 and then
           Grants.Valid_Wire (Item.words (1)) and then Item.words (2) < Maximum_Revision,
         when Close_Collection => Item.tag.length = 1 and then Item.words (0) /= 0 and then
           Item.words (1) = 0 and then Item.words (2) = 0));

   function Valid_Reply
     (Item : CuBit.Messages.Message; Action : Operation;
      Expected_Revision : Unsigned_64 := 0) return Boolean is
   begin
      if Item.tag.flags /= 0 or Item.tag.reserved /= 0 or
        (for some I in 1 .. 3 => Item.words (I) /= 0)
      then return False; end if;
      if Item.tag.label = Status'Enum_Rep (Success) then
         if Item.tag.length /= 1 then return False; end if;
         return (case Action is
            when Open_Collection | Create_Collection => Item.words (0) /= 0,
            when Get_Object => Item.words (0) in 1 .. Maximum_Revision,
            when Set_Object => Expected_Revision < Maximum_Revision and then Item.words (0) = Expected_Revision + 1,
            when Close_Collection => Item.words (0) = 0);
      elsif Item.tag.label = Status'Enum_Rep (Stale) then
         return Action = Get_Object and then Item.tag.length = 1 and then Item.words (0) in 1 .. Maximum_Revision;
      else
         return Item.tag.length = 0 and then Item.words (0) = 0 and then
           (for some Code in Status => Allowed_Error (Action, Code) and then Item.tag.label = Status'Enum_Rep (Code));
      end if;
   end Valid_Reply;
end Config_Object_Messages;
