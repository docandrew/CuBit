package body Config_Schema_Protocol with SPARK_Mode is
   package S renames CCL.Objects.Schemas;
   use type S.Image;
   Empty_Metadata : constant S.Image := (others => <>);
   function Valid_Field (Text : String; Length : Unsigned_32) return Boolean is
     (Length in 1 .. Maximum_Name and then Text'First = 1 and then
      Text'Length = Maximum_Name and then
      Config_Worker_Protocol.Valid_Name (Text (1 .. Natural (Length))) and then
      (for all I in Natural (Length) + 1 .. Maximum_Name => Text (I) = Character'Val (0)));
   function Valid_Header (Item : Frame) return Boolean is
     (Item.Format = Version and then Item.Reserved = 0 and then
      Item.Action in Operation'Enum_Rep (Create) | Operation'Enum_Rep (Recover) and then
      Item.Session /= 0 and then Item.Token not in 0 | Number'Last and then
      Valid_Field (Item.Name, Item.Name_Length) and then
      Valid_Field (Item.Context, Item.Context_Length) and then
      (for all B of Item.Padding => B = 0));
   function Valid_Metadata (Metadata : S.Image) return Boolean is
      Contract : CCL.Objects.Binding;
      Accepted : Boolean;
   begin
      S.Read (Metadata, Contract, Accepted);
      return Accepted;
   end Valid_Metadata;
   function Valid_Request (Item : Frame) return Boolean is
     (Valid_Header (Item) and then Item.Reply = 0 and then
      (if Item.Action = Operation'Enum_Rep (Create) then Valid_Metadata (Item.Metadata)
       else Item.Metadata = Empty_Metadata));
   function Valid_Reply (Item, Request : Frame) return Boolean is
   begin
      if not Valid_Request (Request) or else not Valid_Header (Item) or else
        Item.Session /= Request.Session or else Item.Token /= Request.Token or else
        Item.Action /= Request.Action or else Item.Name_Length /= Request.Name_Length or else
        Item.Context_Length /= Request.Context_Length or else Item.Name /= Request.Name or else
        Item.Context /= Request.Context then return False; end if;
      if Item.Action = Operation'Enum_Rep (Recover) and then Item.Reply = Reply_Kind'Enum_Rep (Loaded) then
         return Valid_Metadata (Item.Metadata);
      end if;
      if Item.Metadata /= Empty_Metadata then return False; end if;
      if Item.Action = Operation'Enum_Rep (Create) then
         return Item.Reply in Reply_Kind'Enum_Rep (Created) | Reply_Kind'Enum_Rep (Already_Exists) |
           Reply_Kind'Enum_Rep (Definition_Conflict) | Reply_Kind'Enum_Rep (Rejected) |
           Reply_Kind'Enum_Rep (Uncertain);
      else
         return Item.Reply in Reply_Kind'Enum_Rep (Absent) | Reply_Kind'Enum_Rep (Load_Failed);
      end if;
   end Valid_Reply;
   procedure Make_Request
     (Action : Operation; Session, Token : Number; Name, Context : String;
      Contract : CCL.Objects.Binding; Item : out Frame; Accepted : out Boolean)
   is
   begin
      Item := (others => <>); Accepted := False;
      if not Config_Worker_Protocol.Valid_Name (Name) or else
        not Config_Worker_Protocol.Valid_Name (Context) then return; end if;
      Item.Format := Version; Item.Action := Operation'Enum_Rep (Action);
      Item.Session := Session; Item.Token := Token;
      Item.Name_Length := Name'Length; Item.Context_Length := Context'Length;
      Item.Name (1 .. Name'Length) := Name; Item.Context (1 .. Context'Length) := Context;
      if Action = Create then
         S.Write (Contract, Item.Metadata, Accepted);
         if not Accepted then Item := (others => <>); return; end if;
      end if;
      Accepted := Valid_Request (Item);
      if not Accepted then Item := (others => <>); end if;
   end Make_Request;
   procedure Make_Reply
     (Request : Frame; Kind : Reply_Kind; Contract : CCL.Objects.Binding;
      Item : out Frame; Accepted : out Boolean)
   is
   begin
      Item := Request; Item.Reply := Reply_Kind'Enum_Rep (Kind);
      Item.Metadata := Empty_Metadata;
      if Kind = Loaded then
         S.Write (Contract, Item.Metadata, Accepted);
         if not Accepted then Item := (others => <>); return; end if;
      end if;
      Accepted := Valid_Reply (Item, Request);
      if not Accepted then Item := (others => <>); end if;
   end Make_Reply;
end Config_Schema_Protocol;
