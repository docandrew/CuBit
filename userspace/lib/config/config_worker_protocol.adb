package body Config_Worker_Protocol with SPARK_Mode is
   function Valid_Name (Text : String) return Boolean is
      After_Dot : Boolean := True;
   begin
      if Text'Length not in 1 .. Maximum_Name then return False; end if;
      for C of Text loop
         if C = '.' then
            if After_Dot then return False; end if;
            After_Dot := True;
         elsif C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' then
            After_Dot := False;
         else
            return False;
         end if;
      end loop;
      return not After_Dot;
   end Valid_Name;

   function Valid_Field (Text : String; Length : Unsigned_32) return Boolean is
     (Length in 1 .. Maximum_Name and then Text'First = 1 and then
      Text'Length = Maximum_Name and then
      Valid_Name (Text (1 .. Natural (Length))) and then
        (for all I in Natural (Length) + 1 .. Maximum_Name =>
           Text (I) = Character'Val (0)));

   function Valid_Header (Item : Frame) return Boolean is
     (Item.Format = Version and then Item.Reserved = 0 and then
      Item.Action in Operation'Enum_Rep (Load) | Operation'Enum_Rep (Commit) and then
      Item.Session /= 0 and then Item.Token not in 0 | Number'Last and then
      Item.Revision <= Maximum_Revision and then
      Valid_Field (Item.Name, Item.Name_Length) and then
      Valid_Field (Item.Context, Item.Context_Length) and then
      (for all B of Item.Padding => B = 0));

   function Valid_Request (Item : Frame; Contract : CCL.Objects.Binding) return Boolean is
     (CCL.Objects.Is_Bound (Contract) and then Valid_Header (Item) and then
      Item.Reply = 0 and then
      (if Item.Action = Operation'Enum_Rep (Load) then
          Item.Revision = 0 and then Item.Value = CCL.Objects.Empty (Contract)
       else Item.Revision < Maximum_Revision and then
          CCL.Objects.Validate (Item.Value, Contract)));

   function Valid_Reply
     (Item, Request : Frame; Contract : CCL.Objects.Binding) return Boolean
   is
   begin
      if not Valid_Request (Request, Contract) or else not Valid_Header (Item) or else
        Item.Session /= Request.Session or else Item.Token /= Request.Token or else
        Item.Action /= Request.Action or else Item.Name_Length /= Request.Name_Length or else
        Item.Context_Length /= Request.Context_Length or else Item.Name /= Request.Name or else
        Item.Context /= Request.Context
      then return False; end if;
      if Item.Action = Operation'Enum_Rep (Load) and then Item.Reply = Reply_Kind'Enum_Rep (Loaded) then
         return Item.Revision > 0 and then CCL.Objects.Validate (Item.Value, Contract);
      end if;
      if Item.Value /= CCL.Objects.Empty (Contract) then return False; end if;
      if Item.Action = Operation'Enum_Rep (Load) then
         return Item.Reply in Reply_Kind'Enum_Rep (Absent) | Reply_Kind'Enum_Rep (Load_Failed)
           and then Item.Revision = 0;
      end if;
      if Item.Reply = Reply_Kind'Enum_Rep (Committed) then
         return Item.Revision = Request.Revision + 1;
      elsif Item.Reply = Reply_Kind'Enum_Rep (Conflict) then
         return Item.Revision /= Request.Revision;
      elsif Item.Reply = Reply_Kind'Enum_Rep (Rejected) then
         return Item.Revision = Request.Revision;
      else
         return Item.Reply = Reply_Kind'Enum_Rep (Uncertain) and then Item.Revision = 0;
      end if;
   end Valid_Reply;

   procedure Make_Request
     (Action : Operation; Session, Token, Expected_Revision : Number;
      Name, Context : String; Contract : CCL.Objects.Binding;
      Value : CCL.Objects.Image; Item : out Frame; Accepted : out Boolean)
   is
   begin
      Item := (Value => CCL.Objects.Empty (Contract), others => <>);
      Accepted := False;
      if not Valid_Name (Name) or else not Valid_Name (Context) then return; end if;
      Item.Format := Version;
      Item.Action := Operation'Enum_Rep (Action);
      Item.Session := Session;
      Item.Token := Token;
      Item.Revision := Expected_Revision;
      Item.Name_Length := Name'Length;
      Item.Context_Length := Context'Length;
      Item.Name (1 .. Name'Length) := Name;
      Item.Context (1 .. Context'Length) := Context;
      if Action = Commit then Item.Value := Value; end if;
      Accepted := Valid_Request (Item, Contract);
      if not Accepted then Item := (Value => CCL.Objects.Empty (Contract), others => <>); end if;
   end Make_Request;

   procedure Make_Reply
     (Request : Frame; Kind : Reply_Kind; Saved_Revision : Number;
      Contract : CCL.Objects.Binding; Value : CCL.Objects.Image;
      Item : out Frame; Accepted : out Boolean)
   is
   begin
      Item := Request;
      Item.Reply := Reply_Kind'Enum_Rep (Kind);
      Item.Revision := Saved_Revision;
      Item.Value := (if Kind = Loaded then Value else CCL.Objects.Empty (Contract));
      Accepted := Valid_Reply (Item, Request, Contract);
      if not Accepted then Item := (Value => CCL.Objects.Empty (Contract), others => <>); end if;
   end Make_Reply;
end Config_Worker_Protocol;
