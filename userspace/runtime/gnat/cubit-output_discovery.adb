pragma Ada_2022;
package body CuBit.Output_Discovery with SPARK_Mode is
   function Header (Service : Provider; Action : Operation;
                    Wire : DP.Wire_Message) return Boolean is
     (Wire.Label = Code (Service, Action) and then Wire.Length = 4 and then
      Wire.Flags = 0 and then Wire.Reserved = 0);

   function Encode_Query (Service : Provider; Item : Query)
      return DP.Wire_Message is
     (Code (Service, Get_Description), 4, 0, 0,
      [Item.Revision, Unsigned_64 (Item.Index), 0, 0]);

   function Decode_Query (Service : Provider; Wire : DP.Wire_Message)
      return Query_Decoding is
     (if not Header (Service, Get_Description, Wire) or else
        Wire.Words (0) = 0 or else Wire.Words (1) not in 1 .. 17 or else
        Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then (Valid => False)
      else (True, (Wire.Words (0), Output_Index (Wire.Words (1)))));

   function Encode_Summary (Service : Provider; Item : Summary)
      return DP.Wire_Message is
     (Code (Service, Get_Catalog), 4, 0, 0,
      [0, Item.Revision, Unsigned_64 (Item.Count), 0]);

   function Decode_Summary (Service : Provider; Wire : DP.Wire_Message)
      return Summary_Decoding is
     (if not Header (Service, Get_Catalog, Wire) or else
        Wire.Words (0) /= 0 or else Wire.Words (1) = 0 or else
        Wire.Words (2) > 17 or else Wire.Words (3) /= 0
      then (Valid => False)
      else (True, (Wire.Words (1), Output_Count (Wire.Words (2)))));

   function Dimensions (Width, Height : Extent) return Unsigned_64 is
     (Unsigned_64 (Width) or Shift_Left (Unsigned_64 (Height), 16));
   function Valid_Dimensions (Word : Unsigned_64) return Boolean is
     (Word <= 16#FFFF_FFFF# and then
      (Word and 16#FFFF#) /= 0 and then Shift_Right (Word, 16) /= 0);

   function Encode_Description (Service : Provider; Item : Description_Result)
      return DP.Wire_Message
   is
      D : Description renames Item.Item;
      Metadata : constant Unsigned_64 := Unsigned_64 (Item.Requested.Index) or
        Shift_Left (Output_Source'Enum_Rep (D.Source), 8) or
        Shift_Left (Unsigned_64 (D.Native_Number), 16) or
        Shift_Left (Output_Role'Enum_Rep (D.Role), 32);
      Active : constant Unsigned_64 :=
        (if D.Role = Detected_Only then 0
         else Dimensions (D.Current_Width, D.Current_Height));
   begin
      return (Code (Service, Get_Description), 4, 0, 0,
        [Item.Requested.Revision, Metadata,
         Dimensions (D.Advertised_Width, D.Advertised_Height), Active]);
   end Encode_Description;

   function Decode_Description (Service : Provider; Wire : DP.Wire_Message)
      return Description_Decoding
   is
      M : constant Unsigned_64 := Wire.Words (1);
      Index : constant Unsigned_64 := M and 16#FF#;
      Source : constant Unsigned_64 := Shift_Right (M, 8) and 16#FF#;
      Native : constant Unsigned_64 := Shift_Right (M, 16) and 16#FFFF#;
      Role : constant Unsigned_64 := Shift_Right (M, 32);
      D : Description;
   begin
      if not Header (Service, Get_Description, Wire) or else
        Wire.Words (0) = 0 or else Index not in 1 .. 17 or else
        Source not in 1 .. 2 or else Native > 15 or else Role > 2 or else
        not Valid_Dimensions (Wire.Words (2))
      then
         return (Valid => False);
      end if;
      if Role = 0 then
         if Wire.Words (3) /= 0 then
            return (Valid => False);
         end if;
         D := (Role => Detected_Only, others => <>);
      else
         if not Valid_Dimensions (Wire.Words (3)) then
            return (Valid => False);
         end if;
         D := (Role => Active_Role'(if Role = 1 then Backend_Ready
                                   else Selected_For_Desktop),
               Current_Width => Extent (Wire.Words (3) and 16#FFFF#),
               Current_Height => Extent (Shift_Right (Wire.Words (3), 16)),
               others => <>);
      end if;
      D.Source := (if Source = 1 then Boot_Framebuffer else Virtio_GPU);
      D.Native_Number := Native_Output_Number (Native);
      D.Advertised_Width := Extent (Wire.Words (2) and 16#FFFF#);
      D.Advertised_Height := Extent (Shift_Right (Wire.Words (2), 16));
      return (True, ((Wire.Words (0), Output_Index (Index)), D));
   end Decode_Description;

   function Respond
     (Service : Provider; Data : Catalog; Revision : Catalog_Revision;
      Request : DP.Wire_Message) return DP.Wire_Message
   is
      Q : constant Query_Decoding := Decode_Query (Service, Request);
      Status : DP.Status_Code := DP.Bad_Object;
   begin
      if Valid_Catalog_Request (Service, Request) then
         return Encode_Summary (Service, (Revision, Data.Count));
      elsif Q.Valid then
         if Q.Value.Revision /= Revision then
            Status := DP.Bad_State;
         elsif Q.Value.Index <= Data.Count then
            return Encode_Description
              (Service, (Q.Value, Data.Items (Q.Value.Index)));
         end if;
      end if;
      return (Request.Label, 1, 0, 0,
        [DP.Status_Code'Enum_Rep (Status), 0, 0, 0]);
   end Respond;
end CuBit.Output_Discovery;
