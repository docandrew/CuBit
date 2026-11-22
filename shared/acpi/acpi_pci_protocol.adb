pragma Ada_2022;
package body ACPI_PCI_Protocol with SPARK_Mode is
   use type P.Segment_Number, P.Bus_Number;
   function Framed (Item : Packet) return Boolean is
     (Item.Length = 4 and then Item.Flags = 0 and then Item.Reserved = 0);
   function Label_For (Kind : Operation) return Unsigned_32 is
     (case Kind is when Header => Header_Reply, when Read => Read_Reply, when Describe => Describe_Reply);
   function Header_Request
     (Key : P.Bus_Key := (others => <>);
      Setup_Deadline : Wire.Time_Instant := Wire.No_Finite_Deadline) return Packet is
     ((Label => Header_Operation, Data => [Version,Unsigned_64 (Key.Segment),
       Unsigned_64 (Key.Bus),Unsigned_64 (Setup_Deadline)], others => <>));
   function Describe_Request (Epoch, ID : Unsigned_64) return Packet is
     ((Label => Describe_Operation, Data => [Epoch,ID,Version,0], others => <>));
   function Read_Request
     (Epoch, ID : Unsigned_64; Region : P.Region; Offset : Unsigned_64;
      Width : P.T.Access_Width; Deadline : Microsecond_Deadline;
      For_Write : Boolean := False; Key : P.Bus_Key := (others => <>)) return Encoded_Request is
      Relative : P.Region := Region;
      Plan : P.View;
      Shape : Unsigned_64;
   begin
      if Epoch = 0 or else ID = 0 then return (Status => Missing_Identity); end if;
      if Deadline = 0 or else Deadline = No_Finite_Deadline then return (Status => Invalid_Deadline); end if;
      if Region.Device.Segment /= Key.Segment then
         return (Status => Rejected_Geometry, Reason => P.Unsupported_Segment);
      elsif Region.Device.Bus /= Key.Bus then
         return (Status => Rejected_Geometry, Reason => P.Unsupported_Bus);
      end if;
      -- The named record identifies its bus. Only function-relative geometry
      -- is packed below; the backend supplies the retained record's key.
      Relative.Device.Segment := 0; Relative.Device.Bus := 0;
      Plan := P.Plan (Relative, Offset, Width, P.Bus_Bytes, For_Write);
      if Plan.Status /= P.Read_Ready then return (Status => Rejected_Geometry, Reason => Plan.Status); end if;
      -- Segment and bus belong to the selected named record, not this shape.
      Shape := Unsigned_64 (Region.Device.Device) * P.Functions_Per_Device + Unsigned_64 (Region.Device.Func)
        or Shift_Left (Region.Base, Base_Shift)
        or Shift_Left (Region.Length, Length_Shift)
        or Shift_Left (Offset, Offset_Shift)
        or Shift_Left (P.T.Access_Width'Pos (Width), Width_Shift)
        or Shift_Left (Version, Version_Shift);
      return (Status => Encoded, Item => (Label => Read_Operation,
        Data => [Epoch, ID, Shape, Unsigned_64 (Deadline)], others => <>));
   end Read_Request;
   function Decode_Request (Item : Packet) return Request is
      Result : Request;
      Shape, Function_Index : Unsigned_64;
   begin
      if not Framed (Item) then return Result; end if;
      if Item.Label = Header_Operation then
         if Item.Data (0) /= Version
           or else Item.Data (1) > Unsigned_64 (P.Segment_Number'Last)
           or else Item.Data (2) > Unsigned_64 (P.Bus_Number'Last) then return Result; end if;
         Result.Key := (P.Segment_Number (Item.Data (1)), P.Bus_Number (Item.Data (2)));
         Result.Setup_Deadline := Wire.Time_Instant (Item.Data (3));
         Result.Kind := Header;
      elsif Item.Label = Describe_Operation then
         if Item.Data (0) = 0 or else Item.Data (1) = 0
           or else Item.Data (2) /= Version or else Item.Data (3) /= 0 then return Result; end if;
         Result.Kind := Describe; Result.Epoch := Item.Data (0); Result.ID := Item.Data (1);
      elsif Item.Label = Read_Operation then
         Shape := Item.Data (2);
         if Item.Data (0) = 0 or else Item.Data (1) = 0
           or else Shift_Right (Shape, Version_Shift) /= Version
           or else (Shape and Reserved_Mask) /= 0 then return Result; end if;
         Function_Index := Shape and Function_Mask;
         Result.Kind := Read; Result.Epoch := Item.Data (0); Result.ID := Item.Data (1);
         Result.Region.Device := (0,0,
           P.Device_Number (Function_Index / P.Functions_Per_Device),
           P.Function_Number (Function_Index mod P.Functions_Per_Device));
         Result.Region.Base := Shift_Right (Shape, Base_Shift) and Base_Mask;
         Result.Region.Length := Shift_Right (Shape, Length_Shift) and Length_Mask;
         Result.Offset := Shift_Right (Shape, Offset_Shift) and Base_Mask;
         Result.Width := P.T.Access_Width'Val (Shift_Right (Shape, Width_Shift) and 3);
         Result.For_Write := (Shift_Right (Shape, Write_Shift) and 1) /= 0;
         Result.Deadline := Microsecond_Deadline (Item.Data (3));
      else return Result;
      end if;
      Result.Valid := True; return Result;
   end Decode_Request;
   function Header_Result (Epoch, ID : Unsigned_64) return Packet is
     ((Label => Header_Reply, Data => [Outcome'Pos (Ready),Epoch,ID,Version], others => <>));
   function Describe_Result (First, Bytes : Unsigned_64) return Packet is
     ((Label => Describe_Reply, Data => [Outcome'Pos (Ready),First,Bytes,Version], others => <>));
   function Read_Result (Value : Unsigned_64) return Packet is
     ((Label => Read_Reply, Data => [Outcome'Pos (Done),Value,0,Version], others => <>));
   function Error_Reply (Kind : Operation; Status : Outcome;
      Reason : P.Outcome := P.Invalid_Window) return Packet is
     ((Label => Label_For (Kind), Data => [Outcome'Pos (Status),0,
       (if Status = Policy_Refused then 1 + P.Outcome'Pos (Reason) else 0),Version], others => <>));
   function Decode_Reply
     (Item : Packet; Kind : Operation; Width : P.T.Access_Width := P.T.Byte_Access) return Reply is
      Result : Reply;
      Code : Outcome;
      Mask : constant Unsigned_64 :=
        (case Width is when P.T.Byte_Access => 16#FF#, when P.T.Word_Access => 16#FFFF#,
         when P.T.Dword_Access => 16#FFFF_FFFF#, when P.T.Qword_Access => Unsigned_64'Last);
   begin
      if not Framed (Item) or else Item.Label /= Label_For (Kind)
        or else Item.Data (3) /= Version or else Item.Data (0) > Outcome'Pos (Outcome'Last)
      then return Result; end if;
      Code := Outcome'Val (Item.Data (0));
      if Code = Ready then
         if Kind = Header then
            if Item.Data (1) = 0 or else Item.Data (2) = 0 then return Result; end if;
            Result.Epoch := Item.Data (1); Result.ID := Item.Data (2);
         elsif Kind = Describe then
            if not Valid_Window_Metadata (Item.Data (1), Item.Data (2)) then return Result; end if;
            Result.First := Item.Data (1); Result.Bytes := Item.Data (2);
         else return Result; end if;
      elsif Code = Done then
         if Kind /= Read or else Item.Data (2) /= 0 or else Item.Data (1) > Mask then return Result; end if;
         Result.Value := Item.Data (1);
      elsif Code = Policy_Refused then
         if Kind /= Read or else Item.Data (1) /= 0 or else Item.Data (2) not in 1 .. P.Outcome'Pos (P.Outcome'Last) + 1
           or else Item.Data (2) = 1 + P.Outcome'Pos (P.Read_Ready) then return Result; end if;
         Result.Has_Reason := True; Result.Reason := P.Outcome'Val (Item.Data (2) - 1);
      elsif Item.Data (1) /= 0 or else Item.Data (2) /= 0 then return Result;
      end if;
      Result.Valid := True; Result.Status := Code; return Result;
   end Decode_Reply;
end ACPI_PCI_Protocol;
