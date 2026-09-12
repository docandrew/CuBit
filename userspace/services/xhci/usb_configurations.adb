package body USB_Configurations with SPARK_Mode => On is
   type Interface_Kind is (Other_Interface, Boot_Mouse, Bulk_Storage);
   type Candidate is record
      Kind : Interface_Kind := Other_Interface;
      Number : Unsigned_8 := 0;
      Expected_Endpoints : Unsigned_8 := 0;
      Seen_Endpoints : Natural range 0 .. Maximum_Descriptor_Bytes := 0;
      Input, Output : Endpoint;
      Invalid : Boolean := False;
   end record;

   procedure Retain (Item : Candidate; Value : in out Configuration) is
   begin
      if Item.Invalid or else
         Item.Seen_Endpoints /= Natural (Item.Expected_Endpoints)
      then
         return;
      end if;
      case Item.Kind is
         when Boot_Mouse =>
            if not Value.Mouse.Present and then Item.Input.Address /= 0 then
               Value.Mouse := (True, Item.Number, Item.Input);
            end if;
         when Bulk_Storage =>
            if not Value.Storage.Present and then
               Item.Expected_Endpoints = 2 and then
               Item.Input.Address /= 0 and then Item.Output.Address /= 0
            then
               Value.Storage := (True, Item.Number, Item.Input, Item.Output);
            end if;
         when Other_Interface => null;
      end case;
   end Retain;

   procedure Decode
     (Data : Bytes; Value : out Configuration; Result : out Decode_Result)
   is
      Item : Candidate;
      Parsed : Configuration;
      Seen_Interfaces : array (Unsigned_8) of Boolean := [others => False];
      Seen_Endpoints : array (Unsigned_8) of Boolean := [others => False];
      Interface_Count : Natural range 0 .. 255 := 0;
      Default_Alternate : Boolean := False;
   begin
      Value := (others => <>);
      Result := Malformed;
      if Data'Length not in 9 .. Maximum_Descriptor_Bytes then
         return;
      end if;
      declare
         Frame : constant Bytes (1 .. Data'Length) := Data;
         Position : Positive range 1 .. Maximum_Descriptor_Bytes + 1 := 10;
         Length : Natural;
      begin
         if Frame (1) /= 9 or else Frame (2) /= 2 or else
            Natural (Frame (3)) + 256 * Natural (Frame (4)) /= Frame'Length
            or else Frame (5) = 0 or else Frame (6) = 0
         then
            return;
         end if;
         Parsed.Value := Frame (6);
         while Position <= Frame'Last loop
            pragma Loop_Invariant (Position <= Frame'Last + 1);
            pragma Loop_Invariant (Item.Seen_Endpoints < Position);
            pragma Loop_Invariant (Interface_Count <= Natural (Frame (5)));
            if Frame'Last - Position < 1 then
               return;
            end if;
            Length := Natural (Frame (Position));
            if Length < 2 or else Length > Frame'Last - Position + 1 then
               return;
            end if;
            case Frame (Position + 1) is
               when 4 =>
                  if Length < 9 then
                     return;
                  end if;
                  Retain (Item, Parsed);
                  Item := (others => <>);
                  Item.Number := Frame (Position + 2);
                  Item.Expected_Endpoints := Frame (Position + 4);
                  Default_Alternate := Frame (Position + 3) = 0;
                  if Default_Alternate then
                     if Seen_Interfaces (Item.Number) or else
                        Interface_Count = Natural (Frame (5))
                     then
                        return;
                     end if;
                     Seen_Interfaces (Item.Number) := True;
                     Interface_Count := Interface_Count + 1;
                     if Frame (Position + 5) = 3 and then
                        Frame (Position + 6) = 1 and then
                        Frame (Position + 7) = 2
                     then
                        Item.Kind := Boot_Mouse;
                     elsif Frame (Position + 5) = 8 and then
                        Frame (Position + 6) = 6 and then
                        Frame (Position + 7) = 16#50#
                     then
                        Item.Kind := Bulk_Storage;
                     end if;
                  end if;
               when 5 =>
                  if Length < 7 then
                     return;
                  end if;
                  Item.Seen_Endpoints := Item.Seen_Endpoints + 1;
                  declare
                     Address : constant Unsigned_8 := Frame (Position + 2);
                     Attributes : constant Unsigned_8 := Frame (Position + 3);
                     Packet : constant Natural := Natural (Frame (Position + 4)) +
                       256 * Natural (Frame (Position + 5));
                     EP : Endpoint;
                  begin
                     if Default_Alternate then
                        if Seen_Endpoints (Address) then
                           return;
                        end if;
                        Seen_Endpoints (Address) := True;
                     end if;
                     if (Address and 16#70#) /= 0 or else
                        (Address and 15) = 0 or else Packet not in 1 .. 1024
                     then
                        Item.Invalid := True;
                     else
                        EP := (Address, Packet, Frame (Position + 6));
                        case Item.Kind is
                           when Boot_Mouse =>
                              if (Attributes and 3) = 3 and then
                                 (Address and 128) /= 0 and then Packet in 3 .. 64
                              then
                                 if Item.Input.Address /= 0 or else EP.Interval = 0 then
                                    Item.Invalid := True;
                                 else
                                    Item.Input := EP;
                                 end if;
                              end if;
                           when Bulk_Storage =>
                              if Attributes /= 2 then
                                 Item.Invalid := True;
                              elsif (Address and 128) /= 0 then
                                 if Item.Input.Address /= 0 then
                                    Item.Invalid := True;
                                 else
                                    Item.Input := EP;
                                 end if;
                              else
                                 if Item.Output.Address /= 0 then
                                    Item.Invalid := True;
                                 else
                                    Item.Output := EP;
                                 end if;
                              end if;
                           when Other_Interface => null;
                        end case;
                     end if;
                  end;
               when 2 => return; -- A second configuration is not a child.
               when others => null; -- Class/companion descriptors are skipped.
            end case;
            Position := Position + Length;
         end loop;
         if Interface_Count /= Natural (Frame (5)) then
            return;
         end if;
         Retain (Item, Parsed);
      end;
      Value := Parsed;
      Result := Decoded;
   end Decode;
end USB_Configurations;
