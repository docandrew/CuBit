------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Compositor_Surface_Wire with SPARK_Mode is

   function Kind_Word (Kind : Request_Kind) return Unsigned_64 is
     (Request_Kind'Enum_Rep (Kind));

   function Encode (Item : Request) return Request_Words is
      Wire : Request_Words := (Kind => Kind_Word (Item.Kind),
                               Surface => Unsigned_64 (Item.Surface), others => 0);
   begin
      case Item.Kind is
         when Present =>
            Wire.A := Pack (Item.Area);
            Wire.B := Item.Input_After;
         when Publish =>
            Wire.A := Pack (Item.Area);
            Wire.B := Item.Input_After;
            Wire.C := Item.Epoch;
            Wire.D := GR.Encode (Item.Grant);
         when Await_Retirement =>
            Wire.A := Item.Retiring_Epoch;
            Wire.B := Item.Ticket;
         when Receive_Input =>
            Wire.A := Item.After;
      end case;
      return Wire;
   end Encode;

   function Is_Identity (Word : Unsigned_64) return Boolean is
     (Word in Pub.Identity);

   function Decode (Wire : Request_Words) return Request_Decoding is
      Area : constant DP.Rectangle := Unpack (Wire.A);
      Surface : DP.Live_Surface_Name;
   begin
      if Wire.Surface = 0 then
         return (Valid => False);
      end if;
      Surface := DP.Live_Surface_Name (Wire.Surface);
      if Wire.Kind = Kind_Word (Present) then
         if not DP.Valid_Damage (Area) or else Wire.C /= 0 or else Wire.D /= 0 then
            return (Valid => False);
         end if;
         return (True, (Kind => Present, Surface => Surface, Area => Area, Input_After => Wire.B));
      elsif Wire.Kind = Kind_Word (Publish) then
         if not DP.Valid_Damage (Area) or else not Is_Identity (Wire.C)
           or else not GR.Valid_Wire (Wire.D)
         then
            return (Valid => False);
         end if;
         return (True, (Kind => Publish, Surface => Surface, Area => Area, Input_After => Wire.B,
                        Epoch => Wire.C, Grant => GR.Decode (Wire.D)));
      elsif Wire.Kind = Kind_Word (Await_Retirement) then
         if not Is_Identity (Wire.A) or else not Is_Identity (Wire.B)
           or else Wire.C /= 0 or else Wire.D /= 0
         then
            return (Valid => False);
         end if;
         return (True, (Kind => Await_Retirement, Surface => Surface,
                        Retiring_Epoch => Wire.A, Ticket => Wire.B));
      elsif Wire.Kind = Kind_Word (Receive_Input) then
         if Wire.B /= 0 or else Wire.C /= 0 or else Wire.D /= 0 then
            return (Valid => False);
         end if;
         return (True, (Kind => Receive_Input, Surface => Surface, After => Wire.A));
      end if;
      return (Valid => False);
   end Decode;

   function Encode (Item : Result) return Result_Words is
      Wire : Result_Words := (Kind => Kind_Word (Item.Kind),
                              Status => DP.Status_Code'Enum_Rep (Item.Status), others => <>);
   begin
      case Item.Kind is
         when Present =>
            null;
         when Publish | Await_Retirement =>
            if Item.Status = DP.Success then
               Wire.A := Item.Epoch;
               Wire.B := Item.Ticket;
            end if;
         when Receive_Input =>
            if Item.Status = DP.Success then
               Wire.Count := Unsigned_64 (Item.Count);
               Wire.A := (if Item.More then 1 else 0);
               for I in 1 .. Item.Count loop
                  Wire.Events (I) :=
                    (DP.Input_Event_Kind'Enum_Rep (Item.Events (I).Kind), Item.Events (I).Serial,
                     Item.Events (I).Payload0, Item.Events (I).Payload1);
               end loop;
            end if;
      end case;
      return Wire;
   end Encode;

   procedure Decode_Status (Word : Unsigned_64; Valid : out Boolean; Status : out DP.Status_Code)
   with Post => (if Valid then Word = DP.Status_Code'Enum_Rep (Status));
   procedure Decode_Status (Word : Unsigned_64; Valid : out Boolean; Status : out DP.Status_Code) is
   begin
      for Candidate in DP.Status_Code loop
         if Word = DP.Status_Code'Enum_Rep (Candidate) then
            Valid := True;
            Status := Candidate;
            return;
         end if;
      end loop;
      Valid := False;
      Status := DP.Invalid_Request;
   end Decode_Status;

   procedure Decode_Event (Wire : Event_Words; Valid : out Boolean; Item : out DP.Input_Envelope)
   with Post => (if Valid then Item.Kind /= DP.No_Input and then not Item.More_Pending
                   and then DP.Valid_Input_Envelope (Item) and then Item.Serial = Wire.Serial);
   procedure Decode_Event (Wire : Event_Words; Valid : out Boolean; Item : out DP.Input_Envelope) is
   begin
      Item := (others => <>);
      Valid := False;
      for Candidate in DP.Input_Event_Kind loop
         if Wire.Kind = DP.Input_Event_Kind'Enum_Rep (Candidate) then
            Item := (Candidate, Wire.Serial, Wire.Payload0, Wire.Payload1, False);
            Valid := Candidate /= DP.No_Input and then DP.Valid_Input_Envelope (Item);
            return;
         end if;
      end loop;
   end Decode_Event;

   function Decode (Wire : Result_Words; Expected : Request_Kind) return Result_Decoding is
      Status_Valid : Boolean;
      Status : DP.Status_Code;
   begin
      if Wire.Kind /= Kind_Word (Expected) then
         return (Valid => False);
      end if;
      Decode_Status (Wire.Status, Status_Valid, Status);
      if not Status_Valid then
         return (Valid => False);
      end if;
      if Status /= DP.Success then
         --  A refusal carries nothing else.
         if Wire.A /= 0 or else Wire.B /= 0 or else Wire.Count /= 0 then
            return (Valid => False);
         end if;
         case Expected is
            when Present => return (True, (Kind => Present, Status => Status));
            when Publish => return (True, (Kind => Publish, Status => Status, others => <>));
            when Await_Retirement =>
               return (True, (Kind => Await_Retirement, Status => Status, others => <>));
            when Receive_Input =>
               return (True, (Kind => Receive_Input, Status => Status, others => <>));
         end case;
      end if;
      case Expected is
         when Present =>
            if Wire.A /= 0 or else Wire.B /= 0 or else Wire.Count /= 0 then
               return (Valid => False);
            end if;
            return (True, (Kind => Present, Status => DP.Success));
         when Publish | Await_Retirement =>
            if not Is_Identity (Wire.A) or else not Is_Identity (Wire.B) or else Wire.Count /= 0 then
               return (Valid => False);
            end if;
            if Expected = Publish then
               return (True, (Kind => Publish, Status => DP.Success, Epoch => Wire.A, Ticket => Wire.B));
            end if;
            return (True, (Kind => Await_Retirement, Status => DP.Success, Epoch => Wire.A, Ticket => Wire.B));
         when Receive_Input =>
            if Wire.Count not in 1 .. Input_Batch or else Wire.A > 1 or else Wire.B /= 0 then
               return (Valid => False);
            end if;
            declare
               Value : Result :=
                 (Kind => Receive_Input, Status => DP.Success, Count => Natural (Wire.Count),
                  More => Wire.A = 1, Events => [others => (others => <>)]);
               Valid : Boolean;
            begin
               for I in 1 .. Value.Count loop
                  Decode_Event (Wire.Events (I), Valid, Value.Events (I));
                  if not Valid or else Value.Events (I).Serial = 0
                    or else (I > 1 and then Value.Events (I - 1).Serial >= Value.Events (I).Serial)
                  then
                     return (Valid => False);
                  end if;
                  pragma Loop_Invariant
                    (for all J in 1 .. I =>
                       Value.Events (J).Kind /= DP.No_Input and then
                       not Value.Events (J).More_Pending and then
                       DP.Valid_Input_Envelope (Value.Events (J)) and then
                       Value.Events (J).Serial /= 0 and then
                       (J = 1 or else Value.Events (J - 1).Serial < Value.Events (J).Serial));
               end loop;
               return (True, Value);
            end;
      end case;
   end Decode;

end Compositor_Surface_Wire;
