with Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Messages;
with Client_Canvas_Geometry;
package body Client_Frame_Pair is
   package Frames renames Client_Frame_Buffer;
   use type DP.Status_Code, DP.Wire_Message, Pub.Configuration_Result;
   use type System.Address;
   use type Debt.Box, DP.Live_Surface_Name;
   procedure Reset (O : in out Owner; Ready : out Boolean) is
   begin
      Ready := Allocated_Bytes (O) = 0 and then not O.Painting and then
        (O.Closing or O.Config.Status /= DP.Success);
      if Ready then
         O.Config := (Status => DP.Invalid_Request);
         O.Next := 1;
         O.Closing := False;
      end if;
   end Reset;
   function Configuration (O : Owner) return Pub.Configuration_Result is (O.Config);
   function Allocated_Bytes (O : Owner) return Natural is
     (Frames.Capacity (O.Frames (1)) + Frames.Capacity (O.Frames (2)));
   function Pending (O : Owner) return Boolean is
     (O.Config.Status = DP.Success and then Debt.Nonempty (Debt.Publication_Damage (O.Damage)));
   function Address (O : Owner) return System.Address is
     (if O.Painting and not O.Closing then Frames.Writable_Address (O.Frames (O.Next))
      else System.Null_Address);
   procedure Configure (O : in out Owner; Surface : DP.Live_Surface_Name; OK : out Boolean) is
      Request : Message := CuBit.Desktop_Messages.From_Wire
        (Pub.Encode_Query ((Surface, 0), False));
      Result : Pub.Configuration_Result;
      Wire : DP.Wire_Message;
   begin
      OK := False;
      if O.Closing or else O.Painting or else
        (O.Config.Status = DP.Success and O.Surface /= Surface)
      then return; end if;
      Request.tag := capCall (CAP_SLOT_DESKTOP, Request, CuBit.Messages.Wait_Forever);
      Wire := CuBit.Desktop_Messages.To_Wire (Request);
      Result := Pub.Decode_Configuration (Wire);
      if Result.Status /= DP.Success or else Wire /= Pub.Encode_Configuration (Result) then return; end if;
      if Result /= O.Config then
         O.Config := Result;
         O.Surface := Surface;
         O.Damage := Debt.Open ((0, 0, Natural (Result.Value.Width), Natural (Result.Value.Height)));
      end if;
      OK := True;
   end Configure;
   procedure Begin_Paint (O : in out Owner; Changed : Debt.Box;
                          Repair : out Debt.Box; Ready : out Boolean) is
      Bytes : Natural;
      Released : Boolean;
   begin
      Repair := Debt.Empty;
      Ready := False;
      if O.Closing or O.Painting or O.Config.Status /= DP.Success then return; end if;
      if Debt.Nonempty (Changed) then
         if not Debt.Contains (Debt.Bounds (O.Damage), Changed) then return; end if;
         Debt.Invalidate (O.Damage, Changed);
      elsif Changed /= Debt.Empty then
         return;
      end if;
      if not Pending (O) then return; end if;
      Bytes := Natural (DP.Byte_Length (O.Config.Value.Layout));
      if Frames.Capacity (O.Frames (O.Next)) /= 0 then
         Frames.Prepare_Write (O.Frames (O.Next), Ready);
         if not Ready then return; end if;
         -- Only the retired, writable candidate may change allocation size.
         -- The other buffer stays visible throughout resize.
         if Frames.Capacity (O.Frames (O.Next)) /= ((Bytes - 1) / 4096 + 1) * 4096 then
            Frames.Release (O.Frames (O.Next), Released);
            Ready := False;
            if not Released then return; end if;
         end if;
      end if;
      if Frames.Capacity (O.Frames (O.Next)) = 0 then
         Frames.Allocate (O.Frames (O.Next), Bytes, Ready);
         if not Ready then return; end if;
      end if;
      Ready := Frames.Writable_Address (O.Frames (O.Next)) /= System.Null_Address;
      if Ready then
         Repair := Debt.Required (O.Damage, O.Next);
         O.Painting := True;
      end if;
   end Begin_Paint;
   procedure Cancel_Paint (O : in out Owner) is
   begin O.Painting := False; end Cancel_Paint;
   procedure Publish (O : in out Owner; Rendered : Debt.Box; Accepted : out Boolean;
                      Input_After : Interfaces.Unsigned_64 := 0) is
      package G renames Client_Canvas_Geometry;
      Changed : Debt.Box;
      Left, Top, Right, Bottom : Natural;
   begin
      Accepted := False;
      if not O.Painting or O.Closing or O.Config.Status /= DP.Success then return; end if;
      O.Painting := False;
      if not Debt.Nonempty (Rendered) or else
        not Debt.Contains (Debt.Bounds (O.Damage), Rendered) or else
        not Debt.Contains (Rendered, Debt.Required (O.Damage, O.Next))
      then return; end if;
      Changed := Debt.Publication_Damage (O.Damage);
      Left := G.Edge (Changed.Left, O.Config.Value.Numerator, O.Config.Value.Denominator);
      Top := G.Edge (Changed.Top, O.Config.Value.Numerator, O.Config.Value.Denominator);
      Right := G.Edge (Changed.Right, O.Config.Value.Numerator, O.Config.Value.Denominator);
      Bottom := G.Edge (Changed.Bottom, O.Config.Value.Numerator, O.Config.Value.Denominator);
      -- Zero physical extent can occur below unit density; publish full damage
      -- because the protocol's empty rectangle is its full-surface sentinel.
      Frames.Present (O.Frames (O.Next), O.Surface, O.Config.Value.Epoch,
        (DP.Pixel_Coordinate (Left), DP.Pixel_Coordinate (Top),
         DP.Pixel_Extent (Right - Left), DP.Pixel_Extent (Bottom - Top)), Accepted, Input_After);
      if Accepted then
         Debt.Published (O.Damage, O.Next, Rendered);
         O.Next := (if O.Next = 1 then 2 else 1);
      end if;
   end Publish;
   procedure Close (O : in out Owner; Released : out Boolean) is
      OK : Boolean;
   begin
      O.Closing := True;
      O.Painting := False;
      Released := True;
      for I in O.Frames'Range loop
         Frames.Release (O.Frames (I), OK);
         Released := Released and OK;
      end loop;
   end Close;
end Client_Frame_Pair;
