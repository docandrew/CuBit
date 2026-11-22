with Compositor_Readback_Copy;
package body Desktop_Readback_Output with SPARK_Mode is
   use type P.Ticket, P.ID, System.Address, G.Pixel_Edge;
   procedure Add_Bytes (Total : in out Byte_Count; Amount : Byte_Count;
      Saturated : in out Boolean)
     with Post => Total >= Total'Old and
       (if Amount <= Byte_Count'Last - Total'Old then
          Total = Total'Old + Amount and Saturated = Saturated'Old
        else Total = Byte_Count'Last and Saturated);
   procedure Add_Bytes (Total : in out Byte_Count; Amount : Byte_Count;
      Saturated : in out Boolean) is
   begin
      if Amount > Byte_Count'Last - Total then
         Total := Byte_Count'Last; Saturated := True;
      else Total := Total + Amount;
      end if;
   end Add_Bytes;
   function Matches (S : State; Writer : P.Ticket; Target : System.Address;
      Width, Height : G.Pixel_Edge; Bytes, Pitch : Natural) return Boolean is
     (S.Status /= Idle and then S.Destination = Writer and then S.Target = Target and then
      S.Width = Width and then S.Height = Height and then S.Bytes = Bytes and then S.Pitch = Pitch);
   procedure Begin_Transfer (S : in out State; Writer : P.Ticket;
      Target : System.Address; Width, Height : G.Pixel_Edge;
      Bytes, Pitch : Natural; Accepted : out Boolean)
   is
      Plan : constant Compositor_Row_Copy.Readback_Batch :=
        Compositor_Row_Copy.Readback_Plan (Width, Height, 0,
          D.Readback_Capacity, Bytes, Pitch, Compositor_Readback_Copy.Maximum_Batch_Bytes);
   begin
      Accepted := False;
      if Width = 0 or else Height = 0 or else S.Status /= Idle or else Writer.Buffer = 0 or else Writer.Epoch = 0 or else
         Writer.Serial = 0 or else Target = System.Null_Address or else
         Plan.Rows = 0 or else not D.Readback_Layout_Matches (Natural (Width), Natural (Height)) or else
         D.Readback_Pending /= D.No_Presentation then return; end if;
      D.Take_Readback (S.Source);
      if S.Source = P.None then return; end if;
      S.Destination := Writer; S.Target := Target; S.Width := Width;
      S.Height := Height; S.Bytes := Bytes; S.Pitch := Pitch; S.Next_Row := 0;
      Compositor_Damage.Clear (S.Repairs);
      Compositor_Damage.Add (S.Repairs, (0, 0, Natural (Width), Natural (Height)));
      S.Region := 1;
      D.Submit_Readback (S.Source, Accepted);
      if Accepted then
         Add_Bytes (S.Readback_Bytes, Byte_Count (Width) * Byte_Count (Height) * 4, S.Saturated);
      end if;
      S.Status := (if Accepted then Transferring else Quarantined);
   end Begin_Transfer;
   procedure Begin_Region_Transfer (S : in out State; Writer : P.Ticket;
      Target : System.Address; Width, Height : G.Pixel_Edge;
      Bytes, Pitch : Natural; Repair : Compositor_Damage.State; Accepted : out Boolean)
   is
      Plan : constant Compositor_Row_Copy.Readback_Batch :=
        Compositor_Row_Copy.Readback_Plan (Width, Height, 0,
          D.Readback_Capacity, Bytes, Pitch, Compositor_Readback_Copy.Maximum_Batch_Bytes);
   begin
      Accepted := False;
      if Compositor_Damage.Count (Repair) = 0 or else
         Compositor_Damage.Bounds (Repair).Right > Natural (Width) or else
         Compositor_Damage.Bounds (Repair).Bottom > Natural (Height)
      then return; end if;
      if Width = 0 or else Height = 0 or else S.Status /= Idle or else Writer.Buffer = 0 or else Writer.Epoch = 0 or else
         Writer.Serial = 0 or else Target = System.Null_Address or else
         Plan.Rows = 0 or else not D.Readback_Layout_Matches (Natural (Width), Natural (Height)) or else
         D.Readback_Pending /= D.No_Presentation then return; end if;
      D.Take_Readback (S.Source);
      if S.Source = P.None then return; end if;
      S.Destination := Writer; S.Target := Target; S.Width := Width;
      S.Height := Height; S.Bytes := Bytes; S.Pitch := Pitch; S.Next_Row := 0;
      S.Repairs := Repair;
      S.Region := 1;
      D.Submit_Region_Readback (S.Source, Repair, Accepted);
      if Accepted then
         for Index in 1 .. Compositor_Damage.Count (Repair) loop
            declare
               Box : constant Compositor_Damage.Box := Compositor_Damage.Item (Repair, Index);
            begin
               Add_Bytes (S.Readback_Bytes,
                 Byte_Count (Box.Right - Box.Left) * Byte_Count (Box.Bottom - Box.Top) * 4,
                 S.Saturated);
            end;
         end loop;
      end if;
      S.Status := (if Accepted then Transferring else Quarantined);
   end Begin_Region_Transfer;
   procedure Poll_Transfer (S : in out State; Writer : P.Ticket) is
      Result : D.Poll_Result;
   begin
      if S.Status /= Transferring or else Writer /= S.Destination then return; end if;
      if D.Readback_Pending /= S.Source then
         S.Status := Quarantined; return;
      end if;
      D.Poll_Readback (Result);
      case Result is
         when D.Pending => null;
         when D.Completed =>
            S.Status := (if D.Readback_Mapping (S.Source) /= System.Null_Address
                         then Copying else Quarantined);
         when D.Idle | D.GPU_Failed => S.Status := Quarantined;
      end case;
   end Poll_Transfer;
   procedure Begin_Copy (S : in out State; Readback, Writer : P.Ticket;
      Target : System.Address; Width, Height : G.Pixel_Edge;
      Bytes, Pitch : Natural; Accepted : out Boolean)
   is
      Plan : constant Compositor_Row_Copy.Readback_Batch :=
        Compositor_Row_Copy.Readback_Plan (Width, Height, 0,
          D.Readback_Capacity, Bytes, Pitch, Compositor_Readback_Copy.Maximum_Batch_Bytes);
   begin
      Accepted := False;
      if Width = 0 or else Height = 0 or else S.Status /= Idle or else Readback = P.None or else Writer = P.None or else
         Writer.Buffer = 0 or else Writer.Epoch = 0 or else Writer.Serial = 0 or else
         Target = System.Null_Address or else Plan.Rows = 0 or else
         not D.Readback_Layout_Matches (Natural (Width), Natural (Height)) or else
         D.Readback_Mapping (Readback) = System.Null_Address then return; end if;
      S.Source := Readback; S.Destination := Writer; S.Target := Target;
      S.Width := Width; S.Height := Height; S.Bytes := Bytes; S.Pitch := Pitch;
      S.Next_Row := 0; S.Status := Copying; Accepted := True;
      Compositor_Damage.Clear (S.Repairs);
      Compositor_Damage.Add (S.Repairs, (0, 0, Natural (Width), Natural (Height)));
      S.Region := 1;
   end Begin_Copy;
   procedure Advance (S : in out State; Writer : P.Ticket;
      Byte_Budget : Natural; Accepted : out Boolean)
   is
      Source : System.Address;
      Rows : Natural;
      Retired : Boolean;
      Repair : Compositor_Damage.Box;
   begin
      Accepted := False;
      if S.Status /= Copying or else Writer /= S.Destination then return; end if;
      Source := D.Readback_Mapping (S.Source);
      if Source = System.Null_Address then S.Status := Quarantined; return; end if;
      Repair := Compositor_Damage.Item (S.Repairs, S.Region);
      Compositor_Readback_Copy.Copy_Region (Source, S.Target, S.Width, S.Height,
        G.Pixel_Edge (S.Next_Row),
        (G.Pixel_Edge (Repair.Left), G.Pixel_Edge (Repair.Top),
         G.Pixel_Edge (Repair.Right), G.Pixel_Edge (Repair.Bottom)),
        D.Readback_Capacity, S.Bytes, S.Pitch, Byte_Budget, Rows);
      if Rows = 0 then return; end if;
      Add_Bytes (S.Copied_Bytes, Byte_Count (Rows) * Byte_Count (Repair.Right - Repair.Left) * 4,
        S.Saturated);
      S.Next_Row := S.Next_Row + Rows;
      if S.Next_Row = Repair.Bottom - Repair.Top then
         if S.Region < Compositor_Damage.Count (S.Repairs) then
            S.Region := S.Region + 1; S.Next_Row := 0; Accepted := True;
         else
            D.Retire_Readback (S.Source, True, True, Retired);
            S.Status := (if Retired then Complete else Quarantined);
            Accepted := Retired;
         end if;
      else Accepted := True;
      end if;
   end Advance;
   procedure Cancel (S : in out State; Writer : P.Ticket; Accepted : out Boolean) is
      Retired : Boolean;
   begin
      Accepted := False;
      if S.Status /= Copying or else Writer /= S.Destination then return; end if;
      if D.Readback_Mapping (S.Source) = System.Null_Address then
         S.Status := Quarantined; return;
      end if;
      -- No CPU copy is pending between calls; GPU completion was required by
      -- Begin_Copy and is revalidated above. Output content is still partial.
      D.Retire_Readback (S.Source, True, True, Retired);
      S.Status := (if Retired then Repaint else Quarantined);
      Accepted := Retired;
   end Cancel;
   procedure Acknowledge (S : in out State; Writer : P.Ticket; Accepted : out Boolean) is
   begin
      Accepted := S.Status in Complete | Repaint and then Writer = S.Destination;
      if not Accepted then return; end if;
      S.Status := Idle; S.Source := P.None; S.Destination := P.None;
      S.Target := System.Null_Address; S.Next_Row := 0;
      Compositor_Damage.Clear (S.Repairs); S.Region := 1;
   end Acknowledge;
end Desktop_Readback_Output;
