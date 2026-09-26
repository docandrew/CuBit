pragma Ada_2022;
package body SNTP with SPARK_Mode is
   Era_Seconds : constant := 2 ** 32;

   function Word32 (Data : Byte_Array; First : Positive) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Data (First)), 24) or
      Shift_Left (Unsigned_32 (Data (First + 1)), 16) or
      Shift_Left (Unsigned_32 (Data (First + 2)), 8) or
      Unsigned_32 (Data (First + 3)))
   with Pre => Data'First = 1 and then Data'Last >= 4 and then
               First <= Data'Last - 3;

   function Word64 (Data : Byte_Array; First : Positive) return Unsigned_64 is
     (Shift_Left (Unsigned_64 (Word32 (Data, First)), 32) or
      Unsigned_64 (Word32 (Data, First + 4)))
   with Pre => Data'First = 1 and then Data'Last >= 8 and then
               First <= Data'Last - 7;

   --  NTP short format (16.16 seconds) to whole milliseconds, rounded up.
   function Short_MS (Value : Unsigned_32) return Unsigned_64 is
     ((Unsigned_64 (Value) * 1_000 + 65_535) / 65_536);

   function Request (Nonce : Unsigned_64) return Packet is
      Result : Packet := [others => 0];
   begin
      Result (1) := 16#23#; -- LI 0, VN 4, mode 3
      for I in 0 .. 7 loop
         Result (41 + I) := Unsigned_8 (Shift_Right (Nonce, 56 - 8 * I) and 255);
      end loop;
      return Result;
   end Request;

   procedure To_Unix_MS
     (Timestamp : Unsigned_64; Result : out UTC_Milliseconds; Success : out Boolean)
   is
      Seconds : Unsigned_64 := Shift_Right (Timestamp, 32);
      Fraction_MS : constant Unsigned_64 :=
        Shift_Right ((Timestamp and 16#FFFF_FFFF#) * 1_000, 32);
   begin
      Result := 0;
      Success := False;
      if Seconds < 2 ** 31 then
         Seconds := Seconds + Era_Seconds;
      end if;
      if Seconds < Unix_Offset then
         return;
      end if;
      Seconds := Seconds - Unix_Offset;
      if Seconds > Latest_UTC_MS / 1_000 then
         return;
      end if;
      Result := Seconds * 1_000 + Fraction_MS;
      Success := True;
   end To_Unix_MS;

   procedure Evaluate
     (Reply : Byte_Array; Nonce : Unsigned_64; Sent_MS, Received_MS : Unsigned_64;
      Result : out Estimate; Status : out Rejection)
   is
      Leap, Version, Mode : Unsigned_8;
      Stratum : Unsigned_8;
      Root_Delay_MS, Root_Dispersion_MS : Unsigned_64;
      Receive_MS, Transmit_MS : UTC_Milliseconds;
      Receive_OK, Transmit_OK : Boolean;
      Round_Trip, Server_Hold, Path_Delay, Uncertainty : Unsigned_64;
   begin
      Result := (others => <>);
      if Reply'Length /= Packet_Length or else Reply'First /= 1 then
         Status := Wrong_Length;
         return;
      end if;
      pragma Assert (Reply'Last = Packet_Length);
      Leap := Shift_Right (Reply (1), 6);
      Version := Shift_Right (Reply (1), 3) and 7;
      Mode := Reply (1) and 7;
      Stratum := Reply (2);
      if Mode /= 4 then
         Status := Wrong_Mode;
         return;
      elsif Version not in 3 .. 4 then
         Status := Wrong_Version;
         return;
      elsif Stratum = 0 then
         --  Kiss-o'-Death: the server asks the client to back off or stop.
         Status := Kiss_Of_Death;
         return;
      elsif Stratum > 15 then
         Status := Bad_Stratum;
         return;
      elsif Leap = 3 then
         Status := Unsynchronized;
         return;
      elsif Word64 (Reply, 25) /= Nonce then
         Status := Origin_Mismatch;
         return;
      elsif Word64 (Reply, 33) = 0 or else Word64 (Reply, 41) = 0 then
         Status := Zero_Timestamp;
         return;
      end if;
      To_Unix_MS (Word64 (Reply, 33), Receive_MS, Receive_OK);
      To_Unix_MS (Word64 (Reply, 41), Transmit_MS, Transmit_OK);
      if not Receive_OK or else not Transmit_OK then
         Status := Out_Of_Range;
         return;
      elsif Transmit_MS < Receive_MS then
         Status := Server_Time_Order;
         return;
      elsif Received_MS < Sent_MS then
         Status := Local_Time_Order;
         return;
      end if;
      Round_Trip := Received_MS - Sent_MS;
      if Round_Trip > Maximum_Round_Trip_MS then
         Status := Round_Trip_Too_Long;
         return;
      end if;
      Root_Delay_MS := Short_MS (Word32 (Reply, 5));
      Root_Dispersion_MS := Short_MS (Word32 (Reply, 9));
      if Root_Delay_MS / 2 + Root_Dispersion_MS > Maximum_Root_Distance_MS then
         Status := Root_Distance_Too_Large;
         return;
      end if;
      --  Network path delay excludes the server's hold time; with
      --  millisecond local readings it can round below zero.
      Server_Hold := Transmit_MS - Receive_MS;
      Path_Delay := (if Server_Hold >= Round_Trip then 0 else Round_Trip - Server_Hold);
      --  UTC at reception is transmit time plus the return leg, taken as
      --  half the path delay. The bound covers either leg being the whole
      --  delay, the server's own root distance, and 1 ms local resolution.
      if Transmit_MS > Latest_UTC_MS - Path_Delay / 2 then
         Status := Out_Of_Range;
         return;
      end if;
      Uncertainty := Path_Delay / 2 + Root_Delay_MS / 2 + Root_Dispersion_MS + 1;
      Result :=
        (UTC_MS => Transmit_MS + Path_Delay / 2,
         Observed_MS => Received_MS,
         Uncertainty_MS => Unsigned_32 (Uncertainty));
      Status := Accepted;
   end Evaluate;

   procedure Combine
     (Items : Estimate_Array; Count : Server_Count;
      Result : out Estimate; Agreeing : out Server_Count; Success : out Boolean)
   is
      Reference : Unsigned_64 := 0;
      Low, High : array (1 .. Maximum_Servers) of Unsigned_64 := [others => 0];
      Usable : array (1 .. Maximum_Servers) of Boolean := [others => False];
      Best : Server_Count := 0;
      Best_Low, Best_High : Unsigned_64 := 0;
   begin
      Result := (others => <>);
      Agreeing := 0;
      Success := False;
      for I in 1 .. Count loop
         Reference := Unsigned_64'Max (Reference, Items (I).Observed_MS);
      end loop;
      --  Project each interval forward to the common reference instant.
      for I in 1 .. Count loop
         declare
            Age : constant Unsigned_64 := Reference - Items (I).Observed_MS;
            Center : Unsigned_64;
            Width : constant Unsigned_64 := Unsigned_64 (Items (I).Uncertainty_MS);
         begin
            if Age <= Latest_UTC_MS - Items (I).UTC_MS then
               Center := Items (I).UTC_MS + Age;
               if Center >= Width and then Center <= Latest_UTC_MS - Width then
                  Low (I) := Center - Width;
                  High (I) := Center + Width;
                  Usable (I) := True;
               end if;
            end if;
         end;
      end loop;
      --  The maximum overlap always includes some interval's lower bound.
      for Candidate in 1 .. Count loop
         if Usable (Candidate) then
            declare
               Point : constant Unsigned_64 := Low (Candidate);
               Covering : Server_Count := 0;
               Intersection_Low : Unsigned_64 := 0;
               Intersection_High : Unsigned_64 := Unsigned_64'Last;
            begin
               for J in 1 .. Count loop
                  pragma Loop_Invariant (Covering < J);
                  if Usable (J) and then Low (J) <= Point and then Point <= High (J) then
                     Covering := Covering + 1;
                     Intersection_Low := Unsigned_64'Max (Intersection_Low, Low (J));
                     Intersection_High := Unsigned_64'Min (Intersection_High, High (J));
                  end if;
               end loop;
               if Covering > Best then
                  Best := Covering;
                  Best_Low := Intersection_Low;
                  Best_High := Intersection_High;
               end if;
            end;
         end if;
      end loop;
      if Best >= 2 and then 2 * Best > Count and then Best_Low <= Best_High and then
        Best_High <= Latest_UTC_MS and then
        (Best_High - Best_Low) / 2 + 1 <= Unsigned_64 (Unsigned_32'Last)
      then
         Result :=
           (UTC_MS => Best_Low + (Best_High - Best_Low) / 2,
            Observed_MS => Reference,
            Uncertainty_MS => Unsigned_32 ((Best_High - Best_Low) / 2 + 1));
         Agreeing := Best;
         Success := True;
      end if;
   end Combine;
end SNTP;
