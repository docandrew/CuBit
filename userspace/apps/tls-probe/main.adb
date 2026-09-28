pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CCL_Manifest_Bindings;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Net_Channels;
with TLS_Clock;
with SPARKNaCl; use SPARKNaCl;
with SPARKTLS; use SPARKTLS;
with SPARKTLS.Client;
with SPARKTLS.Cert_Verify;
with SPARKTLS.RBG;
with SPARKEntropy;
with SPARKTLSCrypto.HMAC_DRBG;
with SPARKTLSCrypto.Hashing.SHA256;
with SPARKTLSCrypto.MAC;
with X509;
with TLS_Probe_Roots;

--  Native SPARKTLS probe (headless tls-probe regression). Links SPARKTLS into
--  a CuBit process, seeds its RBG from SPARKEntropy, loads a test root from
--  memory and runs TLS handshakes over netstack TCP against the loopback
--  fixture in tests/tls/server.py. This validates the library on CuBit
--  before the TLS client service is built around the same adapter.
procedure Main is
   Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Tls_Test;
   package Channels renames CuBit.Net_Channels;
   Server_Name : constant String := "tls-test.cubit.internal";
   Transfer_Size : constant := 16 * 4_096;
   Ring_Size : constant := 32_768;
   Channel_Bytes : constant := Channels.Layout.Header_Bytes + 2 * Ring_Size;

   --  The channel's grant (header and rings), then a scratch buffer.
   Allocation : constant Unsigned_64 :=
     syscall (SYSCALL_SBRK, Channel_Bytes + Transfer_Size);
   Channel_Address : constant System.Address :=
     To_Address (Integer_Address (Allocation));
   Transfer_Address : constant System.Address :=
     To_Address (Integer_Address (Allocation) + Channel_Bytes);
   Transfer : Byte_Seq (0 .. Transfer_Size - 1)
     with Import, Address => Transfer_Address;
   Stream : Channels.Stream;
   Token : Unsigned_64 := 0;

   function Next_Token return Unsigned_64 is
   begin
      Token := Token + 1;
      return Token;
   end Next_Token;

   Roots : aliased Trust_Store;
   Session : Client_Session;
   Passed : Boolean := True;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then Passed := False; end if;
      debugPrint ("tls-probe: " & Name & (if Condition then " PASS" else " FAIL") &
                  ASCII.LF);
   end Check;


   function Error_Name (Code : Error_Code) return String is
     (case Code is
         when No_Error => "none",
         when Bad_Certificate => "bad certificate",
         when Certificate_Unknown => "certificate unknown",
         when Certificate_Expired => "certificate expired",
         when Certificate_Verify_Failed => "certificate verify failed",
         when Handshake_Failure => "handshake failure",
         when Entropy_Failure => "entropy failure",
         when Internal_Error => "internal error",
         when others => "code" & Error_Code'Pos (Code)'Image);

   --  Send everything SPARKTLS has queued.
   function Flush return Boolean is
      Count : N32;
      Sent, Put : Natural;
      Ready : Boolean;
   begin
      loop
         Drain_Ciphertext (Session, Transfer, Count);
         exit when Count = 0;
         Sent := 0;
         while Sent < Natural (Count) loop
            Channels.Write
              (Stream, Transfer_Address + Storage_Offset (Sent),
               Natural (Count) - Sent, Put);
            Sent := Sent + Put;
            if Put = 0 then
               if Channels.Failed (Stream) then
                  return False;
               end if;
               Channels.Await
                 (Stream, Slot, Channels.Layout.Want_Writable,
                  syscall (SYSCALL_GETTIME) + 15_000, Next_Token, Ready);
               if not Ready then
                  return False;
               end if;
            end if;
         end loop;
      end loop;
      return True;
   end Flush;

   --  Read what has arrived from TCP into the session, waiting up to 15 s
   --  for something. False on end of stream, error or timeout.
   function Receive return Boolean is
      Fed, Offset, Length : N32;
      Got : Natural;
      Ready : Boolean;
   begin
      Channels.Await
        (Stream, Slot, Channels.Layout.Want_Readable,
         syscall (SYSCALL_GETTIME) + 15_000, Next_Token, Ready);
      Channels.Read (Stream, Transfer_Address, Transfer_Size, Got);
      if not Ready or else Got = 0 then
         return False;
      end if;
      Length := N32 (Got);
      Offset := 0;
      while Offset < Length loop
         declare
            --  SPARKTLS requires zero-based input.
            Chunk : constant Byte_Seq (0 .. Length - Offset - 1) :=
              Transfer (Offset .. Length - 1);
         begin
            Feed_Ciphertext (Session, Chunk, Fed);
         end;
         if Fed = 0 then
            return False; -- input buffer full without progress
         end if;
         Offset := Offset + Fed;
      end loop;
      return True;
   end Receive;

   --  One handshake (and, when expected to succeed, a PING/PONG exchange).
   procedure Attempt (Port : Unsigned_16; Expect_Success : Boolean; Name : String) is
      Scheme : constant String := "@net:tcp:10.0.2.2:" &
        Port'Image (Port'Image'First + 1 .. Port'Image'Last);
      Reply : Message;
      Open_Token : constant Unsigned_64 := Next_Token;
      Submitted, Completed : Boolean;
      Result : Action;
      Count : N32;
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Established, Exchanged, Failed : Boolean := False;
      Error : Error_Code := No_Error;
   begin
      Channels.Reset (Stream);
      Channels.Submit_Open (Stream, Slot, Scheme, Open_Token, Submitted);
      if Submitted then
         Channels.Wait_For (Open_Token, Reply, Completed);
      end if;
      if not Submitted or else not Completed or else
        not Channels.Opened (Stream, Reply)
      then
         Check (False, Name & ": TCP connect");
         return;
      end if;
      Session := SPARKTLS.Client.Configure
        ((Server_Name => To_Name (Server_Name),
          Trust => Roots'Unchecked_Access,
          Get_Time => TLS_Clock.Now'Access,
          Verify_Mode => Mode_WebPKI,
          others => <>));
      Handshake : loop
         SPARKTLS.Client.Advance (Session, Result);
         case Result is
            when OK => null;
            when Has_Output =>
               exit Handshake when not Flush;
            when Need_Input =>
               exit Handshake when not Receive;
            when Handshake_Done =>
               Established := True;
               exit Handshake;
            when Plaintext_Ready =>
               Read_Plaintext (Session, Transfer, Count);
            when Error_Alert | Shutdown =>
               Failed := True;
               Error := Last_Error (Session);
               --  Send our alert, if any, so the peer sees why.
               if Flush then null; end if;
               exit Handshake;
         end case;
      end loop Handshake;
      if Established then
         debugPrint ("tls-probe: " & Name & ": handshake" &
                     Unsigned_64'Image (syscall (SYSCALL_GETTIME) - Started) & " ms, " &
                     (case Get_Version (Session) is
                        when TLS_1_3 => "TLS 1.3",
                        when TLS_1_2 => "TLS 1.2",
                        when others => "TLS ?") & ASCII.LF);
         Write_Plaintext (Session, [Character'Pos ('P'), Character'Pos ('I'),
                                    Character'Pos ('N'), Character'Pos ('G')], Count);
         if Count = 4 and then Flush then
            Exchange : loop
               SPARKTLS.Client.Advance (Session, Result);
               case Result is
                  when Plaintext_Ready =>
                     Read_Plaintext (Session, Transfer, Count);
                     Exchanged := Count = 4 and then Transfer (0 .. 3) =
                       [Character'Pos ('P'), Character'Pos ('O'),
                        Character'Pos ('N'), Character'Pos ('G')];
                     exit Exchange;
                  when Need_Input =>
                     exit Exchange when not Receive;
                  when Has_Output =>
                     exit Exchange when not Flush;
                  when OK | Handshake_Done => null;
                  when Error_Alert | Shutdown => exit Exchange;
               end case;
            end loop Exchange;
         end if;
         if State (Session) = Connected and then not Write_Limit_Reached (Session) then
            SPARKTLS.Client.Close_Notify (Session);
            if Flush then null; end if;
         end if;
      end if;
      Drop (Session);
      Channels.Close (Stream, Slot);
      if Expect_Success then
         Check (Established and then Exchanged, Name & ": verified handshake and data exchange");
      else
         debugPrint ("tls-probe: " & Name & ": rejected with " & Error_Name (Error) &
                     ASCII.LF);
         Check (not Established and then Failed, Name & ": rejected");
      end if;
   end Attempt;

   Entropy_OK, Roots_OK, Granted : Boolean;
   Arena : Channels.Arena;
   Loaded : Natural;
   Started : Unsigned_64;
   --  Stage-by-stage diagnostics for RBG start-up on CuBit.
   procedure Diagnose is
      State : SPARKEntropy.Entropy_State;
      OK : Boolean;
      Sample : SPARKEntropy.Byte_Seq (0 .. 31);
      ABC : constant SPARKNaCl.Byte_Seq (0 .. 2) :=
        [Character'Pos ('a'), Character'Pos ('b'), Character'Pos ('c')];
      ABC_Digest : constant SPARKTLSCrypto.Hashing.SHA256.Digest :=
        SPARKTLSCrypto.Hashing.SHA256.Hash (ABC);
   begin
      Check (ABC_Digest (0) = 16#BA# and then ABC_Digest (1) = 16#78# and then
             ABC_Digest (31) = 16#AD#, "SHA-256 known answer");
      declare
         use SPARKTLSCrypto.Hashing.SHA256;
         Long : constant SPARKNaCl.Byte_Seq (0 .. 199) := [others => Character'Pos ('a')];
         Expected_Long : constant Digest := [16#c2#, 16#a9#, 16#08#, 16#d9#, 16#8f#, 16#5d#, 16#f9#, 16#87#, 16#ad#, 16#e4#, 16#1b#, 16#5f#, 16#ce#, 16#21#, 16#30#, 16#67#, 16#ef#, 16#bc#, 16#c2#, 16#1e#, 16#f2#, 16#24#, 16#02#, 16#12#, 16#a4#, 16#1e#, 16#54#, 16#b5#, 16#e7#, 16#c2#, 16#8a#, 16#e5#];
         Key : constant SPARKNaCl.Byte_Seq (0 .. 19) := [others => 16#0B#];
         Hi : constant SPARKNaCl.Byte_Seq (0 .. 7) :=
           [16#48#, 16#69#, 16#20#, 16#54#, 16#68#, 16#65#, 16#72#, 16#65#];
         Expected_MAC : constant Digest := [16#b0#, 16#34#, 16#4c#, 16#61#, 16#d8#, 16#db#, 16#38#, 16#53#, 16#5c#, 16#a8#, 16#af#, 16#ce#, 16#af#, 16#0b#, 16#f1#, 16#2b#, 16#88#, 16#1d#, 16#c2#, 16#00#, 16#c9#, 16#83#, 16#3d#, 16#a7#, 16#26#, 16#e9#, 16#37#, 16#6c#, 16#2e#, 16#32#, 16#cf#, 16#f7#];
         Got : Digest;
         Same : Boolean;
      begin
         Got := Hash (Long);
         Same := True;
         for I in Got'Range loop
            Same := Same and then Got (I) = Expected_Long (I);
         end loop;
         Check (Same, "SHA-256 multi-block known answer (bytewise)");
         Check (Got = Expected_Long, "SHA-256 multi-block known answer (array =)");
         SPARKTLSCrypto.MAC.HMAC_SHA_256 (Got, Hi, Key);
         Same := True;
         for I in Got'Range loop
            Same := Same and then Got (I) = Expected_MAC (I);
         end loop;
         Check (Same, "HMAC-SHA-256 RFC 4231 case 1 (bytewise)");
      end;
      Check (SPARKTLSCrypto.HMAC_DRBG.Self_Test, "HMAC-DRBG self-test");
      SPARKEntropy.Init (State, OK);
      Check (OK, "SPARKEntropy start-up health tests");
      if OK then
         SPARKEntropy.Generate (State, Sample, OK);
         Check (OK, "SPARKEntropy output");
      end if;
   end Diagnose;
begin
   Diagnose;
   Started := syscall (SYSCALL_GETTIME);
   SPARKTLS.RBG.Init (Entropy_OK);
   debugPrint ("tls-probe: entropy start-up" &
               Unsigned_64'Image (syscall (SYSCALL_GETTIME) - Started) & " ms, OSR" &
               SPARKTLS.RBG.Entropy_OSR'Image & ", resets" &
               SPARKTLS.RBG.Entropy_Resets'Image & ASCII.LF);
   Check (Entropy_OK, "SPARKEntropy health tests and DRBG instantiation");
   SPARKTLS.Cert_Verify.Load_Roots (Roots, TLS_Probe_Roots.DER, Loaded, Roots_OK);
   Check (Roots_OK and then Loaded = 1, "test root loaded from memory");
   Check (Allocation /= Unsigned_64'Last, "transfer allocation");
   if not (Entropy_OK and Roots_OK) or else Allocation = Unsigned_64'Last then
      return;
   end if;
   Channels.Create_Arena
     (Arena, Slot, Channel_Address, Ring_Size, Ring_Size, 1, Granted);
   Check (Granted, "channel arena");
   if not Granted then
      return;
   end if;
   Channels.Prepare (Stream, Arena, 0, 0, Slot);
   Attempt (18460, True, "valid certificate");
   Attempt (18461, False, "wrong host name");
   Attempt (18462, False, "untrusted root");
   Attempt (18463, False, "expired certificate");
   if Passed then
      debugPrint ("TEST: PASS tls-probe" & ASCII.LF);
   end if;
end Main;
