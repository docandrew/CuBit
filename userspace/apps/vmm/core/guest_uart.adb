------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  The guest's 16550A UART (guest_uart.ads).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Guest_UART with SPARK_Mode is

   --  What MSR reads: in loopback, MCR's outputs (DTR to DSR, RTS to CTS,
   --  OUT1 to RI, OUT2 to DCD), as the 8250 probe expects; otherwise a
   --  connected, idle line (DCD, DSR, CTS).
   function Modem_Lines (U : UART) return Unsigned_8 is
     (if (U.MCR and MCR_Loopback) /= 0 then
        (if (U.MCR and MCR_DTR) /= 0 then MSR_DSR else 0)
        or (if (U.MCR and MCR_RTS) /= 0 then MSR_CTS else 0)
        or (if (U.MCR and MCR_OUT1) /= 0 then MSR_RI else 0)
        or (if (U.MCR and MCR_OUT2) /= 0 then MSR_DCD else 0)
      else MSR_DCD or MSR_DSR or MSR_CTS);

   procedure Read (U : in out UART; R : Register; Value : out Unsigned_8) is
   begin
      case R is
         when Data_Register =>
            if DLAB (U) then
               Value := U.DLL;
            elsif U.Count > 0 then
               Value := U.Store (U.Head);
               U.Head := (U.Head + 1) mod FIFO_Depth;
               U.Count := U.Count - 1;
            else
               Value := 0;
            end if;
         when Interrupt_Enable =>
            Value := (if DLAB (U) then U.DLM else U.IER);
         when Interrupt_Identity =>
            Value := (if FIFO_Enabled (U) then IIR_FIFOs_Enabled else 0) or Identity (U);
            --  Reading IIR takes a THRE interrupt it reports.
            if Identity (U) = IIR_THR_Empty then
               U.THR_Pending := False;
            end if;
         when Line_Control =>
            Value := U.LCR;
         when Modem_Control =>
            Value := U.MCR;
         when Line_Status =>
            Value := LSR_THR_Empty or LSR_Transmitter_Empty
              or (if U.Count > 0 then LSR_Data_Ready else 0);
         when Modem_Status =>
            Value := Modem_Lines (U);
         when Scratch =>
            Value := U.SCR;
      end case;
   end Read;

   procedure Write (U : in out UART; R : Register; Value : Unsigned_8; Effect : out Write_Effect) is
      Before : constant Boolean := Line (U);
      --  A THR write empties and refills THR: the line falls and, unless
      --  received data holds it, rises again.
      Retrigger : Boolean := False;
   begin
      Effect := (others => <>);
      case R is
         when Data_Register =>
            if DLAB (U) then
               U.DLL := Value;
            else
               if (U.MCR and MCR_Loopback) = 0 then
                  Effect.Transmitted := True;
                  Effect.Byte := Value;
               end if;
               U.THR_Pending := False;
               Retrigger := not Line (U);
               U.THR_Pending := True;
            end if;
         when Interrupt_Enable =>
            if DLAB (U) then
               U.DLM := Value;
            else
               --  Enabling ETBEI while THR is empty (always, here) is a
               --  THRE interrupt.
               if (Value and IER_THR_Empty) /= 0 and then (U.IER and IER_THR_Empty) = 0 then
                  U.THR_Pending := True;
               end if;
               U.IER := Value and IER_Writable;
            end if;
         when Interrupt_Identity =>
            --  Clearing the receive FIFO, or switching FIFOs on or off,
            --  empties it (the datasheet: a FIFO-enable change resets both).
            if (Value and FCR_Clear_Receive) /= 0
              or else ((Value and FCR_Enable) /= 0) /= FIFO_Enabled (U)
            then
               Effect.Discarded := U.Count;
               U.Count := 0;
               U.Head := 0;
            end if;
            --  The clear bits are self-clearing; bits 6-7 and the enable stay.
            U.FCR := Value and not (FCR_Clear_Receive or FCR_Clear_Transmit);
         when Line_Control =>
            U.LCR := Value;
         when Modem_Control =>
            U.MCR := Value and MCR_Writable;
         when Line_Status | Modem_Status =>
            null;   --  read-only here (a factory-test write on real parts)
         when Scratch =>
            U.SCR := Value;
      end case;
      Effect.Raise_IRQ := Line (U) and then (not Before or else Retrigger);
   end Write;

   procedure Receive (U : in out UART; B : Unsigned_8; Raise_IRQ : out Boolean) is
      Before : constant Boolean := Line (U);
   begin
      U.Store ((U.Head + U.Count) mod FIFO_Depth) := B;
      U.Count := U.Count + 1;
      Raise_IRQ := Line (U) and then not Before;
   end Receive;
end Guest_UART;
