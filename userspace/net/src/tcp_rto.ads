------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The retransmission timeout (RFC 6298), in milliseconds.
--
--  Round-trip samples update SRTT and RTTVAR with alpha = 1/8 and
--  beta = 1/4; RTO = SRTT + max (G, 4 * RTTVAR), clamped to
--  [Minimum_RTO, Maximum_RTO]; a timeout doubles RTO up to Maximum_RTO
--  (RFC 6298 5.5). The postconditions (proved, tests/net-tcp) are the
--  properties the retransmission timer relies on: no overflow, the RTO is
--  always within its bounds, and backoff never shortens it.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package TCP_RTO with SPARK_Mode, Pure is

   Clock_Granularity : constant := 1;         --  G: the kernel clock (ms)
   Minimum_RTO       : constant := 200;       --  as Linux; RFC 6298 says 1 s
   Maximum_RTO       : constant := 120_000;   --  RFC 6298 2.5 allows >= 60 s
   Initial_RTO       : constant := 1_000;     --  RFC 6298 2.1
   Maximum_Sample    : constant := 600_000;   --  samples beyond 10 min are clipped

   subtype Milliseconds is Unsigned_32;
   subtype RTO_Value is Milliseconds range Minimum_RTO .. Maximum_RTO;
   subtype Sample is Milliseconds range 0 .. Maximum_Sample;

   type Estimator is record
      Measured : Boolean := False;        --  a sample has been taken
      SRTT     : Sample := 0;
      RTTVAR   : Sample := 0;
      RTO      : RTO_Value := Initial_RTO;
   end record;

   --  RFC 6298 2.2 (first sample) and 2.3 (later samples).
   procedure Update (E : in out Estimator; R : Sample) with
     Post => E.Measured and then
             --  (E.RTO is within its bounds by its type.)
             --  RTO covers the smoothed RTT (when within the maximum).
             (if E.SRTT + Clock_Granularity <= Maximum_RTO then
                E.RTO >= Unsigned_32'Min (E.SRTT + Clock_Granularity, Maximum_RTO)
              else E.RTO = Maximum_RTO);

   --  RFC 6298 5.5: on a retransmission timeout, back off.
   procedure Back_Off (E : in out Estimator) with
     Post => E.RTO >= E.RTO'Old and then
             E.RTO = Unsigned_32'Min (2 * E.RTO'Old, Maximum_RTO) and then
             E.Measured = E.Measured'Old and then
             E.SRTT = E.SRTT'Old and then E.RTTVAR = E.RTTVAR'Old;
end TCP_RTO;
