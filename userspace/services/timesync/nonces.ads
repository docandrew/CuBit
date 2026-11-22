with Interfaces; use Interfaces;

--  64-bit SNTP transmit-timestamp nonces (RFC 9109). A server must echo the
--  nonce, so an off-path attacker who cannot predict it cannot forge a
--  reply. RDRAND is used when the CPU reports it; otherwise the value mixes
--  the TSC and monotonic time, which only resists blind guessing and is
--  reported so the weaker mode is visible.
package Nonces is
   procedure Initialize (Hardware : out Boolean);
   function Next return Unsigned_64;
end Nonces;
