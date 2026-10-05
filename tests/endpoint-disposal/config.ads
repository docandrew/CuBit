-- Hosted/proof fixture: only the real table bound, no serial-port dependency.
package Config with SPARK_Mode is
   PER_PROCESS_CAPABILITIES : constant := 64;
end Config;
