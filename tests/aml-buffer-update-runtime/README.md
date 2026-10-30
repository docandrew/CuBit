# Existing Store_Buffer runtime regression

The unchanged existing aml-debug/buffer_store_tests.adb exercises empty/nonempty
updates, short/long inputs, extreme input lower bounds, invalid IDs/non-buffer IDs,
a full byte pool and failure framing. Both release/checked profiles are required.
This suite does not establish proof or whole interpreter/native correctness.
