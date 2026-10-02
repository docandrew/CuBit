#pragma once
#include <stdint.h>
/* Caller serializes lifetimes and pins endpoint slots. Parent must already be
 * acquired and owner-forwardable. Creates a read-only terminal child; does not
 * attach it. Whole-page range. 0 success, 1 invalid/denied; output clears on
 * failure. Caller owns child until retirement, independently of parent borrow. */
uint32_t cubit_intel_forward_presentation(uint64_t desktop_slot, uint64_t parent,
   uint64_t offset, uint64_t bytes, uint64_t *child);
/* Completed linear BGRA8888 only, NOT tiled/compressed/in-flight images.
 * 0 accepted, 1..6 Desktop status, 7 local/uncertain protocol error. Retain child
 * on ALL outcomes. Success is not a release fence; uncertainty requires revoke
 * and confirmed retirement before reuse, never blind retry. */
uint32_t cubit_intel_attach_linear(uint64_t desktop_slot, uint64_t surface,
   uint64_t child, uint64_t width, uint64_t height, uint64_t pitch);
/* 0 retired, 1 pending/uncertain query, 2 rejected. Repeat on the same child.
 * No GPU fence, BO reclamation or permission to free other aliases implied. */
uint32_t cubit_intel_retire_presentation(uint64_t child);
