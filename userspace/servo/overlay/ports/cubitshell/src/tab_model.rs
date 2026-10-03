/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Logical tab ownership, independent of engine residency and native widgets.
//! IDs are never reused during a window's lifetime: delayed UI events cannot
//! select or close a replacement tab. Only the visible projection is bounded.
use std::collections::BTreeMap;
use std::ops::Bound::{Excluded, Unbounded};
#[path = "tab_projection.rs"]
pub mod native;

pub type TabId = u64;
pub const VISIBLE_CAPACITY: usize = native::CAPACITY;

pub struct Tabs<T> {
    entries: BTreeMap<TabId, T>,
    next_id: Option<TabId>,
    active: Option<TabId>,
    first: Option<TabId>,
}

impl<T> Default for Tabs<T> {
    fn default() -> Self {
        Self { entries: BTreeMap::new(), next_id: Some(1), active: None, first: None }
    }
}

impl<T> Tabs<T> {
    pub fn values(&self) -> impl Iterator<Item = &T> { self.entries.values() }
    pub fn drain(&mut self) -> impl Iterator<Item = (TabId, T)> {
        self.active = None; self.first = None;
        std::mem::take(&mut self.entries).into_iter()
    }
    pub fn len(&self) -> usize { self.entries.len() }
    pub fn active(&self) -> Option<TabId> { self.active }
    pub fn get(&self, id: TabId) -> Option<&T> { self.entries.get(&id) }
    pub fn get_mut(&mut self, id: TabId) -> Option<&mut T> { self.entries.get_mut(&id) }

    /// No fixed tab-count limit. Integer exhaustion returns ownership to the
    /// caller without changing selection. Allocation still follows Rust's
    /// allocator policy; this is not an engine-memory admission policy.
    pub fn insert(&mut self, value: T) -> Result<TabId, T> {
        let Some(id) = self.next_id else { return Err(value); };
        self.entries.insert(id, value);
        self.next_id = id.checked_add(1);
        self.active = Some(id);
        if self.first.is_none() { self.first = Some(id); }
        Ok(id)
    }

    pub fn select(&mut self, id: TabId) -> bool {
        if !self.entries.contains_key(&id) { return false; }
        self.active = Some(id);
        true
    }

    pub fn cycle(&mut self, backward: bool) -> Option<TabId> {
        let id = self.active?;
        let next = if backward {
            self.entries.range(..id).next_back().or_else(|| self.entries.last_key_value())
        } else {
            self.entries.range((Excluded(id), Unbounded)).next()
                .or_else(|| self.entries.first_key_value())
        }.map(|(&id, _)| id);
        self.active = next;
        next
    }

    /// Return the value to the engine owner, which must arrange asynchronous
    /// retirement separately. Removing metadata alone proves no reclamation.
    pub fn remove(&mut self, id: TabId) -> Option<T> {
        let value = self.entries.remove(&id)?;
        let neighbor = self.entries.range(id..).next()
            .or_else(|| self.entries.range(..id).next_back()).map(|(&id, _)| id);
        if self.active == Some(id) { self.active = neighbor; }
        if self.first == Some(id) { self.first = neighbor; }
        Some(value)
    }

    /// Ordered IDs for local native rows. Work is O(log(total) + visible),
    /// including when the active tab is thousands of positions off screen.
    /// The selected tab remains visible after selection, close and resize.
    pub fn projection(&mut self, requested: usize) -> Vec<TabId> {
        let limit = requested.min(VISIBLE_CAPACITY);
        let Some(active) = self.active else { return Vec::new(); };
        if limit == 0 { return Vec::new(); }
        let mut first = self.first.unwrap_or(active).min(active);
        if !self.entries.range(first..=active).take(limit).any(|(&id, _)| id == active) {
            first = *self.entries.range(..=active).rev().nth(limit - 1).unwrap().0;
        }
        let mut rows: Vec<_> = self.entries.range(first..).take(limit).map(|(&id, _)| id).collect();
        // Fill trailing space after closes or a wider viewport, preserving order.
        if rows.len() < limit {
            first = *self.entries.range(..=first).rev().nth(limit - rows.len())
                .or_else(|| self.entries.first_key_value()).unwrap().0;
            rows = self.entries.range(first..).take(limit).map(|(&id, _)| id).collect();
        }
        self.first = Some(first);
        rows
    }

    /// Materialize one complete bridge transaction, including titles. A live
    /// model always supplies its active row, even before native geometry exists.
    pub fn snapshot(&mut self, requested: usize, title: impl Fn(&T) -> &str) -> native::Snapshot {
        let ids = self.projection(requested.max(1));
        let mut result = native::Snapshot {
            total: self.len() as u64, active: self.active.unwrap_or(0),
            count: ids.len() as u32, ..Default::default()
        };
        for (row, id) in result.items.iter_mut().zip(ids) {
            row.id = id;
            let text = title(&self.entries[&id]).as_bytes();
            row.length = text.len().min(row.text.len()) as u32;
            for (dest, &byte) in row.text.iter_mut().zip(text) {
                *dest = if (b' '..=b'~').contains(&byte) { byte } else { b'?' };
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_canonicalizes_titles_and_always_includes_active() {
        let mut tabs = Tabs::default();
        assert_eq!(tabs.snapshot(0, |value: &String| value), native::Snapshot::default());
        tabs.insert("a".repeat(80)).unwrap();
        let snapshot = tabs.snapshot(0, |value| value);
        assert_eq!((snapshot.total, snapshot.active, snapshot.count), (1, 1, 1));
        assert_eq!(snapshot.items[0].length, 64);
        assert_eq!(snapshot.items[0].text, [b'a'; 64]);
        tabs.insert("a\n铜".to_string()).unwrap();
        let snapshot = tabs.snapshot(1, |value| value);
        assert_eq!(snapshot.items[0].id, 2);
        assert_eq!(snapshot.items[0].length, 5);
        assert_eq!(&snapshot.items[0].text[..5], b"a????");
        assert!(snapshot.items[0].text[5..].iter().all(|&byte| byte == 0));
        assert!(snapshot.items[1..].iter().all(|row| *row == native::Row::default()));
    }

    #[test]
    fn thousands_of_tabs_and_bounded_projection() {
        let mut tabs = Tabs::default();
        for value in 0..10_000 { assert_eq!(tabs.insert(value), Ok(value + 1)); }
        assert_eq!(tabs.len(), 10_000);
        assert_eq!(tabs.projection(usize::MAX), (9_969..=10_000).collect::<Vec<_>>());
        assert!(tabs.select(1));
        assert_eq!(tabs.projection(4), vec![1, 2, 3, 4]);
        assert_eq!(tabs.cycle(true), Some(10_000));
        assert_eq!(tabs.projection(2), vec![9_999, 10_000]);
        assert_eq!(tabs.cycle(false), Some(1));
    }

    #[test]
    fn stale_ids_never_alias_and_last_close_can_reopen() {
        let mut tabs = Tabs::default();
        let a = tabs.insert("first").unwrap();
        assert_eq!(tabs.remove(a), Some("first"));
        assert_eq!(tabs.active(), None);
        assert!(tabs.projection(32).is_empty());
        let b = tabs.insert("second").unwrap();
        assert_ne!(a, b);
        assert!(!tabs.select(a));
        assert_eq!(tabs.remove(a), None);
        assert_eq!(tabs.active(), Some(b));
        *tabs.get_mut(b).unwrap() = "updated";
        assert_eq!(tabs.get(b), Some(&"updated"));
    }

    #[test]
    fn integer_exhaustion_does_not_wrap_or_change_selection() {
        let mut tabs = Tabs { next_id: Some(u64::MAX), ..Tabs::default() };
        assert_eq!(tabs.insert(7), Ok(u64::MAX));
        assert_eq!(tabs.insert(8), Err(8));
        assert_eq!(tabs.len(), 1);
        assert_eq!(tabs.cycle(false), Some(u64::MAX));
        assert_eq!(tabs.projection(32), vec![u64::MAX]);
    }

    #[test]
    fn mixed_operations_match_ordered_reference() {
        let mut tabs = Tabs::default();
        let mut reference = Vec::new();
        let mut active = None;
        let mut random = 51_u64;
        for step in 0..30_000 {
            random = random.wrapping_mul(6364136223846793005).wrapping_add(1);
            let pick = (random >> 32) as usize;
            if reference.is_empty() || pick % 5 < 2 {
                let id = tabs.insert(step).unwrap(); reference.push(id); active = Some(id);
            } else {
                let index = pick % reference.len();
                let id = reference[index];
                match pick % 5 {
                    2 => {
                        reference.remove(index); assert_eq!(tabs.remove(id).is_some(), true);
                        if active == Some(id) {
                            active = reference.get(index).or_else(|| reference.last()).copied();
                        }
                    },
                    3 => { assert!(tabs.select(id)); active = Some(id); },
                    _ => {
                        let index = reference.iter().position(|id| Some(*id) == active).unwrap();
                        let backward = pick & 1 != 0;
                        active = Some(reference[if backward { (index + reference.len() - 1) % reference.len() }
                            else { (index + 1) % reference.len() }]);
                        assert_eq!(tabs.cycle(backward), active);
                    },
                }
            }
            assert_eq!(tabs.len(), reference.len()); assert_eq!(tabs.active(), active);
            let requested = pick % 40;
            let rows = tabs.projection(requested);
            assert_eq!(rows.len(), reference.len().min(requested.min(VISIBLE_CAPACITY)));
            if !rows.is_empty() {
                assert!(rows.contains(&active.unwrap()));
                let offset = reference.iter().position(|id| *id == rows[0]).unwrap();
                assert_eq!(rows, reference[offset..offset + rows.len()]);
            }
        }
    }
}
